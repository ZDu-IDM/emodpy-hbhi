# LLINs_with_IRS_or_PBO.R

library(data.table)
library(tidyverse)
library(lubridate)
library(rgdal)
library(raster)
library(RColorBrewer)
library(viridis)
library(plot.matrix)
require(sp)
library(rgeos)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
#                     set filepaths and load needed functions
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###

## User-specified information on scenarios to use: 
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
# source functions needed
script_base_filepath = paste(user_path, '/Documents/malaria-sle-hbhi/simulation/plot_results_analyses/optimize_LLIN_IRS', sep='')
source(paste0(script_base_filepath, '/optimization_functions.R'))

# set name of country to run
country_name = 'Burundi'
if(country_name =='Burundi'){
  hbhi_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
  admin_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
  sim_input_reference_filename = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_for_projections.csv')
  
  # filepath for simulation output
  sim_set = 'mortLog_blockNash'  #  options: '', 'mortLogLog_blockOurs', 'mortLog_blockOurs', 'mortLogLog_blockNash', 'mortLog_blockNash'. These are generated running the subset_sim_output.R script. 
  sim_output_filepath = paste(hbhi_dir, '/simulation_output/simulations_future', sep='')
  if(sim_set != ''){
    sim_output_filepath = paste0(sim_output_filepath, '/simulation_subsets/', sim_set)
  }
  

  admin_shapefile_filepath = paste0(hbhi_dir, '/SpatialClustering/reference_rasters_shapefiles/bdi_adm2.shp')
  shapefile_admin_colname='NOMDEP'
  
  # comparison/reference net distribution scenarion (or starting point for comparison)
  current_itn_plan = read.csv(paste0(hbhi_dir, '/simulation_inputs/scenario_comparisons/current_net_plan.csv'))
  net_choice_colname = 'pbo_ig2'  # options: pbo, pbo_ig2
  
  # IRS admin names
  irs_admins = sort(c('Buye', 'Gashoho', 'Kiremba', 'Muyinga'))
  
  # cost per net distributed in an IRS area (how much would be saved if this net weren't distributed - includes materials and distributions costs)
  cost_saved_no_irs_per_net = 4.96
  # cost per net to update to PBO in non-IRS area (difference between cost of distributing conventional and PBO net)
  cost_upgrade_pbo_per_net = 0.87
  
} else{
  warning('Country name not supported')
}
comparison_start_year = 2022
comparison_end_year = 2024
years_included = comparison_end_year - comparison_start_year + 1 

optimization_metric = 'clinical_cases_all'  # options: clinical_cases_u5, clinical_cases_all, pfpr_u5, pfpr_all, deaths_u5, deaths_all


# set of experiment names
experiment_shared_name = 'BDI_projection_'
irs_coverage = 61  # options: 61 or 80
permethrin_mortality =40  # options: 64, 40 (values depend on what has bee run)
# experiment names used for net optimization. In order: 1) all admins have pyrethroid nets, 2) all admins have PBO nets
net_type_names = c('pyr', 'PBO')  # order: from current tool to more expensive tool
net_type_experiment_names = paste0(experiment_shared_name, permethrin_mortality, net_type_names)  # list of experiments with pyr nets and with PBO nets, in that order

# experiment names used for IRS with and without nets. In order: 1) all IRS admins have pyrethroid nets, 2) IRS admins don't have nets
irs_type_names = c('wLLIN', 'woLLIN')  # order: with nets, without nets
# expt_name_irs_regions_with_nets = ifelse(irs_coverage==61, net_type_experiment_names[1], paste0(experiment_shared_name, irs_type_names[1], '_',permethrin_mortality, 'pyr_', irs_coverage, 'IRS'))
expt_name_irs_regions_with_nets = paste0(experiment_shared_name, irs_type_names[1], '_',permethrin_mortality, 'pyr_', irs_coverage, 'IRS')
irs_type_experiment_names = c(expt_name_irs_regions_with_nets, paste0(experiment_shared_name, irs_type_names[2], '_', irs_coverage, 'IRS'))

optim_results_filepath = paste0(sim_output_filepath, '/_plots/optimize_IRScov',irs_coverage,'_permMort', permethrin_mortality)
if(!dir.exists(paste0(sim_output_filepath, '/_plots'))) dir.create(paste0(sim_output_filepath, '/_plots'))
if(!dir.exists(optim_results_filepath)) dir.create(optim_results_filepath)



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
#                     get number of nets used in each admin during comparison perios
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###

# population size in each DS
admin_pop = fread(admin_pop_df_filename)[,c('admin_name','pop_size')] 

# get number of nets needed in each admin based on simulation input coverages (assume same coverage in all scenarios, just different types of nets)
sim_info_reference = read.csv(sim_input_reference_filename)
input_itn_mass_filename = paste0(hbhi_dir, '/simulation_inputs/', sim_info_reference$ITN_filename[sim_info_reference$ScenarioName == net_type_experiment_names[1]], '.csv')
input_itn_epi_filename = paste0(hbhi_dir, '/simulation_inputs/', sim_info_reference$EPI_ITN_filename[sim_info_reference$ScenarioName == net_type_experiment_names[1]], '.csv')
input_itn_anc_filename = paste0(hbhi_dir, '/simulation_inputs/', sim_info_reference$ANC_ITN_filename[sim_info_reference$ScenarioName == net_type_experiment_names[1]], '.csv')
cumulative_nets = get_expected_nets_each_admin_from_sim_inputs(input_itn_mass_filename=input_itn_mass_filename, input_itn_epi_filename=input_itn_epi_filename, input_itn_anc_filename=input_itn_anc_filename,
                                                               admin_pop=admin_pop, comparison_start_year=comparison_start_year, comparison_end_year=comparison_end_year,
                                                               frac_turn_1_each_year=0.0366, frac_born_each_year=0.0385)

# current net distribution plan (which admins are already scheduled to get a PBO net)
current_itn_plan = current_itn_plan[order(current_itn_plan$admin_name),]
current_itn_plan = merge(current_itn_plan[,c('admin_name', net_choice_colname)], cumulative_nets[,c('admin_name','total_nets')], by='admin_name')
# admins are eligible for upgrade if they don't areadly have PBO
admins_eligible_for_upgrade = current_itn_plan$admin_name[which(current_itn_plan[[net_choice_colname]] == 0)]
# exclude IRS districts from list of admins eligible for upgrade - ordered alphabetically
admins_eligible_for_upgrade = sort(admins_eligible_for_upgrade[which(!(admins_eligible_for_upgrade %in% irs_admins))])
# get indices of admins eligible for upgrade (in ordered list)
admin_indices_eligible_for_upgrade = which(sort(admin_pop$admin_name) %in% admins_eligible_for_upgrade)

# number of nets in each of the upgrade-eligible admin
nets_each_admin = cumulative_nets[which(cumulative_nets$admin_name %in% admins_eligible_for_upgrade), 
                                  which(colnames(cumulative_nets) %in% c('admin_name','total_nets'))]
# order alphabetically
nets_each_admin = nets_each_admin[order(nets_each_admin$admin_name),]


# total nets across upgrade-elibigle areas
total_nets_eligible_admin = sum(nets_each_admin$total_nets)

# get number of nets used in mass distributions in IRS admins
num_nets_in_irs_admins = sum(cumulative_nets$num_mass_nets[cumulative_nets$admin_name %in% irs_admins])

# total dollars saved if no nets were distributed in IRS areas
total_saved_no_irs_nets = cost_saved_no_irs_per_net * num_nets_in_irs_admins

# number of nets that could be upgraded to PBO for that cost
number_nets_upgraded = total_saved_no_irs_nets/cost_upgrade_pbo_per_net
pbo_net_budget = number_nets_upgraded


# fraction of all nets that can be PBO
pbo_net_frac = pbo_net_budget / total_nets_eligible_admin


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
#                   find best set of admins to upgrade to PBO (optimization algorithm)
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
#                     read in and format malaria burden simulation output for net optimization in non-IRS areas
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
pfpr_u5_df = data.frame(admin_name=admins_eligible_for_upgrade)
pfpr_all_df = data.frame(admin_name=admins_eligible_for_upgrade)
deaths_u5_df = data.frame(admin_name=admins_eligible_for_upgrade)
deaths_all_df = data.frame(admin_name=admins_eligible_for_upgrade)
clinical_cases_u5_df = data.frame(admin_name=admins_eligible_for_upgrade)
clinical_cases_all_df = data.frame(admin_name=admins_eligible_for_upgrade)
pop_all_df = data.frame(admin_name=admin_pop$admin_name)
pop_u5_df = data.frame(admin_name=admin_pop$admin_name)
# get malaria burden values for all scenarios
for(ss in 1:length(net_type_experiment_names)){
  experiment_name = net_type_experiment_names[ss]
  option_name = net_type_names[ss]
  cur_burden = get_total_burden(sim_output_filepath, experiment_name=experiment_name, admin_pop=admin_pop, comparison_start_year=comparison_start_year, comparison_end_year=comparison_end_year)
  # subset to admins eligible for an upgrade from pyrethroid to PBO nets
  cur_burden = cur_burden[admin_indices_eligible_for_upgrade, ]
    
  pfpr_u5_df = add_new_scenario_column_to_burden_df(target_indicator_df=pfpr_u5_df, cur_burden=cur_burden, indicator_colname='pfpr_u5', option_name=option_name)
  pfpr_all_df = add_new_scenario_column_to_burden_df(target_indicator_df=pfpr_all_df, cur_burden=cur_burden, indicator_colname='pfpr_all', option_name=option_name)
  deaths_u5_df = add_new_scenario_column_to_burden_df(target_indicator_df=deaths_u5_df, cur_burden=cur_burden, indicator_colname='deaths_u5', option_name=option_name)
  deaths_all_df = add_new_scenario_column_to_burden_df(target_indicator_df=deaths_all_df, cur_burden=cur_burden, indicator_colname='deaths_all', option_name=option_name)
  clinical_cases_u5_df = add_new_scenario_column_to_burden_df(target_indicator_df=clinical_cases_u5_df, cur_burden=cur_burden, indicator_colname='clinical_cases_u5', option_name=option_name)
  clinical_cases_all_df = add_new_scenario_column_to_burden_df(target_indicator_df=clinical_cases_all_df, cur_burden=cur_burden, indicator_colname='clinical_cases_all', option_name=option_name)
  pop_all_df = add_new_scenario_column_to_burden_df(target_indicator_df=pop_all_df, cur_burden=cur_burden, indicator_colname='pop_size', option_name=option_name)
  pop_u5_df = add_new_scenario_column_to_burden_df(target_indicator_df=pop_u5_df, cur_burden=cur_burden, indicator_colname='pop_size_u5', option_name=option_name)
}


# check whether output seems reasonable
# increasing doses should only decrease pfpr (and probably also mortality, though the model might not capture this correctly)
#  if any of the higher coverages have higher burden, it likely indicates a problem or that too few seeds were used.
burden_dfs = list(pfpr_u5_df, pfpr_all_df, deaths_u5_df, deaths_all_df, clinical_cases_u5_df, clinical_cases_all_df)
burden_names = c('PfPR U5', 'PfPR all','mortality U5','mortality all', 'clinical cases U5', 'clinical cases all')
for(ii in 1:length(burden_dfs)){
  ww = which((burden_dfs[[ii]][[net_type_names[2]]] - burden_dfs[[ii]][net_type_names[[1]]])>0)
  if(length(ww)>0) print(paste0(burden_names[ii], ',more expensive tool worse than less expensive tool for DS: ', paste(ww, collapse=', '))); cat('\n')
}

# overall burden with more expensive intervention relative to less expensive intervention
for(ii in 1:length(burden_dfs)){
  ww = sum(burden_dfs[[ii]][[net_type_names[2]]]) / sum(burden_dfs[[ii]][net_type_names[[1]]])
  print(paste0(burden_names[ii], ': ', round(ww,2), ' (', round(sum(burden_dfs[[ii]][net_type_names[[1]]]),0),' pyr versus ', round(sum(burden_dfs[[ii]][[net_type_names[2]]]),0),' PBO)')); cat('\n')
}




### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
#                     read in and format malaria burden simulation output IRS areas
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
irs_pfpr_u5_df = data.frame(admin_name=irs_admins)
irs_pfpr_all_df = data.frame(admin_name=irs_admins)
irs_deaths_u5_df = data.frame(admin_name=irs_admins)
irs_deaths_all_df = data.frame(admin_name=irs_admins)
irs_clinical_cases_u5_df = data.frame(admin_name=irs_admins)
irs_clinical_cases_all_df = data.frame(admin_name=irs_admins)
irs_pop_all_df = data.frame(admin_name=admin_pop$admin_name)
irs_pop_u5_df = data.frame(admin_name=admin_pop$admin_name)
# get malaria burden values for all scenarios
for(ss in 1:length(irs_type_experiment_names)){
  experiment_name = irs_type_experiment_names[ss]
  option_name = irs_type_names[ss]
  cur_burden = get_total_burden(sim_output_filepath, experiment_name=experiment_name, admin_pop=admin_pop[which(admin_pop$admin_name %in% irs_admins),], comparison_start_year=comparison_start_year, comparison_end_year=comparison_end_year)

  irs_pfpr_u5_df = add_new_scenario_column_to_burden_df(target_indicator_df=irs_pfpr_u5_df, cur_burden=cur_burden, indicator_colname='pfpr_u5', option_name=option_name)
  irs_pfpr_all_df = add_new_scenario_column_to_burden_df(target_indicator_df=irs_pfpr_all_df, cur_burden=cur_burden, indicator_colname='pfpr_all', option_name=option_name)
  irs_deaths_u5_df = add_new_scenario_column_to_burden_df(target_indicator_df=irs_deaths_u5_df, cur_burden=cur_burden, indicator_colname='deaths_u5', option_name=option_name)
  irs_deaths_all_df = add_new_scenario_column_to_burden_df(target_indicator_df=irs_deaths_all_df, cur_burden=cur_burden, indicator_colname='deaths_all', option_name=option_name)
  irs_clinical_cases_u5_df = add_new_scenario_column_to_burden_df(target_indicator_df=irs_clinical_cases_u5_df, cur_burden=cur_burden, indicator_colname='clinical_cases_u5', option_name=option_name)
  irs_clinical_cases_all_df = add_new_scenario_column_to_burden_df(target_indicator_df=irs_clinical_cases_all_df, cur_burden=cur_burden, indicator_colname='clinical_cases_all', option_name=option_name)
  irs_pop_all_df = add_new_scenario_column_to_burden_df(target_indicator_df=pop_all_df, cur_burden=cur_burden, indicator_colname='pop_size', option_name=option_name)
  irs_pop_u5_df = add_new_scenario_column_to_burden_df(target_indicator_df=pop_u5_df, cur_burden=cur_burden, indicator_colname='pop_size_u5', option_name=option_name)
}


### - - - - - - - - - - - - - - - - - - - ###
#          set metric to be optimized
### - - - - - - - - - - - - - - - - - - - ###
if(optimization_metric == 'deaths_u5') {
  burden_df = deaths_u5_df
  irs_burden_df = irs_deaths_u5_df
}
if(optimization_metric == 'deaths_all') {
  burden_df = deaths_all_df
  irs_burden_df = irs_deaths_all_df
}
if(optimization_metric == 'clinical_cases_u5') {
  burden_df = clinical_cases_u5_df
  irs_burden_df = irs_clinical_cases_u5_df
}
if(optimization_metric == 'clinical_cases_all') {
  burden_df = clinical_cases_all_df
  irs_burden_df = irs_clinical_cases_all_df
}


# fraction of population U5 (assumed to be the same across all experiments)
frac_u5_each_exp = c(as.vector(t(pop_u5_df[,2:ncol(pop_u5_df)] / pop_all_df[,2:ncol(pop_all_df)])), as.vector(t(irs_pop_u5_df[,2:ncol(irs_pop_u5_df)] / irs_pop_all_df[,2:ncol(irs_pop_all_df)])))
frac_u5_assumed = mean(frac_u5_each_exp)
########################################################################################################################
# Optimization algorithms
########################################################################################################################


##################################################
# iterate through random neighborhood descent
##################################################
if(pbo_net_frac >= 1){  # all eligible areas can receive PBO nets
  local_best_states = matrix(rep(2, length(admins_eligible_for_upgrade), ncol=1))
  local_best_states2=local_best_states
  rownames(local_best_states2) = admins_eligible_for_upgrade
  write.csv(local_best_states2, paste0(optim_results_filepath, '/best_PBO_admins_among_eligible_',optimization_metric,'.csv'), row.names=TRUE)
  
} else{  # optimize best allocation of PBO nets
  if(file.exists(paste0(optim_results_filepath, '/best_PBO_admins_among_eligible_',optimization_metric,'.csv'))){
    local_best_states = read.csv(paste0(optim_results_filepath, '/best_PBO_admins_among_eligible_',optimization_metric,'.csv'), row=TRUE)
  } else{
    num_starting_states = 20
    num_steps = 50
    neighborhood_distance = 2  # during local search, calculate burden in neighborhood_distance of current state, move to the one with the lowest malaria burden
    # get a list of num_starting_states starting points from which to run VNS algorithm
    s2 = rep(0, length(admins_eligible_for_upgrade))
    s3 = rep(1,length(admins_eligible_for_upgrade))
    s_rest = c(rbinom(n=(round(num_starting_states/3) * length(admins_eligible_for_upgrade)),size=1, prob=min(1,pbo_net_frac*0.4)),
               rbinom(n=(round(num_starting_states/3) * length(admins_eligible_for_upgrade)),size=1, prob=min(1,pbo_net_frac*0.7)),
               rbinom(n=(round(num_starting_states/3) * length(admins_eligible_for_upgrade)),size=1, prob=min(1,pbo_net_frac*1)))
    starting_states = matrix(data=c(s2,s3,s_rest), nrow=length(admins_eligible_for_upgrade), byrow=FALSE) + 1   # we add 1 so that the starting state indicates the column of the burden dataframe
    
    local_best_states = matrix(NA, ncol=ncol(starting_states), nrow=nrow(starting_states))
    for(start_ii in 1:ncol(starting_states)){
      print(paste0('Beginning the search from starting state ', start_ii, ' out of ', num_starting_states))
      local_best_states[,start_ii] = neighborhood_mutation_search(starting_state_each_ds=starting_states[,start_ii], burden_df=burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget, num_steps=num_steps, neighborhood_distance=neighborhood_distance)  
    }
    local_best_states2 = local_best_states
    rownames(local_best_states2) = admins_eligible_for_upgrade
    write.csv(local_best_states2, paste0(optim_results_filepath, '/best_PBO_admins_among_eligible_',optimization_metric,'.csv'), row.names=TRUE)
  }
}
# calculate burden outcome for each of the best local states
burden_output_states = apply(X=local_best_states, MARGIN=2, FUN=sum_total_burden, burden_df=burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
# find the column with the best burden
best_proposed_state_index = which.min(burden_output_states)[1]
print('Optimization reached same burden for starting states: '); print(which(burden_output_states == min(burden_output_states)))

best_b=sum_total_burden(state_column_each_ds = local_best_states[,best_proposed_state_index], burden_df = burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
noPBO_b=sum_total_burden(state_column_each_ds = rep(1,length(admins_eligible_for_upgrade)), burden_df = burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
(noPBO_b-best_b)/noPBO_b

# get vector of admins selected to upgrade to PBO nets
admins_upgrade_to_PBO = admins_eligible_for_upgrade[which(local_best_states[,best_proposed_state_index] == 2)]





##############################################################################
#  allow some IRS areas to keep nets - this was DONE MANUALLY!!!!!!
##############################################################################
# can keep LLINs in some IRS areas and still upgrade all eligible regions to PBO - which IRS admins to keep conventional nets in?
if(pbo_net_frac >= 1){  # all eligible areas can receive PBO nets
  # data frame of the number of nets that can be upgraded if nets are removed from an IRS area
  net_upgrade_from_each_IRS = cumulative_nets[cumulative_nets$admin_name %in% irs_admins,]
  # total dollars saved if no nets were distributed in IRS areas
  net_upgrade_from_each_IRS$dollars_saved = cost_saved_no_irs_per_net * net_upgrade_from_each_IRS$num_mass_nets
  # number of nets that could be upgraded to PBO for that cost
  net_upgrade_from_each_IRS$number_nets_upgraded = net_upgrade_from_each_IRS$dollars_saved / cost_upgrade_pbo_per_net
  
  sum(net_upgrade_from_each_IRS$number_nets_upgraded)
  
  # find which combinations would still give enough nets to cover eligible areas - result must be positive to work
  sum(net_upgrade_from_each_IRS$number_nets_upgraded[1:4]) - total_nets_eligible_admin  # remove nets from all IRS admins, works, but lots of unspent budget
  sum(net_upgrade_from_each_IRS$number_nets_upgraded[2:4]) - total_nets_eligible_admin  # best option
  sum(net_upgrade_from_each_IRS$number_nets_upgraded[c(1,3,4)]) - total_nets_eligible_admin  # also works, but more unspent budget
  sum(net_upgrade_from_each_IRS$number_nets_upgraded[3:4]) - total_nets_eligible_admin  # XX
  sum(net_upgrade_from_each_IRS$number_nets_upgraded[1:3]) - total_nets_eligible_admin  # XX
  # must remove nets from Kiremba and Muyinga, and then also either from Buye or Gashoho. Closer to budget limit to remove from Gashoho and keep in Buye.
  irs_admins_with_nets = c('Buye')
  irs_admins_without_nets = irs_admins[which(!(irs_admins %in% irs_admins_with_nets))]
  # note: must only include IRS admins keeping nets if all upgrade-elibigle admins were upgraded and there were still nets leftover
} else{
  irs_admins_with_nets = c()
  irs_admins_without_nets = irs_admins
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
###                           create plots of outputs
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
# create data frame of burden outcomes by experiment (experiment on column, admin on row). All admins included, but many will be left NA for non-applicable experiments. 
# Experiment order: 
#    1) all admins have pyrethroid nets
#    2) all admins have PBO nets
#    3) all IRS admins have pyrethroid nets
#    4) IRS admins don't have nets
burden_all_experiments = data.frame(admin_name = admin_pop$admin_name)
burden_all_experiments = merge(burden_all_experiments, burden_df, by='admin_name', all.x=TRUE)
burden_all_experiments = merge(burden_all_experiments, irs_burden_df, by='admin_name', all.x=TRUE)

all_experiment_names = c(net_type_names, irs_type_names)
experiment_colors = c(rgb(0.7,0.5,1), rgb(0.3,0,0.5), rgb(0.1,0.5,0.2), rgb(0.5,1,0.7))

# Create data frame of experiments to plot for each admin in each of three scenarios. 
# Scenario order:
#   1) IRS admins have pyrethroid nets, the admins_upgrade_to_PBO admins only have pyrethroid nets
#   2) none of the IRS areas have nets, the admins_upgrade_to_PBO admins have PBO nets
#   3) the irs_admins_without_nets admins do not have nets, the irs_admins_with_nets admins have pyrethroid nets, the admins_upgrade_to_PBO admins have PBO nets

# create data frame with the intervention scenario in each admin (corresponds to the experiment used for that admin) - scenarios are columns, admins are rows
exp_each_admin_scen = data.frame(admin_name = admin_pop$admin_name,
                                 scen_1 = rep(NA, length(admin_pop$admin_name)),
                                 scen_2 = rep(NA, length(admin_pop$admin_name)),
                                 scen_3 = rep(NA, length(admin_pop$admin_name)))
# fill in scenario 1
exp_each_admin_scen$scen_1[exp_each_admin_scen$admin_name %in% irs_admins] = irs_type_names[1]
exp_each_admin_scen$scen_1[exp_each_admin_scen$admin_name %in% admins_upgrade_to_PBO] = net_type_names[1]

# fill in scenario 2
exp_each_admin_scen$scen_2[exp_each_admin_scen$admin_name %in% irs_admins] = irs_type_names[2]
exp_each_admin_scen$scen_2[exp_each_admin_scen$admin_name %in% admins_upgrade_to_PBO] = net_type_names[2]

# fill in scenario 3
exp_each_admin_scen$scen_3[exp_each_admin_scen$admin_name %in% irs_admins_with_nets] = irs_type_names[1]
exp_each_admin_scen$scen_3[exp_each_admin_scen$admin_name %in% irs_admins_without_nets] = irs_type_names[2]
exp_each_admin_scen$scen_3[exp_each_admin_scen$admin_name %in% admins_upgrade_to_PBO] = net_type_names[2]



# create data frame with the burden in each admin in each scenario - scenarios are columns, admins are rows. Scenario order same as in exp_each_admin_scen
burden_each_admin_scen = data.frame(admin_name = admin_pop$admin_name,
                                 scen_1 = rep(NA, length(admin_pop$admin_name)),
                                 scen_2 = rep(NA, length(admin_pop$admin_name)),
                                 scen_3 = rep(NA, length(admin_pop$admin_name)))
# brute force it since the dataframe is small
for(aa in 1:nrow(burden_each_admin_scen)){
  if(!is.na(exp_each_admin_scen[aa,2])){
    current_admin = burden_each_admin_scen$admin_name[aa]
    for(ss in 2:ncol(burden_each_admin_scen)){
      burden_each_admin_scen[aa, ss] = burden_all_experiments[[exp_each_admin_scen[aa,ss]]][which(burden_all_experiments$admin_name == current_admin)]
    }
  }
}
total_burden_each_scen = colSums(burden_each_admin_scen[2:length(burden_each_admin_scen)], na.rm=TRUE)
pyreth_burden = total_burden_each_scen[1]


##################################
# plot
##################################
# read in shapefile
admin_shapefile = shapefile(admin_shapefile_filepath)
admin_names = admin_pop$admin_name

min_value = min(burden_all_experiments[,2:ncol(burden_all_experiments)], na.rm=TRUE)
max_value = max(burden_all_experiments[,2:ncol(burden_all_experiments)], na.rm=TRUE)
if(grepl('u5', optimization_metric)){
  min_value_perCap = min(burden_all_experiments[,2:ncol(burden_all_experiments)]/(admin_pop$pop_size*frac_u5_assumed)/years_included, na.rm=TRUE)
  max_value_perCap = max(burden_all_experiments[,2:ncol(burden_all_experiments)]/(admin_pop$pop_size*frac_u5_assumed)/years_included, na.rm=TRUE)
} else{
  min_value_perCap = min(burden_all_experiments[,2:ncol(burden_all_experiments)]/admin_pop$pop_size/years_included, na.rm=TRUE)
  max_value_perCap = max(burden_all_experiments[,2:ncol(burden_all_experiments)]/admin_pop$pop_size/years_included, na.rm=TRUE)
}


num_colors = 40
colorscale = colorRampPalette(brewer.pal(9, 'YlGnBu'))(num_colors)


# iterate through scenarios, creating plots for each scenario

# plot intervention scenarios - all admins that are not IRS or selected for PBO upgrade colored in light grey
#   1) IRS areas with nets (dark green), areas selected for PBO upgrade (light purple)
#   2) IRS areas without nets (light green), areas selected for PBO upgrade (dark purple)
#   3) three IRS areas without nets (light green), one IRS area with nets (dark green), areas selected for PBO upgrade (dark purple)


# plot columns
plot_scenario_names = c('current plan','upgrade PBO instead','upgrade PBO instead v2')
plot_scenario_state_values = list()
for(i_scen in 1:length(plot_scenario_names)){
  # get experiment indices
  cur_state_column_each_ds=rep(NA, length(admin_names))
  for(aa in 1:length(cur_state_column_each_ds)){
    if(!(is.na(exp_each_admin_scen$scen_1[aa]))){
      cur_state_column_each_ds[aa] = which(all_experiment_names == exp_each_admin_scen$scen_1[aa])
    }
  }
}


png(paste0(optim_results_filepath, '/map_',optimization_metric,'.png'), width=12.5, height=10, unit='in', res=700)
layout(mat=matrix(1:(3*length(plot_scenario_names)), nrow=3))
par(mar=c(0,0,2,0))  # mar=c(5,4,4,2)

for(i_scen in 1:length(plot_scenario_names)){
  # map with net choice for each admin
  # get experiment indices
  cur_state_column_each_ds=rep(NA, length(admin_names))
  for(aa in 1:length(cur_state_column_each_ds)){
    if(!(is.na(exp_each_admin_scen[[paste0('scen_',i_scen)]][aa]))){
      cur_state_column_each_ds[aa] = which(all_experiment_names == exp_each_admin_scen[[paste0('scen_',i_scen)]][aa])
    }
  }
  
  create_itn_type_map(state_each_ds=cur_state_column_each_ds, admin_names=admin_names, color_each_type=experiment_colors, 
                      admin_shapefile=admin_shapefile, shapefile_admin_colname=shapefile_admin_colname, 
                      pdf_filename='', plot_title=plot_scenario_names[i_scen])
  
  # map with per-capita burden in each admin
  burden_each_admin_cur = burden_each_admin_scen[[paste0('scen_',i_scen)]]
  if(grepl('u5', optimization_metric)){
    burden_each_admin_cur_perCap = burden_each_admin_scen[[paste0('scen_',i_scen)]] / (admin_pop$pop_size*frac_u5_assumed) / years_included
  } else{
    burden_each_admin_cur_perCap = burden_each_admin_scen[[paste0('scen_',i_scen)]] / admin_pop$pop_size / years_included
  }
  create_burden_map(burden_each_admin=burden_each_admin_cur_perCap, admin_names=admin_names, colorscale=colorscale, min_value=min_value_perCap, max_value=max_value_perCap, num_colors=num_colors, 
                    admin_shapefile=admin_shapefile, shapefile_admin_colname=shapefile_admin_colname,
                    pdf_filename='', plot_title=paste0( round(100*sum(burden_each_admin_cur, na.rm=TRUE)/pyreth_burden,1), '% of the current-plan burden'))
  
  # map with burden in each admin
  create_burden_map(burden_each_admin=burden_each_admin_cur, admin_names=admin_names, colorscale=colorscale, min_value=min_value, max_value=max_value, num_colors=num_colors, 
                    admin_shapefile=admin_shapefile, shapefile_admin_colname=shapefile_admin_colname,
                    pdf_filename='', plot_title=paste0('total ', optimization_metric, ': ', round(sum(burden_each_admin_cur, na.rm=TRUE))))
}
dev.off()


png(paste0(optim_results_filepath, '/map_',optimization_metric,'_legend.png'), width=1, height=3, unit='in', res=700)
par(mfrow=c(1,1), mar=c(0,0,2,0))  # mar=c(5,4,4,2)

# legend - colorbar
legend_label_vals = seq(min_value_perCap, max_value_perCap, length.out=5)
legend_image = as.raster(matrix(rev(colorscale[sapply(floor((num_colors)*(legend_label_vals - min_value_perCap) / (max_value_perCap - min_value_perCap))+1, min, num_colors)]), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = paste(optimization_metric, 'per capita'))
text(x=1.5, y = seq(0,1,length.out=5), labels = round(legend_label_vals*1000))
rasterImage(legend_image, 0, 0, 1,1)
dev.off()










##################################
# summary information on burden averted if switch all districts
##################################

# if switch all IRS areas to have no nets and all eligible other areas to have PBO, what is impact?
group_outcomes = colSums(burden_all_experiments[,2:ncol(burden_all_experiments)], na.rm=TRUE)
print(optim_results_filepath); print(optimization_metric)
# all affected areas
original_burden = (group_outcomes[1] + group_outcomes[3])
comparison_burden = (group_outcomes[2] + group_outcomes[4])
print(paste0('    all affected areas: ', round(((original_burden-comparison_burden)/original_burden)*100,1)))
# PBO upgraded areas
original_burden = (group_outcomes[1])
comparison_burden = (group_outcomes[2])
print(paste0('    PBO-upgraded areas: ', round(((original_burden-comparison_burden)/original_burden)*100,1)))
# IRS areas
original_burden = (group_outcomes[3])
comparison_burden = (group_outcomes[4])
print(paste0('    IRS areas: ', round(((original_burden-comparison_burden)/original_burden)*100,1)))

# PBO areas
1 - colSums(burden_df[,2:3])[2] / colSums(burden_df[,2:3])[1]
# IRS areas
1 - colSums(irs_burden_df[,2:3])[2] / colSums(irs_burden_df[,2:3])[1]
# all areas
1 - (colSums(burden_df[,2:3]) + colSums(irs_burden_df[,2:3]))[2] / (colSums(burden_df[,2:3]) + colSums(irs_burden_df[,2:3]))[1]
