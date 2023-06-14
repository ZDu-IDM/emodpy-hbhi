#################################################################################################
# High burden, high impact 
# Monique Ambrose
# April 2021
#
# 
# Compare different ways of allocating a limited number of PBO nets
#
#################################################################################################


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
#                     set filepaths and load needed functions
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
  
  # this budget should be at least as large as the current itn plan
  pbo_net_budget_three_year_with_ig2 = 5100000
  pbo_net_budget_two_year_with_ig2 = 4650000
  pbo_net_budget_three_year_without_ig2 = 1550000
  pbo_net_budget_two_year_without_ig2 = 1420000

} else{
  warning('Country name not supported')
}
comparison_start_year = 2022
comparison_end_year = 2023
years_included = comparison_end_year - comparison_start_year + 1 
if (net_choice_colname == 'pbo_ig2'){
  if (years_included == 3){
    pbo_net_budget = pbo_net_budget_three_year_with_ig2
  } else if(years_included == 2){
    pbo_net_budget = pbo_net_budget_two_year_with_ig2
  } else{
    warning('Unknown PBO net budget for this duration.')
  }
} else if (net_choice_colname == 'pbo'){
  if (years_included == 3){
    pbo_net_budget = pbo_net_budget_three_year_without_ig2
  } else if(years_included == 2){
    pbo_net_budget = pbo_net_budget_two_year_without_ig2
  } else{
    warning('Unknown PBO net budget for this duration.')
  }
} else{
  warning('Did not recognize whether both PBO and IG2 admins will be included in optimization.')
}

optimization_metric = 'deaths_all'  # options: clinical_cases_u5, clinical_cases_all, pfpr_u5, pfpr_all, deaths_u5, deaths_all


# set of experiment names
experiment_shared_name = 'BDI_projection_'
permethrin_mortality = 40  # options: 64, 40 (values depend on what has bee run)
net_type_names = c('pyr', 'PBO')  # order: from current tool to more expensive tool
comparison_scenarios = paste0(experiment_shared_name, permethrin_mortality, net_type_names)
optim_results_filepath = paste0(sim_output_filepath, '/_plots/optimize_PBO_permMort', permethrin_mortality, '_', net_choice_colname, '_',comparison_start_year, '_', comparison_end_year)
if(!dir.exists(paste0(sim_output_filepath, '/_plots'))) dir.create(paste0(sim_output_filepath, '/_plots'))
if(!dir.exists(optim_results_filepath)) dir.create(optim_results_filepath)



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
#                     get number of nets used in each admin during comparison perios
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###

# population size in each DS
admin_pop = fread(admin_pop_df_filename)[,c('admin_name','pop_size')] 




# ## -old version- ##
# # get number of nets distributed in each DS from simulation, rescaled to DS population size (average across all seeds and scenarios since these numbers should not depend on net type)
# # 1) get number of nets distributed in relevant years, 2) get simulated population size, 3) rescale to actual population size, 4) average rescaled values across runs
# #   output is a data table with first column giving admin name and second column giving number of nets in comparison period
# nets_each_admin = get_expected_nets_each_admin(sim_output_filepath=sim_output_filepath, comparison_scenarios=comparison_scenarios, admin_pop=admin_pop, comparison_start_year=comparison_start_year, comparison_end_year=comparison_end_year)

# get number of nets needed in each admin based on simulation input coverages (assume same coverage in all scenarios, just different types of nets)
sim_info_reference = read.csv(sim_input_reference_filename)
input_itn_mass_filename = paste0(hbhi_dir, '/simulation_inputs/', sim_info_reference$ITN_filename[sim_info_reference$ScenarioName == comparison_scenarios[1]], '.csv')
input_itn_epi_filename = paste0(hbhi_dir, '/simulation_inputs/', sim_info_reference$EPI_ITN_filename[sim_info_reference$ScenarioName == comparison_scenarios[1]], '.csv')
input_itn_anc_filename = paste0(hbhi_dir, '/simulation_inputs/', sim_info_reference$ANC_ITN_filename[sim_info_reference$ScenarioName == comparison_scenarios[1]], '.csv')
cumulative_nets = get_expected_nets_each_admin_from_sim_inputs(input_itn_mass_filename=input_itn_mass_filename, input_itn_epi_filename=input_itn_epi_filename, input_itn_anc_filename=input_itn_anc_filename,
                                             admin_pop=admin_pop, comparison_start_year=comparison_start_year, comparison_end_year=comparison_end_year,
                                             frac_turn_1_each_year=0.0366, frac_born_each_year=0.0385)
nets_each_admin = cumulative_nets[,c('admin_name','total_nets')]

# total nets across entire country
total_nets_all = sum(cumulative_nets$total_nets)

# current net budget (number of PBO nets distributed in current plan)
current_itn_plan = current_itn_plan[order(current_itn_plan$admin_name),]
current_itn_plan = merge(current_itn_plan[,c('admin_name', net_choice_colname)], cumulative_nets[,c('admin_name','total_nets')], by='admin_name')
pbo_net_current = sum(current_itn_plan[[net_choice_colname]] * current_itn_plan$total_nets)

pbo_net_frac = pbo_net_budget / total_nets_all


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
#                     read in and format malaria burden simulation output
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
pfpr_u5_df = data.frame(admin_name=admin_pop$admin_name)
pfpr_all_df = data.frame(admin_name=admin_pop$admin_name)
deaths_u5_df = data.frame(admin_name=admin_pop$admin_name)
deaths_all_df = data.frame(admin_name=admin_pop$admin_name)
clinical_cases_u5_df = data.frame(admin_name=admin_pop$admin_name)
clinical_cases_all_df = data.frame(admin_name=admin_pop$admin_name)
pop_all_df = data.frame(admin_name=admin_pop$admin_name)
pop_u5_df = data.frame(admin_name=admin_pop$admin_name)
# get malaria burden values for all scenarios
for(ss in 1:length(comparison_scenarios)){
  experiment_name = comparison_scenarios[ss]
  option_name = net_type_names[ss]
  cur_burden = get_total_burden(sim_output_filepath, experiment_name=experiment_name, admin_pop=admin_pop, comparison_start_year=comparison_start_year, comparison_end_year=comparison_end_year)

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



### - - - - - - - - - - - - - - - - - - - ###
#          set metric to be optimized
### - - - - - - - - - - - - - - - - - - - ###
if(optimization_metric == 'deaths_u5') burden_df = deaths_u5_df
if(optimization_metric == 'deaths_all') burden_df = deaths_all_df
if(optimization_metric == 'clinical_cases_u5') burden_df = clinical_cases_u5_df
if(optimization_metric == 'clinical_cases_all') burden_df = clinical_cases_all_df


########################################################################################################################
# Optimization
########################################################################################################################


##################################################
# iterate through random neighborhood descent
##################################################
if(file.exists(paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'))){
  local_best_states = read.csv(paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'), row.names=1)
} else{
  num_starting_states = 20#50
  num_steps = 200
  neighborhood_distance = 5  # during local search, calculate burden in neighborhood_distance of current state, move to the one with the lowest malaria burden
  # get a list of num_starting_states starting points from which to run VNS algorithm
  s1 = current_itn_plan[[net_choice_colname]] 
  s2 = rep(0, nrow(admin_pop))
  s3 = rep(1, nrow(admin_pop))
  s_rest = c(rbinom(n=(round(num_starting_states/3) * nrow(admin_pop)),size=1, prob=pbo_net_frac*0.4),
             rbinom(n=(round(num_starting_states/3) * nrow(admin_pop)),size=1, prob=pbo_net_frac*0.7),
             rbinom(n=(round(num_starting_states/3) * nrow(admin_pop)),size=1, prob=pbo_net_frac*1))
  starting_states = matrix(data=c(s1,s2,s3,s_rest), nrow=nrow(admin_pop), byrow=FALSE) + 1   # we add 1 so that the starting state indicates the column of the burden dataframe
  
  local_best_states = matrix(NA, ncol=ncol(starting_states), nrow=nrow(starting_states))
  for(start_ii in 1:ncol(starting_states)){
    print(paste0('Beginning the search from starting state ', start_ii, ' out of ', num_starting_states))
    local_best_states[,start_ii] = neighborhood_mutation_search(starting_state_each_ds=starting_states[,start_ii], burden_df=burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget, num_steps=num_steps, neighborhood_distance=neighborhood_distance)  
  }
  local_best_states2 = local_best_states
  rownames(local_best_states2) = sort(admin_pop$admin_name)
  write.csv(local_best_states2, paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'), row.names=TRUE)
}

# calculate burden outcome for each of the best local states
burden_output_states = apply(X=local_best_states, MARGIN=2, FUN=sum_total_burden, burden_df=burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
# find the column with the best burden
best_proposed_state_index = which.min(burden_output_states)[1]
print('Optimization reached same burden for starting states: '); print(which(burden_output_states == min(burden_output_states)))

best_b=sum_total_burden(state_column_each_ds = local_best_states[,best_proposed_state_index], burden_df = burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
noPBO_b=sum_total_burden(state_column_each_ds = rep(1,nrow(admin_pop)), burden_df = burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
proposed_b = sum_total_burden(state_column_each_ds = (1+current_itn_plan[[net_choice_colname]]), burden_df = burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
(proposed_b-best_b)/proposed_b
(noPBO_b-best_b)/noPBO_b




######################################################
# plots
######################################################
# read in shapefile
admin_shapefile = shapefile(admin_shapefile_filepath)
admin_names = admin_pop$admin_name


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
#                     plot malaria burden for each PBO strategy in each DS
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
# color_each_type = c(rgb(0.3,0,0.7), rgb(0.6,0.8,1))
color_each_type = c(rgb(0.7,0.5,1), rgb(0.3,0,0.5))

min_value = min(burden_df[,2:ncol(burden_df)], na.rm=TRUE)
max_value = max(burden_df[,2:ncol(burden_df)], na.rm=TRUE)
if(grepl('u5', optimization_metric)){
  min_value_perCap = min(burden_df[,2:ncol(burden_df)]/pop_u5_df[,2:ncol(burden_df)]/years_included, na.rm=TRUE)
  max_value_perCap = max(burden_df[,2:ncol(burden_df)]/pop_u5_df[,2:ncol(burden_df)]/years_included, na.rm=TRUE)
} else{
  min_value_perCap = min(burden_df[,2:ncol(burden_df)]/pop_all_df[,2:ncol(burden_df)]/years_included, na.rm=TRUE)
  max_value_perCap = max(burden_df[,2:ncol(burden_df)]/pop_all_df[,2:ncol(burden_df)]/years_included, na.rm=TRUE)
}

num_colors = 40
colorscale = colorRampPalette(brewer.pal(9, 'YlGnBu'))(num_colors)
# colorscale = (brewer.pal(num_colors,'Greens'))
# colorscale = colorRampPalette(brewer.pal(9, 'Greens'))(num_colors)

# baseline burden for the all-pyrethroid scenario
pyreth_burden = sum(get_burden_each_admin(burden_df=burden_df, state_column_each_ds=rep(1,length(admin_names))))

# plot columns
plot_scenario_names = c('all pyrethroid LLINs','current plan','optimized plan','all PBO LLINs')
plot_scenario_state_values = list(rep(1,length(admin_names)),
                                  (current_itn_plan[[net_choice_colname]] + 1),
                                  (local_best_states[,best_proposed_state_index]),
                                  rep(2,length(admin_names)))


png(paste0(optim_results_filepath, '/map_',optimization_metric,'.png'), width=12.5, height=10, unit='in', res=700)
layout(mat=matrix(1:(3*length(plot_scenario_names)), nrow=3))
par(mar=c(0,0,2,0))  # mar=c(5,4,4,2)

for(i_scen in 1:length(plot_scenario_names)){
  # map with net choice for each admin
  cur_state_column_each_ds=plot_scenario_state_values[[i_scen]]
  create_itn_type_map(state_each_ds=cur_state_column_each_ds, admin_names=admin_names, color_each_type=color_each_type, 
                      admin_shapefile=admin_shapefile, shapefile_admin_colname=shapefile_admin_colname, 
                      pdf_filename='', plot_title=plot_scenario_names[i_scen])
  
  # map with per-capita burden in each admin
  burden_each_admin_cur = get_burden_each_admin(burden_df=burden_df, state_column_each_ds=cur_state_column_each_ds)
  if(grepl('u5', optimization_metric)){
    pop_size_cur_df = pop_u5_df
  } else{
    pop_size_cur_df = pop_all_df
  }
  pop_sizes_cur = rep(NA, nrow(pop_u5_df))
  for(ii in 1:nrow(pop_u5_df)){
    pop_sizes_cur[ii] = pop_size_cur_df[ii,(cur_state_column_each_ds[ii]+1)]
  } 
  burden_each_admin_cur_perCap = get_burden_each_admin(burden_df=burden_df, state_column_each_ds=cur_state_column_each_ds) / pop_sizes_cur / years_included
  create_burden_map(burden_each_admin=burden_each_admin_cur_perCap, admin_names=admin_names, colorscale=colorscale, min_value=min_value_perCap, max_value=max_value_perCap, num_colors=num_colors, 
                    admin_shapefile=admin_shapefile, shapefile_admin_colname=shapefile_admin_colname,
                    pdf_filename='', plot_title=paste0( round(100*sum(burden_each_admin_cur)/pyreth_burden,1), '% of the all-pyrethroid burden'))
  
  # map with burden in each admin
  create_burden_map(burden_each_admin=burden_each_admin_cur, admin_names=admin_names, colorscale=colorscale, min_value=min_value, max_value=max_value, num_colors=num_colors, 
                    admin_shapefile=admin_shapefile, shapefile_admin_colname=shapefile_admin_colname,
                    pdf_filename='', plot_title=paste0('total ', optimization_metric, ': ', round(sum(burden_each_admin_cur))))
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









