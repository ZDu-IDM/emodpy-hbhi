# plot_costEffectiveness_versus_PfPR.R

# for each admin, plot the deaths or cases averted per net (y-axis) against the average PfPR over a time period (x-axis)
# note: to color-code points according to whether optimized to get PBO, must have already run all relevant scenarios in LLIN_PBO_optimization.R



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
  } else{
  warning('Country name not supported')
}
comparison_start_year = 2022
comparison_end_year = 2023
permethrin_mortality = 40  # options: 64, 40 (values depend on what has bee run)
# set of experiment names
experiment_shared_name = 'BDI_projection_'
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


# get number of nets needed in each admin based on simulation input coverages (assume same coverage in all scenarios, just different types of nets)
sim_info_reference = read.csv(sim_input_reference_filename)
input_itn_mass_filename = paste0(hbhi_dir, '/simulation_inputs/', sim_info_reference$ITN_filename[sim_info_reference$ScenarioName == comparison_scenarios[1]], '.csv')
input_itn_epi_filename = paste0(hbhi_dir, '/simulation_inputs/', sim_info_reference$EPI_ITN_filename[sim_info_reference$ScenarioName == comparison_scenarios[1]], '.csv')
input_itn_anc_filename = paste0(hbhi_dir, '/simulation_inputs/', sim_info_reference$ANC_ITN_filename[sim_info_reference$ScenarioName == comparison_scenarios[1]], '.csv')
cumulative_nets = get_expected_nets_each_admin_from_sim_inputs(input_itn_mass_filename=input_itn_mass_filename, input_itn_epi_filename=input_itn_epi_filename, input_itn_anc_filename=input_itn_anc_filename,
                                                               admin_pop=admin_pop, comparison_start_year=comparison_start_year, comparison_end_year=comparison_end_year,
                                                               frac_turn_1_each_year=0.0366, frac_born_each_year=0.0385)
nets_each_admin = cumulative_nets[,c('admin_name','total_nets')]



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


# overall burden with more expensive intervention relative to less expensive intervention
burden_dfs = list(pfpr_u5_df, pfpr_all_df, deaths_u5_df, deaths_all_df, clinical_cases_u5_df, clinical_cases_all_df)
burden_names = c('PfPR U5', 'PfPR all','mortality U5','mortality all', 'clinical cases U5', 'clinical cases all')
for(ii in 1:length(burden_dfs)){
  ww = sum(burden_dfs[[ii]][[net_type_names[2]]]) / sum(burden_dfs[[ii]][net_type_names[[1]]])
  print(paste0(burden_names[ii], ': ', round(ww,2), ' (', round(sum(burden_dfs[[ii]][net_type_names[[1]]]),0),' pyr versus ', round(sum(burden_dfs[[ii]][[net_type_names[2]]]),0),' PBO)')); cat('\n')
}



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
#         plot cost-effectiveness versus PfPR
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
# color-code point according to whether optimized to get PBO: must have already run all relevant scenarios in LLIN_PBO_optimization.R
color_each_type = c(rgb(0.7,0.5,1), rgb(0.3,0,0.5))


burden_metrics = c('clinical_cases_all', 'clinical_cases_u5', 'deaths_all', 'deaths_u5')
burden_names = c('clinical cases (all ages)', 'clinical cases (U5)', 'deaths (all)', 'deaths (U5)')
png(paste0(optim_results_filepath, '/relation_costEffective_versus_U5PfPR.png'), width=6, height=3*length(burden_metrics), unit='in', res=700)
par(mfrow=c(length(burden_metrics),2))

for(bb in 1:length(burden_metrics)){
  optimization_metric = burden_metrics[bb]

  if(optimization_metric == 'deaths_u5') burden_df = deaths_u5_df
  if(optimization_metric == 'deaths_all') burden_df = deaths_all_df
  if(optimization_metric == 'clinical_cases_u5') burden_df = clinical_cases_u5_df
  if(optimization_metric == 'clinical_cases_all') burden_df = clinical_cases_all_df
  
  if(file.exists(paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'))){
    local_best_states = read.csv(paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'), row.names=1)
    burden_output_states = apply(X=local_best_states, MARGIN=2, FUN=sum_total_burden, burden_df=burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
    # find the column with the best burden
    best_proposed_state_index = which.min(burden_output_states)[1]
    point_color_indices = local_best_states[,best_proposed_state_index]
    point_colors = color_each_type[point_color_indices]
  } else{
    point_colors = rep('black', length(admin_pop))
  }

  # get dataframe with each row an admin and columns containing:
  #    - admin names
  #    - number of nets in admin
  #    - pyreth-PfPR (U5) in admin
  #    - pyreth burden in admin
  #    - PBO burden in admin
  #    - burden averted by switching from pyr to PBO
  #    - burden averted per net by switching from pyr to PBO
  cost_effective_df = data.frame(admin_name = sort(admin_pop$admin_name))
  cost_effective_df = merge(cost_effective_df, nets_each_admin, by='admin_name')
  cost_effective_df = merge(cost_effective_df, pfpr_u5_df[,c('admin_name','pyr')], by='admin_name')
  colnames(cost_effective_df)[colnames(cost_effective_df)=='pyr'] = 'PfPR_U5_pyr'
  cost_effective_df = merge(cost_effective_df, pfpr_all_df[,c('admin_name','pyr')], by='admin_name')
  colnames(cost_effective_df)[colnames(cost_effective_df)=='pyr'] = 'PfPR_all_pyr'
  cost_effective_df = merge(cost_effective_df, burden_df, by='admin_name')
  cost_effective_df$burden_averted = cost_effective_df$pyr - cost_effective_df$PBO
  cost_effective_df$burden_averted_per_net = cost_effective_df$burden_averted / cost_effective_df$total_nets
  
  
  # plot relationships
  plot(cost_effective_df$PfPR_U5_pyr, cost_effective_df$pyr, xlab='PfPR (U5)', ylab=burden_names[bb], type='p', pch=20, bty='L', main=paste0(burden_names[bb],' versus PfPR (U5)'))
  plot(cost_effective_df$PfPR_U5_pyr, cost_effective_df$burden_averted_per_net, xlab='PfPR (U5)', ylab=c(burden_names[bb], ' averted per upgraded net'), type='p', col=point_colors, pch=20, bty='L', main=paste0('Burden averted versus PfPR (U5)'))
}
dev.off()





png(paste0(optim_results_filepath, '/relation_costEffective_versus_PfPR.png'), width=6*0.9, height=3*length(burden_metrics), unit='in', res=700)
par(mfrow=c(length(burden_metrics),2))

for(bb in 1:length(burden_metrics)){
  optimization_metric = burden_metrics[bb]
  
  if(optimization_metric == 'deaths_u5') burden_df = deaths_u5_df
  if(optimization_metric == 'deaths_all') burden_df = deaths_all_df
  if(optimization_metric == 'clinical_cases_u5') burden_df = clinical_cases_u5_df
  if(optimization_metric == 'clinical_cases_all') burden_df = clinical_cases_all_df
  
  
  if(file.exists(paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'))){
    local_best_states = read.csv(paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'), row.names=1)
    burden_output_states = apply(X=local_best_states, MARGIN=2, FUN=sum_total_burden, burden_df=burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
    # find the column with the best burden
    best_proposed_state_index = which.min(burden_output_states)[1]
    point_color_indices = local_best_states[,best_proposed_state_index]
    point_colors = color_each_type[point_color_indices]
  } else{
    point_colors = rep('black', length(admin_pop))
  }
  
  # get dataframe with each row an admin and columns containing:
  #    - admin names
  #    - number of nets in admin
  #    - pyreth-PfPR (U5) in admin
  #    - pyreth burden in admin
  #    - PBO burden in admin
  #    - burden averted by switching from pyr to PBO
  #    - burden averted per net by switching from pyr to PBO
  cost_effective_df = data.frame(admin_name = sort(admin_pop$admin_name))
  cost_effective_df = merge(cost_effective_df, nets_each_admin, by='admin_name')
  cost_effective_df = merge(cost_effective_df, pfpr_u5_df[,c('admin_name','pyr')], by='admin_name')
  colnames(cost_effective_df)[colnames(cost_effective_df)=='pyr'] = 'PfPR_U5_pyr'
  cost_effective_df = merge(cost_effective_df, pfpr_all_df[,c('admin_name','pyr')], by='admin_name')
  colnames(cost_effective_df)[colnames(cost_effective_df)=='pyr'] = 'PfPR_all_pyr'
  cost_effective_df = merge(cost_effective_df, burden_df, by='admin_name')
  cost_effective_df$burden_averted = cost_effective_df$pyr - cost_effective_df$PBO
  cost_effective_df$burden_averted_per_net = cost_effective_df$burden_averted / cost_effective_df$total_nets
  
  
  # plot relationships
  plot(cost_effective_df$PfPR_all_pyr, cost_effective_df$pyr, xlab='PfPR', ylab=burden_names[bb], type='p', pch=20, bty='L', main=paste0(burden_names[bb],' versus PfPR'))
  plot(cost_effective_df$PfPR_all_pyr, cost_effective_df$burden_averted_per_net, xlab='PfPR', ylab=c(burden_names[bb], ' averted per upgraded net'), type='p', col=point_colors, pch=20, bty='L', main=paste0('Burden averted versus PfPR'))
}
dev.off()






png(paste0(optim_results_filepath, '/relation_costEffective_versus_IncidenceAll.png'), width=6*0.9, height=3*length(burden_metrics), unit='in', res=700)
par(mfrow=c(length(burden_metrics),2))

for(bb in 1:length(burden_metrics)){
  optimization_metric = burden_metrics[bb]
  
  if(optimization_metric == 'deaths_u5') burden_df = deaths_u5_df
  if(optimization_metric == 'deaths_all') burden_df = deaths_all_df
  if(optimization_metric == 'clinical_cases_u5') burden_df = clinical_cases_u5_df
  if(optimization_metric == 'clinical_cases_all') burden_df = clinical_cases_all_df
  
  
  if(file.exists(paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'))){
    local_best_states = read.csv(paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'), row.names=1)
    burden_output_states = apply(X=local_best_states, MARGIN=2, FUN=sum_total_burden, burden_df=burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
    # find the column with the best burden
    best_proposed_state_index = which.min(burden_output_states)[1]
    point_color_indices = local_best_states[,best_proposed_state_index]
    point_colors = color_each_type[point_color_indices]
  } else{
    point_colors = rep('black', length(admin_pop))
  }
  
  # get dataframe with each row an admin and columns containing:
  #    - admin names
  #    - number of nets in admin
  #    - pyreth-PfPR (U5) in admin
  #    - pyreth burden in admin
  #    - PBO burden in admin
  #    - burden averted by switching from pyr to PBO
  #    - burden averted per net by switching from pyr to PBO
  cost_effective_df = data.frame(admin_name = sort(admin_pop$admin_name))
  cost_effective_df = merge(cost_effective_df, nets_each_admin, by='admin_name')
  
  cost_effective_df = merge(cost_effective_df, clinical_cases_u5_df[,c('admin_name','pyr')], by='admin_name')
  colnames(cost_effective_df)[colnames(cost_effective_df)=='pyr'] = 'cases_U5_pyr'
  cost_effective_df = merge(cost_effective_df, pop_u5_df[,c('admin_name','pyr')], by='admin_name')
  colnames(cost_effective_df)[colnames(cost_effective_df)=='pyr'] = 'pop_U5_pyr'
  cost_effective_df$incidence_u5 = cost_effective_df$cases_U5_pyr / cost_effective_df$pop_U5_pyr
  
  cost_effective_df = merge(cost_effective_df, clinical_cases_all_df[,c('admin_name','pyr')], by='admin_name')
  colnames(cost_effective_df)[colnames(cost_effective_df)=='pyr'] = 'cases_all_pyr'
  cost_effective_df = merge(cost_effective_df, pop_all_df[,c('admin_name','pyr')], by='admin_name')
  colnames(cost_effective_df)[colnames(cost_effective_df)=='pyr'] = 'pop_all_pyr'
  cost_effective_df$incidence_all = cost_effective_df$cases_all_pyr / cost_effective_df$pop_all_pyr
  
  cost_effective_df = merge(cost_effective_df, burden_df, by='admin_name')
  cost_effective_df$burden_averted = cost_effective_df$pyr - cost_effective_df$PBO
  cost_effective_df$burden_averted_per_net = cost_effective_df$burden_averted / cost_effective_df$total_nets

  
  # plot relationships
  plot(cost_effective_df$incidence_all, cost_effective_df$pyr, xlab='PfPR', ylab=burden_names[bb], type='p', pch=20, bty='L', main=paste0(burden_names[bb],' versus clinical incidence'))
  plot(cost_effective_df$incidence_all, cost_effective_df$burden_averted_per_net, xlab='PfPR', ylab=c(burden_names[bb], ' averted per upgraded net'), type='p', col=point_colors, pch=20, bty='L', main=paste0('Burden averted versus clinical incidence'))
}
dev.off()



if(FALSE){
  plot(cost_effective_df$PfPR_all_pyr, cost_effective_df$PfPR_U5_pyr, xlim=c(0,0.45), ylim=c(0,0.45), xlab='PfPR', ylab='PfPR (U5)', type='p', pch=20, bty='L')
  lines(c(0,0.45), c(0,0.45), col=rgb(0.5,0,0))
}




if(FALSE){
  for(bb in 1:length(burden_metrics)){
    optimization_metric = burden_metrics[bb]
    
    if(optimization_metric == 'deaths_u5') burden_df = deaths_u5_df
    if(optimization_metric == 'deaths_all') burden_df = deaths_all_df
    if(optimization_metric == 'clinical_cases_u5') burden_df = clinical_cases_u5_df
    if(optimization_metric == 'clinical_cases_all') burden_df = clinical_cases_all_df
    
    # baseline burden for the all-pyrethroid scenario
    pyreth_burden = sum(get_burden_each_admin(burden_df=burden_df, state_column_each_ds=rep(1,length(admin_names))))
    
    if(file.exists(paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'))){
      current_plan_burden = sum(get_burden_each_admin(burden_df=burden_df, state_column_each_ds=(current_itn_plan[[net_choice_colname]] + 1)))
      all_pbo_burden = sum(get_burden_each_admin(burden_df=burden_df, state_column_each_ds=rep(2,length(admin_names))))
      
      local_best_states = read.csv(paste0(optim_results_filepath, '/best_PBO_admins_',optimization_metric,'.csv'), row.names=1)
      burden_output_states = apply(X=local_best_states, MARGIN=2, FUN=sum_total_burden, burden_df=burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
      # find the column with the best burden
      best_proposed_state_index = which.min(burden_output_states)[1]
      optim_burden = sum(get_burden_each_admin(burden_df=burden_df, state_column_each_ds=local_best_states[,best_proposed_state_index]))

      print(optim_results_filepath)
      print(paste0('Relative reduction for ', burden_names[bb]))
      print(paste0('    current plan: ', round((1-current_plan_burden/pyreth_burden)*100,1)))
      print(paste0('    optim plan: ', round((1-optim_burden/pyreth_burden)*100,1)))
      print(paste0('    all-PBO plan: ', round((1-all_pbo_burden/pyreth_burden)*100,1)))
    } 
  }
}
