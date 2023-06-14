# compare_simOutput_interventionCoverage.R
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)


####################################################################
# User-specified information on scenarios plotted
####################################################################
source('C:/Users/mambrose/Documents/malaria-sle-hbhi/simulation/plot_results_analyses/compare_simOutput_interventionCoverage_functions.R')

# directory where scenario output is located
hbhi_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
sim_output_dir = paste0(hbhi_dir, '/simulation_output')
seed_subset = 'all'  # options: 'all', 'mortLog', 'mortLogLog'  Note: need to run subset_sim_output on future and past simulations before using anything but 'all'
if(seed_subset == 'all'){
  sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
  sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present')
} else if(seed_subset == 'mortLog'){
  sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
  sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present/simulation_subsets/mortLog')
} else if(seed_subset == 'mortLogLog'){
  sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
  sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present/simulation_subsets/mortLogLog')
}

pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')

# which experiments/scenarios should be plotted?
pyr = 64
chw_cov = 'higher'
scenario_names = c('to-present', 'BAU', 'GF', 'GF with PBO', 'GF at 80%', 'GF at 80% with PBO')
scenario_base_dirs = c(sim_past_output_dir, rep(sim_future_output_dir,5))
experiment_names = c(paste0('BDI_2010_2020_allInter'),
                     paste0('BDI_projection_', pyr,'pyr'),  # BAU
                     paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr'),  # GF
                     paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_PBO', pyr,'pyr'),  # GF with PBO nets
                     paste0('BDI_projection_GF_80Coverage_', pyr,'pyr'),  # GF at 80% coverage
                     paste0('BDI_projection_GF_80CoveragePBO_', pyr,'pyr')
                     )
scenario_filepaths = paste0(scenario_base_dirs, '/', experiment_names)
scenario_input_references = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/', c('Interventions_for_2010_2020', rep('Interventions_for_projections',5)), '.csv')
sim_end_years = c(2020, rep(2030, 5))
# set colors for each scenario
scenario_palette = c('to-present'=rgb(0,0,0), 'BAU'=rgb(0/255,120/255,0/255), 
                     'GF'=rgb(55/255,80/255,220/255),  'GF with PBO'=rgb(90/255,180/255,238/255), 
                     'GF at 80%'=rgb(170/255,51/255,119/255), 'GF at 80% with PBO'=rgb(255/255,140/255,175/255))

# set minimum and maximum years
min_year = 2012
max_year = 2030

# choose whether output will be aggregated to monthly or annual values
plot_by_month = FALSE

# which admin will be plotted?
district_subset = 'districtsAll'  # options: 'districtsAll', 'districtsIRS','districtsCommLLIN', or 'DistrictsOther' ( a manually-specified vector)
irs_districts = c('Buye', 'Gashoho', 'Kiremba','Muyinga')
comm_llin_districts = c('Giteranyi','Ngozi')
other_districts = 'Bubanza'
if(district_subset == 'districtsAll'){
  cur_admins = 'all'
} else if(district_subset == 'districtsIRS'){
  cur_admins = irs_districts
} else if(district_subset == 'districtsCommLLIN'){
  cur_admins = comm_llin_districts
} else if(district_subset == 'DistrictsOther'){
  cur_admins = other_districts
} else{
  warning('Name for the subset of districts to plot not recognized; results for all districts will be shown')
  cur_admins = 'all'
}

# which malaria burden metric will be plotted? (options: PfPR, incidence, mortality). Note: PfPR is measured by microscopy
burden_metric = 'incidence'

# which age group will be plotted? (options: U5 or all)
age_plotted = 'U5'

# fraction of indoor time protected under net - use same value as in simulation. Use this value to back-calculate the use rate from effective use rate.
indoor_protection_fraction = 0.75

####################################################################
####################################################################

gg = plot_simulation_intervention_output(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                               plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                               burden_metric=burden_metric, age_plotted=age_plotted, 
                                               pyr=pyr, chw_cov=chw_cov,
                                               scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, scenario_palette=scenario_palette, 
                                               indoor_protection_fraction=indoor_protection_fraction)


# ####################################################################
# # functions for reading and distilling in simulation output
# ####################################################################
# 
# # ----- malaria burden ----- #
# # function to subset simulation output to appropriate admin and time period, and calculate monthly mean, min, max burdens across all runs
# get_burden_timeseries_exp = function(exp_filepath, exp_name, cur_admins, pop_sizes, min_year, max_year, burden_colname, plot_by_month=TRUE){
#   # read in simulation information, subset to appropriate years
#   cur_sim_output = fread(paste0(exp_filepath, '/malariaBurden_withAdjustments.csv'))
#   cur_sim_output = cur_sim_output[intersect(which(cur_sim_output$year >= min_year), which(cur_sim_output$year <= max_year)),]
#   # subset to appropriate admins
#   cur_sim_output = cur_sim_output[cur_sim_output$admin_name %in% cur_admins,]  
#   
#   # merge to get real-world population sizes in each admin
#   cur_sim_output = merge(cur_sim_output, pop_sizes, by='admin_name')
#   
#   # get simulation population denominator
#   if(age_plotted == 'U5'){
#     cur_sim_output$population = cur_sim_output$Pop_U5
#   } else{
#     cur_sim_output$population = cur_sim_output$Statistical_Population
#   }
#   # get the real-world population size in each admin
#   if(age_plotted == 'U5'){
#     cur_sim_output$true_population = cur_sim_output$pop_size * cur_sim_output$Pop_U5 / cur_sim_output$Statistical_Population
#   } else{
#     cur_sim_output$true_population = cur_sim_output$pop_size
#   }
#     
#   
#   # calculate average over included admins
#   if(grepl('PfPR', burden_colname)){
#     # get total number of positives (PfPR * true population for all included admins), then divide by total population size of all admins
#     cur_sim_output$positives = cur_sim_output[[burden_colname]] * cur_sim_output$true_population
#     cur_sim_output_agg_admin = as.data.frame(cur_sim_output) %>% dplyr::select(month, year, date, admin_name, positives, true_population, Run_Number) %>%
#       group_by(month, year, date, Run_Number) %>%  # take sum of positives and population across admins
#       summarise(total_positives = sum(positives),
#                 total_population = sum(true_population))
#     cur_sim_output_agg_admin$burden = cur_sim_output_agg_admin$total_positives / cur_sim_output_agg_admin$total_population
#     if(!plot_by_month){
#       cur_sim_output_agg_admin = cur_sim_output_agg_admin %>% group_by(year, Run_Number) %>% # take average PfPR across months
#         summarise(burden = mean(burden))
#     }
#     
#   } else{
#     # rescale case (or death) numbers to number present in full admin (with true population instead of simulated population size)
#     cur_sim_output$true_burden = cur_sim_output[[burden_colname]] * cur_sim_output$true_population / cur_sim_output$population
#     # take sum of the number of cases (or deaths) and population sizes acrsoss all included admins
#     select_col_names = c('true_burden', 'month', 'year', 'date', 'true_population', 'Run_Number')
#     cur_sim_output_agg_admin = as.data.frame(cur_sim_output) %>% dplyr::select(match(select_col_names, names(.))) %>%
#       group_by(month, year, date, Run_Number) %>%
#       summarise_all(sum)
#     cur_sim_output_agg_admin$burden = cur_sim_output_agg_admin$true_burden / cur_sim_output_agg_admin$true_population * 1000
#     if(!plot_by_month){
#       cur_sim_output_agg_admin = cur_sim_output_agg_admin %>% group_by(year, Run_Number) %>% # take average PfPR across months
#         summarise(burden = sum(burden))
#     }
#   }
#   
#   # take average, max, and min burdens across simulation seeds
#   if(plot_by_month){
#     cur_sim_output_agg = as.data.frame(cur_sim_output_agg_admin) %>% dplyr::select(month, year, date, burden) %>%
#       group_by(month, year, date) %>%
#       summarise(mean_burden = mean(burden),
#                 max_burden = max(burden),
#                 min_burden = min(burden))
#   } else{
#     cur_sim_output_agg = as.data.frame(cur_sim_output_agg_admin) %>% dplyr::select(year, burden) %>%
#       group_by(year) %>%
#       summarise(mean_burden = mean(burden),
#                 max_burden = max(burden),
#                 min_burden = min(burden))
#   }
# 
#   cur_sim_output_agg$scenario = exp_name
#   return(cur_sim_output_agg)
# }
# 
# 
# 
# 
# 
# # ----- LLIN and IRS intervention coverage ----- #
# # plot ITN use rates through time in entire population (always shows all-age, even when burden plot shows U5)
# # function to subset simulation output to appropriate admin(s) and time period, and calculate monthly net usage and net distribution across all runs
# get_net_use_timeseries_exp = function(exp_filepath, exp_name, cur_admins, pop_sizes, min_year, max_year, indoor_protection_fraction, plot_by_month=TRUE){
#   # read in simulation information, merge to single dataframe, subset to appropriate years
#   net_use_all = fread(paste0(exp_filepath, '/MonthlyUsageLLIN.csv'))
#   net_dist_all = fread(paste0(exp_filepath, '/monthly_Event_Count.csv'))
#   net_dist_all = net_dist_all[,c('admin_name', 'date', 'Run_Number', 'Bednet_Got_New_One', 'Received_IRS')]
#   net_use_all = merge(net_use_all, net_dist_all, by=c('admin_name', 'date', 'Run_Number'))
#   net_use_all$date = as.Date(net_use_all$date)
#   net_use_all$year = lubridate::year(net_use_all$date)
#   net_use_all = net_use_all[intersect(which(net_use_all$year >= min_year), which(net_use_all$year <= max_year)),]
#   
#   # subset to appropriate admins
#   net_use_all = net_use_all[net_use_all$admin_name %in% cur_admins,]  
#   colnames(net_use_all) = gsub(' ','.',colnames(net_use_all))
#   
#   # rescale numbers of nets received, nets used, and IRS received to the true population in each admin, rather than the simulated population size
#   net_use_all = merge(net_use_all, pop_sizes, by='admin_name')
#   colnames(net_use_all)[colnames(net_use_all) == 'pop_size'] = 'true_population'
#   net_use_all$Bednet_Using = net_use_all$Bednet_Using * net_use_all$true_population / net_use_all$Statistical.Population
#   net_use_all$Bednet_Got_New_One = net_use_all$Bednet_Got_New_One * net_use_all$true_population / net_use_all$Statistical.Population
#   net_use_all$Received_IRS = net_use_all$Received_IRS * net_use_all$true_population / net_use_all$Statistical.Population
#   
#   # get sum of numbers across all included admins (keeping runs and months separate)
#   net_use_sums = net_use_all %>% dplyr::select(year, date, Run_Number, Bednet_Using, Bednet_Got_New_One, Received_IRS, true_population) %>% group_by(date, year, Run_Number) %>%
#     summarise_all(sum)
#   net_use_sums$coverage = net_use_sums$Bednet_Using / net_use_sums$true_population / indoor_protection_fraction
#   net_use_sums$new_net_per_cap = net_use_sums$Bednet_Got_New_One / net_use_sums$true_population
#   net_use_sums$irs_per_cap = net_use_sums$Received_IRS / net_use_sums$true_population
#   
#   # get average coverage accross months in a year for the annual report
#   if(!plot_by_month){
#     net_use_sums = net_use_sums %>% group_by(year, Run_Number) %>% # take average across months
#       summarise(coverage = mean(coverage),
#                 new_net_per_cap = sum(new_net_per_cap),
#                 irs_per_cap = sum(irs_per_cap))
#   }
#   
#   # take average, max, and min burdens across simulation seeds
#   if(plot_by_month){
#     net_use_agg = as.data.frame(net_use_sums) %>% dplyr::select(year, date, coverage, new_net_per_cap, irs_per_cap) %>%
#       group_by(year, date) %>%
#       summarise(mean_coverage = mean(coverage),
#                 max_coverage = max(coverage),
#                 min_coverage = min(coverage),
#                 new_net_per_cap = mean(new_net_per_cap),
#                 irs_per_cap = mean(irs_per_cap))
#   } else{
#     net_use_agg = as.data.frame(net_use_sums) %>% dplyr::select(year, coverage, new_net_per_cap, irs_per_cap) %>%
#       group_by(year) %>%
#       summarise(mean_coverage = mean(coverage),
#                 max_coverage = max(coverage),
#                 min_coverage = min(coverage),
#                 new_net_per_cap = mean(new_net_per_cap),
#                 irs_per_cap = mean(irs_per_cap))
#   }
#   
#   net_use_agg$scenario = exp_name
#   return(net_use_agg)
# }
# 
# 
# # ----- CM intervention coverage ----- #
# get_cm_timeseries_exp = function(cm_filepath, pop_sizes, end_year, exp_name, cur_admins, min_year, plot_by_month=TRUE){
#   
#   cm_input = read.csv(cm_filepath)
#   
#   # CM is sometimes repeated for several years but only listed once; change to repeate the appropriate number of times
#   cm_input$years_repeated = cm_input$duration/365
#   if(any(cm_input$years_repeated>1)){
#     cur_cm_years = unique(cm_input$year)
#     for(yy in cur_cm_years){
#       # get first instance of this year
#       cur_year = cm_input[cm_input$year==yy,]
#       if(cur_year$years_repeated[1]>1){
#         for(rr in 1:(cur_year$years_repeated[1] - 1)){
#           temp_year = cur_year
#           temp_year$year = cur_year$year + rr
#           temp_year$simday = cur_year$simday + rr*365
#           cm_input = rbind(cm_input, temp_year)
#         }
#       }
#     }
#   }
#   if(any(cm_input$duration==-1) & (max(cm_input$year)<end_year)){
#     cm_repeated = cm_input[cm_input$duration == -1,]
#     for(rr in 1:(end_year - cm_repeated$year[1])){
#       temp_year = cm_repeated
#       temp_year$year = cm_repeated$year + rr
#       temp_year$simday = cm_repeated$simday + rr*365
#       cm_input = rbind(cm_input, temp_year)
#     }
#   }
#   # if there are multiple values in a single year (for a single DS/LGA), take the mean of those values
#   cm_input <- cm_input %>% group_by(year, admin_name, seed) %>%
#     summarise_all(mean) %>% ungroup()
#   
#   cm_input = cm_input[intersect(which(cm_input$year >= min_year), which(cm_input$year <= end_year)),]
#   
#   
#   # get population-weighted CM coverage across admins
#   cm_input = merge(cm_input, pop_sizes, by='admin_name')
#   cm_input$multiplied_U5_cm = cm_input$U5_coverage * cm_input$pop_size
#   
#   # get sum of population sizes and multiplied CM coverage across included admins
#   cm_input_agg_admin <- cm_input %>% dplyr::select(year, seed, multiplied_U5_cm, pop_size) %>% group_by(year, seed) %>%
#     summarise_all(sum) %>% ungroup()
#   # get population-weighted U5 coverage across all included admin by dividing by su  of population sizes
#   cm_input_agg_admin$U5_coverage = cm_input_agg_admin$multiplied_U5_cm / cm_input_agg_admin$pop_size
#   
#   
#   # take average, max, and min burdens across simulation seeds
#   if(plot_by_month){
#     # subdivide year values and add dates
#     # date dataframe
#     included_years = unique(cm_input_agg_admin$year)
#     all_months = as.Date(paste0(rep(included_years, each=12),'-',c('01','02','03','04','05','06','07','08','09','10','11','12'), '-01' ))
#     date_df = data.frame(year=rep(included_years, each=12), date=all_months)
#     cm_input_agg_admin_monthly = merge(cm_input_agg_admin, date_df, by='year', all.x=TRUE, all.y=TRUE)
#     cm_agg = as.data.frame(cm_input_agg_admin_monthly) %>% dplyr::select(year, date, U5_coverage) %>%
#       group_by(year, date) %>%
#       summarise(mean_coverage = mean(U5_coverage),
#                 max_coverage = max(U5_coverage),
#                 min_coverage = min(U5_coverage))
#   } else{
#     cm_agg = as.data.frame(cm_input_agg_admin) %>% dplyr::select(year, U5_coverage) %>%
#       group_by(year) %>%
#       summarise(mean_coverage = mean(U5_coverage),
#                 max_coverage = max(U5_coverage),
#                 min_coverage = min(U5_coverage))
#   }
#   
#   cm_agg$scenario = exp_name
#   return(cm_agg)
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####################################################################
# # combine simulation output from multiple scenarios
# ####################################################################
# 
# pop_sizes = read.csv(pop_filepath)
# pop_sizes = pop_sizes[,c('admin_name','pop_size')]
# # if we include all admins, get list of names from population size dataframe
# if(cur_admins[1] == 'all'){
#   cur_admins = unique(pop_sizes$admin_name)
# }
# 
# # create output directories
# if(!dir.exists(paste0(sim_future_output_dir, '/_plots'))) dir.create(paste0(sim_future_output_dir, '/_plots'))
# if(!dir.exists(paste0(sim_future_output_dir, '/_plots/timeseries_dfs'))) dir.create(paste0(sim_future_output_dir, '/_plots/timeseries_dfs'))
# if(plot_by_month){
#   time_string = 'monthly'
# } else time_string = 'annual'
# 
# 
# # ----- malaria burden ----- #
# 
# # Get output column name for specified burden metric
# # Note: need to divide by pop size and multiply by 1000 if not PfPR
# burden_colname = NA
# if(burden_metric == 'PfPR'){
#   if(age_plotted == 'U5'){
#     burden_colname = 'PfPR_U5'
#   } else if(age_plotted == 'all'){
#     burden_colname = 'PfPR_MiP_adjusted'
#   }
# } else if(burden_metric == 'incidence'){
#   if(age_plotted == 'U5'){
#     burden_colname = 'New_clinical_cases_U5'
#   } else if(age_plotted == 'all'){
#     burden_colname = 'New_Clinical_Cases'
#   }
# }  else if(burden_metric == 'mortality'){
#   if(age_plotted == 'U5'){
#     burden_colname = 'total_mortality_U5_1'
#   } else if(age_plotted == 'all'){
#     burden_colname = 'total_mortality_1'
#   }
# } 
# if(is.na(burden_colname)){
#   warning('PROBLEM DETECTED: name of burden metric or age group not currently supported')
# }
# 
# 
# # check whether burden output already exists for this comparison
# burden_df_filepath = paste0(sim_future_output_dir, '/_plots/timeseries_dfs/df_burden_',time_string,'Timeseries_', burden_metric, '_', age_plotted, '_pyr', pyr, '_', chw_cov, 'CHW_',district_subset,'.csv')
# if(file.exists(burden_df_filepath)){
#   burden_df = read.csv(burden_df_filepath)
# } else{
#   # iterate through scenarios, storing relevant output
#   burden_df = data.frame()
#   for(ee in 1:length(scenario_filepaths)){
#     cur_sim_output_agg = get_burden_timeseries_exp(exp_filepath = scenario_filepaths[ee],
#                                                    exp_name = scenario_names[ee], 
#                                                    cur_admins=cur_admins, pop_sizes = pop_sizes, min_year=min_year, max_year=max_year, burden_colname=burden_colname, plot_by_month=plot_by_month)
#     if(nrow(burden_df)==0){
#       burden_df = cur_sim_output_agg
#     } else{
#       burden_df = rbind(burden_df, cur_sim_output_agg)
#     }
#   }
#   
#   # add the final 'to-present' row to all future simulations for a continuous plot
#   to_present_df = burden_df[burden_df$scenario == 'to-present',]
#   if(plot_by_month){
#     final_to_present_row = to_present_df[as.Date(to_present_df$date) == max(as.Date(to_present_df$date)),]
#     for(ss in 2:length(scenario_names)){
#       final_to_present_row$scenario = scenario_names[ss]
#       burden_df = rbind(burden_df, final_to_present_row)
#     }
#   } else{
#     final_to_present_row = to_present_df[to_present_df$year == max(to_present_df$year),]
#     for(ss in 2:length(scenario_names)){
#       final_to_present_row$scenario = scenario_names[ss]
#       burden_df = rbind(burden_df, final_to_present_row)
#     }
#   }
#   write.csv(burden_df, burden_df_filepath, row.names=FALSE)
# }
# 
# 
# 
# 
# # ----- LLIN and IRS intervention coverage ----- #
# 
# # check whether LLIN/IRS output already exists for this comparison
# llin_df_filepath = paste0(sim_future_output_dir, '/_plots/timeseries_dfs/df_llin_irs_',time_string,'Timeseries', '_pyr', pyr, '_', chw_cov, 'CHW_',district_subset,'.csv')
# if(file.exists(llin_df_filepath)){
#   net_use_df = read.csv(llin_df_filepath)
# } else{
#   # iterate through scenarios, storing relevant output
#   net_use_df = data.frame()
#   for(ee in 1:length(scenario_filepaths)){
#     cur_net_agg = get_net_use_timeseries_exp(exp_filepath = scenario_filepaths[ee],
#                                              exp_name = scenario_names[ee], 
#                                              cur_admins=cur_admins, pop_sizes=pop_sizes, min_year=min_year, max_year=max_year, indoor_protection_fraction=indoor_protection_fraction, plot_by_month=plot_by_month)
#     if(nrow(net_use_df)==0){
#       net_use_df = cur_net_agg
#     } else{
#       net_use_df = rbind(net_use_df, cur_net_agg)
#     }
#   }
#   
#   # first, remove the final 'to-present' month or year - it should was overwritten in the pick-up from burn-in
#   # then, add the final 'to-present' row to all future simulations for a continuous plot
#   if(plot_by_month){
#     # remove excess month from to-present simulation
#     max_to_present_date = max(net_use_df$date[net_use_df$scenario == 'to-present'])
#     row_to_remove = intersect(which(net_use_df$scenario == 'to-present'), which(net_use_df$date == max_to_present_date))
#     net_use_df = net_use_df[-row_to_remove,]
#     
#     # join past and future simulation trajectories
#     to_present_df = net_use_df[net_use_df$scenario == 'to-present',]
#     final_to_present_row = to_present_df[as.Date(to_present_df$date) == max(as.Date(to_present_df$date)),]
#     for(ss in 2:length(scenario_names)){
#       final_to_present_row$scenario = scenario_names[ss]
#       net_use_df = rbind(net_use_df, final_to_present_row)
#     }
#   } else{
#     # remove excess year from to-present simulation
#     max_to_present_date = max(net_use_df$year[net_use_df$scenario == 'to-present'])
#     row_to_remove = intersect(which(net_use_df$scenario == 'to-present'), which(net_use_df$year == max_to_present_date))
#     net_use_df = net_use_df[-row_to_remove,]
#     
#     # join past and future simulation trajectories
#     to_present_df = net_use_df[net_use_df$scenario == 'to-present',]
#     final_to_present_row = to_present_df[to_present_df$year == max(to_present_df$year),]
#     for(ss in 2:length(scenario_names)){
#       final_to_present_row$scenario = scenario_names[ss]
#       net_use_df = rbind(net_use_df, final_to_present_row)
#     }
#   }
#   write.csv(net_use_df, llin_df_filepath, row.names=FALSE)
# }
#   
# 
# 
# 
# # ----- Case management ----- #
# 
# # check whether LLIN/IRS output already exists for this comparison
# cm_df_filepath = paste0(sim_future_output_dir, '/_plots/timeseries_dfs/df_cm_',time_string,'Timeseries', '_pyr', pyr, '_', chw_cov, 'CHW_',district_subset,'.csv')
# if(file.exists(cm_df_filepath)){
#   cm_df = read.csv(cm_df_filepath)
# } else{
#   # iterate through scenarios, storing input CM coverages
#   cm_df = data.frame()
#   for(ee in 1:length(scenario_filepaths)){
#     intervention_csv_filepath = scenario_input_references[ee]
#     intervention_file_info = read.csv(intervention_csv_filepath)
#     experiment_intervention_name = experiment_names[ee]
#     end_year = sim_end_years[ee]
#     cur_int_row = which(intervention_file_info$ScenarioName == experiment_intervention_name)
#     # read in intervention files
#     cm_filepath = paste0(hbhi_dir, '/simulation_inputs/', intervention_file_info$CM_filename[cur_int_row], '.csv')
#     
#     cur_cm_agg = get_cm_timeseries_exp(cm_filepath=cm_filepath, pop_sizes=pop_sizes, end_year=end_year, exp_name = scenario_names[ee], 
#                                        cur_admins=cur_admins, min_year=min_year, plot_by_month=plot_by_month)
#     
#     if(nrow(cm_df)==0){
#       cm_df = cur_cm_agg
#     } else{
#       cm_df = rbind(cm_df, cur_cm_agg)
#     }
#   }
#   
#   # add the final 'to-present' row to all future simulations for a continuous plot
#   if(plot_by_month){
#     # join past and future simulation trajectories
#     to_present_df = cm_df[cm_df$scenario == 'to-present',]
#     final_to_present_row = to_present_df[as.Date(to_present_df$date) == max(as.Date(to_present_df$date)),]
#     for(ss in 2:length(scenario_names)){
#       final_to_present_row$scenario = scenario_names[ss]
#       cm_df = rbind(cm_df, final_to_present_row)
#     }
#   } else{
#     # join past and future simulation trajectories
#     to_present_df = cm_df[cm_df$scenario == 'to-present',]
#     final_to_present_row = to_present_df[to_present_df$year == max(to_present_df$year),]
#     for(ss in 2:length(scenario_names)){
#       final_to_present_row$scenario = scenario_names[ss]
#       cm_df = rbind(cm_df, final_to_present_row)
#     }
#   }
#   write.csv(cm_df, cm_df_filepath, row.names=FALSE)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# ####################################################################
# # create scenario-comparison plots
# ####################################################################
# 
# 
# # ----- malaria burden ----- #
# 
# if(plot_by_month){
#   g_burden = ggplot(burden_df, aes(x=as.Date(date), y=mean_burden, color=scenario)) +
#     geom_ribbon(aes(ymin=min_burden, ymax=max_burden, fill=scenario), alpha=0.1, color=NA)+
#     scale_fill_manual(values = scenario_palette) + 
#     geom_line(size=1) + 
#     scale_color_manual(values = scenario_palette) + 
#     xlab('date') + 
#     ylab(paste0(burden_metric, ' - ', age_plotted)) + 
#     theme_classic()+ 
#     theme(legend.position = "none")
# } else{
#   g_burden = ggplot(burden_df, aes(x=year, y=mean_burden, color=scenario)) +
#     geom_ribbon(aes(ymin=min_burden, ymax=max_burden, fill=scenario), alpha=0.1, color=NA)+
#     scale_fill_manual(values = scenario_palette) + 
#     geom_line(size=1) + 
#     scale_color_manual(values = scenario_palette) + 
#     xlab('year') + 
#     ylab(paste0(burden_metric, ' - ', age_plotted)) + 
#     theme_classic()+ 
#     theme(legend.position = "none")
# }
# 
# 
# 
# # ----- LLIN use and distribution ----- #
# 
# # plot net use through time
# if(plot_by_month){
#   g_net_use = ggplot(net_use_df, aes(x=as.Date(date), y=mean_coverage, color=scenario)) +
#     geom_ribbon(aes(ymin=min_coverage, ymax=max_coverage, fill=scenario), alpha=0.1, color=NA)+
#     scale_fill_manual(values = scenario_palette) + 
#     geom_line(size=1) + 
#     scale_color_manual(values = scenario_palette) + 
#     xlab('date') + 
#     ylab(paste0('LLIN use (all ages)')) + 
#     theme_classic()
# } else{
#   g_net_use = ggplot(net_use_df, aes(x=year, y=mean_coverage, color=scenario)) +
#     geom_ribbon(aes(ymin=min_coverage, ymax=max_coverage, fill=scenario), alpha=0.1, color=NA)+
#     scale_fill_manual(values = scenario_palette) + 
#     geom_line(size=1) + 
#     scale_color_manual(values = scenario_palette) + 
#     geom_hline(yintercept=0.22, alpha=0.1)+
#     geom_hline(yintercept=0.39, alpha=0.1)+
#     xlab('year') + 
#     ylab(paste0('LLIN use (all ages)')) + 
#     theme_classic()
# }
# 
# # plot net distribution numbers through time (how many nets distributed in each month or year per person?)
# if(plot_by_month){
#   g_net_dist = ggplot(net_use_df, aes(x=as.Date(date), y=new_net_per_cap, color=scenario)) +
#     geom_point(size=1) + 
#     scale_color_manual(values = scenario_palette) + 
#     xlab('date') + 
#     ylab(paste0('LLINs distributed per person')) + 
#     theme_classic()+ 
#     theme(legend.position = "none")
# } else{
#   g_net_dist = ggplot(net_use_df, aes(x=year, y=new_net_per_cap, color=scenario)) +
#     geom_point(size=2) + 
#     geom_line(alpha=0.2, size=2) +
#     scale_color_manual(values = scenario_palette) + 
#     xlab('year') + 
#     ylab(paste0('LLINs distributed per person')) + 
#     theme_classic()+ 
#     theme(legend.position = "none")
# }
# 
# 
# 
# # ----- IRS ----- #
# 
# if(plot_by_month){
#   g_irs = ggplot(net_use_df, aes(x=as.Date(date), y=irs_per_cap, color=scenario)) +
#     geom_point(size=1) + 
#     scale_color_manual(values = scenario_palette) + 
#     xlab('date') + 
#     ylab(paste0('IRS rounds per person')) + 
#     theme_classic()+ 
#     theme(legend.position = "none")
# } else{
#   g_irs = ggplot(net_use_df, aes(x=year, y=irs_per_cap, color=scenario)) +
#     geom_point(size=2) + 
#     geom_line(alpha=0.2, size=2) +
#     scale_color_manual(values = scenario_palette) + 
#     xlab('year') + 
#     ylab(paste0('IRS per person')) + 
#     theme_classic()+ 
#     theme(legend.position = "none")
# }
# 
# 
# 
# # ----- Case management ----- #
# 
# if(plot_by_month){
#   g_cm = ggplot(cm_df, aes(x=as.Date(date), y=mean_coverage, color=scenario)) +
#     geom_ribbon(aes(ymin=min_coverage, ymax=max_coverage, fill=scenario), alpha=0.1, color=NA)+
#     scale_fill_manual(values = scenario_palette) + 
#     geom_line(size=1) + 
#     scale_color_manual(values = scenario_palette) + 
#     xlab('date') + 
#     ylab(paste0('Effective treatment rate (U5)')) + 
#     theme_classic()+ 
#     theme(legend.position = "none")
# } else{
#   g_cm = ggplot(cm_df, aes(x=year, y=mean_coverage, color=scenario)) +
#     geom_ribbon(aes(ymin=min_coverage, ymax=max_coverage, fill=scenario), alpha=0.1, color=NA)+
#     scale_fill_manual(values = scenario_palette) + 
#     geom_line(size=1) + 
#     scale_color_manual(values = scenario_palette) + 
#     geom_hline(yintercept=0.22, alpha=0.1)+
#     geom_hline(yintercept=0.39, alpha=0.1)+
#     xlab('year') + 
#     ylab(paste0('Effective treatment rate (U5)')) + 
#     theme_classic()+ 
#     theme(legend.position = "none")
# }
# 
# 
# 
# 
# # ----- combine burden and intervention plots ----- #
# 
# # # grid.arrange(g_burden, g_net_use, g_net_dist, ncol=1, nrow=3)
# # plot_grid(g_burden, g_net_use, g_cm, ncol=1, nrow=3, align='v', axis='lr')
# # ggsave(paste0(sim_future_output_dir, '/_plots/',time_string,'Timeseries_', burden_metric, '_', age_plotted, '_versusInterventions_pyr', pyr, '_', chw_cov, 'CHW_',district_subset,'.png'), dpi=600, width=7, height=5.5, units='in')
# 
# 
# # also include plot with IRS rounds per person (when there are two rounds in the same buildinging, counts it twice)
# plot_grid(g_burden, g_net_use, g_cm, g_irs, ncol=1, nrow=4, align='v', axis='lr')
# ggsave(paste0(sim_future_output_dir, '/_plots/',time_string,'Timeseries_', burden_metric, '_', age_plotted, '_versusInterventions_pyr', pyr, '_', chw_cov, 'CHW_',district_subset,'_withIRS.png'), dpi=600, width=7, height=7.4, units='in')
# 
# 
# 
# 
# 
# 
# 
