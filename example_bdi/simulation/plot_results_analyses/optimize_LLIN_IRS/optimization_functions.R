#################################################################################################
# High burden, high impact 
# smc_optimization_functions.R
# Monique Ambrose
# September 2020
#
# 
# Functions used to find the SMC age and cycle option in each DS that maximizes impact within budget constraints
#   see main script smc_age_cycles_optimization.R
#
#################################################################################################


library(data.table)
library(tidyverse)
library(lubridate)
library(rgdal)
library(raster)
library(RColorBrewer)
library(lubridate)


# function that returns dataframe where each row is DS and columns contain U5 and all-age total number of cases, total number of deaths, incidence, death rate, and average pfpr within the time period in each admin, averaged over all seeds
get_total_burden = function(sim_output_filepath, experiment_name, admin_pop, comparison_start_year, comparison_end_year, cur_admins='all', overwrite_files=FALSE){
  output_filename = paste0(sim_output_filepath, '/', experiment_name, '/totalBurden_', comparison_start_year,'_', comparison_end_year,'_', cur_admins,'.csv')
  if(file.exists(output_filename) & !overwrite_files){
    burden_means = read.csv(output_filename)
  } else{
    burden_df = fread(paste0(sim_output_filepath, '/', experiment_name, '/malariaBurden_withAdjustments.csv'))
    # subset to appropriate time period
    burden_df = burden_df[intersect(which(burden_df$year >= comparison_start_year), which(burden_df$year <= comparison_end_year)),]
    if(!(cur_admins[1] == 'all')){
      # subset to appropriate admins
      burden_df = burden_df[burden_df$admin_name %in% cur_admins,] 
    }
    
    # PfPR
    pfpr_u5_means = burden_df[,c('admin_name', 'PfPR_U5')] %>% dplyr::group_by(admin_name) %>% dplyr::summarise_all(mean) %>% dplyr::ungroup()
    pfpr_all_means = burden_df[,c('admin_name', 'PfPR_MiP_adjusted')] %>% dplyr::group_by(admin_name) %>% dplyr::summarise_all(mean) %>% dplyr::ungroup()
    pfpr_means = merge(pfpr_u5_means, pfpr_all_means, by='admin_name')
    colnames(pfpr_means)[which(colnames(pfpr_means) == 'PfPR_U5')] = 'pfpr_u5'
    colnames(pfpr_means)[which(colnames(pfpr_means) == 'PfPR_MiP_adjusted')] = 'pfpr_all'
    
    
    # mortality
    # divide mortality by total population size in simulation; will later multiply by true population of DS
    burden_df$mortality_pp_u5 = (burden_df$total_mortality_U5_1*1 + burden_df$total_mortality_U5_2*1) / 2 / burden_df$Statistical_Population  # weighted average of mortality estimates (1/2 mort_1, 1/2 mort_2)
    burden_df$mortality_pp_all = (burden_df$total_mortality_1*1 + burden_df$total_mortality_2*1) / 2 / burden_df$Statistical_Population  # weighted average of mortality estimates (1/2 mort_1, 1/2 mort_2)
    mortality_sums = burden_df[,c('admin_name', 'Run_Number', 'mortality_pp_u5', 'mortality_pp_all')] %>% dplyr::group_by(admin_name, Run_Number) %>% dplyr::summarise_all(sum) %>% dplyr::ungroup()
    mortality_sums = mortality_sums[,c('admin_name', 'mortality_pp_u5', 'mortality_pp_all')] %>% dplyr::group_by(admin_name) %>% dplyr::summarise_all(mean) %>% dplyr::ungroup()
    # multiply by admin population to get total number of deaths
    mortality_sums = merge(mortality_sums, admin_pop, by='admin_name')
    mortality_sums$deaths_u5 = mortality_sums$mortality_pp_u5 * mortality_sums$pop_size
    mortality_sums$deaths_all = mortality_sums$mortality_pp_all * mortality_sums$pop_size
    
    # mortality rate  (number of deaths in a year in each admin divided by the U5 or all-age population size times 1000)
    # take sum of deaths within each year
    burden_df_annual =  burden_df %>% dplyr::group_by(admin_name, Run_Number, year) %>% 
      dplyr::summarise(total_mortality_all_1 = sum(total_mortality_1),
                       total_mortality_all_2 = sum(total_mortality_2),
                       New_clinical_cases_all = sum(New_Clinical_Cases),
                       Pop_all = mean(Statistical_Population),
                       total_mortality_U5_1 = sum(total_mortality_U5_1),
                       total_mortality_U5_2 = sum(total_mortality_U5_2),
                       New_clinical_cases_U5 = sum(New_clinical_cases_U5),
                       Pop_U5 = mean(Pop_U5)
      ) %>% 
      dplyr::ungroup()
    burden_df_annual$mortality_rate_u5 = (burden_df_annual$total_mortality_U5_1*1 + burden_df_annual$total_mortality_U5_2*1) / 2 / burden_df_annual$Pop_U5 * 1000  # weighted average of mortality estimates (1/2 mort_1, 1/2 mort_2)
    burden_df_annual$mortality_rate_all = (burden_df_annual$total_mortality_all_1*1 + burden_df_annual$total_mortality_all_2*1) / 2 / burden_df_annual$Pop_all * 1000  # weighted average of mortality estimates (1/2 mort_1, 1/2 mort_2)
    # remove any NA rows
    burden_df_annual = burden_df_annual[!is.na(burden_df_annual$mortality_rate_u5),]
    burden_df_annual = burden_df_annual[!is.na(burden_df_annual$mortality_rate_all),]
    # get average annual rate across all included years
    mortality_rate_means = burden_df_annual[,c('admin_name', 'mortality_rate_u5', 'mortality_rate_all')] %>% dplyr::group_by(admin_name) %>% 
      dplyr::summarise(mortality_rate_u5 = mean(mortality_rate_u5),
                       mortality_rate_all = mean(mortality_rate_all)) %>% 
      dplyr::ungroup()
    
    
    
    # clinical cases
    # divide number of clinical cases by total population size in simulation; will later multiply by true population of DS to get estimated number of clinical cases in that DS
    burden_df$cases_pp_u5 = burden_df$New_clinical_cases_U5 / burden_df$Statistical_Population
    burden_df$cases_pp_all = burden_df$New_Clinical_Cases / burden_df$Statistical_Population
    case_sums = burden_df[,c('admin_name', 'Run_Number', 'cases_pp_u5', 'cases_pp_all')] %>% dplyr::group_by(admin_name, Run_Number) %>% dplyr::summarise_all(sum) %>% dplyr::ungroup()
    case_sums = case_sums[,c('admin_name', 'cases_pp_u5', 'cases_pp_all')] %>% dplyr::group_by(admin_name) %>% dplyr::summarise_all(mean) %>% dplyr::ungroup()
    # multiply by admin population to get total number of cases
    case_sums = merge(case_sums, admin_pop, by='admin_name')
    case_sums$clinical_cases_u5 = case_sums$cases_pp_u5 * case_sums$pop_size
    case_sums$clinical_cases_all = case_sums$cases_pp_all * case_sums$pop_size
    
    # incidence (number of cases in a year in each admin divided by the U5 or all-age population size times 1000)
    burden_df_annual$incidence_u5 = burden_df_annual$New_clinical_cases_U5 / burden_df_annual$Pop_U5 * 1000
    burden_df_annual$incidence_all = burden_df_annual$New_clinical_cases_all / burden_df_annual$Pop_all * 1000
    # remove any NA rows
    burden_df_annual = burden_df_annual[!is.na(burden_df_annual$incidence_u5),]
    burden_df_annual = burden_df_annual[!is.na(burden_df_annual$incidence_all),]
    # get average annual rate across all included years
    incidence_means = burden_df_annual[,c('admin_name', 'incidence_u5', 'incidence_all')] %>% dplyr::group_by(admin_name) %>% 
      dplyr::summarise(incidence_u5 = mean(incidence_u5),
                       incidence_all = mean(incidence_all)) %>% 
      dplyr::ungroup()
    
    
    
    
    # fraction of population U5
    burden_df$frac_u5 = burden_df$Pop_U5 / burden_df$Statistical_Population
    frac_u5 = burden_df[,c('admin_name', 'frac_u5')] %>% dplyr::group_by(admin_name) %>% dplyr::summarise_all(mean) %>% dplyr::ungroup()
    
    burden_means = merge(pfpr_means, mortality_sums, by='admin_name')
    burden_means = merge(burden_means, case_sums, by=c('admin_name', 'pop_size'))
    burden_means = merge(burden_means, mortality_rate_means, by=c('admin_name'))
    burden_means = merge(burden_means, incidence_means, by=c('admin_name'))
    burden_means = merge(burden_means, frac_u5, by=c('admin_name'))
    burden_means$pop_size_u5 = burden_means$pop_size * burden_means$frac_u5
    
    write.csv(burden_means, output_filename, row.names=FALSE)
  }
  return(burden_means)
}




# function that returns dataframe where each row is DS and column contains total number of cases, total number of deaths, or average pfpr within the time period
get_total_U1_burden = function(sim_output_filepath, experiment_name, admin_pop, comparison_start_year, comparison_end_year, cur_admins='all'){
  output_filename = paste0(sim_output_filepath, '/', experiment_name, '/totalU1Burden_', comparison_start_year,'_', comparison_end_year,'_', cur_admins,'.csv')
  if(file.exists(output_filename) & !overwrite_files){
    burden_means = read.csv(output_filename)
  } else{
    
    burden_df = fread(paste0(sim_output_filepath, '/', experiment_name, '/malariaBurden_withAdjustments.csv'), check.names=TRUE)
    # subset to appropriate time period
    burden_df = burden_df[intersect(which(burden_df$year >= comparison_start_year), which(burden_df$year <= comparison_end_year)),]
    if(!(cur_admins[1] == 'all')){
      # subset to appropriate admins
      burden_df = burden_df[burden_df$admin_name %in% cur_admins,] 
    }
    
    # PfPR
    pfpr_means = burden_df[,c('admin_name', 'PfPR_U1')] %>% dplyr::group_by(admin_name) %>% dplyr::summarise_all(mean) %>% dplyr::ungroup()
    colnames(pfpr_means)[which(colnames(pfpr_means) == 'PfPR_U1')] = 'pfpr_u1'
    
    # number of deaths
    # divide mortality by total population size in simulation; will later multiply by true population of DS
    burden_df$mortality_pp_u1 = (burden_df$total_mortality_U1_1*1 + burden_df$total_mortality_U1_2*1) / 2 / burden_df$Statistical_Population  # weighted average of mortality estimates (1/2 mort_1, 1/2 mort_2)
    mortality_sums = burden_df[,c('admin_name', 'Run_Number', 'mortality_pp_u1')] %>% dplyr::group_by(admin_name, Run_Number) %>% dplyr::summarise_all(sum) %>% dplyr::ungroup()
    mortality_sums = mortality_sums[,c('admin_name', 'mortality_pp_u1')] %>% dplyr::group_by(admin_name) %>% dplyr::summarise_all(mean) %>% dplyr::ungroup()
    # multiply by admin population to get total number of deaths
    mortality_sums = merge(mortality_sums, admin_pop, by='admin_name')
    mortality_sums$deaths_u1 = mortality_sums$mortality_pp_u1 * mortality_sums$pop_size
    
    # mortality rate  (number of deaths in a year in each admin divided by the U1 population size times 1000)
    # take sum of deaths within each year
    burden_df_annual =  burden_df %>% dplyr::group_by(admin_name, Run_Number, year) %>% 
      dplyr::summarise(total_mortality_U1_1 = sum(total_mortality_U1_1),
                       total_mortality_U1_2 = sum(total_mortality_U1_2),
                       New_clinical_cases_U1 = sum(New_clinical_cases_U1),
                       Pop_U1 = mean(Pop_U1)
      ) %>% 
      dplyr::ungroup()
    burden_df_annual$mortality_rate_u1 = (burden_df_annual$total_mortality_U1_1*1 + burden_df_annual$total_mortality_U1_2*1) / 2 / burden_df_annual$Pop_U1 * 1000  # weighted average of mortality estimates (1/2 mort_1, 1/2 mort_2)
    # remove any NA rows
    burden_df_annual = burden_df_annual[!is.na(burden_df_annual$mortality_rate_u1),]
    # get average annual rate across all included years
    mortality_rate_means = burden_df_annual[,c('admin_name', 'mortality_rate_u1')] %>% dplyr::group_by(admin_name) %>% 
      dplyr::summarise(mortality_rate_u1 = mean(mortality_rate_u1)) %>% 
      dplyr::ungroup()
    
    
    
    # clinical cases
    # divide number of clinical cases by total population size in simulation; will later multiply by true population of DS to get estimated number of clinical cases in that DS
    burden_df$cases_pp_u1 = burden_df$New_clinical_cases_U1 / burden_df$Statistical_Population
    case_sums = burden_df[,c('admin_name', 'Run_Number', 'cases_pp_u1')] %>% group_by(admin_name, Run_Number) %>% summarise_all(sum) %>% ungroup()
    case_sums = case_sums[,c('admin_name', 'cases_pp_u1')] %>% group_by(admin_name) %>% summarise_all(mean) %>% ungroup()
    # multiply by admin population to get total number of cases
    case_sums = merge(case_sums, admin_pop, by='admin_name')
    case_sums$clinical_cases_u1 = case_sums$cases_pp_u1 * case_sums$pop_size
    
    
    
    # incidence (number of cases in a year in each admin divided by the U1 population size times 1000)
    burden_df_annual$incidence_u1 = burden_df_annual$New_clinical_cases_U1 / burden_df_annual$Pop_U1 * 1000
    # remove any NA rows
    burden_df_annual = burden_df_annual[!is.na(burden_df_annual$incidence_u1),]
    # get average annual rate across all included years
    incidence_means = burden_df_annual[,c('admin_name', 'incidence_u1')] %>% dplyr::group_by(admin_name) %>% 
      dplyr::summarise(incidence_u1 = mean(incidence_u1)) %>% 
      dplyr::ungroup()
    
    
    # fraction of population U1
    burden_df$frac_u1 = burden_df$Pop_U1 / burden_df$Statistical_Population
    frac_u1 = burden_df[,c('admin_name', 'frac_u1')] %>% group_by(admin_name) %>% summarise_all(mean) %>% ungroup()
    
    burden_means = merge(pfpr_means, mortality_sums, by='admin_name')
    burden_means = merge(burden_means, case_sums, by=c('admin_name', 'pop_size'))
    burden_means = merge(burden_means, mortality_rate_means, by=c('admin_name'))
    burden_means = merge(burden_means, incidence_means, by=c('admin_name'))
    burden_means = merge(burden_means, frac_u1, by=c('admin_name'))
    burden_means$pop_size_u1 = burden_means$pop_size * burden_means$frac_u1
    
    write.csv(burden_means, output_filename, row.names=FALSE)
  }
  
  return(burden_means)
}




# get_total_relative_burden = function(sim_output_filepath, reference_experiment_name, comparison_experiment_name, comparison_scenario_name, comparison_start_year, comparison_end_year, admin_pop, cur_admins='all'){
#   reference_df = get_total_burden(sim_output_filepath=sim_output_filepath, experiment_name=reference_experiment_name, admin_pop=admin_pop, comparison_start_year=comparison_start_year, comparison_end_year=comparison_end_year, cur_admins=cur_admins)
#   comparison_df = get_total_burden(sim_output_filepath=sim_output_filepath, experiment_name=comparison_experiment_name, admin_pop=admin_pop, comparison_start_year=comparison_start_year, comparison_end_year=comparison_end_year, cur_admins=cur_admins)
#   # align seeds
#   reference_df = reference_df[order(reference_df$Run_Number),]
#   comparison_df = comparison_df[order(comparison_df$Run_Number),]
#   
#   relative_burden_df = data.frame('Run_Number' = reference_df$Run_Number)
#   # iterate through burden indicators, calculating relative burden and adding to dataframe
#   burden_indicators = colnames(reference_df)[-which(colnames(reference_df) == 'Run_Number')]
#   for(bb in 1:length(burden_indicators)){
#     relative_burden_cur = (comparison_df[[burden_indicators[bb]]] - reference_df[[burden_indicators[bb]]]) / reference_df[[burden_indicators[bb]]]
#     relative_burden_df[[burden_indicators[bb]]] = relative_burden_cur
#   }
#   relative_burden_df$scenario = comparison_scenario_name
#   return(relative_burden_df)
# }




# merge burden column with existing burden dataframe, updating new column name to be the name of the scenario
add_new_scenario_column_to_burden_df = function(target_indicator_df, cur_burden, indicator_colname, option_name){
  if(nrow(target_indicator_df)==0){
    target_indicator_df = cur_burden[,c('admin_name',indicator_colname)]
  } else{
    target_indicator_df = merge(target_indicator_df, cur_burden[,c('admin_name',indicator_colname)], by='admin_name')
  }
  colnames(target_indicator_df)[which(colnames(target_indicator_df) == indicator_colname)] = option_name
  return(target_indicator_df)
}





# # get number of nets distributed in each DS from simulation, rescaled to DS population size (average across all seeds and scenarios since these numbers should not depend on net type)
# # 1) get number of nets distributed in relevant years, 2) get simulated population size, 3) rescale to actual population size, 4) average rescaled values across runs
# #   output is a data table with first column giving admin name and second column giving number of nets in comparison period
# get_num_nets = function(sim_output_filepath, comparison_scenario_name, admin_pop, comparison_start_year, comparison_end_year){
#   event_df = fread(paste0(sim_output_filepath, '/', comparison_scenario_name, '/monthly_Event_Count.csv'))
#   event_df$year = lubridate::year(as.Date(event_df$date))
#   # subset to appropriate time period
#   event_df = event_df[intersect(which(event_df$year >= comparison_start_year), which(event_df$year <= comparison_end_year)),]
#   # get sums across years in time period
#   event_sums = event_df[,c('admin_name', 'Run_Number', 'Bednet_Got_New_One')] %>% group_by(admin_name, Run_Number) %>% summarise_all(sum) %>% ungroup()
#   
#   
#   # rescale to appropriate population size of admin
#   burden_df = fread(paste0(sim_output_filepath, '/', comparison_scenario_name, '/malariaBurden_withAdjustments.csv'))
#   # subset to appropriate time period
#   burden_df = burden_df[intersect(which(burden_df$year >= comparison_start_year), which(burden_df$year <= comparison_end_year)),]
#   # get sums across years in time period
#   sim_pop = burden_df[,c('admin_name', 'Run_Number', 'Statistical_Population')] %>% group_by(admin_name, Run_Number) %>% summarise_all(mean) %>% ungroup()
#   
#   # get nets per person
#   event_sums = merge(event_sums, sim_pop, by=c('admin_name','Run_Number'))
#   event_sums$nets_per_person = event_sums$Bednet_Got_New_One / event_sums$Statistical_Population
#   
#   # get means across runs
#   nets_per_capita = event_sums[,c('admin_name','Run_Number','nets_per_person')] %>% group_by(admin_name) %>% summarise_all(mean) %>% ungroup()
#   
#   # rescale numbers to true population size
#   nets_each_admin = merge(nets_per_capita, admin_pop, by='admin_name')
#   nets_each_admin$num_nets = nets_each_admin$nets_per_person * nets_each_admin$pop_size
# 
#   return(nets_each_admin[,c('admin_name','num_nets')])
# }
# 
# 
# get_expected_nets_each_admin = function(sim_output_filepath, comparison_scenarios, admin_pop, comparison_start_year, comparison_end_year){
#   for(ss in 1:length(comparison_scenarios)){
#     nets_each_admin_cur = get_num_nets(sim_output_filepath=sim_output_filepath, comparison_scenario_name=comparison_scenarios[ss], admin_pop=admin_pop, comparison_start_year=comparison_start_year, comparison_end_year=comparison_end_year)
#     if(ss==1){
#       nets_each_admin_all = nets_each_admin_cur
#     } else{
#       nets_each_admin_all = merge(nets_each_admin_all, nets_each_admin_cur, by='admin_name')
#     }
#     colnames(nets_each_admin_all)[which(colnames(nets_each_admin_all) == 'num_nets')] = paste0('ss',ss)
#   }
#   
#   # get average across all scenarios
#   nets_each_admin_all$num_nets = apply(nets_each_admin_all[,grep('ss',colnames(nets_each_admin_all))], MARGIN=1, FUN=mean)
#   
#   return(nets_each_admin_all[,c('admin_name','num_nets')])  #ACK: issue here is that we give 'the same' net to multiple people (they share a net), but output will look like separate nets here. I think it's better to calculate expected number of nets based on admin population size and distribution coverages.
# }



get_expected_nets_each_admin_from_sim_inputs = function(input_itn_mass_filename, input_itn_epi_filename, input_itn_anc_filename, admin_pop, comparison_start_year, comparison_end_year, frac_turn_1_each_year=0.0366, frac_born_each_year=0.0385){
  
  cumulative_nets = data.frame(admin_name = sort(unique(admin_pop$admin_name)), num_mass_nets = rep(0,length(unique(admin_pop$admin_name))), num_epi_nets = rep(0,length(unique(admin_pop$admin_name))), num_anc_nets = rep(0,length(unique(admin_pop$admin_name))))
  ##  ========================  ##
  ##     mass distribution
  ##  ========================  ##
  mass_df = fread(input_itn_mass_filename)
  # subset to appropriate time period
  mass_df = mass_df[intersect(which(mass_df$year >= comparison_start_year), which(mass_df$year <= comparison_end_year)),]
  # iterate through years of mass distributions
  if(nrow(mass_df>0)){
    mass_dist_years = unique(mass_df$year)
    for(yy in 1:length(mass_dist_years)){
      # subset to current year
      mass_df_yy = mass_df[mass_df$year == mass_dist_years[yy],]
      # get average nets per capita for each admin across seeds
      mass_df_ave = mass_df_yy[,c('admin_name','nets_per_capita')] %>% group_by(admin_name) %>% summarise_all(mean) %>% ungroup()
      # merge with population dataframe
      mass_df_ave = merge(mass_df_ave, admin_pop, by='admin_name')
      # calculate total number of nets in admin
      mass_df_ave$num_mass_nets = mass_df_ave$nets_per_capita * mass_df_ave$pop_size
      # re-order to same order as cumulative_mass_nets data frame
      mass_df_ave = mass_df_ave[order(mass_df_ave$admin_name),]
      # add total number of mass distribution nets
      cumulative_nets$num_mass_nets = cumulative_nets$num_mass_nets + mass_df_ave$num_mass_nets
    }
  }
   
  ##  ========================  ##
  ##    EPI distribution
  ##  ========================  ##
  epi_df = fread(input_itn_epi_filename)
  # subset to age of child going to EPI visit (other age is for siblings who share the net)
  epi_df = epi_df[epi_df$birthday_age == 1,]
  # iterate through years, taking total numbers
  # check if duration for a previous year is -1, if so, use that value for all years; otherwise subset to comparison years
  if(any(epi_df$duration[epi_df$year < comparison_end_year] == -1)){
    # assume that all admins were specified once with a duration -1 and ignore following years
    if(length(unique(epi_df$year[epi_df$duration == -1]))>1) warning('ISSUE DETECTED: it looks like the EPI ITN distributions are not formatted in the simple way expected. Need to update function to use current format.')
    epi_df = epi_df[epi_df$year == unique(epi_df$year[epi_df$duration == -1]),]
    # get coverage at the population level (multiply by fraction of population in targeted age group to get population-level coverage)
    epi_df$nets_per_capita = epi_df$coverage * frac_turn_1_each_year
    
    # get average nets per capita for each admin across seeds
    epi_df_ave = epi_df[,c('admin_name','nets_per_capita')] %>% group_by(admin_name) %>% summarise_all(mean) %>% ungroup()
    # merge with population dataframe
    epi_df_ave = merge(epi_df_ave, admin_pop, by='admin_name')
    # calculate total number of nets in admin
    epi_df_ave$num_epi_nets = epi_df_ave$nets_per_capita * epi_df_ave$pop_size
    # re-order to same order as cumulative_mass_nets data frame
    epi_df_ave = epi_df_ave[order(epi_df_ave$admin_name),]
    # cumulative nets are this year's nets times the number of years
    cumulative_nets$num_epi_nets = cumulative_nets$num_epi_nets + epi_df_ave$num_epi_nets * (comparison_end_year - comparison_start_year + 1)
  } else{
    epi_df = epi_df[intersect(which(epi_df$year >= comparison_start_year), which(epi_df$year <= comparison_end_year)),]
    
    # iterate through years of mass distributions
    if(nrow(epi_df>0)){
      epi_dist_years = unique(epi_df$year)
      for(yy in 1:length(epi_dist_years)){
        # subset to current year
        epi_df_yy = epi_df[epi_df$year == epi_dist_years[yy],]
        # get coverage at the population level (multiply by fraction of population in targeted age group to get population-level coverage)
        epi_df_yy$nets_per_capita = epi_df_yy$coverage * frac_turn_1_each_year
        # get average nets per capita for each admin across seeds
        epi_df_ave = epi_df_yy[,c('admin_name','nets_per_capita')] %>% group_by(admin_name) %>% summarise_all(mean) %>% ungroup()
        # merge with population dataframe
        epi_df_ave = merge(epi_df_ave, admin_pop, by='admin_name')
        # calculate total number of nets in admin
        epi_df_ave$num_epi_nets = epi_df_ave$nets_per_capita * epi_df_ave$pop_size
        # re-order to same order as cumulative_mass_nets data frame
        epi_df_ave = epi_df_ave[order(epi_df_ave$admin_name),]
        # add total number of mass distribution nets
        cumulative_nets$num_epi_nets = cumulative_nets$num_epi_nets + epi_df_ave$num_epi_nets
      }
    }
  }

  
  ##  ========================  ##
  ##    ANC distribution
  ##  ========================  ##
  anc_df = fread(input_itn_anc_filename)
  # iterate through years, taking total numbers
  # check if duration for a previous year is -1, if so, use that value for all years; otherwise subset to comparison years
  if(any(anc_df$duration[anc_df$year < comparison_end_year] == -1)){
    # assume that all admins were specified once with a duration -1 and ignore following years
    if(length(unique(anc_df$year[anc_df$duration == -1]))>1) warning('ISSUE DETECTED: it looks like the ANC ITN distributions are not formatted in the simple way expected. Need to update function to use current format.')
    anc_df = anc_df[anc_df$year == unique(anc_df$year[anc_df$duration == -1]),]
    # get coverage at the population level (multiply by fraction of population in targeted age group to get population-level coverage)
    anc_df$nets_per_capita = anc_df$coverage * frac_born_each_year
    
    # get average nets per capita for each admin across seeds
    anc_df_ave = anc_df[,c('admin_name','nets_per_capita')] %>% group_by(admin_name) %>% summarise_all(mean) %>% ungroup()
    # merge with population dataframe
    anc_df_ave = merge(anc_df_ave, admin_pop, by='admin_name')
    # calculate total number of nets in admin
    anc_df_ave$num_anc_nets = anc_df_ave$nets_per_capita * anc_df_ave$pop_size
    # re-order to same order as cumulative_mass_nets data frame
    anc_df_ave = anc_df_ave[order(anc_df_ave$admin_name),]
    # cumulative nets are this year's nets times the number of years
    cumulative_nets$num_anc_nets = cumulative_nets$num_anc_nets + anc_df_ave$num_anc_nets * (comparison_end_year - comparison_start_year + 1)
  } else{
    anc_df = anc_df[intersect(which(anc_df$year >= comparison_start_year), which(anc_df$year <= comparison_end_year)),]
    
    # iterate through years of mass distributions
    if(nrow(anc_df>0)){
      anc_dist_years = unique(anc_df$year)
      for(yy in 1:length(anc_dist_years)){
        # subset to current year
        anc_df_yy = anc_df[anc_df$year == anc_dist_years[yy],]
        # get coverage at the population level (multiply by fraction of population in targeted age group to get population-level coverage)
        anc_df_yy$nets_per_capita = anc_df_yy$coverage * frac_born_each_year
        # get average nets per capita for each admin across seeds
        anc_df_ave = anc_df_yy[,c('admin_name','nets_per_capita')] %>% group_by(admin_name) %>% summarise_all(mean) %>% ungroup()
        # merge with population dataframe
        anc_df_ave = merge(anc_df_ave, admin_pop, by='admin_name')
        # calculate total number of nets in admin
        anc_df_ave$num_anc_nets = anc_df_ave$nets_per_capita * anc_df_ave$pop_size
        # re-order to same order as cumulative_mass_nets data frame
        anc_df_ave = anc_df_ave[order(anc_df_ave$admin_name),]
        # add total number of mass distribution nets
        cumulative_nets$num_anc_nets = cumulative_nets$num_anc_nets + anc_df_ave$num_anc_nets
      }
    }
  }
  
  cumulative_nets$total_nets = cumulative_nets$num_mass_nets + cumulative_nets$num_epi_nets + cumulative_nets$num_anc_nets
  return(cumulative_nets)
}




get_burden_each_admin = function(burden_df, state_column_each_ds){
  burden_each_admin = rep(NA, nrow(burden_df))
  for(i_ds in 1:dim(burden_df)[1]){
    burden_each_admin[i_ds] = burden_df[i_ds,(state_column_each_ds[i_ds] + 1)]  # plus 1 because the first column is the admin name
  }
  return(burden_each_admin)
}

# function that calculates total burden across country for a given secnario
sum_over_ds = function(ds_df, state_column_each_ds){
  cur_sum = 0
  for(i_ds in 1:dim(ds_df)[1]){
    cur_sum = cur_sum + ds_df[i_ds,(state_column_each_ds[i_ds] + 1)]  # plus 1 because the first column is the admin name
  }
  return(cur_sum)
}


# function that calculates total burden across country given the smc strategy in each DS, after checking that doses are under limit
#  note that the indices of burden_df must match with doses_df; the admin_names column should either be the final column or should be removed
sum_total_burden = function(state_column_each_ds, burden_df, nets_each_admin, pbo_net_budget){
  cur_nets = sum(nets_each_admin$total_nets[state_column_each_ds==2])
  if(cur_nets <= pbo_net_budget){
    return(sum_over_ds(ds_df=burden_df, state_column_each_ds=state_column_each_ds))
  }else{
    return(Inf)
  }
}


##################################################
# local optimization within local neighborhood
##################################################



# iterative neighborhood mutation search
#   1) from a starting state S1 (will want to re-run with many, many starting states), calculate the starting outcome measure
#   2) create X1 test states in the neighborhood of the starting state by randomly changing the state of each admin with a probability equal to neighborhood_distance/num_admins
#   3) evaluate the outcome measure for all drawn neighborhoods and select the best (of these and the starting state) as the starting state for the new iteration, S2
#   4) repeat steps 2-3 num_steps times and return the best state along with the outcome measure for that state
neighborhood_mutation_search = function(starting_state_each_ds, burden_df, nets_each_admin, pbo_net_budget, num_steps=500, neighborhood_distance=5){

  store_all_local_states = matrix(NA, ncol=(num_steps+1), nrow=length(starting_state_each_ds))
  store_all_local_states[,1]=starting_state_each_ds
  best_col_so_far = 1
  burden_in_best_col = sum_total_burden(burden_df=burden_df, nets_each_admin=nets_each_admin, state_column_each_ds=store_all_local_states[,1], pbo_net_budget=pbo_net_budget) 
  for(ss in 1:num_steps){
    # get matrix of proposed states (each test state is a column)
    # start with a vector of states (ordered so that it can later be converted into the desired matrix)
    proposed_states_vect = rep(store_all_local_states[,best_col_so_far], times=num_steps)
    # mutate each state with probability = neighborhood_distance/nrow(store_all_local_states)
    proposed_states_vect = proposed_states_vect + rbinom(n=length(proposed_states_vect), size=1, prob=neighborhood_distance/nrow(store_all_local_states))
    proposed_states_vect[proposed_states_vect==3] = 1  # states that change from 2 should go to 1 instead of 3
    # convert into matrix
    proposed_states = matrix(data=proposed_states_vect, ncol=num_steps, byrow=FALSE)
    
    # calculate burden outcome for each of the proposed states
    burden_proposed_states = apply(X=proposed_states, MARGIN=2, FUN=sum_total_burden, burden_df=burden_df, nets_each_admin=nets_each_admin, pbo_net_budget=pbo_net_budget)
    
    # find the column with the best burden
    best_proposed_state_index = which.min(burden_proposed_states)[1]
    
    # compare best proposed state to pprevious best state, and update if appropriate
    # check whether this new column is now the best
    if(burden_proposed_states[best_proposed_state_index] < burden_in_best_col){
      burden_in_best_col = burden_proposed_states[best_proposed_state_index]
      best_col_so_far = ss+1
      store_all_local_states[,(ss+1)] = proposed_states[,best_proposed_state_index]
    } else{
      store_all_local_states[,(ss+1)] = store_all_local_states[,(ss)]
    }
  }
  return(store_all_local_states[,(num_steps+1)])  # return best option out of search
}




##############################
# plots
##############################


# function to create maps showing which ITN type is used in each admin
create_itn_type_map = function(state_each_ds, admin_names, color_each_type, admin_shapefile, shapefile_admin_colname='NOMDEP', pdf_filename='', plot_title=''){
  if(pdf_filename != '') pdf(pdf_filename, width=12, height=6/4*3, useDingbats = FALSE)
  
  itn_types_ordered = data.frame('ds_ordered'=admin_shapefile[[shapefile_admin_colname]], 'itn_value'=rep(NA, length(admin_shapefile[[shapefile_admin_colname]])))
  for (i_ds in 1:length(itn_types_ordered$ds_ordered)){
    itn_types_ordered$itn_value[i_ds] = state_each_ds[which(toupper(admin_names) == toupper(itn_types_ordered$ds_ordered[i_ds]))]
  }
  col_cur = color_each_type[itn_types_ordered$itn_value]
  col_cur[is.na(col_cur)] = 'grey'
  plot(admin_shapefile, col=col_cur, border=rgb(0.3,0.3,0.3), main=plot_title)

  if(pdf_filename != '') dev.off()
}


# function to create map of burden in each DS (with scalebar)
create_burden_map = function(burden_each_admin, admin_names, colorscale, min_value, max_value, num_colors=9, admin_shapefile, shapefile_admin_colname='NOMDEP', pdf_filename='', plot_title=''){
  if(pdf_filename != '') pdf(pdf_filename, width=12, height=6/4*3, useDingbats = FALSE)
  
  vals_ordered = data.frame('ds_ordered'=admin_shapefile[[shapefile_admin_colname]], 'value'=rep(NA, length(admin_shapefile[[shapefile_admin_colname]])))
  for (i_ds in 1:length(vals_ordered$ds_ordered)){
    vals_ordered$value[i_ds] = burden_each_admin[which(toupper(admin_names) == toupper(vals_ordered$ds_ordered[i_ds]))]
  }
  col_cur = colorscale[sapply(floor((num_colors)*(vals_ordered$value - min_value) / (max_value - min_value))+1, min, num_colors)]
  col_cur[is.na(col_cur)] = 'grey'
  plot(admin_shapefile, col=col_cur, border=rgb(0.3,0.3,0.3), main=plot_title)
  
  if(pdf_filename != '') dev.off()
  
}


# function to create coverage maps
create_coverage_map = function(cov_df, ds_colname, value_colname, year_colname, years_include, admin_shapefile, shapefile_admin_colname, colorscale, numcolors, min_value, max_value, pdf_filename=''){
  if(pdf_filename != '') pdf(pdf_filename, width=12, height=6/4*3, useDingbats = FALSE)
  
  for(yy in 1:length(years_include)){
    # subset to appropriate year
    cov_df_yy = cov_df[cov_df[[year_colname]] == years_include[yy],]
    
    # if(all(toupper(cov_df_yy[[ds_colname]]) %in% admin_shapefile[[shapefile_admin_colname]])){
    
    cov_df_ordered = data.frame('ds_ordered'=admin_shapefile[[shapefile_admin_colname]], 'value'=rep(NA, length(admin_shapefile[[shapefile_admin_colname]])))
    for (i_ds in 1:length(cov_df_ordered$ds_ordered)){
      cur_row = which(toupper(cov_df_yy[[ds_colname]]) == toupper(cov_df_ordered$ds_ordered[i_ds]))
      if (length(cur_row)==1){
        cov_df_ordered$value[i_ds] = cov_df_yy[[value_colname]][cur_row]
      }
    }
    # iptp_2018_ordered = merge(LGAs_shapefile_order, iptp_2018, by.x='LGA_order', by.y='LGA', all.x=TRUE)
    col_cur = colorscale[sapply(floor((num_colors)*(cov_df_ordered$value - min_value) / (max_value - min_value))+1, min, num_colors)]
    col_cur[is.na(col_cur)] = 'grey'
    plot(admin_shapefile, col=col_cur, border=rgb(0.3,0.3,0.3), main=paste0(value_colname, ' - ', years_include[yy]))
    # } else{
    #   print('not all DS names matched between shapefile and intervention file')
    # }
  }
  if(pdf_filename != '') dev.off()
}



