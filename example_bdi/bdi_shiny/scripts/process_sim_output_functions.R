# process_aggregate_sim_output_functions.R


library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(rgdal)
library(lubridate)




###########################################################################################################
# total simulation burden over specified interval, averaged across seeds but separated by district
###########################################################################################################


# function that returns dataframe where each row is an admin and columns contain U5 and all-age total number of cases, total number of deaths, incidence, death rate, and average pfpr within the time period in each admin, averaged over all seeds
get_total_burden = function(sim_output_filepath, experiment_name, admin_pop, comparison_start_year, comparison_end_year, district_subset, cur_admins='all', overwrite_files=FALSE){
  output_filename = paste0(sim_output_filepath, '/', experiment_name, '/totalBurden_', comparison_start_year,'_', comparison_end_year,'_', district_subset,'.csv')
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




# function that returns dataframe where each row is an admin and columns contain U1 total number of cases, total number of deaths, incidence, death rate, and average pfpr within the time period in each admin, averaged over all seeds
get_total_U1_burden = function(sim_output_filepath, experiment_name, admin_pop, comparison_start_year, comparison_end_year, district_subset, cur_admins='all', overwrite_files=FALSE){
  output_filename = paste0(sim_output_filepath, '/', experiment_name, '/totalU1Burden_', comparison_start_year,'_', comparison_end_year,'_', district_subset,'.csv')
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






###############################################################################################################################
# total simulation burden over specified interval, aggregated across all incldued districts and separated by seed
###############################################################################################################################


get_cumulative_burden = function(sim_output_filepath, experiment_name, start_year, end_year, admin_pop, district_subset='allDistricts', cur_admins='all', LLIN2y_flag=FALSE, overwrite_files=FALSE){
#'  @description get cumulative U5 and all-age burden over specified years in specified districts (for all malaria metrics, separate values for each seed)
#'  @return save and return data frame where each row is a seed and each column is the total over all included years of different burden metrics:
#'      sum of:
#'         - clinical cases (all ages)
#'         - clinical cases (U5)
#'         - deaths (all ages) - upper, lower, average parameter estimates
#'         - deaths (U5) - upper, lower, average parameter estimates
#'         - mLBW
#'         - malaria-attributable stillbirths
#'       average of annual (population weighted):
#'         - PfPR (all ages)
#'         - PfPR (U5)
#'         - incidence (all ages)
#'         - incidence (U5)
#'         - death rate (all ages)
#'         - death rate (U5)
#'         - mLBW
#'         - stillbirths


  output_filename = paste0(sim_output_filepath, '/', experiment_name, '/cumulativeBurden_', start_year, '_', end_year, '_', district_subset, '.csv')
  if(file.exists(output_filename) & !overwrite_files){
    df_aggregated = read.csv(output_filename)
  }else{
    # if we include all admins, get list of names from population size dataframe
    if(cur_admins[1] == 'all'){
      cur_admins = unique(admin_pop$admin_name)
    }
    
    cur_file = fread(paste0(sim_output_filepath, '/', experiment_name, '/malariaBurden_withAdjustments.csv'), check.names=TRUE)
    # filter to relevant years
    cur_file = cur_file[cur_file$year <= end_year,]
    cur_file = cur_file[cur_file$year >= start_year,]
    # merge population sizes in each admin
    df = merge(cur_file, admin_pop, by='admin_name')
    # subset to appropriate admins
    df = df[df$admin_name %in% cur_admins,]  
    
    # all age metrics - rescaled to full population
    df$positives_all_ages = df$PfPR_MiP_adjusted * df$pop_size
    df$cases_all_ages = df$New_Clinical_Cases * df$pop_size / df$Statistical_Population
    df$deaths_1_all_ages = df$total_mortality_1 * df$pop_size / df$Statistical_Population
    df$deaths_2_all_ages = df$total_mortality_2 * df$pop_size / df$Statistical_Population
    df$num_mLBW = df$mLBW_births * df$pop_size / df$Statistical_Population
    df$num_mStillbirths = df$MiP_stillbirths * df$pop_size / df$Statistical_Population
    # U5 metrics - rescaled to full population
    df$pop_size_U5 = df$pop_size * (df$Pop_U5 / df$Statistical_Population)  # assumes fraction of individual U5 in simulation is same as fraction in full population
    df$positives_U5 = df$PfPR_U5 * df$pop_size_U5
    df$cases_U5 = df$New_clinical_cases_U5 * df$pop_size_U5 / df$Pop_U5
    df$deaths_1_U5 = df$total_mortality_U5_1 * df$pop_size_U5 / df$Pop_U5
    df$deaths_2_U5 = df$total_mortality_U5_2 * df$pop_size_U5 / df$Pop_U5
    
    df_aggregated = df %>% group_by(Run_Number) %>%
      dplyr::summarize(pop_all_sum = sum(pop_size),
                       pop_U5_sum = sum(pop_size_U5),
                       cases_all_sum = sum(cases_all_ages),
                       cases_U5_sum = sum(cases_U5),
                       positives_all_sum = sum(positives_all_ages),
                       positives_U5_sum = sum(positives_U5),
                       deaths_1_all_sum = sum(deaths_1_all_ages),
                       deaths_2_all_sum = sum(deaths_2_all_ages),
                       deaths_1_U5_sum = sum(deaths_1_U5),
                       deaths_2_U5_sum = sum(deaths_2_U5),
                       mLBW_sum = sum(num_mLBW),
                       mStill_sum = sum(num_mStillbirths),
                       num_values_grouped = n())
    
    
    # clinical incidence (annual): sum of number of cases over all months / (pop size) / (number of years) * 1000
    #     = sum of number of cases over all months / (sum of pop size over all months /  number of months) / (number of months / 12)  * 1000
    #     = sum of number of cases over all months / (sum of pop size over all months / 12)  * 1000
    df_aggregated$incidence_all = (df_aggregated$cases_all_sum / (df_aggregated$pop_all_sum / 12) * 1000)
    df_aggregated$incidence_U5 = (df_aggregated$cases_U5_sum / (df_aggregated$pop_U5_sum / 12) * 1000)
    df_aggregated$death_rate_1_all = (df_aggregated$deaths_1_all_sum / (df_aggregated$pop_all_sum / 12) * 1000)
    df_aggregated$death_rate_2_all = (df_aggregated$deaths_2_all_sum / (df_aggregated$pop_all_sum / 12) * 1000)
    df_aggregated$death_rate_mean_all = (df_aggregated$death_rate_1_all + df_aggregated$death_rate_2_all) / 2
    df_aggregated$death_rate_1_U5 = (df_aggregated$deaths_1_U5_sum / (df_aggregated$pop_U5_sum / 12) * 1000)
    df_aggregated$death_rate_2_U5 = (df_aggregated$deaths_2_U5_sum / (df_aggregated$pop_U5_sum / 12) * 1000)
    df_aggregated$death_rate_mean_U5 = (df_aggregated$death_rate_1_U5 + df_aggregated$death_rate_2_U5) / 2
    df_aggregated$average_PfPR_all = (df_aggregated$positives_all_sum  / (df_aggregated$pop_all_sum))
    df_aggregated$average_PfPR_U5 = (df_aggregated$positives_U5_sum  / (df_aggregated$pop_U5_sum))
    df_aggregated$annual_num_mLBW = (df_aggregated$mLBW_sum / (end_year - start_year + 1))
    df_aggregated$annual_num_mStill = (df_aggregated$mStill_sum / (end_year - start_year + 1))
    
    
    df_aggregated = df_aggregated[,which(colnames(df_aggregated) %in% c('Run_Number','cases_all_sum','cases_U5_sum', 'deaths_1_all_sum', 'deaths_2_all_sum', 'deaths_1_U5_sum', 'deaths_2_U5_sum', 'mLBW_sum', 'mStill_sum',
                                                                        'incidence_all', 'incidence_U5', 'death_rate_1_all', 'death_rate_2_all', 'death_rate_1_U5', 'death_rate_2_U5', 'annual_num_mLBW', 'annual_num_mStill', 
                                                                        'death_rate_mean_all', 'death_rate_mean_U5', 'average_PfPR_all','average_PfPR_U5'))]
    write.csv(df_aggregated, output_filename, row.names=FALSE)
  }
  return(df_aggregated)
}








get_cumulative_U1_burden = function(sim_output_filepath, experiment_name, start_year, end_year, admin_pop, district_subset=district_subset, cur_admins='all', LLIN2y_flag=FALSE, overwrite_files=FALSE){
  #'  @description get cumulative U1 burden over specified years in specified districts (for all malaria metrics, separate values for each seed)
  #'  @return save and return data frame where each row is a seed and each column is the total over all included years of different burden metrics:
  #'      sum of:
  #'         - clinical cases (U1)
  #'         - deaths (U1) - upper, lower, average parameter estimates
  #'       average of annual (population weighted):
  #'         - PfPR (U1)
  #'         - incidence (U1)
  #'         - death rate (U1)

  

  output_filename = paste0(sim_output_filepath, '/', experiment_name, '/cumulativeBurden_IPTi_', start_year, '_', end_year, '_', district_subset, '.csv')
  if(file.exists(output_filename) & !overwrite_files){
    df_aggregated = read.csv(output_filename)
  }else{
    # if we include all admins, get list of names from population size dataframe
    if(cur_admins[1] == 'all'){
      cur_admins = unique(admin_pop$admin_name)
    }
    
    cur_file = fread(paste0(sim_output_filepath, '/', experiment_name, '/malariaBurden_withAdjustments.csv'), check.names=TRUE)
    # filter to relevant years
    cur_file = cur_file[cur_file$year <= end_year,]
    cur_file = cur_file[cur_file$year >= start_year,]
    # merge population sizes in each admin
    df = merge(cur_file, admin_pop, by='admin_name')
    # subset to appropriate admins
    df = df[df$admin_name %in% cur_admins,]  
    
    # U1 metrics - rescaled to full population
    df$pop_size_U1 = df$pop_size * (df$Pop_U1 / df$Statistical_Population)  # assumes fraction of individual U1 in simulation is same as fraction in full population
    df$positives_U1 = df$PfPR_U1 * df$pop_size_U1
    df$cases_U1 = df$New_clinical_cases_U1 * df$pop_size_U1 / df$Pop_U1
    df$deaths_1_U1 = df$total_mortality_U1_1 * df$pop_size_U1 / df$Pop_U1
    df$deaths_2_U1 = df$total_mortality_U1_2 * df$pop_size_U1 / df$Pop_U1
    
    df_aggregated = df %>% dplyr::group_by(Run_Number) %>%
      dplyr::summarize(pop_U1_sum = sum(pop_size_U1),
                       cases_U1_sum = sum(cases_U1),
                       positives_U1_sum = sum(positives_U1),
                       deaths_1_U1_sum = sum(deaths_1_U1),
                       deaths_2_U1_sum = sum(deaths_2_U1),
                       num_values_grouped = n())
    
    
    # clinical incidence (annual): sum of number of cases over all months / (pop size) / (number of years) * 1000
    #     = sum of number of cases over all months / (sum of pop size over all months /  number of months) / (number of months / 12)  * 1000
    #     = sum of number of cases over all months / (sum of pop size over all months / 12)  * 1000
    df_aggregated$incidence_U1 = (df_aggregated$cases_U1_sum / (df_aggregated$pop_U1_sum / 12) * 1000)
    df_aggregated$death_rate_1_U1 = (df_aggregated$deaths_1_U1_sum / (df_aggregated$pop_U1_sum / 12) * 1000)
    df_aggregated$death_rate_2_U1 = (df_aggregated$deaths_2_U1_sum / (df_aggregated$pop_U1_sum / 12) * 1000)
    df_aggregated$death_rate_mean_U1 = (df_aggregated$death_rate_1_U1 + df_aggregated$death_rate_2_U1) / 2
    df_aggregated$average_PfPR_U1 = (df_aggregated$positives_U1_sum  / (df_aggregated$pop_U1_sum))
    
    df_aggregated = df_aggregated[,which(colnames(df_aggregated) %in% c('Run_Number','cases_U1_sum', 'deaths_1_U1_sum', 'deaths_2_U1_sum',
                                                                        'incidence_U1', 'death_rate_1_U1', 'death_rate_2_U1', 
                                                                        'death_rate_mean_U1', 'average_PfPR_U1'))]
    write.csv(df_aggregated, output_filename, row.names=FALSE)
  }
  return(df_aggregated)
}





####################################################################################
# relative simulation burden between two experiments over specified time interval
####################################################################################

get_relative_burden = function(sim_output_filepath, reference_experiment_name, comparison_experiment_name, comparison_scenario_name, start_year, end_year, admin_pop, district_subset='allDistricts', cur_admins='all', LLIN2y_flag=FALSE, overwrite_files=FALSE){
  #'  @description get relative change in U5 and all-age burden when comparing between two simulations in specified years and in specified districts (for all malaria metrics, separate values for each seed)
  #'  @return data frame where each row is a seed and each column is the relative change of different burden metrics, calculated as (comparison - reference) / reference:


  reference_df = get_cumulative_burden(sim_output_filepath=sim_output_filepath, experiment_name=reference_experiment_name, start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  comparison_df = get_cumulative_burden(sim_output_filepath=sim_output_filepath, experiment_name=comparison_experiment_name, start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  # align seeds
  reference_df = reference_df[order(reference_df$Run_Number),]
  comparison_df = comparison_df[order(comparison_df$Run_Number),]
  
  relative_burden_df = data.frame('Run_Number' = reference_df$Run_Number)
  # iterate through burden indicators, calculating relative burden and adding to dataframe
  burden_indicators = colnames(reference_df)[-which(colnames(reference_df) == 'Run_Number')]
  for(bb in 1:length(burden_indicators)){
    relative_burden_cur = (comparison_df[[burden_indicators[bb]]] - reference_df[[burden_indicators[bb]]]) / reference_df[[burden_indicators[bb]]]
    relative_burden_df[[burden_indicators[bb]]] = relative_burden_cur
  }
  relative_burden_df$scenario = comparison_scenario_name
  return(relative_burden_df)
}




get_relative_U1_burden = function(sim_output_filepath, reference_experiment_name, comparison_experiment_name, start_year, end_year, admin_pop, district_subset='allDistricts', cur_admins='all', LLIN2y_flag=FALSE, overwrite_files=FALSE){
  #'  @description get relative change in U1 burden when comparing between two simulations in specified years and in specified districts (for all malaria metrics, separate values for each seed)
  #'  @return data frame where each row is a seed and each column is the relative change of different burden metrics, calculated as (comparison - reference) / reference:
  
  reference_df = get_cumulative_U1_burden(sim_output_filepath=sim_output_filepath, experiment_name=reference_experiment_name, start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  comparison_df = get_cumulative_U1_burden(sim_output_filepath=sim_output_filepath, experiment_name=comparison_experiment_name, start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  # align seeds
  reference_df = reference_df[order(reference_df$Run_Number),]
  comparison_df = comparison_df[order(comparison_df$Run_Number),]
  
  relative_burden_df = data.frame('Run_Number' = reference_df$Run_Number)
  # iterate through burden indicators, calculating relative burden and adding to dataframe
  burden_indicators = colnames(reference_df)[-which(colnames(reference_df) == 'Run_Number')]
  for(bb in 1:length(burden_indicators)){
    relative_burden_cur = (comparison_df[[burden_indicators[bb]]] - reference_df[[burden_indicators[bb]]]) / reference_df[[burden_indicators[bb]]]
    relative_burden_df[[burden_indicators[bb]]] = relative_burden_cur
  }
  relative_burden_df$scenario = 'withIPTi'
  return(relative_burden_df)
}






####################################################################################
# timeseries of simulation burden and/or interventions
####################################################################################

get_burden_timeseries_exp = function(exp_filepath, exp_name, cur_admins, pop_sizes, min_year, max_year, burden_colname, age_plotted, plot_by_month=TRUE){
  #'  @description subset simulation output to appropriate admin and time period, and calculate monthly or annual mean, min, max burdens (for a specified malaria burden metric) across all runs
  #'  @return data frame where each row is a time point and there are columns for the mean, minimum, and maximum burden value across seeds, and also a column for the scenario name
  
  # read in simulation information, subset to appropriate years
  cur_sim_output = fread(paste0(exp_filepath, '/malariaBurden_withAdjustments.csv'))
  cur_sim_output = cur_sim_output[intersect(which(cur_sim_output$year >= min_year), which(cur_sim_output$year <= max_year)),]
  # subset to appropriate admins
  cur_sim_output = cur_sim_output[cur_sim_output$admin_name %in% cur_admins,]  
  
  # merge to get real-world population sizes in each admin
  cur_sim_output = merge(cur_sim_output, pop_sizes, by='admin_name')
  
  # get simulation population denominator
  if(age_plotted == 'U5'){
    cur_sim_output$population = cur_sim_output$Pop_U5
  } else{
    cur_sim_output$population = cur_sim_output$Statistical_Population
  }
  # get the real-world population size in each admin
  if(age_plotted == 'U5'){
    cur_sim_output$true_population = cur_sim_output$pop_size * cur_sim_output$Pop_U5 / cur_sim_output$Statistical_Population
  } else{
    cur_sim_output$true_population = cur_sim_output$pop_size
  }
  
  
  # calculate average over included admins
  if(grepl('PfPR', burden_colname)){
    # get total number of positives (PfPR * true population for all included admins), then divide by total population size of all admins
    cur_sim_output$positives = cur_sim_output[[burden_colname]] * cur_sim_output$true_population
    cur_sim_output_agg_admin = as.data.frame(cur_sim_output) %>% dplyr::select(month, year, date, admin_name, positives, true_population, Run_Number) %>%
      dplyr::group_by(month, year, date, Run_Number) %>%  # take sum of positives and population across admins
      dplyr::summarise(total_positives = sum(positives),
                       total_population = sum(true_population))
    cur_sim_output_agg_admin$burden = cur_sim_output_agg_admin$total_positives / cur_sim_output_agg_admin$total_population
    if(!plot_by_month){
      cur_sim_output_agg_admin = cur_sim_output_agg_admin %>% dplyr::group_by(year, Run_Number) %>% # take average PfPR across months
        dplyr::summarise(burden = mean(burden))
    }
    
  } else{
    # rescale case (or death) numbers to number present in full admin (with true population instead of simulated population size)
    cur_sim_output$true_burden = cur_sim_output[[burden_colname]] * cur_sim_output$true_population / cur_sim_output$population
    # take sum of the number of cases (or deaths) and population sizes acrsoss all included admins
    select_col_names = c('true_burden', 'month', 'year', 'date', 'true_population', 'Run_Number')
    cur_sim_output_agg_admin = as.data.frame(cur_sim_output) %>% dplyr::select(match(select_col_names, names(.))) %>%
      dplyr::group_by(month, year, date, Run_Number) %>%
      dplyr::summarise_all(sum)
    cur_sim_output_agg_admin$burden = cur_sim_output_agg_admin$true_burden / cur_sim_output_agg_admin$true_population * 1000
    if(!plot_by_month){
      cur_sim_output_agg_admin = cur_sim_output_agg_admin %>% dplyr::group_by(year, Run_Number) %>% # take average PfPR across months
        dplyr::summarise(burden = sum(burden))
    }
  }
  
  # take average, max, and min burdens across simulation seeds
  if(plot_by_month){
    cur_sim_output_agg = as.data.frame(cur_sim_output_agg_admin) %>% dplyr::select(month, year, date, burden) %>%
      dplyr::group_by(month, year, date) %>%
      dplyr::summarise(mean_burden = mean(burden),
                       max_burden = max(burden),
                       min_burden = min(burden))
  } else{
    cur_sim_output_agg = as.data.frame(cur_sim_output_agg_admin) %>% dplyr::select(year, burden) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(mean_burden = mean(burden),
                       max_burden = max(burden),
                       min_burden = min(burden))
  }
  
  cur_sim_output_agg$scenario = exp_name
  return(cur_sim_output_agg)
}




get_net_use_timeseries_exp = function(exp_filepath, exp_name, cur_admins, pop_sizes, min_year, max_year, indoor_protection_fraction, plot_by_month=TRUE){
  #'  @description subset simulation output to appropriate admin(s) and time period, and calculate monthly net usage, net distribution, and IRS coverage across all runs
  #'  @return data frame where each row is a time point and there are columns for the mean, minimum, and maximum ITN coverage across seeds, as well as columns for mean nets distributed and IRS coverages, and also a column for the scenario name
  
  # read in simulation information, merge to single dataframe, subset to appropriate years
  net_use_all = fread(paste0(exp_filepath, '/MonthlyUsageLLIN.csv'))
  net_dist_all = fread(paste0(exp_filepath, '/monthly_Event_Count.csv'))
  net_dist_all = net_dist_all[,c('admin_name', 'date', 'Run_Number', 'Bednet_Got_New_One', 'Received_IRS')]
  net_use_all = merge(net_use_all, net_dist_all, by=c('admin_name', 'date', 'Run_Number'))
  net_use_all$date = as.Date(net_use_all$date)
  net_use_all$year = lubridate::year(net_use_all$date)
  net_use_all = net_use_all[intersect(which(net_use_all$year >= min_year), which(net_use_all$year <= max_year)),]
  
  # subset to appropriate admins
  net_use_all = net_use_all[net_use_all$admin_name %in% cur_admins,]  
  colnames(net_use_all) = gsub(' ','.',colnames(net_use_all))
  
  # rescale numbers of nets received, nets used, and IRS received to the true population in each admin, rather than the simulated population size
  net_use_all = merge(net_use_all, pop_sizes, by='admin_name')
  colnames(net_use_all)[colnames(net_use_all) == 'pop_size'] = 'true_population'
  net_use_all$Bednet_Using = net_use_all$Bednet_Using * net_use_all$true_population / net_use_all$Statistical.Population
  net_use_all$Bednet_Got_New_One = net_use_all$Bednet_Got_New_One * net_use_all$true_population / net_use_all$Statistical.Population
  net_use_all$Received_IRS = net_use_all$Received_IRS * net_use_all$true_population / net_use_all$Statistical.Population
  
  # get sum of numbers across all included admins (keeping runs and months separate)
  net_use_sums = net_use_all %>% dplyr::select(year, date, Run_Number, Bednet_Using, Bednet_Got_New_One, Received_IRS, true_population) %>% dplyr::group_by(date, year, Run_Number) %>%
    dplyr::summarise_all(sum)
  net_use_sums$coverage = net_use_sums$Bednet_Using / net_use_sums$true_population / indoor_protection_fraction
  net_use_sums$new_net_per_cap = net_use_sums$Bednet_Got_New_One / net_use_sums$true_population
  net_use_sums$irs_per_cap = net_use_sums$Received_IRS / net_use_sums$true_population
  
  # get average coverage accross months in a year for the annual report
  if(!plot_by_month){
    net_use_sums = net_use_sums %>% dplyr::group_by(year, Run_Number) %>% # take average across months
      dplyr::summarise(coverage = mean(coverage),
                       new_net_per_cap = sum(new_net_per_cap),
                       irs_per_cap = sum(irs_per_cap))
  }
  
  # take average, max, and min burdens across simulation seeds
  if(plot_by_month){
    net_use_agg = as.data.frame(net_use_sums) %>% dplyr::select(year, date, coverage, new_net_per_cap, irs_per_cap) %>%
      dplyr::group_by(year, date) %>%
      dplyr::summarise(mean_coverage = mean(coverage),
                       max_coverage = max(coverage),
                       min_coverage = min(coverage),
                       new_net_per_cap = mean(new_net_per_cap),
                       irs_per_cap = mean(irs_per_cap))
  } else{
    net_use_agg = as.data.frame(net_use_sums) %>% dplyr::select(year, coverage, new_net_per_cap, irs_per_cap) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(mean_coverage = mean(coverage),
                       max_coverage = max(coverage),
                       min_coverage = min(coverage),
                       new_net_per_cap = mean(new_net_per_cap),
                       irs_per_cap = mean(irs_per_cap))
  }
  
  net_use_agg$scenario = exp_name
  return(net_use_agg)
}


# ----- CM intervention coverage ----- #
get_cm_timeseries_exp = function(cm_filepath, pop_sizes, end_year, exp_name, cur_admins, min_year, plot_by_month=TRUE){
  
  cm_input = read.csv(cm_filepath)
  # subset to appropriate admins
  cm_input = cm_input[cm_input$admin_name %in% cur_admins,]  
  
  # CM is sometimes repeated for several years but only listed once; change to repeate the appropriate number of times
  cm_input$years_repeated = cm_input$duration/365
  if(any(cm_input$years_repeated>1)){
    cur_cm_years = unique(cm_input$year)
    for(yy in cur_cm_years){
      # get first instance of this year
      cur_year = cm_input[cm_input$year==yy,]
      if(cur_year$years_repeated[1]>1){
        for(rr in 1:(cur_year$years_repeated[1] - 1)){
          temp_year = cur_year
          temp_year$year = cur_year$year + rr
          temp_year$simday = cur_year$simday + rr*365
          cm_input = rbind(cm_input, temp_year)
        }
      }
    }
  }
  if(any(cm_input$duration==-1) & (max(cm_input$year)<end_year)){
    cm_repeated = cm_input[cm_input$duration == -1,]
    for(rr in 1:(end_year - cm_repeated$year[1])){
      temp_year = cm_repeated
      temp_year$year = cm_repeated$year + rr
      temp_year$simday = cm_repeated$simday + rr*365
      cm_input = rbind(cm_input, temp_year)
    }
  }
  # if there are multiple values in a single year (for a single DS/LGA), take the mean of those values
  cm_input <- cm_input %>% group_by(year, admin_name, seed) %>%
    summarise_all(mean) %>% ungroup()
  
  cm_input = cm_input[intersect(which(cm_input$year >= min_year), which(cm_input$year <= end_year)),]
  
  
  # get population-weighted CM coverage across admins
  cm_input = merge(cm_input, pop_sizes, by='admin_name')
  cm_input$multiplied_U5_cm = cm_input$U5_coverage * cm_input$pop_size
  
  # get sum of population sizes and multiplied CM coverage across included admins
  cm_input_agg_admin <- cm_input %>% dplyr::select(year, seed, multiplied_U5_cm, pop_size) %>% group_by(year, seed) %>%
    summarise_all(sum) %>% ungroup()
  # get population-weighted U5 coverage across all included admin by dividing by su  of population sizes
  cm_input_agg_admin$U5_coverage = cm_input_agg_admin$multiplied_U5_cm / cm_input_agg_admin$pop_size
  
  
  # take average, max, and min burdens across simulation seeds
  if(plot_by_month){
    # subdivide year values and add dates
    # date dataframe
    included_years = unique(cm_input_agg_admin$year)
    all_months = as.Date(paste0(rep(included_years, each=12),'-',c('01','02','03','04','05','06','07','08','09','10','11','12'), '-01' ))
    date_df = data.frame(year=rep(included_years, each=12), date=all_months)
    cm_input_agg_admin_monthly = merge(cm_input_agg_admin, date_df, by='year', all.x=TRUE, all.y=TRUE)
    cm_agg = as.data.frame(cm_input_agg_admin_monthly) %>% dplyr::select(year, date, U5_coverage) %>%
      dplyr::group_by(year, date) %>%
      dplyr::summarise(mean_coverage = mean(U5_coverage),
                       max_coverage = max(U5_coverage),
                       min_coverage = min(U5_coverage))
  } else{
    cm_agg = as.data.frame(cm_input_agg_admin) %>% dplyr::select(year, U5_coverage) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(mean_coverage = mean(U5_coverage),
                       max_coverage = max(U5_coverage),
                       min_coverage = min(U5_coverage))
  }
  
  cm_agg$scenario = exp_name
  return(cm_agg)
  
}







