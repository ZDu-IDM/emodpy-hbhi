#################################################################################################
# get_data_functions.R
# Monique Ambrose
# April 2020
# 
# Purpose: read in and format data that will be used for comparing SMC impact in dhis2 vs simulations
# 
##################################################################################################
library(data.table)
library(dplyr)
library(lubridate)
library(haven)


get_smc_data = function(smc_info_filename){
  smc_info = fread(smc_info_filename)
  # subset to first round
  if('round' %in% colnames(smc_info)){
    smc_info = smc_info[smc_info$round == 1,]
  }
  if(all(c('date_first_round', 'year','admin_name' ,'coverage_high_access_U5', 'high_access_U5', 'coverage_low_access_U5') %in% colnames(smc_info))){
    # smc_info$month = lubridate::month(smc_info$date_first_round)
    smc_info$received_smc = TRUE
    smc_info$coverage_U5 = smc_info$coverage_high_access_U5 * smc_info$high_access_U5 + smc_info$coverage_low_access_U5 * (1-smc_info$high_access_U5)
    smc_info = smc_info %>% dplyr::select(year, admin_name, received_smc, coverage_U5, date_first_round) %>%
      mutate(month_first_round = lubridate::month(as.Date(date_first_round)))
    
    # calculate the number of years SMC has been given in a DS
    smc_info$SMC_years = 0
    for(yy in sort(unique(smc_info$year))){
      ds_in_yy = unique(smc_info$admin_name[smc_info$year == yy])
      for(i_ds in 1:length(ds_in_yy)){
        prev_times_smc = smc_info$SMC_years[intersect(which(smc_info$admin_name == ds_in_yy[i_ds]), which(smc_info$year == (yy-1)))]
        if(length(prev_times_smc)>0){  # number of times received SMC is one plus previous time
          smc_info$SMC_years[intersect(which(smc_info$admin_name == ds_in_yy[i_ds]), which(smc_info$year == yy))] = smc_info$SMC_years[intersect(which(smc_info$admin_name == ds_in_yy[i_ds]), which(smc_info$year == (yy-1)))][1] +1
        } else{  #  this is the first time DS receives SMC (assumes there are never instances where SMC is skipped for a year)
          smc_info$SMC_years[intersect(which(smc_info$admin_name == ds_in_yy[i_ds]), which(smc_info$year == yy))] = 1
        }
      }
    }
  } else {
    warning("Expected columns missing from SMC file. Needed columns: 'date_first_round', 'year','admin_name', 'coverage_high_access_U5', 'high_access_U5', 'coverage_low_access_U5'")
    smc_info = NA
  }
  return(smc_info)
}


get_dhis2_data = function(dhis2_filepath, pop_arch){
  dhis2_data = read_dta(dhis2_filepath)
  
  if(all(c('adm2', 'year', 'month', 'conf_u5', 'conf_ov5') %in% colnames(dhis2_data))){
    # make sure the admin names are consistent
    dhis2_data$admin_name = dhis2_data$adm2
    dhis2_data = standardize_admin_names_in_df(target_names_df=pop_arch, origin_names_df=dhis2_data, target_names_col='admin_name', origin_names_col='admin_name')

    # retain relevant columns
    dhis2_data_2 = dhis2_data %>%
      dplyr::select(year, month, admin_name, 
                    # allout_u5, allout_ov5,
                    # test_rdt_u5, test_rdt_ov5,
                    conf_u5, conf_ov5)

    # drop any rows with NA values
    dhis2_data_drop_na = na.omit(dhis2_data_2)
    
    # often 0 is entered for u5 alongside much larger numbers for ov5, almost certainly a data-records issue 
    #   rather than true 0s. No perfect way to deal with this, but I think we'll get less bias if we remove all rows that contain any zeros. 
    dhis2_data_drop_0 = dhis2_data_drop_na %>% filter(conf_u5>0) %>% filter(conf_ov5>0) # %>% filter(maltreat_u5>0) %>% filter(maltreat_ov5>0)

    # aggregate counts within each DS-month-year (across multiple health facilities)
    dhis2_data_sums <- dhis2_data_drop_0 %>% group_by(year, month, admin_name) %>% 
      summarise_all(sum) %>% ungroup()
  } else{
    warning("Expected columns missing from DHIS2 case data.")
    dhis2_data_sums=NA
  }
  return(dhis2_data_sums)
}


get_sim_data = function(sim_output_filepath, rel_prob_cm_o5, num_nmf_month_u5, num_nmf_month_o5, years_included){
 #'
 #'
 #' @param sim_output_stem_filepath
 #' @param rel_prob_cm_o5 the probability someone O5 seeks treatment for fever relative to individuals U5
 #' @param num_nmf_month_u5 estimated monthly probability of NMF for U5
 #' @param num_nmf_month_o5 estimated monthly probability of NMF for O5
 #' @param years_included years to filter to from simulation output
  
  # simulation output for what we *believe* the incidence and prevalence to have been in previous years
  sim_burden_0 = fread(sim_output_filepath, check.names=TRUE)
  # get average across all runs (for each month, year, and  DS)
  sim_burden_0 = sim_burden_0  %>%  filter(year %in% years_included) %>%
    dplyr::select(year, month, admin_name, Run_Number, 
                  Statistical_Population, PfPR_MiP_adjusted, New_Clinical_Cases,
                  Pop_U5, PfPR_U5, New_clinical_cases_U5)
  sim_burden = sim_burden_0 %>% group_by(month, year, admin_name) %>% summarise_all(mean) %>% ungroup() 
  
  # adjust for the assumed CM rates for each age in each DS through time in simulations
  sim_burden$frac_U5_sim_cm_adjusted = sim_burden$New_clinical_cases_U5 / ((sim_burden$New_Clinical_Cases - sim_burden$New_clinical_cases_U5) * rel_prob_cm_o5 + 
                                                                             sim_burden$New_clinical_cases_U5)
  
  # adjust for treatment-seeking for NMFs
  # assumptions: 
  #  - rate of NMF is constant through the year
  #  - rate of treatment-seeking for fever is the same for NMF as for malaria symptoms
  #  - number of NMF that will be malaria positive estimated as number of NMF multiplied by prevalence (though 
  #      this may not be strictly correct given diagnostics may miss low-density infections or pick up on 
  #      recently cleared infections)
  sim_burden$NMF_u5 = num_nmf_month_u5 * sim_burden$Pop_U5
  sim_burden$NMF_o5 = num_nmf_month_o5 * (sim_burden$Statistical_Population - sim_burden$Pop_U5)
  
  # probability a NMF corresponds with a malaria infection
  sim_burden$m_NMF_u5 = sim_burden$NMF_u5 * sim_burden$PfPR_U5
  # prevalence in o5 population not recorded, but we can calculate it from PfPR_U5, pop_u5, and full population.
  #     PfPR_all = (PfPR_u5 * pop_u5 + PfPR_o5 * pop_o5)/pop_all --> PfPR_o5 = (PfPR_all * pop_all - PfPR_u5 * pop_u5)/pop_o5
  sim_burden$PfPR_O5 = (sim_burden$PfPR_MiP_adjusted * sim_burden$Statistical_Population - sim_burden$PfPR_U5 * sim_burden$Pop_U5) / (sim_burden$Statistical_Population - sim_burden$Pop_U5)
  sim_burden$m_NMF_o5 = sim_burden$NMF_o5 * sim_burden$PfPR_O5
  
  # calculate the relative number of individuals seeking treatment for fever and testing positive for malaria who are U5
  sim_burden$frac_U5_sim_cm_nmf_adjusted = (sim_burden$New_clinical_cases_U5 + sim_burden$m_NMF_u5) / 
    ((sim_burden$New_Clinical_Cases - sim_burden$New_clinical_cases_U5 + sim_burden$m_NMF_o5) * rel_prob_cm_o5 + 
       (sim_burden$New_clinical_cases_U5 + sim_burden$m_NMF_u5) * 1)
  
  sim_burden$num_U5_treat_sim = (sim_burden$New_clinical_cases_U5 + sim_burden$m_NMF_u5)
  sim_burden$num_O5_treat_sim = (sim_burden$New_Clinical_Cases - sim_burden$New_clinical_cases_U5 + sim_burden$m_NMF_o5) * rel_prob_cm_o5
  
  
  return(sim_burden)
}

# # check outputs
# mm = 8
# # number of NMF
# mean(sim_burden$NMF_u5[(sim_burden$month == mm) & (sim_burden$year==2018)])
# mean(sim_burden$NMF_o5[(sim_burden$month == mm) & (sim_burden$year==2018)])
# # number of NMF with malaria
# mean(sim_burden$m_NMF_u5[(sim_burden$month == mm) & (sim_burden$year==2018)])
# mean(sim_burden$m_NMF_o5[(sim_burden$month == mm) & (sim_burden$year==2018)])
# # number clinical malaria
# mean(sim_burden$New_clinical_cases_U5[(sim_burden$month == mm) & (sim_burden$year==2018)])
# mean(sim_burden$New_Clinical_Cases[(sim_burden$month == mm) & (sim_burden$year==2018)] - sim_burden$New_clinical_cases_U5[(sim_burden$month == mm) & (sim_burden$year==2018)])
# # number of malaria cases, unadjuested for treatment-seeking rates
# mean(sim_burden$m_NMF_u5[(sim_burden$month == mm) & (sim_burden$year==2018)] + sim_burden$New_clinical_cases_U5[(sim_burden$month == mm) & (sim_burden$year==2018)])
# mean(sim_burden$m_NMF_o5[(sim_burden$month == mm) & (sim_burden$year==2018)] + sim_burden$New_Clinical_Cases[(sim_burden$month == mm) & (sim_burden$year==2018)] - sim_burden$New_clinical_cases_U5[(sim_burden$month == mm) & (sim_burden$year==2018)])
# 
# # fraction of malaria cases in U5, unadjuested for treatment-seeking rates
# mean((sim_burden$m_NMF_u5[(sim_burden$month == mm) & (sim_burden$year==2018)] + sim_burden$New_clinical_cases_U5[(sim_burden$month == mm) & (sim_burden$year==2018)]) / 
#        (sim_burden$m_NMF_u5[(sim_burden$month == mm) & (sim_burden$year==2018)]  + sim_burden$m_NMF_o5[(sim_burden$month == mm) & (sim_burden$year==2018)] + sim_burden$New_Clinical_Cases[(sim_burden$month == mm) & (sim_burden$year==2018)]))
# 
# # fraction of cases in U5, adjusted to include NPF and uneven rates of treatment-seeking
# mean(sim_burden$frac_U5_sim_cm_nmf_adjusted[sim_burden$month == mm])





merge_smc_datasets = function(smc_info, dhis2_data_sums, sim_burden, use_conf=FALSE){
  # combine into single dataframe containing columns:
  #  - year
  #  - month
  #  - admin_name
  #  - received_smc: was SMC given this month or the previous month?
  #  - frac_U5_dhis2: DHIS2 fraction of cases U5
  #  - frac_U5_sim: simulation fraction of cases U5
  #  - frac_U5_sim_cm_adjusted: simulation fraction of cases U5, adjusted for CM
  #  - frac_U5_sim_cm_nmf_adjusted: simulation fraction of cases U5, adjusted for CM and NMF
  #  - New_clinical_cases_U5: number of new malaria clincal cases for U5
  #  - New_Clinical_Cases: number of new malaria clincal cases for all ages
  #  - m_NMF_u5: estimated number of NMFs that are infected with malaria for U5
  #  - m_NMF_o5: estimated number of NMFs that are infected with malaria for O5

  merged_df  = left_join(sim_burden, dhis2_data_sums)
  merged_df$frac_U5_sim = merged_df$New_clinical_cases_U5 / merged_df$New_Clinical_Cases
  # if(use_conf){
  merged_df$frac_U5_dhis2 = merged_df$conf_u5 / (merged_df$conf_u5 + merged_df$conf_ov5)
  # } else{
  #   merged_df$frac_U5_dhis2 = merged_df$maltreat_u5 / (merged_df$maltreat_u5 + merged_df$maltreat_ov5)
  # }
  # merge in SMC info
  merged_df = left_join(merged_df, smc_info)
  merged_df$received_smc[is.na(merged_df$received_smc)] = FALSE
  merged_df$SMC_years[is.na(merged_df$SMC_years)] = 0
  merged_df$coverage_U5[is.na(merged_df$coverage_U5)] = 0
  
  return(merged_df)
}
