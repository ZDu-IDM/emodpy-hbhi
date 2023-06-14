# functions_smc_sim_fit
# functions to help analyze SMC simulations and calculate how well they match reference data
library(data.table)
library(dplyr)

# combine simulation results from across multiple experiments, preferably with the same columns
combine_experiments = function(sim_output_base_filepath, experiment_names){
  combined_sims = data.frame()
  for(ii in 1:length(experiment_names)){
    cur_exp = fread(paste0(sim_output_base_filepath,'/', experiment_names[ii], '/dailyNewInfections.csv'))
    cur_exp$expt_num = ii
    if(all(colnames(combined_sims) == colnames(cur_exp))){
      combined_sims = rbind(combined_sims, cur_exp)
    } else{
      combined_sims = merge(combined_sims, cur_exp)
    }
  }
  return(combined_sims)
}




# process the simulation results to get mean values for each set of parameters and efficacy compared to when the drug parameters are zero
analyze_treat_efficacy = function(sim_output, drug_day, comparison_day=28, drug_factors=c('Sulf_C50',  'Sulf_kill', 'Pyri_C50', 'Pyri_kill', 'Amod_C50', 'Amod_kill')){
  # subset to after treatment and reset time (now refers to days after treatment)
  sim_output2 = sim_output[sim_output$Day == drug_day + comparison_day]

  # get mean values for same parameter sets across runs
  colnames_include = intersect(colnames(sim_output2), c(drug_factors, 'drug_combo'))
  mean_sim_output = sim_output2 %>% group_by(across(all_of(colnames_include))) %>%
    summarise_all(mean)
  
  # subset to control no-drug scenario for comparison base
  control_sim_output = mean_sim_output[which(rowSums(mean_sim_output[,colnames(mean_sim_output) %in% drug_factors]) == 0),]
  
  if(nrow(control_sim_output)==1) {
    # compare treatment success rates with versus without drug to get efficacy
    mean_sim_output$pe_true_prevalence = 1 - mean_sim_output$True_Prevalence / control_sim_output$True_Prevalence
    mean_sim_output$pe_micro_prevalence = 1 - mean_sim_output$Micros_Prevalence / control_sim_output$Micros_Prevalence
    return(mean_sim_output)
  } else{
    warning('An issue has arisen in the "analyze_treat_efficacy() function with the control simulations. Please check.')
    return(NA)
  }
}





# process the simulation results to get mean values for each set of parameters and efficacy compared to when the drug parameters are zero
analyze_prevent_efficacy = function(sim_output, drug_day, drug_factors=c('Sulf_C50',  'Sulf_kill', 'Pyri_C50', 'Pyri_kill', 'Amod_C50', 'Amod_kill')){
  # subset to after treatment and reset time (now refers to days after treatment)
  sim_output2 = sim_output[sim_output$Day > drug_day]
  sim_output2$Day = sim_output2$Day - drug_day
  
  # get total cases that happened after challenge
  colnames_include = intersect(colnames(sim_output2), c('Run_Number', 'expt_num', drug_factors, 'challenge_interval', 'drug_combo'))
  sim_output3 = sim_output2 %>% group_by(across(all_of(colnames_include))) %>%
    summarise(total_new_clinical=sum(New_Clinical_Cases),
              total_new_infect=sum(New_Infections),
              Pop=mean(Pop))
  
  # get cases per person (before aggregating across runs and experiments in case population size changes)
  sim_output3$new_clinical_pp = sim_output3$total_new_clinical / sim_output3$Pop
  sim_output3$new_infect_pp = sim_output3$total_new_infect / sim_output3$Pop
  
  # get mean values for same parameter sets across runs and experiments
  colnames_include = intersect(colnames(sim_output2), c(drug_factors, 'challenge_interval', 'drug_combo'))
  mean_sim_output = sim_output3 %>% group_by(across(all_of(colnames_include))) %>%
    summarise_all(mean)
  
  # subset to control no-drug scenario for comparison base
  control_sim_output = mean_sim_output[which(rowSums(mean_sim_output[,colnames(mean_sim_output) %in% drug_factors]) == 0),]
  if('drug_combo' %in% colnames(control_sim_output)){
    control_sim_output = control_sim_output[control_sim_output$drug_combo=='none',]
  }
  colnames(control_sim_output)[colnames(control_sim_output)=='new_clinical_pp'] = 'control_new_clinical_pp'
  colnames(control_sim_output)[colnames(control_sim_output)=='new_infect_pp'] = 'control_new_infect_pp'
  control_sim_output = control_sim_output[,which(colnames(control_sim_output) %in% c('challenge_interval', 'control_new_clinical_pp', 'control_new_infect_pp'))]
  
  num_challenge_intervals = length(unique(mean_sim_output$challenge_interval))
  if(nrow(control_sim_output)==num_challenge_intervals) {
    # merge the control results into the full simulation results
    mean_sim_output2 = merge(mean_sim_output, control_sim_output, all.x=TRUE, by=c('challenge_interval'))
    
    # compare number of cases with versus without drug to get protective efficacy
    mean_sim_output2$pe_clinical = 1 - mean_sim_output2$new_clinical_pp / mean_sim_output2$control_new_clinical_pp
    mean_sim_output2$pe_infect = 1 - mean_sim_output2$new_infect_pp / mean_sim_output2$control_new_infect_pp
    return(mean_sim_output2)
  } else{
    warning('An issue has arisen in the "analyze_treat_efficacy() function with the control simulations. Please check.')
    return(NA)
  }

    
}


# get weighted mean PE during an interval (to match with reference)
#   assumes that the challenges days in the simulations were already selected to avoid skewing estimates (i.e., there aren't more/fewer simulations run with challenge days early in the challenge period than late in the challenge period)
get_pe_in_time_period = function(sim_results, challenge_day_min=0, challenge_day_max=28, drug_factors){
  # subset to challenge trials within this interval
  eligible_rows = which(sim_results$challenge_interval %in% challenge_day_min:challenge_day_max)
  if('drug_combo' %in% colnames(sim_results)){
    eligible_rows = intersect(eligible_rows, which(sim_results$drug_combo != 'none'))
  } else{
    eligible_rows = intersect(eligible_rows, which(rowSums(sim_results[,drug_factors])>0))
  }
  
  eligible_results = sim_results[eligible_rows,]
  
  # get mean across all simulations with the same parameter set in this challenge period
  colnames_include = intersect(colnames(eligible_results), c(drug_factors, 'drug_combo'))
  period_pe_sim = eligible_results %>% group_by(across(all_of(colnames_include))) %>%
    summarise_all(mean)
  
  return(period_pe_sim)
}
