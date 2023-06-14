# future_projection_intervention_inputs.R

# these inputs are specific to a particular set of Burundi scenarios - these scripts should be replaced/edited depending on the use case


# needed inputs for current analyses:
#  - CM (continue coverage from final year in "to-present" simulations)
#  - NMF (same as in "to-present" run)
#  - SMC
#  - PMC
#  - RTS,S
#  - IPTp
#  - IRS
#     - none
#  - LLIN mass distribution
#     - LLINS, mass distribution: assume same coverage and block/kill as most recent distribution; use distribution year from future scenarios sheet
#     - LLINS, mass distribution: assume same coverage as most recent distribution for each LGA; use net type and distribution years from future scenarios sheet (several versions); consider doing several versions of resistance levels
#  - LLIN routine distribution
#     - LLINs, routine distribution: assume same coverage and block/kill as final year of to-present presentations (I believe these were all assuming pyrethroid nets)
#     - LLINs, routine distribution: assume same coverage as in final year of to-present presentations and use net type from future scenarios sheet (several versions)

library(lubridate)
library(reshape2)
library(dplyr)

hbhi_dir = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration/hbhi_nigeria/snt_2022'
script_dir = 'C:/Users/moniqueam/Documents/malaria-nga-snt22'
base_sim_input_dir = paste0(hbhi_dir, '/simulation_inputs')

source(paste0(script_dir,'/data_processing/setup_inputs/add_hut_trial_mort_block.R'))
source(paste0(script_dir,'/standardize_admin_names.R'))

# simulation inputs for "to-present" simulations
intervention_files_past = read.csv(paste0(base_sim_input_dir, '/_intervention_file_references/Interventions_to_present.csv'))
# create directory for projections
ifelse(!dir.exists(paste0(base_sim_input_dir, '/interventions_projections')), dir.create(paste0(base_sim_input_dir, '/interventions_projections')), FALSE)

projection_start_year = 2022  # assumed to be Jan 1
future_start_year = 2023  # any years between projecton_start_year and future_start_year have already been implemented, so should try to use existing data, if available
projection_end_year = 2030  # assumed to be Dec 31

# WHO scenario information for GF
gf_scenarios = read.csv('C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration/nigeria_who/NGA_2022_SNT/future_projection_scenarios_20230225.csv')
colnames(gf_scenarios)[colnames(gf_scenarios)=='adm1'] = 'State'
colnames(gf_scenarios)[colnames(gf_scenarios)=='adm2'] = 'admin_name'

# get dataframe with standardized admin names, states, archetype, and population information
ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
archetype_info = read.csv(ds_pop_df_filename)
# standardize names
gf_scenarios$admin_name = standardize_admin_names_in_vector(target_names=archetype_info$LGA, origin_names=gf_scenarios$admin_name)


# insecticide resistance
mortality_df_name = paste0(hbhi_dir,'/simulation_inputs/intermediate_files/insecticide_resistance/permethrin_mortality_admin_estimates.csv')

# for versions with increased coverage, what should the coverage be increasing to and over how many days
coverage_increase_target = 0.6  # the minimum intervention coverage
coverage_increase_start_day = 180 + 365*(future_start_year-projection_start_year)  # day of simulation when increase in coverage begins
coverage_increase_per_month = 0.10  # amount by which coverage increases each 30 days for scenarios with increasing coverage, up to maximum of original coverage or coverage_increase_target 
num_increase_periods = ceiling(coverage_increase_target / coverage_increase_per_month) # how many times do we need to do the increase to be sure that all LGAs will reach the target coverage
period_length = 30  # days in each increase period


print_coverages = TRUE

################################################################################
# CM: continue coverage from final year into future projections
################################################################################
# read in 2010-2021 input, subset to final year, update year and simday to first day of future projections, and set duration to -1
cm_past = read.csv(paste0(base_sim_input_dir, '/', intervention_files_past$CM_filename[1],'.csv'))
use_max_past_cm_coverage = TRUE
if(use_max_past_cm_coverage){
  # for future projections, assume a 'best case scenario' from past coverage, with each admin taking the max of prior years.
  cm_past_sub = cm_past %>% dplyr::select(admin_name, U5_coverage, adult_coverage, severe_coverage) %>%
    group_by(admin_name) %>%
    summarise_all(max, na.rm=TRUE) %>%
    ungroup()
} else{
  # use coverage from final year
  cm_past_sub = cm_past[cm_past$year == max(cm_past$year),]
}
cm_past_sub$year = projection_start_year
cm_past_sub$simday = 0
cm_past_sub$duration = -1
if(nrow(cm_past_sub) == nrow(distinct(cm_past_sub))) {
  write.csv(cm_past_sub, paste0(base_sim_input_dir, '/interventions_projections/cm_bau.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the CM input file.')
}

if(print_coverages){
  print('Case management')
  cov_all = cm_bau$U5_coverage
  print(paste0('.... Continuing coverage: ', length(unique(cm_bau$admin_name)), ' LGAs w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
}


#### increased coverage scenarios
#  effective coverage in U5 increased to coverage_increase_target; other coverages adjusted accordingly
cm_bau = read.csv(paste0(base_sim_input_dir, '/interventions_projections/cm_bau.csv'))
cm_bau$year = floor(projection_start_year + coverage_increase_start_day/365)
cm_bau$eventual_max_coverage = sapply(cm_bau$U5_coverage, max, coverage_increase_target) # eventual coverage will be the max of the original coverage and coverage_increase_target
# create dataframe with all the increasing campaigns, starting with continuing current coverage until the period of increases begins
cm_increaseCov = cm_bau
cm_increaseCov$duration = coverage_increase_start_day

# iterate through increase periods, adding rows for the new campaigns with updated coverage - be sure not to have overlapping campaigns or gaps in campaigns
for(ii in 1:num_increase_periods){
  cur_campaign = cm_bau
  cur_campaign$U5_coverage = pmin((cm_bau$U5_coverage + coverage_increase_per_month*ii), cm_bau$eventual_max_coverage)
  cur_campaign$adult_coverage = cur_campaign$U5_coverage
  cur_campaign$severe_coverage = sapply(cur_campaign$U5_coverage*2, min, max(0.8, coverage_increase_target))  # maximum severe coverage was 0.8 in BAU
  cur_campaign$simday = coverage_increase_start_day + period_length * (ii-1)
  if(ii<num_increase_periods){
    cur_campaign$duration = period_length
  } else{
    cur_campaign$duration = -1
  }
  cm_increaseCov = rbind(cm_increaseCov, cur_campaign)
}
cm_increaseCov$year = floor(cm_increaseCov$simday/365) + projection_start_year
if(nrow(cm_increaseCov) == nrow(distinct(cm_increaseCov))){
  write.csv(cm_increaseCov, paste0(base_sim_input_dir, '/interventions_projections/cm_', round(100*coverage_increase_target),'cov.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the CM input file.')
}





################################################################################
# SMC: continue coverage from final year into future projections
################################################################################
smc_past = read.csv(paste0(base_sim_input_dir, '/', intervention_files_past$SMC_filename[1],'.csv'))
smc_past$date_first_round = as.Date(smc_past$date_first_round)
smc_past$doy_first_round = as.numeric(yday(smc_past$date_first_round))  # get the day of the year when SMC campaign starts

# get coverages from final year of implementation in each admin, if available, otherwise, use national average
# coverage and round info columns
smc_cov_columns = c('round',"coverage_high_access_U5", "coverage_low_access_U5", "coverage_high_access_5_10", "coverage_low_access_5_10",  "high_access_U5", "high_access_5_10", "adherence", "doy_first_round" )
smc_past_latest = smc_past[smc_past$year == max(smc_past$year),c(smc_cov_columns, 'admin_name')]
national_average_by_round = smc_past_latest[,smc_cov_columns] %>% 
  group_by(round) %>%
  summarise_all(mean, na.rm=TRUE)
# assume only four rounds
national_average_by_round = national_average_by_round[national_average_by_round$round <=4,]
national_average_by_round$doy_first_round = round(national_average_by_round$doy_first_round[national_average_by_round$round==1])

# determine which admins receive SMC in future projections
# NSP
smc_nsp = gf_scenarios[,c('admin_name', 'smc_nsp')]
unique(smc_nsp$smc_nsp)
smc_nsp$receive_smc = sapply(smc_nsp$smc_nsp, function(x){!grepl('No SMC', x)})
# fund
smc_fund = gf_scenarios[,c('admin_name', 'smc_fund')]
unique(smc_fund$smc_fund)
smc_fund$receive_smc = sapply(smc_fund$smc_fund, function(x){!grepl('No SMC', x)})
# imp
smc_imp = gf_scenarios[,c('admin_name', 'smc_imp')]
unique(smc_imp$smc_imp)
smc_imp$receive_smc = sapply(smc_imp$smc_imp, function(x){!grepl('No SMC', x)})
# create list of net scenarios with corresponding vector describing the name of each (allowing for looping later)
scenario_smc_names = c('nsp', 'fund', 'imp')
scenario_smc_status_list = list(smc_nsp, smc_fund, smc_imp)

# add in SMC from recent campaigns that will be in future projection simulations - assume that for 2022, was implemented in admins where smc_imp$receive_smc is true
recent_smc = smc_imp



# iterate through scenarios, preparing input files
for(ii in 1:length(scenario_smc_status_list)){

  # check whether all admins have received SMC in the to-present simulations (if so, use same coverages and rounds)
  # get admin names that will receive SMC in future projection simulations
  future_admins = unique(scenario_smc_status_list[[ii]][['admin_name']][scenario_smc_status_list[[ii]][['receive_smc']]])
  recent_admins = unique(recent_smc$admin_name[recent_smc$receive_smc])
  all_smc_admins = unique(c(future_admins, recent_admins))
  if(any(!(all_smc_admins %in% smc_past_latest$admin_name))){
    new_admins = data.frame(admin_name=all_smc_admins[which(!(all_smc_admins %in% smc_past_latest$admin_name))])
    # merge in the national average coverage dataframe with the new admins
    smc_projection_coverages = merge(new_admins, national_average_by_round, all=TRUE)
    smc_projection_coverages = merge(smc_projection_coverages, smc_past_latest, all=TRUE)
  } else{
    smc_projection_coverages = smc_past_latest
  }
  # subset to admins included in recent distribution (just 2022) and admins that will have annual SMC campaigns for all remaining years of future projections
  smc_future_coverages = smc_projection_coverages[smc_projection_coverages$admin_name %in% future_admins, ]
  smc_recent_coverages = smc_projection_coverages[smc_projection_coverages$admin_name %in% recent_admins, ]
  
  # add the years for future campaigns
  future_years = data.frame(year=future_start_year:projection_end_year)
  smc_future_coverages = merge(smc_future_coverages, future_years, all=TRUE)
  smc_recent_coverages$year = 2022
  smc_projection_campaign = merge(smc_future_coverages, smc_recent_coverages, all=TRUE)
  
  # add the day of simulation for each SMC campaign (based on round number, day of year for first round, and year)
  smc_projection_campaign$simday = (smc_projection_campaign$year - projection_start_year) * 365 + smc_projection_campaign$doy_first_round + 28*(smc_projection_campaign$round-1)
  
  if(nrow(smc_projection_campaign) == nrow(distinct(smc_projection_campaign))) {
    write.csv(smc_projection_campaign, paste0(base_sim_input_dir, '/interventions_projections/smc_', scenario_smc_names[ii], '.csv'), row.names = FALSE)
  } else{
    warning('PROBLEM DETECTED: there are duplicate campaigns in the SMC input file.')
  }
  
  if(print_coverages){
    print('SMC')
    smc_projection_campaign2 = smc_projection_campaign[smc_projection_campaign$year>=2023,]
    smc_projection_campaign1 = smc_projection_campaign[smc_projection_campaign$year<2023,]
    cAll_admins = (unique(smc_projection_campaign$admin_name))
    c1_admins = (unique(smc_projection_campaign1$admin_name))
    c2_admins = (unique(smc_projection_campaign2$admin_name))
    c1_admins[which(!(c1_admins %in% c2_admins))]
    
    smc_projection_campaign2$total_U5_coverage = smc_projection_campaign2$coverage_high_access_U5*smc_projection_campaign2$high_access_U5 +  smc_projection_campaign2$coverage_low_access_U5*(1-smc_projection_campaign2$high_access_U5)
    print(paste0('....', scenario_smc_names[ii],': ', length(unique(smc_projection_campaign2$admin_name)), ' LGAs w/ coverage ', round(min(smc_projection_campaign2$total_U5_coverage),2), '-', round(max(smc_projection_campaign2$total_U5_coverage),2), '; median=', round(median(smc_projection_campaign2$total_U5_coverage),2)))
    print(paste0('......Num admins any year: ', length(cAll_admins), '; before 2023: ', length(c1_admins), '; 2023 and after: ', length(c2_admins), '. Intersect size=', length(intersect(c1_admins, c2_admins)), '. Num admins in 2022, not later=', length(which(!(c1_admins %in% c2_admins))), '. Num admins in 2023, but not earlier=', length(which(!(c2_admins %in% c1_admins)))))
  }
}


######## higher coverage version
# maximum coverage in the high-access group (from when original SMC file was created)
max_high_access_coverage = 0.98
# iterate through scenarios, preparing input files
for(ii in 1:length(scenario_smc_status_list)){
  projection_campaign_higher = read.csv(paste0(base_sim_input_dir, '/interventions_projections/smc_', scenario_smc_names[ii], '.csv'))
  # change the U5 coverage to coverage_increase_target for future campaigns
  increase_rows = which(projection_campaign_higher$simday > coverage_increase_start_day)
  if( projection_campaign_higher$high_access_U5[1] * max_high_access_coverage <= coverage_increase_target){  # note: assumes all rows have same high-access fraction
    projection_campaign_higher$coverage_high_access_U5[increase_rows] = max_high_access_coverage
    projection_campaign_higher$coverage_low_access_U5[increase_rows] = pmax(projection_campaign_higher$coverage_low_access_U5[increase_rows], (coverage_increase_target - projection_campaign_higher$coverage_high_access_U5[increase_rows] * projection_campaign_higher$high_access_U5[increase_rows])/ (1-projection_campaign_higher$high_access_U5[increase_rows]))
  } else{
    projection_campaign_higher$coverage_high_access_U5[increase_rows] = coverage_increase_target / projection_campaign_higher$high_access_U5[increase_rows]
    projection_campaign_higher$coverage_low_access_U5[increase_rows] = pmax(projection_campaign_higher$coverage_low_access_U5[increase_rows], 0)
  }
  if(nrow(projection_campaign_higher) == nrow(distinct(projection_campaign_higher))) {
    write.csv(projection_campaign_higher, paste0(base_sim_input_dir, '/interventions_projections/smc_', scenario_smc_names[ii], '_', round(100*coverage_increase_target), 'cov.csv'), row.names = FALSE)
  } else{
    warning('PROBLEM DETECTED: there are duplicate campaigns in the SMC input file.')
  }
}



######### strange schedule from Emmanuel where SMC is only given for two years (2024-2025) in GF states
if(FALSE){
  projection_campaign_partial_fund = read.csv(paste0(base_sim_input_dir, '/interventions_projections/smc_fund.csv'))
  projection_campaign_partial_fund = projection_campaign_partial_fund[projection_campaign_partial_fund$year <=2025,]
  if(nrow(projection_campaign_partial_fund) == nrow(distinct(projection_campaign_partial_fund))) {
    write.csv(projection_campaign_partial_fund, paste0(base_sim_input_dir, '/interventions_projections/smc_fund_partialEndAt2025.csv'), row.names = FALSE)
  } else{
    warning('PROBLEM DETECTED: there are duplicate campaigns in the SMC input file.')
  }
  
  # get list of admins to simulate that are GF and get SMC
  smc_admins = unique(projection_campaign_partial_fund$admin_name)
  gf_admins = unique(gf_scenarios$admin_name[gf_scenarios$funder=='Global Fund'])
  smc_gf_admins = intersect(smc_admins, gf_admins)
  smc_gf_df = data.frame(admin_name = smc_gf_admins, GF=TRUE, SMC=TRUE)
  write.csv(smc_gf_df, paste0(base_sim_input_dir, '/interventions_projections/admin_subset_gf_smc.csv'), row.names = FALSE)
}






# save list of admins that are scheduled to have SMC in the funded/prioritized plan AND are in states funded by GF - this is for subsetting some simulations for scenario
if(FALSE){
  smc_fund_gf = gf_scenarios[,c('admin_name', 'smc_fund', 'funder', 'State')]
  smc_fund_gf$receive_smc = sapply(smc_fund_gf$smc_fund, function(x){!grepl('No SMC', x)})
  smc_fund_gf = smc_fund_gf[smc_fund_gf$funder=='Global Fund',]
  smc_fund_gf_smc = smc_fund_gf[smc_fund_gf$receive_smc,]
  length(unique(smc_fund_gf_smc$State))
  smc_fund_gf_admins = data.frame(admin_name=unique(smc_fund_gf$admin_name[smc_fund_gf$funder=='Global Fund' & smc_fund_gf$receive_smc]), 'funder_GF'=TRUE,'receive_SMC_in_pri_plan'=TRUE)
  write.csv(smc_fund_gf_admins, paste0(base_sim_input_dir, '/interventions_projections/admin_subset_GF_funded_and_smc_fund.csv'), row.names = FALSE)
}


################################################################################
# PMC: use DHS estimate from vaccines to estimate coverage
################################################################################
### Steps
## 1 Load vaccine coverage data from DHS
## 2 Load PMC scenarios and combine data
## 3 Save csv files

# set ages for PMC (in days) - called 'touchpoints' - each must be matched with a vaccine coverage estimate
touchpoints = round(c(6, 9, 12, 15)*30.4)
corresponding_vaccine_estimates = c( 'vacc_dpt3', 'vacc_measles', 'vacc_measles', 'vacc_measles')
touchpoints_df = data.frame('pmc_touchpoints' = touchpoints, 'vaccine' = corresponding_vaccine_estimates)

#  Load vaccine coverage data from DHS
vacc_coverage = read.csv(file.path(hbhi_dir, 'estimates_from_DHS', 'DHS_vaccine_admin_minN30_2018.csv'))
vacc_coverage$admin_name = vacc_coverage$NOMDEP
var_names = unique(corresponding_vaccine_estimates)
# ages (in months) when each vaccine is received (to allow for correcting for surveyed children who are not yet that age)
vacc_ages = c(2, 3, 9)
max_child_age_surveyed = 5*12  # in months
maximum_coverage = 0.92
for(vv in 1:length(var_names)){
  # get vaccination coverage rates (with mean and CIs assuming binomial sampling)
  vacc_coverage[[var_names[vv]]] = vacc_coverage[[paste0(var_names[vv], '_num_true')]] / vacc_coverage[[paste0(var_names[vv], '_num_total')]]
  # adjust for the surveyed children who are under the age when they would receive the vaccine
  vacc_coverage[[var_names[vv]]] = vacc_coverage[[var_names[vv]]] / (1-vacc_ages[vv]/max_child_age_surveyed)
  # set maximum possible coverage
  vacc_coverage[[var_names[vv]]] = sapply(vacc_coverage[[var_names[vv]]], min, maximum_coverage)
}
vacc_coverage = vacc_coverage %>% dplyr::select(one_of(c('admin_name', var_names)))
vacc_coverage_long = melt(vacc_coverage, id.vars=c("admin_name"), value.name='coverage')
colnames(vacc_coverage_long)[colnames(vacc_coverage_long)=='variable'] = 'vaccine'
# merge dataframes to get the coverage at each touchpoint for each LGA
pmc_coverage = merge(vacc_coverage_long, touchpoints_df, all=TRUE)



# determine which admins receive PMC in future projections
# NSP
pmc_nsp = gf_scenarios[,c('admin_name', 'ipti_nsp')]
unique(pmc_nsp$ipti_nsp)
pmc_nsp$receive_pmc = sapply(pmc_nsp$ipti_nsp, function(x){!grepl('No IPTi', x)})
# fund
pmc_fund = gf_scenarios[,c('admin_name', 'ipti_fund')]
unique(pmc_fund$ipti_fund)
pmc_fund$receive_pmc = sapply(pmc_fund$ipti_fund, function(x){!grepl('No IPTi', x)})
# imp
pmc_imp = gf_scenarios[,c('admin_name', 'ipti_imp')]
unique(pmc_imp$ipti_imp)
pmc_imp$receive_pmc = sapply(pmc_imp$ipti_imp, function(x){!grepl('No IPTi', x)})
# create list of net scenarios with corresponding vector describing the name of each (allowing for looping later)
scenario_pmc_names = c('nsp', 'fund', 'imp')
scenario_pmc_status_list = list(pmc_nsp, pmc_fund, pmc_imp)


# iterate through scenarios, preparing input files (set coverage to 0 in non-IPTi admins)
for(ii in 1:length(scenario_pmc_status_list)){
  pmc_coverage_cur = pmc_coverage
  pmc_admins = unique(scenario_pmc_status_list[[ii]][['admin_name']][scenario_pmc_status_list[[ii]][['receive_pmc']]])
  # remove all non-pmc admins to zero
  pmc_coverage_cur = pmc_coverage_cur[which(pmc_coverage_cur$admin_name %in% pmc_admins),]
  
  # set day of simulation when PMC begins - currently assumes it starts at the forward-projection, not the recent-past
  #    need to adjust the start day to take into account the delay due to it being a birth-triggered intervention (only children born after the startday will eventually receive the intervention)
  #    --> shift the simulation start day earlier so that children who reach the first touchpoint by the intervention start day will receive the intervention
  pmc_coverage_cur$simday = max(0, 365 * (future_start_year - projection_start_year) - min(touchpoints))
  # set information about the variation in age when PMC is given
  pmc_coverage_cur$distribution_name = 'LOG_NORMAL_DISTRIBUTION'
  pmc_coverage_cur$distribution_mean = 0
  pmc_coverage_cur$distribution_std = 0.05

  # save output
  if(nrow(pmc_coverage_cur) == nrow(distinct(pmc_coverage_cur))) {
    write.csv(pmc_coverage_cur, paste0(base_sim_input_dir, '/interventions_projections/pmc_', scenario_pmc_names[ii], '.csv'), row.names = FALSE)
  } else{
    warning('PROBLEM DETECTED: there are duplicate campaigns in the PMC input file.')
  }
  
  if(print_coverages){
    print('PMC')
    cov_all = pmc_coverage_cur$coverage
    print(paste0('....', scenario_pmc_names[ii],': ', length(unique(pmc_coverage_cur$admin_name)), ' LGAs w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
  }
}



######## higher coverage version - assumes immediate higher coverage rather than scale-up, across all touchpoints
# iterate through scenarios, preparing input files
for(ii in 1:length(scenario_pmc_status_list)){
  projection_campaign_higher = read.csv(paste0(base_sim_input_dir, '/interventions_projections/pmc_', scenario_pmc_names[ii], '.csv'))
  projection_campaign_higher$coverage = coverage_increase_target
  if(nrow(projection_campaign_higher) == nrow(distinct(projection_campaign_higher))) {
    write.csv(projection_campaign_higher, paste0(base_sim_input_dir, '/interventions_projections/pmc_', scenario_pmc_names[ii], '_', round(100*coverage_increase_target), 'cov.csv'), row.names = FALSE)
  } else{
    warning('PROBLEM DETECTED: there are duplicate campaigns in the PMC input file.')
  }
}





# # OLD VERSION: used for IPTi post-processing
# # calculate average coverage across doses, with a minimum 20% coverage
# vacc_coverage$IPTicov = apply(vacc_coverage[,which(colnames(vacc_coverage) %in% paste0(var_names,'_est'))], 1, sum) / length(var_names)
# 
# ipti_coverage = vacc_coverage[,c('admin_name', 'IPTicov')]
# ipti_coverage = ipti_coverage %>% dplyr::mutate(
#   IPTicov = ifelse(IPTicov < 0.2, 0.2, IPTicov)
# )
# ipti_coverage$ipti_cov_mean = ipti_coverage$IPTicov
# # hist(ipti_coverage$IPTicov, xlim=c(0,1), col=rgb(8/255,29/255,88/255), border=rgb(0.3,0.3,0.3), xlab='coverage', main='DPT and measles vaccine coverage (DHS - 2018)', cex.main=1, ylab='number of districts')
# 
# # iterate through scenarios, preparing input files (set coverage to 0 in non-IPTi admins)
# for(ii in 1:length(scenario_pmc_status_list)){
#   ipti_coverage_cur = ipti_coverage
#   ipti_admins = unique(scenario_pmc_status_list[[ii]][['admin_name']][scenario_pmc_status_list[[ii]][['receive_pmc']]])
#   # set all non-IPTi admins to zero
#   ipti_coverage_cur[which(!(ipti_coverage_cur$admin_name %in% ipti_admins)), which(colnames(ipti_coverage_cur) != 'admin_name')] = 0
#   # indicate whether or not an admin receives IPTi
#   ipti_coverage_cur$IPTyn = 1
#   ipti_coverage_cur$IPTyn[which(!(ipti_coverage_cur$admin_name %in% ipti_admins))] = 0
#   
#   # set day of simulation when PMC begins - currently assumes it starts at the forward-projection, not the recent-past
#   ipti_coverage_cur$simday = 365 * (future_start_year - projection_start_year)
#   # set information about the variation in age when PMC is given
#   ipti_coverage_cur$distribution_name = 'LOG_NORMAL_DISTRIBUTION'
#   ipti_coverage_cur$distribution_std = 0.1752
#   # update column name for compatibility with simulation intervention function
#   colnames(ipti_coverage_cur)[colnames(ipti_coverage_cur)=='ipti_cov_mean'] = 'coverage'
#   
#   # save output
#   if(nrow(ipti_coverage_cur) == nrow(distinct(ipti_coverage_cur))) {
#     write.csv(ipti_coverage_cur, paste0(base_sim_input_dir, '/interventions_projections/pmc_', scenario_pmc_names[ii], '.csv'), row.names = FALSE)
#   } else{
#     warning('PROBLEM DETECTED: there are duplicate campaigns in the PMC input file.')
#   }
# }






################################################################################
# RTSS: use DHS estimate from vaccines to estimate coverage
################################################################################
### Steps
## 1 Load vaccine coverage data from DHS
## 2 Load PMC scenarios and combine data
## 3 Save csv files

# set touchpoints
primary_series_day = round(7*30.4)  # old value: 270  # initial series finishes around nine months
booster_day = round(20*30.4)  # old value: 720  # booster at around two years of age

# set day of simulation when vaccinations will be rolled out
#    need to adjust the start day to take into account the delay due to it being a birth-triggered intervention (only children born after the startday will eventually receive the intervention)
#    --> shift the simulation start day earlier so that children who reach the first touchpoint by the intervention start day will receive the intervention
vacc_rollout_day =  max(1, 365 * (2025 - projection_start_year) + 1  - primary_series_day) # start day in simulation

#  Load vaccine coverage data from DHS
vacc_coverage = read.csv(file.path(hbhi_dir, 'estimates_from_DHS', 'DHS_vaccine_admin_minN30_2018.csv'))
vacc_coverage$admin_name = vacc_coverage$NOMDEP
var_names = c( 'vacc_dpt3', 'vacc_measles')  # 'vacc_dpt1', 'vacc_dpt2',
# ages (in months) when each vaccine is received (to allow for correcting for surveyed children who are not yet that age)
vacc_ages = c(3, 9)  # 1, 2, 
max_child_age_surveyed = 5*12  # in months
maximum_coverage = 0.92
for(vv in 1:length(var_names)){
  # get vaccination coverage rates (with mean and CIs assuming binomial sampling)
  vacc_coverage[[paste0(var_names[vv], '_est')]] = vacc_coverage[[paste0(var_names[vv], '_num_true')]] / vacc_coverage[[paste0(var_names[vv], '_num_total')]]
  # adjust for the surveyed children who are under the age when they would receive the vaccine
  vacc_coverage[[paste0(var_names[vv], '_est')]] = vacc_coverage[[paste0(var_names[vv], '_est')]] / (1-vacc_ages[vv]/max_child_age_surveyed)
  # set maximum possible coverage
  vacc_coverage[[paste0(var_names[vv], '_est')]] = sapply(vacc_coverage[[paste0(var_names[vv], '_est')]], min, maximum_coverage)
}
# calculate average coverage across doses, with a minimum 20% coverage
vacc_coverage$coverage = apply(vacc_coverage[,which(colnames(vacc_coverage) %in% paste0(var_names,'_est'))], 1, sum) / length(var_names)

# ggplot(vacc_coverage)+
#   geom_density(aes(x=coverage), color='black')+
#   geom_density(aes(x=vacc_dpt1_est), color=rgb(0.8,1,0.8))+
#   geom_density(aes(x=vacc_dpt2_est), color=rgb(0.2,0.8,0.2))+
#   geom_density(aes(x=vacc_dpt3_est), color=rgb(0,0.4,0))+
#   geom_density(aes(x=vacc_measles_est), color='red')+
#   theme_bw()


vacc_coverage = vacc_coverage[,c('admin_name', 'coverage')]
vacc_coverage = vacc_coverage %>% dplyr::mutate(
  coverage = ifelse(coverage < 0.2, 0.2, coverage)
)


# determine which admins receive PMC in future projections
# NSP - all vaccine levels
vacc_nsp = gf_scenarios[,c('admin_name', 'vaccine_nsp')]
unique(vacc_nsp$vaccine_nsp)
vacc_nsp$receive_vacc = sapply(vacc_nsp$vaccine_nsp, function(x){!grepl('No Vac', x)})
# NSP - only vaccine priority 1 or 1p
vacc_nsp1 = gf_scenarios[,c('admin_name', 'vaccine_cat')]
unique(vacc_nsp1$vaccine_cat)
vacc_nsp1$receive_vacc = sapply(vacc_nsp1$vaccine_cat, function(x){grepl('Vac1', x)})
# fund
vacc_fund = gf_scenarios[,c('admin_name', 'vac_fund')]
unique(vacc_fund$vac_fund)
vacc_fund$receive_vacc = sapply(vacc_fund$vac_fund, function(x){!grepl('Not-pri', x)})
# create list of net scenarios with corresponding vector describing the name of each (allowing for looping later)
scenario_vacc_names = c('catAll', 'cat1', 'cat1p')
scenario_vacc_status_list = list(vacc_nsp, vacc_nsp1, vacc_fund)


# iterate through scenarios, preparing input files (set coverage to 0 in non-IPTi admins)
for(ii in 1:length(scenario_vacc_status_list)){
  vacc_coverage_cur = vacc_coverage
  vacc_admins = unique(scenario_vacc_status_list[[ii]][['admin_name']][scenario_vacc_status_list[[ii]][['receive_vacc']]])
  # set all non-vacc admins to zero
  vacc_coverage_cur[which(!(vacc_coverage_cur$admin_name %in% vacc_admins)), which(colnames(vacc_coverage_cur) != 'admin_name')] = 0
  # remove rows where admin does not receive vaccine
  vacc_coverage_cur = vacc_coverage_cur[which(vacc_coverage_cur$admin_name %in% vacc_admins),]
  
  # add additional information about vaccine distribution
  vacc_coverage_cur$deploy_type = 'EPI'  # type of distribution
  vacc_coverage_cur$RTSS_day = vacc_rollout_day  # start day in simulation
  vacc_coverage_cur$vaccine='primary'  # primary series
  vacc_coverage_cur$rtss_touchpoints = primary_series_day
  vacc_coverage_cur$distribution_name = 'CONSTANT_DISTRIBUTION'  # determines the distribution around the age of receiving vaccine
  vacc_coverage_cur$distribution_std = 1
  vacc_coverage_cur$initial_effect = 0.8
  vacc_coverage_cur$decay_time_constant = 592.406651165031
  vacc_coverage_cur$decay_class = 'WaningEffectExponential'
  
  
  # add in the dates and types for the booster
  vacc_coverage_boost = vacc_coverage_cur
  vacc_coverage_boost$coverage=0.8  # booster series has 80% coverage among those receiving initial series
  vacc_coverage_boost$vaccine='booster'  # booster series
  vacc_coverage_boost$rtss_touchpoints = booster_day
  vacc_coverage_boost$initial_effect = 0.4
  vacc_coverage_all = rbind(vacc_coverage_cur, vacc_coverage_boost)
  
  # save output
  if(nrow(vacc_coverage_all) == nrow(distinct(vacc_coverage_all))) {
    write.csv(vacc_coverage_all, paste0(base_sim_input_dir, '/interventions_projections/vacc_', scenario_vacc_names[ii], '.csv'), row.names = FALSE)
  } else{
    warning('PROBLEM DETECTED: there are duplicate campaigns in the vaccine input file.')
  }
  
  if(print_coverages){
    print('Vaccine')
    cov_all = vacc_coverage_all$coverage[vacc_coverage_all$vaccine=='primary']
    print(paste0('....', scenario_vacc_names[ii],': ', length(unique(vacc_coverage_all$admin_name)), ' LGAs w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
  }
}



######## higher coverage version - assumes immediate higher coverage for primary series rather than scale-up
# iterate through scenarios, preparing input files
for(ii in 1:length(scenario_vacc_status_list)){
  projection_campaign_higher = read.csv(paste0(base_sim_input_dir, '/interventions_projections/vacc_', scenario_vacc_names[ii], '.csv'))
  projection_campaign_higher$coverage[projection_campaign_higher$vaccine=='primary'] = coverage_increase_target
  if(nrow(projection_campaign_higher) == nrow(distinct(projection_campaign_higher))) {
    write.csv(projection_campaign_higher, paste0(base_sim_input_dir, '/interventions_projections/vacc_', scenario_vacc_names[ii], '_', round(100*coverage_increase_target), 'cov.csv'), row.names = FALSE)
  } else{
    warning('PROBLEM DETECTED: there are duplicate campaigns in the vaccine input file.')
  }
}




# create csv of admins that get vaccine or PMC in the fund plan (so that additional seeds can be run tgiven high stochasticity in mortality)
if(FALSE){
  vacc_admins = read.csv(paste0(base_sim_input_dir, '/interventions_projections/vacc_cat1p.csv'))
  vacc_admins = distinct(data.frame(admin_name = vacc_admins$admin_name, vaccine=TRUE))
  pmc_admins = read.csv(paste0(base_sim_input_dir, '/interventions_projections/pmc_fund.csv'))
  pmc_admins = distinct(data.frame(admin_name = pmc_admins$admin_name, PMC=TRUE))
  extra_seed_admins = merge(vacc_admins, pmc_admins, all=TRUE)
  write.csv(extra_seed_admins, paste0(base_sim_input_dir, '/interventions_projections/admins_need_extra_seeds.csv'), row.names = FALSE)
}


################################################################################
# IRS: two versions with different effective coverages
################################################################################
# set parameters for IRS - based off a Sumishield50WG-like product (neonicotinoids: clothianidin) with parameters based on Sherrad-Smith et al. 
irs_initial_kill = 0.67
irs_mean_duration = 302
irs_coverage = 0.8
effective_irs_coverage_multiplier = 1-0.22  # reduces effective individual coverage to account for dwelling modification/washing, migration, incomplete spraying, etc.
irs_doys =  round(c(7,12)*30.4) # day of year for spraying
start_year_irs = 2024
irs_start_day = (start_year_irs-projection_start_year)*365 + irs_doys[1]  # assume spraying occurs in 2024
irs_admins = gf_scenarios$admin_name[gf_scenarios$irs_nsp == 'IRS']
if(length(irs_admins) > 0){
  irs_future = data.frame(admin_name = irs_admins, 
                          effective_coverage = irs_coverage * effective_irs_coverage_multiplier,
                          year = start_year_irs,
                          simday = irs_start_day,
                          initial_kill = irs_initial_kill,
                          mean_duration = irs_mean_duration)
  # add copy for each spray day in a year
  irs_copy=irs_future
  if(length(irs_doys)>1){
    for(ii in 2:length(irs_doys)){
      irs_cur = irs_copy
      irs_cur$simday = (start_year_irs-projection_start_year)*365 + irs_doys[ii] 
      irs_future = rbind(irs_future,irs_cur)
    }
  }
  
  # add IRS for each year in projection
  irs_copy=irs_future
  if(start_year_irs < projection_end_year){
    for(yy in 1:length((start_year_irs+1):projection_end_year)){
      irs_cur = irs_copy
      irs_cur$year = start_year_irs + yy
      irs_cur$simday = irs_copy$simday + 365*yy
      irs_future = rbind(irs_future,irs_cur)
    }
  }
  if(nrow(irs_future) == nrow(distinct(irs_future))) {
    write.csv(irs_future, paste0(base_sim_input_dir, '/interventions_projections/irs_', round(100*irs_coverage), 'cov.csv'), row.names = FALSE)
  } else{
    warning('PROBLEM DETECTED: there are duplicate campaigns in the vaccine input file.')
  }
  
#   ####### higher coverage version
#   irs_higher_cov = irs_future
#   irs_higher_cov$effective_coverage[irs_higher_cov$simday > coverage_increase_start_day] = coverage_increase_target
#   if(nrow(irs_higher_cov) == nrow(distinct(irs_higher_cov))) {
#     write.csv(irs_higher_cov, paste0(base_sim_input_dir, '/interventions_projections/irs_', round(100*coverage_increase_target), 'cov.csv'), row.names = FALSE)
#   } else{
#     warning('PROBLEM DETECTED: there are duplicate campaigns in the vaccine input file.')
#   }
}




################################################################################
#  LLIN mass distribution
################################################################################
frac_reassign_feed_survive=0.9
# LLIN input from to-present simulations
llin_mass_to_present = read.csv(paste0(base_sim_input_dir, '/', intervention_files_past$ITN_filename[1],'.csv'))
llin_mass_past = llin_mass_to_present[llin_mass_to_present$year < projection_start_year,]

# extract the years of mass distributions for each state (year is specified in one of the WHO columns, but need regular expressions to extract; month should be same as prior distribution, before 2022, since 2022 months often not specified)
lga_mass_years = gf_scenarios[,c('State', 'admin_name', 'mass_llins_fund')]
extract_years = function(net_info){return(gsub("[\\(\\)]", "", regmatches(net_info, gregexpr("\\(.*?\\)", net_info))[[1]]))}
lga_mass_years$extracted_years = sapply(lga_mass_years$mass_llins_fund, extract_years)
lga_mass_years$extracted_years[lga_mass_years$extracted_years=='2023,2026'] = '2023'
lga_mass_years$year = as.numeric(lga_mass_years$extracted_years)

# add rows for future years of distribution - assume every three years up until projection_end_year. Will use same net type and coverage
min_year = min(lga_mass_years$year)
for(yy in min_year:(min_year+2)){
  lga_mass_years_cur = lga_mass_years[lga_mass_years$year==yy,]
  if(yy<(projection_end_year-2)){
    future_dist_years = seq(yy+3, projection_end_year, 3)
    for(future_year in future_dist_years){
      lga_mass_years_cur$year = future_year
      lga_mass_years = rbind(lga_mass_years, lga_mass_years_cur)
    }
  }
}
lga_mass_years = lga_mass_years[,c('admin_name', 'year')]

# determine the coverage and month to use in each LGA for distributions (the rounded mean month from distributions and maximum previous coverage before projection_start_year)
lga_mass_past_aggregated = llin_mass_past %>%
  mutate(date = as.Date(date),
         month = month(date)) %>% 
  group_by(admin_name, State) %>%
  summarise(month = round(mean(month)),
            coverage=max(coverage),
            itn_u5=max(itn_u5),
            itn_5_10=max(itn_5_10),
            itn_10_15=max(itn_10_15),
            itn_15_20=max(itn_15_20),
            itn_o20=max(itn_o20),
            net_life_lognormal_mu=mean(net_life_lognormal_mu),
            net_life_lognormal_sigma=mean(net_life_lognormal_sigma),
            indoor_net_protection=mean(indoor_net_protection))

# combine the coverage, month, and year information
llin_date_cov_df = merge(lga_mass_years, lga_mass_past_aggregated)


# get the latest bio_mortality in each LGA for determining insecticide resistance in future projections
bio_mortality_wide_df = read.csv(mortality_df_name)
if('X' %in% colnames(bio_mortality_wide_df)) bio_mortality_wide_df = bio_mortality_wide_df[,-which(colnames(bio_mortality_wide_df)=='X')]
# switch to long format
bio_mortality_df = melt(bio_mortality_wide_df, id.vars='admin_name')
bio_mortality_df$year = as.numeric(gsub('X','',bio_mortality_df$variable))
bio_mortality_df$bio_mortality = bio_mortality_df$value
bio_mortality_df = bio_mortality_df[bio_mortality_df$year==max(bio_mortality_df$year),]
bio_mortality_df = bio_mortality_df[,c('admin_name','bio_mortality')]


# set up the net type to be used for distributions in each year / state (these will differ depending on the scenario)
# NSP
lga_mass_nettype_nsp = gf_scenarios[,c('admin_name', 'mass_llins_nsp')]
colnames(lga_mass_nettype_nsp)[colnames(lga_mass_nettype_nsp)=='mass_llins_nsp'] = 'llin_type'
unique(lga_mass_nettype_nsp$llin_type)
# fund
lga_mass_nettype_fund = gf_scenarios[,c('admin_name', 'mass_llins_fund')]
colnames(lga_mass_nettype_fund)[colnames(lga_mass_nettype_fund)=='mass_llins_fund'] = 'llin_type'
unique(lga_mass_nettype_fund$llin_type)
# imp
lga_mass_nettype_imp = gf_scenarios[,c('admin_name', 'llins_imp')]
colnames(lga_mass_nettype_imp)[colnames(lga_mass_nettype_imp)=='llins_imp'] = 'llin_type'
unique(lga_mass_nettype_imp$llin_type)
# create list of net scenarios with corresponding vector describing the name of each (allowing for looping later)
scenario_net_names = c('nsp', 'fund', 'imp')
scenario_net_type_list = list(lga_mass_nettype_nsp, lga_mass_nettype_fund, lga_mass_nettype_imp)
scenario_campaign_list =list()


# find the block/kill values given the net type in each LGA and the bio-mortality in each LGA
# function to update / standardize the names of different types of nets. Note that we do not treat 'urban' specifications differently
standardize_net_type_names = function(net_name){
  if(grepl('PBO', net_name) | grepl('pbo', net_name) | grepl('Pyrethroid-piperonyl butoxide', net_name)){
    return('PBO')
  } else if(grepl('Dual AI', net_name) | grepl('IG2', net_name) | grepl('ig2', net_name)){
    return('IG2')
  } else if(grepl('none', net_name) | grepl('No ITN', net_name) | grepl('NoITN', net_name) | is.na(net_name)){
    return(NA)
  } else{
    return('standard')
  }
}


# given the years/month/coverages of distribution and the bio-mortality and net type for each admin, combine into dataframe with relevant distribution info
add_net_efficacy_info = function(llin_date_cov_type_df, bio_mortality_df){
  # merge in the bio-mortality values into the net type dataframe
  llin_campaign_df = merge(llin_date_cov_type_df, bio_mortality_df)
  # standardize net type names
  llin_campaign_df$llin_type = sapply(llin_campaign_df$llin_type, standardize_net_type_names)
  llin_campaign_df = add_kill_block_to_df(itn_df=llin_campaign_df, frac_reassign_feed_survive=frac_reassign_feed_survive)
  
  # if the net type is 'No ITN' (recoded as NA), the campaign row should be deleted
  if(any(is.na(llin_campaign_df$llin_type))) llin_campaign_df = llin_campaign_df[-which(is.na(llin_campaign_df$llin_type)),]
  return(llin_campaign_df)
}



# grab any distributions that were included in the 'to-present' input file, but will occur during the 'future projection' simulations (i.e., the date is after the end of the 'to-present' simulations)
llin_mass_carry_over = llin_mass_to_present[llin_mass_to_present$year >= projection_start_year,]
llin_mass_carry_over = llin_mass_carry_over[,c('year','admin_name','State','llin_type')]
# add the coverage and month (based on previous distributions) to the information about the year and llin type
llin_mass_carry_over = merge(llin_mass_carry_over, lga_mass_past_aggregated, all.x=TRUE)
# add in the bio-mortality values for all admins and get the corresponding block/kill parameters given the net type
llin_mass_carry_over_campaign = add_net_efficacy_info(llin_date_cov_type_df=llin_mass_carry_over, bio_mortality_df=bio_mortality_df)



# prepare the campaign input file for each of the scenarios:
#   - add in the bio-mortality values for all admins and get the corresponding block/kill parameters given the net type
#   - also add in any carry-over campaigns from the to-present file
for(ii in 1:length(scenario_net_type_list)){
  # merge the net type information with the dataframe with rows for each campaign
  llin_date_cov_type_df = merge(llin_date_cov_df, scenario_net_type_list[[ii]])
  # get the blocking/killing given net type and bio-mortality
  scenario_campaign = add_net_efficacy_info(llin_date_cov_type_df=llin_date_cov_type_df, bio_mortality_df=bio_mortality_df)
  
  # add in any carry-over campaigns from the to-present file
  scenario_campaign = merge(scenario_campaign, llin_mass_carry_over_campaign, all=TRUE)
  
  # add in the date and day of simulation
  scenario_campaign$date = as.Date(paste0(scenario_campaign$year, '-', scenario_campaign$month, '-01'))
  scenario_campaign$simday = as.numeric(scenario_campaign$date - as.Date(paste0(projection_start_year, '-01-01')))
  
  # make sure that there aren't duplicates anywhere such that an admin gets multiple repeated distributions in a year
  check_df = scenario_campaign[,c('admin_name', 'year')]
  if(nrow(check_df) != nrow(distinct(check_df))){
    warning('PROBLEM DETECTED: some admins have multiple distributions in a year')
  } else{
    write.csv(scenario_campaign, paste0(base_sim_input_dir, '/interventions_projections/llin_mass_', scenario_net_names[ii], '.csv'), row.names = FALSE)
  }

  if(print_coverages){
    print('LLIN mass dist')
    scenario_campaign_distinct_LGA = distinct(scenario_campaign[scenario_campaign$year>=future_start_year,c('admin_name','coverage')])
    cov_all = scenario_campaign_distinct_LGA$coverage
    print(paste0('....', scenario_net_names[ii],': ', length(unique(scenario_campaign$admin_name)), ' LGAs w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
    # net types
    scenario_campaign_distinct_LGA = distinct(scenario_campaign[scenario_campaign$year>=future_start_year,c('admin_name', 'llin_type')])
    print(paste0('....... PBO:', length(unique(scenario_campaign_distinct_LGA$admin_name[scenario_campaign_distinct_LGA$llin_type=='PBO'])), 
                 '; IG2: ', length(unique(scenario_campaign_distinct_LGA$admin_name[scenario_campaign_distinct_LGA$llin_type=='IG2'])),
                 '; standard: ', length(unique(scenario_campaign$admin_name)) - length(unique(scenario_campaign_distinct_LGA$admin_name[scenario_campaign_distinct_LGA$llin_type=='PBO'])) - length(unique(scenario_campaign_distinct_LGA$admin_name[scenario_campaign_distinct_LGA$llin_type=='IG2']))))
  }
}



######## higher coverage version - assumes higher coverage from future_start_year onward
for(coverage_increase_target in c(0.6, 0.8)){
  # iterate through scenarios, preparing input files
  for(ii in 1:length(scenario_net_type_list)){
    projection_campaign_higher = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_mass_', scenario_net_names[ii], '.csv'))
    coverage_colnames = c('coverage', 'itn_u5', 'itn_5_10', 'itn_10_15', 'itn_15_20', 'itn_o20')
    increase_rows = which(projection_campaign_higher$simday > coverage_increase_start_day)
    for (cc in 1:length(coverage_colnames)){
      projection_campaign_higher[increase_rows, which(colnames(projection_campaign_higher) == coverage_colnames[cc])] = sapply(projection_campaign_higher[increase_rows, which(colnames(projection_campaign_higher) == coverage_colnames[cc])], max, coverage_increase_target)
    }
    if(nrow(projection_campaign_higher) == nrow(distinct(projection_campaign_higher))) {
      write.csv(projection_campaign_higher, paste0(base_sim_input_dir, '/interventions_projections/llin_mass_', scenario_net_names[ii], '_', round(100*coverage_increase_target), 'cov.csv'), row.names = FALSE)
    } else{
      warning('PROBLEM DETECTED: there are duplicate campaigns in the LLIN input file.')
    }
  }
}






######## version of LLIN campaigns for simulations with IRS (no mass distributions in LGAs receiving IRS)
# get admins with IRS
irs_future = read.csv(paste0(base_sim_input_dir, '/interventions_projections/irs_80cov.csv'))

# iterate through scenarios, preparing input files
if(TRUE){
  filename_versions = c(paste0(base_sim_input_dir, '/interventions_projections/llin_mass_nsp.csv'),
                        paste0(base_sim_input_dir, '/interventions_projections/llin_mass_nsp_60cov.csv'),
                        paste0(base_sim_input_dir, '/interventions_projections/llin_mass_nsp_80cov.csv'))
  for(ff in 1:length(filename_versions)){
    cur_filename = filename_versions[ff]
    irs_filename_version = gsub('.csv', '_withIRS.csv', cur_filename)
    
    projection_campaign_wIRS = read.csv(cur_filename)
    rows_admin_date_with_irs = intersect(which(projection_campaign_wIRS$admin_name %in% irs_future$admin_name), which(projection_campaign_wIRS$simday>min(irs_future$simday)))
    projection_campaign_wIRS = projection_campaign_wIRS[-rows_admin_date_with_irs,]
    if(nrow(projection_campaign_wIRS) == nrow(distinct(projection_campaign_wIRS))) {
      write.csv(projection_campaign_wIRS, irs_filename_version, row.names = FALSE)
    } else{
      warning('PROBLEM DETECTED: there are duplicate campaigns in the LLIN input file.')
    }
  }
}



if(FALSE){
  # save list of admins where the type of net is different between plans
  net_types = gf_scenarios[,c('admin_name', 'mass_llins_nsp', 'mass_llins_fund', 'llins_imp')]
  # change to consistent names
  for (cc in 2:ncol(net_types)){
    net_types[,cc] = sapply(net_types[,cc], standardize_net_type_names)
  }
  # subset to admins where the row values are different
  different_nets = net_types %>% 
    filter(mass_llins_nsp != llins_imp | is.na(llins_imp))
  write.csv(different_nets, paste0(base_sim_input_dir, '/interventions_projections/llin_mass_net_types_differ.csv'), row.names = FALSE)
}






################################################################################
#  LLIN ANC distribution
################################################################################
llin_anc_past = read.csv(paste0(base_sim_input_dir, '/', intervention_files_past$ANC_ITN_filename[1],'.csv'))
# subest to 2019 distribution
llin_anc_sub= llin_anc_past[llin_anc_past$year == 2019,]
llin_anc_future = llin_anc_sub
llin_anc_future$year = projection_start_year
llin_anc_future$simday = 0
llin_anc_future$duration = -1
llin_anc_future$llin_type='standard'
llin_anc_future = add_kill_block_to_df(itn_df=llin_anc_future, frac_reassign_feed_survive=frac_reassign_feed_survive)

# llin_anc_future$duration = 365
# # add for each year in projection
# if(projection_start_year<projection_end_year){
#   for(yy in 1:length((projection_start_year+1):projection_end_year)){
#     llin_anc_cur = llin_anc_sub
#     llin_anc_cur$year = projection_start_year+yy
#     llin_anc_cur$simday = 365*yy
#     llin_anc_cur$duration = 365
#     llin_anc_future = rbind(llin_anc_future,llin_anc_cur)
#   }
# }
if(nrow(llin_anc_future) == nrow(distinct(llin_anc_future))) {
  write.csv(llin_anc_future, paste0(base_sim_input_dir, '/interventions_projections/llin_anc_bau.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the ANC LLIN input file.')
}


if(print_coverages){
  print('LLIN ANC')
  cov_all = llin_anc_future$coverage
  print(paste0('.... Continuing coverage: ', length(unique(llin_anc_future$admin_name)), ' LGAs w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
}



######## higher coverage version - assumes higher coverage from future_start_year onward
for(coverage_increase_target in c(0.6, 0.8)){
  # set the current coverage to be only until future high coverage begins (on coverage_increase_start_day)
  projection_campaign_higher = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_anc_bau.csv'))
  projection_campaign_higher$duration = coverage_increase_start_day
  if(nrow(projection_campaign_higher) != length(archetype_info$admin_name)){
    warning('some LGAs are missing from the LLIN ANC dataframe')
  }
  llin_anc_higher = projection_campaign_higher
  llin_anc_future$coverage = coverage_increase_target
  llin_anc_future$year = floor(projection_start_year + coverage_increase_start_day/365)
  llin_anc_future$simday = coverage_increase_start_day
  llin_anc_future$duration = -1
  projection_campaign_higher = rbind(projection_campaign_higher, llin_anc_future)
  
  if(nrow(projection_campaign_higher) == nrow(distinct(projection_campaign_higher))) {
    write.csv(projection_campaign_higher, paste0(base_sim_input_dir, '/interventions_projections/llin_anc_', round(100*coverage_increase_target), 'cov.csv'), row.names = FALSE)
  } else{
    warning('PROBLEM DETECTED: there are duplicate campaigns in the ANC LLIN input file.')
  }
}

















# ##### update block/kill to pyrethroid w/bioassay=0.4.
# llin_future_04 = llin_anc_future
# llin_future_04 = llin_future_04[,-which(colnames(llin_future_04) %in% c('block_initial','kill_initial'))]
# llin_future_04$bio_mortality = 0.4
# llin_future_04 = add_kill_block_to_df(itn_df=llin_future_04, frac_reassign_feed_survive=frac_reassign_feed_survive)
# write.csv(llin_future_04, paste0(base_sim_input_dir, '/interventions_projections/llin_anc_04pyr.csv'), row.names = FALSE)
# 
# 
# ##### update block/kill to PBO w/bioassay the same as in 2019 distribution. 
# llin_future_PBO = llin_anc_future
# llin_future_PBO$net_type_PBO = 1
# llin_future_PBO = llin_future_PBO[,-which(colnames(llin_future_PBO) %in% c('block_initial','kill_initial'))]
# llin_future_PBO = add_kill_block_to_df(itn_df=llin_future_PBO, frac_reassign_feed_survive=frac_reassign_feed_survive)
# write.csv(llin_future_PBO, paste0(base_sim_input_dir, '/interventions_projections/llin_anc_PBO.csv'), row.names = FALSE)
# 
# 
# ##### update block/kill to PBO w/bioassay=0.4. 
# llin_future_PBO_04 = llin_anc_future
# llin_future_PBO_04$net_type_PBO = 1
# llin_future_PBO_04$bio_mortality = 0.4
# llin_future_PBO_04 = llin_future_PBO_04[,-which(colnames(llin_future_PBO_04) %in% c('block_initial','kill_initial'))]
# llin_future_PBO_04 = add_kill_block_to_df(itn_df=llin_future_PBO_04, frac_reassign_feed_survive=frac_reassign_feed_survive)
# write.csv(llin_future_PBO_04, paste0(base_sim_input_dir, '/interventions_projections/llin_anc_04PBO.csv'), row.names = FALSE)






# 
# 
# 
# ################################################################################
# #  update LLIN types and coverages to GF plan
# ################################################################################
# admins_with_PBO = gf_scenarios$adm2[gf_scenarios$llins_mass22_pri %in% c('MILDA-IG2', 'MILDA-PBO')]
# admins_with_pyr = gf_scenarios$adm2[gf_scenarios$llins_mass22_pri %in% c('Pyrethroids')]
# admins_without_LLINs = gf_scenarios$adm2[gf_scenarios$llins_mass22_pri %in% c('Sans campagne MILDA')]
# 
# # check whether all admins are appropriately assigned a net type
# ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
# admin_names = read.csv(ds_pop_df_filename)
# admin_names = admin_names$admin_name
# if(!all(admin_names %in% c(admins_with_PBO, admins_with_pyr, admins_without_LLINs))) warning('some admin names not matching')
# if(length(admin_names) != length(c(admins_with_PBO, admins_with_pyr, admins_without_LLINs))) warning('some admin names not included')
# if(!all(sort(admin_names) == sort(c(admins_with_PBO, admins_with_pyr, admins_without_LLINs)))) warning('some admin names do not match')
# # check that there are no admins that appear in both pyr and PBO
# if(any(admins_with_PBO %in% admins_with_pyr)) warning('some admins were included twice: once with PBO and once with pyr')
# 
# # use BAU coverage with the GF net type
# # read in all-pyr and all-PBO files, subsetting to the correct set of admins for each, and save
# net_base_filepath = paste0(base_sim_input_dir, '/interventions_projections/')
# filenames_pyr = c('llin_mass_bau.csv', 'llin_mass_04pyr.csv',
#                   'llin_anc_bau.csv', 'llin_anc_04pyr.csv',
#                   'llin_epi_bau.csv', 'llin_epi_04pyr.csv')
# filenames_PBO = c('llin_mass_PBO.csv', 'llin_mass_04PBO.csv',
#                   'llin_anc_PBO.csv', 'llin_anc_04PBO.csv',
#                   'llin_epi_PBO.csv', 'llin_epi_04PBO.csv')
# filenames_GF = c('llin_mass_gf_64pyr.csv', 'llin_mass_gf_40pyr.csv',
#                  'llin_anc_gf_64pyr.csv', 'llin_anc_gf_40pyr.csv',
#                  'llin_epi_gf_64pyr.csv', 'llin_epi_gf_40pyr.csv')
# filenames_GF_PBO = c('llin_mass_gf_PBO64pyr.csv', 'llin_mass_gf_PBO40pyr.csv')
# filenames_GF_80cov = c('llin_mass_gf_64pyr_80cov.csv', 'llin_mass_gf_40pyr_80cov.csv',
#                  'llin_anc_gf_64pyr_80cov.csv', 'llin_anc_gf_40pyr_80cov.csv',
#                  'llin_epi_gf_64pyr_80cov.csv', 'llin_epi_gf_40pyr_80cov.csv')
# filenames_GF_PBO_80cov = c('llin_mass_gf_PBO64pyr_80cov.csv', 'llin_mass_gf_PBO40pyr_80cov.csv',
#                        'llin_anc_gf_PBO64pyr_80cov.csv', 'llin_anc_gf_PBO40pyr_80cov.csv',
#                        'llin_epi_PBO.csv', 'llin_epi_04PBO.csv')
# 
# 
# ### GF net types - mass LLIN distributions (some districts do not receive any nets)
# for(ff in grep('mass', filenames_GF)){
#   pyr_values = read.csv(paste0(net_base_filepath, filenames_pyr[ff]))
#   pbo_values = read.csv(paste0(net_base_filepath, filenames_PBO[ff]))
#   
#   # subset to admins in each group
#   pyr_values = pyr_values[pyr_values$admin_name %in% admins_with_pyr,]
#   pyr_values$net_type_PBO = 0
#   pbo_values = pbo_values[pbo_values$admin_name %in% admins_with_PBO,]
#   pbo_values$net_type_PBO = 1
#   
#   # combine
#   llin_all = merge(pyr_values, pbo_values, by=intersect(colnames(pyr_values), colnames(pbo_values)), all.x=TRUE, all.y=TRUE)
#   # reorder by admin name
#   llin_all_ordered = llin_all[order(llin_all$admin_name),]
#   write.csv(llin_all_ordered, paste0(net_base_filepath, filenames_GF[ff]), row.names = FALSE)
# }
# 
# 
# ### GF mass LLIN distributions with all PBO nets (some districts do not receive any nets)
# for(ff in grep('mass', filenames_GF_PBO)){
#   pbo_values = read.csv(paste0(net_base_filepath, filenames_PBO[ff]))
#   
#   # subset to admins in each group
#   pbo_values = pbo_values[pbo_values$admin_name %in% c(admins_with_pyr, admins_with_PBO),]
#   pbo_values$net_type_PBO = 1
#   
#   # combine
#   llin_all = pbo_values
#   # reorder by admin name
#   llin_all_ordered = llin_all[order(llin_all$admin_name),]
#   write.csv(llin_all_ordered, paste0(net_base_filepath, filenames_GF_PBO[ff]), row.names = FALSE)
# }
# 
# ### GF net types - epi and anc LLIN distributions (districts without mass campaigns still receive pyrethroid ANC and EPI nets)
# for(ff in c(grep('anc', filenames_GF), grep('epi', filenames_GF))){
#   pyr_values = read.csv(paste0(net_base_filepath, filenames_pyr[ff]))
#   pbo_values = read.csv(paste0(net_base_filepath, filenames_PBO[ff]))
# 
#   # subset to admins in each group
#   pyr_values = pyr_values[pyr_values$admin_name %in% c(admins_with_pyr, admins_without_LLINs),]
#   pyr_values$net_type_PBO = 0
#   pbo_values = pbo_values[pbo_values$admin_name %in% admins_with_PBO,]
#   pbo_values$net_type_PBO = 1
#   
#   # combine
#   llin_all = merge(pyr_values, pbo_values, by=intersect(colnames(pyr_values), colnames(pbo_values)), all.x=TRUE, all.y=TRUE)
#   # reorder by admin name
#   llin_all_ordered = llin_all[order(llin_all$admin_name),]
#   write.csv(llin_all_ordered, paste0(net_base_filepath, filenames_GF[ff]), row.names = FALSE)
# }
# 
# 
# ### GF 80% coverage - mass LLIN distributions
# # replace effective coverage with 80% for U5, with lower rates in other age groups determined by rate relative to U5: 
# 
# # get country-average relative ITN use rates for different age groups
# relative_use_rates = read.csv(paste0(hbhi_dir,'/estimates_from_DHS/DHS_weighted_ITN_use_by_age_relative_to_u5.csv'))
# relative_use_rates = relative_use_rates[1,grepl('use_relative_to', colnames(relative_use_rates))]
# age_group_names=gsub('X','',colnames(relative_use_rates))
# age_group_names=gsub('_use_relative_to_u5','',age_group_names)
# relative_use_by_age = as.numeric(relative_use_rates)
# use_by_age_80cov = relative_use_by_age * 0.8
# 
# # use 80% coverage with the GF net type
# for(ff in grep('mass', filenames_GF)){
#   gf_values_80cov = read.csv(paste0(net_base_filepath, filenames_GF[ff]))
# 
#   # replace effective coverage if it is lower than the new effective coverage
#   for(aa in 1:length(age_group_names)){
#     gf_values_80cov[[paste0('itn_',age_group_names[aa])]] = sapply(gf_values_80cov[[paste0('itn_',age_group_names[aa])]], max, use_by_age_80cov[aa])
#   }
#   write.csv(gf_values_80cov, paste0(net_base_filepath, filenames_GF_80cov[ff]), row.names = FALSE)
# }
# # use 80% coverage with PBO net type in all mass-distribution districts
# for(ff in grep('mass', filenames_GF_PBO)){
#   gf_values_80covPBO = read.csv(paste0(net_base_filepath, filenames_GF_PBO[ff]))
#   
#   # replace effective coverage if it is lower than the new effective coverage
#   for(aa in 1:length(age_group_names)){
#     gf_values_80covPBO[[paste0('itn_',age_group_names[aa])]] = sapply(gf_values_80covPBO[[paste0('itn_',age_group_names[aa])]], max, use_by_age_80cov[aa])
#   }
#   write.csv(gf_values_80covPBO, paste0(net_base_filepath, filenames_GF_PBO_80cov[ff]), row.names = FALSE)
# }
# 
# ### GF 80% coverage - anc LLIN distributions
# for(ff in grep('anc', filenames_GF)){
#   gf_values_80cov = read.csv(paste0(net_base_filepath, filenames_GF[ff]))
#   
#   # replace effective coverage if it is lower than the new effective coverage
#   gf_values_80cov$coverage = sapply(gf_values_80cov$coverage, max, 0.8)
# 
#   write.csv(gf_values_80cov, paste0(net_base_filepath, filenames_GF_80cov[ff]), row.names = FALSE)
# }
# # use 80% coverage with PBO LLINs everywhere
# for(ff in grep('anc', filenames_PBO)){
#   gf_values_80covPBO = read.csv(paste0(net_base_filepath, filenames_PBO[ff]))
#   
#   # replace effective coverage if it is lower than the new effective coverage
#   gf_values_80covPBO$coverage = sapply(gf_values_80covPBO$coverage, max, 0.8)
#   
#   write.csv(gf_values_80covPBO, paste0(net_base_filepath, filenames_GF_PBO_80cov[ff]), row.names = FALSE)
# }
# 
# ### GF 80% coverage - epi LLIN distributions - already above 80% coverage in EPI visit, so can use the same values as GF
# 
# 
# 
# 
# ### GF plan for CHW-mediated ITN distribution. assume pyrethroid nets
# net_base_filepath = paste0(base_sim_input_dir, '/interventions_projections/')
# CHW_ITN_admins = gf_scenarios$adm2[gf_scenarios$llins_comm_pri == 1]
# lower_monthly_coverage = 0.095
# higher_monthly_coverage = 0.175
# highest_monthly_coverage = 1
# # get country-average relative ITN use rates for different age groups
# relative_use_rates = read.csv(paste0(hbhi_dir,'/estimates_from_DHS/DHS_weighted_ITN_use_by_age_relative_to_u5.csv'))
# relative_use_rates = relative_use_rates[1,grepl('use_relative_to', colnames(relative_use_rates))]
# age_group_names=gsub('X','',colnames(relative_use_rates))
# age_group_names=gsub('_use_relative_to_u5','',age_group_names)
# relative_use_by_age = as.numeric(relative_use_rates)
# 
# 
# # generate files for pyrethroid and PBO nets; for permethrin mortality=0.64 and 0.40; and for lower and higher montly coverages
# mass_dist_filenames = c('llin_mass_bau.csv','llin_mass_04pyr.csv', 'llin_mass_PBO.csv', 'llin_mass_04PBO.csv')
# filename_suffixes = c('_64perm', '_40perm', '_PBO64perm', '_PBO40perm')
# for(ff in 1:length(mass_dist_filenames)){
#   # use same permethrin mortality, kill/block rates, net retention times, etc. as each seed from mass net distribution file
#   mass_dist_df = read.csv(paste0(net_base_filepath, mass_dist_filenames[ff]))
#   mass_dist_df = mass_dist_df[,-which(colnames(mass_dist_df) =='nets_per_capita')]
#   # subset to first year and included admins
#   chw_1yr_df = mass_dist_df[mass_dist_df$year == min(mass_dist_df$year),]
#   chw_1yr_df$year = projection_start_year
#   chw_1yr_df$simday = 1
#   chw_1yr_df = chw_1yr_df[chw_1yr_df$admin_name %in% CHW_ITN_admins,]
#   # repeat for all years in projection
#   chw_df = chw_1yr_df
#   for(yy in 1:length((projection_start_year+1):projection_end_year)){
#     chw_1yr_df$year = ((projection_start_year+1):projection_end_year)[yy]
#     chw_1yr_df$simday = 365*yy
#     chw_df = rbind(chw_df, chw_1yr_df)
#   }
#   # replace coverage with monthly checking coverage from CHWs - lower estimate
#   use_by_age = lower_monthly_coverage * relative_use_by_age
#   for(aa in 1:length(age_group_names)){
#     chw_df[[paste0('itn_',age_group_names[aa])]] = use_by_age[aa]
#   }
#   write.csv(chw_df, paste0(base_sim_input_dir, '/interventions_projections/llin_monthly_chw_lower', filename_suffixes[ff], '.csv'), row.names = FALSE)
#   # replace coverage with monthly checking coverage from CHWs - higher estimate
#   use_by_age = higher_monthly_coverage * relative_use_by_age
#   for(aa in 1:length(age_group_names)){
#     chw_df[[paste0('itn_',age_group_names[aa])]] = use_by_age[aa]
#   }
#   write.csv(chw_df, paste0(base_sim_input_dir, '/interventions_projections/llin_monthly_chw_higher', filename_suffixes[ff], '.csv'), row.names = FALSE)
#   # replace coverage with monthly checking coverage from CHWs - highest estimate
#   use_by_age = highest_monthly_coverage * relative_use_by_age
#   for(aa in 1:length(age_group_names)){
#     chw_df[[paste0('itn_',age_group_names[aa])]] = use_by_age[aa]
#   }
#   write.csv(chw_df, paste0(base_sim_input_dir, '/interventions_projections/llin_monthly_chw_highest', filename_suffixes[ff], '.csv'), row.names = FALSE)
# }
# 
# 
# 
# ### New version of GF plan for CHW-mediated ITN distribution: instead of monthly checking with dont_allow_duplicates=1, which seems to have DTK bugs, have a discard time of exactly 365 and distribute nets every 365 days, at 80% coverage for U5
# net_base_filepath = paste0(base_sim_input_dir, '/interventions_projections/')
# CHW_ITN_admins = gf_scenarios$adm2[gf_scenarios$llins_comm_pri == 1]
# U5_LLIN_use_comm = 0.8
# # get country-average relative ITN use rates for different age groups
# relative_use_rates = read.csv(paste0(hbhi_dir,'/estimates_from_DHS/DHS_weighted_ITN_use_by_age_relative_to_u5.csv'))
# relative_use_rates = relative_use_rates[1,grepl('use_relative_to', colnames(relative_use_rates))]
# age_group_names=gsub('X','',colnames(relative_use_rates))
# age_group_names=gsub('_use_relative_to_u5','',age_group_names)
# relative_use_by_age = as.numeric(relative_use_rates)
# 
# 
# # generate files for pyrethroid and PBO nets; for permethrin mortality=0.64 and 0.40; and for lower and higher montly coverages
# mass_dist_filenames = c('llin_mass_bau.csv','llin_mass_04pyr.csv', 'llin_mass_PBO.csv', 'llin_mass_04PBO.csv')
# filename_suffixes = c('_64perm', '_40perm', '_PBO64perm', '_PBO40perm')
# for(ff in 1:length(mass_dist_filenames)){
#   # use same permethrin mortality, kill/block rates, net retention times, etc. as each seed from mass net distribution file
#   mass_dist_df = read.csv(paste0(net_base_filepath, mass_dist_filenames[ff]))
#   mass_dist_df = mass_dist_df[,-which(colnames(mass_dist_df) =='nets_per_capita')]
#   # subset to first year and included admins
#   chw_1yr_df = mass_dist_df[mass_dist_df$year == min(mass_dist_df$year),]
#   chw_1yr_df$year = projection_start_year
#   chw_1yr_df$simday = 1
#   chw_1yr_df = chw_1yr_df[chw_1yr_df$admin_name %in% CHW_ITN_admins,]
#   # repeat for all years in projection
#   chw_df = chw_1yr_df
#   for(yy in 1:length((projection_start_year+1):projection_end_year)){
#     chw_1yr_df$year = ((projection_start_year+1):projection_end_year)[yy]
#     chw_1yr_df$simday = 365*yy+1 
#     chw_df = rbind(chw_df, chw_1yr_df)
#   }
#   # replace coverage with strategic plan target of 80% use in U5, rescaled for other ages' use rates
#   use_by_age = U5_LLIN_use_comm * relative_use_by_age
#   for(aa in 1:length(age_group_names)){
#     chw_df[[paste0('itn_',age_group_names[aa])]] = use_by_age[aa]
#   }
#   write.csv(chw_df, paste0(base_sim_input_dir, '/interventions_projections/llin_annual_chw', filename_suffixes[ff], '.csv'), row.names = FALSE)
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
# ################################################################################
# #  update all LLIN projection inputs to assume a 2-year net retention time
# ################################################################################
# 
# longer_logmu=6.594
# 
# old_itn_filename_bases = c("llin_anc_04PBO.csv", "llin_anc_04pyr.csv" , 
#                       "llin_anc_bau.csv",  "llin_anc_gf_40pyr.csv",  "llin_anc_gf_40pyr_80cov.csv" , 
#                       "llin_anc_gf_64pyr.csv",  "llin_anc_gf_64pyr_80cov.csv",  "llin_anc_gf_PBO40pyr_80cov.csv", 
#                       "llin_anc_gf_PBO64pyr_80cov.csv", "llin_anc_PBO.csv" , "llin_annual_chw_40perm.csv", 
#                       "llin_annual_chw_64perm.csv",  "llin_annual_chw_PBO40perm.csv",  "llin_annual_chw_PBO64perm.csv", 
#                       "llin_epi_04PBO.csv", "llin_epi_04pyr.csv" ,  "llin_epi_bau.csv" , 
#                       "llin_epi_gf_40pyr.csv", "llin_epi_gf_64pyr.csv", "llin_epi_PBO.csv", 
#                       "llin_mass_04PBO.csv", "llin_mass_04pyr.csv",  "llin_mass_bau.csv" , 
#                       "llin_mass_gf_40pyr.csv", "llin_mass_gf_40pyr_80cov.csv", "llin_mass_gf_64pyr.csv", 
#                       "llin_mass_gf_64pyr_80cov.csv",  "llin_mass_gf_PBO40pyr.csv",  "llin_mass_gf_PBO40pyr_80cov.csv", 
#                       "llin_mass_gf_PBO64pyr.csv", "llin_mass_gf_PBO64pyr_80cov.csv", "llin_mass_PBO.csv")
# 
# old_itn_filenames = paste0(base_sim_input_dir, '/interventions_projections/', old_itn_filename_bases)
# 
# for(ff in 1:length(old_itn_filenames)){
#   cur_file = read.csv(old_itn_filenames[ff])
#   cur_file$net_life_lognormal_mu = longer_logmu
#   write.csv(cur_file, gsub('.csv', '_2yLLIN.csv', old_itn_filenames[ff]))
# }
# 
# 
# 
# 
# # add to-present LLIN input files with the longer LLIN halflife
# longer_logmu=6.594
# 
# old_itn_filename_bases = c('anc_itn_use_coverages_mort_2010_2020.csv', 'epi_itn_use_coverages_mort_2010_2020.csv', 'itn_mass_coverages_mort_2010_2020.csv')
# old_itn_filenames = paste0(base_sim_input_dir, '/interventions_2010_2020/', old_itn_filename_bases)
# for(ff in 1:length(old_itn_filenames)){
#   cur_file = read.csv(old_itn_filenames[ff])
#   cur_file$net_life_lognormal_mu = longer_logmu
#   write.csv(cur_file, gsub('.csv', '_2yLLIN.csv', old_itn_filenames[ff]))
# }
# 
# 
# 
# 
# 
# 
