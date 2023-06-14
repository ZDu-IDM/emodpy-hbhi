# future_projection_intervention_inputs.R

# these inputs are specific to a particular set of Burundi scenarios - these scripts should be replaced/edited depending on the use case


# needed inputs for current analyses:
#  - CM (continue 2021 coverage from "to-present" run)
#  - NMF (same as in "to-present" run)
#  - IRS
#     - IRS (continue IRS districts scheduled for 2021-2023; assume 80% effective coverage; assume "fludora fusion")
#     - IRS (continue IRS districts scheduled for 2021-2023; assume 61% effective coverage; assume "fludora fusion")
#  - LLIN mass distribution
#     - LLINS, mass distribution: assume same coverage and block/kill as 2019 distribution in a July 2023 and July 2026 distribution
#     - LLINS, mass distribution: assume same coverage as 2019 distribution in a July 2023 and July 2026 distribution; update block/kill to pyrethroid w/bioassay=0.4.
#     - LLINS, mass distribution: assume same coverage as 2019 distribution in a July 2023 and July 2026 distribution; update block/kill to PBO w/bioassay=0.64. 
#     - LLINS, mass distribution: assume same coverage as 2019 distribution in a July 2023 and July 2026 distribution; update block/kill to PBO w/bioassay=0.4. 
#  - LLIN routine distribution
#     - LLINs, routine distribution: assume same coverage and block/kill as in 2021
#     - LLINs, routine distribution: assume same coverage as in 2021; update block/kill to pyrethroid w/bioassay=0.4.
#     - LLINs, routine distribution: assume same coverage as in 2021; update block/kill to PBO w/bioassay=0.64.
#     - LLINs, routine distribution: assume same coverage as in 2021; update block/kill to PBO w/bioassay=0.4.


library(lubridate)
library(reshape2)
library(dplyr)

hbhi_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/snt_2023'
base_sim_input_dir = paste0(hbhi_dir, '/simulation_inputs')

script_dir = "C:/Users/moniqueam/Documents/malaria-snt-core"
source(paste0(script_dir,'/data_processing/setup_inputs/add_hut_trial_mort_block.R'))
source(paste0(script_dir,'/standardize_admin_names.R'))

# simulation inputs for "to-present" simulations
intervention_files_past = read.csv(paste0(base_sim_input_dir, '/_intervention_file_references/Interventions_to_present.csv'))
# create directory for projections
ifelse(!dir.exists(paste0(base_sim_input_dir, '/interventions_projections')), dir.create(paste0(base_sim_input_dir, '/interventions_projections')), FALSE)

projection_start_year = 2022  # assumed to be Jan 1
projection_end_year = 2030  # assumed to be Dec 31
future_start_year = 2024  # when new interventions typically begin
num_samples = 50  # number of seeds run in future projections

# WHO scenario information for GF
gf_scenarios = read.csv('C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/data/Burundi/WHO/snt_2022/BDI_NSP_PRI_mixes_reviewed.csv')
colnames(gf_scenarios)[colnames(gf_scenarios)=='adm1'] = 'State'
colnames(gf_scenarios)[colnames(gf_scenarios)=='adm2'] = 'admin_name'

# get dataframe with standardized admin names, states, archetype, and population information
ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
archetype_info = read.csv(ds_pop_df_filename)
# standardize names
gf_scenarios$admin_name = standardize_admin_names_in_vector(target_names=archetype_info$admin_name, origin_names=gf_scenarios$admin_name)

# # insecticide resistance
# mortality_df_name = paste0(hbhi_dir,'/ento/insecticide_resistance_DS_means/insecticide_Permethrin_mortality_DS_means.csv')

print_coverages = TRUE


################################################################################
# CM: continue coverage from final year into future projections
################################################################################
adult_multiplier = 0.5

##### BAU #####
# read in 2010-2021 input, subset to final year, update year and simday to first day of future projections, and set duration to -1
cm_past = read.csv(paste0(base_sim_input_dir, '/', intervention_files_past$CM_filename[1],'.csv'))
# final year
cm_past_sub = cm_past[cm_past$year == max(cm_past$year),]
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
  cov_all = cm_past_sub$U5_coverage
  print(paste0('.... Continuing coverage: ', length(unique(cm_past_sub$admin_name)), ' admins w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
}

##### 80% coverage #####
# effective coverage in U5 increased to 80% (if under 80% in 2020, otherwise kept as is); other coverages adjusted accordingly
cm_bau = read.csv(paste0(base_sim_input_dir, '/interventions_projections/cm_bau.csv'))
cm_80cov = cm_bau
cm_80cov$U5_coverage = sapply(cm_80cov$U5_coverage, max, 0.8)
cm_80cov$adult_coverage = cm_80cov$U5_coverage * adult_multiplier
cm_80cov$severe_coverage = sapply(cm_80cov$severe_coverage, max, 0.9)
if(nrow(cm_80cov) == nrow(distinct(cm_80cov))) {
  write.csv(cm_80cov, paste0(base_sim_input_dir, '/interventions_projections/cm_80cov.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the CM input file.')
}


##### PRI and NSP #####
# effective CM coverage increased by CHWs after future_start_year
cm_bau = read.csv(paste0(base_sim_input_dir, '/interventions_projections/cm_bau.csv'))
# keep the first year the same, and make changes after future_start_year
cm_bau$duration = (future_start_year-projection_start_year)*365
# estimated increase in coverage: 20% of children/adults who would not have received care in 2016 are seen by expanded CHW (adults only included if >5 is part of the CHW target)
u5_unseen_CHW_cov = 0.2
o5_unseen_CHW_cov = 0.2
# prioritized plan
admins_with_u5_CHW_cov = gf_scenarios$admin_name[gf_scenarios$iccm_pri %in% c('iCCM+PECADOM >5', 'iCCM')]
admins_with_o5_CHW_cov = gf_scenarios$admin_name[gf_scenarios$iccm_pri %in% c('iCCM+PECADOM >5')]
cm_CHW_pri = cm_bau
cm_CHW_pri$duration = -1
cm_CHW_pri$simday =  (future_start_year-projection_start_year)*365 + 1
cm_CHW_pri$year = future_start_year
cm_CHW_pri$U5_coverage[cm_CHW_pri$admin_name %in% admins_with_u5_CHW_cov] = cm_CHW_pri$U5_coverage[cm_CHW_pri$admin_name %in% admins_with_u5_CHW_cov] + (1-cm_CHW_pri$U5_coverage[cm_CHW_pri$admin_name %in% admins_with_u5_CHW_cov]) * u5_unseen_CHW_cov
cm_CHW_pri$adult_coverage[cm_CHW_pri$admin_name %in% admins_with_o5_CHW_cov] = cm_CHW_pri$adult_coverage[cm_CHW_pri$admin_name %in% admins_with_o5_CHW_cov] + (1-cm_CHW_pri$adult_coverage[cm_CHW_pri$admin_name %in% admins_with_o5_CHW_cov]) * o5_unseen_CHW_cov
cm_CHW_pri_all = merge(cm_CHW_pri, cm_bau, all=TRUE)
if(nrow(cm_CHW_pri_all) == nrow(distinct(cm_CHW_pri_all))) {
  write.csv(cm_CHW_pri_all, paste0(base_sim_input_dir, '/interventions_projections/cm_pri.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the CM input file.')
}

if(print_coverages){
  print('Case management')
  cov_all = cm_CHW_pri_all$U5_coverage
  print(paste0('.... U5 coverage: ', length(unique(cm_CHW_pri_all$admin_name)), ' admins w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
  cov_all = cm_CHW_pri_all$adult_coverage
  print(paste0('.... adult coverage: ', length(unique(cm_CHW_pri_all$admin_name)), ' admins w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
}

# NSP
admins_with_u5_CHW_cov = gf_scenarios$admin_name[gf_scenarios$iccm_nsp %in% c('iCCM+PECADOM >5', 'iCCM')]
admins_with_o5_CHW_cov = gf_scenarios$admin_name[gf_scenarios$iccm_nsp %in% c('iCCM+PECADOM >5')]
cm_CHW_nsp = cm_bau
cm_CHW_nsp$duration = -1
cm_CHW_nsp$simday =  (future_start_year-projection_start_year)*365 + 1
cm_CHW_nsp$year = future_start_year
cm_CHW_nsp$U5_coverage[cm_CHW_nsp$admin_name %in% admins_with_u5_CHW_cov] = cm_CHW_nsp$U5_coverage[cm_CHW_nsp$admin_name %in% admins_with_u5_CHW_cov] + (1-cm_CHW_nsp$U5_coverage[cm_CHW_nsp$admin_name %in% admins_with_u5_CHW_cov]) * u5_unseen_CHW_cov
cm_CHW_nsp$adult_coverage[cm_CHW_nsp$admin_name %in% admins_with_o5_CHW_cov] = cm_CHW_nsp$adult_coverage[cm_CHW_nsp$admin_name %in% admins_with_o5_CHW_cov] + (1-cm_CHW_nsp$adult_coverage[cm_CHW_nsp$admin_name %in% admins_with_o5_CHW_cov]) * o5_unseen_CHW_cov
cm_CHW_nsp_all = merge(cm_CHW_nsp, cm_bau, all=TRUE)
if(nrow(cm_CHW_nsp_all) == nrow(distinct(cm_CHW_nsp_all))) {
  write.csv(cm_CHW_nsp_all, paste0(base_sim_input_dir, '/interventions_projections/cm_nsp.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the CM input file.')
}



################################################################################
# IRS
################################################################################
irs_day_of_year = 258  # around September 15th
effective_irs_coverage_multiplier=0.78  # reduces effective individual coverage to account for dwelling modification/washing, migration, incomplete spraying, etc.
irs_past = read.csv(paste0(base_sim_input_dir, '/', intervention_files_past$IRS_filename[1],'.csv'))

##### BAU #####
# subset to 2022
irs_sub = irs_past[irs_past$year == 2022,]
irs_future = irs_sub
irs_future$year = projection_start_year
irs_future$simday = irs_day_of_year
# add IRS for each year in projection
if(projection_start_year<projection_end_year){
  for(yy in 1:length((projection_start_year+1):projection_end_year)){
    irs_cur = irs_sub
    irs_cur$year = projection_start_year+yy
    irs_cur$simday = 365*yy + irs_day_of_year
    irs_future = rbind(irs_future,irs_cur)
  }
}
if(nrow(irs_future) == nrow(distinct(irs_future))) {
  write.csv(irs_future, paste0(base_sim_input_dir, '/interventions_projections/irs_bau.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the IRS input file.')
}

###### IRS only continues until future_start_year-1 ########
irs_future_stopped = irs_future[irs_future$year < future_start_year,]
write.csv(irs_future_stopped, paste0(base_sim_input_dir, '/interventions_projections/irs_before', future_start_year, '.csv'), row.names = FALSE)


if(print_coverages){
  print('IRS: BAU')
  cov_all = irs_future_stopped$effective_coverage / effective_irs_coverage_multiplier
  print(paste0('.... IRS effective coverage: ', length(unique(irs_future_stopped$admin_name)), ' admins w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
}


# ##### 80% coverage #####
# irs_future_80 = irs_future
# irs_future_80$effective_coverage = 0.8 * effective_irs_coverage_multiplier
# if(nrow(irs_future_80) == nrow(distinct(irs_future_80))) {
#   write.csv(irs_future_80, paste0(base_sim_input_dir, '/interventions_projections/irs_80cov.csv'), row.names = FALSE)
# } else{
#   warning('PROBLEM DETECTED: there are duplicate campaigns in the IRS input file.')
# }



##### PRI and NSP #####
# set parameters for IRS - plan to use actellic and fludora fusion, so choose parameters between those products for all years, with parameters based on Sherrad-Smith et al. 
# create IRS input file with different multipliers for different seeds
irs_kill_seed_multipliers = c(0.3, 0.15, 0.45)  # c(0.3, 0.2, 0.4, 0.1, 0.5)
irs_initial_kill_unscaled = (0.85+0.6)/2
insecticide_halflife = (255+100)/2
irs_mean_duration = insecticide_halflife / log(2)
irs_coverage = 0.8
irs_doys = irs_day_of_year # day(s) of year for spraying
start_year_irs_change = future_start_year
irs_start_day = (start_year_irs_change-projection_start_year)*365 + irs_doys[1]

# before the IRS change, continue BAU
if(projection_start_year < start_year_irs_change){
  irs_sub = irs_past[irs_past$year == 2022,]
  irs_future_bau = irs_sub
  irs_future_bau$year = projection_start_year
  irs_future_bau$simday = irs_day_of_year
  # add IRS for each year in projection
  if((projection_start_year+1)<start_year_irs_change){
    for(yy in 1:length((projection_start_year+1):(start_year_irs_change-1))){
      irs_cur = irs_sub
      irs_cur$year = projection_start_year+yy
      irs_cur$simday = 365*yy + irs_day_of_year
      irs_future_bau = rbind(irs_future_bau,irs_cur)
    }
  }
}

# admins with IRS in prioritized plan and current version of NSP
admins_with_irs_pri = gf_scenarios$admin_name[gf_scenarios$irs_pri == 1]
admins_with_irs_nsp = gf_scenarios$admin_name[gf_scenarios$irs_yn == 1]
admins_list = list(admins_with_irs_pri, admins_with_irs_nsp)
scenario_names = c('pri','nsp')
for(ii in 1:length(admins_list)){
  irs_admins = admins_list[[ii]]
  if(length(irs_admins) > 0){
    irs_future = data.frame(admin_name = irs_admins, 
                            effective_coverage = irs_coverage * effective_irs_coverage_multiplier,
                            year = start_year_irs_change,
                            simday = irs_start_day,
                            initial_kill_unscaled = irs_initial_kill_unscaled,
                            mean_duration = irs_mean_duration)
    # add copy for each spray day in a year
    irs_copy=irs_future
    if(length(irs_doys)>1){
      for(ii in 2:length(irs_doys)){
        irs_cur = irs_copy
        irs_cur$simday = (start_year_irs_change-projection_start_year)*365 + irs_doys[ii] 
        irs_future = rbind(irs_future,irs_cur)
      }
    }
    # add IRS for each year in projection
    irs_copy=irs_future
    if(start_year_irs_change < projection_end_year){
      for(yy in 1:length((start_year_irs_change+1):projection_end_year)){
        irs_cur = irs_copy
        irs_cur$year = start_year_irs_change + yy
        irs_cur$simday = irs_copy$simday + 365*yy
        irs_future = rbind(irs_future,irs_cur)
      }
    }
    
    # add seeds with initial kill rates rescaled based on irs_kill_seed_multipliers 
    if(length(irs_kill_seed_multipliers)>1){  # determine whether there will be different values for different seeds
      num_repeats = ceiling((num_samples+1) / length(irs_kill_seed_multipliers))
      multiplier_each_seed = rep(irs_kill_seed_multipliers, num_repeats)[1:(num_samples+1)]
      seeds_df = data.frame('seed'=1:(num_samples+1), 'kill_multiplier'=multiplier_each_seed)
      irs_future = merge(irs_future, seeds_df, all=TRUE)
      irs_future$initial_kill = irs_future$initial_kill_unscaled * irs_future$kill_multiplier
    } else if(length(irs_kill_seed_multipliers) == 1){
      irs_future$initial_kill = irs_future$initial_kill_unscaled * irs_kill_seed_multipliers[1]
    } else{
      warning('PROBLEM: need to specify a valid parameter value for irs_kill_seed_multipliers.')
    }
    
    # combine the BAU IRS years with the future projection years
    irs_future = merge(irs_future_bau, irs_future, all=TRUE)
    irs_future = irs_future[,which(colnames(irs_future) %in% c('admin_name', 'year','round','initial_kill','mean_duration', 'effective_coverage','simday', 'insecticide', 'seed'))]
    if(nrow(irs_future) == nrow(distinct(irs_future))) {
      write.csv(irs_future, paste0(base_sim_input_dir, '/interventions_projections/irs_', scenario_names[ii], '.csv'), row.names = FALSE)
    } else{
      warning('PROBLEM DETECTED: there are duplicate campaigns in the IRS input file.')
    }
  }
}


# # version of NSP IRS with lower initial kill value
# irs_nsp_lower_kill = read.csv(paste0(base_sim_input_dir, '/interventions_projections/irs_nsp.csv'))
# effective_irs_kill_multiplier_lower = 0.1
# irs_nsp_lower_kill$initial_kill = irs_nsp_lower_kill$initial_kill / effective_irs_kill_multiplier * effective_irs_kill_multiplier_lower
# write.csv(irs_nsp_lower_kill, paste0(base_sim_input_dir, '/interventions_projections/irs', round(100*effective_irs_kill_multiplier_lower), '_nsp.csv'), row.names = FALSE)


#############################################
# functions for LLINs
#############################################
first_year_not_pri1 = future_start_year  # first year when the new net types are used; nets from pri1 are used before that

# find the block/kill values given the net type in each LGA and the bio-mortality in each LGA
# function to update / standardize the names of different types of nets. Note that we do not treat 'urban' specifications differently
standardize_net_type_names = function(net_name){
  if(grepl('PBO', net_name) | grepl('pbo', net_name) | grepl('Pyrethroid-piperonyl butoxide', net_name)){
    return('PBO')
  } else if(grepl('Dual AI', net_name) | grepl('IG2', net_name) | grepl('ig2', net_name)){
    return('IG2')
  } else if(grepl('none', net_name) | grepl('No ITN', net_name) | grepl('NoITN', net_name) | grepl('Sans camp', net_name) | grepl('community', net_name) | grepl('0', net_name) |  is.na(net_name)){
    return(NA)
  } else{
    return('standard')
  }
}

# given the years/month/coverages of distribution and net type for each admin, combine into dataframe with relevant distribution info
add_net_efficacy_info = function(llin_campaign_df){
  # standardize net type names
  llin_campaign_df$llin_type = sapply(llin_campaign_df$llin_type, standardize_net_type_names)
  llin_campaign_df = add_kill_block_to_df(itn_df=llin_campaign_df, frac_reassign_feed_survive=frac_reassign_feed_survive)
  
  # if the net type is 'No ITN' (recoded as NA), the campaign row should be deleted
  if(any(is.na(llin_campaign_df$llin_type))) llin_campaign_df = llin_campaign_df[-which(is.na(llin_campaign_df$llin_type)),]
  return(llin_campaign_df)
}



################################################################################
#  LLIN mass distribution
################################################################################
frac_reassign_feed_survive = 0.9  # fraction of mosquitos that should feed and die that are assigned to feed and survive in the dtk
future_mass_itn_years = c(2022, 2025, 2028)
future_mass_itn_months = c(9,9,9)
admin_standard_2022 = 'Ryansoro'
llin_mass_past = read.csv(paste0(base_sim_input_dir, '/', intervention_files_past$ITN_filename[1],'.csv'))

##### get coverage, timing, and bioassay mortality for future distributions #####
# LLINS, mass distribution: assume same coverage and block/kill as 2019 distribution in future distributions
# subset to 2019 distribution
llin_mass_past_sub= llin_mass_past[llin_mass_past$year == 2019,]
llin_mass_past_sub = llin_mass_past_sub[,-which(colnames(llin_mass_past_sub)=='dist_date')]
llin_mass_future = llin_mass_past_sub
llin_mass_future$year = future_mass_itn_years[1]
llin_mass_future$month = future_mass_itn_months[1]
llin_mass_future$simday = round((future_mass_itn_years[1] - projection_start_year)*365 + 30.4*future_mass_itn_months[1] + 15)
if(length(future_mass_itn_years)>1){
  for(yy in 2:length(future_mass_itn_years)){
    llin_mass_past_cur = llin_mass_past_sub
    llin_mass_past_cur$year = future_mass_itn_years[yy]
    llin_mass_past_cur$month = future_mass_itn_months[yy]
    llin_mass_past_cur$simday = round((future_mass_itn_years[yy] - projection_start_year)*365 + 30.4*future_mass_itn_months[yy] + 15)
    llin_mass_future = rbind(llin_mass_future, llin_mass_past_cur)
  }
}
llin_mass_future = llin_mass_future[,which(!(colnames(llin_mass_future) %in% c('block_initial','kill_initial','llin_type')))]


##### net type and block/kill for each scenario #####
# set up the net type to be used for distributions in each year / state (these will differ depending on the scenario)
# NSP
lga_mass_nettype_nsp = gf_scenarios[,c('admin_name', 'llins_mass_nsp_type')]
colnames(lga_mass_nettype_nsp)[colnames(lga_mass_nettype_nsp)=='llins_mass_nsp_type'] = 'llin_type'
unique(lga_mass_nettype_nsp$llin_type)
# prioritized - v1 (this is mostly BAU from 2022)
lga_mass_nettype_pri1 = gf_scenarios[,c('admin_name', 'llins_mass_pri_type_v1')]
colnames(lga_mass_nettype_pri1)[colnames(lga_mass_nettype_pri1)=='llins_mass_pri_type_v1'] = 'llin_type'
unique(lga_mass_nettype_pri1$llin_type)
# prioritized - v2 (priority upgrades of pyrethroid nets from BAU 2022)
lga_mass_nettype_pri2 = gf_scenarios[,c('admin_name', 'llins_mass_pri_type_v2')]
colnames(lga_mass_nettype_pri2)[colnames(lga_mass_nettype_pri2)=='llins_mass_pri_type_v2'] = 'llin_type'
unique(lga_mass_nettype_pri2$llin_type)
# standard pyrethroid in all districts (including those with IRS)
lga_mass_nettype_allPyr = data.frame('admin_name'=gf_scenarios$admin_name, 'llin_type'='standard')
# IG2 in all districts (including those with IRS)
lga_mass_nettype_allIG2 = data.frame('admin_name'=gf_scenarios$admin_name, 'llin_type'='IG2')
# create list of net scenarios with corresponding vector describing the name of each (allowing for looping later)
scenario_net_names = c('nsp_noIRS', 'pri1', 'pri2', 'allPyr', 'allIG2')
scenario_net_type_list = list(lga_mass_nettype_nsp, lga_mass_nettype_pri1, lga_mass_nettype_pri2, lga_mass_nettype_allPyr, lga_mass_nettype_allIG2)


# prepare the campaign input file for each of the scenarios and add in the net type for all admins and get the corresponding block/kill parameters
for(ii in 1:length(scenario_net_type_list)){
  # separate distributions into earlier year(s) following prioritized plan and later years following scenario plan
  llin_mass_old = llin_mass_future[llin_mass_future$year < first_year_not_pri1,]
  llin_mass_new = llin_mass_future[llin_mass_future$year >= first_year_not_pri1,]
  
  # merge the net type information with the dataframe with rows for each campaign
  llin_date_cov_type_old = merge(llin_mass_old, lga_mass_nettype_pri1)
  llin_date_cov_type_new = merge(llin_mass_new, scenario_net_type_list[[ii]])
  
    # change the 2022 net type for select admins
  llin_date_cov_type_old$llin_type[llin_date_cov_type_old$admin_name %in% admin_standard_2022 & llin_date_cov_type_old$year==2022] = 'standard'
  
  # merge all campaigns
  llin_campaign_df = merge(llin_date_cov_type_old, llin_date_cov_type_new, all=TRUE)
  
  # get the blocking/killing given net type and bio-mortality
  scenario_campaign = add_net_efficacy_info(llin_campaign_df=llin_campaign_df)
  
  # add in the date and day of simulation
  scenario_campaign$date = as.Date(paste0(scenario_campaign$year, '-', scenario_campaign$month, '-15'))
  scenario_campaign$simday = as.numeric(scenario_campaign$date - as.Date(paste0(projection_start_year, '-01-01')))
  
  # make sure that there aren't duplicates anywhere such that an admin gets multiple repeated distributions in a year
  check_df = scenario_campaign[scenario_campaign$seed==1,c('admin_name', 'year')]
  if(nrow(check_df) != nrow(distinct(check_df))){
    warning('PROBLEM DETECTED: some admins have multiple distributions in a year')
  } else{
    write.csv(scenario_campaign, paste0(base_sim_input_dir, '/interventions_projections/llin_mass_', scenario_net_names[ii], '.csv'), row.names = FALSE)
  }
  
  if(print_coverages){
    print(paste0('LLIN mass dist - ', scenario_net_names[ii],': '))
    scenario_campaign_distinct_project = distinct(scenario_campaign[scenario_campaign$year>=first_year_not_pri1,c('admin_name','itn_u5')])
    cov_all = scenario_campaign_distinct_project$itn_u5
    print(paste0('....', length(unique(scenario_campaign_distinct_project$admin_name)), ' admins w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
    # net types
    scenario_campaign_distinct_project = distinct(scenario_campaign[scenario_campaign$year>=first_year_not_pri1,c('admin_name', 'llin_type')])
    print(paste0('.... PBO:', length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='PBO'])), 
                 '; IG2: ', length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='IG2'])),
                 '; standard: ', length(unique(scenario_campaign_distinct_project$admin_name)) - length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='PBO'])) - length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='IG2']))))
  }
}

# remove IRS districts from NSP file for version with IRS
irs_nsp = read.csv(paste0(base_sim_input_dir, '/interventions_projections/irs_nsp.csv'))
irs_nsp = irs_nsp[irs_nsp$effective_coverage > 0,]
irs_nsp = irs_nsp[,c('admin_name', 'year')]
irs_nsp = distinct(irs_nsp)
irs_nsp$no_irs=FALSE
llin_nsp_w_irs = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_mass_nsp_noIRS.csv'))
llin_nsp_w_irs = merge(llin_nsp_w_irs, irs_nsp, all.x=TRUE)
llin_nsp_w_irs$no_irs[is.na(llin_nsp_w_irs$no_irs)]=TRUE
llin_nsp_w_irs = llin_nsp_w_irs[llin_nsp_w_irs$no_irs,]
# make sure that there aren't duplicates anywhere such that an admin gets multiple repeated distributions in a year
check_df = llin_nsp_w_irs[llin_nsp_w_irs$seed==1,c('admin_name', 'year')]
if(nrow(check_df) != nrow(distinct(check_df))){
  warning('PROBLEM DETECTED: some admins have multiple distributions in a year')
} else{
  write.csv(llin_nsp_w_irs, paste0(base_sim_input_dir, '/interventions_projections/llin_mass_nsp.csv'), row.names = FALSE)
}

if(print_coverages){
  print(paste0('LLIN mass dist - nsp_withIRS: '))
  scenario_campaign_distinct_project = distinct(llin_nsp_w_irs[llin_nsp_w_irs$year>=first_year_not_pri1,c('admin_name','itn_u5')])
  cov_all = scenario_campaign_distinct_project$itn_u5
  print(paste0('....', length(unique(scenario_campaign_distinct_project$admin_name)), ' admins w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
  # net types
  scenario_campaign_distinct_project = distinct(llin_nsp_w_irs[llin_nsp_w_irs$year>=first_year_not_pri1,c('admin_name', 'llin_type')])
  print(paste0('.... PBO:', length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='PBO'])), 
               '; IG2: ', length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='IG2'])),
               '; standard: ', length(unique(scenario_campaign_distinct_project$admin_name)) - length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='PBO'])) - length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='IG2']))))
}






################################################################################
# LLIN continuous community distribution
################################################################################
# distribute nets every 365 days, with a discard of 365 days, at U5_LLIN_use_comm use coverage for U5

#### set coverage and bioassay mortality for each admin
# use same permethrin mortality, kill/block rates, net retention times, etc. as each seed from mass net distribution file
llin_mass_past = read.csv(paste0(base_sim_input_dir, '/', intervention_files_past$ITN_filename[1],'.csv'))
# subset to 2019 distribution
llin_mass_past_sub= llin_mass_past[llin_mass_past$year == 2019,]
llin_mass_past_sub = llin_mass_past_sub[,which(colnames(llin_mass_past_sub) %in% c('admin_name', 'seed', 'net_life_lognormal_mu', 'net_life_lognormal_sigma', 'bio_mortality','indoor_net_protection'))]
llin_admin_info = llin_mass_past_sub
# get country-average relative ITN use rates for different age groups
U5_LLIN_use_comm = 0.8
relative_use_rates = read.csv(paste0(hbhi_dir,'/estimates_from_DHS/DHS_weighted_ITN_use_by_age_relative_to_u5.csv'))
relative_use_rates = relative_use_rates[1,grepl('use_relative_to', colnames(relative_use_rates))]
age_group_names=gsub('X','',colnames(relative_use_rates))
age_group_names=gsub('_use_relative_to_u5','',age_group_names)
relative_use_by_age = as.numeric(relative_use_rates)


##### admin subset and net type for each scenario #####
# set up the net type to be used for distributions in each year / state (these will differ depending on the scenario)
# NSP
lga_comm_nettype_nsp = gf_scenarios[,c('admin_name', 'llins_comm_nsp_type')]
colnames(lga_comm_nettype_nsp)[colnames(lga_comm_nettype_nsp)=='llins_comm_nsp_type'] = 'llin_type'
unique(lga_comm_nettype_nsp$llin_type)
# prioritized - v1 (this is mostly BAU from 2022)
lga_comm_nettype_pri1 = gf_scenarios[,c('admin_name', 'llins_comm_pri_type_v1')]
colnames(lga_comm_nettype_pri1)[colnames(lga_comm_nettype_pri1)=='llins_comm_pri_type_v1'] = 'llin_type'
unique(lga_comm_nettype_pri1$llin_type)
# prioritized - v2 (priority upgrades of pyrethroid nets from BAU 2022)
lga_comm_nettype_pri2 = gf_scenarios[,c('admin_name', 'llins_comm_pri_type_v2')]
colnames(lga_comm_nettype_pri2)[colnames(lga_comm_nettype_pri2)=='llins_comm_pri_type_v2'] = 'llin_type'
unique(lga_comm_nettype_pri2$llin_type)
# create list of net scenarios with corresponding vector describing the name of each (allowing for looping later)
scenario_net_names = c('nsp', 'pri1', 'pri2')
scenario_net_type_list = list(lga_comm_nettype_nsp, lga_comm_nettype_pri1, lga_comm_nettype_pri2)


# prepare the campaign input file for each of the scenarios and add in the net type for all admins and get the corresponding block/kill parameters
for(ii in 1:length(scenario_net_type_list)){
  # separate distributions into earlier year(s) following prioritized plan and later years following scenario plan
  llin_comm_old = llin_admin_info
  llin_comm_new = llin_admin_info
  
  # merge the net type information with the dataframe with rows for each campaign
  llin_date_cov_type_old = merge(llin_comm_old, lga_comm_nettype_pri1)
  llin_date_cov_type_new = merge(llin_comm_new, scenario_net_type_list[[ii]])
  
  # get the blocking/killing given net type and bio-mortality and remove admins without community distribution
  llin_date_cov_type_old = add_net_efficacy_info(llin_campaign_df=llin_date_cov_type_old)
  llin_date_cov_type_new = add_net_efficacy_info(llin_campaign_df=llin_date_cov_type_new)
  
  # repeat for all years in projection
  llin_date_cov_type_all = data.frame()
  if(projection_start_year < future_start_year){
    # add pri1 for the initial years
    for(yy in 1:length(projection_start_year:(future_start_year-1))){
      cur_df = llin_date_cov_type_old
      cur_df$year = projection_start_year + yy - 1
      cur_df$simday = (yy-1)*365+1
      if(nrow(llin_date_cov_type_all)>0){
        llin_date_cov_type_all = merge(llin_date_cov_type_all, cur_df, all=TRUE)
      } else{
        llin_date_cov_type_all = cur_df
      }
    }
  }
  # add the current scenario for future years
  if(future_start_year < projection_end_year){
    for(yy in 1:length(future_start_year:projection_end_year)){
      cur_df = llin_date_cov_type_new
      cur_df$year = future_start_year  + yy - 1 
      cur_df$simday = (future_start_year+yy-1 - projection_start_year)*365+1
      if(nrow(llin_date_cov_type_all)>0){
        llin_date_cov_type_all = merge(llin_date_cov_type_all, cur_df, all=TRUE)
      } else{
        llin_date_cov_type_all = cur_df
      }
    }
  }
  
  # add in coverage, rescaled for age-based use
  use_by_age = U5_LLIN_use_comm * relative_use_by_age
  for(aa in 1:length(age_group_names)){
    llin_date_cov_type_all[[paste0('itn_',age_group_names[aa])]] = use_by_age[aa]
  }
  
  
  # make sure that there aren't duplicates anywhere such that an admin gets multiple repeated distributions in a year
  check_df = llin_date_cov_type_all[llin_date_cov_type_all$seed==1,c('admin_name', 'year')]
  if(nrow(check_df) != nrow(distinct(check_df))){
    warning('PROBLEM DETECTED: some admins have multiple distributions in a year')
  } else{
    write.csv(llin_date_cov_type_all, paste0(base_sim_input_dir, '/interventions_projections/llin_comm_', scenario_net_names[ii], '.csv'), row.names = FALSE)
  }
  
  if(print_coverages){
    print(paste0('LLIN comm dist - ', scenario_net_names[ii],': '))
    scenario_campaign_distinct_project = distinct(llin_date_cov_type_all[llin_date_cov_type_all$year>=first_year_not_pri1,c('admin_name','itn_u5')])
    cov_all = scenario_campaign_distinct_project$itn_u5
    print(paste0('....', length(unique(scenario_campaign_distinct_project$admin_name)), ' admins w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
    # net types
    scenario_campaign_distinct_project = distinct(llin_date_cov_type_all[llin_date_cov_type_all$year>=first_year_not_pri1,c('admin_name', 'llin_type')])
    print(paste0('.... PBO:', length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='PBO'])), 
                 '; IG2: ', length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='IG2'])),
                 '; standard: ', length(unique(scenario_campaign_distinct_project$admin_name)) - length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='PBO'])) - length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='IG2']))))
  }
}





################################################################################
#  LLIN routine distribution
################################################################################

##### net type and block/kill for each scenario #####
# set up the net type to be used for distributions in each year / state (these will differ depending on the scenario)
# NSP
lga_rout_nettype_nsp = gf_scenarios[,c('admin_name', 'llins_rout_nsp_type')]
colnames(lga_rout_nettype_nsp)[colnames(lga_rout_nettype_nsp)=='llins_rout_nsp_type'] = 'llin_type'
unique(lga_rout_nettype_nsp$llin_type)
# prioritized - v1 (this is mostly BAU from 2022)
lga_rout_nettype_pri1 = gf_scenarios[,c('admin_name', 'llins_rout_pri_type_v1')]
colnames(lga_rout_nettype_pri1)[colnames(lga_rout_nettype_pri1)=='llins_rout_pri_type_v1'] = 'llin_type'
unique(lga_rout_nettype_pri1$llin_type)
# prioritized - v2 (priority upgrades of pyrethroid nets from BAU 2022)
lga_rout_nettype_pri2 = gf_scenarios[,c('admin_name', 'llins_rout_pri_type_v2')]
colnames(lga_rout_nettype_pri2)[colnames(lga_rout_nettype_pri2)=='llins_rout_pri_type_v2'] = 'llin_type'
unique(lga_rout_nettype_pri2$llin_type)
# create list of net scenarios with corresponding vector describing the name of each (allowing for looping later)
scenario_net_names = c('nsp', 'pri1', 'pri2')
scenario_net_type_list = list(lga_rout_nettype_nsp, lga_rout_nettype_pri1, lga_rout_nettype_pri2)



#######   ANC   ########
# get coverage, timing, and bioassay mortality for future distributions
llin_anc_past = read.csv(paste0(base_sim_input_dir, '/', intervention_files_past$ANC_ITN_filename[1],'.csv'))
# subset to 2021 distribution
llin_anc_sub= llin_anc_past[llin_anc_past$year == 2021,]
llin_anc_future = llin_anc_sub
llin_anc_future = llin_anc_future[,which(!(colnames(llin_anc_future) %in% c('block_initial','kill_initial','llin_type', 'year', 'simday', 'duration')))]

# prepare the campaign input file for each of the scenarios and add in the net type for all admins and get the corresponding block/kill parameters
for(ii in 1:length(scenario_net_type_list)){
  
  # separate distributions into earlier year(s) following prioritized plan and later years following scenario plan
  llin_anc_old = llin_anc_future
  llin_anc_old$year = projection_start_year
  llin_anc_old$simday = 0
  llin_anc_old$duration = (first_year_not_pri1 - projection_start_year)*365
  
  llin_anc_new = llin_anc_future
  llin_anc_new$year = first_year_not_pri1
  llin_anc_new$simday = (first_year_not_pri1 - projection_start_year)*365 + 1
  llin_anc_new$duration = -1
  
  # merge the net type information with the dataframe with rows for each campaign
  llin_date_cov_type_old = merge(llin_anc_old, lga_rout_nettype_pri1)
  llin_date_cov_type_new = merge(llin_anc_new, scenario_net_type_list[[ii]])
  
  # merge all campaigns
  llin_anc_df = merge(llin_date_cov_type_old, llin_date_cov_type_new, all=TRUE)
  
  # get the blocking/killing given net type and bio-mortality
  scenario_file = add_net_efficacy_info(llin_campaign_df=llin_anc_df)
  
  # make sure that there aren't duplicates anywhere such that an admin gets multiple repeated distributions in a year
  check_df = scenario_file[scenario_file$seed==1,c('admin_name', 'year')]
  if(nrow(check_df) != nrow(distinct(check_df))){
    warning('PROBLEM DETECTED: some admins have multiple distributions in a year')
  } else{
    write.csv(scenario_file, paste0(base_sim_input_dir, '/interventions_projections/llin_anc_', scenario_net_names[ii], '.csv'), row.names = FALSE)
  }
  
  if(print_coverages){
    print(paste0('LLIN ANC dist - ', scenario_net_names[ii],': '))
    scenario_campaign_distinct_project = distinct(scenario_file[scenario_file$year>=first_year_not_pri1,c('admin_name','coverage')])
    cov_all = scenario_campaign_distinct_project$coverage
    print(paste0('....', length(unique(scenario_campaign_distinct_project$admin_name)), ' admins w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
    # net types
    scenario_campaign_distinct_project = distinct(scenario_file[scenario_file$year>=first_year_not_pri1,c('admin_name', 'llin_type')])
    print(paste0('.... PBO:', length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='PBO'])), 
                 '; IG2: ', length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='IG2'])),
                 '; standard: ', length(unique(scenario_campaign_distinct_project$admin_name)) - length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='PBO'])) - length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='IG2']))))
  }
}


# ##### update block/kill to pyrethroid w/bioassay=0.4.
# llin_future_04 = llin_anc_future
# llin_future_04 = llin_future_04[,-which(colnames(llin_future_04) %in% c('block_initial','kill_initial'))]
# llin_future_04$bio_mortality = 0.4
# llin_future_04 = add_kill_block_to_df(itn_df=llin_future_04, frac_reassign_feed_survive=frac_reassign_feed_survive)
# write.csv(llin_future_04, paste0(base_sim_input_dir, '/interventions_projections/llin_anc_04pyr.csv'), row.names = FALSE)



#######   EPI   ########
# get coverage, timing, and bioassay mortality for future distributions
llin_epi_past = read.csv(paste0(base_sim_input_dir, '/', intervention_files_past$EPI_ITN_filename[1],'.csv'))
# subset to 2021 distribution
llin_epi_sub= llin_epi_past[llin_epi_past$year == 2021,]
llin_epi_future = llin_epi_sub
llin_epi_future = llin_epi_future[,which(!(colnames(llin_epi_future) %in% c('block_initial','kill_initial','llin_type', 'year', 'simday', 'duration')))]

# prepare the campaign input file for each of the scenarios and add in the net type for all admins and get the corresponding block/kill parameters
for(ii in 1:length(scenario_net_type_list)){
  
  # separate distributions into earlier year(s) following prioritized plan and later years following scenario plan
  llin_epi_old = llin_epi_future
  llin_epi_old$year = projection_start_year
  llin_epi_old$simday = 0
  llin_epi_old$duration = (first_year_not_pri1 - projection_start_year)*365
  
  llin_epi_new = llin_epi_future
  llin_epi_new$year = first_year_not_pri1
  llin_epi_new$simday = (first_year_not_pri1 - projection_start_year)*365 + 1
  llin_epi_new$duration = -1
  
  # merge the net type information with the dataframe with rows for each campaign
  llin_date_cov_type_old = merge(llin_epi_old, lga_rout_nettype_pri1)
  llin_date_cov_type_new = merge(llin_epi_new, scenario_net_type_list[[ii]])
  
  # merge all campaigns
  llin_epi_df = merge(llin_date_cov_type_old, llin_date_cov_type_new, all=TRUE)
  
  # get the blocking/killing given net type and bio-mortality
  scenario_file = add_net_efficacy_info(llin_campaign_df=llin_epi_df)
  
  # make sure that there aren't duplicates anywhere such that an admin gets multiple repeated distributions in a year
  check_df = scenario_file[scenario_file$seed==1 & scenario_file$birthday_age==1, c('admin_name', 'year')]
  if(nrow(check_df) != nrow(distinct(check_df))){
    warning('PROBLEM DETECTED: some admins have multiple distributions in a year')
  } else{
    write.csv(scenario_file, paste0(base_sim_input_dir, '/interventions_projections/llin_epi_', scenario_net_names[ii], '.csv'), row.names = FALSE)
  }
  
  if(print_coverages){
    print(paste0('LLIN EPI dist - ', scenario_net_names[ii],': '))
    scenario_campaign_distinct_project = distinct(scenario_file[scenario_file$year>=first_year_not_pri1 & scenario_file$birthday_age==1,c('admin_name','coverage')])
    cov_all = scenario_campaign_distinct_project$coverage
    print(paste0('....', length(unique(scenario_campaign_distinct_project$admin_name)), ' admins w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; median=', round(median(cov_all),2)))
    # net types
    scenario_campaign_distinct_project = distinct(scenario_file[scenario_file$year>=first_year_not_pri1,c('admin_name', 'llin_type')])
    print(paste0('.... PBO:', length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='PBO'])), 
                 '; IG2: ', length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='IG2'])),
                 '; standard: ', length(unique(scenario_campaign_distinct_project$admin_name)) - length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='PBO'])) - length(unique(scenario_campaign_distinct_project$admin_name[scenario_campaign_distinct_project$llin_type=='IG2']))))
  }
}





###############################################
# add scenarios with higher resistance
###############################################
##### update block/kill to bioassay_mortality=0.5
new_biomortality = 0.5
updated_scenario_filenames = c(paste0(base_sim_input_dir, '/interventions_projections/llin_epi_pri1.csv'),
                               paste0(base_sim_input_dir, '/interventions_projections/llin_epi_pri2.csv'),
                               paste0(base_sim_input_dir, '/interventions_projections/llin_anc_pri1.csv'),
                               paste0(base_sim_input_dir, '/interventions_projections/llin_anc_pri2.csv'),
                               paste0(base_sim_input_dir, '/interventions_projections/llin_comm_pri1.csv'),
                               paste0(base_sim_input_dir, '/interventions_projections/llin_comm_pri2.csv'),
                               paste0(base_sim_input_dir, '/interventions_projections/llin_mass_pri1.csv'),
                               paste0(base_sim_input_dir, '/interventions_projections/llin_mass_pri2.csv'),
                               paste0(base_sim_input_dir, '/interventions_projections/llin_mass_allIG2.csv'),
                               paste0(base_sim_input_dir, '/interventions_projections/llin_mass_allPyr.csv')
                               )
for(ff in 1:length(updated_scenario_filenames)){
  update_campaign = read.csv(updated_scenario_filenames[ff])
  update_campaign = update_campaign[,which(!(colnames(update_campaign) %in% c('block_initial','kill_initial','bio_mortality')))]
  update_campaign$bio_mortality = new_biomortality
  # get the blocking/killing given net type and bio-mortality
  update_campaign = add_net_efficacy_info(llin_campaign_df=update_campaign)
  if(nrow(update_campaign) == nrow(distinct(update_campaign))) {
    write.csv(update_campaign, gsub('.csv', '_50mort.csv', updated_scenario_filenames[ff]), row.names = FALSE)
  } else{
    warning('PROBLEM DETECTED: there are duplicate campaigns in the LLIN input file.')
  }
}







################################################################################
# PMC: use DHS estimate from vaccines to estimate coverage
################################################################################
### Steps
## 1 Load vaccine coverage data from DHS
## 2 Load PMC scenarios and combine data
## 3 Save csv files

# set ages for PMC (in days) - called 'touchpoints' - each must be matched with a vaccine coverage estimate
touchpoints = round(c(2.5, 3.5, 6, 9, 12, 15, 18, 24)*30.4)
corresponding_vaccine_estimates = c(rep('vacc_dpt3', 3), rep('vacc_measles', 5))
# coverage multipliers for doses that don't correspond with existing EPI visits
outside_schedule_multiplier1 = 0.7  # there are vitamin A doses scheduled these months, but not standard vaccines
outside_schedule_multiplier2 = 0.5  # no doses scheduled according to WHO's Burundi EPI schedule
touchpoint_multipliers = c(1, 1, outside_schedule_multiplier1, 1, outside_schedule_multiplier1, outside_schedule_multiplier2, 1, outside_schedule_multiplier1)
touchpoints_df = data.frame('pmc_touchpoints' = touchpoints, 'vaccine' = corresponding_vaccine_estimates, 'touchpoint_coverage_multipliers'=touchpoint_multipliers)

#  Load vaccine coverage data from DHS
vacc_coverage = read.csv(file.path(hbhi_dir, 'estimates_from_DHS', 'DHS_vaccine_admin_minN30_2016.csv'))
vacc_coverage$admin_name = vacc_coverage$NOMDEP
var_names = c('vacc_dpt3', 'vacc_measles')
# ages (in months) when each DHS vaccine is received (to allow for correcting for surveyed children who are not yet that age)
vacc_ages = c(3, 9)
max_child_age_surveyed = 5*12  # in months
maximum_coverage = 0.9
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
pmc_coverage$coverage = pmc_coverage$coverage * pmc_coverage$touchpoint_coverage_multipliers
pmc_coverage = pmc_coverage[,which(!colnames(pmc_coverage) %in% c('touchpoint_coverage_multipliers'))]

# determine which admins receive PMC in future projections
# NSP
pmc_nsp = gf_scenarios[,c('admin_name', 'cp_nsp')]
unique(pmc_nsp$cp_nsp)
pmc_nsp$receive_pmc = sapply(pmc_nsp$cp_nsp, function(x){x==1})
# fund
pmc_pri = gf_scenarios[,c('admin_name', 'cp_pri')]
unique(pmc_pri$cp_pri)
pmc_pri$receive_pmc = sapply(pmc_pri$cp_pri, function(x){x==1})
# create list of net scenarios with corresponding vector describing the name of each (allowing for looping later)
scenario_pmc_names = c('nsp', 'pri')
scenario_pmc_status_list = list(pmc_nsp, pmc_pri)


# iterate through scenarios, preparing input files (set coverage to 0 in non-IPTi admins)
for(ii in 1:length(scenario_pmc_status_list)){
  pmc_coverage_cur = pmc_coverage
  pmc_admins = unique(scenario_pmc_status_list[[ii]][['admin_name']][scenario_pmc_status_list[[ii]][['receive_pmc']]])
  # remove all non-pmc admins to zero
  pmc_coverage_cur = pmc_coverage_cur[which(pmc_coverage_cur$admin_name %in% pmc_admins),]
  
  # set day of simulation when PMC begins - need to adjust the start day to take into account the delay due to it being a birth-triggered intervention (only children born after the startday will eventually receive the intervention)
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
    print(paste0('....', scenario_pmc_names[ii],': ', length(unique(pmc_coverage_cur$admin_name)), ' admins w/ coverage ', round(min(cov_all),2), '-', round(max(cov_all),2), '; mean=', round(mean(cov_all),2), '; median=', round(median(cov_all),2)))
  }
}







################################################################################
# RTSS: use DHS estimate from vaccines to estimate coverage
################################################################################
### Steps
## 1 Load vaccine coverage data from DHS
## 2 Load PMC scenarios and combine data
## 3 Save csv files

# set touchpoints
primary_series_day = round(9*30.4)  # initial series finishes
booster_day = round(18*30.4)  # booster

# set day of simulation when vaccinations will be rolled out
#    need to adjust the start day to take into account the delay due to it being a birth-triggered intervention (only children born after the startday will eventually receive the intervention)
#    --> shift the simulation start day earlier so that children who reach the first touchpoint by the intervention start day will receive the intervention
vacc_rollout_day =  max(1, round(365 * (2024.5 - projection_start_year)) + 1  - primary_series_day) # start day in simulation

#  Load vaccine coverage data from DHS
vacc_coverage = read.csv(file.path(hbhi_dir, 'estimates_from_DHS', 'DHS_vaccine_admin_minN30_2016.csv'))
vacc_coverage$admin_name = vacc_coverage$NOMDEP
var_names = c( 'vacc_dpt3', 'vacc_measles')  # 'vacc_dpt1', 'vacc_dpt2',
# ages (in months) when each vaccine is received (to allow for correcting for surveyed children who are not yet that age)
vacc_ages = c(3, 9)  # 1, 2, 
max_child_age_surveyed = 5*12  # in months
maximum_coverage = 0.9
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


# determine which admins receive vaccine in future projections
# NSP
vacc_nsp = gf_scenarios[,c('admin_name', 'vac_nsp')]
unique(vacc_nsp$vac_nsp)
vacc_nsp$receive_vacc = sapply(vacc_nsp$vac_nsp, function(x){x==1})
# Prioritized plan
vacc_pri = gf_scenarios[,c('admin_name', 'vac_pri')]
unique(vacc_pri$vac_pri)
vacc_pri$receive_vacc = sapply(vacc_pri$vac_pri, function(x){x==1})
# create list of net scenarios with corresponding vector describing the name of each (allowing for looping later)
scenario_vacc_names = c('nsp', 'pri')
scenario_vacc_status_list = list(vacc_nsp, vacc_pri)


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







################################################################################
#  update LLIN types and coverages to GF plan
################################################################################
admins_with_PBO = gf_scenarios$admin_name[gf_scenarios$llins_mass22_pri %in% c('MILDA-IG2', 'MILDA-PBO')]
admins_with_pyr = gf_scenarios$admin_name[gf_scenarios$llins_mass22_pri %in% c('Pyrethroids')]
admins_without_LLINs = gf_scenarios$admin_name[gf_scenarios$llins_mass22_pri %in% c('Sans campagne MILDA')]

# check whether all admins are appropriately assigned a net type
ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
admin_names = read.csv(ds_pop_df_filename)
admin_names = admin_names$admin_name
if(!all(admin_names %in% c(admins_with_PBO, admins_with_pyr, admins_without_LLINs))) warning('some admin names not matching')
if(length(admin_names) != length(c(admins_with_PBO, admins_with_pyr, admins_without_LLINs))) warning('some admin names not included')
if(!all(sort(admin_names) == sort(c(admins_with_PBO, admins_with_pyr, admins_without_LLINs)))) warning('some admin names do not match')
# check that there are no admins that appear in both pyr and PBO
if(any(admins_with_PBO %in% admins_with_pyr)) warning('some admins were included twice: once with PBO and once with pyr')

# use BAU coverage with the GF net type
# read in all-pyr and all-PBO files, subsetting to the correct set of admins for each, and save
net_base_filepath = paste0(base_sim_input_dir, '/interventions_projections/')
filenames_pyr = c('llin_mass_bau.csv', 'llin_mass_04pyr.csv',
                  'llin_anc_bau.csv', 'llin_anc_04pyr.csv',
                  'llin_epi_bau.csv', 'llin_epi_04pyr.csv')
filenames_PBO = c('llin_mass_PBO.csv', 'llin_mass_04PBO.csv',
                  'llin_anc_PBO.csv', 'llin_anc_04PBO.csv',
                  'llin_epi_PBO.csv', 'llin_epi_04PBO.csv')
filenames_GF = c('llin_mass_gf_64pyr.csv', 'llin_mass_gf_40pyr.csv',
                 'llin_anc_gf_64pyr.csv', 'llin_anc_gf_40pyr.csv',
                 'llin_epi_gf_64pyr.csv', 'llin_epi_gf_40pyr.csv')
filenames_GF_PBO = c('llin_mass_gf_PBO64pyr.csv', 'llin_mass_gf_PBO40pyr.csv')
filenames_GF_80cov = c('llin_mass_gf_64pyr_80cov.csv', 'llin_mass_gf_40pyr_80cov.csv',
                 'llin_anc_gf_64pyr_80cov.csv', 'llin_anc_gf_40pyr_80cov.csv',
                 'llin_epi_gf_64pyr_80cov.csv', 'llin_epi_gf_40pyr_80cov.csv')
filenames_GF_PBO_80cov = c('llin_mass_gf_PBO64pyr_80cov.csv', 'llin_mass_gf_PBO40pyr_80cov.csv',
                       'llin_anc_gf_PBO64pyr_80cov.csv', 'llin_anc_gf_PBO40pyr_80cov.csv',
                       'llin_epi_PBO.csv', 'llin_epi_04PBO.csv')


### GF net types - mass LLIN distributions (some districts do not receive any nets)
for(ff in grep('mass', filenames_GF)){
  pyr_values = read.csv(paste0(net_base_filepath, filenames_pyr[ff]))
  pbo_values = read.csv(paste0(net_base_filepath, filenames_PBO[ff]))
  
  # subset to admins in each group
  pyr_values = pyr_values[pyr_values$admin_name %in% admins_with_pyr,]
  pyr_values$net_type_PBO = 0
  pbo_values = pbo_values[pbo_values$admin_name %in% admins_with_PBO,]
  pbo_values$net_type_PBO = 1
  
  # combine
  llin_all = merge(pyr_values, pbo_values, by=intersect(colnames(pyr_values), colnames(pbo_values)), all.x=TRUE, all.y=TRUE)
  # reorder by admin name
  llin_all_ordered = llin_all[order(llin_all$admin_name),]
  write.csv(llin_all_ordered, paste0(net_base_filepath, filenames_GF[ff]), row.names = FALSE)
}


### GF mass LLIN distributions with all PBO nets (some districts do not receive any nets)
for(ff in grep('mass', filenames_GF_PBO)){
  pbo_values = read.csv(paste0(net_base_filepath, filenames_PBO[ff]))
  
  # subset to admins in each group
  pbo_values = pbo_values[pbo_values$admin_name %in% c(admins_with_pyr, admins_with_PBO),]
  pbo_values$net_type_PBO = 1
  
  # combine
  llin_all = pbo_values
  # reorder by admin name
  llin_all_ordered = llin_all[order(llin_all$admin_name),]
  write.csv(llin_all_ordered, paste0(net_base_filepath, filenames_GF_PBO[ff]), row.names = FALSE)
}

### GF net types - epi and anc LLIN distributions (districts without mass campaigns still receive pyrethroid ANC and EPI nets)
for(ff in c(grep('anc', filenames_GF), grep('epi', filenames_GF))){
  pyr_values = read.csv(paste0(net_base_filepath, filenames_pyr[ff]))
  pbo_values = read.csv(paste0(net_base_filepath, filenames_PBO[ff]))

  # subset to admins in each group
  pyr_values = pyr_values[pyr_values$admin_name %in% c(admins_with_pyr, admins_without_LLINs),]
  pyr_values$net_type_PBO = 0
  pbo_values = pbo_values[pbo_values$admin_name %in% admins_with_PBO,]
  pbo_values$net_type_PBO = 1
  
  # combine
  llin_all = merge(pyr_values, pbo_values, by=intersect(colnames(pyr_values), colnames(pbo_values)), all.x=TRUE, all.y=TRUE)
  # reorder by admin name
  llin_all_ordered = llin_all[order(llin_all$admin_name),]
  write.csv(llin_all_ordered, paste0(net_base_filepath, filenames_GF[ff]), row.names = FALSE)
}


### GF 80% coverage - mass LLIN distributions
# replace effective coverage with 80% for U5, with lower rates in other age groups determined by rate relative to U5: 

# get country-average relative ITN use rates for different age groups
relative_use_rates = read.csv(paste0(hbhi_dir,'/estimates_from_DHS/DHS_weighted_ITN_use_by_age_relative_to_u5.csv'))
relative_use_rates = relative_use_rates[1,grepl('use_relative_to', colnames(relative_use_rates))]
age_group_names=gsub('X','',colnames(relative_use_rates))
age_group_names=gsub('_use_relative_to_u5','',age_group_names)
relative_use_by_age = as.numeric(relative_use_rates)
use_by_age_80cov = relative_use_by_age * 0.8

# use 80% coverage with the GF net type
for(ff in grep('mass', filenames_GF)){
  gf_values_80cov = read.csv(paste0(net_base_filepath, filenames_GF[ff]))

  # replace effective coverage if it is lower than the new effective coverage
  for(aa in 1:length(age_group_names)){
    gf_values_80cov[[paste0('itn_',age_group_names[aa])]] = sapply(gf_values_80cov[[paste0('itn_',age_group_names[aa])]], max, use_by_age_80cov[aa])
  }
  write.csv(gf_values_80cov, paste0(net_base_filepath, filenames_GF_80cov[ff]), row.names = FALSE)
}
# use 80% coverage with PBO net type in all mass-distribution districts
for(ff in grep('mass', filenames_GF_PBO)){
  gf_values_80covPBO = read.csv(paste0(net_base_filepath, filenames_GF_PBO[ff]))
  
  # replace effective coverage if it is lower than the new effective coverage
  for(aa in 1:length(age_group_names)){
    gf_values_80covPBO[[paste0('itn_',age_group_names[aa])]] = sapply(gf_values_80covPBO[[paste0('itn_',age_group_names[aa])]], max, use_by_age_80cov[aa])
  }
  write.csv(gf_values_80covPBO, paste0(net_base_filepath, filenames_GF_PBO_80cov[ff]), row.names = FALSE)
}

### GF 80% coverage - anc LLIN distributions
for(ff in grep('anc', filenames_GF)){
  gf_values_80cov = read.csv(paste0(net_base_filepath, filenames_GF[ff]))
  
  # replace effective coverage if it is lower than the new effective coverage
  gf_values_80cov$coverage = sapply(gf_values_80cov$coverage, max, 0.8)

  write.csv(gf_values_80cov, paste0(net_base_filepath, filenames_GF_80cov[ff]), row.names = FALSE)
}
# use 80% coverage with PBO LLINs everywhere
for(ff in grep('anc', filenames_PBO)){
  gf_values_80covPBO = read.csv(paste0(net_base_filepath, filenames_PBO[ff]))
  
  # replace effective coverage if it is lower than the new effective coverage
  gf_values_80covPBO$coverage = sapply(gf_values_80covPBO$coverage, max, 0.8)
  
  write.csv(gf_values_80covPBO, paste0(net_base_filepath, filenames_GF_PBO_80cov[ff]), row.names = FALSE)
}

### GF 80% coverage - epi LLIN distributions - already above 80% coverage in EPI visit, so can use the same values as GF




### GF plan for CHW-mediated ITN distribution. assume pyrethroid nets
net_base_filepath = paste0(base_sim_input_dir, '/interventions_projections/')
CHW_ITN_admins = gf_scenarios$admin_name[gf_scenarios$llins_comm_pri == 1]
lower_monthly_coverage = 0.095
higher_monthly_coverage = 0.175
highest_monthly_coverage = 1
# get country-average relative ITN use rates for different age groups
relative_use_rates = read.csv(paste0(hbhi_dir,'/estimates_from_DHS/DHS_weighted_ITN_use_by_age_relative_to_u5.csv'))
relative_use_rates = relative_use_rates[1,grepl('use_relative_to', colnames(relative_use_rates))]
age_group_names=gsub('X','',colnames(relative_use_rates))
age_group_names=gsub('_use_relative_to_u5','',age_group_names)
relative_use_by_age = as.numeric(relative_use_rates)


# generate files for pyrethroid and PBO nets; for permethrin mortality=0.64 and 0.40; and for lower and higher montly coverages
mass_dist_filenames = c('llin_mass_bau.csv','llin_mass_04pyr.csv', 'llin_mass_PBO.csv', 'llin_mass_04PBO.csv')
filename_suffixes = c('_64perm', '_40perm', '_PBO64perm', '_PBO40perm')
for(ff in 1:length(mass_dist_filenames)){
  # use same permethrin mortality, kill/block rates, net retention times, etc. as each seed from mass net distribution file
  mass_dist_df = read.csv(paste0(net_base_filepath, mass_dist_filenames[ff]))
  mass_dist_df = mass_dist_df[,-which(colnames(mass_dist_df) =='nets_per_capita')]
  # subset to first year and included admins
  chw_1yr_df = mass_dist_df[mass_dist_df$year == min(mass_dist_df$year),]
  chw_1yr_df$year = projection_start_year
  chw_1yr_df$simday = 1
  chw_1yr_df = chw_1yr_df[chw_1yr_df$admin_name %in% CHW_ITN_admins,]
  # repeat for all years in projection
  chw_df = chw_1yr_df
  for(yy in 1:length((projection_start_year+1):projection_end_year)){
    chw_1yr_df$year = ((projection_start_year+1):projection_end_year)[yy]
    chw_1yr_df$simday = 365*yy
    chw_df = rbind(chw_df, chw_1yr_df)
  }
  # replace coverage with monthly checking coverage from CHWs - lower estimate
  use_by_age = lower_monthly_coverage * relative_use_by_age
  for(aa in 1:length(age_group_names)){
    chw_df[[paste0('itn_',age_group_names[aa])]] = use_by_age[aa]
  }
  write.csv(chw_df, paste0(base_sim_input_dir, '/interventions_projections/llin_monthly_chw_lower', filename_suffixes[ff], '.csv'), row.names = FALSE)
  # replace coverage with monthly checking coverage from CHWs - higher estimate
  use_by_age = higher_monthly_coverage * relative_use_by_age
  for(aa in 1:length(age_group_names)){
    chw_df[[paste0('itn_',age_group_names[aa])]] = use_by_age[aa]
  }
  write.csv(chw_df, paste0(base_sim_input_dir, '/interventions_projections/llin_monthly_chw_higher', filename_suffixes[ff], '.csv'), row.names = FALSE)
  # replace coverage with monthly checking coverage from CHWs - highest estimate
  use_by_age = highest_monthly_coverage * relative_use_by_age
  for(aa in 1:length(age_group_names)){
    chw_df[[paste0('itn_',age_group_names[aa])]] = use_by_age[aa]
  }
  write.csv(chw_df, paste0(base_sim_input_dir, '/interventions_projections/llin_monthly_chw_highest', filename_suffixes[ff], '.csv'), row.names = FALSE)
}







################################################################################
#  update all LLIN projection inputs to assume a 2-year net retention time
################################################################################

longer_logmu=6.594

old_itn_filename_bases = c("llin_anc_04PBO.csv", "llin_anc_04pyr.csv" , 
                      "llin_anc_bau.csv",  "llin_anc_gf_40pyr.csv",  "llin_anc_gf_40pyr_80cov.csv" , 
                      "llin_anc_gf_64pyr.csv",  "llin_anc_gf_64pyr_80cov.csv",  "llin_anc_gf_PBO40pyr_80cov.csv", 
                      "llin_anc_gf_PBO64pyr_80cov.csv", "llin_anc_PBO.csv" , "llin_annual_chw_40perm.csv", 
                      "llin_annual_chw_64perm.csv",  "llin_annual_chw_PBO40perm.csv",  "llin_annual_chw_PBO64perm.csv", 
                      "llin_epi_04PBO.csv", "llin_epi_04pyr.csv" ,  "llin_epi_bau.csv" , 
                      "llin_epi_gf_40pyr.csv", "llin_epi_gf_64pyr.csv", "llin_epi_PBO.csv", 
                      "llin_mass_04PBO.csv", "llin_mass_04pyr.csv",  "llin_mass_bau.csv" , 
                      "llin_mass_gf_40pyr.csv", "llin_mass_gf_40pyr_80cov.csv", "llin_mass_gf_64pyr.csv", 
                      "llin_mass_gf_64pyr_80cov.csv",  "llin_mass_gf_PBO40pyr.csv",  "llin_mass_gf_PBO40pyr_80cov.csv", 
                      "llin_mass_gf_PBO64pyr.csv", "llin_mass_gf_PBO64pyr_80cov.csv", "llin_mass_PBO.csv")

old_itn_filenames = paste0(base_sim_input_dir, '/interventions_projections/', old_itn_filename_bases)

for(ff in 1:length(old_itn_filenames)){
  cur_file = read.csv(old_itn_filenames[ff])
  cur_file$net_life_lognormal_mu = longer_logmu
  write.csv(cur_file, gsub('.csv', '_2yLLIN.csv', old_itn_filenames[ff]))
}




# add to-present LLIN input files with the longer LLIN halflife
longer_logmu=6.594

old_itn_filename_bases = c('anc_itn_use_coverages_mort_2010_2020.csv', 'epi_itn_use_coverages_mort_2010_2020.csv', 'itn_mass_coverages_mort_2010_2020.csv')
old_itn_filenames = paste0(base_sim_input_dir, '/interventions_2010_2020/', old_itn_filename_bases)
for(ff in 1:length(old_itn_filenames)){
  cur_file = read.csv(old_itn_filenames[ff])
  cur_file$net_life_lognormal_mu = longer_logmu
  write.csv(cur_file, gsub('.csv', '_2yLLIN.csv', old_itn_filenames[ff]))
}






