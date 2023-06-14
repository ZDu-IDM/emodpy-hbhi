# future_projection_intervention_inputs_GF_PAAR.R

# these inputs are specific to a particular set of Nigeria scenarios (the March 31, 2023 request from the GF on Within Allocation versus PAAR scenarios)
# for many interventions, the WA scenario has interventions discontinued after 2024 or 2025. For SMC, it is sometimes subset to a few states. 


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

print_coverages = TRUE


################################################################################
# admin subset: only GF LGAs
################################################################################
fund_gf = gf_scenarios[,c('admin_name', 'funder', 'State')]
fund_gf = fund_gf[fund_gf$funder=='Global Fund',]
length(unique(fund_gf$State))
gf_admins = unique(fund_gf$admin_name)
write.csv(fund_gf, paste0(base_sim_input_dir, '/interventions_projections/admin_subset_GF.csv'), row.names = FALSE)


################################################################################
# CM: continue coverage from final year into future projections
################################################################################
# WA version where CM stops at the beginning of 2026
final_cm_year = 2025

#  effective coverage in U5 increased to coverage_increase_target; other coverages adjusted accordingly
cm_paar = read.csv(paste0(base_sim_input_dir, '/interventions_projections/cm_bau.csv'))
# subset to GF admins
cm_paar = cm_paar[cm_paar$admin_name %in% gf_admins,]
if(print_coverages){
  print('Case management')
  cov_df = data.frame(admin_name=cm_paar$admin_name, coverage=cm_paar$U5_coverage)
  print(paste0('.... Num LGAs: ', length(unique(cov_df$admin_name)),  '; median coverage=', round(median(cov_df$coverage),2)))
}
# set duration of CM coverage
cm_paar$duration = (final_cm_year - projection_start_year + 1)*365
if(nrow(cm_paar) == nrow(distinct(cm_paar))){
  write.csv(cm_paar, paste0(base_sim_input_dir, '/interventions_projections/cm_bau_PAAR_sc1.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the CM input file.')
}

# check final date receiving SMC
as.Date('2022/01/01') + cm_paar$duration[1]


################################################################################
# SMC: continue coverage from final year into future projections
################################################################################
# for PAAR sc1, continue SMC coverage in 2024 and 2025, and remove SMC in 2026
smc_paar = read.csv(paste0(base_sim_input_dir, '/interventions_projections/smc_fund.csv'))
# subset to GF admins
smc_paar = smc_paar[smc_paar$admin_name %in% gf_admins,]
if(print_coverages){
  print('SMC')
  cov_df = data.frame(admin_name=smc_paar$admin_name, coverage=smc_paar$coverage_high_access_U5*smc_paar$high_access_U5 +  smc_paar$coverage_low_access_U5*(1-smc_paar$high_access_U5), year=smc_paar$year)
  cov_df = cov_df[cov_df$year>2023,]
  print(paste0('.... Num LGAs: ', length(unique(cov_df$admin_name)),  '; median coverage=', round(median(cov_df$coverage),2)))
}
# remove all of 2026
smc_paar = smc_paar[smc_paar$year <=2025,]
if(nrow(smc_paar) == nrow(distinct(smc_paar))) {
  write.csv(smc_paar, paste0(base_sim_input_dir, '/interventions_projections/smc_fund_PAAR_sc1.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the SMC input file.')
}



################################################################################
#  LLIN mass distribution
################################################################################# 
frac_reassign_feed_survive=0.9

### All scenarios should have Katsina state's 2024 distribution delayed to 2025
llin_paar_sc0 = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_mass_fund.csv'))
llin_paar_sc0$date = as.Date(llin_paar_sc0$date)
katsina_admins = gf_scenarios$admin_name[gf_scenarios$State == 'Katsina']
llin_paar_sc0$simday[llin_paar_sc0$admin_name %in% katsina_admins & llin_paar_sc0$year == 2024] = llin_paar_sc0$simday[llin_paar_sc0$admin_name %in% katsina_admins & llin_paar_sc0$year == 2024] + 365
llin_paar_sc0$date[llin_paar_sc0$admin_name %in% katsina_admins & llin_paar_sc0$year == 2024] = llin_paar_sc0$date[llin_paar_sc0$admin_name %in% katsina_admins & llin_paar_sc0$year == 2024] + 365
llin_paar_sc0$year[llin_paar_sc0$admin_name %in% katsina_admins & llin_paar_sc0$year == 2024] = 2025

# subset to GF admins
llin_paar_sc0 = llin_paar_sc0[llin_paar_sc0$admin_name %in% gf_admins,]
if(print_coverages){
  print('LLIN - mass')
  cov_df = data.frame(admin_name=llin_paar_sc0$admin_name, coverage=llin_paar_sc0$itn_u5, year=llin_paar_sc0$year, llin_type=llin_paar_sc0$llin_type)
  cov_df = cov_df[cov_df$year>2023,]
  print(paste0('.... Num LGAs: ', length(unique(cov_df$admin_name)),  '; median coverage=', round(median(cov_df$coverage),2)))
  # net types
  scenario_campaign_distinct_LGA = distinct(cov_df[,c('admin_name', 'llin_type')])
  print(paste0('....... PBO:', length(unique(scenario_campaign_distinct_LGA$admin_name[scenario_campaign_distinct_LGA$llin_type=='PBO'])), 
               '; IG2: ', length(unique(scenario_campaign_distinct_LGA$admin_name[scenario_campaign_distinct_LGA$llin_type=='IG2'])),
               '; standard: ', length(unique(scenario_campaign_distinct_LGA$admin_name)) - length(unique(scenario_campaign_distinct_LGA$admin_name[scenario_campaign_distinct_LGA$llin_type=='PBO'])) - length(unique(scenario_campaign_distinct_LGA$admin_name[scenario_campaign_distinct_LGA$llin_type=='IG2']))))
}

if(nrow(llin_paar_sc0) == nrow(distinct(llin_paar_sc0))) {
  write.csv(llin_paar_sc0, paste0(base_sim_input_dir, '/interventions_projections/llin_mass_fund_PAAR.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the LLIN input file.')
}

### scenario 1: funded/prioritized in 2024, only five states in 2025, and none in 2026 - for now, select Kano plus smallest states
# # population sizes for 2025 states
# archetype_info_state = archetype_info %>% dplyr::select('State', 'pop_size') %>% group_by(State) %>% summarise_all(mean)
# archetype_info_state[archetype_info_state$State %in% c('Katsina', 'Kano', 'Taraba', 'Delta', 'Kaduna', 'Niger', 'Yobe'),]
llin_states_2025_paar = c('Kano', 'Delta', 'Yobe', 'Niger', 'Katsina')
llin_admins_2025_paar = gf_scenarios$admin_name[gf_scenarios$State %in% llin_states_2025_paar]

final_llin_year = 2025
llin_paar_sc1 = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_mass_fund_PAAR.csv'))
# remove campaigns after final_llin_year
llin_paar_sc1 = llin_paar_sc1[llin_paar_sc1$simday <= (final_llin_year - projection_start_year + 1)*365,]
# remove rows from 2025 for all states not in llin_admins_2025_paar
llin_paar_sc1 = llin_paar_sc1[-intersect(which(llin_paar_sc1$year == 2025), which(!(llin_paar_sc1$admin_name %in% llin_admins_2025_paar))),]

if(nrow(llin_paar_sc1) == nrow(distinct(llin_paar_sc1))) {
  write.csv(llin_paar_sc1, paste0(base_sim_input_dir, '/interventions_projections/llin_mass_fund_PAAR_sc1.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the LLIN input file.')
}


### scenarios 4-5: proioritized plan for all years, but using IG2 nets everywhere
llin_paar_ig2 = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_mass_fund_PAAR.csv'))
llin_paar_ig2$llin_type[llin_paar_ig2$year >= 2024] = 'IG2'
llin_paar_ig2 = add_kill_block_to_df(itn_df=llin_paar_ig2, frac_reassign_feed_survive=frac_reassign_feed_survive)
if(nrow(llin_paar_ig2) == nrow(distinct(llin_paar_ig2))) {
  write.csv(llin_paar_ig2, paste0(base_sim_input_dir, '/interventions_projections/llin_mass_fund_PAAR_allIG2.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the LLIN input file.')
}


### scenario 3_pyr: prioritized plan for all years, but using pyrethroid-only nets everywhere
llin_paar_pyr = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_mass_fund_PAAR.csv'))
llin_paar_pyr$llin_type[llin_paar_pyr$year >= 2024] = 'standard'
llin_paar_pyr = add_kill_block_to_df(itn_df=llin_paar_pyr, frac_reassign_feed_survive=frac_reassign_feed_survive)
if(nrow(llin_paar_pyr) == nrow(distinct(llin_paar_pyr))) {
  write.csv(llin_paar_pyr, paste0(base_sim_input_dir, '/interventions_projections/llin_mass_fund_PAAR_pyr.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the LLIN input file.')
}



### scenario 1_pyr: scenario 1 (within allocation), but using pyrethroid-only nets everywhere
llin_paar_sc1_pyr = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_mass_fund_PAAR_sc1.csv'))
llin_paar_sc1_pyr$llin_type[llin_paar_sc1_pyr$year >= 2024] = 'standard'
llin_paar_sc1_pyr = add_kill_block_to_df(itn_df=llin_paar_sc1_pyr, frac_reassign_feed_survive=frac_reassign_feed_survive)
if(nrow(llin_paar_sc1_pyr) == nrow(distinct(llin_paar_sc1_pyr))) {
  write.csv(llin_paar_sc1_pyr, paste0(base_sim_input_dir, '/interventions_projections/llin_mass_fund_PAAR_sc1_pyr.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the LLIN input file.')
}


# check that the appropriate states are getting campaigns each year; compare against the version where not all GF states get IG2
# View(distinct(llin_paar_ig2[which(llin_paar_ig2$admin_name %in%  gf_scenarios$admin_name[gf_scenarios$funder=='Global Fund']) ,c('State', 'year')]))
ll4 = llin_paar_ig2 %>% filter(year>=2024) %>% dplyr::select(-c('llin_type')) %>% rename('block_ig2'='block_initial', 'kill_ig2'='kill_initial')
ll0 = llin_paar_sc0 %>% filter(year>=2024) %>% rename('llin_type_0'='llin_type', 'block_0'='block_initial', 'kill_0'='kill_initial')
llpyr = llin_paar_pyr %>% filter(year>=2024) %>% dplyr::select(-c('llin_type')) %>% rename('block_pyr'='block_initial', 'kill_pyr'='kill_initial')

llin_compare = merge(ll0, ll4, all=TRUE)
llin_compare = merge(llin_compare, llpyr, all=TRUE)
llin_compare = llin_compare[intersect(which(llin_compare$year>2023), which(llin_compare$admin_name %in%  gf_scenarios$admin_name[gf_scenarios$funder=='Global Fund'])),]
# extract only one distribution for each LGA (i.e., if distributions occur in 2024, 2027, 2030, only inlcude one)
llin_compare = llin_compare %>% dplyr::select(-c('year', 'month', 'date', 'simday', 'itn_u5','itn_5_10', 'itn_10_15','itn_15_20', 'itn_o20','net_life_lognormal_mu','net_life_lognormal_sigma','indoor_net_protection')) 
llin_compare = distinct(llin_compare)
library(reshape2)
library(ggplot2)
library(viridis)
llin_compare_block = llin_compare %>% dplyr::select(c('admin_name', 'State', 'coverage', 'llin_type_0', 'bio_mortality', 'block_0', 'block_ig2', 'block_pyr'))
llin_compare_block = melt(llin_compare_block, id.vars=c('admin_name', 'State', 'coverage', 'llin_type_0', 'bio_mortality'), variable.name='scenario', value.name='block_initial')
gg_block = ggplot(data=llin_compare_block, aes(x=block_initial, group=scenario, fill=scenario)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic()
gg_block2 =  ggplot(data=llin_compare_block, aes(x=scenario, y=block_initial, fill=scenario)) +
  geom_violin(adjust=1.5, alpha=.4) +
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_classic()

llin_compare_kill = llin_compare %>% dplyr::select(c('admin_name', 'State', 'coverage', 'llin_type_0', 'bio_mortality', 'kill_0', 'kill_ig2', 'kill_pyr'))
llin_compare_kill = melt(llin_compare_kill, id.vars=c('admin_name', 'State', 'coverage', 'llin_type_0', 'bio_mortality'), variable.name='scenario', value.name='kill_initial')
gg_kill = ggplot(data=llin_compare_kill, aes(x=kill_initial, group=scenario, fill=scenario)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic()

gg_kill2 = ggplot(data=llin_compare_kill, aes(x=scenario, y=kill_initial, fill=scenario)) +
  geom_violin(adjust=1.5, alpha=.4) +
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_classic()

ggplot(data=llin_compare_block, aes(x=bio_mortality, group=scenario, fill=scenario)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic()

par(mfrow=c(1,2))
plot( llin_compare$block_0, llin_compare$block_ig2, xlab='original blocking', ylab='IG2 blocking', col=as.factor(llin_compare$llin_type_0), type='p', bty='L', pch=20)
abline(a=0,b=1)
plot( llin_compare$kill_0, llin_compare$kill_ig2, xlab='original killing', ylab='IG2 killing', col=as.factor(llin_compare$llin_type_0), type='p', bty='L', pch=20)
abline(a=0,b=1)
par(mfrow=c(1,2))

par(mfrow=c(1,2))
plot( llin_compare$block_pyr, llin_compare$block_ig2, xlab='pyr blocking', ylab='IG2 blocking', col=as.factor(llin_compare$llin_type_0), type='p', bty='L', pch=20)
abline(a=0,b=1)
plot( llin_compare$kill_pyr, llin_compare$kill_ig2, xlab='pyr killing', ylab='IG2 killing', col=as.factor(llin_compare$llin_type_0), type='p', bty='L', pch=20)
abline(a=0,b=1)
par(mfrow=c(1,2))




if(FALSE){
  ll4 = llin_paar_ig2 %>% filter(year>=2024) %>% dplyr::select(c('admin_name', 'llin_type')) %>% rename('type_ig2'='llin_type')
  ll0 = llin_paar_sc0 %>% filter(year>=2024) %>% dplyr::select(c('admin_name', 'llin_type')) %>% rename('type_original'='llin_type')
  ll4 = distinct(ll4)
  ll0 = distinct(ll0)
  ll_types = merge(ll0,ll4, all=TRUE)
  ll_types$different = ll_types$type_ig2 != ll_types$type_original
  ll_different = ll_types[ll_types$different,]
  
  write.csv(ll_different, paste0(base_sim_input_dir, '/interventions_projections/admins_different_IG2_PBO_PAAR_scenarios.csv'), row.names = FALSE)
}




################################################################################
#  LLIN ANC distribution
################################################################################
lower_coverage_year = 2024

### scenario 1: reduce coverage by 70% in all areas in 2024 and remove after 2024
anc_llin_paar_sc1 = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_anc_bau.csv'))
if(nrow(anc_llin_paar_sc1) != length(archetype_info$admin_name)){
  warning('some LGAs are missing from the LLIN ANC dataframe')
}
# subset to GF admins
anc_llin_paar_sc1 = anc_llin_paar_sc1[anc_llin_paar_sc1$admin_name %in% gf_admins,]
if(print_coverages){
  print('LLIN - routine')
  cov_df = data.frame(admin_name=anc_llin_paar_sc1$admin_name, coverage=anc_llin_paar_sc1$coverage)
  print(paste0('.... Num LGAs: ', length(unique(cov_df$admin_name)),  '; median coverage=', round(median(cov_df$coverage),2)))
}
# current coverage lasts until beginning of 2024
anc_llin_paar_sc1$duration = (lower_coverage_year - projection_start_year + 1)*365
anc_lower_year = anc_llin_paar_sc1
anc_lower_year$coverage = anc_llin_paar_sc1$coverage * 0.7
anc_lower_year$year = lower_coverage_year
anc_lower_year$simday = (lower_coverage_year - projection_start_year + 1)*365
anc_lower_year$duration = 365
anc_llin_paar_sc1 = rbind(anc_llin_paar_sc1, anc_lower_year)

if(nrow(anc_llin_paar_sc1) == nrow(distinct(anc_llin_paar_sc1))) {
  write.csv(anc_llin_paar_sc1, paste0(base_sim_input_dir, '/interventions_projections/llin_anc_bau_PAAR_sc1.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the ANC LLIN input file.')
}


### scenarios 2-3: reduce coverage by 70% in all areas in all years 2024 and after
anc_llin_paar_sc2 = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_anc_bau.csv'))
# current coverage lasts until beginning of 2024
anc_llin_paar_sc2$duration = (lower_coverage_year - projection_start_year + 1)*365
anc_lower_year = anc_llin_paar_sc2
anc_lower_year$coverage = anc_llin_paar_sc2$coverage * 0.7
anc_lower_year$year = lower_coverage_year
anc_lower_year$simday = (lower_coverage_year - projection_start_year + 1)*365
anc_lower_year$duration = -1
anc_llin_paar_sc2 = rbind(anc_llin_paar_sc2, anc_lower_year)

if(nrow(anc_llin_paar_sc2) == nrow(distinct(anc_llin_paar_sc2))) {
  write.csv(anc_llin_paar_sc2, paste0(base_sim_input_dir, '/interventions_projections/llin_anc_bau_PAAR_sc2.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the ANC LLIN input file.')
}


### scenarios 4-5: reduce coverage by 70% in all areas in all years 2024 and after; switch to IG2 starting in 2024
anc_llin_paar_sc4 = read.csv(paste0(base_sim_input_dir, '/interventions_projections/llin_anc_bau_PAAR_sc2.csv'))
# switch to IG2 in 2024
anc_llin_paar_sc4$llin_type='standard'
anc_llin_paar_sc4$llin_type[anc_llin_paar_sc4$simday >= (lower_coverage_year - projection_start_year + 1)*365] = 'IG2'
anc_llin_paar_sc4 = add_kill_block_to_df(itn_df=anc_llin_paar_sc4, frac_reassign_feed_survive=frac_reassign_feed_survive)

if(nrow(anc_llin_paar_sc4) == nrow(distinct(anc_llin_paar_sc4))) {
  write.csv(anc_llin_paar_sc4, paste0(base_sim_input_dir, '/interventions_projections/llin_anc_bau_PAAR_sc4.csv'), row.names = FALSE)
} else{
  warning('PROBLEM DETECTED: there are duplicate campaigns in the ANC LLIN input file.')
}







################################################################################
# PMC
################################################################################
pmc_gf = read.csv(paste0(base_sim_input_dir, '/interventions_projections/pmc_fund.csv'))
# subset to GF admins
pmc_gf = pmc_gf[pmc_gf$admin_name %in% gf_admins,]
if(print_coverages){
  print('PMC')
  cov_df = data.frame(admin_name=pmc_gf$admin_name, coverage=pmc_gf$coverage)
  print(paste0('.... Num LGAs: ', length(unique(cov_df$admin_name)),  '; median coverage=', round(median(cov_df$coverage),2)))
}






################################################################################
# Vaccine
################################################################################
vacc_gf = read.csv(paste0(base_sim_input_dir, '/interventions_projections/vacc_cat1p.csv'))
# subset to GF admins
vacc_gf = vacc_gf[vacc_gf$admin_name %in% gf_admins,]
if(print_coverages){
  print('Vaccine')
  cov_df = data.frame(admin_name=pmc_gf$admin_name, coverage=pmc_gf$coverage)
  print(paste0('.... Num LGAs: ', length(unique(cov_df$admin_name)),  '; median coverage=', round(median(cov_df$coverage),2)))
}





################################################################################
# IPTp
################################################################################
iptp_gf = read.csv(paste0(base_sim_input_dir, '/IPTp/estimated_past_IPTp_each_DS.csv'))
# subset to GF admins
iptp_gf = iptp_gf[iptp_gf$admin_name %in% gf_admins,]
if(print_coverages){
  print('IPTp')
  cov_df = data.frame(admin_name=iptp_gf$admin_name, coverage=iptp_gf$X2021)
  print(paste0('.... Num LGAs: ', length(unique(cov_df$admin_name)),  '; median coverage=', round(median(cov_df$coverage),2)))
}









