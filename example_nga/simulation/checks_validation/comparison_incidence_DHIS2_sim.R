#################################################################################################################################################
# comparison_incidence_DHIS2_sim.R
# SNT - Nigeria
# contact: Monique Ambrose
# January 2023
#
# create a series of plots that compare simulation and data, either for the same (matched) DS or for mismatched DS to see how well the 
#    simulation seasonality and incidence trends align with DHIS2 data
#
#################################################################################################################################################


###########################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                                setup
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###########################################################################

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(data.table)
library(RColorBrewer)
library(tidyr)
library(tibble)
library(dplyr)
library(reshape2)
# library(hablar)
library(haven)
library(raster)

###################################################################
#   read in and format data and simulation output
###################################################################
script_dir = 'C:/Users/moniqueam/Documents/malaria-nga-snt22'
base_filepath = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration'
base_hbhi_filepath = paste0(base_filepath, '/hbhi_nigeria/snt_2022')
pop_arch_filepath = paste0(base_hbhi_filepath, '/admin_pop_archetype.csv')
shapefile_filepath = paste0(base_filepath, '/hbhi_nigeria/SpatialClustering/reference_rasters_shapefiles/NGA_DS_clusteringProjection.shp')
sim_filepath_2010 = paste0(base_hbhi_filepath, '/simulation_output/simulations_to_present/NGA_toPresent_allInter_v9')
num_zeros_allowed = 1

source(paste0(script_dir,'/standardize_admin_names.R'))

if(!dir.exists(paste0(sim_filepath_2010, '/_plots'))) dir.create(paste0(sim_filepath_2010, '/_plots'))
if(!dir.exists(paste0(sim_filepath_2010, '/_plots/_validation'))) dir.create(paste0(sim_filepath_2010, '/_plots/_validation'))


# - - - - - - - - - - - - - - - - #
# LGA populations and archetypes
# - - - - - - - - - - - - - - - - #
pop_arch = fread(pop_arch_filepath)


# - - - - - - - - - - - - - - - - #
# functions
# - - - - - - - - - - - - - - - - #
get_cases_relative_to_reference_period = function(incidence_dhis2, reference_years=2018){
  # Rescale each LGA's case count relative to reference_years to get trends across months and years.
  incidence_ref_period = incidence_dhis2[incidence_dhis2$year %in% reference_years,]
  mean_incidence_ref_period = incidence_ref_period %>% group_by(admin_name) %>%
    summarise(mean_conf_all_age_ref_period = mean(conf_all_age))
  incidence_dhis2 = merge(incidence_dhis2, mean_incidence_ref_period, by='admin_name', all.x=TRUE)
  incidence_dhis2$rescaled_conf_all_age = incidence_dhis2$conf_all_age / incidence_dhis2$mean_conf_all_age_ref_period
  return(incidence_dhis2)
}

get_values_relative_to_reference_period = function(df, value_column, reference_years=2018){
  # Rescale each LGA's case count (or incidence) relative to reference_years to get trends across months and years.
  df_ref_period = df[df$year %in% reference_years,]
  mean_ref_period = df_ref_period %>% group_by(admin_name) %>%
    summarise(mean_val_ref_period = mean(get(value_column)))
  df = merge(df, mean_ref_period, by='admin_name', all.x=TRUE)
  df[paste0('rescaled_', value_column)] = df[value_column] / df$mean_val_ref_period
  return(df)
}


# - - - - - - - - - - - - - - - - #
# format DHIS2 data
# - - - - - - - - - - - - - - - - #
dataset_choice = 'from_2022'
reference_years = c(2018)
if(dataset_choice =='from_2022'){
  # use the monthly LGA cases from 2014-2018, obtained in 2022
  new_incidence = read_dta(paste0(base_filepath, '/nigeria_who/NGA_2022_SNT/_Submitted_data/Routine data/monthly_lga.dta'))
  incidence_dhis2 = as.data.table(new_incidence)
  incidence_dhis2$admin_name = incidence_dhis2$adm2
  incidence_dhis2 = incidence_dhis2[(incidence_dhis2$admin_name != ''),]
  incidence_dhis2$conf_all_age = incidence_dhis2$conf_ov5 + incidence_dhis2$conf_u5
  incidence_dhis2$date = as.Date(paste0(incidence_dhis2$year, '-', incidence_dhis2$month, '-01'), tryFormats = c("%Y-%m-%d", "%y-%m-%d"))
  # make sure the admin names are consistent
  incidence_dhis2 = standardize_admin_names_in_df(target_names_df=pop_arch, origin_names_df=incidence_dhis2, target_names_col='admin_name', origin_names_col='admin_name')
  incidence_dhis2 = merge(incidence_dhis2, pop_arch[,c('admin_name', 'Archetype', 'pop_size')], all.x=TRUE)
  # incidence_dhis2_lga_2022 = incidence_dhis2
} else if(dataset_choice =='from_2019'){
  # using the monthly LGA cases from 2014-2018, obtained in 2019
  filename_incidence = paste0(base_filepath, '/nigeria_who/Routine_data/Monthly_data/monthly_lga_14-18.csv') # year, month, adm2, clinical_u5, clinical_a5, conf_u5, conf_a5
  incidence_dhis2 = fread(filename_incidence)
  incidence_dhis2$LGA = incidence_dhis2$adm2
  incidence_dhis2 = incidence_dhis2[(incidence_dhis2$LGA != ''),]
  incidence_dhis2$clinical_all_age = incidence_dhis2$clinical_a5 + incidence_dhis2$clinical_u5
  incidence_dhis2$conf_all_age = incidence_dhis2$conf_a5 + incidence_dhis2$conf_u5
  incidence_dhis2$date = as.Date(paste0(incidence_dhis2$year, '-', incidence_dhis2$month, '-01'), tryFormats = c("%Y-%m-%d", "%y-%m-%d"))
  # make sure the admin names are consistent
  incidence_dhis2 = standardize_admin_names_in_df(target_names_df=pop_arch, origin_names_df=incidence_dhis2, target_names_col='admin_name', origin_names_col='admin_name')
  incidence_dhis2 = merge(incidence_dhis2, pop_arch[,c('LGA', 'Archetype')], all.x=TRUE)
  # incidence_dhis2_lga_2019 = incidence_dhis2
} else if(dataset_choice =='from_2019_RIA'){
  # using the monthly LGA cases from RIA, also with info on archetype
  filename_incidence = paste0(base_hbhi_filepath, '/incidence/RIA_by_LGA_and_rep_DS.csv') # year, month, adm2, clinical_u5, clinical_a5, conf_u5, conf_a5
  incidence_dhis2 = fread(filename_incidence)
  incidence_dhis2 = incidence_dhis2[(incidence_dhis2$admin_name != ''),]
  incidence_dhis2$conf_all_age = incidence_dhis2$AllagesOutpatientMalariaC
  incidence_dhis2$date = as.Date(incidence_dhis2$date)
  incidence_dhis2$year = format(incidence_dhis2$date, format="%Y")
  # make sure the admin names are consistent
  incidence_dhis2 = standardize_admin_names_in_df(target_names_df=pop_arch, origin_names_df=incidence_dhis2, target_names_col='admin_name', origin_names_col='admin_name')
  # incidence_dhis2_ria_2010 = incidence_dhis2
}
# get incidence using LGA's total population size
incidence_dhis2$Incidence = incidence_dhis2$conf_all_age / incidence_dhis2$pop_size * 1000
# rescale each LGA's case count and incidence relative to the mean from the reference period
incidence_dhis2 = get_cases_relative_to_reference_period(incidence_dhis2=incidence_dhis2, reference_years=reference_years)



# - - - - - - - - - - - - - - - - #
# simulation output
# - - - - - - - - - - - - - - - - #
pfpr_case_all_2010 = fread(paste0(sim_filepath_2010, '/All_Age_monthly_Cases.csv'))
pfpr_case_all_2010[,date:=as.Date(date)]
pfpr_case_all_2010$year = lubridate::year(pfpr_case_all_2010$date)
pfpr_case_all_2010$month = lubridate::month(pfpr_case_all_2010$date)

# mean values across runs
pfpr_case_all_runMeans  = pfpr_case_all_2010 %>% group_by(date, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 
# make sure the admin names are consistent
pfpr_case_all_runMeans = standardize_admin_names_in_df(target_names_df=pop_arch, origin_names_df=pfpr_case_all_runMeans, target_names_col='admin_name', origin_names_col='admin_name')


# get incidence of cases that would have been recorded as confirmed malaria
pfpr_case_all_runMeans$treatment_incidence_include_NMF = (pfpr_case_all_runMeans$Received_Treatment + pfpr_case_all_runMeans$Received_NMF_Treatment) / pfpr_case_all_runMeans$`Statistical Population` * 1000

# get incidence rescaled relative to reference year (to compare against DHIS2 data)
pfpr_case_all_runMeans = get_values_relative_to_reference_period(df=pfpr_case_all_runMeans, value_column='treatment_incidence_include_NMF', reference_years=reference_years)
pfpr_case_all_runMeans$rescaled_treatment_incidence_include_NMF[pfpr_case_all_runMeans$rescaled_treatment_incidence_include_NMF >9999] = NA


# ######## 
# # U5 - note: does not currently include NMF and malaria treatment rates for U5 - would need to add that for the following to work.
# pfpr_case_u5_2010 = fread(paste0(sim_filepath_2010, '/U5_PfPR_ClinicalIncidence_severeTreatment.csv'))
# pfpr_case_u5_2010[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
# # mean values across runs
# pfpr_case_u5_runMeans  = pfpr_case_u5_2010 %>% group_by(date, admin_name) %>%
#   summarise_all(mean) %>% ungroup()
# pfpr_case_u5_runMeans$admin_name = toupper(pfpr_case_u5_runMeans$admin_name)
# 
# # get incidence of cases that would have been recorded as confirmed malaria
# pfpr_case_u5_runMeans$treatment_incidence_include_NMF = (pfpr_case_u5_runMeans$Received_Treatment + pfpr_case_u5_runMeans$Received_NMF_Treatment) / pfpr_case_u5_runMeans$`Statistical Population` * 1000
# 
# # get incidence rescaled relative to reference year (to compare against DHIS2 data)
# pfpr_case_u5_runMeans = get_values_relative_to_reference_period(df=pfpr_case_u5_runMeans, value_column='treatment_incidence_include_NMF', reference_years=reference_years)
# pfpr_case_u5_runMeans$rescaled_treatment_incidence_include_NMF[pfpr_case_u5_runMeans$rescaled_treatment_incidence_include_NMF >9999] = NA



# - - - - - - - - - - - - - - - - #
# years and DS with SMC
# - - - - - - - - - - - - - - - - #
smc_filename = file.path(base_hbhi_filepath, 'simulation_inputs', 'interventions_2010_toPresent', 'smc_2010_toPresent.csv')
smc_dt = fread(smc_filename)
smc_dt$receivedSMC = smc_dt$coverage_high_access_U5 > 0
smc_dt = smc_dt[,c('year','admin_name','receivedSMC')]
smc_dt = distinct(smc_dt)
# make sure the admin names are consistent
smc_dt = standardize_admin_names_in_df(target_names_df=pop_arch, origin_names_df=smc_dt, target_names_col='admin_name', origin_names_col='admin_name')





#######################################################################################################################################
# process dhis2 and simulation data into same state as dhis2 data were originally used to create archetypes and calibrate seasonality
#######################################################################################################################################
###### DHIS2
incidence_2018 = incidence_dhis2[incidence_dhis2$year == 2018,]
incidence_2018$is_zero = incidence_2018$conf_all_age == 0
mean_incidence_2018 = incidence_2018 %>% group_by(admin_name) %>%
  summarise(mean_year_conf_all_age = mean(conf_all_age),
            num_zeros = sum(is_zero))
incidence_2018 = merge(incidence_2018, mean_incidence_2018, by='admin_name', all.x=TRUE)
incidence_2018$rescaled_conf_all_age = incidence_2018$conf_all_age / incidence_2018$mean_year_conf_all_age
incidence_2018$rescaled_conf_all_age[incidence_2018$num_zeros > num_zeros_allowed] = NA
incidence_2019 = incidence_dhis2[incidence_dhis2$year == 2019,]
incidence_2019$is_zero = incidence_2019$conf_all_age == 0
mean_incidence_2019 = incidence_2019 %>% group_by(admin_name) %>%
  summarise(mean_year_conf_all_age = mean(conf_all_age),
            num_zeros = sum(is_zero))
incidence_2019 = merge(incidence_2019, mean_incidence_2019, by='admin_name', all.x=TRUE)
incidence_2019$rescaled_conf_all_age = incidence_2019$conf_all_age / incidence_2019$mean_year_conf_all_age
incidence_2019$rescaled_conf_all_age[incidence_2019$num_zeros > num_zeros_allowed] = NA
incidence_2018_2019 = rbind(incidence_2018, incidence_2019)
incidence_2018_2019_original = incidence_2018_2019

# get within-State monthly average (across LGAs and 2018/2019)
dhis2_incidence_state_ave = incidence_2018_2019 %>% group_by(adm1, month) %>%
  summarise(mean_rescaled_conf_all_age = mean(rescaled_conf_all_age, na.rm=TRUE))


####### simulations
sim_incidence_2018 = pfpr_case_all_runMeans[pfpr_case_all_runMeans$year == 2018,]
mean_sim_incidence_2018 = sim_incidence_2018 %>% group_by(admin_name) %>%
  summarise(mean_year_incidence = mean(treatment_incidence_include_NMF))
sim_incidence_2018 = merge(sim_incidence_2018, mean_sim_incidence_2018, by='admin_name', all.x=TRUE)
sim_incidence_2018$rescaled_incidence_all_age = sim_incidence_2018$treatment_incidence_include_NMF / sim_incidence_2018$mean_year_incidence
sim_incidence_2019 = pfpr_case_all_runMeans[pfpr_case_all_runMeans$year == 2019,]
mean_sim_incidence_2019 = sim_incidence_2019 %>% group_by(admin_name) %>%
  summarise(mean_year_incidence = mean(treatment_incidence_include_NMF))
sim_incidence_2019 = merge(sim_incidence_2019, mean_sim_incidence_2019, by='admin_name', all.x=TRUE)
sim_incidence_2019$rescaled_incidence_all_age = sim_incidence_2019$treatment_incidence_include_NMF / sim_incidence_2019$mean_year_incidence
sim_incidence_2018_2019 = rbind(sim_incidence_2018, sim_incidence_2019)
sim_incidence_2018_2019_original = sim_incidence_2018_2019



# get within-State monthly average (across LGAs and 2018/2019)
# need to merge in population/archetype information to get state associated with each admin
sim_incidence_2018_2019 = merge(sim_incidence_2018_2019, pop_arch[,c('admin_name','State')])
sim_incidence_2018_2019$adm1 = sim_incidence_2018_2019$State
sim_incidence_state_ave = sim_incidence_2018_2019 %>% group_by(adm1, month) %>%
  summarise(mean_rescaled_incidence_all_age = mean(rescaled_incidence_all_age, na.rm=TRUE))




###################################################################
# merge DHS and corresponding simulation values
###################################################################

# match the same DS from simulation and data
incidence_matched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans, by=c('admin_name', 'year','month', 'date'), all.x=TRUE)
# incidence_matched$treatment_incidence_include_NMF = (incidence_matched$Received_Treatment + incidence_matched$Received_NMF_Treatment) / incidence_matched$`Statistical Population` * 1000
# incidence: match date from sim and data but shuffle DS
LGAs = unique(pfpr_case_all_runMeans$admin_name)
LGAs_shuffled = LGAs[c(2:length(LGAs), 1)]
pfpr_case_all_runMeans_mis = pfpr_case_all_runMeans
pfpr_case_all_runMeans_mis$admin_name = sapply(pfpr_case_all_runMeans$admin_name, function(x) LGAs_shuffled[which(LGAs == x)])
incidence_mismatched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans_mis, by=c('admin_name', 'year','month', 'date'), all.x=TRUE)
incidence_mismatched$treatment_incidence_include_NMF = (incidence_mismatched$Received_Treatment + incidence_mismatched$Received_NMF_Treatment) / incidence_mismatched$`Statistical Population` * 1000

# add smc info
incidence_matched = merge(incidence_matched, smc_dt, by=c('year','admin_name'), all.x=TRUE)
incidence_matched$receivedSMC[is.na(incidence_matched$receivedSMC)] = FALSE
incidence_matched = incidence_matched[-which(is.na(incidence_matched$month)),]
incidence_mismatched = incidence_mismatched[-which(is.na(incidence_mismatched$month)),]

# get average for each DS-month across all years
incidence_ds_month_means_matched = incidence_matched[,c('admin_name','year','month','Incidence', 'rescaled_conf_all_age', 'treatment_incidence_include_NMF', 'rescaled_treatment_incidence_include_NMF')] %>%  group_by(month, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 
incidence_ds_month_means_mismatched = incidence_mismatched[,c('admin_name','year','month','Incidence','treatment_incidence_include_NMF')] %>%  group_by(month, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 



###########################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                            plot comparisons
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###########################################################################


#################################################################################################
# plot timeseries of DHIS2 incidence in each DS through time
#################################################################################################
png(paste0(sim_filepath_2010, '/_plots/_validation/DHIS2_timeseries.png'), width=8, height=5, units='in', res=900)
par(mfrow=c(1,1), mar=c(4,4,2,2))
plot(incidence_matched[admin_name == unique(incidence_matched$admin_name)[1], date], (incidence_matched[admin_name == unique(incidence_matched$admin_name)[1], Incidence]), type='l', xlim=c(min(incidence_matched$date), max(incidence_matched$date)), ylim=c(0,max(incidence_matched$Incidence, na.rm=TRUE)), xlab='date', ylab=c('DHIS2 incidence ','(confirmed + presumed malaria)/population*1000'), main=paste('DHIS2 timeseries'), bty='L', col='white')
for (i_ds in 1:length(unique(incidence_matched$admin_name))){
  ds_df = incidence_matched[admin_name == unique(incidence_matched$admin_name)[i_ds],]
  ds_df = ds_df[order(ds_df$date),]
  # plot(ds_df$date, ds_df$maltreat_u5, type='b', pch=c(21,19)[ds_df$received_smc+1], main=paste('DHIS2: ', LGAs_plot[i_ds]))
  lines(ds_df$date, (ds_df$Incidence),  col='black')
}
dt_mean = incidence_matched[,c('date','Incidence')] %>% group_by(date) %>%  
  summarise_all(median, na.rm=TRUE) %>% ungroup() 
lines(dt_mean$date, (dt_mean$Incidence), lwd=3, col='blue')
dev.off()



#################################################################################################
# plot timeseries of rescaled DHIS2 AND simulation incidence in each LGA through time
#################################################################################################

years_in_plot = 2016:2022
gg = ggplot()+
  geom_line(data=incidence_matched[incidence_matched$year %in% years_in_plot,], aes(x=date, y=rescaled_conf_all_age, group=admin_name), color='blue')+
  geom_line(data=incidence_matched[incidence_matched$year %in% years_in_plot,], aes(x=date, y=rescaled_treatment_incidence_include_NMF, group=admin_name), color='red')+
  theme_bw()+
  theme(legend.position='none')+
  facet_wrap('adm1', nrow=5, scales='free')
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_compare_rescaled_incidence_sim_DHIS2_byLGA.png'), gg, width=15, height=10)

ggplot()+
  # geom_line(data=incidence_matched[incidence_matched$year %in% years_in_plot,], aes(x=date, y=rescaled_conf_all_age, group=admin_name), color='blue')+
  geom_line(data=incidence_matched[incidence_matched$year %in% years_in_plot,], aes(x=date, y=rescaled_treatment_incidence_include_NMF, group=admin_name), color='red')+
  theme_bw()+
  theme(legend.position='none')+
  facet_wrap('adm1', nrow=5, scales='free')



###### show timeseries for subset of LGAs
admin_subset = unique(incidence_matched$admin_name)[1:25]
year_subset = 2017:2020
gg=ggplot()+
  geom_line(data=incidence_matched[incidence_matched$admin_name %in% admin_subset,], aes(x=date, y=rescaled_conf_all_age), color='blue')+
  geom_line(data=incidence_matched[incidence_matched$admin_name %in% admin_subset,], aes(x=date, y=rescaled_treatment_incidence_include_NMF), color='red')+
  theme_bw()+
  facet_wrap('admin_name', nrow=5)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_compare_rescaled_incidence_sim_DHIS2_subsetLGA.png'), gg, width=15, height=10)

gg=ggplot()+
  geom_line(data=incidence_matched[(incidence_matched$admin_name %in% admin_subset) & (incidence_matched$year %in% year_subset),], aes(x=date, y=rescaled_conf_all_age), color='blue')+
  geom_line(data=incidence_matched[(incidence_matched$admin_name %in% admin_subset) & (incidence_matched$year %in% year_subset),], aes(x=date, y=rescaled_treatment_incidence_include_NMF), color='red')+
  theme_bw()+
  facet_wrap('admin_name', nrow=5)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_compare_rescaled_incidence_sim_DHIS2_subsetLGA_subsetYears.png'), gg, width=15, height=10)




####################################################################################################################################
# plot DHIS2 and sim data after processing following rescaling originally used to create archetypes and calibrate seasonality
####################################################################################################################################
gg_dhis2 = ggplot() + 
  geom_line(data=incidence_2018_2019, aes(x=month, y=rescaled_conf_all_age, color=admin_name, linetype=factor(year))) + 
  geom_line(data=dhis2_incidence_state_ave, aes(x=month, y=mean_rescaled_conf_all_age), color='black', size=2) + 
  coord_cartesian(ylim = c(0,4)) +
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('adm1', nrow=5, scales='free')
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/rescaled_dhis2_monthly_cases_state_2018_2019_average_excessZerosRemoved.png'), gg_dhis2, width=25, height=12)

gg_sim = ggplot() + 
  geom_line(data=sim_incidence_2018_2019, aes(x=month, y=rescaled_incidence_all_age, color=admin_name, linetype=factor(year))) + 
  geom_line(data=sim_incidence_state_ave, aes(x=month, y=mean_rescaled_incidence_all_age), color='black', size=2) + 
  coord_cartesian(ylim = c(0,4)) +
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('adm1', nrow=5, scales='free')
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/rescaled_sim_monthly_cases_state_2018_2019_average_excessZerosRemoved.png'), gg_sim, width=25, height=12)


###### show timeseries for subset of LGAs with both sim and dhis2 on same plot
gg = ggplot() + 
  geom_line(data=incidence_2018_2019[(incidence_2018_2019$admin_name %in% admin_subset),], aes(x=month, y=rescaled_conf_all_age, linetype=factor(year)), color='blue') + 
  geom_line(data=sim_incidence_2018_2019[(sim_incidence_2018_2019$admin_name %in% admin_subset),], aes(x=month, y=rescaled_incidence_all_age, linetype=factor(year)), color='red') + 
  coord_cartesian(ylim = c(0,4)) +
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('admin_name', nrow=5)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/rescaled_monthly_cases_2018_2019_excessZerosRemoved_subsetLGA.png'), gg, width=25, height=12)


#####################################################################################################
# timeseries comparison of average monthly incidence in simulation and DHIS2 in each DS
#####################################################################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# raw values - average across all years
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# include both DS-years with and without SMC
png(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_raw_monthly_incidence_comparison_sim_dhis2.png'), width=700, height=350)
par(mfrow=c(1,1))
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_matched$Incidence[incidence_ds_month_means_matched$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_matched$treatment_incidence_include_NMF[incidence_ds_month_means_matched$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_matched$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,max(c(incidence_ds_month_means_matched$Incidence, incidence_ds_month_means_matched$treatment_incidence_include_NMF), na.rm=TRUE)), type='b', bty='L', ylab='incidence', xlab='month')
points(incidence_ds_month_means_matched$month+jitter, incidence_ds_month_means_matched$treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_matched$month+jitter, incidence_ds_month_means_matched$Incidence, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=100, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
dev.off()
# include both DS-years with and without SMC - zoom in on part of plot where most data are
png(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_raw_monthly_incidence_comparison_sim_dhis2_zoom.png'), width=700, height=350)
par(mfrow=c(1,1))
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_matched$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,quantile(c(incidence_ds_month_means_matched$Incidence, incidence_ds_month_means_matched$treatment_incidence_include_NMF), probs=0.995, na.rm=TRUE)*1.1), type='b', bty='L', ylab='incidence', xlab='month')
points(incidence_ds_month_means_matched$month+jitter, incidence_ds_month_means_matched$treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_matched$month+jitter, incidence_ds_month_means_matched$Incidence, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=80, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
dev.off()


# separate by DS-years with and DS-years without SMC
###### full range of data
png(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_raw_monthly_incidence_comparison_sim_dhis2_withWithoutSMC_allYears.png'), width=700, height=350)
par(mfrow=c(1,2))
# DS-years with SMC
incidence_ds_month_means_smc = incidence_matched[(incidence_matched$receivedSMC),c('admin_name','year','month','Incidence','treatment_incidence_include_NMF')] %>%  group_by(month, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_smc$Incidence[incidence_ds_month_means_smc$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_smc$treatment_incidence_include_NMF[incidence_ds_month_means_smc$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_smc$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,max(c(incidence_ds_month_means_matched$Incidence, incidence_ds_month_means_matched$treatment_incidence_include_NMF), na.rm=TRUE)), type='b', bty='L', ylab='incidence', xlab='month', main='LGA-years with SMC')
points(incidence_ds_month_means_smc$month+jitter, incidence_ds_month_means_smc$treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_smc$month+jitter, incidence_ds_month_means_smc$Incidence, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=100, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')

# DS-years without SMC
incidence_ds_month_means_nosmc = incidence_matched[(!incidence_matched$receivedSMC),c('admin_name','year','month','Incidence','treatment_incidence_include_NMF')] %>%  group_by(month, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_nosmc$Incidence[incidence_ds_month_means_nosmc$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_nosmc$treatment_incidence_include_NMF[incidence_ds_month_means_nosmc$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_nosmc$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,max(c(incidence_ds_month_means_matched$Incidence, incidence_ds_month_means_matched$treatment_incidence_include_NMF), na.rm=TRUE)), type='b', bty='L', ylab='incidence', xlab='month', main='LGA-years without SMC')
points(incidence_ds_month_means_nosmc$month+jitter, incidence_ds_month_means_nosmc$treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_nosmc$month+jitter, incidence_ds_month_means_nosmc$Incidence, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=100, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
dev.off()
par(mfrow=c(1,1))

####### zoomed in
png(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_raw_monthly_incidence_comparison_sim_dhis2_withWithoutSMC_allYears_zoom.png'), width=700, height=350)
par(mfrow=c(1,2))
# DS-years with SMC
incidence_ds_month_means_smc = incidence_matched[(incidence_matched$receivedSMC),c('admin_name','year','month','Incidence','treatment_incidence_include_NMF')] %>%  group_by(month, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_smc$Incidence[incidence_ds_month_means_smc$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_smc$treatment_incidence_include_NMF[incidence_ds_month_means_smc$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_smc$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,quantile(c(incidence_ds_month_means_matched$Incidence, incidence_ds_month_means_matched$treatment_incidence_include_NMF), probs=0.995, na.rm=TRUE)*1.1), type='b', bty='L', ylab='incidence', xlab='month', main='LGA-years with SMC')
points(incidence_ds_month_means_smc$month+jitter, incidence_ds_month_means_smc$treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_smc$month+jitter, incidence_ds_month_means_smc$Incidence, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=100, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')

# DS-years without SMC
incidence_ds_month_means_nosmc = incidence_matched[(!incidence_matched$receivedSMC),c('admin_name','year','month','Incidence','treatment_incidence_include_NMF')] %>%  group_by(month, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_nosmc$Incidence[incidence_ds_month_means_nosmc$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_nosmc$treatment_incidence_include_NMF[incidence_ds_month_means_nosmc$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_nosmc$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,quantile(c(incidence_ds_month_means_matched$Incidence, incidence_ds_month_means_matched$treatment_incidence_include_NMF), probs=0.995, na.rm=TRUE)*1.1), type='b', bty='L', ylab='incidence', xlab='month', main='LGA-years without SMC')
points(incidence_ds_month_means_nosmc$month+jitter, incidence_ds_month_means_nosmc$treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_nosmc$month+jitter, incidence_ds_month_means_nosmc$Incidence, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=100, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
dev.off()
par(mfrow=c(1,1))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# rescaled values
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# rescaled_conf_all_age ~ rescaled_treatment_incidence_include_NMF
# include both DS-years with and without SMC
png(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_rescaled_monthly_incidence_comparison_sim_dhis2.png'), width=700, height=350)
par(mfrow=c(1,1))
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_matched$rescaled_conf_all_age[incidence_ds_month_means_matched$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_matched$rescaled_treatment_incidence_include_NMF[incidence_ds_month_means_matched$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_matched$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,max(c(incidence_ds_month_means_matched$rescaled_conf_all_age, incidence_ds_month_means_matched$rescaled_treatment_incidence_include_NMF), na.rm=TRUE)), type='b', bty='L', ylab='rescaled incidence', xlab='month')
points(incidence_ds_month_means_matched$month+jitter, incidence_ds_month_means_matched$rescaled_treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_matched$month+jitter, incidence_ds_month_means_matched$rescaled_conf_all_age, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=100, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
dev.off()
# include both DS-years with and without SMC - zoom in on part of plot where most data are
png(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_rescaled_monthly_incidence_comparison_sim_dhis2_zoom.png'), width=700, height=350)
par(mfrow=c(1,1))
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_matched$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,quantile(c(incidence_ds_month_means_matched$rescaled_conf_all_age, incidence_ds_month_means_matched$rescaled_treatment_incidence_include_NMF), probs=0.995, na.rm=TRUE)*1.1), type='b', bty='L', ylab='rescaled incidence', xlab='month')
points(incidence_ds_month_means_matched$month+jitter, incidence_ds_month_means_matched$rescaled_treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_matched$month+jitter, incidence_ds_month_means_matched$rescaled_conf_all_age, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=4, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
dev.off()

# subset to reference_years - no longer using average for each LGA-month value across years
incidence_matched_subset = incidence_matched[incidence_matched$year %in% reference_years,]
# include both DS-years with and without SMC - zoom in on part of plot where most data are
png(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_rescaled_monthly_incidence_comparison_sim_dhis2_zoom_subsetYears.png'), width=700, height=350)
par(mfrow=c(1,1))
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_matched_subset$rescaled_conf_all_age[incidence_matched_subset$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_matched_subset$rescaled_treatment_incidence_include_NMF[incidence_matched_subset$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_matched_subset$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,quantile(c(incidence_matched_subset$rescaled_conf_all_age, incidence_matched_subset$rescaled_treatment_incidence_include_NMF), probs=0.995, na.rm=TRUE)*1.1), type='b', bty='L', ylab='rescaled incidence', xlab='month')
points(incidence_matched_subset$month+jitter, incidence_matched_subset$rescaled_treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_matched_subset$month+jitter, incidence_matched_subset$rescaled_conf_all_age, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=3, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
dev.off()


########### with versus without SMC (reference years only)
incidence_matched_subset = incidence_matched[incidence_matched$year %in% reference_years,]
# create separate plots for LGA-years with and without SMC - zoom in on part of plot where most data are
png(paste0(sim_filepath_2010, '/_plots/_validation/timeseries_rescaled_monthly_incidence_comparison_sim_dhis2_withWithoutSMC_zoom_subsetYears.png'), width=700, height=350)
par(mfrow=c(1,2))
# DS-years with SMC
incidence_ds_month_means_smc = incidence_matched_subset[(incidence_matched_subset$receivedSMC),c('admin_name','year','month','rescaled_conf_all_age','rescaled_treatment_incidence_include_NMF')] %>%  group_by(month, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_smc$rescaled_conf_all_age[incidence_ds_month_means_smc$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_smc$rescaled_treatment_incidence_include_NMF[incidence_ds_month_means_smc$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_smc$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,quantile(c(incidence_ds_month_means_matched$rescaled_conf_all_age, incidence_ds_month_means_matched$rescaled_treatment_incidence_include_NMF), probs=0.995, na.rm=TRUE)*1.1), type='b', bty='L', ylab='rescaled incidence', xlab='month', main='LGA-years with SMC')
points(incidence_ds_month_means_smc$month+jitter, incidence_ds_month_means_smc$rescaled_treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_smc$month+jitter, incidence_ds_month_means_smc$rescaled_conf_all_age, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=4, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')

# DS-years without SMC
incidence_ds_month_means_nosmc = incidence_matched[(!incidence_matched$receivedSMC),c('admin_name','year','month','rescaled_conf_all_age','rescaled_treatment_incidence_include_NMF')] %>%  group_by(month, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_nosmc$rescaled_conf_all_age[incidence_ds_month_means_nosmc$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_nosmc$rescaled_treatment_incidence_include_NMF[incidence_ds_month_means_nosmc$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_nosmc$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,quantile(c(incidence_ds_month_means_matched$rescaled_conf_all_age, incidence_ds_month_means_matched$rescaled_treatment_incidence_include_NMF), probs=0.995, na.rm=TRUE)*1.1), type='b', bty='L', ylab='rescaled incidence', xlab='month', main='LGA-years without SMC')
points(incidence_ds_month_means_nosmc$month+jitter, incidence_ds_month_means_nosmc$rescaled_treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_nosmc$month+jitter, incidence_ds_month_means_nosmc$rescaled_conf_all_age, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
# legend(x=0.7,y=100, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
dev.off()
par(mfrow=c(1,1))




#################################################################################################
#   scatterplots of simulated versus DHIS2 incidence for each DS (separate dots for each year)
#################################################################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# scatter with all months and years, color by year - using raw data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
linearMod = lm(Incidence ~ treatment_incidence_include_NMF, data=incidence_matched)  # build linear regression model on full data
# matched DS
p_match_all = ggplot(incidence_matched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(aes(col=as.factor(year)), shape=20, alpha=1) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  # geom_abline(slope=summary(linearMod)$coefficients[2],intercept=c(0,summary(linearMod)$coefficients[1]))+
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred') +
  scale_color_brewer(palette="Spectral")+ 
  ylab("monthly incidence from DHIS2") + 
  xlab("monthly incidence from simulation") +
  # ggtitle(paste0('all years: R^2=',round(summary(linearMod)$r.squared,2), ', slope=', round(summary(linearMod)$coefficients[2],2)))+ 
  theme_classic() 
# theme(legend.position = "none")
p_match_all
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_raw_incidence_match_sim_DHIS2_allYear_allMonth.png'), p_match_all, width=6, height=4.5)

# zoomed in
linearMod = lm(Incidence ~ treatment_incidence_include_NMF, data=incidence_matched)  # build linear regression model on full data
p_match_all = ggplot(incidence_matched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(aes(col=as.factor(year)), shape=20, alpha=1) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  # geom_abline(slope=summary(linearMod)$coefficients[2],intercept=c(0,summary(linearMod)$coefficients[1]))+
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred', label.x=0, label.y=150) +
  scale_color_brewer(palette="Spectral")+ 
  ylab("monthly incidence from DHIS2") + 
  xlab("monthly incidence from simulation") +
  coord_cartesian(xlim=c(0,150), ylim=c(0,150))+
  # ggtitle(paste0('all years: R^2=',round(summary(linearMod)$r.squared,2), ', slope=', round(summary(linearMod)$coefficients[2],2)))+ 
  theme_classic() 
# theme(legend.position = "none")
p_match_all
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_raw_incidence_match_sim_DHIS2_allYear_allMonth_zoom.png'), p_match_all, width=6, height=4.5)

# zoom in and later years only
linearMod = lm(Incidence ~ treatment_incidence_include_NMF, data=incidence_matched[incidence_matched$year>2017,])  # build linear regression model on full data
p_match_all = ggplot(incidence_matched[incidence_matched$year>2017,], aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(aes(col=as.factor(year)), shape=20, alpha=1) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  # geom_abline(slope=summary(linearMod)$coefficients[2],intercept=c(0,summary(linearMod)$coefficients[1]))+
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred') +
  scale_color_brewer(palette="Spectral")+ 
  ylab("monthly incidence from DHIS2") + 
  xlab("monthly incidence from simulation") +
  coord_cartesian(xlim=c(0,150), ylim=c(0,150))+
  # ggtitle(paste0('all years: R^2=',round(summary(linearMod)$r.squared,2), ', slope=', round(summary(linearMod)$coefficients[2],2)))+ 
  theme_classic() 
# theme(legend.position = "none")
p_match_all
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_raw_incidence_match_sim_DHIS2_laterYears_allMonth_zoom.png'), p_match_all, width=6, height=4.5)


# for (yy in seq(2013,2019,2)){
#   p_match_yy = ggplot(incidence_matched[incidence_matched$year == yy,], aes(y=Incidence, x=treatment_incidence_include_NMF)) +
#     geom_point(shape=20, alpha=1) + 
#     geom_abline(slope=1, intercept=c(0,0)) +
#     geom_smooth(method=lm)+
#     ggtitle(yy)+ 
#     theme_classic() 
#   # theme(legend.position = "none")
#   print(p_match_yy)
# }
# mismatched DS
p_mismatch_all = ggplot(incidence_mismatched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(aes(col=as.factor(year)), shape=20, alpha=1) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  # geom_abline(slope=summary(linearMod)$coefficients[2],intercept=c(0,summary(linearMod)$coefficients[1]))+
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred') +
  scale_color_brewer(palette="Spectral")+ 
  ylab("monthly incidence from DHIS2") + 
  xlab("monthly incidence from simulation") +
  # ggtitle(paste0('all years: R^2=',round(summary(linearMod)$r.squared,2), ', slope=', round(summary(linearMod)$coefficients[2],2)))+ 
  theme_classic() 
# theme(legend.position = "none")
p_mismatch_all
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_raw_incidence_mismatch_sim_DHIS2_allYear_allMonth.png'), p_mismatch_all, width=6, height=4.5)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# scatter with all months and years, color by year - using rescaled data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
linearMod = lm(rescaled_conf_all_age ~ rescaled_treatment_incidence_include_NMF, data=incidence_matched)  # build linear regression model on full data
# matched DS
p_match_all = ggplot(incidence_matched, aes(y=rescaled_conf_all_age, x=rescaled_treatment_incidence_include_NMF)) +
  geom_point(aes(col=as.factor(year)), shape=20, alpha=1) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  # geom_abline(slope=summary(linearMod)$coefficients[2],intercept=c(0,summary(linearMod)$coefficients[1]))+
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred') +
  scale_color_brewer(palette="Spectral")+ 
  ylab("monthly incidence from DHIS2 (rescaled)") + 
  xlab("monthly incidence from simulation (rescaled)") +
  # ggtitle(paste0('all years: R^2=',round(summary(linearMod)$r.squared,2), ', slope=', round(summary(linearMod)$coefficients[2],2)))+ 
  theme_classic() 
# theme(legend.position = "none")
p_match_all
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_rescaled_incidence_match_sim_DHIS2_allYear_allMonth.png'), p_match_all, width=6, height=4.5)



# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# #   scatterplots of simulated and DHIS2 incidence for each DS, faceted by month or month-year
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # matched DS
# p_match_all = ggplot(incidence_matched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
#   geom_point(aes(col=year), shape=20, alpha=0.5) + 
#   geom_abline(slope=1, intercept=c(0,0)) +
#   geom_smooth(method=lm)+
#   ylab("incidence from DHIS2") + 
#   xlab("incidence from simulation") +
#   theme_classic() +
#   theme(legend.position = "none")
# # mismatched DS
# p_mismatch_all = ggplot(incidence_mismatched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
#   geom_point(aes(col=year), shape=20, alpha=0.5) + 
#   geom_abline(slope=1, intercept=c(0,0)) +
#   geom_smooth(method=lm)+
#   ylab("incidence from DHIS2") + 
#   xlab("incidence from simulation") +
#   theme_classic() +
#   theme(legend.position = "none")
# 
# # facet plot to break out by year and month
# # matched DS
# gg = p_match_all + facet_grid(year ~ month)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_incidence_match_sim_DHIS2.png'), gg, width=11, height=7.5)
# # mismatched DS
# gg = p_mismatch_all + facet_grid(year ~ month)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_incidence_mismatch_sim_DHIS2.png'), gg, width=11, height=7.5)
# 
# # facet plot broken out by month, color by year
# # matched DS
# gg = p_match_all + facet_grid(. ~ month)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_monthly_incidence_match_sim_DHIS2.png'), gg, width=12, height=3)
# # mismatched DS
# gg = p_mismatch_all + facet_grid(. ~ month)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_monthly_incidence_mismatch_sim_DHIS2.png'), gg, width=12, height=3)
# 
# 
# # - - - - - - - - - - - - - - - #
# # zoom into specific months
# # - - - - - - - - - - - - - - - #
# months = c(7,8)
# p_match_all = ggplot(incidence_matched[incidence_matched$month %in% months,], aes(y=Incidence, x=treatment_incidence_include_NMF)) +
#   geom_point(aes(col=year), shape=20, alpha=0.5) + 
#   geom_abline(slope=1, intercept=c(0,0)) +
#   geom_smooth(method=lm)+
#   theme_classic() +
#   theme(legend.position = "none")
# gg = p_match_all + facet_grid(year ~ month)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_incidence_match_sim_DHIS2_months78.pdf'), gg, width=20, height=20)
# # mismatched DS
# p_mismatch_all = ggplot(incidence_mismatched[incidence_mismatched$month %in% months,], aes(y=Incidence, x=treatment_incidence_include_NMF)) +
#   geom_point(aes(col=year), shape=20, alpha=0.5) + 
#   geom_abline(slope=1, intercept=c(0,0)) +
#   geom_smooth(method=lm)+
#   theme_classic() +
#   theme(legend.position = "none")
# gg = p_mismatch_all + facet_grid(year ~ month)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_incidence_mismatch_sim_DHIS2_months78.pdf'), gg, width=20, height=20)



#################################################################################################
#   scatterplots of simulated versus DHIS2 monthly incidence for each DS (average across year)
#################################################################################################

# facet plot broken out by month, showing average monthly values for each DS across all years
# matched DS
p_match_all = ggplot(incidence_ds_month_means_matched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(shape=20, alpha=0.5) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  ylab('DHIS2 incidence') + 
  xlab('simulation incidence') +
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred', aes(label = ..r.label..)) +
  theme_classic() +
  theme(legend.position = "none")
gg = p_match_all + facet_grid(. ~ month)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_mean_monthly_incidence_match_sim_DHIS2.png'), gg, width=12, height=3)
# mismatched DS
p_mismatch_all = ggplot(incidence_ds_month_means_mismatched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(shape=20, alpha=0.5) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  ylab('DHIS2 incidence') + 
  xlab('simulation incidence') +
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred', aes(label = ..r.label..)) +
  theme_classic() +
  theme(legend.position = "none")
gg = p_mismatch_all + facet_grid(. ~ month)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_mean_monthly_incidence_mismatch_sim_DHIS2.png'), gg, width=12, height=3)



##########################################################################################################
#   violinplots with residuals in each month, broken out by year
##########################################################################################################
########### raw values
# matched DS
incidence_matched$residual = incidence_matched$Incidence - incidence_matched$treatment_incidence_include_NMF
p_match_all = ggplot(incidence_matched[incidence_matched$year<2019,], aes(y=residual, x=as.factor(month))) +
  geom_violin()+
  ylim(c(-160,160))+
  ylab('DHIS2 incidence - simulated')+
  xlab('month')+
  geom_abline(slope=0, intercept=c(0,0))+
  theme_classic() +
  theme(legend.position = "none")
gg = p_match_all + facet_grid(. ~ year)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/violin_raw_incidence_residual_match_sim_DHIS2.png'), gg, width=7.4, height=3)
# mismatched DS
incidence_mismatched$residual = incidence_mismatched$Incidence - incidence_mismatched$treatment_incidence_include_NMF
p_mismatch_all = ggplot(incidence_mismatched[incidence_mismatched$year<2019,], aes(y=residual, x=as.factor(month))) +
  geom_violin()+
  ylim(c(-160,160))+
  ylab('DHIS2 incidence - simulated')+
  xlab('month')+
  geom_abline(slope=0, intercept=c(0,0))+
  theme_classic() +
  theme(legend.position = "none")
gg = p_mismatch_all + facet_grid(. ~ year)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/violin_raw_incidence_residual_mismatch_sim_DHIS2.png'), gg, width=7.4, height=3)

# mean absolute difference in each year
incidence_matched$abs_residual = abs(incidence_matched$residual)
mean_residuals_matched = incidence_matched[,c('year','abs_residual')] %>% group_by(year) %>%  
  summarise_all(mean, na.rm=TRUE) %>% ungroup() 
incidence_mismatched$abs_residual = abs(incidence_mismatched$residual)
mean_residuals_mismatched = incidence_mismatched[,c('year','abs_residual')] %>% group_by(year) %>%  
  summarise_all(mean, na.rm=TRUE) %>% ungroup() 


########### rescaled values
# rescaled_conf_all_age ~ rescaled_treatment_incidence_include_NMF
min_max = 15
# matched DS
incidence_matched$residual = incidence_matched$rescaled_conf_all_age - incidence_matched$rescaled_treatment_incidence_include_NMF
p_match_all = ggplot(incidence_matched[incidence_matched$year<2019,], aes(y=residual, x=as.factor(month))) +
  geom_violin()+
  ylim(c(-1*min_max, min_max))+
  ylab('DHIS2 - simulated rescaled incidence')+
  xlab('month')+
  geom_abline(slope=0, intercept=c(0,0))+
  theme_classic() +
  theme(legend.position = "none")
gg = p_match_all + facet_grid(. ~ year)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/violin_rescaled_incidence_residual_match_sim_DHIS2.png'), gg, width=7.4, height=3)
# mismatched DS
incidence_mismatched$residual = incidence_mismatched$rescaled_conf_all_age - incidence_mismatched$rescaled_treatment_incidence_include_NMF
p_mismatch_all = ggplot(incidence_mismatched[incidence_mismatched$year<2019,], aes(y=residual, x=as.factor(month))) +
  geom_violin()+
  ylim(c(-1*min_max, min_max))+
  ylab('DHIS2 - simulated rescaled incidence')+
  xlab('month')+
  geom_abline(slope=0, intercept=c(0,0))+
  theme_classic() +
  theme(legend.position = "none")
gg = p_mismatch_all + facet_grid(. ~ year)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/violin_rescaled_incidence_residual_mismatch_sim_DHIS2.png'), gg, width=7.4, height=3)


# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# #   violinplots of residuals of simulated and DHIS2 incidence *relative to July* for each DS
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# month_value = 7
# # subset to just July values
# # matched
# incidence_matched_july = incidence_matched[incidence_matched$month == month_value,c('admin_name','year','month','Incidence','treatment_incidence_include_NMF')]
# colnames(incidence_matched_july) = c('admin_name','year','month','dhis2_july_incidence','sim_july_incidence')
# incidence_matched_july = incidence_matched_july[,-c('month')]
# incidence_matched = merge(incidence_matched, incidence_matched_july, by=c('admin_name','year'), all.x=TRUE)
# incidence_matched$dhis2_incidence_relative = incidence_matched$Incidence / incidence_matched$dhis2_july_incidence
# incidence_matched$sim_incidence_relative = incidence_matched$treatment_incidence_include_NMF / incidence_matched$sim_july_incidence
# incidence_matched$residual_relative = incidence_matched$dhis2_incidence_relative - incidence_matched$sim_incidence_relative
# # mismatched
# incidence_mismatched_july = incidence_mismatched[incidence_mismatched$month == month_value,c('admin_name','year','month','Incidence','treatment_incidence_include_NMF')]
# colnames(incidence_mismatched_july) = c('admin_name','year','month','dhis2_july_incidence','sim_july_incidence')
# incidence_mismatched_july = incidence_mismatched_july[,-c('month')]
# incidence_mismatched = merge(incidence_mismatched, incidence_mismatched_july, by=c('admin_name','year'), all.x=TRUE)
# incidence_mismatched$dhis2_incidence_relative = incidence_mismatched$Incidence / incidence_mismatched$dhis2_july_incidence
# incidence_mismatched$sim_incidence_relative = incidence_mismatched$treatment_incidence_include_NMF / incidence_mismatched$sim_july_incidence
# incidence_mismatched$residual_relative = incidence_mismatched$dhis2_incidence_relative - incidence_mismatched$sim_incidence_relative
# 
# # yearly plot with violins of residuals for each month, relative to july
# # matched DS
# p_match_all = ggplot(incidence_matched[incidence_matched$year<2019,], aes(y=residual_relative, x=as.factor(month))) +
#   geom_violin()+
#   ylim(c(-80,20))+
#   ylab('DHIS2 incidence - simulated')+
#   xlab('month')+
#   geom_abline(slope=0, intercept=c(0,0))+
#   theme_classic() +
#   theme(legend.position = "none")
# gg = p_match_all + facet_grid(. ~ year)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/violin_incidence_relativeJuly_match_sim_DHIS2.png'), gg, width=12, height=3)
# # mismatched DS
# p_mismatch_all = ggplot(incidence_mismatched[incidence_mismatched$year<2019,], aes(y=residual_relative, x=as.factor(month))) +
#   geom_violin()+
#   ylim(c(-80,20))+
#   ylab('DHIS2 incidence - simulated')+
#   xlab('month')+
#   geom_abline(slope=0, intercept=c(0,0))+
#   theme_classic() +
#   theme(legend.position = "none")
# gg = p_mismatch_all + facet_grid(. ~ year)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/violin_incidence_relativeJuly_mismatch_sim_DHIS2.png'), gg, width=12, height=3)



################################################################################################
#   histogram of incidence differences between matched sim/data and mismatched sim/data
################################################################################################
####================================####
#  matched versus mismatched DS
####================================####
pdf(paste0(sim_filepath_2010, '/_plots/_validation/hist_incidence_abs_dif_sim_DHIS2_mismatchDS.pdf'), width=6, height=6)
#  ----  all years  ----  #
# relative difference for matching DS
match_rel_dif = abs(incidence_matched$Incidence - incidence_matched$treatment_incidence_include_NMF)

# mismatched DS (multiple mis-matches)
LGAs = unique(pfpr_case_all_runMeans$admin_name)
LGAs_shuffled = LGAs[c(2:length(LGAs), 1)]
mismatch_rel_dif = c()
for(ii in 1:20){
  pfpr_case_all_runMeans_mis = pfpr_case_all_runMeans
  pfpr_case_all_runMeans_mis$admin_name = sapply(pfpr_case_all_runMeans$admin_name, function(x) LGAs[c((ii+1):length(LGAs), 1:ii)][which(LGAs == x)])
  incidence_mismatched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans_mis, by=c('admin_name', 'year','month'), all.x=TRUE)
  incidence_mismatched$treatment_incidence_include_NMF = (incidence_mismatched$Received_Treatment + incidence_mismatched$Received_NMF_Treatment) / incidence_mismatched$`Statistical Population` * 1000
  mismatch_rel_dif = c(mismatch_rel_dif, abs(incidence_mismatched$Incidence - incidence_mismatched$treatment_incidence_include_NMF))
}
# compare histograms
par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
hist(match_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='',  xlab='difference')
mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
title(main='matched DS, all years', line=0.5)
hist(mismatch_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='', xlab='difference')
title(main='mismatched DS, all years', line=0.5)
mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)

#  ----  2010, 2014, 2017  ----  #
for(yy in 2013:2019){
  # relative difference for matching DS
  match_rel_dif = abs(incidence_matched[incidence_matched$year==yy,]$Incidence - incidence_matched[incidence_matched$year==yy,]$treatment_incidence_include_NMF)
  # mismatched DS (multiple mis-matches)
  LGAs = unique(pfpr_case_all_runMeans$admin_name)
  LGAs_shuffled = LGAs[c(2:length(LGAs), 1)]
  mismatch_rel_dif = c()
  for(ii in 1:20){
    pfpr_case_all_runMeans_mis = pfpr_case_all_runMeans
    pfpr_case_all_runMeans_mis$admin_name = sapply(pfpr_case_all_runMeans$admin_name, function(x) LGAs[c((ii+1):length(LGAs), 1:ii)][which(LGAs == x)])
    incidence_mismatched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans_mis, by=c('admin_name', 'year','month'), all.x=TRUE)
    incidence_mismatched$treatment_incidence_include_NMF = (incidence_mismatched$Received_Treatment + incidence_mismatched$Received_NMF_Treatment) / incidence_mismatched$`Statistical Population` * 1000
    mismatch_rel_dif = c(mismatch_rel_dif, abs(incidence_mismatched[incidence_mismatched$year==yy,]$Incidence - incidence_mismatched[incidence_mismatched$year==yy,]$treatment_incidence_include_NMF))
  }
  # compare histograms
  par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
  hist(match_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='',  xlab='difference')
  mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
  title(main=paste0('matched DS, ', yy), line=0.5)
  hist(mismatch_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='', xlab='difference')
  title(main=paste0('mismatched DS, ', yy), line=0.5)
  mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
}
dev.off()
par(mfrow=c(1,1))


####================================####
#  matched versus mismatched year
####================================####
pdf(paste0(sim_filepath_2010, '/_plots/_validation/hist_incidence_abs_dif_sim_DHIS2_mismatchYears.pdf'), width=6, height=6)
year_true_vect = rep(c(2013, 2015, 2017, 2019), each=3)
mismatch_year_vect = c(2015,2017, 2019, 2013, 2017, 2019, 2013, 2015, 2019, 2013, 2015, 2017)
mismatch_rel_dif = c()
for(ii in 1:length(year_true_vect)){
  year_true = year_true_vect[ii]
  mismatch_year = mismatch_year_vect[ii]
  match_rel_dif = abs(incidence_matched[incidence_matched$year==year_true,]$Incidence - incidence_matched[incidence_matched$year==year_true,]$treatment_incidence_include_NMF)
  # mismatched years
  pfpr_case_all_runMeans_mis = pfpr_case_all_runMeans
  pfpr_case_all_runMeans_mis$year[pfpr_case_all_runMeans_mis$year == year_true] = NA
  pfpr_case_all_runMeans_mis$year[pfpr_case_all_runMeans_mis$year == mismatch_year] = year_true
  incidence_mismatched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans_mis, by=c('admin_name', 'year','month'), all.x=TRUE)
  incidence_mismatched$treatment_incidence_include_NMF = (incidence_mismatched$Received_Treatment + incidence_mismatched$Received_NMF_Treatment) / incidence_mismatched$`Statistical Population` * 1000
  mismatch_rel_dif = c(mismatch_rel_dif, abs(incidence_mismatched[incidence_mismatched$year==year_true,]$Incidence - incidence_mismatched[incidence_mismatched$year==year_true,]$treatment_incidence_include_NMF))
  # compare histograms
  par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
  hist(match_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='',  xlab=' difference')
  title(main=paste0('matched: sim and data from ', year_true), line=0.5)
  mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
  hist(mismatch_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='', xlab=' difference')
  title(main=paste0('mismatched: data from ', year_true, ', sim from ' , mismatch_year), line=0.5)
  mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
}
dev.off()
par(mfrow=c(1,1))



####################################################################################
# matrix of median differences for matched/mismatched years of sim and data
####################################################################################
years = 2013:2019
median_abs_dif = matrix(NA, nrow=length(years), ncol=length(years))
median_dif = matrix(NA, nrow=length(years), ncol=length(years))
# rows are sim, columns are data
for(rr in 1:length(years)){
  for(cc in 1:length(years)){
    year_true = years[cc]
    mismatch_year = years[rr]
    pfpr_case_all_runMeans_mis = pfpr_case_all_runMeans
    if (year_true != mismatch_year){
      pfpr_case_all_runMeans_mis$year[pfpr_case_all_runMeans_mis$year == year_true] = NA
      pfpr_case_all_runMeans_mis$year[pfpr_case_all_runMeans_mis$year == mismatch_year] = year_true
    }
    incidence_mismatched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans_mis, by=c('admin_name', 'year','month'), all.x=TRUE)
    incidence_mismatched$treatment_incidence_include_NMF = (incidence_mismatched$Received_Treatment + incidence_mismatched$Received_NMF_Treatment) / incidence_mismatched$`Statistical Population` * 1000
    mismatch_abs_dif = abs(incidence_mismatched[incidence_mismatched$year==year_true,]$Incidence - incidence_mismatched[incidence_mismatched$year==year_true,]$treatment_incidence_include_NMF)
    median_abs_dif[rr,cc] = median(mismatch_abs_dif)
    mismatch_dif = (incidence_mismatched[incidence_mismatched$year==year_true,]$Incidence - incidence_mismatched[incidence_mismatched$year==year_true,]$treatment_incidence_include_NMF)
    median_dif[rr,cc] = median(mismatch_dif)    
  }
}
# matrix showing median difference between simulated and DHIS2 incidence
colnames(median_dif) = years
rownames(median_dif) = years
median_dif_df = as.data.frame(median_dif)
median_dif_df$year_sim = rownames(median_dif_df)
median_dif_long = gather(median_dif_df, year_data, difference, '2013':'2019',factor_key=TRUE)
gg = ggplot(median_dif_long, aes(year_sim, forcats::fct_rev(year_data))) +
  geom_tile(aes(fill = difference)) + 
  geom_text(aes(label = round(difference, 1))) +
  scale_fill_gradient2(low = "blue", mid='white', midpoint=0, high = "red")+
  ggtitle('median(dhis2 incidence - sim incidence)')
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/heat_incidence_year_dif_sim_DHIS2.pdf'), gg, width=10, height=10)

# matrix showing median absolute difference between simulated and DHIS2 incidence
colnames(median_abs_dif) = years
rownames(median_abs_dif) = years
# heatmap(median_abs_dif, Rowv=NA, Colv=NA, revC=TRUE)
median_abs_dif_df = as.data.frame(median_abs_dif)
median_abs_dif_df$year_sim = rownames(median_abs_dif_df)
median_abs_dif_long = gather(median_abs_dif_df, year_data, difference, '2013':'2019',factor_key=TRUE)
gg = ggplot(median_abs_dif_long, aes(year_sim, forcats::fct_rev(year_data))) +
  geom_tile(aes(fill = difference)) + 
  geom_text(aes(label = round(difference, 1))) +
  scale_fill_gradient(low = "white", high = "forestgreen")+
  ggtitle('median(abs(dhis2 incidence - sim incidence))')
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/heat_incidence_year_abs_dif_sim_DHIS2.pdf'), gg, width=10, height=10)








