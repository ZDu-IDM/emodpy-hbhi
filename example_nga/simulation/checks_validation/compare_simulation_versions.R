# compare_simulation_versions.R

library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)

base_filepath = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration/hbhi_nigeria'
version1_filepath = paste0(base_filepath, '/simulation_output/2010_to_2020_v10/NGA 2010-20 burnin_hs+itn+smc')
version2_filepath = paste0(base_filepath, '/snt_2022/simulation_output/2010_to_present/NGA_toPresent_allInter_v5')
pop_arch_filepath = paste0(base_filepath, '/snt_2022/admin_pop_archetype.csv')

pop_arch = fread(pop_arch_filepath)


# - - - - - - - - - - - - - - - - #
# simulation output
# - - - - - - - - - - - - - - - - #
get_prev_inc_sim_outputs = function(filepath){
  # read in and reformat simulation output
  pfpr_case_all = fread(paste0(filepath, '/All_Age_monthly_Cases.csv'))
  pfpr_case_all[,date:=as.Date(date)]
  pfpr_case_all$year = lubridate::year(pfpr_case_all$date)
  pfpr_case_all$month = lubridate::month(pfpr_case_all$date)
  colnames(pfpr_case_all)[colnames(pfpr_case_all)=='LGA'] = 'admin_name'
  pfpr_case_u5 = fread(paste0(filepath, '/U5_PfPR_ClinicalIncidence_severeTreatment.csv'))
  pfpr_case_u5[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
  colnames(pfpr_case_u5)[colnames(pfpr_case_u5)=='LGA'] = 'admin_name'
  
  # get incidence of cases that would have been recorded as confirmed malaria
  pfpr_case_all$treatment_incidence_include_NMF = (pfpr_case_all$Received_Treatment + pfpr_case_all$Received_NMF_Treatment) / pfpr_case_all$`Statistical Population` * 1000
  pfpr_case_all$clinical_incidence = pfpr_case_all$`New Clinical Cases` / pfpr_case_all$`Statistical Population` * 1000
  pfpr_case_u5$clinical_incidence = pfpr_case_u5$`Cases U5` / pfpr_case_u5$`Pop U5` * 1000

  # mean values across runs
  pfpr_case_all_means  = pfpr_case_all %>% group_by(date, admin_name) %>%  
    summarise_all(mean) %>% ungroup() 
  pfpr_case_u5_means  <- pfpr_case_u5 %>% group_by(date, admin_name) %>%  
    summarise_all(mean) %>% ungroup() 
  

  return(list(pfpr_case_all_means, pfpr_case_u5_means))
}


v1_output = get_prev_inc_sim_outputs(filepath = version1_filepath)
pfpr_case_all_v1 = v1_output[[1]]
pfpr_case_u5_v1 = v1_output[[2]]
pfpr_case_all_v1$simset = 'v1'
pfpr_case_u5_v1$simset = 'v1'

v2_output = get_prev_inc_sim_outputs(filepath = version2_filepath)
pfpr_case_all_v2 = v2_output[[1]]
pfpr_case_u5_v2 = v2_output[[2]]
pfpr_case_all_v2$simset = 'v2'
pfpr_case_u5_v2$simset = 'v2'


pfpr_case_all_comb = merge(pfpr_case_all_v1, pfpr_case_all_v2, all=TRUE)
pfpr_case_u5_comb = merge(pfpr_case_u5_v1, pfpr_case_u5_v2, all=TRUE)


# Timeseries plots
gg1 = ggplot(pfpr_case_all_comb, aes(x=date, y=`PfHRP2 Prevalence`, group=interaction(simset, admin_name), color=simset))+
  geom_line()
gg2 = ggplot(pfpr_case_all_comb, aes(x=date, y=clinical_incidence, group=interaction(simset, admin_name), color=simset))+
  geom_line()
gg3 = ggplot(pfpr_case_all_comb, aes(x=date, y=treatment_incidence_include_NMF, group=interaction(simset, admin_name), color=simset))+
  geom_line()
gg_all = ggarrange(gg1, gg2, gg3,
          labels = c("PfPR", "Clinical", "Treated"),
          ncol = 1, nrow = 3)
ggexport(gg_all, filename=paste0(version2_filepath, '/_plots/_validation/compareSimVersions_allAge_2019_2022.png'))


gg1 = ggplot(pfpr_case_u5_comb, aes(x=date, y=`PfPR U5`, group=interaction(simset, admin_name), color=simset))+
  geom_line()
gg2 = ggplot(pfpr_case_u5_comb, aes(x=date, y=clinical_incidence, group=interaction(simset, admin_name), color=simset))+
  geom_line()
gg_u5 = ggarrange(gg1, gg2,
                   labels = c("PfPR", "Clinical"),
                   ncol = 1, nrow = 2)
ggexport(gg_u5, filename=paste0(version2_filepath, '/_plots/_validation/compareSimVersions_U5_2019_2022.png'))


# show for subset of LGAs
admin_subset = unique(pfpr_case_all_comb$admin)[1:9]
gga=ggplot(pfpr_case_all_comb[pfpr_case_all_comb$admin_name %in% admin_subset,], aes(x=date, y=`PfHRP2 Prevalence`, group=interaction(simset, admin_name), color=simset))+
  geom_line()+
  facet_wrap('admin_name', nrow=3)
ggb=ggplot(pfpr_case_all_comb[pfpr_case_all_comb$admin_name %in% admin_subset,], aes(x=date, y=clinical_incidence, group=interaction(simset, admin_name), color=simset))+
  geom_line()+
  facet_wrap('admin_name', nrow=3)
gg_ab = ggarrange(gga, ggb,
                  labels = c("PfPR", "Clinical"),
                  ncol = 2, nrow = 1)
ggexport(gg_ab, filename=paste0(version2_filepath, '/_plots/_validation/compareSimVersions_LGA_2019_2022.png'),
         width=1200,height=600)





#######################################################################################################################################
# process simulation data into same state as dhis2 data were originally used to create archetypes and calibrate seasonality
#######################################################################################################################################
plot_sim_processed_like_dhis2 = function(pfpr_case_all, pop_arch){
  sim_incidence_2018 = pfpr_case_all[pfpr_case_all$year == 2018,]
  mean_sim_incidence_2018 = sim_incidence_2018 %>% group_by(admin_name) %>%
    summarise(mean_year_incidence = mean(treatment_incidence_include_NMF))
  sim_incidence_2018 = merge(sim_incidence_2018, mean_sim_incidence_2018, by='admin_name', all.x=TRUE)
  sim_incidence_2018$rescaled_incidence_all_age = sim_incidence_2018$treatment_incidence_include_NMF / sim_incidence_2018$mean_year_incidence
  sim_incidence_2019 = pfpr_case_all[pfpr_case_all$year == 2019,]
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
  
  gg_sim = ggplot() + 
    geom_line(data=sim_incidence_2018_2019, aes(x=month, y=rescaled_incidence_all_age, color=admin_name, linetype=factor(year))) + 
    geom_line(data=sim_incidence_state_ave, aes(x=month, y=mean_rescaled_incidence_all_age), color='black', size=2) + 
    coord_cartesian(ylim = c(0,4)) +
    theme_bw()+
    theme(legend.position="none") + 
    ggtitle(pfpr_case_all$simset[1])+
    facet_wrap('adm1', nrow=5, scales='free')
  return(list(sim_incidence_2018_2019, gg_sim))
}

plot_output_v1 = plot_sim_processed_like_dhis2(pfpr_case_all=pfpr_case_all_v1, pop_arch=pop_arch)
sim_incidence_2018_2019_v1 = plot_output_v1[[1]]
plot_output_v1[[2]]
ggsave(paste0(version2_filepath, '/_plots/_validation/rescaled_sim_monthly_cases_state_2018_2019_average_19Version.png'), plot_output_v1[[2]], width=25, height=12)

plot_output_v2 = plot_sim_processed_like_dhis2(pfpr_case_all=pfpr_case_all_v2, pop_arch=pop_arch)
sim_incidence_2018_2019_v2 = plot_output_v2[[1]]
plot_output_v2[[2]]
ggsave(paste0(version2_filepath, '/_plots/_validation/rescaled_sim_monthly_cases_state_2018_2019_average_22Version.png'), plot_output_v2[[2]], width=25, height=12)



