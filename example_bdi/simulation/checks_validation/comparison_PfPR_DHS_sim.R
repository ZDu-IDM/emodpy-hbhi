 #################################################################################################################################################
# comparison_PfPR_DHS_sim.R
# HBHI - Burkina Faso
# contact: Monique Ambrose
# October 2020

# currently, we claim that our calibration and parameterization was successful because we can see that simulated DS trajectories often visually 
#    agree fairly well with survey/surveillance data. To look at this a bit more closely and to see how DS-specific this match is, create a 
#    series of plots that compare simulation and data, either for the same (matched) DS or for mismatched DS.
# main types of comparisons are:
#    1) comparison of PfPR seasonality (plot the DHS PfPR according to month of survey, along with corresponding simulation values)
#    2) scatter plot comparisions of PfPR between observed DHS and corresponding simulation values
#    3) change between survey years (percent or absolute change within a DS or admin1 area in the aggregated PfPR in different survey years)
#    4) histograms of PfPR values, differences, and likelihoods

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
library(hablar)

###################################################################
#   read in and format data and simulation output
###################################################################
 country_name = 'Burundi'
 if(country_name == 'Burundi'){
   hbhi_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/snt_2023'
   sim_filepath_2010 = paste0(hbhi_dir, '/simulation_output/simulations_to_present/BDI_toPresent_allInter_v6')
   ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
   dhs_years_list = list(c(2012, 2013), c(2016, 2017))  # list of years included in each DHS survey
 } else if(country_name == "Burkina"){
   box_filepath = 'C:/Users/mambrose/Box'
   hbhi_dir = paste0(box_filepath, '/hbhi_burkina')
   sim_filepath_2010 = paste0(hbhi_dir, '/simulation_output/2010_to_2020/_v19/BF 2010_2020 allInterventions')
   ds_pop_df_filename = file.path(hbhi_dir, 'burkina_DS_pop.csv')
 }

# - - - - - - - - - - - - - - - - #
# DHS PfPR data
# - - - - - - - - - - - - - - - - #
dhs_pfpr_fname = file.path(hbhi_dir, '/estimates_from_DHS/DHS_admin_monthly_microscopy.csv')
dhs_pfpr = fread(dhs_pfpr_fname)
dhs_pfpr$date = as.Date(paste0(dhs_pfpr$year,'-',dhs_pfpr$month,'-01'))
dhs_pfpr$p_test_mean = dhs_pfpr$num_pos/dhs_pfpr$num_tested

# - - - - - - - - - - - - - - - - #
# simulation output
# - - - - - - - - - - - - - - - - #
pfpr_case_u5_2010 = fread(paste0(sim_filepath_2010, '/U5_PfPR_ClinicalIncidence_severeTreatment.csv'))
pfpr_case_u5_2010[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
# mean values across runs
pfpr_case_u5_runMeans  <- pfpr_case_u5_2010 %>% group_by(date, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 

# - - - - - - - - - - - - - - - - - #
# archetype for each DS
# - - - - - - - - - - - - - - - - - #
# create data tables specifying which DS belong to which archetype (and the rep DS for that archetype) 
ds_pop_df = fread(ds_pop_df_filename)
ds_pop_df = ds_pop_df[,c('admin_name', 'pop_size', 'cluster_id','seasonality_archetype')]
pfpr_with_arch = merge(x=pfpr_case_u5_runMeans, y=ds_pop_df, by=c('admin_name'), all.x=TRUE)





###################################################################
# merge DHS and corresponding simulation values
###################################################################
# match the same DS from simulation and data
pfpr_matched = merge(x=dhs_pfpr, y=pfpr_with_arch, by=c('admin_name','date'), all.x=TRUE)

# PfPR: match date from sim and data but shuffle DS
admin_names = unique(pfpr_with_arch$admin_name)
admin_names_shuffled = admin_names[c(2:length(admin_names), 1)]
pfpr_with_arch_mis = pfpr_with_arch
pfpr_with_arch_mis$admin_name = sapply(pfpr_with_arch$admin_name, function(x) admin_names_shuffled[which(admin_names == x)])
pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_with_arch_mis, by=c('admin_name','date'), all.x=TRUE)

# relative and absolute differences - matched
pfpr_matched$error = (pfpr_matched$`PfPR U5` - pfpr_matched$p_test_mean) * -1
pfpr_matched$abs_error = abs(pfpr_matched$`PfPR U5` - pfpr_matched$p_test_mean)
pfpr_matched$rel_error = abs(pfpr_matched$`PfPR U5` - sapply(pfpr_matched$p_test_mean, max,0.0001))/sapply(pfpr_matched$p_test_mean, max,0.0001)
# relative and absolute differences -  mismatched
pfpr_mismatched$error = (pfpr_mismatched$`PfPR U5` - pfpr_mismatched$p_test_mean) * -1
pfpr_mismatched$abs_error = abs(pfpr_mismatched$`PfPR U5` - pfpr_mismatched$p_test_mean)
pfpr_mismatched$rel_error = abs(pfpr_mismatched$`PfPR U5` - sapply(pfpr_mismatched$p_test_mean, max,0.0001))/sapply(pfpr_mismatched$p_test_mean, max,0.0001)




#######################################################################
# calculate aggregated PfPR (to year and to admin1 level)
#######################################################################
# get annual averages for each DS
# matched DS
pfpr_matched$prod_dhs_pfpr_ss = pfpr_matched$p_test_mean * pfpr_matched$num_tested
pfpr_matched$prod_sim_pfpr_ss = pfpr_matched$`PfPR U5` * pfpr_matched$num_tested
pfpr_matched_annual = pfpr_matched[,c('admin_name','year.y', 'prod_dhs_pfpr_ss','num_tested', 'prod_sim_pfpr_ss', 'cluster_id')]  %>% group_by(year.y, admin_name, cluster_id) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_matched_annual$pfpr_dhs_mean = pfpr_matched_annual$prod_dhs_pfpr_ss/pfpr_matched_annual$num_tested
pfpr_matched_annual$pfpr_sim_mean = pfpr_matched_annual$prod_sim_pfpr_ss/pfpr_matched_annual$num_tested
# misatched DS
pfpr_mismatched$prod_dhs_pfpr_ss = pfpr_mismatched$p_test_mean * pfpr_mismatched$num_tested
pfpr_mismatched$prod_sim_pfpr_ss = pfpr_mismatched$`PfPR U5` * pfpr_mismatched$num_tested
pfpr_mismatched_annual = pfpr_mismatched[,c('admin_name','year.y', 'prod_dhs_pfpr_ss','num_tested', 'prod_sim_pfpr_ss', 'cluster_id')]  %>% group_by(year.y, admin_name, cluster_id) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_mismatched_annual$pfpr_dhs_mean = pfpr_mismatched_annual$prod_dhs_pfpr_ss/pfpr_mismatched_annual$num_tested
pfpr_mismatched_annual$pfpr_sim_mean = pfpr_mismatched_annual$prod_sim_pfpr_ss/pfpr_mismatched_annual$num_tested

# # get annual average for each admin 1 (instead of admin2=health district) - because DHS not powered at admin2
# pfpr_matched_annual_admin1 = pfpr_matched[,c('admin1','year.y', 'prod_dhs_pfpr_ss','num_tested', 'prod_sim_pfpr_ss')]  %>% group_by(year.y, admin1) %>%  
#   summarise_all(sum) %>% ungroup() 
# pfpr_matched_annual_admin1$pfpr_dhs_mean = pfpr_matched_annual_admin1$prod_dhs_pfpr_ss/pfpr_matched_annual_admin1$num_tested
# pfpr_matched_annual_admin1$pfpr_sim_mean = pfpr_matched_annual_admin1$prod_sim_pfpr_ss/pfpr_matched_annual_admin1$num_tested




###########################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                            plot comparisons
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###########################################################################

##################################################################
# compare seasonality patterns (plots of PfPR by month)
##################################################################
# save the mean and median absolute and relative differences for each month across all years and DS
median_dif_pfpr_monthly = rep(NA,12)
mean_abs_dif_pfpr_monthly = rep(NA,12)
median_abs_dif_pfpr_monthly = rep(NA,12)
mean_rel_dif_pfpr_monthly = rep(NA,12)
median_rel_dif_pfpr_monthly = rep(NA,12)
median_pfpr_dhs_monthly = rep(NA, 12)
median_pfpr_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_dif_pfpr_monthly[mm] = median(pfpr_matched$error[pfpr_matched$month.x == mm], na.rm=TRUE)
  mean_abs_dif_pfpr_monthly[mm] = mean(pfpr_matched$abs_error[pfpr_matched$month.x == mm], na.rm=TRUE)
  median_abs_dif_pfpr_monthly[mm] = median(pfpr_matched$abs_error[pfpr_matched$month.x == mm], na.rm=TRUE)
  mean_rel_dif_pfpr_monthly[mm] = mean(pfpr_matched$rel_error[pfpr_matched$month.x == mm], na.rm=TRUE)
  median_rel_dif_pfpr_monthly[mm] = median(pfpr_matched$rel_error[pfpr_matched$month.x == mm], na.rm=TRUE)
  median_pfpr_dhs_monthly[mm] = median(pfpr_matched$p_test_mean[pfpr_matched$month.x == mm], na.rm=TRUE)
  median_pfpr_sim_monthly[mm] = median(pfpr_matched$`PfPR U5`[pfpr_matched$month.x == mm], na.rm=TRUE)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# plot monthly values in DHS dataset and from corresponding times and locations in the simulation 
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
par(mfrow=c(1,1), mar=c(5,4,4,2))
set.seed(1); jitter = runif(length(pfpr_matched$month.x), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,1.1), type='b', bty='L', ylab='U5 PfPR', xlab='month')
points(pfpr_matched$month.x+jitter, pfpr_matched$p_test_mean, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
points(pfpr_matched$month.x+jitter, pfpr_matched$`PfPR U5`, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
lines(1:12, median_pfpr_dhs_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_pfpr_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=0.99, c('DHS','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
text(1:12, rep(1.1, 12), round(median_abs_dif_pfpr_monthly,2), col='grey')

## - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# plot error in each month
## - - - - - - - - - - - - - - - - - - - - - - - - - - ##
par(mfrow=c(1,1), mar=c(5,4,4,2))
set.seed(1); jitter = runif(length(pfpr_matched$month.x), min=-0.15, max=0.15)
plot(1:12, median_abs_dif_pfpr_monthly, type='l', bty='L', ylab='U5 PfPR or difference in PfPR', xlab='month', ylim=c(0,1))
points(pfpr_matched$month.x+jitter, pfpr_matched$p_test_mean, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
points(pfpr_matched$month.x+jitter, pfpr_matched$`PfPR U5`, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
lines(1:12, median_pfpr_dhs_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_pfpr_sim_monthly, col=rgb(0.83,0,0.1))
lines(1:12, median_dif_pfpr_monthly, col='salmon')
legend(x=0.7,y=0.99, c('DHS','simulation', 'median difference', 'median absolute difference'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1), 'salmon', 'black'), bty='n')

# plot(1:12, median_rel_dif_pfpr_monthly, type='b', ylim=c(0,1), bty='L', ylab='median relative difference between DHS and simulation PfPR', xlab='month')

# ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# # zoom in on July and plot with DS names, colored by archetype
# ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# july_pfpr_matched = pfpr_matched[which(pfpr_matched$month.x == 7),]
# plot(NA, xlim=c(0,1), ylim=c(0,1), xlab='simulation U5 PfPR', ylab='DHS U5 PfPR', bty='L', main='Comparison of simulation and DHS PfPR from July clusters')
# lines(c(0,1), c(0,1), col='grey')
# text(x=july_pfpr_matched$`PfPR U5`, y=july_pfpr_matched$p_test_mean, labels= july_pfpr_matched$admin_name, col=c('red','green','blue')[july_pfpr_matched$cluster_id], cex=0.8)



###################################################################
#   scatterplots of simulated and DHS PfPR for each DS
###################################################################

# set of scatterplots: four plots returned, all from the same scenario but from different years
#   include regression lines (lm weighted by survey size) and correlation values
create_PfPR_scatters = function(pfpr_df, x_col_name, x_lab, y_col_name, y_lab, dhs_years_list){
  plot_list = list()
  # matched DS
  plot_list[[1]] = ggplot(pfpr_df, aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(aes(size=num_tested, col=year.y), shape=20, alpha=0.5) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm, mapping = aes(weight = num_tested))+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  # look at each year separately
  for(yy in 1:length(dhs_years_list)){
    plot_list[[1 + yy]] = ggplot(pfpr_df[pfpr_df$year.y %in% dhs_years_list[[yy]],], aes_string(x=x_col_name, y=y_col_name)) +
      geom_point(aes(size=num_tested), shape=20, alpha=0.5) + 
      geom_abline(slope=1, intercept=c(0,0)) +
      geom_smooth(method=lm, mapping = aes(weight = num_tested))+
      stat_cor(method = "pearson", col='darkred') +
      scale_size(limits = c(1,300), range = c(1, 8)) + 
      xlim(0, 1) + 
      ylim(0, 1) +
      xlab(x_lab) + 
      ylab(y_lab)+
      theme_classic() +
      theme(legend.position = "none")
  }
 
  return(plot_list)
}


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# compare matched DS versus mismatched DS
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# plot point for each DHS survey cluster (separate points in each month in a DS)
p_match_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_matched, y_col_name="p_test_mean", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="DHS U5 PfPR", dhs_years_list=dhs_years_list)
p_mismatch_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_mismatched, y_col_name="p_test_mean", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="DHS U5 PfPR", dhs_years_list=dhs_years_list)
grid.arrange(grobs=c(p_match_DHS_plots, p_mismatch_DHS_plots), nrow=2)
ifelse(!dir.exists(paste0(sim_filepath_2010, '/_plots')), dir.create(paste0(sim_filepath_2010, '/_plots')), FALSE)
ggsave(file = paste0(sim_filepath_2010,'/_plots/scatter_PfPR_sim_DHS_match_mismatch.pdf'), arrangeGrob(grobs = c(p_match_DHS_plots, p_mismatch_DHS_plots), nrow=2), width=12, height=6)
ggsave(file = paste0(sim_filepath_2010,'/_plots/scatter_PfPR_sim_DHS_match_mismatch.png'), arrangeGrob(grobs = c(p_match_DHS_plots, p_mismatch_DHS_plots), nrow=2), width=12, height=6, units='in')


# annual weighted average
#   (take weighted average of all DHS (and matching simulation) PfPR values within a year rather than one point per month sampled)
p_match_annual_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_matched_annual, x_col_name="pfpr_sim_mean", x_lab="simulation U5 PfPR", y_col_name="pfpr_dhs_mean", y_lab="DHS U5 PfPR", dhs_years_list=dhs_years_list)
p_mismatch_annual_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_mismatched_annual, x_col_name="pfpr_sim_mean", x_lab="simulation U5 PfPR", y_col_name="pfpr_dhs_mean", y_lab="DHS U5 PfPR", dhs_years_list=dhs_years_list)
grid.arrange(grobs=c(p_match_annual_DHS_plots, p_mismatch_annual_DHS_plots), nrow=2)
ggsave(file = paste0(sim_filepath_2010,'/_plots/scatter_annual_PfPR_sim_DHS_match_mismatch.png'), arrangeGrob(grobs = c(p_match_annual_DHS_plots, p_mismatch_annual_DHS_plots), nrow=2), width=12, height=6, units='in')


# # zoom in on 2010 and plot with DS names, colored by archetype
# pfpr_matched_annual_2010 = pfpr_matched_annual[pfpr_matched_annual$year.y == 2010,]
# plot(NA, xlim=c(0,1), ylim=c(0,1), xlab='simulation U5 PfPR', ylab='DHS U5 PfPR', bty='L', main='Comparison of 2010 simulation and DHS (annual, weighted)')
# lines(c(0,1), c(0,1), col='grey')
# text(x=pfpr_matched_annual_2010$pfpr_sim_mean, y=pfpr_matched_annual_2010$pfpr_dhs_mean, labels= pfpr_matched_annual_2010$admin_name, col=c('red','green','blue')[pfpr_matched_annual_2010$cluster_id], cex=0.8)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# compare oberved DHS-simulation relationship with what would be expected if survey were done in simulated population
#   (assume local PfPR = DS simulated PfPR)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# what does this look like if we sample pretend DHS observations from simulation PfPR? essentially make up a dhs dataset from sampling the simulated population
pfpr_matched$sim_survey_U5_pos = rbinom(length(pfpr_matched$num_tested), size=pfpr_matched$num_tested, prob=pfpr_matched$`PfPR U5`)
pfpr_matched$sim_survey_U5_PfPR = pfpr_matched$sim_survey_U5_pos / pfpr_matched$num_tested

# plot point for each DHS survey cluster (separate points in each month in a DS)
p_match_sim_plots = create_PfPR_scatters(pfpr_df=pfpr_matched, y_col_name="sim_survey_U5_PfPR", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="simulation survey U5 PfPR", dhs_years_list=dhs_years_list)
grid.arrange(grobs=c(p_match_DHS_plots, p_match_sim_plots), nrow=2)
ggsave(file = paste0(sim_filepath_2010,'/_plots/scatter_PfPR_sim_simSurvey.png'), arrangeGrob(grobs = c(p_match_DHS_plots, p_match_sim_plots), nrow=2), width=12, height=6, units='in')



# annual weighted average
pfpr_matched$sim_survey_prod_dhs_pfpr_ss = pfpr_matched$sim_survey_U5_PfPR * pfpr_matched$num_tested
pfpr_matched_sim_survey_annual = pfpr_matched[,c('admin_name','year.y', 'sim_survey_prod_dhs_pfpr_ss','num_tested', 'prod_sim_pfpr_ss', 'cluster_id')]  %>% group_by(year.y, admin_name, cluster_id) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_matched_sim_survey_annual$pfpr_sim_survey_dhs_mean = pfpr_matched_sim_survey_annual$sim_survey_prod_dhs_pfpr_ss / pfpr_matched_sim_survey_annual$num_tested
pfpr_matched_sim_survey_annual$pfpr_sim_mean = pfpr_matched_sim_survey_annual$prod_sim_pfpr_ss / pfpr_matched_sim_survey_annual$num_tested

p_annual_sim_survey_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_matched_sim_survey_annual, x_col_name="pfpr_sim_mean", x_lab="simulation U5 PfPR", y_col_name="pfpr_sim_survey_dhs_mean", y_lab="simulation survey U5 PfPR", dhs_years_list=dhs_years_list)
grid.arrange(grobs=c(p_match_annual_DHS_plots, p_annual_sim_survey_DHS_plots), nrow=2)
ggsave(file = paste0(sim_filepath_2010,'/_plots/scatter_annual_PfPR_sim_simSurvey.png'), arrangeGrob(grobs = c(p_match_annual_DHS_plots, p_annual_sim_survey_DHS_plots), nrow=2), width=12, height=6, units='in')


# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # compare oberved DHS-simulation relationship with what would be expected if survey were done in simulated population
# #   (assume local PfPR is drawn from a distribution centered around the DS's simulated PfPR)
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # functions to draw local PfPR from several different distribution options 
# # Beta distribution with mean equal to DS's PfPR
# get_local_PfPR = function(mean, variance=0.01){
#   if(mean>0){
#     if(variance >= (mean*(1-mean))){
#       variance = mean * (1-mean) * 9/10
#     }
#     vv = mean * (1-mean) / variance
#     alpha = mean * vv
#     beta = (1-mean)*vv
#     local_PfPR = rbeta(1, shape1=alpha, shape2=beta)
#   } else{
#     local_PfPR = 0
#   }
#   return(local_PfPR)
# }
# # Normal distribution with mean equal to DS's PfPR
# get_local_PfPR2 = function(mean, variance=0.01){
#   if(mean>0){
#     local_PfPR=-9
#     while((local_PfPR<0) | (local_PfPR>1)){
#       local_PfPR = rnorm(1, mean=mean, sd=variance^(1/2))
#     }
#   } else{
#     local_PfPR = 0
#   }
#   return(local_PfPR)
# }
# # Gamma distribution with mean equal to DS's PfPR
# get_local_PfPR3 = function(mean, variance=0.01){
#   scale = variance/mean
#   shape = mean/scale
#   if(mean>0){
#     local_PfPR=-9
#     while((local_PfPR<0) | (local_PfPR>1)){
#       local_PfPR = rgamma(1, shape=shape, scale=scale)
#     }
#   } else{
#     local_PfPR = 0
#   }
#   return(local_PfPR)
# }
# # Normal distribution with mean equal to DS's PfPR and a variance increasing as mean increases
# get_local_PfPR4 = function(mean, variance=0.01){
#   mod_var = (1-abs(mean-0.5)/.6) * variance
#   if(mean>0){
#     local_PfPR=-9
#     while((local_PfPR<0) | (local_PfPR>1)){
#       local_PfPR = rnorm(1, mean=mean, sd=mod_var^(1/2))
#     }
#   } else{
#     local_PfPR = 0
#   }
#   return(local_PfPR)
# }
# # # plot histogram of densities for different means and variances for a choice of sampling distribution
# # get_local_PfPR_func = get_local_PfPR4
# # mean_vals = c(0.001, 0.01, 0.075,0.2)
# # var_vals = c(0.01, 0.075, 0.2)
# # par(mfrow=c(length(var_vals), length(mean_vals)))
# # for(vv in 1:length(var_vals)){
# #   for(mm in 1:length(mean_vals)){
# #     mean_val = mean_vals[mm]
# #     var_cur = var_vals[vv]
# #     hist(sapply(rep(mean_val,100000), get_local_PfPR_func, variance=var_cur), main=paste0('true mean:', mean_val, '; var: ', var_cur, '; mean:', round(mean(sapply(rep(mean_val,100000), get_local_PfPR_func, variance=var_cur)),2)), breaks = seq(0,1,0.05))
# #   }
# # }
# 
# # what does this look like if we sample pretend DHS observations from simulation PfPR where local PfPR is assumed to be drawn from Beta? essentially make up a dhs dataset from sampling the simulated population
# variance = 0.125
# dist = 'N_adjV'
# if(dist == 'N'){
#   pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR2, variance = variance)
# } else if(dist == 'N_adjV'){
#   pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR4, variance = variance)
# } else{
#   pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR, variance = variance)
# }
# pfpr_matched$sim_survey2_U5_pos = rbinom(length(pfpr_matched$num_tested), size=pfpr_matched$num_tested, prob=pfpr_matched$local_sim_PfPR)
# pfpr_matched$sim_survey2_U5_PfPR = pfpr_matched$sim_survey2_U5_pos / pfpr_matched$num_tested
# 
# p_match_sim_plots = create_PfPR_scatters(pfpr_df=pfpr_matched, y_col_name="sim_survey2_U5_PfPR", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="simulation survey U5 PfPR")
# gg = grid.arrange(p_match_DHS_plots[[1]], p_match_DHS_plots[[2]], p_match_DHS_plots[[3]], p_match_DHS_plots[[4]],
#                   p_match_sim_plots[[1]], p_match_sim_plots[[2]], p_match_sim_plots[[3]], p_match_sim_plots[[4]],
#                   nrow = 2)
# ggsave(paste0(hbhi_dir, '/project_notes/figures/scatter_PfPR_sim_simSurvey2_',dist,'var',round(variance*1000),'.png'), gg, width=12, height=6)
# 
# 
# 
# ###############################################################################################
# #   compare percent change and absolute change in sample PfPR between pairs of DHS years
# ###############################################################################################
# # function to create scatter plots comparisons of simulation versus DHS change in PfPR between 2010 and 2014, 2010 and 2017, and 2014 and 2017
# create_scatter_change_comparison = function(pfpr_df,
#                                             sim_col_10_14, sim_col_10_17, sim_col_14_17,
#                                             dhs_col_10_14, dhs_col_10_17, dhs_col_14_17,
#                                             xlab, ylab,
#                                             min_xylim, max_xylim,
#                                             include_lm=TRUE,
#                                             include_seasonArch = FALSE,
#                                             arch_colors=c('black','black','black'),
#                                             admin_name='admin_name'){
#   pp1 = ggplot(pfpr_df, aes_string(x=sim_col_10_14, y=dhs_col_10_14)) +
#     geom_point(aes_string(group=admin_name, size='min_ss_10_14'), shape=20) +
#     ylim(min_xylim, max_xylim) +
#     xlim(min_xylim, max_xylim) +
#     xlab(xlab) +
#     ylab(ylab)+
#     ggtitle('between 2010 and 2014') +
#     theme_classic() +
#     theme(legend.position = "none")
#   pp2 = ggplot(pfpr_df, aes_string(x=sim_col_10_17, y=dhs_col_10_17)) +
#     geom_point(aes_string(group=admin_name, size='min_ss_10_17'), shape=20) +
#     ylim(min_xylim, max_xylim) +
#     xlim(min_xylim, max_xylim) +
#     xlab(xlab) +
#     ylab(ylab)+
#     ggtitle('between 2010 and 2017') +
#     theme_classic() +
#     theme(legend.position = "none")
#   pp3 = ggplot(pfpr_df, aes_string(x=sim_col_14_17, y=dhs_col_14_17)) +
#     geom_point(aes_string(group=admin_name, size='min_ss_14_17',), shape=20) +
#     ylim(min_xylim, max_xylim) +
#     xlim(min_xylim, max_xylim) +
#     xlab(xlab) +
#     ylab(ylab)+
#     ggtitle('between 2014 and 2017') +
#     theme_classic() +
#     theme(legend.position = "none")
#   if(include_seasonArch){
#     pp1 = pp1 +
#       geom_point(aes(size=min_ss_10_14, col=as.factor(cluster_id)), shape=20) +
#       scale_color_manual(values=arch_colors)
#     pp2 = pp2 +
#       geom_point(aes(size=min_ss_10_17, col=as.factor(cluster_id)), shape=20) +
#       scale_color_manual(values=arch_colors)
#     pp3 = pp3 +
#       geom_point(aes(size=min_ss_14_17, col=as.factor(cluster_id)), shape=20) +
#       scale_color_manual(values=arch_colors)
#   }
#   if(include_lm){
#     pp1 = pp1 +
#       geom_smooth(method=lm, mapping = aes(weight = min_ss_10_14))+
#       stat_cor(method = "pearson", col='darkred')
#     pp2 = pp2 +
#       geom_smooth(method=lm, mapping = aes(weight = min_ss_10_17))+
#       stat_cor(method = "pearson", col='darkred')
#     pp3 = pp3 +
#       geom_smooth(method=lm, mapping = aes(weight = min_ss_14_17))+
#       stat_cor(method = "pearson", col='darkred')
#   }
#   gg = grid.arrange(pp1, pp2, pp3, nrow = 1)
#   return(gg)
# }
# 
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # calculate changes between years for DHS and simulation
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # get percent change and raw change for dhs
# pfpr_matched_annual_wide_dhs = dcast(pfpr_matched_annual, admin_name + cluster_id ~ year.y, value.var='pfpr_dhs_mean')
# # move 2017 values to 2016 column
# pfpr_matched_annual_wide_dhs$`2016`[is.na(pfpr_matched_annual_wide_dhs$`2016`)] = pfpr_matched_annual_wide_dhs$`2017`[is.na(pfpr_matched_annual_wide_dhs$`2016`)]
# pfpr_matched_annual_wide_dhs=pfpr_matched_annual_wide_dhs[,-which(colnames(pfpr_matched_annual_wide_dhs) %in% c('2013', '2017'))]
# colnames(pfpr_matched_annual_wide_dhs) = c('admin_name','cluster_id','dhs_2012','dhs_2016','dhs_2017')
# pfpr_matched_annual_wide_dhs$dhs_rel_2010_2014 = (pfpr_matched_annual_wide_dhs$dhs_2014-pfpr_matched_annual_wide_dhs$dhs_2010)/pfpr_matched_annual_wide_dhs$dhs_2010*100
# pfpr_matched_annual_wide_dhs$dhs_rel_2010_2017 = (pfpr_matched_annual_wide_dhs$dhs_2017-pfpr_matched_annual_wide_dhs$dhs_2010)/pfpr_matched_annual_wide_dhs$dhs_2010*100
# pfpr_matched_annual_wide_dhs$dhs_rel_2014_2017 = (pfpr_matched_annual_wide_dhs$dhs_2017-pfpr_matched_annual_wide_dhs$dhs_2014)/pfpr_matched_annual_wide_dhs$dhs_2014*100
# pfpr_matched_annual_wide_dhs$dhs_abs_2010_2014 = (pfpr_matched_annual_wide_dhs$dhs_2014-pfpr_matched_annual_wide_dhs$dhs_2010)
# pfpr_matched_annual_wide_dhs$dhs_abs_2010_2017 = (pfpr_matched_annual_wide_dhs$dhs_2017-pfpr_matched_annual_wide_dhs$dhs_2010)
# pfpr_matched_annual_wide_dhs$dhs_abs_2014_2017 = (pfpr_matched_annual_wide_dhs$dhs_2017-pfpr_matched_annual_wide_dhs$dhs_2014)
# 
# # get percent change and raw change for simulation
# pfpr_matched_annual_wide_sim = dcast(pfpr_matched_annual, admin_name + cluster_id ~ year.y, value.var='pfpr_sim_mean')
# # move 2018 values to 2017 column
# pfpr_matched_annual_wide_sim$`2017`[is.na(pfpr_matched_annual_wide_sim$`2017`)] = pfpr_matched_annual_wide_sim$`2018`[is.na(pfpr_matched_annual_wide_sim$`2017`)]
# pfpr_matched_annual_wide_sim=pfpr_matched_annual_wide_sim[,-5]
# colnames(pfpr_matched_annual_wide_sim) = c('admin_name','cluster_id','sim_2010','sim_2014','sim_2017')
# pfpr_matched_annual_wide_sim$sim_rel_2010_2014 = (pfpr_matched_annual_wide_sim$sim_2014-pfpr_matched_annual_wide_sim$sim_2010)/pfpr_matched_annual_wide_sim$sim_2010*100
# pfpr_matched_annual_wide_sim$sim_rel_2010_2017 = (pfpr_matched_annual_wide_sim$sim_2017-pfpr_matched_annual_wide_sim$sim_2010)/pfpr_matched_annual_wide_sim$sim_2010*100
# pfpr_matched_annual_wide_sim$sim_rel_2014_2017 = (pfpr_matched_annual_wide_sim$sim_2017-pfpr_matched_annual_wide_sim$sim_2014)/pfpr_matched_annual_wide_sim$sim_2014*100
# pfpr_matched_annual_wide_sim$sim_abs_2010_2014 = (pfpr_matched_annual_wide_sim$sim_2014-pfpr_matched_annual_wide_sim$sim_2010)
# pfpr_matched_annual_wide_sim$sim_abs_2010_2017 = (pfpr_matched_annual_wide_sim$sim_2017-pfpr_matched_annual_wide_sim$sim_2010)
# pfpr_matched_annual_wide_sim$sim_abs_2014_2017 = (pfpr_matched_annual_wide_sim$sim_2017-pfpr_matched_annual_wide_sim$sim_2014)
# 
# # get DHS survey sizes (take minimum survey size between the two compared years)
# pfpr_matched_annual_wide_size = dcast(pfpr_matched_annual, admin_name + cluster_id ~ year.y, value.var='num_tested')
# # move 2018 values to 2017 column
# pfpr_matched_annual_wide_size$`2017`[is.na(pfpr_matched_annual_wide_size$`2017`)] = pfpr_matched_annual_wide_size$`2018`[is.na(pfpr_matched_annual_wide_size$`2017`)]
# pfpr_matched_annual_wide_size=pfpr_matched_annual_wide_size[,-5]
# colnames(pfpr_matched_annual_wide_size) = c('admin_name','cluster_id','survey_size_2010','survey_size_2014','survey_size_2017')
# pfpr_matched_annual_wide = merge(pfpr_matched_annual_wide_dhs, pfpr_matched_annual_wide_sim, by=c('admin_name', 'cluster_id'))
# pfpr_matched_annual_wide = merge(pfpr_matched_annual_wide, pfpr_matched_annual_wide_size, by=c('admin_name', 'cluster_id'))
# # get minimum survey size between the two years
# pfpr_matched_annual_wide$min_ss_10_14 = pmin(pfpr_matched_annual_wide$survey_size_2010, pfpr_matched_annual_wide$survey_size_2014)
# pfpr_matched_annual_wide$min_ss_10_17 = pmin(pfpr_matched_annual_wide$survey_size_2010, pfpr_matched_annual_wide$survey_size_2017)
# pfpr_matched_annual_wide$min_ss_14_17 = pmin(pfpr_matched_annual_wide$survey_size_2014, pfpr_matched_annual_wide$survey_size_2017)
# 
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # scatter plot of PfPR percent change for simulation versus dhs
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# min_val = min(s(pfpr_matched_annual_wide[,c('dhs_rel_2010_2014', 'dhs_rel_2010_2017', 'dhs_rel_2014_2017', 'sim_rel_2010_2014', 'sim_rel_2010_2017', 'sim_rel_2014_2017')]), na.rm=TRUE)
# max_val = 110
# gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide,
#                                       sim_col_10_14='sim_rel_2010_2014', sim_col_10_17='sim_rel_2010_2017', sim_col_14_17='sim_rel_2014_2017',
#                                       dhs_col_10_14='dhs_rel_2010_2014', dhs_col_10_17='dhs_rel_2010_2017', dhs_col_14_17='dhs_rel_2014_2017',
#                                       xlab='percent change in simulation U5 PfPR', ylab='percent change in DHS U5 PfPR',
#                                       min_xylim=min_val, max_xylim=max_val,
#                                       include_lm=FALSE,
#                                       include_seasonArch = FALSE)
# ggsave(paste0(hbhi_dir, '/project_notes/figures/scatter_percent_change_PfPR_sim.png'), gg, width=12, height=4)
# 
# gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide,
#                                  sim_col_10_14='sim_rel_2010_2014', sim_col_10_17='sim_rel_2010_2017', sim_col_14_17='sim_rel_2014_2017',
#                                  dhs_col_10_14='dhs_rel_2010_2014', dhs_col_10_17='dhs_rel_2010_2017', dhs_col_14_17='dhs_rel_2014_2017',
#                                  xlab='percent change in simulation U5 PfPR', ylab='percent change in DHS U5 PfPR',
#                                  min_xylim=min_val, max_xylim=max_val,
#                                  include_lm=TRUE,
#                                  include_seasonArch = FALSE)
# ggsave(paste0(hbhi_dir, '/project_notes/figures/scatter_percent_change_PfPR_sim_withLM.png'), gg, width=12, height=4)
# 
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # scatter plot of PfPR change for simulation versus dhs
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# min_val = -0.75
# max_val = 0.5
# arch_colors = c(rgb(60,87,170, maxColorValue=255),rgb(114, 193, 80, maxColorValue=255), rgb(242, 68,72, maxColorValue=255))
# gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide,
#                                       sim_col_10_14='sim_abs_2010_2014', sim_col_10_17='sim_abs_2010_2017', sim_col_14_17='sim_abs_2014_2017',
#                                       dhs_col_10_14='dhs_abs_2010_2014', dhs_col_10_17='dhs_abs_2010_2017', dhs_col_14_17='dhs_abs_2014_2017',
#                                       xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
#                                       min_xylim=min_val, max_xylim=max_val,
#                                       include_lm=FALSE,
#                                       include_seasonArch = FALSE)
# ggsave(paste0(hbhi_dir, '/project_notes/figures/scatter_change_PfPR_sim.png'), gg, width=12, height=4)
# 
# gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide,
#                                       sim_col_10_14='sim_abs_2010_2014', sim_col_10_17='sim_abs_2010_2017', sim_col_14_17='sim_abs_2014_2017',
#                                       dhs_col_10_14='dhs_abs_2010_2014', dhs_col_10_17='dhs_abs_2010_2017', dhs_col_14_17='dhs_abs_2014_2017',
#                                       xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
#                                       min_xylim=min_val, max_xylim=max_val,
#                                       include_lm=TRUE,
#                                       include_seasonArch = FALSE)
# ggsave(paste0(hbhi_dir, '/project_notes/figures/scatter_change_PfPR_sim_withLM.png'), gg, width=12, height=4)
# 
# gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide,
#                                       sim_col_10_14='sim_abs_2010_2014', sim_col_10_17='sim_abs_2010_2017', sim_col_14_17='sim_abs_2014_2017',
#                                       dhs_col_10_14='dhs_abs_2010_2014', dhs_col_10_17='dhs_abs_2010_2017', dhs_col_14_17='dhs_abs_2014_2017',
#                                       xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
#                                       min_xylim=min_val, max_xylim=max_val,
#                                       include_lm=TRUE,
#                                       include_seasonArch = TRUE,
#                                       arch_colors=arch_colors)
# ggsave(paste0(hbhi_dir, '/project_notes/figures/scatter_change_PfPR_sim_withLM_colorArch.png'), gg, width=12, height=4)
# 
# 
# # plot DS names
# par(mfrow=c(1,3))
# plot(NA, ylim=c(min_val, max_val), xlim=c(min_val,max_val), bty='L', xlab='Simulation PfPR change', ylab='DHS PfPR change', main='Change from 2010 to 2014')
# lines(c(-1,1), c(-1,1), col='grey')
# text(x=pfpr_matched_annual_wide$sim_abs_2010_2014, y=pfpr_matched_annual_wide$dhs_abs_2010_2014, labels= pfpr_matched_annual_wide$admin_name, col=c('red','green','blue')[pfpr_matched_annual_wide$cluster_id], cex=0.8)
# plot(NA, ylim=c(min_val, max_val), xlim=c(min_val,max_val), bty='L', xlab='Simulation PfPR change', ylab='DHS PfPR change', main='Change from 2010 to 2017')
# lines(c(-1,1), c(-1,1), col='grey')
# text(x=pfpr_matched_annual_wide$sim_abs_2010_2017, y=pfpr_matched_annual_wide$dhs_abs_2010_2017, labels= pfpr_matched_annual_wide$admin_name, col=c('red','green','blue')[pfpr_matched_annual_wide$cluster_id], cex=0.8)
# plot(NA, ylim=c(min_val, max_val), xlim=c(min_val,max_val), bty='L', xlab='Simulation PfPR change', ylab='DHS PfPR change', main='Change from 2014 to 2017')
# lines(c(-1,1), c(-1,1), col='grey')
# text(x=pfpr_matched_annual_wide$sim_abs_2014_2017, y=pfpr_matched_annual_wide$dhs_abs_2014_2017, labels= pfpr_matched_annual_wide$admin_name, col=c('red','green','blue')[pfpr_matched_annual_wide$cluster_id], cex=0.8)
# # 
# 
# 
# # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # # scatter plot of annual PfPR change for simulation versus DHS, aggregated to admin1 level
# # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # 
# # # get later year pfpr relative to earlier year for dhs
# # pfpr_matched_annual_admin1_wide_dhs = dcast(pfpr_matched_annual_admin1, admin1 ~ year.y, value.var='pfpr_dhs_mean')
# # # move 2018 values to 2017 column
# # pfpr_matched_annual_admin1_wide_dhs$`2017`[is.na(pfpr_matched_annual_admin1_wide_dhs$`2017`)] = pfpr_matched_annual_admin1_wide_dhs$`2018`[is.na(pfpr_matched_annual_admin1_wide_dhs$`2017`)]
# # pfpr_matched_annual_admin1_wide_dhs=pfpr_matched_annual_admin1_wide_dhs[,-5]
# # colnames(pfpr_matched_annual_admin1_wide_dhs) = c('admin1','dhs_2010','dhs_2014','dhs_2017')
# # pfpr_matched_annual_admin1_wide_dhs$dhs_abs_2010_2014 = (pfpr_matched_annual_admin1_wide_dhs$dhs_2014-pfpr_matched_annual_admin1_wide_dhs$dhs_2010)
# # pfpr_matched_annual_admin1_wide_dhs$dhs_abs_2010_2017 = (pfpr_matched_annual_admin1_wide_dhs$dhs_2017-pfpr_matched_annual_admin1_wide_dhs$dhs_2010)
# # pfpr_matched_annual_admin1_wide_dhs$dhs_abs_2014_2017 = (pfpr_matched_annual_admin1_wide_dhs$dhs_2017-pfpr_matched_annual_admin1_wide_dhs$dhs_2014)
# # 
# # # get later year pfpr relative to earlier year for simulation
# # pfpr_matched_annual_admin1_wide_sim = dcast(pfpr_matched_annual_admin1, admin1 ~ year.y, value.var='pfpr_sim_mean')
# # # move 2018 values to 2017 column
# # pfpr_matched_annual_admin1_wide_sim$`2017`[is.na(pfpr_matched_annual_admin1_wide_sim$`2017`)] = pfpr_matched_annual_admin1_wide_sim$`2018`[is.na(pfpr_matched_annual_admin1_wide_sim$`2017`)]
# # pfpr_matched_annual_admin1_wide_sim=pfpr_matched_annual_admin1_wide_sim[,-5]
# # colnames(pfpr_matched_annual_admin1_wide_sim) = c('admin1','sim_2010','sim_2014','sim_2017')
# # pfpr_matched_annual_admin1_wide_sim$sim_abs_2010_2014 = (pfpr_matched_annual_admin1_wide_sim$sim_2014-pfpr_matched_annual_admin1_wide_sim$sim_2010)
# # pfpr_matched_annual_admin1_wide_sim$sim_abs_2010_2017 = (pfpr_matched_annual_admin1_wide_sim$sim_2017-pfpr_matched_annual_admin1_wide_sim$sim_2010)
# # pfpr_matched_annual_admin1_wide_sim$sim_abs_2014_2017 = (pfpr_matched_annual_admin1_wide_sim$sim_2017-pfpr_matched_annual_admin1_wide_sim$sim_2014)
# # 
# # # get DHS survey sizes
# # pfpr_matched_annual_admin1_wide_size = dcast(pfpr_matched_annual_admin1, admin1 ~ year.y, value.var='num_tested')
# # # move 2018 values to 2017 column
# # pfpr_matched_annual_admin1_wide_size$`2017`[is.na(pfpr_matched_annual_admin1_wide_size$`2017`)] = pfpr_matched_annual_admin1_wide_size$`2018`[is.na(pfpr_matched_annual_admin1_wide_size$`2017`)]
# # pfpr_matched_annual_admin1_wide_size=pfpr_matched_annual_admin1_wide_size[,-5]
# # colnames(pfpr_matched_annual_admin1_wide_size) = c('admin1','survey_size_2010','survey_size_2014','survey_size_2017')
# # pfpr_matched_annual_admin1_wide = merge(pfpr_matched_annual_admin1_wide_dhs, pfpr_matched_annual_admin1_wide_sim, by=c('admin1'))
# # pfpr_matched_annual_admin1_wide = merge(pfpr_matched_annual_admin1_wide, pfpr_matched_annual_admin1_wide_size, by=c('admin1'))
# # 
# # # get minimum survey size between the two years
# # pfpr_matched_annual_admin1_wide$min_ss_10_14 = pmin(pfpr_matched_annual_admin1_wide$survey_size_2010, pfpr_matched_annual_admin1_wide$survey_size_2014)
# # pfpr_matched_annual_admin1_wide$min_ss_10_17 = pmin(pfpr_matched_annual_admin1_wide$survey_size_2010, pfpr_matched_annual_admin1_wide$survey_size_2017)
# # pfpr_matched_annual_admin1_wide$min_ss_14_17 = pmin(pfpr_matched_annual_admin1_wide$survey_size_2014, pfpr_matched_annual_admin1_wide$survey_size_2017)
# # 
# # 
# # # scatter plot of year-to-year absolute differences for simulation versus dhs
# # min_val = -0.6
# # max_val = 0.23
# # gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_admin1_wide, 
# #                                       sim_col_10_14='sim_abs_2010_2014', sim_col_10_17='sim_abs_2010_2017', sim_col_14_17='sim_abs_2014_2017',
# #                                       dhs_col_10_14='dhs_abs_2010_2014', dhs_col_10_17='dhs_abs_2010_2017', dhs_col_14_17='dhs_abs_2014_2017',
# #                                       xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
# #                                       min_xylim=min_val, max_xylim=max_val,
# #                                       include_lm=FALSE,
# #                                       include_seasonArch = FALSE,
# #                                       admin_name='admin1')
# # ggsave(paste0(hbhi_dir, '/project_notes/figures/scatter_change_PfPR_sim_admin1.png'), gg, width=12, height=4)
# # gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_admin1_wide, 
# #                                       sim_col_10_14='sim_abs_2010_2014', sim_col_10_17='sim_abs_2010_2017', sim_col_14_17='sim_abs_2014_2017',
# #                                       dhs_col_10_14='dhs_abs_2010_2014', dhs_col_10_17='dhs_abs_2010_2017', dhs_col_14_17='dhs_abs_2014_2017',
# #                                       xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
# #                                       min_xylim=min_val, max_xylim=max_val,
# #                                       include_lm=TRUE,
# #                                       include_seasonArch = FALSE,
# #                                       admin_name='admin1')
# # ggsave(paste0(hbhi_dir, '/project_notes/figures/scatter_change_PfPR_sim_admin1_wLine.png'), gg, width=12, height=4)
# 
# 
# 
# 
# 
# #################################################################
# # histograms of PfPR differences and likelihoods
# #################################################################
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# #   histogram of PfPR differences between matched and mismatched DS from sim/data, across years
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# pdf(paste0(hbhi_dir, '/project_notes/figures/hist_PfPR_abs_dif_sim_DHS_mismatchDS.pdf'), width=6, height=6)
# #  ----  all years  ----  #
# # relative difference for matching DS
# match_rel_dif = abs(pfpr_matched$p_test_mean - pfpr_matched$`PfPR U5`)
# 
# # mismatched DS (multiple mis-matches)
# admin_names = unique(pfpr_case_u5_runMeans$admin_name)
# admin_names_shuffled = admin_names[c(2:length(admin_names), 1)]
# mismatch_rel_dif = c()
# for(ii in 1:20){
#   pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
#   pfpr_case_u5_runMeans_mis$admin_name = sapply(pfpr_case_u5_runMeans$admin_name, function(x) admin_names[c((ii+1):length(admin_names), 1:ii)][which(admin_names == x)])
#   pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('admin_name','date'), all.x=TRUE)
#   mismatch_rel_dif = c(mismatch_rel_dif, abs(pfpr_mismatched$p_test_mean - pfpr_mismatched$`PfPR U5`))
# }
# # compare histograms
# par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
# hist(match_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='',  xlab='difference')
# mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
# title(main='matched DS, all years', line=0.5)
# hist(mismatch_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
# title(main='mismatched DS, all years', line=0.5)
# mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
# 
# #  ----  2010, 2014, 2017  ----  #
# for(year in c(2010, 2014, 2017, 2018)){
#   # relative difference for matching DS
#   match_rel_dif = abs(pfpr_matched[pfpr_matched$year.y==year,]$p_test_mean - pfpr_matched[pfpr_matched$year.y==year,]$`PfPR U5`)
#   
#   # mismatched DS (multiple mis-matches)
#   mismatch_rel_dif = c()
#   for(ii in 1:20){
#     pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
#     pfpr_case_u5_runMeans_mis$admin_name = sapply(pfpr_case_u5_runMeans$admin_name, function(x) admin_names[c((ii+1):length(admin_names), 1:ii)][which(admin_names == x)])
#     pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('admin_name','date'), all.x=TRUE)
#     mismatch_rel_dif = c(mismatch_rel_dif, abs(pfpr_mismatched[pfpr_mismatched$year.y==year,]$p_test_mean - pfpr_mismatched[pfpr_mismatched$year.y==year,]$`PfPR U5`))
#   }
#   # compare histograms
#   par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
#   hist(match_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
#   title(main=paste0('matched DS, ', year), line=0.5)
#   mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
#   hist(mismatch_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
#   title(main=paste0('mismatched DS, ', year), line=0.5)
#   mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
# }
# dev.off()
# par(mfrow=c(1,1))
# 
# 
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# #   histogram of PfPR differences between matched and mismatched year from sim/data, across years
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# pdf(paste0(hbhi_dir, '/project_notes/figures/hist_PfPR_abs_dif_sim_DHS_mismatchYears.pdf'), width=6, height=6)
# year_true_vect = c(2010, 2010, 2014, 2014, 2017, 2017)
# mismatch_year_vect = c(2014, 2017, 2010, 2017, 2010, 2014)
# for(ii in 1:length(year_true_vect)){
#   year_true = year_true_vect[ii]
#   mismatch_year = mismatch_year_vect[ii]
#   match_rel_dif = abs(pfpr_matched[pfpr_matched$year.y==year_true,]$p_test_mean - pfpr_matched[pfpr_matched$year.y==year_true,]$`PfPR U5`)
#   
#   # mismatched years
#   pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
#   pfpr_case_u5_runMeans_mis$year[pfpr_case_u5_runMeans_mis$year == year_true] = NA
#   pfpr_case_u5_runMeans_mis$year[pfpr_case_u5_runMeans_mis$year == mismatch_year] = year_true
#   pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('admin_name','date'), all.x=TRUE)
#   mismatch_rel_dif = abs(pfpr_mismatched[pfpr_mismatched$year.y==year_true,]$p_test_mean - pfpr_mismatched[pfpr_mismatched$year.y==year_true,]$`PfPR U5`)
#   
#   # compare histograms
#   par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
#   hist(match_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
#   title(main=paste0('matched: sim and data from ', year_true), line=0.5)
#   mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
#   hist(mismatch_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
#   title(main=paste0('mismatched: data from ', year_true, ', sim from ' , mismatch_year), line=0.5)
#   mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
# }
# dev.off()
# par(mfrow=c(1,1))
# 
# 
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # how extreme is matched likelihood compared to when DS are scrambled at random?
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 
# # probability of drawing observed DHS positives if simulation PfPR is true probability
# pfpr_matched$num_U5_pos = pfpr_matched$p_test_mean * pfpr_matched$num_tested
# match_lik = dbinom(round(pfpr_matched$num_U5_pos), size=pfpr_matched$num_tested, prob=pfpr_matched$`PfPR U5`)
# match_loglik = sum(log(match_lik))
# # mismatched DS (multiple mis-matches)
# admin_names = unique(pfpr_case_u5_runMeans$admin_name)
# num_reps = 300
# 
# gg_list = list()
# gg_index = 1
# #  ----  2010, 2014, 2017  ----  #
# for(year in list(c(2010), c(2014), c(2017, 2018))){
#   print(paste('working on year', year))
#   # likelihood of drawing observed DHS positives if simulation PfPR is true probability
#   match_lik = dbinom(round(pfpr_matched[pfpr_matched$year.y %in% year,]$num_U5_pos), size=pfpr_matched[pfpr_matched$year.y %in% year,]$num_tested, prob=pfpr_matched[pfpr_matched$year.y %in% year,]$`PfPR U5`)
#   match_loglik = sum(log(match_lik))
#   
#   # mismatched DS (multiple mis-matches)
#   mismatch_loglik = rep(NA,num_reps)
#   for(ii in 1:num_reps){
#     pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
#     admin_names_shuffled = sample(admin_names, size=length(admin_names), replace=FALSE)
#     pfpr_case_u5_runMeans_mis$admin_name = sapply(pfpr_case_u5_runMeans$admin_name, function(x) admin_names_shuffled[which(admin_names == x)])
#     pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('admin_name','date'), all.x=TRUE)
#     pfpr_mismatched$num_U5_pos = pfpr_mismatched$p_test_mean * pfpr_mismatched$num_tested
#     mismatch_lik_cur = dbinom(round(pfpr_mismatched[pfpr_mismatched$year.y %in% year,]$num_U5_pos), size=pfpr_mismatched[pfpr_mismatched$year.y %in% year,]$num_tested, prob=pfpr_mismatched[pfpr_mismatched$year.y %in% year,]$`PfPR U5`)
#     mismatch_loglik[ii] = sum(log(mismatch_lik_cur))
#   }
#   # histogram
#   gg_list[[gg_index]] = ggplot(as.data.frame(mismatch_loglik), aes(mismatch_loglik))+
#     geom_histogram(aes(y=..density..), bins=40, fill='grey', color='darkgrey')+
#     geom_vline(xintercept=match_loglik, color='red', size=2) +
#     xlab('ln(probability of DHS observations)')+
#     # xlim(min(mismatch_loglik, na.rm=TRUE), max(c(mismatch_loglik,match_loglik), na.rm=TRUE)) + 
#     theme_classic()
#   gg_index = gg_index+1
# }
# gg = grid.arrange(gg_list[[1]], gg_list[[2]], gg_list[[3]], nrow = 1)
# # ggsave(paste0(hbhi_dir, '/project_notes/figures/hist_logprob_DHS_PfPR_match_mismatch.png'), gg, width=7.2, height=2)
# ggsave(paste0(hbhi_dir, '/project_notes/figures/hist_logprob_DHS_PfPR_match_mismatch_2014_17_18.png'), gg_list[[1]], width=3.6, height=3)
# 
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# # how does matched likelihood compare to when we draw PfPR directly from simulation?
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# pdf(paste0(hbhi_dir, '/project_notes/figures/hist_loglik_simSurvey_PfPR.pdf'), width=5, height=5.5)
# 
# #  ----  all years  ----  #
# # likelihood of drawing observed DHS positives if simulation PfPR is true probability
# pfpr_matched$num_U5_pos = pfpr_matched$p_test_mean * pfpr_matched$num_tested
# match_lik = dbinom(round(pfpr_matched$num_U5_pos), size=pfpr_matched$num_tested, prob=pfpr_matched$`PfPR U5`)
# match_loglik = sum(log(match_lik))
# num_reps = 300
# sim_survey_loglik = rep(NA,num_reps)
# for(ii in 1:num_reps){
#   # what does this look like if we sample pretend DHS observations from simulation PfPR? essentially make up a dhs dataset from sampling the simulated population
#   pfpr_matched$sim_survey_U5_pos = rbinom(length(pfpr_matched$num_tested), size=pfpr_matched$num_tested, prob=pfpr_matched$`PfPR U5`)
#   # likelihood of drawing observed surveyed positives if simulation PfPR is true probability
#   sim_survey_lik = dbinom(round(pfpr_matched$sim_survey_U5_pos), size=pfpr_matched$num_tested, prob=pfpr_matched$`PfPR U5`)
#   sim_survey_loglik[ii] = sum(log(sim_survey_lik))
# }
# # histogram
# par(mfrow=c(1,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
# hist(sim_survey_loglik, breaks=seq(min(c(sim_survey_loglik[is.finite(sim_survey_loglik)], match_loglik)), max(c(match_loglik, sim_survey_loglik)), length.out=100), freq=FALSE, main='', xlab='loglikelihood of sim survey PfPR observations')
# # title(main='sim survey PfPR likelihoods, all years', line=0.5)
# abline(v=match_loglik, col='red', lwd=3)
# mtext('DHS survey', line=-0.85, col='red', side=2)
# dev.off()
# 
# # draw from simulation where local PfPR is drawn from Beta with mean equal to DS PfPR
# variance = 0.01
# pdf(paste0(hbhi_dir, '/project_notes/figures/hist_loglik_simSurvey2_PfPR_var',round(variance*1000),'.pdf'), width=5, height=5.5)
# # png(paste0(hbhi_dir, '/project_notes/figures/hist_loglik_simSurvey2_PfPR_var',round(variance*1000),'.png'), width=720, height=480)
# 
# #  ----  all years  ----  #
# # likelihood of drawing observed DHS positives if simulation PfPR is true probability
# pfpr_matched$num_U5_pos = pfpr_matched$p_test_mean * pfpr_matched$num_tested
# match_lik = dbinom(round(pfpr_matched$num_U5_pos), size=pfpr_matched$num_tested, prob=pfpr_matched$`PfPR U5`)
# match_loglik = sum(log(match_lik))
# 
# num_reps = 300
# sim_survey_loglik = rep(NA,num_reps)
# for(ii in 1:num_reps){
#   # what does this look like if we sample pretend DHS observations from simulation PfPR? essentially make up a dhs dataset from sampling the simulated population
#   pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR, variance = variance)
#   pfpr_matched$sim_survey2_U5_pos = rbinom(length(pfpr_matched$num_tested), size=pfpr_matched$num_tested, prob=pfpr_matched$local_sim_PfPR)
#   
#   # likelihood of drawing observed surveyed positives if simulation PfPR is true probability
#   sim_survey_lik = dbinom(round(pfpr_matched$sim_survey2_U5_pos), size=pfpr_matched$num_tested, prob=pfpr_matched$`PfPR U5`)
#   sim_survey_loglik[ii] = sum(log(sim_survey_lik))
# }
# 
# # histogram
# par(mfrow=c(1,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
# hist(sim_survey_loglik, breaks=seq(min(c(sim_survey_loglik[is.finite(sim_survey_loglik)], match_loglik)), max(c(match_loglik, sim_survey_loglik)), length.out=100), freq=FALSE, main='', xlab='loglikelihood of sim survey PfPR observations')
# # title(main='sim survey PfPR likelihoods, all years', line=0.5)
# abline(v=match_loglik, col='red', lwd=3)
# # mtext('DHS survey', line=-0.85, col='red', side=2)
# dev.off()
# 
# 
