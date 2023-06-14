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
# library(hablar)

###################################################################
#   read in and format data and simulation output
###################################################################

script_dir = 'C:/Users/moniqueam/Documents/malaria-nga-snt22'
orig_hbhi_filepath = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration/hbhi_nigeria'
base_hbhi_filepath = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration/hbhi_nigeria/snt_2022'
dhs_pfpr_filepath = paste0(base_hbhi_filepath, '/estimates_from_DHS/DHS_monthly_microscopy_adminLevelDataOnly.csv')
# dhs_pfpr_filepath = paste0(orig_hbhi_filepath, '/DS DHS estimates/U5/pfpr/DHS.data_surveyd')
pop_arch_filepath = paste0(base_hbhi_filepath, '/admin_pop_archetype.csv')
sim_filepath_2010 = paste0(base_hbhi_filepath, '/simulation_output/simulations_to_present/NGA_toPresent_allInter_v9')

source(paste0(script_dir,'/standardize_admin_names.R'))

if(!dir.exists(paste0(sim_filepath_2010, '/_plots'))) dir.create(paste0(sim_filepath_2010, '/_plots'))
if(!dir.exists(paste0(sim_filepath_2010, '/_plots/_validation'))) dir.create(paste0(sim_filepath_2010, '/_plots/_validation'))



# - - - - - - - - - - - - - - - - #
# LGA populations and archetypes
# - - - - - - - - - - - - - - - - #
pop_arch = fread(pop_arch_filepath)

# - - - - - - - - - - - - - - - - #
# DHS PfPR data
# - - - - - - - - - - - - - - - - #
dhs_pfpr = read.csv(dhs_pfpr_filepath)
colnames(dhs_pfpr)[colnames(dhs_pfpr) == 'num_tested'] = 'num_U5_sampled'
dhs_pfpr$p_test_mean = dhs_pfpr$num_pos / dhs_pfpr$num_U5_sampled
dhs_pfpr$date = as.Date(paste0(dhs_pfpr$year,"-", dhs_pfpr$month,"-01"),format="%Y-%m-%d")


# dhs_pfpr_2010_fname = file.path(dhs_pfpr_filepath, 'DS10_pfpr_ym_micro.csv')
# dhs_pfpr_2015_fname = file.path(dhs_pfpr_filepath, 'DS15_pfpr_ym_micro.csv')
# dhs_pfpr_2018_fname = file.path(dhs_pfpr_filepath, 'DS18_pfpr_ym_micro.csv')
# dhs_pfpr_2010 = fread(dhs_pfpr_2010_fname)
# dhs_pfpr_2015 = fread(dhs_pfpr_2015_fname)
# dhs_pfpr_2018 = fread(dhs_pfpr_2018_fname)
# # # reformat 2018 to match other years
# # dhs_pfpr_2018$time2= sapply(dhs_pfpr_2018$time2, function(x) paste0(strsplit(x, '-')[[1]][2],"-",strsplit(x, '-')[[1]][1]))
# # colnames(dhs_pfpr_2018)[colnames(dhs_pfpr_2018)=='mean'] = 'p_test_mean'
# # colnames(dhs_pfpr_2018)[colnames(dhs_pfpr_2018)=='sd'] = 'p_test_sd'
# # colnames(dhs_pfpr_2018)[colnames(dhs_pfpr_2018)=='se'] = 'p_test_std.error'
# # combine dhs from multiple years
# dhs_pfpr = rbind(dhs_pfpr_2010, dhs_pfpr_2015, dhs_pfpr_2018)
# colnames(dhs_pfpr)[colnames(dhs_pfpr) == 'PfPr'] = 'p_test_mean'
# dhs_pfpr = dhs_pfpr[!is.na(dhs_pfpr$p_test_mean),]
# dhs_pfpr$month = sapply(dhs_pfpr$time2, function(x) strsplit(x, '-')[[1]][1])
# dhs_pfpr$year = sapply(dhs_pfpr$time2, function(x) strsplit(x, '-')[[1]][2])
# dhs_pfpr[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
# colnames(dhs_pfpr)[colnames(dhs_pfpr) == 'Number of Kids'] = 'num_U5_sampled'
# dhs_pfpr$admin_name = toupper(dhs_pfpr$LGA)
# make sure the admin names are consistent
dhs_pfpr = standardize_admin_names_in_df(target_names_df=pop_arch, origin_names_df=dhs_pfpr, target_names_col='admin_name', origin_names_col='admin_name')


# - - - - - - - - - - - - - - - - #
# simulation output
# - - - - - - - - - - - - - - - - #
pfpr_case_all_2010 = fread(paste0(sim_filepath_2010, '/All_Age_monthly_Cases.csv'))
pfpr_case_all_2010[,date:=as.Date(date)]
pfpr_case_all_2010$year = lubridate::year(pfpr_case_all_2010$date)
pfpr_case_all_2010$month = lubridate::month(pfpr_case_all_2010$date)
pfpr_case_u5_2010 = fread(paste0(sim_filepath_2010, '/U5_PfPR_ClinicalIncidence_severeTreatment.csv'))
pfpr_case_u5_2010[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
# mean values across runs
pfpr_case_all_runMeans  <- pfpr_case_all_2010 %>% group_by(date, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 
pfpr_case_u5_runMeans  <- pfpr_case_u5_2010 %>% group_by(date, admin_name) %>%  
  summarise_all(mean) %>% ungroup() 
# pfpr_case_u5_runMeans$admin_name = toupper(pfpr_case_u5_runMeans$admin_name)
# make sure the admin names are consistent
pfpr_case_u5_runMeans = standardize_admin_names_in_df(target_names_df=pop_arch, origin_names_df=pfpr_case_u5_runMeans, target_names_col='admin_name', origin_names_col='admin_name')

# - - - - - - - - - - - - - - - - - #
# archetype and admin 1 for each DS
# - - - - - - - - - - - - - - - - - #
# create data tables specifying which DS belong to which archetype (and the rep DS for that archetype) and admin1

# add column with admin1 of each DS
admin1DS = fread(pop_arch_filepath)
admin1DS = admin1DS[,c('admin_name', 'State', 'Archetype')]
pfpr_case_u5_runMeans = merge(x=pfpr_case_u5_runMeans, y=admin1DS, by=c('admin_name'), all.x=TRUE)

# add columns with archetype of each DS
# get seasonality archetype identity for each DS
pfpr_case_u5_runMeans$archID = NA
season_arch = unique(admin1DS$Archetype)
for (aa in 1:length(season_arch)){
  ds_in_arch = unique(admin1DS$admin_name[admin1DS$Archetype == season_arch[aa]])
  pfpr_case_u5_runMeans$archID[pfpr_case_u5_runMeans$admin_name %in% ds_in_arch] = aa
}




###################################################################
# merge DHS and corresponding simulation values
###################################################################
# match the same DS from simulation and data
pfpr_matched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans, by=c('admin_name','date'), all.x=TRUE)

# PfPR: match date from sim and data but shuffle DS
LGAs = unique(pfpr_case_u5_runMeans$admin_name)
LGAs_shuffled = LGAs[c(2:length(LGAs), 1)]
pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
pfpr_case_u5_runMeans_mis$admin_name = sapply(pfpr_case_u5_runMeans$admin_name, function(x) LGAs_shuffled[which(LGAs == x)])
pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('admin_name','date'), all.x=TRUE)

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
pfpr_matched$prod_dhs_pfpr_ss = pfpr_matched$p_test_mean * pfpr_matched$num_U5_sampled
pfpr_matched$prod_sim_pfpr_ss = pfpr_matched$`PfPR U5` * pfpr_matched$num_U5_sampled
pfpr_matched_annual = pfpr_matched[,c('admin_name','year.y', 'prod_dhs_pfpr_ss','num_U5_sampled', 'prod_sim_pfpr_ss', 'archID')]  %>% group_by(year.y, admin_name, archID) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_matched_annual$pfpr_dhs_mean = pfpr_matched_annual$prod_dhs_pfpr_ss/pfpr_matched_annual$num_U5_sampled
pfpr_matched_annual$pfpr_sim_mean = pfpr_matched_annual$prod_sim_pfpr_ss/pfpr_matched_annual$num_U5_sampled
# misatched DS
pfpr_mismatched$prod_dhs_pfpr_ss = pfpr_mismatched$p_test_mean * pfpr_mismatched$num_U5_sampled
pfpr_mismatched$prod_sim_pfpr_ss = pfpr_mismatched$`PfPR U5` * pfpr_mismatched$num_U5_sampled
pfpr_mismatched_annual = pfpr_mismatched[,c('admin_name','year.y', 'prod_dhs_pfpr_ss','num_U5_sampled', 'prod_sim_pfpr_ss', 'archID')]  %>% group_by(year.y, admin_name, archID) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_mismatched_annual$pfpr_dhs_mean = pfpr_mismatched_annual$prod_dhs_pfpr_ss/pfpr_mismatched_annual$num_U5_sampled
pfpr_mismatched_annual$pfpr_sim_mean = pfpr_mismatched_annual$prod_sim_pfpr_ss/pfpr_mismatched_annual$num_U5_sampled

# get annual average for each admin 1 (instead of admin2=health district) - because DHS not powered at admin2
pfpr_matched_annual_admin1 = pfpr_matched[,c('State','year.y', 'prod_dhs_pfpr_ss','num_U5_sampled', 'prod_sim_pfpr_ss')]  %>% group_by(year.y, State) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_matched_annual_admin1$pfpr_dhs_mean = pfpr_matched_annual_admin1$prod_dhs_pfpr_ss/pfpr_matched_annual_admin1$num_U5_sampled
pfpr_matched_annual_admin1$pfpr_sim_mean = pfpr_matched_annual_admin1$prod_sim_pfpr_ss/pfpr_matched_annual_admin1$num_U5_sampled

# get annual average for each Archetype (instead of admin2=health district) - because DHS not powered at admin2
pfpr_matched_annual_arch = pfpr_matched[,c('year.y', 'prod_dhs_pfpr_ss','num_U5_sampled', 'prod_sim_pfpr_ss', 'archID')]  %>% group_by(year.y, archID) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_matched_annual_arch$pfpr_dhs_mean = pfpr_matched_annual_arch$prod_dhs_pfpr_ss/pfpr_matched_annual_arch$num_U5_sampled
pfpr_matched_annual_arch$pfpr_sim_mean = pfpr_matched_annual_arch$prod_sim_pfpr_ss/pfpr_matched_annual_arch$num_U5_sampled



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
pfpr_matched$month.x = as.numeric(pfpr_matched$month.x)
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
# text(x=july_pfpr_matched$`PfPR U5`, y=july_pfpr_matched$p_test_mean, labels= july_pfpr_matched$admin_name, col=c('red','green','blue')[july_pfpr_matched$archID], cex=0.8)



###################################################################
#   scatterplots of simulated and DHS PfPR for each DS
###################################################################

# set of scatterplots: four plots returned, all from the same scenario but from different years
#   include regression lines (lm weighted by survey size) and correlation values
create_PfPR_scatters = function(pfpr_df, x_col_name, x_lab, y_col_name, y_lab){
  # matched DS
  p_all = ggplot(pfpr_df, aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(aes(size=num_U5_sampled, col=year.y), shape=20, alpha=0.5) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm, mapping = aes(weight = num_U5_sampled))+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  # look at each year separately
  p_2010 = ggplot(pfpr_df[pfpr_df$year.y==2010,], aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(aes(size=num_U5_sampled), shape=20, alpha=0.5) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm, mapping = aes(weight = num_U5_sampled))+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  p_2015 = ggplot(pfpr_df[pfpr_df$year.y==2015,], aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(aes(size=num_U5_sampled), shape=20, alpha=0.5) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm, mapping = aes(weight = num_U5_sampled))+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  p_2018 = ggplot(pfpr_df[pfpr_df$year.y%in%c(2018),], aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(aes(size=num_U5_sampled), shape=20, alpha=0.5) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm, mapping = aes(weight = num_U5_sampled))+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  p_2021 = ggplot(pfpr_df[pfpr_df$year.y%in%c(2021),], aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(aes(size=num_U5_sampled), shape=20, alpha=0.5) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm, mapping = aes(weight = num_U5_sampled))+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  return(list(p_all, p_2010, p_2015, p_2018, p_2021))
}


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# compare matched DS versus mismatched DS
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# plot point for each DHS survey cluster (separate points in each month in a DS)
p_match_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_matched, y_col_name="p_test_mean", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="DHS U5 PfPR")
p_mismatch_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_mismatched, y_col_name="p_test_mean", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="DHS U5 PfPR")
gg = grid.arrange(p_match_DHS_plots[[1]], p_match_DHS_plots[[2]], p_match_DHS_plots[[3]], p_match_DHS_plots[[4]], p_match_DHS_plots[[5]],
                  p_mismatch_DHS_plots[[1]], p_mismatch_DHS_plots[[2]], p_mismatch_DHS_plots[[3]], p_mismatch_DHS_plots[[4]],  p_mismatch_DHS_plots[[5]],
                  nrow = 2)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_PfPR_sim_DHS.pdf'), gg, width=20, height=10)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_PfPR_sim_DHS.png'), gg, width=12, height=6)


# annual weighted average
#   (take weighted average of all DHS (and matching simulation) PfPR values within a year rather than one point per month sampled)
p_match_annual_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_matched_annual, x_col_name="pfpr_sim_mean", x_lab="simulation U5 PfPR", y_col_name="pfpr_dhs_mean", y_lab="DHS U5 PfPR")
p_mismatch_annual_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_mismatched_annual, x_col_name="pfpr_sim_mean", x_lab="simulation U5 PfPR", y_col_name="pfpr_dhs_mean", y_lab="DHS U5 PfPR")
gg = grid.arrange(p_match_annual_DHS_plots[[1]], p_match_annual_DHS_plots[[2]], p_match_annual_DHS_plots[[3]], p_match_annual_DHS_plots[[4]], p_match_annual_DHS_plots[[5]],
                  p_mismatch_annual_DHS_plots[[1]], p_mismatch_annual_DHS_plots[[2]], p_mismatch_annual_DHS_plots[[3]], p_mismatch_annual_DHS_plots[[4]], p_mismatch_annual_DHS_plots[[5]],
                  nrow = 2)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_annual_PfPR_sim_DHS.pdf'), gg, width=12, height=6)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_annual_PfPR_sim_DHS.png'), gg, width=12, height=6)


# zoom in on 2010 and plot with DS names, colored by archetype
pfpr_matched_annual_2010 = pfpr_matched_annual[pfpr_matched_annual$year.y == 2010,]
plot(NA, xlim=c(0,1), ylim=c(0,1), xlab='simulation U5 PfPR', ylab='DHS U5 PfPR', bty='L', main='Comparison of 2010 simulation and DHS (annual, weighted)')
lines(c(0,1), c(0,1), col='grey')
text(x=pfpr_matched_annual_2010$pfpr_sim_mean, y=pfpr_matched_annual_2010$pfpr_dhs_mean, labels= pfpr_matched_annual_2010$admin_name, col=rainbow(length(unique(pfpr_matched_annual_2010$archID)))[pfpr_matched_annual_2010$archID], cex=0.8)

pfpr_matched_2010 = pfpr_matched[pfpr_matched$year.y == 2010,]
plot(NA, xlim=c(0,1), ylim=c(0,1), xlab='simulation U5 PfPR', ylab='DHS U5 PfPR', bty='L', main='Comparison of 2010 simulation and DHS')
lines(c(0,1), c(0,1), col='grey')
text(x=pfpr_matched_2010$`PfPR U5`, y=pfpr_matched_2010$p_test_mean, labels= pfpr_matched_2010$admin_name, col=rainbow(length(unique(pfpr_matched_2010$archID)))[pfpr_matched_2010$archID], cex=0.8)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# compare observed DHS-simulation relationship with what would be expected if survey were done in simulated population
#   (assume local PfPR = DS simulated PfPR)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# what does this look like if we sample pretend DHS observations from simulation PfPR? essentially make up a dhs dataset from sampling the simulated population
pfpr_matched$sim_survey_U5_pos = rbinom(length(pfpr_matched$num_U5_sampled), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
pfpr_matched$sim_survey_U5_PfPR = pfpr_matched$sim_survey_U5_pos / pfpr_matched$num_U5_sampled

# plot point for each DHS survey cluster (separate points in each month in a DS)
p_match_sim_plots = create_PfPR_scatters(pfpr_df=pfpr_matched, y_col_name="sim_survey_U5_PfPR", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="simulation survey U5 PfPR")
gg = grid.arrange(p_match_DHS_plots[[1]], p_match_DHS_plots[[2]], p_match_DHS_plots[[3]], p_match_DHS_plots[[4]], p_match_DHS_plots[[5]],
                  p_match_sim_plots[[1]], p_match_sim_plots[[2]], p_match_sim_plots[[3]], p_match_sim_plots[[4]], p_match_sim_plots[[5]],
                  nrow = 2)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_PfPR_sim_simSurvey.png'), gg, width=12, height=6)


# annual weighted average
pfpr_matched$sim_survey_prod_dhs_pfpr_ss = pfpr_matched$sim_survey_U5_PfPR * pfpr_matched$num_U5_sampled
pfpr_matched_sim_survey_annual = pfpr_matched[,c('admin_name','year.y', 'sim_survey_prod_dhs_pfpr_ss','num_U5_sampled', 'prod_sim_pfpr_ss', 'archID')]  %>% group_by(year.y, admin_name, archID) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_matched_sim_survey_annual$pfpr_sim_survey_dhs_mean = pfpr_matched_sim_survey_annual$sim_survey_prod_dhs_pfpr_ss / pfpr_matched_sim_survey_annual$num_U5_sampled
pfpr_matched_sim_survey_annual$pfpr_sim_mean = pfpr_matched_sim_survey_annual$prod_sim_pfpr_ss / pfpr_matched_sim_survey_annual$num_U5_sampled

p_annual_sim_survey_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_matched_sim_survey_annual, x_col_name="pfpr_sim_mean", x_lab="simulation U5 PfPR", y_col_name="pfpr_sim_survey_dhs_mean", y_lab="simulation survey U5 PfPR")
gg = grid.arrange(p_match_annual_DHS_plots[[1]], p_match_annual_DHS_plots[[2]], p_match_annual_DHS_plots[[3]], p_match_annual_DHS_plots[[4]], p_match_annual_DHS_plots[[5]],
                  p_annual_sim_survey_DHS_plots[[1]], p_annual_sim_survey_DHS_plots[[2]], p_annual_sim_survey_DHS_plots[[3]], p_annual_sim_survey_DHS_plots[[4]], p_annual_sim_survey_DHS_plots[[5]],
                  nrow = 2)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_annual_PfPR_sim_simSurvey.png'), gg, width=12, height=6)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# compare observed DHS-simulation relationship with what would be expected if survey were done in simulated population
#   (assume local PfPR is drawn from a distribution centered around the DS's simulated PfPR)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# functions to draw local PfPR from several different distribution options 
# Beta distribution with mean equal to DS's PfPR
get_local_PfPR = function(mean, variance=0.01){
  if(mean>0){
    if(variance >= (mean*(1-mean))){
      variance = mean * (1-mean) * 9/10
    }
    vv = mean * (1-mean) / variance
    alpha = mean * vv
    beta = (1-mean)*vv
    local_PfPR = rbeta(1, shape1=alpha, shape2=beta)
  } else{
    local_PfPR = 0
  }
  return(local_PfPR)
}
# Normal distribution with mean equal to DS's PfPR
get_local_PfPR2 = function(mean, variance=0.01){
  if(mean>0){
    local_PfPR=-9
    while((local_PfPR<0) | (local_PfPR>1)){
      local_PfPR = rnorm(1, mean=mean, sd=variance^(1/2))
    }
  } else{
    local_PfPR = 0
  }
  return(local_PfPR)
}
# Gamma distribution with mean equal to DS's PfPR
get_local_PfPR3 = function(mean, variance=0.01){
  scale = variance/mean
  shape = mean/scale
  if(mean>0){
    local_PfPR=-9
    while((local_PfPR<0) | (local_PfPR>1)){
      local_PfPR = rgamma(1, shape=shape, scale=scale)
    }
  } else{
    local_PfPR = 0
  }
  return(local_PfPR)
}
# Normal distribution with mean equal to DS's PfPR and a variance increasing as mean increases
get_local_PfPR4 = function(mean, variance=0.01){
  mod_var = (1-abs(mean-0.5)/.6) * variance
  if(is.na(mean)){
    local_PfPR = NA
  } else if(mean>0){
    local_PfPR=-9
    while((local_PfPR<0) | (local_PfPR>1)){
      local_PfPR = rnorm(1, mean=mean, sd=mod_var^(1/2))
    }
  } else{
    local_PfPR = 0
  }
  return(local_PfPR)
}
# # plot histogram of densities for different means and variances for a choice of sampling distribution
# get_local_PfPR_func = get_local_PfPR4
# mean_vals = c(0.001, 0.01, 0.075,0.2)
# var_vals = c(0.01, 0.075, 0.2)
# par(mfrow=c(length(var_vals), length(mean_vals)))
# for(vv in 1:length(var_vals)){
#   for(mm in 1:length(mean_vals)){
#     mean_val = mean_vals[mm]
#     var_cur = var_vals[vv]
#     hist(sapply(rep(mean_val,100000), get_local_PfPR_func, variance=var_cur), main=paste0('true mean:', mean_val, '; var: ', var_cur, '; mean:', round(mean(sapply(rep(mean_val,100000), get_local_PfPR_func, variance=var_cur)),2)), breaks = seq(0,1,0.05))
#   }
# }

# what does this look like if we sample pretend DHS observations from simulation PfPR where local PfPR is assumed to be drawn from Beta? essentially make up a dhs dataset from sampling the simulated population
variance = 0.125
dist = 'N_adjV'
if(dist == 'N'){
  pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR2, variance = variance)
} else if(dist == 'N_adjV'){
  pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR4, variance = variance)
} else{
  pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR, variance = variance)
}
pfpr_matched$sim_survey2_U5_pos = rbinom(length(pfpr_matched$num_U5_sampled), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$local_sim_PfPR)
pfpr_matched$sim_survey2_U5_PfPR = pfpr_matched$sim_survey2_U5_pos / pfpr_matched$num_U5_sampled

p_match_sim_plots = create_PfPR_scatters(pfpr_df=pfpr_matched, y_col_name="sim_survey2_U5_PfPR", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="simulation survey U5 PfPR")
gg = grid.arrange(p_match_DHS_plots[[1]], p_match_DHS_plots[[2]], p_match_DHS_plots[[3]], p_match_DHS_plots[[4]], p_match_DHS_plots[[5]],
                  p_match_sim_plots[[1]], p_match_sim_plots[[2]], p_match_sim_plots[[3]], p_match_sim_plots[[4]], p_match_sim_plots[[5]],
                  nrow = 2)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_PfPR_sim_simSurvey2_',dist,'var',round(variance*1000),'.png'), gg, width=12, height=6)



###############################################################################################
#   compare percent change and absolute change in sample PfPR between 2010, 2015, and 2018
###############################################################################################
# function to create scatter plots comparisons of simulation versus DHS change in PfPR between 2010 and 2015, 2010 and 2018, and 2015 and 2018
create_scatter_change_comparison = function(pfpr_df, 
                                            sim_col_10_14, sim_col_10_17, sim_col_14_17,
                                            dhs_col_10_14, dhs_col_10_17, dhs_col_14_17,
                                            xlab, ylab,
                                            min_xylim, max_xylim,
                                            include_lm=TRUE,
                                            include_seasonArch = FALSE,
                                            include_1_1_abline = FALSE,
                                            arch_colors=c('black','black','black'),
                                            admin_name='admin_name'){
  pp1 = ggplot(pfpr_df, aes_string(x=sim_col_10_14, y=dhs_col_10_14)) +
    geom_point(aes_string(group=admin_name, size='min_ss_10_14'), shape=20) + 
    ylim(min_xylim, max_xylim) + 
    xlim(min_xylim, max_xylim) + 
    xlab(xlab) + 
    ylab(ylab)+
    ggtitle('between 2010 and 2015') +
    theme_classic() +
    theme(legend.position = "none")
  pp2 = ggplot(pfpr_df, aes_string(x=sim_col_10_17, y=dhs_col_10_17)) +
    geom_point(aes_string(group=admin_name, size='min_ss_10_17'), shape=20) + 
    ylim(min_xylim, max_xylim) + 
    xlim(min_xylim, max_xylim) + 
    xlab(xlab) + 
    ylab(ylab)+
    ggtitle('between 2010 and 2018') +
    theme_classic() +
    theme(legend.position = "none")
  pp3 = ggplot(pfpr_df, aes_string(x=sim_col_14_17, y=dhs_col_14_17)) +
    geom_point(aes_string(group=admin_name, size='min_ss_14_17',), shape=20) + 
    ylim(min_xylim, max_xylim) + 
    xlim(min_xylim, max_xylim) + 
    xlab(xlab) + 
    ylab(ylab)+
    ggtitle('between 2015 and 2018') +
    theme_classic() +
    theme(legend.position = "none")
  if(include_seasonArch){
    pp1 = pp1 +
      geom_point(aes(size=min_ss_10_14, col=as.factor(archID)), shape=20) + 
      scale_color_manual(values=arch_colors)
    pp2 = pp2 +
      geom_point(aes(size=min_ss_10_17, col=as.factor(archID)), shape=20) + 
      scale_color_manual(values=arch_colors)
    pp3 = pp3 +
      geom_point(aes(size=min_ss_14_17, col=as.factor(archID)), shape=20) + 
      scale_color_manual(values=arch_colors)
  }
  if(include_lm){
    pp1 = pp1 + 
      geom_smooth(method=lm, mapping = aes(weight = min_ss_10_14))+
      stat_cor(method = "pearson", col='darkred')
    pp2 = pp2 + 
      geom_smooth(method=lm, mapping = aes(weight = min_ss_10_17))+
      stat_cor(method = "pearson", col='darkred')
    pp3 = pp3 + 
      geom_smooth(method=lm, mapping = aes(weight = min_ss_14_17))+
      stat_cor(method = "pearson", col='darkred')
  }
  if(include_1_1_abline){
    pp1 = pp1 + 
      geom_abline(intercept=0, slope=1, color='darkgrey')
    pp2 = pp2 + 
      geom_abline(intercept=0, slope=1, color='darkgrey')
    pp3 = pp3 + 
      geom_abline(intercept=0, slope=1, color='darkgrey')
  }
  gg = grid.arrange(pp1, pp2, pp3, nrow = 1)
  return(gg)
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# calculate changes between years for DHS and simulation
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# get percent change and raw change for dhs
pfpr_matched_annual_wide_dhs = dcast(pfpr_matched_annual, admin_name + archID ~ year.y, value.var='pfpr_dhs_mean')
colnames(pfpr_matched_annual_wide_dhs) = c('admin_name','archID','dhs_2010','dhs_2015','dhs_2018')
pfpr_matched_annual_wide_dhs$dhs_rel_2010_2015 = (pfpr_matched_annual_wide_dhs$dhs_2015-pfpr_matched_annual_wide_dhs$dhs_2010)/pfpr_matched_annual_wide_dhs$dhs_2010*100
pfpr_matched_annual_wide_dhs$dhs_rel_2010_2018 = (pfpr_matched_annual_wide_dhs$dhs_2018-pfpr_matched_annual_wide_dhs$dhs_2010)/pfpr_matched_annual_wide_dhs$dhs_2010*100
pfpr_matched_annual_wide_dhs$dhs_rel_2015_2018 = (pfpr_matched_annual_wide_dhs$dhs_2018-pfpr_matched_annual_wide_dhs$dhs_2015)/pfpr_matched_annual_wide_dhs$dhs_2015*100
pfpr_matched_annual_wide_dhs$dhs_abs_2010_2015 = (pfpr_matched_annual_wide_dhs$dhs_2015-pfpr_matched_annual_wide_dhs$dhs_2010)
pfpr_matched_annual_wide_dhs$dhs_abs_2010_2018 = (pfpr_matched_annual_wide_dhs$dhs_2018-pfpr_matched_annual_wide_dhs$dhs_2010)
pfpr_matched_annual_wide_dhs$dhs_abs_2015_2018 = (pfpr_matched_annual_wide_dhs$dhs_2018-pfpr_matched_annual_wide_dhs$dhs_2015)

# get percent change and raw change for simulation
pfpr_matched_annual_wide_sim = dcast(pfpr_matched_annual, admin_name + archID ~ year.y, value.var='pfpr_sim_mean')
colnames(pfpr_matched_annual_wide_sim) = c('admin_name','archID','sim_2010','sim_2015','sim_2018')
pfpr_matched_annual_wide_sim$sim_rel_2010_2015 = (pfpr_matched_annual_wide_sim$sim_2015-pfpr_matched_annual_wide_sim$sim_2010)/pfpr_matched_annual_wide_sim$sim_2010*100
pfpr_matched_annual_wide_sim$sim_rel_2010_2018 = (pfpr_matched_annual_wide_sim$sim_2018-pfpr_matched_annual_wide_sim$sim_2010)/pfpr_matched_annual_wide_sim$sim_2010*100
pfpr_matched_annual_wide_sim$sim_rel_2015_2018 = (pfpr_matched_annual_wide_sim$sim_2018-pfpr_matched_annual_wide_sim$sim_2015)/pfpr_matched_annual_wide_sim$sim_2015*100
pfpr_matched_annual_wide_sim$sim_abs_2010_2015 = (pfpr_matched_annual_wide_sim$sim_2015-pfpr_matched_annual_wide_sim$sim_2010)
pfpr_matched_annual_wide_sim$sim_abs_2010_2018 = (pfpr_matched_annual_wide_sim$sim_2018-pfpr_matched_annual_wide_sim$sim_2010)
pfpr_matched_annual_wide_sim$sim_abs_2015_2018 = (pfpr_matched_annual_wide_sim$sim_2018-pfpr_matched_annual_wide_sim$sim_2015)

# get DHS survey sizes (take minimum survey size between the two compared years)
pfpr_matched_annual_wide_size = dcast(pfpr_matched_annual, admin_name + archID ~ year.y, value.var='num_U5_sampled')
colnames(pfpr_matched_annual_wide_size) = c('admin_name','archID','survey_size_2010','survey_size_2015','survey_size_2018')
pfpr_matched_annual_wide = merge(pfpr_matched_annual_wide_dhs, pfpr_matched_annual_wide_sim, by=c('admin_name', 'archID'))
pfpr_matched_annual_wide = merge(pfpr_matched_annual_wide, pfpr_matched_annual_wide_size, by=c('admin_name', 'archID'))
# get minimum survey size between the two years
pfpr_matched_annual_wide$min_ss_10_14 = pmin(pfpr_matched_annual_wide$survey_size_2010, pfpr_matched_annual_wide$survey_size_2015)
pfpr_matched_annual_wide$min_ss_10_17 = pmin(pfpr_matched_annual_wide$survey_size_2010, pfpr_matched_annual_wide$survey_size_2018)
pfpr_matched_annual_wide$min_ss_14_17 = pmin(pfpr_matched_annual_wide$survey_size_2015, pfpr_matched_annual_wide$survey_size_2018)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# scatter plot of PfPR percent change for simulation versus dhs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
min_val = min((pfpr_matched_annual_wide[,c('dhs_rel_2010_2015', 'dhs_rel_2010_2018', 'dhs_rel_2015_2018', 'sim_rel_2010_2015', 'sim_rel_2010_2018', 'sim_rel_2015_2018')]), na.rm=TRUE)
max_val = 200
gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide, 
                                      sim_col_10_14='sim_rel_2010_2015', sim_col_10_17='sim_rel_2010_2018', sim_col_14_17='sim_rel_2015_2018',
                                      dhs_col_10_14='dhs_rel_2010_2015', dhs_col_10_17='dhs_rel_2010_2018', dhs_col_14_17='dhs_rel_2015_2018',
                                      xlab='percent change in simulation U5 PfPR', ylab='percent change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=FALSE,
                                      include_seasonArch = FALSE)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_percent_change_PfPR_sim.png'), gg, width=12, height=4)

gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide, 
                                 sim_col_10_14='sim_rel_2010_2015', sim_col_10_17='sim_rel_2010_2018', sim_col_14_17='sim_rel_2015_2018',
                                 dhs_col_10_14='dhs_rel_2010_2015', dhs_col_10_17='dhs_rel_2010_2018', dhs_col_14_17='dhs_rel_2015_2018',
                                 xlab='percent change in simulation U5 PfPR', ylab='percent change in DHS U5 PfPR',
                                 min_xylim=min_val, max_xylim=max_val,
                                 include_lm=TRUE,
                                 include_seasonArch = FALSE)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_percent_change_PfPR_sim_withLM.png'), gg, width=12, height=4)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# scatter plot of PfPR change for simulation versus dhs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
min_val = -0.75
max_val = 0.5
arch_colors = c(rgb(60,87,170, maxColorValue=255),rgb(114, 193, 80, maxColorValue=255), rgb(242, 68,72, maxColorValue=255), rgb(80,30,10, maxColorValue=255))
gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide, 
                                      sim_col_10_14='sim_abs_2010_2015', sim_col_10_17='sim_abs_2010_2018', sim_col_14_17='sim_abs_2015_2018',
                                      dhs_col_10_14='dhs_abs_2010_2015', dhs_col_10_17='dhs_abs_2010_2018', dhs_col_14_17='dhs_abs_2015_2018',
                                      xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=FALSE,
                                      include_seasonArch = FALSE)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_change_PfPR_sim.png'), gg, width=12, height=4)

gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide, 
                                      sim_col_10_14='sim_abs_2010_2015', sim_col_10_17='sim_abs_2010_2018', sim_col_14_17='sim_abs_2015_2018',
                                      dhs_col_10_14='dhs_abs_2010_2015', dhs_col_10_17='dhs_abs_2010_2018', dhs_col_14_17='dhs_abs_2015_2018',
                                      xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=TRUE,
                                      include_seasonArch = FALSE)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_change_PfPR_sim_withLM.png'), gg, width=12, height=4)

gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide, 
                                      sim_col_10_14='sim_abs_2010_2015', sim_col_10_17='sim_abs_2010_2018', sim_col_14_17='sim_abs_2015_2018',
                                      dhs_col_10_14='dhs_abs_2010_2015', dhs_col_10_17='dhs_abs_2010_2018', dhs_col_14_17='dhs_abs_2015_2018',
                                      xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=TRUE,
                                      include_seasonArch = TRUE,
                                      arch_colors=arch_colors)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_change_PfPR_sim_withLM_colorArch.png'), gg, width=12, height=4)


# plot DS names
par(mfrow=c(1,3))
plot(NA, ylim=c(min_val, max_val), xlim=c(min_val,max_val), bty='L', xlab='Simulation PfPR change', ylab='DHS PfPR change', main='Change from 2010 to 2015')
lines(c(-1,1), c(-1,1), col='grey')
text(x=pfpr_matched_annual_wide$sim_abs_2010_2015, y=pfpr_matched_annual_wide$dhs_abs_2010_2015, labels= pfpr_matched_annual_wide$admin_name, col=c('red','green','blue')[pfpr_matched_annual_wide$archID], cex=0.8)
plot(NA, ylim=c(min_val, max_val), xlim=c(min_val,max_val), bty='L', xlab='Simulation PfPR change', ylab='DHS PfPR change', main='Change from 2010 to 2018')
lines(c(-1,1), c(-1,1), col='grey')
text(x=pfpr_matched_annual_wide$sim_abs_2010_2018, y=pfpr_matched_annual_wide$dhs_abs_2010_2018, labels= pfpr_matched_annual_wide$admin_name, col=c('red','green','blue')[pfpr_matched_annual_wide$archID], cex=0.8)
plot(NA, ylim=c(min_val, max_val), xlim=c(min_val,max_val), bty='L', xlab='Simulation PfPR change', ylab='DHS PfPR change', main='Change from 2015 to 2018')
lines(c(-1,1), c(-1,1), col='grey')
text(x=pfpr_matched_annual_wide$sim_abs_2015_2018, y=pfpr_matched_annual_wide$dhs_abs_2015_2018, labels= pfpr_matched_annual_wide$admin_name, col=c('red','green','blue')[pfpr_matched_annual_wide$archID], cex=0.8)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# scatter plot of annual PfPR change for simulation versus DHS, aggregated to admin1 level
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
pfpr_matched_annual_admin1$admin1 = pfpr_matched_annual_admin1$State
# get later year pfpr relative to earlier year for dhs
pfpr_matched_annual_admin1_wide_dhs = dcast(pfpr_matched_annual_admin1, admin1 ~ year.y, value.var='pfpr_dhs_mean')
colnames(pfpr_matched_annual_admin1_wide_dhs) = c('admin1','dhs_2010','dhs_2015','dhs_2018', 'dhs_2021')
pfpr_matched_annual_admin1_wide_dhs$dhs_abs_2010_2015 = (pfpr_matched_annual_admin1_wide_dhs$dhs_2015-pfpr_matched_annual_admin1_wide_dhs$dhs_2010)
pfpr_matched_annual_admin1_wide_dhs$dhs_abs_2010_2018 = (pfpr_matched_annual_admin1_wide_dhs$dhs_2018-pfpr_matched_annual_admin1_wide_dhs$dhs_2010)
pfpr_matched_annual_admin1_wide_dhs$dhs_abs_2015_2018 = (pfpr_matched_annual_admin1_wide_dhs$dhs_2018-pfpr_matched_annual_admin1_wide_dhs$dhs_2015)

# get later year pfpr relative to earlier year for simulation
pfpr_matched_annual_admin1_wide_sim = dcast(pfpr_matched_annual_admin1, admin1 ~ year.y, value.var='pfpr_sim_mean')
colnames(pfpr_matched_annual_admin1_wide_sim) = c('admin1','sim_2010','sim_2015','sim_2018', 'sim_2021')
pfpr_matched_annual_admin1_wide_sim$sim_abs_2010_2015 = (pfpr_matched_annual_admin1_wide_sim$sim_2015-pfpr_matched_annual_admin1_wide_sim$sim_2010)
pfpr_matched_annual_admin1_wide_sim$sim_abs_2010_2018 = (pfpr_matched_annual_admin1_wide_sim$sim_2018-pfpr_matched_annual_admin1_wide_sim$sim_2010)
pfpr_matched_annual_admin1_wide_sim$sim_abs_2015_2018 = (pfpr_matched_annual_admin1_wide_sim$sim_2018-pfpr_matched_annual_admin1_wide_sim$sim_2015)

# get DHS survey sizes
pfpr_matched_annual_admin1_wide_size = dcast(pfpr_matched_annual_admin1, admin1 ~ year.y, value.var='num_U5_sampled')
colnames(pfpr_matched_annual_admin1_wide_size) = c('admin1','survey_size_2010','survey_size_2015','survey_size_2018', 'survey_size_2021')
pfpr_matched_annual_admin1_wide = merge(pfpr_matched_annual_admin1_wide_dhs, pfpr_matched_annual_admin1_wide_sim, by=c('admin1'))
pfpr_matched_annual_admin1_wide = merge(pfpr_matched_annual_admin1_wide, pfpr_matched_annual_admin1_wide_size, by=c('admin1'))

# get minimum survey size between the two years
pfpr_matched_annual_admin1_wide$min_ss_10_14 = pmin(pfpr_matched_annual_admin1_wide$survey_size_2010, pfpr_matched_annual_admin1_wide$survey_size_2015)
pfpr_matched_annual_admin1_wide$min_ss_10_17 = pmin(pfpr_matched_annual_admin1_wide$survey_size_2010, pfpr_matched_annual_admin1_wide$survey_size_2018)
pfpr_matched_annual_admin1_wide$min_ss_14_17 = pmin(pfpr_matched_annual_admin1_wide$survey_size_2015, pfpr_matched_annual_admin1_wide$survey_size_2018)


# scatter plot of year-to-year absolute differences for simulation versus dhs
min_val = -0.4
max_val = 0.23
gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_admin1_wide, 
                                      sim_col_10_14='sim_abs_2010_2015', sim_col_10_17='sim_abs_2010_2018', sim_col_14_17='sim_abs_2015_2018',
                                      dhs_col_10_14='dhs_abs_2010_2015', dhs_col_10_17='dhs_abs_2010_2018', dhs_col_14_17='dhs_abs_2015_2018',
                                      xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=FALSE,
                                      include_seasonArch = FALSE,
                                      include_1_1_abline=TRUE,
                                      admin_name='admin1')
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_change_PfPR_sim_admin1.png'), gg, width=12, height=4)
gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_admin1_wide, 
                                      sim_col_10_14='sim_abs_2010_2015', sim_col_10_17='sim_abs_2010_2018', sim_col_14_17='sim_abs_2015_2018',
                                      dhs_col_10_14='dhs_abs_2010_2015', dhs_col_10_17='dhs_abs_2010_2018', dhs_col_14_17='dhs_abs_2015_2018',
                                      xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=TRUE,
                                      include_seasonArch = FALSE,
                                      include_1_1_abline=TRUE,
                                      admin_name='admin1')
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/scatter_change_PfPR_sim_admin1_wLine.png'), gg, width=12, height=4)





#################################################################
# histograms of PfPR differences and likelihoods
#################################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#   histogram of PfPR differences between matched and mismatched DS from sim/data, across years
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
pdf(paste0(sim_filepath_2010, '/_plots/_validation/hist_PfPR_abs_dif_sim_DHS_mismatchDS.pdf'), width=6, height=6)
#  ----  all years  ----  #
# relative difference for matching DS
match_rel_dif = abs(pfpr_matched$p_test_mean - pfpr_matched$`PfPR U5`)

# mismatched DS (multiple mis-matches)
LGAs = unique(pfpr_case_u5_runMeans$admin_name)
LGAs_shuffled = LGAs[c(2:length(LGAs), 1)]
mismatch_rel_dif = c()
for(ii in 1:20){
  pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
  pfpr_case_u5_runMeans_mis$admin_name = sapply(pfpr_case_u5_runMeans$admin_name, function(x) LGAs[c((ii+1):length(LGAs), 1:ii)][which(LGAs == x)])
  pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('admin_name','date'), all.x=TRUE)
  mismatch_rel_dif = c(mismatch_rel_dif, abs(pfpr_mismatched$p_test_mean - pfpr_mismatched$`PfPR U5`))
}
# compare histograms
par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
hist(match_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='',  xlab='difference')
mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
title(main='matched DS, all years', line=0.5)
hist(mismatch_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
title(main='mismatched DS, all years', line=0.5)
mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)

#  ----  2010, 2015, 2018  ----  #
for(year in c(2010, 2015, 2018, 2018)){
  # relative difference for matching DS
  match_rel_dif = abs(pfpr_matched[pfpr_matched$year.y==year,]$p_test_mean - pfpr_matched[pfpr_matched$year.y==year,]$`PfPR U5`)
  
  # mismatched DS (multiple mis-matches)
  mismatch_rel_dif = c()
  for(ii in 1:20){
    pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
    pfpr_case_u5_runMeans_mis$admin_name = sapply(pfpr_case_u5_runMeans$admin_name, function(x) LGAs[c((ii+1):length(LGAs), 1:ii)][which(LGAs == x)])
    pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('admin_name','date'), all.x=TRUE)
    mismatch_rel_dif = c(mismatch_rel_dif, abs(pfpr_mismatched[pfpr_mismatched$year.y==year,]$p_test_mean - pfpr_mismatched[pfpr_mismatched$year.y==year,]$`PfPR U5`))
  }
  # compare histograms
  par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
  hist(match_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
  title(main=paste0('matched DS, ', year), line=0.5)
  mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
  hist(mismatch_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
  title(main=paste0('mismatched DS, ', year), line=0.5)
  mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
}
dev.off()
par(mfrow=c(1,1))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#   histogram of PfPR differences between matched and mismatched year from sim/data, across years
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
pdf(paste0(sim_filepath_2010, '/_plots/_validation/hist_PfPR_abs_dif_sim_DHS_mismatchYears.pdf'), width=6, height=6)
year_true_vect = c(2010, 2010, 2015, 2015, 2018, 2018)
mismatch_year_vect = c(2015, 2018, 2010, 2018, 2010, 2015)
for(ii in 1:length(year_true_vect)){
  year_true = year_true_vect[ii]
  mismatch_year = mismatch_year_vect[ii]
  match_rel_dif = abs(pfpr_matched[pfpr_matched$year.y==year_true,]$p_test_mean - pfpr_matched[pfpr_matched$year.y==year_true,]$`PfPR U5`)
  
  # mismatched years
  pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
  pfpr_case_u5_runMeans_mis$year[pfpr_case_u5_runMeans_mis$year == year_true] = NA
  pfpr_case_u5_runMeans_mis$year[pfpr_case_u5_runMeans_mis$year == mismatch_year] = year_true
  pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('admin_name','date'), all.x=TRUE)
  mismatch_rel_dif = abs(pfpr_mismatched[pfpr_mismatched$year.y==year_true,]$p_test_mean - pfpr_mismatched[pfpr_mismatched$year.y==year_true,]$`PfPR U5`)
  
  # compare histograms
  par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
  hist(match_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
  title(main=paste0('matched: sim and data from ', year_true), line=0.5)
  mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
  hist(mismatch_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
  title(main=paste0('mismatched: data from ', year_true, ', sim from ' , mismatch_year), line=0.5)
  mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
}
dev.off()
par(mfrow=c(1,1))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# how extreme is matched likelihood compared to when DS are scrambled at random?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# probability of drawing observed DHS positives if simulation PfPR is true probability
pfpr_matched$num_U5_pos = pfpr_matched$p_test_mean * pfpr_matched$num_U5_sampled
match_lik = dbinom(round(pfpr_matched$num_U5_pos), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
match_loglik = sum(log(match_lik))
# mismatched DS (multiple mis-matches)
LGAs = unique(pfpr_case_u5_runMeans$admin_name)
num_reps = 300

gg_list = list()
gg_index = 1
#  ----  2010, 2015, 2018  ----  #
for(year in list(c(2010), c(2015), c(2018, 2018))){
  print(paste('working on year', year))
  # likelihood of drawing observed DHS positives if simulation PfPR is true probability
  match_lik = dbinom(round(pfpr_matched[pfpr_matched$year.y %in% year,]$num_U5_pos), size=pfpr_matched[pfpr_matched$year.y %in% year,]$num_U5_sampled, prob=pfpr_matched[pfpr_matched$year.y %in% year,]$`PfPR U5`)
  match_loglik = sum(log(match_lik))
  
  # mismatched DS (multiple mis-matches)
  mismatch_loglik = rep(NA,num_reps)
  for(ii in 1:num_reps){
    pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
    LGAs_shuffled = sample(LGAs, size=length(LGAs), replace=FALSE)
    pfpr_case_u5_runMeans_mis$admin_name = sapply(pfpr_case_u5_runMeans$admin_name, function(x) LGAs_shuffled[which(LGAs == x)])
    pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('admin_name','date'), all.x=TRUE)
    pfpr_mismatched$num_U5_pos = pfpr_mismatched$p_test_mean * pfpr_mismatched$num_U5_sampled
    mismatch_lik_cur = dbinom(round(pfpr_mismatched[pfpr_mismatched$year.y %in% year,]$num_U5_pos), size=pfpr_mismatched[pfpr_mismatched$year.y %in% year,]$num_U5_sampled, prob=pfpr_mismatched[pfpr_mismatched$year.y %in% year,]$`PfPR U5`)
    mismatch_loglik[ii] = sum(log(mismatch_lik_cur))
  }
  # histogram
  gg_list[[gg_index]] = ggplot(as.data.frame(mismatch_loglik), aes(mismatch_loglik))+
    geom_histogram(aes(y=..density..), bins=40, fill='grey', color='darkgrey')+
    geom_vline(xintercept=match_loglik, color='red', size=2) +
    xlab('ln(probability of DHS observations)')+
    # xlim(min(mismatch_loglik, na.rm=TRUE), max(c(mismatch_loglik,match_loglik), na.rm=TRUE)) + 
    theme_classic()
  gg_index = gg_index+1
}
gg = grid.arrange(gg_list[[1]], gg_list[[2]], gg_list[[3]], nrow = 1)
# ggsave(paste0(sim_filepath_2010, '/_plots/_validation/hist_logprob_DHS_PfPR_match_mismatch.png'), gg, width=7.2, height=2)
ggsave(paste0(sim_filepath_2010, '/_plots/_validation/hist_logprob_DHS_PfPR_match_mismatch_2015_17_18.png'), gg_list[[1]], width=3.6, height=3)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# how does matched likelihood compare to when we draw PfPR directly from simulation?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
pdf(paste0(sim_filepath_2010, '/_plots/_validation/hist_loglik_simSurvey_PfPR.pdf'), width=5, height=5.5)

#  ----  all years  ----  #
# likelihood of drawing observed DHS positives if simulation PfPR is true probability
pfpr_matched$num_U5_pos = pfpr_matched$p_test_mean * pfpr_matched$num_U5_sampled
match_lik = dbinom(round(pfpr_matched$num_U5_pos), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
match_loglik = sum(log(match_lik))
num_reps = 300
sim_survey_loglik = rep(NA,num_reps)
for(ii in 1:num_reps){
  # what does this look like if we sample pretend DHS observations from simulation PfPR? essentially make up a dhs dataset from sampling the simulated population
  pfpr_matched$sim_survey_U5_pos = rbinom(length(pfpr_matched$num_U5_sampled), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
  # likelihood of drawing observed surveyed positives if simulation PfPR is true probability
  sim_survey_lik = dbinom(round(pfpr_matched$sim_survey_U5_pos), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
  sim_survey_loglik[ii] = sum(log(sim_survey_lik))
}
# histogram
par(mfrow=c(1,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
hist(sim_survey_loglik, breaks=seq(min(c(sim_survey_loglik[is.finite(sim_survey_loglik)], match_loglik)), max(c(match_loglik, sim_survey_loglik)), length.out=100), freq=FALSE, main='', xlab='loglikelihood of sim survey PfPR observations')
# title(main='sim survey PfPR likelihoods, all years', line=0.5)
abline(v=match_loglik, col='red', lwd=3)
mtext('DHS survey', line=-0.85, col='red', side=2)
dev.off()

# draw from simulation where local PfPR is drawn from Beta with mean equal to DS PfPR
variance = 0.01
pdf(paste0(sim_filepath_2010, '/_plots/_validation/hist_loglik_simSurvey2_PfPR_var',round(variance*1000),'.pdf'), width=5, height=5.5)
# png(paste0(sim_filepath_2010, '/_plots/_validation/hist_loglik_simSurvey2_PfPR_var',round(variance*1000),'.png'), width=720, height=480)

#  ----  all years  ----  #
# likelihood of drawing observed DHS positives if simulation PfPR is true probability
pfpr_matched$num_U5_pos = pfpr_matched$p_test_mean * pfpr_matched$num_U5_sampled
match_lik = dbinom(round(pfpr_matched$num_U5_pos), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
match_loglik = sum(log(match_lik))

num_reps = 300
sim_survey_loglik = rep(NA,num_reps)
for(ii in 1:num_reps){
  # what does this look like if we sample pretend DHS observations from simulation PfPR? essentially make up a dhs dataset from sampling the simulated population
  pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR, variance = variance)
  pfpr_matched$sim_survey2_U5_pos = rbinom(length(pfpr_matched$num_U5_sampled), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$local_sim_PfPR)
  
  # likelihood of drawing observed surveyed positives if simulation PfPR is true probability
  sim_survey_lik = dbinom(round(pfpr_matched$sim_survey2_U5_pos), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
  sim_survey_loglik[ii] = sum(log(sim_survey_lik))
}

# histogram
par(mfrow=c(1,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
hist(sim_survey_loglik, breaks=seq(min(c(sim_survey_loglik[is.finite(sim_survey_loglik)], match_loglik)), max(c(match_loglik, sim_survey_loglik)), length.out=100), freq=FALSE, main='', xlab='loglikelihood of sim survey PfPR observations')
# title(main='sim survey PfPR likelihoods, all years', line=0.5)
abline(v=match_loglik, col='red', lwd=3)
# mtext('DHS survey', line=-0.85, col='red', side=2)
dev.off()


