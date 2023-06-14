# seasonality_archetype_creation.R
# November 2022
# contact: mambrose
#
# Goal: We want to create groups of LGAs (archetypes) that have similar seasonality patterns so that we can calibrate 
#       our model's monthly larval habitat appropriately. The fewer groups that can accurately capture the full diversity 
#       of seasonality patterns across the country, the easier the calibration process is. 
# Context and approach: Originally in 2019, we used spatial clustering with climate information, vector species, 
#       MAP PfPR, etc. to create archetypes and then re-allocated some LGAs to new archetypes when certain archetypes 
#       didn't have large enough 2010 DHS sample sizes. However, those calibrations were done against the more limited RIA 
#       incidence dataset, whose smaller sample sizes didn't show as much seasonality. Now that we have the more complete 
#       incidence from routine data, we are clustering directly based on that information instead. 



library(ggplot2)
library(ggpubr)
library(gridExtra)
library(data.table)
library(RColorBrewer)
library(tidyr)
library(tibble)
library(dplyr)
library(reshape2)
library(stringr)
library(haven)
library(raster)
library(viridis)
library(prettyGraphs) 


###########################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                                setup
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###########################################################################

base_filepath = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration'
base_hbhi_filepath = paste0(base_filepath, '/hbhi_nigeria')
pop_arch_filepath = paste0(base_hbhi_filepath, '/nigeria_LGA_pop.csv')
shapefile_filepath = paste0(base_hbhi_filepath, '/SpatialClustering/reference_rasters_shapefiles/NGA_DS_clusteringProjection.shp')
dhs_pfpr_filepath = paste0(base_hbhi_filepath, '/DS DHS estimates/U5/pfpr/DHS.data_surveyd')

# LGA populations and archetypes
pop_arch = fread(pop_arch_filepath)
# shapefile with LGA locations
country_mask = shapefile(shapefile_filepath)

# account for naming differences
get_compatible_state_name = function(name){
  if(name=='Akwa-Ibom') name='Akwa lbom'
  if(name=='Cross-River') name='Cross River'
  if(name=='FCT-Abuja') name='Federal Capital Territory'
  return (name)
}

rescaled_max_incidence = 100  # 50

script_dir = 'C:/Users/moniqueam/Documents/malaria-nga-snt22'
source(paste0(script_dir,'/standardize_admin_names.R'))

save_files = TRUE  # this should almost always be TRUE, but option for FALSE allowed for debugging/testing without overwriting existing files

###########################################################################
######### Approach (as of 11/15/2022) used for creating clusters ##########
###########################################################################
# cluster NGA states based on the monthly LGA cases from 2018-2019 using the file shared in 2022
# - - - - - - - - - - - - - - - - #
# read in and format data
# - - - - - - - - - - - - - - - - #
new_incidence = read_dta(paste0(base_filepath, '/nigeria_who/NGA_2022_SNT/_Submitted_data/Routine data/monthly_lga.dta'))
incidence_dhis2 = as.data.table(new_incidence)
incidence_dhis2$LGA = incidence_dhis2$adm2
incidence_dhis2 = incidence_dhis2[(incidence_dhis2$LGA != ''),]
incidence_dhis2$conf_all_age = incidence_dhis2$conf_ov5 + incidence_dhis2$conf_u5
incidence_dhis2$date = as.Date(paste0(incidence_dhis2$year, '-', incidence_dhis2$month, '-01'), tryFormats = c("%Y-%m-%d", "%y-%m-%d"))
incidence_dhis2$adm1 = sapply(incidence_dhis2$adm1, get_compatible_state_name)
num_zeros_allowed = 1

# rescale 2017, 2018, and 2019 relative to their respective annual means
include_2017 = FALSE
incidence_2017 = incidence_dhis2[incidence_dhis2$year == 2017,]
incidence_2017$is_zero = incidence_2017$conf_all_age == 0
mean_incidence_2017 = incidence_2017 %>% group_by(LGA) %>%
  summarise(mean_year_conf_all_age = mean(conf_all_age),
            num_zeros = sum(is_zero))
incidence_2017 = merge(incidence_2017, mean_incidence_2017, by='LGA', all.x=TRUE)
incidence_2017$rescaled_conf_all_age = incidence_2017$conf_all_age / incidence_2017$mean_year_conf_all_age
incidence_2017$rescaled_conf_all_age[incidence_2017$num_zeros > num_zeros_allowed] = NA
incidence_2018 = incidence_dhis2[incidence_dhis2$year == 2018,]
incidence_2018$is_zero = incidence_2018$conf_all_age == 0
mean_incidence_2018 = incidence_2018 %>% group_by(LGA) %>%
  summarise(mean_year_conf_all_age = mean(conf_all_age),
            num_zeros = sum(is_zero))
incidence_2018 = merge(incidence_2018, mean_incidence_2018, by='LGA', all.x=TRUE)
incidence_2018$rescaled_conf_all_age = incidence_2018$conf_all_age / incidence_2018$mean_year_conf_all_age
incidence_2018$rescaled_conf_all_age[incidence_2018$num_zeros > num_zeros_allowed] = NA
incidence_2019 = incidence_dhis2[incidence_dhis2$year == 2019,]
incidence_2019$is_zero = incidence_2019$conf_all_age == 0
mean_incidence_2019 = incidence_2019 %>% group_by(LGA) %>%
  summarise(mean_year_conf_all_age = mean(conf_all_age),
            num_zeros = sum(is_zero))
incidence_2019 = merge(incidence_2019, mean_incidence_2019, by='LGA', all.x=TRUE)
incidence_2019$rescaled_conf_all_age = incidence_2019$conf_all_age / incidence_2019$mean_year_conf_all_age
incidence_2019$rescaled_conf_all_age[incidence_2019$num_zeros > num_zeros_allowed] = NA
incidence_2018_2019 = rbind(incidence_2018, incidence_2019)
if(include_2017){
   incidence_2017_2018_2019 = rbind(incidence_2017, incidence_2018_2019)
   incidence_2018_2019 = incidence_2017_2018_2019
}
incidence_2018_2019_original = incidence_2018_2019

# get within-State monthly average (across LGAs and 2018/2019)
incidence_state_ave = incidence_2018_2019 %>% group_by(adm1, month) %>%
  summarise(mean_rescaled_conf_all_age = mean(rescaled_conf_all_age, na.rm=TRUE))
gg = ggplot() + 
  geom_line(data=incidence_2018_2019, aes(x=month, y=rescaled_conf_all_age, color=LGA, linetype=factor(year))) + 
  geom_line(data=incidence_state_ave, aes(x=month, y=mean_rescaled_conf_all_age), color='black', size=2) + 
  coord_cartesian(ylim = c(0,4)) +
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('adm1', nrow=5, scales='free')
if(save_files) ggsave(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/rescaled_dhis2_monthly_cases_state_2018_2019_average_excessZerosRemoved.png'), gg, width=25, height=12)

gg = ggplot() + 
  geom_line(data=incidence_state_ave, aes(x=month, y=mean_rescaled_conf_all_age), color='black', size=2) + 
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('adm1', nrow=5)
if(save_files) ggsave(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/rescaled_dhis2_monthly_cases_state_2018_2019_average_noLGA.png'), gg, width=25, height=12)



# - - - - - - - - - - - - #
# create clusters
# - - - - - - - - - - - - #
# get cluster of states to include in same archetype based on rescaled monthly values
num_clusters = 5
monthly_vals = dcast(incidence_state_ave[!is.na(incidence_state_ave$mean_rescaled_conf_all_age),], adm1 ~ month, value.var="mean_rescaled_conf_all_age")

# include latitude in clustering?
include_lat = TRUE
lat_rescale = 1
if(include_lat){
  # get mean latitude among all LGAs in a state
  lat_means = pop_arch %>% group_by(State) %>%
    summarise(lat = mean(lat))
  # rescale latitude importance in clustering
  lat_means$lat = lat_means$lat / mean(lat_means$lat) * lat_rescale
  colnames(lat_means)[colnames(lat_means)=='State'] = 'adm1'
  monthly_vals = merge(monthly_vals, lat_means, all=TRUE)
}

monthly_val_states = monthly_vals$adm1
monthly_vals = monthly_vals[,-which(colnames(monthly_vals) =='adm1')]
set.seed(1)
kmeans_result = kmeans(monthly_vals, centers=num_clusters, nstart=15, iter.max=30)
cluster_results = data.frame('adm1' = monthly_val_states, 'cluster' = kmeans_result$cluster)
if(save_files) write.csv(cluster_results, paste0(base_hbhi_filepath, '/snt_2022/incidence/cluster',num_clusters,'_results.csv'), row.names=FALSE)
# cluster_results = read.csv(paste0(base_hbhi_filepath, '/snt_2022/incidence/cluster',num_clusters,'_results_preReassign.csv'))



# - - - - - - - - - - - - #
# save archetype incidence
# - - - - - - - - - - - - #
# get means within clusters for each month
incidence_2018_2019 = merge(incidence_2018_2019_original, cluster_results, by='adm1', all=TRUE)
incidence_clust_ave = incidence_2018_2019 %>% group_by(cluster, month) %>%
  summarise(mean_rescaled_conf_all_age = mean(rescaled_conf_all_age, na.rm=TRUE))

# also save rescaled incidence version with maximum monthly value is rescaled_max_incidence cases per 1000 individuals
incidence_clust_max = incidence_clust_ave %>% group_by(cluster) %>%
  summarise(max_inc = max(mean_rescaled_conf_all_age, na.rm=TRUE))
incidence_clust_ave = merge(incidence_clust_ave, incidence_clust_max, all.x=TRUE)
incidence_clust_ave$monthly_incidence = incidence_clust_ave$mean_rescaled_conf_all_age / incidence_clust_ave$max_inc * rescaled_max_incidence
if(save_files) write.csv(incidence_clust_ave, paste0(base_hbhi_filepath, '/snt_2022/incidence/archetype_seasonality_incidence_',rescaled_max_incidence,'max.csv'), row.names=FALSE)

gg = ggplot() + 
  geom_line(data=incidence_clust_ave, aes(x=month, y=monthly_incidence), color='black', size=2) + 
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('cluster', nrow=2)
if(save_files) ggsave(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/archetype_seasonal_incidence_',rescaled_max_incidence,'max.png'), gg, width=25, height=12)




# - - - - - - - - - - - - - - - - - - - - - - - - #
# add cluster and representative admin to new archetype file
# - - - - - - - - - - - - - - - - - - - - - - - - #
# assign a 'representative' admin to each archetype (this admin's demographics and climate files will be used for calibration)
# choose the LGA whose average 2018/2019 incidence values are closest to the incidence used for the archetype
update_pop_arch_file = function(pop_arch, cluster_results, incidence_2018_2019, incidence_clust_ave, num_clusters, filename_suffix=''){
  # create updated archetype file with the Archetype number and representative admin
  pop_arch_new = pop_arch
  pop_arch_new$Archetype = NA
  for(ss in 1:nrow(cluster_results)){
    cur_state = cluster_results$adm1[ss]
    update_rows = which(sapply(pop_arch_new$State, get_compatible_state_name) == get_compatible_state_name(cur_state))
    pop_arch_new$Archetype[update_rows] = cluster_results$cluster[ss]
  }
  if(length(unique(pop_arch_new$State[is.na(pop_arch_new$Archetype)]))>0) warning('Some states not assigned an archetype')
  pop_arch_new$admin_name = pop_arch_new$LGA
  
  # get 2018/2019 average for each admin
  ave_incidence_2018_2019 = incidence_2018_2019 %>% group_by(LGA, month) %>%
    summarise(mean_rescaled_conf_all_age = mean(rescaled_conf_all_age, na.rm=TRUE))
  
  # match admin names and get cluster for each admin
  ave_incidence_2018_2019$admin_name = standardize_admin_names_in_vector(target_names=pop_arch_new$admin_name, origin_names=ave_incidence_2018_2019$LGA)
  # unique(ave_incidence_2018_2019$admin_name[(which(ave_incidence_2018_2019$admin_name != ave_incidence_2018_2019$LGA))])
  ave_incidence_2018_2019 = merge(ave_incidence_2018_2019, pop_arch_new[,c('admin_name', 'Archetype')])
  # see which is closest to the archetype average used for calibration
  rep_admin = rep(NA, num_clusters)
  for(nn in 1:num_clusters){
    # subset to current archetype
    cur_arch_inc = incidence_clust_ave[incidence_clust_ave$cluster==nn,]
    cur_arch_inc$cluster_inc = cur_arch_inc$mean_rescaled_conf_all_age
    cur_admin_inc = ave_incidence_2018_2019[ave_incidence_2018_2019$Archetype == nn,]
    if(nrow(cur_admin_inc)>0){
      # get the difference between monthly values in LGAs versus their archetype
      cur_admin_inc = merge(cur_admin_inc, cur_arch_inc[,c('cluster_inc', 'month')])
      cur_admin_inc$diff_between = abs(cur_admin_inc$mean_rescaled_conf_all_age - cur_admin_inc$cluster_inc)
      cur_mean_differences = cur_admin_inc %>% group_by(admin_name) %>%
        summarise(mean_dif = mean(diff_between))
      # get the admin where the difference is smallest
      rep_admin[nn] = cur_mean_differences$admin_name[which.min(cur_mean_differences$mean_dif)]
    }else{
      rep_admin[nn] = NA
    }
  }

  # add the representative admins to the archetype/pop file
  pop_arch_new = merge(pop_arch_new, data.frame('seasonality_archetype'=rep_admin, 'Archetype'=as.character(1:num_clusters)), all.x=TRUE)
  pop_arch_new$pop_size = pop_arch_new$geopode.pop
  if(save_files) write.csv(pop_arch_new, paste0(base_hbhi_filepath, '/snt_2022/admin_pop_archetype', filename_suffix, '.csv'))
  
  # create calibration incidence file with appropriate column names
  incidence_clust_ave = read.csv(paste0(base_hbhi_filepath, '/snt_2022/incidence/archetype_seasonality_incidence_',rescaled_max_incidence,'max.csv'))
  incidence_clust_ave = merge(incidence_clust_ave, data.frame('seasonality_archetype'=rep_admin, 'cluster'=as.character(1:num_clusters)), all.x=TRUE)
  incidence_clust_ave$cases = incidence_clust_ave$monthly_incidence
  incidence_clust_ave$population = 1000
  incidence_clust_ave$incidence = incidence_clust_ave$monthly_incidence
  incidence_clust_ave = incidence_clust_ave[,c('month', 'cases', 'seasonality_archetype', 'population', 'incidence')]
  if(save_files) write.csv(incidence_clust_ave, paste0(base_hbhi_filepath, '/snt_2022/simulation_inputs/incidence/archetype_incidence.csv'), row.names=FALSE)
  
  return(list(pop_arch_new, rep_admin))
}


# - - - - - - - - - - - - #
# plot clustering results
# - - - - - - - - - - - - #
create_cluster_result_plots = function(incidence_2018_2019, incidence_clust_ave, rep_admin, num_clusters, cluster_results, country_mask, filename_suffix=''){
  gg = ggplot() + 
    geom_line(data=incidence_2018_2019, aes(x=month, y=rescaled_conf_all_age, color=adm1, group_by=LGA, linetype=factor(year))) + 
    geom_line(data=incidence_clust_ave, aes(x=month, y=mean_rescaled_conf_all_age), color='black', size=2) + 
    coord_cartesian(ylim = c(0,4)) +
    theme_bw()+
    theme(legend.position="none") + 
    facet_wrap('cluster', nrow=2, scales='free')
  if(save_files) ggsave(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/rescaled_dhis2_monthly_cases_cluster',num_clusters,'_2018_2019_average', filename_suffix, '.png'), gg, width=25, height=12)
  
  
  # plot the rep admins
  gg = ggplot() + 
    geom_line(data=incidence_2018_2019[incidence_2018_2019$LGA %in% rep_admin,], aes(x=month, y=rescaled_conf_all_age, linetype=factor(year)), color='blue') + 
    geom_line(data=incidence_clust_ave, aes(x=month, y=mean_rescaled_conf_all_age), color='black', size=2) + 
    coord_cartesian(ylim = c(0,4)) +
    theme_bw()+
    theme(legend.position="none") + 
    facet_wrap('cluster', nrow=2, scales='free')
  if(save_files) ggsave(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/rescaled_dhis2_monthly_cases_cluster',num_clusters,'_withRepAdmin', filename_suffix, '.png'), gg, width=25, height=12)
  
  
  #### Plot for BMGF NCO States ####
  nco_states = c('Kano', 'Yobe', 'Sokoto')
  nco_clusters = unique(cluster_results$cluster[cluster_results$adm1 %in% nco_states])
  gg = ggplot() + 
    geom_line(data=incidence_2018_2019[incidence_2018_2019$adm1 %in% nco_states,], aes(x=month, y=rescaled_conf_all_age, color=adm1, group_by=LGA, linetype=factor(year))) + 
    geom_line(data=incidence_clust_ave[incidence_clust_ave$cluster %in% nco_clusters,], aes(x=month, y=mean_rescaled_conf_all_age), color='black', size=2) + 
    coord_cartesian(ylim = c(0,4)) +
    theme_bw()+
    theme(legend.position="none") + 
    facet_wrap('cluster', nrow=2, scales='free')
  if(save_files) ggsave(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/rescaled_dhis2_monthly_cases_cluster',num_clusters,'_2018_2019_average_NCOstates', filename_suffix, '.png'), gg, width=5, height=7)
  
  
  #### plot map with clustering ####
  # plot coloring DS according to archetype DS
  clust_cols = rainbow(num_clusters, alpha=0.4)
  country_mask$color = 'black'
  for(ss in 1:nrow(cluster_results)){
    country_mask$color[country_mask$State == get_compatible_state_name(cluster_results$adm1[ss])] = clust_cols[cluster_results$cluster[ss]]
  }
  if(save_files) png(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/map_of_cluster', num_clusters, '_lgas', filename_suffix, '.png'), width=5, height=5, units='in', res=900)
  plot(country_mask, col=country_mask$color)  
  if(save_files) dev.off()
}





# - - - - - - - - - - - - - - #
# call functions to generate outputs. NOTE: option to manually reassign clusters here, which requires user input
# - - - - - - - - - - - - - - #
# IMPORTANT NOTE: the reassign numbers will change and need to be set manually each time, after looking at the results, if it is determined that
#    one archetype is not reliable and should instead be represented by a different archetype.
manually_reassign_cluster=TRUE

if(manually_reassign_cluster){
  ggplot() + 
    geom_line(data=incidence_2018_2019, aes(x=month, y=rescaled_conf_all_age, color=adm1, group_by=LGA, linetype=factor(year))) + 
    geom_line(data=incidence_clust_ave, aes(x=month, y=mean_rescaled_conf_all_age), color='black', size=2) + 
    coord_cartesian(ylim = c(0,4)) +
    theme_bw()+
    theme(legend.position="none") + 
    facet_wrap('cluster', nrow=2, scales='free')
  
  filename_suffix='_preReassign'
  if(save_files) write.csv(cluster_results, paste0(base_hbhi_filepath, '/snt_2022/incidence/cluster',num_clusters,'_results_preReassign.csv'), row.names=FALSE)
} else{
  filename_suffix=''
}
# create files and plots for original clustering
arch_output = update_pop_arch_file(pop_arch, cluster_results, incidence_2018_2019, incidence_clust_ave, num_clusters, filename_suffix=filename_suffix)
rep_admin=arch_output[[2]]
create_cluster_result_plots(incidence_2018_2019, incidence_clust_ave, rep_admin, num_clusters, cluster_results, country_mask, filename_suffix=filename_suffix)


if(manually_reassign_cluster){

  # # # # # # # # # # # # # # 
  # USER INPUT NEEDED: manually reassign one of the archetypes, which is showing a suspicious downward trajectory through the year, likely not due to intrinsic seasonality
  reassign_from_arch_num = 2
  reassign_to_arch_num = 5
  # # # # # # # # # # # # # # 
  
  # update cluster numbers
  cluster_results$cluster[cluster_results$cluster==reassign_from_arch_num] = reassign_to_arch_num
  if(save_files) write.csv(cluster_results, paste0(base_hbhi_filepath, '/snt_2022/incidence/cluster',num_clusters,'_results.csv'), row.names=FALSE)
  incidence_2018_2019 = merge(incidence_2018_2019_original, cluster_results, by='adm1', all=TRUE)
  
  # create files and plots for new clustering
  arch_output = update_pop_arch_file(pop_arch, cluster_results, incidence_2018_2019, incidence_clust_ave, num_clusters, filename_suffix='')
  rep_admin=arch_output[[2]]
  create_cluster_result_plots(incidence_2018_2019, incidence_clust_ave, rep_admin, num_clusters, cluster_results, country_mask, filename_suffix='')
}



# - - - - - - - - - - - - - - #
# plot full timeseries, rescaled to 2018, for all LGAs, faceted to archetype
# - - - - - - - - - - - - - - #

# rescale each LGA's value so that the mean from 2017-2018 is the same (looking for seasonality, not transmission-intensity patterns)
incidence_2018 = incidence_dhis2[incidence_dhis2$year == 2018,]
incidence_2018$is_zero = incidence_2018$conf_all_age == 0
mean_incidence_2018 = incidence_2018 %>% group_by(LGA) %>%
  summarise(mean_year_conf_all_age = mean(conf_all_age),
            num_zeros = sum(is_zero))
incidence_res_2018 = merge(incidence_dhis2, mean_incidence_2018, by='LGA', all.x=TRUE)
incidence_res_2018$rescaled_conf_all_age = incidence_res_2018$conf_all_age / incidence_res_2018$mean_year_conf_all_age
incidence_res_2018$rescaled_conf_all_age[incidence_res_2018$num_zeros > num_zeros_allowed] = NA

# add in archetype identity
cluster_results = read.csv(paste0(base_hbhi_filepath, '/snt_2022/incidence/cluster',num_clusters,'_results.csv'))
incidence_res_2018 = merge(incidence_res_2018, cluster_results, by='adm1', all.x=TRUE)

# plot rescaled timeseries, faceted by archetype
gg = ggplot(incidence_res_2018, aes(x=date, y=rescaled_conf_all_age, color=LGA)) + 
  geom_line() + 
  coord_cartesian(ylim=c(0,8), xlim=c(as.Date('2017-01-01'), as.Date('2020-05-01')))+
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('cluster', nrow=2)#, scales='free')
ggsave(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/rescaled_dhis2_LGAtimeseries_byCluster.png'), gg, width=10, height=8)




###########################################################################
######### Approach with timeseries decomposition (attempted 11/28/2022) ##########
###########################################################################
# attempted to try to remove general trend up/down within a year, but it was not successful at that

# read in and format data
new_incidence = read_dta(paste0(base_filepath, '/nigeria_who/NGA_2022_SNT/_Submitted_data/Routine data/monthly_lga.dta'))
incidence_dhis2 = as.data.table(new_incidence)
incidence_dhis2$LGA = incidence_dhis2$adm2
# incidence_dhis2$LGA = standardize_admin_names_in_df(target_names_df=pop_arch, origin_names_df=incidence_dhis2, target_names_col='LGA', origin_names_col='LGA')
incidence_dhis2 = incidence_dhis2[(incidence_dhis2$LGA != ''),]
incidence_dhis2$conf_all_age = incidence_dhis2$conf_ov5 + incidence_dhis2$conf_u5
incidence_dhis2$date = as.Date(paste0(incidence_dhis2$year, '-', incidence_dhis2$month, '-01'), tryFormats = c("%Y-%m-%d", "%y-%m-%d"))


seasonal_trends = data.frame('admin_name'=c(), 'state'=c(), 'month'=c(), 'rescaled_inc'=c())
excluded_lgas = c()
start_year=2017
start_month=1
end_year=2020
end_month=5
start_date = as.Date(paste0(start_year, '-', start_month, '-01'))
end_date = as.Date(paste0(end_year, '-', end_month, '-01'))
incidence_dhis2_recent = incidence_dhis2[(incidence_dhis2$date >= start_date) & (incidence_dhis2$date <= end_date),]
all_months_df = data.frame(date = seq.Date(from=start_date, to=end_date, by='month'))
inc_lgas = unique(incidence_dhis2_recent$LGA)
for(ii in 1:length(inc_lgas)){
  # subset to current LGA info
  cur_lga = inc_lgas[ii]
  cur_state = incidence_dhis2_recent$adm1[incidence_dhis2_recent$LGA == cur_lga][1]
  cur_incidence = incidence_dhis2_recent[incidence_dhis2_recent$LGA == cur_lga,]
  cur_incidence = merge(cur_incidence, all_months_df, all=TRUE)
  cur_incidence = cur_incidence[order(cur_incidence$date)]
  
  # check whether the data are of sufficient quality with these dates
  if(any(is.na(cur_incidence$conf_all_age)) | sum(cur_incidence$conf_all_age == 0)>5){
    # try removing the first year
    start_year2=start_year+1
    start_date2 = as.Date(paste0(start_year2, '-', start_month, '-01'))
    cur_incidence = cur_incidence[cur_incidence$year >= start_year2,]
    if(any(is.na(cur_incidence$conf_all_age)) | sum(cur_incidence$conf_all_age == 0)>5){
      include_lga = FALSE
      excluded_lgas = c(excluded_lgas, cur_lga)
    } else{
      include_lga = TRUE
      cur_ts = ts(data=cur_incidence$conf_all_age, start=c(start_year2, start_month), end=c(end_year, end_month), frequency=12)
    }
  } else{
    include_lga = TRUE
    cur_ts = ts(data=cur_incidence$conf_all_age, start=c(start_year, start_month), end=c(end_year, end_month), frequency=12)
  }
  
  # get seasonal trend and rescale
  if(include_lga){
    cur_decomp = decompose(cur_ts)
    # plot(cur_decomp)
    cur_seasonal_trend = as.vector(cur_decomp$seasonal)[1:12]
    # rescale seasonal trend so that the min is 10 and max is 50
    cur_seasonal_trend = cur_seasonal_trend - min(cur_seasonal_trend)
    cur_seasonal_trend = cur_seasonal_trend / max(cur_seasonal_trend) * 40
    cur_seasonal_trend = cur_seasonal_trend + 10
    cur_trend_df = data.frame('admin_name'=rep(cur_lga, 12), 'state'=rep(cur_state, 12), 'month'=1:12, 'rescaled_inc'=cur_seasonal_trend)
    seasonal_trends = rbind(seasonal_trends, cur_trend_df)
  }
}
if(length(excluded_lgas)>0){
  warning(paste0('Excluded ', length(excluded_lgas), ' LGAs due to NAs or a concerning number of zero entries'))
}


# get within-State monthly average (across LGAs)
incidence_state_ave = seasonal_trends %>% group_by(state, month) %>%
  summarise(mean_rescaled_inc = mean(rescaled_inc, na.rm=TRUE))

gg = ggplot() + 
  geom_line(data=seasonal_trends, aes(x=month, y=rescaled_inc, color=admin_name)) + 
  geom_line(data=incidence_state_ave, aes(x=month, y=mean_rescaled_inc), color='black', size=2) + 
  # coord_cartesian(ylim = c(0,4)) +
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('state', nrow=5, scales='free')
ggsave(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/rescaled_dhis2_monthly_cases_seasonal_decomposition.png'), gg, width=25, height=12)





################################################################################################
######### Alternative and/or previous approaches and additional plots and exploration ##########
################################################################################################


# - - - - - - - - - - - - - - - - #
# incidence from dhis2
# - - - - - - - - - - - - - - - - #
create_dhis2_incidence_plots = function(incidence_dhis2, plot_output_dir){
  
  # plot rescaled timeseries, faceted by archetype
  gg = ggplot(incidence_dhis2[incidence_dhis2$year>2015], aes(x=date, y=rescaled_conf_all_age, color=LGA)) + 
    geom_line() + 
    theme_bw()+
    theme(legend.position="none") + 
    facet_wrap('Archetype', nrow=10, scales='free')
  ggsave(paste0(plot_output_dir, '/rescaled_dhis2_timeseries_cases_by_archetype.png'), gg, width=10, height=20)
  
  # plot rescaled timeseries by month, faceted by archetype
  gg = ggplot(incidence_dhis2[incidence_dhis2$year>2016], aes(x=month, y=rescaled_conf_all_age, color=LGA, group_by=year, linetype=factor(year))) + 
    geom_line() + 
    theme_bw()+
    theme(legend.position="none") + 
    facet_wrap('Archetype', nrow=5, scales='free')
  ggsave(paste0(plot_output_dir, '/rescaled_dhis2_monthly_cases_by_archetype.png'), gg, width=25, height=12)
  
  # plot rescaled timeseries by month, faceted by archetype, only in 2018
  gg = ggplot(incidence_dhis2[incidence_dhis2$year==2018], aes(x=month, y=rescaled_conf_all_age, color=LGA, group_by=year, linetype=factor(year))) + 
    geom_line() + 
    theme_bw()+
    theme(legend.position="none") + 
    facet_wrap('Archetype', nrow=5, scales='free')
  ggsave(paste0(plot_output_dir, '/rescaled_dhis2_monthly_cases_by_archetype_2018.png'), gg, width=25, height=12)
  
  
  if('adm1' %in% colnames(incidence_dhis2)){
    # plot rescaled timeseries, faceted by state
    gg = ggplot(incidence_dhis2[incidence_dhis2$year>2015], aes(x=date, y=rescaled_conf_all_age, color=LGA)) + 
      geom_line() + 
      theme_bw()+
      theme(legend.position="none") + 
      facet_wrap('adm1', nrow=10, scales='free')
    ggsave(paste0(plot_output_dir, '/rescaled_dhis2_timeseries_cases_by_state.png'), gg, width=10, height=20)
    
    # plot rescaled timeseries by month, faceted by state
    gg = ggplot(incidence_dhis2[incidence_dhis2$year>2016], aes(x=month, y=rescaled_conf_all_age, color=LGA, group_by=year, linetype=factor(year))) + 
      geom_line() + 
      theme_bw()+
      theme(legend.position="none") + 
      facet_wrap('adm1', nrow=5, scales='free')
    ggsave(paste0(plot_output_dir, '/rescaled_dhis2_monthly_cases_by_state.png'), gg, width=25, height=12)
    
    # plot rescaled timeseries by month, faceted by state, only 2018
    gg = ggplot(incidence_dhis2[incidence_dhis2$year==2018], aes(x=month, y=rescaled_conf_all_age, color=LGA, group_by=year, linetype=factor(year))) + 
      geom_line() + 
      theme_bw()+
      theme(legend.position="none") + 
      facet_wrap('adm1', nrow=5, scales='free')
    ggsave(paste0(plot_output_dir, '/rescaled_dhis2_monthly_cases_by_state_2018.png'), gg, width=25, height=12)
    
    # plot rescaled timeseries by month, faceted by archetype, only in 2018, color by State
    gg = ggplot(incidence_dhis2[incidence_dhis2$year==2018], aes(x=month, y=rescaled_conf_all_age, color=adm1, group_by=LGA, linetype=factor(year))) + 
      geom_line() + 
      theme_bw()+
      theme(legend.position="none") + 
      facet_wrap('Archetype', nrow=5, scales='free')
    ggsave(paste0(plot_output_dir, '/rescaled_dhis2_monthly_cases_by_archetype_2018_colorState.png'), gg, width=25, height=12)
  }
}




update_lga_name_match = function(lga_name){
  replace_list = list( 'Abua/Odual' = 'Abua-Odual',
                       'AMAC' = 'Abuja Municipal',
                       'Ado Odo/Ota' = 'Ado-Odo/Ota',
                       'Ado-Odo-Ota' = 'Ado-Odo/Ota',
                       'Afikpo-North' = 'Afikpo North',
                       'Afikpo-South' = 'Afikpo South',
                       'Aiyedaade' = 'Aiyedade',
                       'Aiyekire (Gbonyin)' = 'Aiyedire',
                       'Ajeromi/Ifelodun' = 'Ajeromi-Ifelodun',
                       'Amuwo Odofin' = 'Amuwo-Odofin',
                       'Arewa' = 'Arewa-Dandi',
                       'Askira/Uba' = 'Askira-Uba',
                       # 'Atakumosa' = 'XX',
                       'Atakunmosa East' = 'Atakumosa East',
                       'Atakunmosa West' = 'Atakumosa West',
                       'Atisbo' = 'Atigbo',
                       'Barkin Ladi' = 'Barikin Ladi',
                       'Bassa' = 'Bassa2',
                       'Bekwarra' = 'Bekwara',
                       'Birnin Kudu' = 'Birni Kudu',
                       'Birnin Gwari' = 'Birnin-Gwari',
                       'Birniwa' = 'Biriniwa',
                       'Dambam' = 'Damban',
                       'Danbatta' = 'Dambatta',
                       'Dange-Shuni' = 'Dange-Shnsi',
                       'Danko/Wasagu' = 'Wasagu/Danko',
                       'Efon' = 'Efon-Alayee',
                       'Emuoha' = 'Emohua',
                       'Esan North-East' = 'Esan North East',
                       'Esan South-East' = 'Esan South East',
                       'Ese Odo' = 'Ese-Odo',
                       'Ezinihitte-Mbaise'= 'Ezinihitte',
                       'Ganye' = 'Ganaye',
                       'Girei' = 'Gireri',
                       'Gwadabawa' = 'Gawabawa',
                       'Ibeju Lekki' = 'Ibeju/Lekki',
                       'Ibeju-Lekki' = 'Ibeju/Lekki',
                       'Ido-Osi' = 'Idosi-Osi',
                       'Ifako/Ijaye' = 'Ifako-Ijaye',
                       'Ifelodun' = 'Ifelodun1',
                       'Ihitte/Uboma' = 'Ihitte-Uboma',
                       'Ijebu Ode' = 'Ijebu ode',
                       'Ilejemeje' = 'Ilemeji',
                       'Imeko Afon' = 'Imeko-Afon',
                       'Irepodun' = 'Irepodun1',
                       'Irepodun/Ifelodun' = 'Irepodun-Ifelodun',
                       'Ise/Orun' = 'Ise-Orun',
                       'Isuikwuato' = 'Isuikwato',
                       'Jemaa' = 'Jema\'a',
                       'Karim-Lamido' = 'Karin-Lamido',
                       'Kaura Namoda' = 'Kaura-Namoda',
                       'Kolokuma-Opokuma' = 'Kolokuma/Opokuma',
                       'Makarfi' = 'Markafi',
                       'Matazu' = 'Matazuu',
                       'Mbatoli' = 'Mbaitoli',
                       'Nasarawa' = 'Nasarawa2',
                       'Nasarawa Eggon' = 'Nasarawa-Eggon',
                       'Nassarawa' = 'Nasarawa1',
                       'Obi' = 'Obi1',
                       'Obi Nwga' = 'Obi Nwa',
                       'Obio/Akpor' = 'Obia/Akpor',
                       'Ogori/Magongo' = 'Ogori/Mangongo',
                       'Ogun Waterside' = 'Ogun waterside',
                       'Oji River' = 'Oji-River',
                       'Ola-Oluwa' = 'Ola-oluwa',
                       'Olamaboro' = 'Olamabolo',
                       'Onuimo' = 'Unuimo',
                       'Oshimili-North' = 'Oshimili North',
                       'Oshimili-South' = 'Oshimili South',
                       'Oshodi/Isolo' = 'Oshodi-Isolo',
                       'Otukpo' = 'Oturkpo',
                       'Ovia North-East' = 'Ovia North East',
                       'Ovia South-West' = 'Ovia South West',
                       'Owerri Municipal' = 'Owerri-Municipal',
                       'Paikoro' = 'Pailoro',
                       'Sabon Gari' = 'Sabon-Gari',
                       'Shongom' = 'Shomgom',
                       'Southern-Ijaw' = 'Southern Ijaw',
                       'Surulere' = 'Surulere1',
                       'Takai' = 'Takali',
                       'Tarmuwa' = 'Tarmua',
                       'Umu-Nneochi' = 'Umu-Neochi',
                       'Urue Offong/Oruko' = 'Urue-Offong/Oruko',
                       'Yenagoa' = 'Yenegoa',
                       # 'Yewa North' = 'XX',
                       # 'Yewa South' = 'XX',
                       'Zangon Kataf' = 'Zango-Kataf')
  if(lga_name %in% names(replace_list)){
    lga_name = replace_list[lga_name][[1]]
  }
  lga_name = str_replace_all(lga_name, pattern=' ', replacement='-')
  lga_name = str_replace_all(lga_name, pattern='/', replacement='-')
  return(lga_name)
}

incidence_dhis2 = merge(incidence_dhis2, pop_arch[,c('LGA', 'Archetype')], all.x=TRUE)
unique(incidence_dhis2$LGA[which(!(incidence_dhis2$LGA %in% pop_arch$LGA))])


################
# using the monthly LGA cases from 2014-2018, obtained in 2019
filename_incidence = paste0(base_filepath, '/nigeria_who/Routine_data/Monthly_data/monthly_lga_14-18.csv') # year, month, adm2, clinical_u5, clinical_a5, conf_u5, conf_a5
incidence_dhis2 = fread(filename_incidence)
incidence_dhis2$LGA = incidence_dhis2$adm2
incidence_dhis2$LGA = sapply(incidence_dhis2$LGA, update_lga_name_match)
incidence_dhis2 = incidence_dhis2[(incidence_dhis2$LGA != ''),]
# incidence_dhis2$LGA = toupper(incidence_dhis2$LGA)
incidence_dhis2$clinical_all_age = incidence_dhis2$clinical_a5 + incidence_dhis2$clinical_u5
incidence_dhis2$conf_all_age = incidence_dhis2$conf_a5 + incidence_dhis2$conf_u5
incidence_dhis2$date = as.Date(paste0(incidence_dhis2$year, '-', incidence_dhis2$month, '-01'), tryFormats = c("%Y-%m-%d", "%y-%m-%d"))
pop_arch2 =pop_arch
pop_arch2$LGA = sapply(pop_arch2$LGA, update_lga_name_match)
# sort(unique(incidence_dhis2$LGA)[which(!(unique(incidence_dhis2$LGA)) %in% unique(pop_arch2$LGA))])
# sort(unique(pop_arch2$LGA)[which(!(unique(pop_arch2$LGA)) %in% unique(incidence_dhis2$LGA))])
incidence_dhis2 = merge(incidence_dhis2, pop_arch2[,c('LGA', 'Archetype')], all.x=TRUE)

# rescale each LGA's value so that the mean from 2017-2018 is the same (looking for seasonality, not transmission-intensity patterns)
incidence_2018 = incidence_dhis2[incidence_dhis2$year == 2018,]
mean_incidence_2018 = incidence_2018 %>% group_by(LGA) %>%
  summarise(mean_conf_all_age_2018 = mean(conf_all_age))
incidence_dhis2 = merge(incidence_dhis2, mean_incidence_2018, by='LGA', all.x=TRUE)
incidence_dhis2$rescaled_conf_all_age = incidence_dhis2$conf_all_age / incidence_dhis2$mean_conf_all_age_2018

create_dhis2_incidence_plots(incidence_dhis2=incidence_dhis2, plot_output_dir=paste0(base_hbhi_filepath, '/project_notes/figures/validation/incidence_lga_2019'))
incidence_dhis2_lga_2019 = incidence_dhis2


################
# using the monthly LGA cases from 2014-2020 obtained in 2022
new_incidence = read_dta(paste0(base_filepath, '/nigeria_who/NGA_2022_SNT/_Submitted_data/Routine data/monthly_lga.dta'))
incidence_dhis2 = as.data.table(new_incidence)
incidence_dhis2$LGA = incidence_dhis2$adm2
incidence_dhis2 = incidence_dhis2[(incidence_dhis2$LGA != ''),]
# incidence_dhis2$LGA = toupper(incidence_dhis2$LGA)
incidence_dhis2$conf_all_age = incidence_dhis2$conf_ov5 + incidence_dhis2$conf_u5
incidence_dhis2$date = as.Date(paste0(incidence_dhis2$year, '-', incidence_dhis2$month, '-01'), tryFormats = c("%Y-%m-%d", "%y-%m-%d"))
incidence_dhis2 = merge(incidence_dhis2, pop_arch[,c('LGA', 'Archetype')], all.x=TRUE)

# rescale each LGA's value so that the mean from 2018 is the same (looking for seasonality, not transmission-intensity patterns)
incidence_2018 = incidence_dhis2[incidence_dhis2$year == 2018,]
mean_incidence_2018 = incidence_2018 %>% group_by(LGA) %>%
  summarise(mean_conf_all_age_2018 = mean(conf_all_age))
incidence_dhis2 = merge(incidence_dhis2, mean_incidence_2018, by='LGA', all.x=TRUE)
incidence_dhis2$rescaled_conf_all_age = incidence_dhis2$conf_all_age / incidence_dhis2$mean_conf_all_age_2018
incidence_dhis2_lga_2022 = incidence_dhis2

ifelse(!dir.exists(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/full_lga_timeseries')), dir.create(paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/full_lga_timeseries')), FALSE)
if(save_files) create_dhis2_incidence_plots(incidence_dhis2=incidence_dhis2, plot_output_dir=paste0(base_hbhi_filepath, '/snt_2022/incidence/_plots/full_lga_timeseries'))



# incidence_2020 = incidence_dhis2[incidence_dhis2$year == 2020,]
# ggplot(incidence_2020, aes(x=date, y=conf_all_age, color=LGA))+
#    geom_line()+
#    theme_bw()+
#    theme(legend.position="none") + 
#    facet_wrap('adm1', nrow=5, scales='free')



################
# using the monthly LGA cases from RIA, also with info on archetype
filename_incidence = paste0(base_hbhi_filepath, '/incidence/RIA_by_LGA_and_rep_DS.csv') # year, month, adm2, clinical_u5, clinical_a5, conf_u5, conf_a5
incidence_dhis2 = fread(filename_incidence)
incidence_dhis2 = incidence_dhis2[(incidence_dhis2$LGA != ''),]
# incidence_dhis2$LGA = toupper(incidence_dhis2$LGA)
incidence_dhis2$conf_all_age = incidence_dhis2$AllagesOutpatientMalariaC
incidence_dhis2$date = as.Date(incidence_dhis2$date)
incidence_dhis2$year = format(incidence_dhis2$date, format="%Y")

# rescale each LGA's value so that the mean from 2017-2018 is the same (looking for seasonality, not transmission-intensity patterns)
incidence_2018 = incidence_dhis2[incidence_dhis2$year == 2018,]
mean_incidence_2018 = incidence_2018 %>% group_by(LGA) %>%
  summarise(mean_conf_all_age_2018 = mean(conf_all_age))
incidence_dhis2 = merge(incidence_dhis2, mean_incidence_2018, by='LGA', all.x=TRUE)
incidence_dhis2$rescaled_conf_all_age = incidence_dhis2$conf_all_age / incidence_dhis2$mean_conf_all_age_2018
incidence_dhis2$Archetype = incidence_dhis2$repDS

create_dhis2_incidence_plots(incidence_dhis2=incidence_dhis2, plot_output_dir=paste0(base_hbhi_filepath, '/project_notes/figures/validation/incidence_ria_2019'))
incidence_dhis2_ria_2010 = incidence_dhis2



# - - - - - - - - - - - - - - - - #
# incidence by archetype used for fitting
# - - - - - - - - - - - - - - - - #
filename_incidence_arch = paste0(base_hbhi_filepath, '/incidence/archetype_incidence_NGA_RIA_v3.csv') # year, month, adm2, clinical_u5, clinical_a5, conf_u5, conf_a5
incidence_arch = fread(filename_incidence_arch)
incidence_arch$Archetype = incidence_arch$seasonality
# rescale each archetype's value so that the mean from 2017-2018 is the same (looking for seasonality, not transmission-intensity patterns)
incidence_arch_means = incidence_arch %>% group_by(Archetype) %>%
  summarise(mean_incidence = mean(incidence))
incidence_arch = merge(incidence_arch, incidence_arch_means, by='Archetype', all.x=TRUE)
incidence_arch$rescaled_incidence = incidence_arch$incidence / incidence_arch$mean_incidence





# plot rescaled timeseries by month, faceted by archetype, only in 2018, color by State
incidence_dhis2 = incidence_dhis2_lga_2019[!is.na(incidence_dhis2_lga_2019$Archetype) & incidence_dhis2_lga_2019$year == 2018,]
# within-archetype monthly average (across LGAs in 2018)
incidence_arc_ave = incidence_dhis2 %>% group_by(Archetype, month) %>%
  summarise(mean_rescaled_conf_all_age = mean(rescaled_conf_all_age, na.rm=TRUE))
gg = ggplot() + 
  geom_line(data=incidence_dhis2[incidence_dhis2$year==2018], aes(x=month, y=rescaled_conf_all_age, color=LGA, group_by=LGA, linetype=factor(year))) + # color=adm1
  geom_line(data=incidence_arch, aes(x=month, y=rescaled_incidence), color='black', size=2) + 
  geom_line(data=incidence_arc_ave, aes(x=month, y=mean_rescaled_conf_all_age), color=rgb(0.2,0,0,0.5), size=2) + 
  coord_cartesian(ylim = c(0,4)) +
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('Archetype', nrow=5, scales='free')
ggsave(paste0(base_hbhi_filepath, '/project_notes/figures/validation/incidence_lga_2019/rescaled_dhis2_monthly_cases_by_archetype_2018_withFitLine2.png'), gg, width=25, height=12)  # _colorState

incidence_dhis2 = incidence_dhis2_lga_2022
gg = ggplot() + 
  geom_line(data=incidence_dhis2[incidence_dhis2$year %in% c(2018, 2019)], aes(x=month, y=rescaled_conf_all_age, color=adm1, group_by=LGA, linetype=factor(year))) + 
  geom_line(data=incidence_arch, aes(x=month, y=rescaled_incidence), color='black', size=2) + 
  coord_cartesian(ylim = c(0,4)) +
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('Archetype', nrow=5, scales='free')
ggsave(paste0(base_hbhi_filepath, '/project_notes/figures/validation/incidence_lga_2022/rescaled_dhis2_monthly_cases_by_archetype_2019_colorState_withFitLine.png'), gg, width=25, height=12)


incidence_dhis2 = incidence_dhis2_ria_2010
gg = ggplot() + 
  geom_line(data=incidence_dhis2[incidence_dhis2$year==2018], aes(x=month, y=rescaled_conf_all_age, color=LGA, group_by=LGA, linetype=factor(year))) + 
  geom_line(data=incidence_arch, aes(x=month, y=rescaled_incidence), color='black', size=2) + 
  coord_cartesian(ylim = c(0,4)) +
  theme_bw()+
  theme(legend.position="none") + 
  facet_wrap('Archetype', nrow=5, scales='free')
ggsave(paste0(base_hbhi_filepath, '/project_notes/figures/validation/incidence_ria_2019/rescaled_dhis2_monthly_cases_by_archetype_2018_withFitLine.png'), gg, width=25, height=12)





#####################
# do clusters look the same each time?
for(ii in 1:5){
  incidence_2018_2019 = rbind(incidence_2018, incidence_2019)
  incidence_state_ave = incidence_2018_2019[incidence_2018_2019$year %in% c(2018, 2019),] %>% group_by(adm1, month) %>%
    summarise(mean_rescaled_conf_all_age = mean(rescaled_conf_all_age, na.rm=TRUE))
  
  # get cluster of states to include in same archetype based on rescaled monthly values
  num_clusters = 4
  monthly_vals = dcast(incidence_state_ave[!is.na(incidence_state_ave$mean_rescaled_conf_all_age),], adm1 ~ month, value.var="mean_rescaled_conf_all_age")
  monthly_val_states = monthly_vals$adm1
  monthly_vals = monthly_vals[,-which(colnames(monthly_vals) =='adm1')]
  kmeans_result = kmeans(monthly_vals, centers=num_clusters, nstart=15, iter.max=30)
  cluster_results = data.frame('adm1' = monthly_val_states, 'cluster' = kmeans_result$cluster)
  
  # get means within clusters for each month
  incidence_2018_2019 = merge(incidence_2018_2019, cluster_results, by='adm1', all=TRUE)
  incidence_clust_ave = incidence_2018_2019[incidence_2018_2019$year %in% c(2018, 2019),] %>% group_by(cluster, month) %>%
    summarise(mean_rescaled_conf_all_age = mean(rescaled_conf_all_age, na.rm=TRUE))
  
  # plot coloring DS according to archetype DS
  clust_cols = rainbow(num_clusters, alpha=0.4)
  country_mask$color = 'black'
  for(ss in 1:nrow(cluster_results)){
    country_mask$color[country_mask$State == get_compatible_state_name(cluster_results$adm1[ss])] = clust_cols[cluster_results$cluster[ss]]
  }
  png(paste0(base_hbhi_filepath, '/project_notes/figures/validation/incidence_lga_2022/map_of_cluster', num_clusters, '_lgas_sample',ii,'.png'), width=5, height=5, units='in', res=900)
  plot(country_mask, col=country_mask$color)  
  dev.off()
}


# 
# # also cluster based on latitude (states of similar latitude more likely to have similar seasonality)
# incidence_2018_2019 = rbind(incidence_2018, incidence_2019)
# incidence_2018_2019$LGA = sapply(incidence_2018_2019$LGA, update_lga_name_match)
# pop_arch2 = pop_arch
# pop_arch2$LGA = sapply(pop_arch2$LGA, update_lga_name_match)
# incidence_2018_2019 = merge(incidence_2018_2019, pop_arch2[,c('LGA','lat')], all.x=TRUE)
# incidence_state_ave = incidence_2018_2019[incidence_2018_2019$year %in% c(2018, 2019),] %>% group_by(adm1, month) %>%
#   summarise(mean_rescaled_conf_all_age = mean(rescaled_conf_all_age, na.rm=TRUE),
#             mean_lat = mean(lat))
# 
# 
# # get cluster of states to include in same archetype based on rescaled monthly values
# num_clusters = 6
# monthly_vals = dcast(incidence_state_ave[!is.na(incidence_state_ave$mean_rescaled_conf_all_age),], adm1 ~ month, value.var="mean_rescaled_conf_all_age")
# monthly_val_states = monthly_vals$adm1
# monthly_vals = monthly_vals[,-which(colnames(monthly_vals) =='adm1')]
# kmeans_result = kmeans(monthly_vals, centers=num_clusters, nstart=15, iter.max=30)
# cluster_results = data.frame('adm1' = monthly_val_states, 'cluster' = kmeans_result$cluster)
# 
# # get means within clusters for each month
# incidence_2018_2019 = merge(incidence_2018_2019, cluster_results, by='adm1', all=TRUE)
# incidence_clust_ave = incidence_2018_2019[incidence_2018_2019$year %in% c(2018, 2019),] %>% group_by(cluster, month) %>%
#   summarise(mean_rescaled_conf_all_age = mean(rescaled_conf_all_age, na.rm=TRUE))
# 
# # plot coloring DS according to archetype DS
# clust_cols = rainbow(num_clusters, alpha=0.4)
# country_mask$color = 'black'
# for(ss in 1:nrow(cluster_results)){
#   country_mask$color[country_mask$State == get_compatible_state_name(cluster_results$adm1[ss])] = clust_cols[cluster_results$cluster[ss]]
# }
# png(paste0(base_hbhi_filepath, '/project_notes/figures/validation/incidence_lga_2022/map_of_cluster', num_clusters, '_lgas_sample',ii,'_withLat.png'), width=5, height=5, units='in', res=900)
# plot(country_mask, col=country_mask$color)  
# dev.off()
# 
# gg = ggplot() + 
#   geom_line(data=incidence_2018_2019, aes(x=month, y=rescaled_conf_all_age, color=adm1, group_by=LGA, linetype=factor(year))) + 
#   geom_line(data=incidence_clust_ave, aes(x=month, y=mean_rescaled_conf_all_age), color='black', size=2) + 
#   coord_cartesian(ylim = c(0,4)) +
#   theme_bw()+
#   theme(legend.position="none") + 
#   facet_wrap('cluster', nrow=2, scales='free')
# ggsave(paste0(base_hbhi_filepath, '/project_notes/figures/validation/incidence_lga_2022/rescaled_dhis2_monthly_cases_cluster',num_clusters,'_2018_2019_average_withLat.png'), gg, width=25, height=12)




##############################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# is prevalence too different within an archetype to use the same sweeps?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
##############################################################################



# - - - - - - - - - - - - - - - - #
# DHS PfPR data
# - - - - - - - - - - - - - - - - #
dhs_pfpr_2010_fname = file.path(dhs_pfpr_filepath, 'DS10_pfpr_ym_micro.csv')
dhs_pfpr_2015_fname = file.path(dhs_pfpr_filepath, 'DS15_pfpr_ym_micro.csv')
dhs_pfpr_2018_fname = file.path(dhs_pfpr_filepath, 'DS18_pfpr_ym_micro.csv')
dhs_pfpr_2010 = fread(dhs_pfpr_2010_fname)
dhs_pfpr_2015 = fread(dhs_pfpr_2015_fname)
dhs_pfpr_2018 = fread(dhs_pfpr_2018_fname)
# # reformat 2018 to match other years
# dhs_pfpr_2018$time2= sapply(dhs_pfpr_2018$time2, function(x) paste0(strsplit(x, '-')[[1]][2],"-",strsplit(x, '-')[[1]][1]))
# colnames(dhs_pfpr_2018)[colnames(dhs_pfpr_2018)=='mean'] = 'p_test_mean'
# colnames(dhs_pfpr_2018)[colnames(dhs_pfpr_2018)=='sd'] = 'p_test_sd'
# colnames(dhs_pfpr_2018)[colnames(dhs_pfpr_2018)=='se'] = 'p_test_std.error'
# combine dhs from multiple years
dhs_pfpr = rbind(dhs_pfpr_2010, dhs_pfpr_2015, dhs_pfpr_2018)
colnames(dhs_pfpr)[colnames(dhs_pfpr) == 'PfPr'] = 'p_test_mean'
dhs_pfpr = dhs_pfpr[!is.na(dhs_pfpr$p_test_mean),]
dhs_pfpr$month = sapply(dhs_pfpr$time2, function(x) strsplit(x, '-')[[1]][1])
dhs_pfpr$year = sapply(dhs_pfpr$time2, function(x) strsplit(x, '-')[[1]][2])
dhs_pfpr[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
colnames(dhs_pfpr)[colnames(dhs_pfpr) == 'Number of Kids'] = 'num_U5_sampled'
# dhs_pfpr$LGA = toupper(dhs_pfpr$LGA)



# plot coloring DS according to archetype DS
# country_mask$LGA = toupper(country_mask$NOMDEP)
colors_range_0_to_1 = add.alpha(pals::parula(101), alpha=0.5)
colors_range_nsamples = rev(pals::coolwarm(max(dhs_pfpr$num_U5_sampled)*2+1))[max(dhs_pfpr$num_U5_sampled):(max(dhs_pfpr$num_U5_sampled)*2+1)]
for(yy in unique(dhs_pfpr$year)){
  dhs_pfpr_yy = dhs_pfpr[dhs_pfpr$year == yy,]
  dhs_pfpr_yy = dhs_pfpr_yy[dhs_pfpr_yy$num_U5_sampled>0,]
  country_mask$pfpr_yy = NA
  country_mask$nsamples_yy = NA
  for(ss in 1:nrow(dhs_pfpr_yy)){
    match_rows = which(country_mask$LGA == dhs_pfpr_yy$LGA[ss])
    if(length(match_rows>0)){
      country_mask$pfpr_yy[match_rows] = colors_range_0_to_1[round(100*dhs_pfpr_yy$p_test_mean[ss])+1]
      country_mask$nsamples_yy[match_rows] = colors_range_nsamples[dhs_pfpr_yy$num_U5_sampled[ss]]
    }
  }
  
  png(paste0(base_hbhi_filepath, '/project_notes/figures/validation/incidence_lga_2022/map_of_DHS',yy,'_pfpr.png'), width=12, height=5, units='in', res=900)
  
  layout(matrix(c(1,1,1,2,3,3,3,4, 1,1,1,5,3,3,3,6),nrow=2, byrow=TRUE))
  # microscopy prevalence
  plot(country_mask, col=country_mask$pfpr_yy, main=paste0('microscopy prevalence - DHS ', yy))
  legend_image = as.raster(matrix(rev(colors_range_0_to_1[1+round(seq(0,1,length.out=20)*100)]), ncol=1))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'PfPR')
  text(x=1.5, y = seq(0,1,length.out=5), labels = seq(0,1,length.out=5))
  rasterImage(legend_image, 0, 0, 1,1)
  
  
  # number of samples
  plot(country_mask, col=country_mask$nsamples_yy, main=paste0('num U5 sampled - DHS ', yy))
  legend_image = as.raster(matrix(rev(colors_range_nsamples[round(seq(1,max(dhs_pfpr$num_U5_sampled),length.out=20))]), ncol=1))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'num')
  text(x=1.5, y = seq(0,1,length.out=5), labels = seq(0,max(dhs_pfpr$num_U5_sampled),length.out=5))
  rasterImage(legend_image, 0, 0, 1,1)
  dev.off()
}
  
  
  
  
  
  # png(paste0(base_hbhi_filepath, '/project_notes/figures/validation/incidence_lga_2022/map_of_DHS',yy,'_pfpr.png'), width=5, height=5, units='in', res=900)
  # par(mfrow=c(1,2))
  # plot(country_mask, col=country_mask$pfpr_yy, main=paste0('microscopy prevalence - DHS ', yy))  
  # plot(country_mask, text=country_mask$nsamples_yy, main=paste0('num U5 sampled - DHS ', yy))  

par(mfrow=c(1,1))

