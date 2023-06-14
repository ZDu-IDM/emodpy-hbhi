# comparison_ITN_use_sim_DHS.R

library(ggplot2)
library(gridExtra)
library(lubridate)

hbhi_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
# simulation output folder
sim_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_to_present/BDI_2010_2020_allInter')
ifelse(!dir.exists(paste0(sim_output_dir, '/_plots')), dir.create(paste0(sim_output_dir, '/_plots')), FALSE)
indoor_net_protection = 0.75

# read in monthly ITN use from simulation
sim_itn_use = read.csv(paste0(sim_output_dir, '/MonthlyUsageLLIN.csv'))
sim_itn_use$date = as.Date(sim_itn_use$date)
# sim_itn_use$year = lubridate::year(sim_itn_use$date)
# sim_itn_use$month = lubridate::month(sim_itn_use$date)
sim_itn_use$itn_use_cov = sim_itn_use$Bednet_Using / sim_itn_use$Statistical.Population / indoor_net_protection
sim_itn_use = sim_itn_use[,which(colnames(sim_itn_use) %in% c('admin_name','date','Run_Number','itn_use_cov'))]

# read in ITN use observed in DHS
dhs_years = c(2010, 2012, 2016)
dhs_months = c(10,12,10)
itn_age_names = c('itn_u5','itn_5_10','itn_10_15', 'itn_15_20','itn_o20')   # 'itn_all'
weight_each_group = c(0.182975962, 0.150899016, 0.121332579, 0.104243898, 0.44055)
for(yy in 1:length(dhs_years)){
  dhs_cur = read.csv(paste0(hbhi_dir, '/estimates_from_DHS/DHS_admin_', dhs_years[yy],'.csv'))
  # dhs_cur$year = dhs_years[yy]
  dhs_cur$date = as.Date(paste0(dhs_years[yy],'-', dhs_months[yy],'-01'))
  dhs_cur = dhs_cur[,which(colnames(dhs_cur) %in% c('NOMDEP',  'date', paste0(itn_age_names,'_num_total'), paste0(itn_age_names,'_num_true') ))]
  for(aa in 1:length(itn_age_names)){
    dhs_cur[[paste0(itn_age_names[aa],'_dhs_est')]] = dhs_cur[[paste0(itn_age_names[aa],'_num_true')]] / dhs_cur[[paste0(itn_age_names[aa],'_num_total')]]
  }
  if(yy==1){
    dhs_itn_use = dhs_cur
  } else{
    dhs_itn_use = rbind(dhs_itn_use, dhs_cur)
  }
}
dhs_itn_use$Run_Number=0

# roughly weight dhs rate by number of individuals in each age group
dhs_itn_use$itn_WAge_dhs_est = 0
for(aa in 1:length(itn_age_names)){
  dhs_itn_use$itn_WAge_dhs_est = dhs_itn_use$itn_WAge_dhs_est + dhs_itn_use[[paste0(itn_age_names[aa],'_dhs_est')]] * weight_each_group[aa]
}
dhs_itn_use$dhs_size_WAge = apply(dhs_itn_use[,grep(pattern= '_num_total', colnames(dhs_itn_use))],1,min)

sim_dhs_itn_use = merge(sim_itn_use, dhs_itn_use, by.x=c('admin_name','date','Run_Number'), by.y=c('NOMDEP','date','Run_Number'), all.x=TRUE)



#################
# plots
#################

# plot ITN use through time for a single admin, across all seeds
admin_name1 = 'Gitega'
sim_itn_use_1admin = sim_itn_use[sim_itn_use$admin_name == admin_name1,]
ggplot(sim_itn_use_1admin, aes(x=date, y=itn_use_cov, group=Run_Number)) + 
  geom_line(aes(color=Run_Number), show.legend=FALSE) + 
  ggtitle(paste0('Simulated LLIN use: ', admin_name1, ' (multiple runs)')) + 
  ylab('LLIN use (all ages)') + 
  theme_classic()

# all_admin_names = unique(sim_dhs_itn_use$admin_name)
# gg_list = list()
# for(aa in 1:length(all_admin_names)){
#   itn_use_1admin = sim_dhs_itn_use[sim_dhs_itn_use$admin_name == all_admin_names[aa],]
#   gg_list[[aa]] = ggplot(itn_use_1admin, ) + 
#     geom_line(aes(x=date, y=itn_use_cov, group=Run_Number, color=Run_Number), show.legend=FALSE) + 
#     geom_point(aes(x=date, y=itn_all_dhs_est, size=itn_all_num_total)) +  #, show.legend=FALSE) + 
#     ggtitle(all_admin_names[aa]) + 
#     ylab('LLIN use (all ages)') + 
#     theme_classic()
# }
# # grid.arrange(grobs=gg_list, ncol=5)
# ggsave(file = paste0(sim_output_dir, '/_plots/LLIN_use_comparison_sim_DHSallAges.pdf'), arrangeGrob(grobs = gg_list, ncol = 5), width=20, height=30, units='in')



gg_list = list()
for(aa in 1:length(all_admin_names)){
  itn_use_1admin = sim_dhs_itn_use[sim_dhs_itn_use$admin_name == all_admin_names[aa],]
  gg_list[[aa]] = ggplot(itn_use_1admin, ) + 
    geom_line(aes(x=date, y=itn_use_cov, group=Run_Number, color=Run_Number), show.legend=FALSE) + 
    geom_point(aes(x=date, y=itn_u5_dhs_est, size=itn_u5_num_total)) +  #, show.legend=FALSE) + 
    ggtitle(all_admin_names[aa]) + 
    ylab('LLIN use (DHS=U5)') + 
    theme_classic()
}
# grid.arrange(grobs=gg_list, ncol=5)
ggsave(file = paste0(sim_output_dir, '/_plots/LLIN_use_comparison_sim_DHSu5.pdf'), arrangeGrob(grobs = gg_list, ncol = 5), width=20, height=30, units='in')






gg_list = list()
for(aa in 1:length(all_admin_names)){
  itn_use_1admin = sim_dhs_itn_use[sim_dhs_itn_use$admin_name == all_admin_names[aa],]
  gg_list[[aa]] = ggplot(itn_use_1admin, ) + 
    geom_line(aes(x=date, y=itn_use_cov, group=Run_Number, color=Run_Number), show.legend=FALSE) + 
    geom_point(aes(x=date, y=itn_WAge_dhs_est, size=dhs_size_WAge)) +  #, show.legend=FALSE) + 
    ggtitle(all_admin_names[aa]) + 
    ylab('LLIN use (DHS, all ages)') + 
    theme_classic()
}
# grid.arrange(grobs=gg_list, ncol=5)
ggsave(file = paste0(sim_output_dir, '/_plots/LLIN_use_comparison_sim_DHSWAge.pdf'), arrangeGrob(grobs = gg_list, ncol = 5), width=20, height=30, units='in')








plot(sim_dhs_itn_use$itn_use_cov, sim_dhs_itn_use$itn_WAge_dhs_est, type='p', pch=20, col=as.factor(lubridate::year(sim_dhs_itn_use$date)), cex=sim_dhs_itn_use$dhs_size_WAge/max(sim_dhs_itn_use$dhs_size_WAge, na.rm=TRUE)*3, xlab='simulated ITN use', ylab='admin-level DHS use', bty='L', xlim=c(0,0.9), ylim=c(0,0.9))
lines(c(0,1), c(0,1))

length(which(sim_dhs_itn_use$itn_use_cov[which(lubridate::year(sim_dhs_itn_use$date) == 2016)] > sim_dhs_itn_use$itn_WAge_dhs_est[which(lubridate::year(sim_dhs_itn_use$date) == 2016)]))
length(which(sim_dhs_itn_use$itn_use_cov[which(lubridate::year(sim_dhs_itn_use$date) == 2016)] < sim_dhs_itn_use$itn_WAge_dhs_est[which(lubridate::year(sim_dhs_itn_use$date) == 2016)]))






####################################################################
# plot with cluster outputs instead of aggregated to admin
####################################################################


# read in ITN use observed in DHS
dhs_years = c(2010, 2012, 2016)
dhs_months = c(10,12,10)
itn_age_names = c('itn_u5','itn_5_10','itn_10_15', 'itn_15_20','itn_o20')   # 'itn_all'
weight_each_group = c(0.182975962, 0.150899016, 0.121332579, 0.104243898, 0.44055)
for(yy in 1:length(dhs_years)){
  dhs_cur = read.csv(paste0(hbhi_dir, '/estimates_from_DHS/DHS_cluster_outputs_', dhs_years[yy],'.csv'))
  # dhs_cur$year = dhs_years[yy]
  dhs_cur$date = as.Date(paste0(dhs_years[yy],'-', dhs_months[yy],'-01'))
  dhs_cur = dhs_cur[,which(colnames(dhs_cur) %in% c('NOMDEP',  'date', paste0(itn_age_names,'_num_total'), paste0(itn_age_names,'_num_true') ))]
  for(aa in 1:length(itn_age_names)){
    dhs_cur[[paste0(itn_age_names[aa],'_dhs_est')]] = dhs_cur[[paste0(itn_age_names[aa],'_num_true')]] / dhs_cur[[paste0(itn_age_names[aa],'_num_total')]]
  }
  if(yy==1){
    dhs_itn_use_clust = dhs_cur
  } else{
    dhs_itn_use_clust = rbind(dhs_itn_use_clust, dhs_cur)
  }
}


# roughly weight dhs rate by number of individuals in each age group
dhs_itn_use_clust$itn_WAge_dhs_est = 0
for(aa in 1:length(itn_age_names)){
  dhs_itn_use_clust$itn_WAge_dhs_est = dhs_itn_use_clust$itn_WAge_dhs_est + dhs_itn_use_clust[[paste0(itn_age_names[aa],'_dhs_est')]] * weight_each_group[aa]
}
dhs_itn_use_clust$dhs_size_WAge = apply(dhs_itn_use_clust[,grep(pattern= '_num_total', colnames(dhs_itn_use_clust))],1,min)


min_cluster_size = min(dhs_itn_use_clust$dhs_size_WAge, na.rm=TRUE)
max_cluster_size = max(dhs_itn_use_clust$dhs_size_WAge, na.rm=TRUE)
gg_list = list()
for(aa in 1:length(all_admin_names)){
  itn_use_1admin = sim_dhs_itn_use[sim_dhs_itn_use$admin_name == all_admin_names[aa],]
  dhs_itn_use_clust_1admin = dhs_itn_use_clust[dhs_itn_use_clust$NOMDEP == all_admin_names[aa],]
  gg_list[[aa]] = ggplot(itn_use_1admin, ) + 
    geom_line(aes(x=date, y=itn_use_cov, group=Run_Number, color=Run_Number), show.legend=FALSE) + 
    geom_point(data=dhs_itn_use_clust_1admin, aes(x=date, y=itn_WAge_dhs_est, size=dhs_size_WAge), show.legend=FALSE) + 
    scale_size_continuous(breaks=seq(min_cluster_size,max_cluster_size,1), range = c(1,3)) + 
    ggtitle(all_admin_names[aa]) + 
    ylab('LLIN use (DHS, all ages)') + 
    theme_classic()
}
# grid.arrange(grobs=gg_list, ncol=5)
ggsave(file = paste0(sim_output_dir, '/_plots/LLIN_use_comparison_sim_DHSClusterWAge.pdf'), arrangeGrob(grobs = gg_list, ncol = 5), width=20, height=30, units='in')
ggsave(file = paste0(sim_output_dir, '/_plots/LLIN_use_comparison_sim_DHSClusterWAge.png'), arrangeGrob(grobs = gg_list, ncol = 8), width=32, height=24, units='in')






##############################################################
# compare simulation with back-of-envelope expected use
##############################################################
start_year = 2010
end_year = 2020
# read in simulation input files
itn_mass = read.csv(paste0(hbhi_dir, '/simulation_inputs/interventions_2010_2020/itn_mass_coverages_mort_2010_2020.csv'))
itn_anc = read.csv(paste0(hbhi_dir, '/simulation_inputs/interventions_2010_2020/anc_itn_use_coverages_mort_2010_2020.csv'))
itn_epi = read.csv(paste0(hbhi_dir, '/simulation_inputs/interventions_2010_2020/epi_itn_use_coverages_mort_2010_2020.csv'))
itn_use_seasonality = read.csv(paste0(hbhi_dir, '/simulation_inputs/ITN_use_seasonality.csv'))
itn_use_scalar = rep(c(rep(itn_use_seasonality$itn_use_scalar, each=30),rep(itn_use_seasonality$itn_use_scalar[12],5)),times=length(start_year:end_year))
# plot(itn_use_scalar)
# abline(v=c(365*(0:15)),col=rgb(1,0,0,0.2), lwd=3)

itn_age_names = c('itn_u5','itn_5_10','itn_10_15', 'itn_15_20','itn_o20')   # 'itn_all'
weight_each_group = c(0.182975962, 0.150899016, 0.121332579, 0.104243898, 0.44055)
net_life_lognormal_mu = 5.892646
net_life_lognormal_sigma = 0.8

cur_admin = 'Bubanza'
itn_mass = itn_mass[(itn_mass$admin_name == cur_admin) & (itn_mass$seed ==1),]
itn_anc = itn_anc[(itn_anc$admin_name == cur_admin) & (itn_anc$seed ==1),]
itn_epi = itn_epi[(itn_epi$admin_name == cur_admin) & (itn_epi$seed ==1),]

# approximate nets given each day of simulation
approx_pop = 1000
daily_nets_dist = rep(0, 365*length(start_year:end_year))
# from mass distributions
for(rr in 1:nrow(itn_mass)){
  new_nets = 0
  for(aa in 1:length(itn_age_names)){
    new_nets = new_nets + approx_pop * weight_each_group[aa] * itn_mass[[itn_age_names[aa]]][rr]
  }
  daily_nets_dist[itn_mass$simday[rr]] = daily_nets_dist[itn_mass$simday[rr]] + new_nets
}
# from ANC visits
# assumes birth rate of 40 births per 1000 person per year, 2 people covered by ANC net
daily_anc_opportunities = 40/365 * 2
for(yy in 1:length(start_year:end_year)){
  # get coverage for closest year
  cur_coverage = itn_anc$coverage[which.min(abs(itn_anc$year-(start_year:end_year)[yy]))]
  ave_daily_nets = daily_anc_opportunities * cur_coverage
  daily_nets_dist[(1+(yy-1)*365):(yy*365)] = daily_nets_dist[(1+(yy-1)*365):(yy*365)] + ave_daily_nets
}
# from EPI
# assumes 1st and 3rd birthday rate of 40 births per 1000 person per year
daily_1_3_birthdays_opportunities = 40/365
for(yy in 1:length(start_year:end_year)){
  # get coverage for closest year
  itn_epi_1 = itn_epi[itn_epi$birthday_age==1,]
  itn_epi_3 = itn_epi[itn_epi$birthday_age==3,]
  cur_coverage_1 = itn_epi_1$coverage[which.min(abs(itn_epi_1$year-(start_year:end_year)[yy]))]
  cur_coverage_3 = itn_epi_3$coverage[which.min(abs(itn_epi_3$year-(start_year:end_year)[yy]))]
  ave_daily_nets = daily_1_3_birthdays_opportunities * (cur_coverage_1 + cur_coverage_3)
  daily_nets_dist[(1+(yy-1)*365):(yy*365)] = daily_nets_dist[(1+(yy-1)*365):(yy*365)] + ave_daily_nets
}

plot(cumsum(daily_nets_dist))


# calculate nets remaining given retention distribution
remaining_nets = rep(0, 365*length(start_year:end_year))
for(dd in 1:length(daily_nets_dist)){
  net_shadow = daily_nets_dist[dd] * ((1-plnorm(0:(365*length(start_year:end_year)), meanlog=net_life_lognormal_mu , sdlog=net_life_lognormal_sigma)))
  remaining_nets[dd:length(remaining_nets)] = remaining_nets[dd:length(remaining_nets)] +  net_shadow[1:length(dd:length(remaining_nets))] * (1-remaining_nets[dd:length(remaining_nets)]/approx_pop)
}

# plot(remaining_nets/approx_pop, type='p', pch=20)


# rescale for seasonal use rates
plot(remaining_nets/approx_pop * itn_use_scalar, type='l', pch=20, bty='L', lwd=2, ylab=c('nets in use'), xlab='day of simulation', col=rgb(0,0.7,0.2))
abline(v=c(365*(0:15)),col=rgb(1,0,0,0.05), lwd=3)
# add line for simulation output
sim_itn_use_1admin = sim_dhs_itn_use[(sim_dhs_itn_use$admin_name == cur_admin) & (sim_dhs_itn_use$Run_Number == 0),]
lines(sim_itn_use_1admin$date - as.Date(paste0(start_year,'-01-01')), sim_itn_use_1admin$itn_use_cov, col='blue', lwd=2)
# add DHS points
points(sim_itn_use_1admin$date - as.Date(paste0(start_year,'-01-01')), sim_itn_use_1admin$itn_WAge_dhs_est, pch=20, cex=2)
legend('bottomright',c('back-of-envelope approximation','simulation output', 'DHS'), lwd=c(2,2,NA), pch=c(NA,NA,20), col=c(rgb(0,0.7,0.2),'blue', 'black'), bty='n')



####################################################################################################
# explore different net decay rates in back-of-envelope estimates - do they better match DHS data?
####################################################################################################
net_life_lognormal_mu = 6.3

# calculate nets remaining given retention distribution
remaining_nets_2 = rep(0, 365*length(start_year:end_year))
for(dd in 1:length(daily_nets_dist)){
  net_shadow = daily_nets_dist[dd] * ((1-plnorm(0:(365*length(start_year:end_year)), meanlog=net_life_lognormal_mu , sdlog=net_life_lognormal_sigma)))
  remaining_nets_2[dd:length(remaining_nets_2)] = remaining_nets_2[dd:length(remaining_nets_2)] +  net_shadow[1:length(dd:length(remaining_nets_2))] * (1-remaining_nets_2[dd:length(remaining_nets_2)]/approx_pop)
}

# plot(remaining_nets/approx_pop, type='p', pch=20)


# rescale for seasonal use rates
plot(remaining_nets_2/approx_pop * itn_use_scalar, type='l', pch=20, bty='L', lwd=2, ylab=c('nets in use'), xlab='day of simulation', col=rgb(0,0.7,0.2))
abline(v=c(365*(0:15)),col=rgb(1,0,0,0.05), lwd=3)
# add line for simulation output
sim_itn_use_1admin = sim_dhs_itn_use[(sim_dhs_itn_use$admin_name == cur_admin) & (sim_dhs_itn_use$Run_Number == 0),]
lines(sim_itn_use_1admin$date - as.Date(paste0(start_year,'-01-01')), sim_itn_use_1admin$itn_use_cov, col='blue', lwd=2)
# add DHS points
points(sim_itn_use_1admin$date - as.Date(paste0(start_year,'-01-01')), sim_itn_use_1admin$itn_WAge_dhs_est, pch=20, cex=2)
legend('bottomright',c(paste0('back-of-envelope approximation, mu=',net_life_lognormal_mu),'simulation output', 'DHS'), lwd=c(2,2,NA), pch=c(NA,NA,20), col=c(rgb(0,0.7,0.2),'blue', 'black'), bty='n')


get_lognormal_mu_from_A_halflife(A_halflife_days=(2*365), itn_lognorm_sigma=0.8)

