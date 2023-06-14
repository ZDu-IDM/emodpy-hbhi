#################################################################################################
# check_smc_impact.R
# Monique Ambrose
# Jan 2023
# 
# Purpose: see whether SMC assumptions we currently make for simulations produces 
#    results that match real-world observations
# 
##################################################################################################
library(data.table)
library(dplyr)
library(viridis)
library(ggplot2)
library(gridExtra)

# sim output folder
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
base_filepath = paste0(user_path, '/Dropbox (IDM)/NU_collaboration')
script_dir = paste0(user_path, '/Documents/malaria-nga-snt22')
hbhi_dir = paste0(user_path, '/Dropbox (IDM)/NU_collaboration/hbhi_nigeria/snt_2022')
sim_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_to_present/NGA_toPresent_allInter_v12')
sim_output_filepath = paste0(sim_output_dir, '/malariaBurden_withAdjustments.csv')
sim_version_name=''
smc_coverage_multiplier=1
dhis2_filepath = paste0(base_filepath, '/nigeria_who/NGA_2022_SNT/_Submitted_data/Routine data/monthly_lga.dta')
dhis_string=''
u5_dhis_col = 'conf_u5'
o5_dhis_col = 'conf_ov5'
plot_month = 10

save_plot_filepath = paste0(sim_output_dir, '/_plots/_validation')
pop_arch_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')

source(paste0(script_dir,'/standardize_admin_names.R'))
source(paste0(script_dir, '/simulation/checks_validation/functions_get_data_smc_impact.R'))

if(!dir.exists(paste0(sim_output_dir, '/_plots'))) dir.create(paste0(sim_output_dir, '/_plots'))
if(!dir.exists(paste0(sim_output_dir, '/_plots/_validation'))) dir.create(paste0(sim_output_dir, '/_plots/_validation'))

##========================================##
# read in,format, and merge datasets
##========================================##

# --- standardized LGA names --- #
pop_arch = fread(pop_arch_filepath)

# --- SMC data --- #
# information on which DS received SMC in each year
smc_info_filename = paste0(hbhi_dir, "/simulation_inputs/interventions_2010_toPresent/smc_2010_toPresent.csv")
smc_info = get_smc_data(smc_info_filename)


# --- dhis2 data --- #
# dhis2 data for the observed cases at health facilities
# get aggregated counts of cases in each admin-month-year (across multiple health facilities)
dhis2_data_sums = get_dhis2_data(dhis2_filepath, pop_arch)
years_included = sort(unique(dhis2_data_sums$year))


# --- simulation output --- #
num_nmf_month_u5 = 0.22  # estimated monthly probability of NMF for U5
num_nmf_month_o5 = 0.11  # estimated monthly probability of NMF for O5
rel_prob_cm_o5 = 1  # previously: 0.5  # relative to U5, what is the probability someone O5 seeks treatment for fever

sim_burden = get_sim_data(sim_output_filepath=sim_output_filepath, rel_prob_cm_o5=rel_prob_cm_o5, num_nmf_month_u5=num_nmf_month_u5, num_nmf_month_o5=num_nmf_month_o5, years_included=years_included)


# --- merge datasets --- #
merged_df = merge_smc_datasets(smc_info, dhis2_data_sums, sim_burden)
# mean(merged_df$New_clinical_cases_U5/merged_df$Pop_U5) / mean(merged_df$New_clinical_cases_U5/merged_df$Pop_U5)
# mean((merged_df$NMF_u5 - merged_df$m_NMF_u5)/merged_df$Pop_U5) / mean(merged_df$New_clinical_cases_U5/merged_df$Pop_U5)
# mean(merged_df$m_NMF_u5/merged_df$Pop_U5) / mean(merged_df$New_clinical_cases_U5/merged_df$Pop_U5)
# mean(merged_df$PfPR_U5)




# create data table for plotting
dt = as.data.table(merged_df)
# Add some useful columns for plotting
dt[,months_received:=sum(received_smc==TRUE,na.rm=TRUE),by=.(year,admin_name)]
dt[,received_this_year:=months_received>0]
dt[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
dt[,sim_maltreat_u5:=(New_clinical_cases_U5 + m_NMF_u5)]
dt[,sim_maltreat_ov5:=((New_Clinical_Cases - New_clinical_cases_U5 + m_NMF_o5) * rel_prob_cm_o5)]


# change column names to agree with expected (hard-coded) values in script
if(u5_dhis_col != 'conf_u5'){
  colnames(dt)[colnames(dt)=='conf_u5'] = 'original_conf_u5'
  colnames(dt)[colnames(dt)==u5_dhis_col] = 'conf_u5'
  u5_dhis_col = 'conf_u5'
}
if(o5_dhis_col != 'conf_ov5'){
  colnames(dt)[colnames(dt)=='conf_ov5'] = 'original_conf_ov5'
  colnames(dt)[colnames(dt)==o5_dhis_col] = 'conf_ov5'
  o5_dhis_col = 'conf_ov5'
}

##=============================================##
# create plots monthly patterns (for a single simulation scenario)
##=============================================##


# scatterplots showing monthly simulation patterns (bottom row) and monthly dhis2 values (top row)
if(TRUE){
  # show scatterplot of treated malaria cases in U5 and O5 (from malaria and NMF)
  years_scatter = 2017:2020
  max_sim=350
  max_dhis=12500
  for(yy in 1:length(years_scatter)){
    png(paste(save_plot_filepath, '/compare_ov5_u5_sim_dhis2_monthly_maltreat_',sim_version_name,'_CM_NMF_adjusted_',years_scatter[yy],'.png', sep=''), width=9, height=7, units='in', res=900)
    par(mfrow=c(2,2))
    dt_yy = dt %>% filter(year == years_scatter[yy])
    # dhis2 data
    plot(NA, ylim=c(0,max_dhis), xlim=c(0.5, 12.5), xlab='month', ylab='Observed cases U5', bty='L', main=paste('dhis2 -', years_scatter[yy]))
    for(mm in 1:12){
      dt_yy_mm = dt_yy %>% filter(month == mm)
      values = dt_yy_mm[[u5_dhis_col]]
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(dt_yy_mm$received_smc)])
    }
    plot(NA, ylim=c(0,max_dhis), xlim=c(0.5, 12.5), xlab='month', ylab='Observed cases O5', bty='L', main=paste('dhis2 -', years_scatter[yy]))
    for(mm in 1:12){
      dt_yy_mm = dt_yy %>% filter(month == mm)
      values = dt_yy_mm[[o5_dhis_col]]
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(dt_yy_mm$received_smc)])
    }
    # simulation  
    plot(NA, ylim=c(0,max_sim), xlim=c(0.5, 12.5), xlab='month', ylab='Observed cases U5', bty='L', main=paste('sim -', years_scatter[yy]))
    for(mm in 1:12){
      dt_yy_mm = dt_yy %>% filter(month == mm)
      values = dt_yy_mm$sim_maltreat_u5
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(dt_yy_mm$received_smc)])
    }
    plot(NA, ylim=c(0,max_sim), xlim=c(0.5, 12.5), xlab='month', ylab='Observed cases O5', bty='L', main=paste('sim -', years_scatter[yy]))
    for(mm in 1:12){
      dt_yy_mm = dt_yy %>% filter(month == mm)
      values = dt_yy_mm$sim_maltreat_ov5
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(dt_yy_mm$received_smc)])
    }
    dev.off()
  }
  par(mfrow=c(1,1))
  

  
  # show scatterplot of treated malaria cases in U5 and O5 (from malaria and NMF) - only for admins that received SMC that year
  years_scatter = 2017:2020
  max_sim=350
  max_dhis=12500
  for(yy in 1:length(years_scatter)){
    png(paste(save_plot_filepath, '/compare_ov5_u5_sim_dhis2_monthly_cases_',sim_version_name,'_CM_NMF_adjusted_',years_scatter[yy],'_receivedSMC.png', sep=''), width=9, height=7, units='in', res=900)
    par(mfrow=c(2,2))
    dt_yy = dt %>% filter(year == years_scatter[yy], received_this_year)
    # dhis2 data
    plot(NA, ylim=c(0,max_dhis), xlim=c(0.5, 12.5), xlab='month', ylab='Observed cases U5', bty='L', main=paste('dhis2 -', years_scatter[yy]))
    for(mm in 1:12){
      dt_yy_mm = dt_yy %>% filter(month == mm)
      values = dt_yy_mm[[u5_dhis_col]]
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(dt_yy_mm$received_smc)])
    }
    plot(NA, ylim=c(0,max_dhis), xlim=c(0.5, 12.5), xlab='month', ylab='Observed cases O5', bty='L', main=paste('dhis2 -', years_scatter[yy]))
    for(mm in 1:12){
      dt_yy_mm = dt_yy %>% filter(month == mm)
      values = dt_yy_mm[[o5_dhis_col]]
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(dt_yy_mm$received_smc)])
    }
    # simulation  
    plot(NA, ylim=c(0,max_sim), xlim=c(0.5, 12.5), xlab='month', ylab='Observed cases U5', bty='L', main=paste('sim -', years_scatter[yy]))
    for(mm in 1:12){
      dt_yy_mm = dt_yy %>% filter(month == mm)
      values = dt_yy_mm$sim_maltreat_u5
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(dt_yy_mm$received_smc)])
    }
    plot(NA, ylim=c(0,max_sim), xlim=c(0.5, 12.5), xlab='month', ylab='Observed cases O5', bty='L', main=paste('sim -', years_scatter[yy]))
    for(mm in 1:12){
      dt_yy_mm = dt_yy %>% filter(month == mm)
      values = dt_yy_mm$sim_maltreat_ov5
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(dt_yy_mm$received_smc)])
    }
    dev.off()
  }
  par(mfrow=c(1,1))
}


# monthly fraction of cases U5 in each ds-month, separated by month; separate plots for DHIS2 and sim. Sim data adjusted for CM and NMF
month_CM_NMF_adjusted = TRUE
if(month_CM_NMF_adjusted){
  # pdf(paste(save_plot_filepath, '/compare_smc_sim_dhis2_monthly_', dhis_string, '_smcMult', smc_coverage_multiplier*100, '_CM_NMF_adjusted.pdf', sep=''), width=9, height=5.5, useDingbats=FALSE)
  # pdf(paste(save_plot_filepath, '/compare_smc_sim_dhis2_monthly_maltreat_smcMult', smc_coverage_multiplier*100, '_CM_NMF_adjusted_new.pdf', sep=''), width=9, height=5.5, useDingbats=FALSE)
  # pdf(paste(save_plot_filepath, '/compare_smc_sim_dhis2_monthly_maltreat_',sim_version_name,'_CM_NMF_adjusted.pdf', sep=''), width=9, height=5.5, useDingbats=FALSE)
  # par(mfrow=c(1,2))
  png(paste(save_plot_filepath, '/compare_smc_sim_dhis2_monthly_maltreat_',sim_version_name,'_CM_NMF_adjusted2.png', sep=''), width=2*length(years_included), height=4, units='in', res=900)
  par(mfrow=c(2,length(years_included)), mar=c(4,4,2,2), mgp=c(2,0.75,0))
  for (yy in 1:length(years_included)){
    merged_df_yy = merged_df %>% filter(year == years_included[yy])
    plot(NA, ylim=c(0,1), xlim=c(0.5, 12.5), xlab='month', ylab='fraction of cases U5', bty='L', main=paste('dhis2 -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy %>% filter(month == mm)
      values = merged_df_mm$frac_U5_dhis2
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)], cex=0.4)
    }
  }
  for (yy in 1:length(years_included)){
    merged_df_yy = merged_df %>% filter(year == years_included[yy])
    plot(NA, ylim=c(0,1), xlim=c(0.5, 12.5), xlab='month', ylab='fraction of cases U5', bty='L', main=paste('sim -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy %>% filter(month == mm)
      values = merged_df_mm$frac_U5_sim_cm_nmf_adjusted
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)], cex=0.4)
    }
  }
  dev.off()
  
  png(paste(save_plot_filepath, '/compare_smc_sim_dhis2_monthly_maltreat_',sim_version_name,'_CM_NMF_adjusted2_receivedSMConly.png', sep=''), width=2*length(years_included), height=4, units='in', res=900)
  par(mfrow=c(2,length(years_included)), mar=c(4,4,2,2), mgp=c(2,0.75,0))
  for (yy in 1:length(years_included)){
    merged_df_yy = merged_df %>% filter(year == years_included[yy])
    plot(NA, ylim=c(0,1), xlim=c(0.5, 12.5), xlab='month', ylab='fraction of cases U5', bty='L', main=paste('dhis2 -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy[merged_df_yy$received_smc,] %>% filter(month == mm)
      values = merged_df_mm$frac_U5_dhis2
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=21, cex=0.4)
    }
  }
  for (yy in 1:length(years_included)){
    merged_df_yy = merged_df %>% filter(year == years_included[yy])
    plot(NA, ylim=c(0,1), xlim=c(0.5, 12.5), xlab='month', ylab='fraction of cases U5', bty='L', main=paste('sim -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy[merged_df_yy$received_smc,] %>% filter(month == mm)
      values = merged_df_mm$frac_U5_sim_cm_nmf_adjusted
      points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=21, cex=0.4)
    }
  }
  dev.off()
}





# create violin plots comparing average U5 cases relative to a reference month in admins that did and did not receive SMC for reference and simulation
if(TRUE){
  
  # U5 and O5 incidence (not fraction of cases in U5), normalized to July
  # Create column normalizing DHIS2 data to specific month (here, July)
  base_month = 7
  norm_frac = dt[month==base_month,.(admin_name,year,month,conf_u5)]
  setnames(norm_frac,"conf_u5","ref_conf_u5")
  norm_frac[,month:=NULL]
  norm_frac = distinct(norm_frac)
  dt2 = merge(dt, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
  dt2[,norm_U5_dhis2:=conf_u5/ref_conf_u5]
  
  # Create column normalizing CM-adjusted sims to specific month (here, July)
  norm_frac = dt2[month==base_month,.(admin_name,year,month,sim_maltreat_u5)]
  setnames(norm_frac,"sim_maltreat_u5","ref_sim_maltreat_u5")
  norm_frac[,month:=NULL]
  norm_frac = distinct(norm_frac)
  dt3 = merge(dt2, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
  dt3[,norm_U5_sim:=sim_maltreat_u5/ref_sim_maltreat_u5]
  dt_rel_month = dt3
  
  plot_year=2019
  # violin plot
  gg_smc = ggplot()+
    theme_bw()+
    geom_violin(data=dt_rel_month[(year==plot_year & received_this_year==TRUE)],
                aes(x=month,y=norm_U5_dhis2,group=interaction(date)), color='black', fill=NA)+
    geom_violin(data=dt_rel_month[(year==plot_year & received_this_year==TRUE)],
                aes(x=month,y=norm_U5_sim,group=interaction(date)),
                color=rgb(0,0.5,0.5,0.5), fill=NA)+
    geom_smooth(data=dt_rel_month[(year==plot_year & received_this_year==TRUE)],
                aes(x=month,y=norm_U5_dhis2,group=received_this_year), color="black")+
    geom_smooth(data=dt_rel_month[(year==plot_year & received_this_year==TRUE)],
                aes(x=month,y=norm_U5_sim,group=received_this_year),
                color=rgb(0,0.5,0.5))+
    ylab(paste0('Malaria cases in U5, relative to July, in ', plot_year))+
    xlab('month')+
    coord_cartesian(ylim=c(-0.1, 15))+
    ggtitle(paste('Admins that received SMC','; monthly U5 NMF=', num_nmf_month_u5, sep=''))
  ggsave(paste0(save_plot_filepath,"/compare_U5_incidence_sim_dhis2_relativeJuly_",plot_year,"_receivedSMC.png"), gg_smc, height=5, width=6)
  
  gg_no_smc = ggplot()+
    theme_bw()+
    geom_violin(data=dt_rel_month[(year==plot_year & received_this_year==FALSE)],
                aes(x=month,y=norm_U5_dhis2,group=interaction(date)), color='black', fill=NA)+
    geom_violin(data=dt_rel_month[(year==plot_year & received_this_year==FALSE)],
                aes(x=month,y=norm_U5_sim,group=interaction(date)),
                color=rgb(0,0.5,0.5,0.5), fill=NA)+
    geom_smooth(data=dt_rel_month[(year==plot_year & received_this_year==FALSE)],
                aes(x=month,y=norm_U5_dhis2,group=received_this_year), color="black")+
    geom_smooth(data=dt_rel_month[(year==plot_year & received_this_year==FALSE)],
                aes(x=month,y=norm_U5_sim,group=received_this_year),
                color=rgb(0,0.5,0.5))+
    ylab(paste0('Malaria cases in U5, relative to July, in ', plot_year))+
    xlab('month')+
    coord_cartesian(ylim=c(-0.1, 15))+
    ggtitle(paste('Admins that did not receive SMC','; monthly U5 NMF=', num_nmf_month_u5, sep=''))
  ggsave(paste0(save_plot_filepath,"/compare_U5_incidence_sim_dhis2_relativeJuly_",plot_year,"_noSMC.png"), gg_no_smc, height=5, width=6)
}

# create timeseries for each admin with and without SMC and for U5 and O5
if(FALSE){

  # minimum July num-cases to be included
  min_july_cases_dhis2 = 40
  min_july_cases_sim = 10
  # U5 and O5 incidence (not fraction of cases in U5), normalized to July
  # Create column normalizing DHIS2 data to specific month (here, July)
  base_month = 7
  norm_frac = dt[month==base_month,.(admin_name,year,month,conf_u5, conf_ov5)]
  setnames(norm_frac,"conf_u5","ref_conf_u5")
  setnames(norm_frac,"conf_ov5","ref_conf_ov5")
  norm_frac[,month:=NULL]
  norm_frac = distinct(norm_frac)
  dt2 = merge(dt, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
  # get rid of rescaling when July numbers are too small
  dt2$ref_conf_u5[dt2$ref_conf_u5 < min_july_cases] = NA
  dt2$ref_conf_ov5[dt2$ref_conf_ov5 < min_july_cases] = NA
  dt2[,norm_U5_dhis2:=conf_u5/ref_conf_u5]
  dt2[,norm_O5_dhis2:=conf_ov5/ref_conf_ov5]
  
  # Create column normalizing CM-adjusted sims to specific month (here, July)
  norm_frac = dt2[month==base_month,.(admin_name,year,month,sim_maltreat_u5, sim_maltreat_ov5)]
  setnames(norm_frac,"sim_maltreat_u5","ref_sim_maltreat_u5")
  setnames(norm_frac,"sim_maltreat_ov5","ref_sim_maltreat_ov5")
  norm_frac[,month:=NULL]
  norm_frac = distinct(norm_frac)
  dt3 = merge(dt2, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
  # get rid of rescaling when July numbers are too small
  dt3$ref_sim_maltreat_u5[dt3$ref_sim_maltreat_u5 < min_july_cases] = NA
  dt3$ref_sim_maltreat_ov5[dt3$ref_sim_maltreat_ov5 < min_july_cases] = NA
  dt3[,norm_U5_sim:=sim_maltreat_u5/ref_sim_maltreat_u5]
  dt3[,norm_O5_sim:=sim_maltreat_ov5/ref_sim_maltreat_ov5]
  
  # subset to relevant years for averaging
  averaged_years = 2018:2019
  # get averages across included years
  dt_admin_ave = dt3 %>% select(admin_name, year, month, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>%
    filter(year %in% averaged_years) %>%
    drop_na(norm_U5_dhis2) %>%
    group_by(admin_name, month, received_this_year) %>%
    summarise_all(mean, na.rm=TRUE)
  dt_mean_across_admins = dt_admin_ave %>% group_by(month, received_this_year) %>%
    summarise_all(mean, na.rm=TRUE)

  # timeseries plot (each admin gets a line) - Ages U5
  g_smc_dhis2 = ggplot() +
    geom_line(data=dt_admin_ave, aes(x=month, y=norm_U5_dhis2, color=admin_name))+    
    geom_point(data=dt_admin_ave, aes(x=month, y=norm_U5_dhis2, color=admin_name), size=0.1)+
    geom_line(data=dt_mean_across_admins, aes(x=month, y=norm_U5_dhis2), color='black', size=1)+
    ylim(c(0,4.5))+
    # coord_cartesian(ylim=c(0,5)) +
    theme(legend.position='none')+
    facet_wrap(~received_this_year, nrow=1)
  g_smc_sim = ggplot() +
    geom_line(data=dt_admin_ave, aes(x=month, y=norm_U5_sim, color=admin_name))+    
    geom_point(data=dt_admin_ave, aes(x=month, y=norm_U5_sim, color=admin_name), size=0.1)+
    geom_line(data=dt_mean_across_admins, aes(x=month, y=norm_U5_sim), color='black', size=1)+
    ylim(c(0,4.5))+
    # coord_cartesian(ylim=c(0,5)) +
    theme(legend.position='none')+
    facet_wrap(~received_this_year, nrow=1)
  grid.arrange(g_smc_dhis2,  g_smc_sim, nrow=2)
  
  
  # timeseries plot (each admin gets a line) - Ages O5
  g_smc_dhis2 = ggplot() +
    geom_line(data=dt_admin_ave, aes(x=month, y=norm_O5_dhis2, color=admin_name))+    
    geom_point(data=dt_admin_ave, aes(x=month, y=norm_O5_dhis2, color=admin_name), size=0.1)+
    geom_line(data=dt_mean_across_admins, aes(x=month, y=norm_O5_dhis2), color='black', size=1)+
    ylim(c(0,4.5))+
    # coord_cartesian(ylim=c(0,5)) +
    theme(legend.position='none')+
    facet_wrap(~received_this_year, nrow=1)
  g_smc_sim = ggplot() +
    geom_line(data=dt_admin_ave, aes(x=month, y=norm_O5_sim, color=admin_name))+    
    geom_point(data=dt_admin_ave, aes(x=month, y=norm_O5_sim, color=admin_name), size=0.1)+
    geom_line(data=dt_mean_across_admins, aes(x=month, y=norm_O5_sim), color='black', size=1)+
    ylim(c(0,4.5))+
    # coord_cartesian(ylim=c(0,5)) +
    theme(legend.position='none')+
    facet_wrap(~received_this_year, nrow=1)
  grid.arrange(g_smc_dhis2,  g_smc_sim, nrow=2)
}



# create timeseries for each admin with and without SMC and for U5 and O5, normalized to first month getting SMC
if(TRUE){

  # minimum number of cases in month SMC began for an LGA to be included
  min_ref_cases_dhis2 = 40
  min_ref_cases_sim = 10
  
  # if the month of first round is not specified, assume it is in default_first_month
  default_first_month = round(mean(dt$month_first_round, na.rm=TRUE))
  dt$month_first_round[is.na(dt$month_first_round)] = default_first_month
  dt$months_since_first_round = dt$month - dt$month_first_round
  
  # U5 and O5 incidence (not fraction of cases in U5), normalized to first month
  # Create column normalizing DHIS2 and sim data to first month
  norm_frac = dt[months_since_first_round==0,.(admin_name,year,months_since_first_round,conf_u5, conf_ov5, sim_maltreat_u5, sim_maltreat_ov5)]
  setnames(norm_frac,"conf_u5","ref_conf_u5")
  setnames(norm_frac,"conf_ov5","ref_conf_ov5")
  setnames(norm_frac,"sim_maltreat_u5","ref_sim_maltreat_u5")
  setnames(norm_frac,"sim_maltreat_ov5","ref_sim_maltreat_ov5")
  norm_frac[,months_since_first_round:=NULL]
  norm_frac = distinct(norm_frac)
  dt2 = merge(dt, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
  # get rid of rescaling when reference numbers are too small
  dt2$ref_conf_u5[dt2$ref_conf_u5 < min_ref_cases_dhis2] = NA
  dt2$ref_conf_ov5[dt2$ref_conf_ov5 < min_ref_cases_dhis2] = NA
  dt2$ref_sim_maltreat_u5[dt2$ref_sim_maltreat_u5 < min_ref_cases_sim] = NA
  dt2$ref_sim_maltreat_ov5[dt2$ref_sim_maltreat_ov5 < min_ref_cases_sim] = NA
  dt2[,norm_U5_dhis2:=conf_u5/ref_conf_u5]
  dt2[,norm_O5_dhis2:=conf_ov5/ref_conf_ov5]
  dt2[,norm_U5_sim:=sim_maltreat_u5/ref_sim_maltreat_u5]
  dt2[,norm_O5_sim:=sim_maltreat_ov5/ref_sim_maltreat_ov5]
  
  # subset to relevant years for averaging
  averaged_years = 2018:2019
  # get averages across included years
  dt_admin_ave = dt2 %>% select(admin_name, year, months_since_first_round, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>%
    filter(year %in% averaged_years) %>%
    # drop_na(norm_U5_dhis2) %>%
    group_by(admin_name, months_since_first_round, received_this_year) %>%
    summarise_all(mean, na.rm=TRUE) %>% ungroup()
  dt_mean_across_admins = dt_admin_ave %>% select(months_since_first_round, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>% 
    group_by(months_since_first_round, received_this_year) %>%
    summarise_all(mean, na.rm=TRUE)
  dt_median_across_admins = dt_admin_ave %>% select(months_since_first_round, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>% 
    group_by(months_since_first_round, received_this_year) %>%
    summarise_all(median, na.rm=TRUE)
  
  # timeseries plot (each admin gets a line) - Ages U5
  # only include LGAs with SMC
  dt_admin_ave = dt_admin_ave[dt_admin_ave$received_this_year==TRUE,]
  dt_mean_across_admins = dt_mean_across_admins[dt_mean_across_admins$received_this_year==TRUE,]
  dt_median_across_admins = dt_median_across_admins[dt_median_across_admins$received_this_year==TRUE,]
  g_smc_dhis2 = ggplot() +
    geom_line(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_dhis2, color=admin_name), alpha=0.35)+    
    geom_point(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_dhis2, color=admin_name), size=0.1, alpha=0.35)+
    geom_line(data=dt_mean_across_admins, aes(x=months_since_first_round, y=norm_U5_dhis2), color='black', size=2)+
    ylab('normalized U5 incidence') + 
    xlab('months since first SMC cycle') + 
    ggtitle('Surveillance data') + 
    ylim(c(0,3.5))+
    # coord_cartesian(ylim=c(0,5)) +
    theme_classic() +
    theme(legend.position='none')#+
    # facet_wrap(~received_this_year, nrow=1)
  g_smc_sim = ggplot() +
    geom_line(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_sim, color=admin_name), alpha=0.35)+    
    geom_point(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_sim, color=admin_name), size=0.1, alpha=0.35)+
    geom_line(data=dt_median_across_admins, aes(x=months_since_first_round, y=norm_U5_sim), color='black', size=2)+
    ylab('normalized U5 incidence') + 
    xlab('months since first SMC cycle') + 
    ggtitle('Simulation output') + 
    ylim(c(0,3.5))+
    # coord_cartesian(ylim=c(0,5)) +
    theme_classic() +
    theme(legend.position='none')#+
    # facet_wrap(~received_this_year, nrow=1)
  gg_total = grid.arrange(g_smc_sim, g_smc_dhis2,  nrow=2)
  ggsave(paste0(save_plot_filepath,"/compare_U5_incidence_sim_dhis2_relativeFirstSMCMonth_",min(averaged_years),"_", max(averaged_years),".png"), gg_total, height=8*0.9, width=4*0.9)
  
  
  # # timeseries plot (each admin gets a line) - Ages O5
  # g_smc_dhis2 = ggplot() +
  #   geom_line(data=dt_admin_ave, aes(x=month, y=norm_O5_dhis2, color=admin_name))+    
  #   geom_point(data=dt_admin_ave, aes(x=month, y=norm_O5_dhis2, color=admin_name), size=0.1)+
  #   geom_line(data=dt_mean_across_admins, aes(x=month, y=norm_O5_dhis2), color='black', size=1)+
  #   ylim(c(0,4.5))+
  #   # coord_cartesian(ylim=c(0,5)) +
  #   theme(legend.position='none')+
  #   facet_wrap(~received_this_year, nrow=1)
  # g_smc_sim = ggplot() +
  #   geom_line(data=dt_admin_ave, aes(x=month, y=norm_O5_sim, color=admin_name))+    
  #   geom_point(data=dt_admin_ave, aes(x=month, y=norm_O5_sim, color=admin_name), size=0.1)+
  #   geom_line(data=dt_mean_across_admins, aes(x=month, y=norm_O5_sim), color='black', size=1)+
  #   ylim(c(0,4.5))+
  #   # coord_cartesian(ylim=c(0,5)) +
  #   theme(legend.position='none')+
  #   facet_wrap(~received_this_year, nrow=1)
  # grid.arrange(g_smc_dhis2,  g_smc_sim, nrow=2)
  
  
  if(TRUE){ # same formatting as above, but use month of year and relative to January and line is median
    
    # use first month as Jan
    ref_month = 1
    dt$months_since_first_round = dt$month - ref_month
    
    # U5 and O5 incidence (not fraction of cases in U5), normalized to first month
    # Create column normalizing DHIS2 and sim data to first month
    norm_frac = dt[month==ref_month,.(admin_name,year,months_since_first_round,conf_u5, conf_ov5, sim_maltreat_u5, sim_maltreat_ov5)]
    setnames(norm_frac,"conf_u5","ref_conf_u5")
    setnames(norm_frac,"conf_ov5","ref_conf_ov5")
    setnames(norm_frac,"sim_maltreat_u5","ref_sim_maltreat_u5")
    setnames(norm_frac,"sim_maltreat_ov5","ref_sim_maltreat_ov5")
    norm_frac[,months_since_first_round:=NULL]
    norm_frac = distinct(norm_frac)
    dt2 = merge(dt, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
    # get rid of rescaling when reference numbers are too small
    dt2$ref_conf_u5[dt2$ref_conf_u5 < min_ref_cases_dhis2] = NA
    dt2$ref_conf_ov5[dt2$ref_conf_ov5 < min_ref_cases_dhis2] = NA
    dt2$ref_sim_maltreat_u5[dt2$ref_sim_maltreat_u5 < min_ref_cases_sim] = NA
    dt2$ref_sim_maltreat_ov5[dt2$ref_sim_maltreat_ov5 < min_ref_cases_sim] = NA
    dt2[,norm_U5_dhis2:=conf_u5/ref_conf_u5]
    dt2[,norm_O5_dhis2:=conf_ov5/ref_conf_ov5]
    dt2[,norm_U5_sim:=sim_maltreat_u5/ref_sim_maltreat_u5]
    dt2[,norm_O5_sim:=sim_maltreat_ov5/ref_sim_maltreat_ov5]
    
    # subset to relevant years for averaging
    averaged_years = 2018:2019
    # get averages across included years
    dt_admin_ave = dt2 %>% select(admin_name, year, months_since_first_round, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>%
      filter(year %in% averaged_years) %>%
      # drop_na(norm_U5_dhis2) %>%
      group_by(admin_name, months_since_first_round, received_this_year) %>%
      summarise_all(mean, na.rm=TRUE) %>% ungroup()
    dt_mean_across_admins = dt_admin_ave %>% select(months_since_first_round, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>% 
      group_by(months_since_first_round, received_this_year) %>%
      summarise_all(mean, na.rm=TRUE)
    dt_median_across_admins = dt_admin_ave %>% select(months_since_first_round, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>% 
      group_by(months_since_first_round, received_this_year) %>%
      summarise_all(median, na.rm=TRUE)
    
    # timeseries plot (each admin gets a line) - Ages U5
    dt_admin_ave$months_since_first_round = dt_admin_ave$months_since_first_round + 1
    dt_median_across_admins$months_since_first_round =  dt_median_across_admins$months_since_first_round + 1
    
    # only include LGAs with SMC
    dt_admin_ave = dt_admin_ave[dt_admin_ave$received_this_year==TRUE,]
    dt_mean_across_admins = dt_mean_across_admins[dt_mean_across_admins$received_this_year==TRUE,]
    dt_median_across_admins = dt_median_across_admins[dt_median_across_admins$received_this_year==TRUE,]
    g_smc_dhis2 = ggplot() +
      geom_line(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_dhis2, color=admin_name), alpha=0.35)+    
      geom_point(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_dhis2, color=admin_name), size=0.1, alpha=0.35)+
      geom_line(data=dt_median_across_admins, aes(x=months_since_first_round, y=norm_U5_dhis2), color='black', size=2)+
      ylab('normalized U5 incidence') + 
      xlab('months since first SMC cycle') + 
      ggtitle('Surveillance data') + 
      ylim(c(0,3.5))+
      # coord_cartesian(ylim=c(0,5)) +
      theme_classic() +
      theme(legend.position='none')#+
    # facet_wrap(~received_this_year, nrow=1)
    g_smc_sim = ggplot() +
      geom_line(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_sim, color=admin_name), alpha=0.35)+    
      geom_point(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_sim, color=admin_name), size=0.1, alpha=0.35)+
      geom_line(data=dt_median_across_admins, aes(x=months_since_first_round, y=norm_U5_sim), color='black', size=2)+
      ylab('normalized U5 incidence') + 
      xlab('months since first SMC cycle') + 
      ggtitle('Simulation output') + 
      ylim(c(0,3.5))+
      # coord_cartesian(ylim=c(0,5)) +
      theme_classic() +
      theme(legend.position='none')#+
    # facet_wrap(~received_this_year, nrow=1)
    gg_total = grid.arrange(g_smc_sim, g_smc_dhis2,  nrow=2)
    ggsave(paste0(save_plot_filepath,"/compare_U5_incidence_sim_dhis2_relativeJan_",min(averaged_years),"_", max(averaged_years),".png"), gg_total, height=8*0.9, width=4*0.9)
    
    
  }
  
  if(TRUE){  # similar to above, but use the final year BEFORE getting SMC
    eligible_first_years = 2018:2020  # may want to restrict which years to plot, given quality of DHIS2 data
    # subset to first year of SMC in each DS
    df_first_smc = dt[(dt$SMC_years == 1) & (dt$year %in% eligible_first_years),]
    df_first_smc = distinct(df_first_smc[,c('admin_name', 'year')])
    # create new dataframe for final year without SMC
    df_final_no_smc = data.frame()
    for(i_ds in 1:nrow(df_first_smc)){
      rel_rows = intersect(which(dt$admin_name == df_first_smc$admin_name[i_ds]), which(dt$year == (df_first_smc$year[i_ds]-1)))
      df_final_no_smc=rbind(df_final_no_smc, dt[rel_rows,])
    } 

    
    # set month relative to first month of SMC (to match previous version with SMC)
    df_final_no_smc$month_first_round = 1#[is.na(df_final_no_smc$month_first_round)] = default_first_month
    df_final_no_smc$months_since_first_round = df_final_no_smc$month - df_final_no_smc$month_first_round
    
    # U5 and O5 incidence (not fraction of cases in U5), normalized to first month
    # Create column normalizing DHIS2 and sim data to first month
    norm_frac = df_final_no_smc[months_since_first_round==0,.(admin_name,year,months_since_first_round,conf_u5, conf_ov5, sim_maltreat_u5, sim_maltreat_ov5)]
    setnames(norm_frac,"conf_u5","ref_conf_u5")
    setnames(norm_frac,"conf_ov5","ref_conf_ov5")
    setnames(norm_frac,"sim_maltreat_u5","ref_sim_maltreat_u5")
    setnames(norm_frac,"sim_maltreat_ov5","ref_sim_maltreat_ov5")
    norm_frac[,months_since_first_round:=NULL]
    norm_frac = distinct(norm_frac)
    dt2 = merge(df_final_no_smc, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
    # get rid of rescaling when reference numbers are too small
    dt2$ref_conf_u5[dt2$ref_conf_u5 < min_ref_cases_dhis2] = NA
    dt2$ref_conf_ov5[dt2$ref_conf_ov5 < min_ref_cases_dhis2] = NA
    dt2$ref_sim_maltreat_u5[dt2$ref_sim_maltreat_u5 < min_ref_cases_sim] = NA
    dt2$ref_sim_maltreat_ov5[dt2$ref_sim_maltreat_ov5 < min_ref_cases_sim] = NA
    dt2[,norm_U5_dhis2:=conf_u5/ref_conf_u5]
    dt2[,norm_O5_dhis2:=conf_ov5/ref_conf_ov5]
    dt2[,norm_U5_sim:=sim_maltreat_u5/ref_sim_maltreat_u5]
    dt2[,norm_O5_sim:=sim_maltreat_ov5/ref_sim_maltreat_ov5]
    
    # get averages across included years
    dt_admin_ave = dt2 %>% select(admin_name, year, months_since_first_round, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>%
      filter(year %in% averaged_years) %>%
      # drop_na(norm_U5_dhis2) %>%
      group_by(admin_name, months_since_first_round, received_this_year) %>%
      summarise_all(mean, na.rm=TRUE) %>% ungroup()
    dt_mean_across_admins = dt_admin_ave %>% select(months_since_first_round, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>% 
      group_by(months_since_first_round, received_this_year) %>%
      summarise_all(mean, na.rm=TRUE)
    dt_median_across_admins = dt_admin_ave %>% select(months_since_first_round, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>%
      group_by(months_since_first_round, received_this_year) %>%
      summarise_all(median, na.rm=TRUE)
    
    # timeseries plot (each admin gets a line) - Ages U5
    # change back to months (since 'first round' is January)
    dt_admin_ave$months_since_first_round = dt_admin_ave$months_since_first_round + 1
    dt_median_across_admins$months_since_first_round =  dt_median_across_admins$months_since_first_round + 1
    g_smc_dhis2 = ggplot() +
      geom_line(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_dhis2, color=admin_name), alpha=0.35)+    
      geom_point(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_dhis2, color=admin_name), size=0.1, alpha=0.35)+
      geom_line(data=dt_median_across_admins, aes(x=months_since_first_round, y=norm_U5_dhis2), color='black', size=2)+
      ylab('normalized U5 incidence') + 
      xlab('month') + 
      ggtitle('Surveillance data') + 
      ylim(c(0,3.5))+
      # coord_cartesian(ylim=c(0,5)) +
      theme_classic() +
      theme(legend.position='none')#+
    # facet_wrap(~received_this_year, nrow=1)
    g_smc_sim = ggplot() +
      geom_line(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_sim, color=admin_name), alpha=0.35)+    
      geom_point(data=dt_admin_ave, aes(x=months_since_first_round, y=norm_U5_sim, color=admin_name), size=0.1, alpha=0.35)+
      geom_line(data=dt_median_across_admins, aes(x=months_since_first_round, y=norm_U5_sim), color='black', size=2)+
      ylab('normalized U5 incidence') + 
      xlab('month') + 
      ggtitle('Simulation output') + 
      ylim(c(0,3.5))+
      # coord_cartesian(ylim=c(0,5)) +
      theme_classic() +
      theme(legend.position='none')#+
    # facet_wrap(~received_this_year, nrow=1)
    gg_total = grid.arrange(g_smc_sim, g_smc_dhis2,  nrow=2)
    ggsave(paste0(save_plot_filepath,"/compare_U5_incidence_sim_dhis2_yearBeforeSMC_relativeJan_",min(averaged_years),"_", max(averaged_years),".png"), gg_total, height=8*0.9, width=4*0.9)
    
    
    
  }
}

##=============================================##
# create plots looking at monthly patterns with state-level averages
##=============================================##

if(FALSE){
  # change column names to agree with hard-coded values
  if(u5_dhis_col != 'conf_u5'){
    colnames(dt)[colnames(dt)=='conf_u5'] = 'original_conf_u5'
    colnames(dt)[colnames(dt)==u5_dhis_col] = 'conf_u5'
  }
  if(o5_dhis_col != 'conf_ov5'){
    colnames(dt)[colnames(dt)=='conf_ov5'] = 'original_conf_ov5'
    colnames(dt)[colnames(dt)==o5_dhis_col] = 'conf_ov5'
  }
  
  # minimum July num-cases to be included
  min_july_cases = 10
  # U5 and O5 incidence (not fraction of cases in U5), normalized to July
  # Create column normalizing DHIS2 data to specific month (here, July)
  base_month = 7
  norm_frac = dt[month==base_month,.(admin_name,year,month,conf_u5, conf_ov5)]
  setnames(norm_frac,"conf_u5","ref_conf_u5")
  setnames(norm_frac,"conf_ov5","ref_conf_ov5")
  norm_frac[,month:=NULL]
  norm_frac = distinct(norm_frac)
  dt2 = merge(dt, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
  # get rid of rescaling when July numbers are too small
  dt2$ref_conf_u5[dt2$ref_conf_u5 < min_july_cases] = NA
  dt2$ref_conf_ov5[dt2$ref_conf_ov5 < min_july_cases] = NA
  dt2[,norm_U5_dhis2:=conf_u5/ref_conf_u5]
  dt2[,norm_O5_dhis2:=conf_ov5/ref_conf_ov5]
  
  # Create column normalizing CM-adjusted sims to specific month (here, July)
  norm_frac = dt2[month==base_month,.(admin_name,year,month,sim_maltreat_u5, sim_maltreat_ov5)]
  setnames(norm_frac,"sim_maltreat_u5","ref_sim_maltreat_u5")
  setnames(norm_frac,"sim_maltreat_ov5","ref_sim_maltreat_ov5")
  norm_frac[,month:=NULL]
  norm_frac = distinct(norm_frac)
  dt3 = merge(dt2, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
  # get rid of rescaling when July numbers are too small
  dt3$ref_sim_maltreat_u5[dt3$ref_sim_maltreat_u5 < min_july_cases] = NA
  dt3$ref_sim_maltreat_ov5[dt3$ref_sim_maltreat_ov5 < min_july_cases] = NA
  dt3[,norm_U5_sim:=sim_maltreat_u5/ref_sim_maltreat_u5]
  dt3[,norm_O5_sim:=sim_maltreat_ov5/ref_sim_maltreat_ov5]

  # merge in state names
  dt_rel_month = merge(dt3, pop_arch[,c('admin_name', 'State')], by='admin_name', all=TRUE)
  averaged_years = 2018:2019  #   subset to relevant years for averaging
  # get averages across included years and across LGAs in state
  dt_state_ave = dt_rel_month %>% select(State, year, month, norm_U5_dhis2, norm_U5_sim, norm_O5_dhis2, norm_O5_sim, received_this_year) %>%
    filter(year %in% averaged_years) %>%
    drop_na(norm_U5_dhis2) %>%
    group_by(State, month, received_this_year) %>%
    summarise_all(mean, na.rm=TRUE)
    # summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE))
  
  # timeseries plot (each admin gets a line) - Ages U5
  g_smc_dhis2 = ggplot() +
    geom_line(data=dt_state_ave, aes(x=month, y=norm_U5_dhis2, color=State))+    
    geom_point(data=dt_state_ave, aes(x=month, y=norm_U5_dhis2, color=State), size=0.1)+
    coord_cartesian(ylim=c(0,5)) +
    theme(legend.position='none')+
    facet_wrap(~received_this_year, nrow=1)
  g_smc_sim = ggplot() +
    geom_line(data=dt_state_ave, aes(x=month, y=norm_U5_sim, color=State))+    
    geom_point(data=dt_state_ave, aes(x=month, y=norm_U5_sim, color=State), size=0.1)+
    coord_cartesian(ylim=c(0,5)) +
    theme(legend.position='none')+
    facet_wrap(~received_this_year, nrow=1)
  grid.arrange(g_smc_dhis2,  g_smc_sim, nrow=2)

  
  # timeseries plot (each admin gets a line) - Ages O5
  g_smc_dhis2 = ggplot() +
    geom_line(data=dt_state_ave, aes(x=month, y=norm_O5_dhis2, color=State))+    
    geom_point(data=dt_state_ave, aes(x=month, y=norm_O5_dhis2, color=State), size=0.1)+
    coord_cartesian(ylim=c(0,5)) +
    theme(legend.position='none')+
    facet_wrap(~received_this_year, nrow=1)
  g_smc_sim = ggplot() +
    geom_line(data=dt_state_ave, aes(x=month, y=norm_O5_sim, color=State))+    
    geom_point(data=dt_state_ave, aes(x=month, y=norm_O5_sim, color=State), size=0.1)+
    coord_cartesian(ylim=c(0,5)) +
    theme(legend.position='none')+
    facet_wrap(~received_this_year, nrow=1)
  grid.arrange(g_smc_dhis2,  g_smc_sim, nrow=2)
  
}



##===================================================================================================================================##
# effect size in simulations between final year without SMC and first year with SMC; include hist across admins, with comparison against references
##===================================================================================================================================##
 

if(TRUE){
  
  plot_month = 9
  eligible_first_years = 2018:2022  # may want to restrict which years to plot, given quality of DHIS2 data
  # subset to first year of SMC in each DS
  df_first_smc = dt[(dt$SMC_years == 1) & (dt$month == plot_month),]
  df_first_smc = df_first_smc[,c('admin_name', 'year','Pop_U5', 'PfPR_U5', 'conf_u5', 'sim_maltreat_u5')]
  colnames(df_first_smc) = c('admin_name', 'year','Pop_U5', 'PfPR_U5_SMC','dhis_case_u5_SMC', 'sim_case_u5_SMC')
  # add columns to dataframe for final year without SMC
  df_first_smc$PfPR_U5_noSMC = rep(NA, nrow(df_first_smc))
  df_first_smc$dhis_case_u5_noSMC = rep(NA, nrow(df_first_smc))
  df_first_smc$sim_case_u5_noSMC = rep(NA, nrow(df_first_smc))
  for(i_ds in 1:nrow(df_first_smc)){
    rel_row = intersect(intersect(which(dt$admin_name == df_first_smc$admin_name[i_ds]), which(dt$year == (df_first_smc$year[i_ds]-1))), which(dt$month == plot_month))
    if(length(rel_row) == 1){
      df_first_smc$PfPR_U5_noSMC[i_ds] = dt$PfPR_U5[rel_row]
      df_first_smc$dhis_case_u5_noSMC[i_ds] = dt$conf_u5[rel_row]
      df_first_smc$sim_case_u5_noSMC[i_ds] = dt$sim_maltreat_u5[rel_row]
    }
  } 
  df_first_smc = df_first_smc[df_first_smc$year %in% eligible_first_years,]
  
  png(paste(save_plot_filepath, '/compare_before_after_SMC_with_reference_',sim_version_name,'_CM_NMF_adjusted_Sept.png', sep=''), width=4, height=8, units='in', res=900)
  # hist(df_first_smc$noSMC_num_U5_treat_sim - df_first_smc$SMC_num_U5_treat_sim,breaks=seq(-50,400,20),  main='reduction in incidence from year before to first year of SMC')
  par(mfrow=c(2,1))
  # plot results from simulations
  sim_ymax = 50
  hist((1-(df_first_smc$sim_case_u5_SMC/df_first_smc$sim_case_u5_noSMC ))*100,breaks=seq(-75,100,5), ylim=c(0,sim_ymax), main='Percent reduction in U5 incidence',  xlab=c('percent reduction in incidence', ' from year before to first year of SMC'), ylab='frequency among simulated LGAs')
  for(ii in c(80, 82, 70, 86, 60, 74,75,82)){
    arrows(x0=ii, y1=sim_ymax*0.9, y0=sim_ymax, col=rgb(0,0,1,0.75), lwd=1, length=0.1)  # multiple sources: see SMC_impact_data.xlsx
  }

  # hist((1-(df_first_smc$PfPR_U5_SMC/df_first_smc$PfPR_U5_noSMC))*100, breaks=seq(-75,100,5), main='Percent reduction in U5 prevalence', xlab=c('percent reduction in prevalence', ' from year before to first year of SMC'), ylab='frequency among simulated LGAs')
  # abline(v=60, col='blue', lwd=3)  # Druetz et al. 2018
  # legend('topleft', legend=c('ref=Druetz et al. 2018'), col=rgb(0,0,1,0.75), pch=c('|'), bty='n')
  
  # plot results from dhis2
  df_first_smc$percent_reduction_dhis2 = (1-(df_first_smc$dhis_case_u5_SMC/df_first_smc$dhis_case_u5_noSMC ))*100
  # truncate anything smaller than -75% to -75%
  df_first_smc$percent_reduction_dhis2 = sapply(df_first_smc$percent_reduction_dhis2, max, -75)
  dhis2_ymax=12
  hist(df_first_smc$percent_reduction_dhis2, breaks=seq(-75,100,5),  main='Percent reduction in U5 incidence', xlab=c('percent reduction in incidence', ' from year before to first year of SMC'), ylab='frequency among LGAs in dhis2', ylim=c(0,dhis2_ymax))
  for(ii in c(80, 82, 70, 86, 60, 74,75,82)){
    arrows(x0=ii, y1=dhis2_ymax*0.9,y0=dhis2_ymax, col=rgb(0,0,1,0.75), lwd=1, length=0.1)  # multiple sources: see SMC_impact_data.xlsx
  }
  dev.off()
  
  
  png(paste(save_plot_filepath, '/compare_before_after_SMC_with_reference_',sim_version_name,'_CM_NMF_adjusted_Sept_subsetOnlyAdminInDhis2.png', sep=''), width=4, height=8, units='in', res=900)
  # hist(df_first_smc$noSMC_num_U5_treat_sim - df_first_smc$SMC_num_U5_treat_sim,breaks=seq(-50,400,20),  main='reduction in incidence from year before to first year of SMC')
  par(mfrow=c(2,1))
  # plot results from dhis2
  df_first_smc$percent_reduction_dhis2 = (1-(df_first_smc$dhis_case_u5_SMC/df_first_smc$dhis_case_u5_noSMC ))*100
  # truncate anything smaller than -75% to -75%
  df_first_smc$percent_reduction_dhis2 = sapply(df_first_smc$percent_reduction_dhis2, max, -75)
  dhis2_ymax=12
  hist(df_first_smc$percent_reduction_dhis2, breaks=seq(-75,100,5),  main='Percent reduction in U5 incidence', xlab=c('percent reduction in incidence', ' from year before to first year of SMC'), ylab='frequency among LGAs in dhis2', ylim=c(0,dhis2_ymax))
  for(ii in c(80, 82, 70, 86, 60, 74,75,82)){
    arrows(x0=ii, y1=dhis2_ymax*0.9,y0=dhis2_ymax, col=rgb(0,0,1,0.75), lwd=1, length=0.1)  # multiple sources: see SMC_impact_data.xlsx
  }
  
  # plot results from simulations
  df_first_smc$percent_reduction_sim = (1-(df_first_smc$sim_case_u5_SMC/df_first_smc$sim_case_u5_noSMC ))*100
  sim_ymax = 50
  hist(df_first_smc$percent_reduction_sim[!is.na(df_first_smc$percent_reduction_dhis2)], breaks=seq(-75,100,5), ylim=c(0,sim_ymax), main='Percent reduction in U5 incidence',  xlab=c('percent reduction in incidence', ' from year before to first year of SMC'), ylab='frequency among simulated LGAs')
  for(ii in c(80, 82, 70, 86, 60, 74,75,82)){
    arrows(x0=ii, y1=sim_ymax*0.9, y0=sim_ymax, col=rgb(0,0,1,0.75), lwd=1, length=0.1)  # multiple sources: see SMC_impact_data.xlsx
  }
  
  # hist((1-(df_first_smc$PfPR_U5_SMC/df_first_smc$PfPR_U5_noSMC))*100, breaks=seq(-75,100,5), main='Percent reduction in U5 prevalence', xlab=c('percent reduction in prevalence', ' from year before to first year of SMC'), ylab='frequency among simulated LGAs')
  # abline(v=60, col='blue', lwd=3)  # Druetz et al. 2018
  # legend('topleft', legend=c('ref=Druetz et al. 2018'), col=rgb(0,0,1,0.75), pch=c('|'), bty='n')
  

  dev.off()
}

  
  

  
if(FALSE){
  # OLD VERSION - only looking at simulation outputs, not dhis2
  
  # plot effect size in simulations between final year without SMC and first year with SMC
  sim_burden_all_years = get_sim_data(sim_output_filepath=sim_output_filepath, rel_prob_cm_o5=rel_prob_cm_o5, num_nmf_month_u5=num_nmf_month_u5, num_nmf_month_o5=num_nmf_month_o5, years_included=2014:2019)
  sim_burden_all_years = left_join(sim_burden_all_years, smc_info)
  sim_burden_all_years$SMC_years[is.na(sim_burden_all_years$SMC_years)] = 0
  # subset to first year of SMC in each DS
  df_first_smc = sim_burden_all_years[(sim_burden_all_years$SMC_years == 1) & (sim_burden_all_years$month == plot_month),]
  df_first_smc = df_first_smc[,c('admin_name', 'year','Pop_U5', 'PfPR_U5','New_clinical_cases_U5','m_NMF_u5', 'num_U5_treat_sim')]
  colnames(df_first_smc) = c('admin_name', 'year','Pop_U5', 'SMC_PfPR_U5','SMC_New_clinical_cases_U5','SMC_m_NMF_u5', 'SMC_num_U5_treat_sim')
  # add columns to dataframe for final year without SMC
  df_first_smc$noSMC_PfPR_U5 = rep(NA, nrow(df_first_smc))
  df_first_smc$noSMC_New_clinical_cases_U5 = rep(NA, nrow(df_first_smc))
  df_first_smc$noSMC_m_NMF_u5 = rep(NA, nrow(df_first_smc))
  df_first_smc$noSMC_num_U5_treat_sim = rep(NA, nrow(df_first_smc))
  df_no_smc = data.frame()  #df_first_smc[1,]
  for(i_ds in 1:nrow(df_first_smc)){
    rel_row = intersect(intersect(which(sim_burden_all_years$admin_name == df_first_smc$admin_name[i_ds]), which(sim_burden_all_years$year == (df_first_smc$year[i_ds]-1))), which(sim_burden_all_years$month == plot_month))
    if(length(rel_row) == 1){
      df_first_smc$noSMC_PfPR_U5[i_ds] = sim_burden_all_years$PfPR_U5[rel_row]
      df_first_smc$noSMC_New_clinical_cases_U5[i_ds] = sim_burden_all_years$New_clinical_cases_U5[rel_row]
      df_first_smc$noSMC_m_NMF_u5[i_ds] = sim_burden_all_years$m_NMF_u5[rel_row]
      df_first_smc$noSMC_num_U5_treat_sim[i_ds] = sim_burden_all_years$num_U5_treat_sim[rel_row]
    }
  }
  
  if(FALSE){
    plot(df_first_smc$noSMC_num_U5_treat_sim, df_first_smc$SMC_num_U5_treat_sim, ylim=c(0,450), xlim=c(0,450), xlab='U5 incidence without SMC', ylab='U5 incidence with SMC', bty='L', main='U5 incidence with and without SMC')
    points(460,55, pch=20, col='blue', cex=3)
    plot(df_first_smc$noSMC_PfPR_U5, df_first_smc$SMC_PfPR_U5, ylim=c(0,1), xlim=c(0,1), xlab='U5 prevalence without SMC', ylab='U5 prevalence with SMC', bty='L', main='U5 prevalence with and without SMC')
    points(460,55, pch=20, col='blue', cex=3)
  } 
  png(paste(save_plot_filepath, '/compare_before_after_SMC_with_reference_',sim_version_name,'_CM_NMF_adjusted_simOnly.png', sep=''), width=10, height=6, units='in', res=900)
  # hist(df_first_smc$noSMC_num_U5_treat_sim - df_first_smc$SMC_num_U5_treat_sim,breaks=seq(-50,400,20),  main='reduction in incidence from year before to first year of SMC')
  par(mfrow=c(1,2))
  hist((1-(df_first_smc$SMC_num_U5_treat_sim/df_first_smc$noSMC_num_U5_treat_sim ))*100,breaks=seq(-75,100,5),  main='Percent reduction in U5 incidence', xlab=c('percent reduction in incidence', ' from year before to first year of SMC'), ylab='frequency among simulated LGAs')
  for(ii in c(80, 82, 70, 86, 60, 74,75,82)){
    # abline(v=ii, col=rgb(0,0,1,0.5), lwd=1)  # Zongo et al. 2015
    arrows(x0=ii, y1=25,y0=30, col=rgb(0,0,1,0.75), lwd=1, length=0.1)  # Zongo et al. 2015
  }
  legend('topleft', legend=c('ref=Zongo et al. 2015'), col=rgb(0,0,1,0.75), pch=c('V'), bty='n')
  
  hist((1-(df_first_smc$SMC_PfPR_U5/df_first_smc$noSMC_PfPR_U5))*100, breaks=seq(-75,100,5), main='Percent reduction in U5 prevalence', xlab=c('percent reduction in prevalence', ' from year before to first year of SMC'), ylab='frequency among simulated LGAs')
  abline(v=60, col='blue', lwd=3)  # Druetz et al. 2018
  legend('topleft', legend=c('ref=Druetz et al. 2018'), col=rgb(0,0,1,0.75), pch=c('|'), bty='n')
  dev.off()
}






##===================================================================================================================================##
# linear regression to see whether SMC impact decreases through time in a DS ( potentially as a result of resistance)
##===================================================================================================================================##
if(FALSE){
  par(mfrow=c(1,2))
  with(merged_df[merged_df$month == plot_month,], plot(frac_U5_dhis2~SMC_years, col=rainbow(15)[round(10*coverage_U5)+1], ylab='fraction of cases in U5 in DHIS2 dataset', xlab='year of SMC', bty='L'))
  lm1 <- lm(frac_U5_dhis2~SMC_years+admin_name+coverage_U5, data=merged_df[merged_df$month==9,])
  anova(lm1)
  
  plot(NA, ylim=c(0,0.8), xlim=c(0,5), ylab='fraction of cases in U5 in DHIS2 dataset', xlab='year of SMC', bty='L')
  for(i_ds in 1:length(unique(merged_df$admin_name))){
    cur_df = merged_df[intersect(which(merged_df$month==plot_month), which(merged_df$admin_name == unique(merged_df$admin_name)[i_ds])),]
    lines(cur_df$SMC_years, cur_df$frac_U5_dhis2, col='black')#rainbow(70)[i_ds])
    points(cur_df$SMC_years, cur_df$frac_U5_dhis2, col=rainbow(5)[cur_df$year-2014], pch=20)#rainbow(70)[i_ds])
  }
  legend('bottomright', legend=c(2015,2016,2017,2018), pch=20, col=rainbow(5)[1:4], bty='n')
  
}



  
  # 
  # 
  # # plot timeseries of number of cases in dhis2 data for separate ds, U5 as well as O5
  # plot_individual_ds = FALSE
  # if(plot_individual_ds){
  #   admin_names_plot = c(unique(dt$admin_name)[10:18])
  #   par(mfrow=c(3,3), mar=c(2,2,3,2))
  #   for (i_ds in 1:length(admin_names_plot)){
  #     ds_df = dt[admin_name == admin_names_plot[i_ds],]
  #     ds_df = ds_df[order(ds_df$date),]
  #     plot(ds_df$date, ds_df[[u5_dhis_col]], type='b', pch=c(21,19)[ds_df$received_smc+1], main=paste('DHIS2: ', admin_names_plot[i_ds]))
  #     lines(ds_df$date, ds_df[[o5_dhis_col]], type='b', col='red')
  #   }
  #   par(mfrow=c(3,3), mar=c(2,2,3,2))
  #   for (i_ds in 1:length(admin_names_plot)){
  #     ds_df = dt[admin_name == admin_names_plot[i_ds],]
  #     ds_df = ds_df[order(ds_df$date),]
  #     plot(ds_df$date, ds_df$sim_maltreat_u5, type='b', pch=c(21,19)[ds_df$received_smc+1], main=paste('sim: ', admin_names_plot[i_ds]))
  #     lines(ds_df$date, ds_df$sim_maltreat_ov5, type='b', col='red')
  #   }
  #   par(mfrow=c(1,1))
  # }
  # 
  # 
  # plot_ds_timeseries_all = FALSE
  # if(plot_ds_timeseries_all){
  #   png(paste(save_plot_filepath, '/compare_timeseries_sim_dhis2_monthly_frac_U5_',sim_version_name,'_CM_NMF_adjusted.png', sep=''), width=6, height=7, units='in', res=900)
  #   par(mar=c(4,4,2,2), mfrow=c(2,1))
  #   # plot all ds on one figure, showing fraction of cases in U5 instead of number of cases in U5 and number in O5
  #   smc_cols=c('black','blue','green')
  #   admin_names_plot = c(unique(dt$admin_name))
  #   ds_df = dt[admin_name == admin_names_plot[1],]
  #   plot(NA, xlim=c(min(ds_df$date), max(ds_df$date)), ylim=c(0,1), xlab='date', ylab = 'fraction cases U5', axes=FALSE, main='DHIS2 data')
  #   axis(1, ds_df$date, format(ds_df$date, "%b %Y"))
  #   axis(2)
  #   for (i_ds in 1:length(admin_names_plot)){
  #     ds_df = dt[admin_name == admin_names_plot[i_ds],]
  #     ds_df = ds_df[order(ds_df$date),]
  #     lines(ds_df$date, ds_df[[u5_dhis_col]]/(ds_df[[u5_dhis_col]] + ds_df[[o5_dhis_col]]), type='l', col=rgb(0.5,0.5,0.5,0.5))
  #     lines(ds_df$date, ds_df[[u5_dhis_col]]/(ds_df[[u5_dhis_col]] + ds_df[[o5_dhis_col]]), type='p', col=smc_cols[1+ds_df$received_this_year+ds_df$received_smc], pch=20, cex=0.5)
  #   }
  #   
  #   plot(NA, xlim=c(min(ds_df$date), max(ds_df$date)), ylim=c(0,1), xlab='date', ylab = 'fraction cases U5', axes=FALSE, main='simulation output')
  #   axis(1, ds_df$date, format(ds_df$date, "%b %Y"))
  #   axis(2)
  #   for (i_ds in 1:length(admin_names_plot)){
  #     ds_df = dt[admin_name == admin_names_plot[i_ds],]
  #     ds_df = ds_df[order(ds_df$date),]
  #     lines(ds_df$date, ds_df$sim_maltreat_u5/(ds_df$sim_maltreat_u5 + ds_df$sim_maltreat_ov5), type='l', col=rgb(0.5,0.5,0.5,0.5))
  #     lines(ds_df$date, ds_df$sim_maltreat_u5/(ds_df$sim_maltreat_u5 + ds_df$sim_maltreat_ov5), type='p', col=smc_cols[1+ds_df$received_this_year+ds_df$received_smc], pch=20, cex=0.5)
  #   }
  #   dev.off()
  #   par(mfrow=c(1,1))
  # }
  # 
  # 
  # 
  # if(FALSE){
  #   # show PfPR in U5 and O5
  #   yy = length(years_included)
  #   merged_df_yy = merged_df %>% filter(year == years_included[yy])
  #   par(mfrow=c(1,2))
  #   plot(NA, ylim=c(0,1), xlim=c(0.5, 12.5), xlab='month', ylab='PfPR U5', bty='L', main=paste('sim -', years_included[yy]))
  #   for(mm in 1:12){
  #     merged_df_mm = merged_df_yy %>% filter(month == mm)
  #     values = merged_df_mm$PfPR_U5
  #     points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
  #   }
  #   plot(NA, ylim=c(0,1), xlim=c(0.5, 12.5), xlab='month', ylab='PfPR O5', bty='L', main=paste('sim -', years_included[yy]))
  #   for(mm in 1:12){
  #     merged_df_mm = merged_df_yy %>% filter(month == mm)
  #     values = merged_df_mm$PfPR_O5
  #     points((mm+runif(length(values),-0.3,0.3)), values, col=rev(viridis(12))[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
  #   }
  # }
  # if(FALSE){
  #   # for DS that have data from non-SMC years and SMC years, compare the number of cases during the smc months in the no-SMC years with the number of cases in the with-SMC years.
  #   # for each DS, get mean number of cases per month for months 8, 9, 10, 11 (the months following SMC administration, since SMC is simulated at end of month) without and with SMC in that year.
  #   # subset to relevant months
  #   dt_compare_months = dt[month %in% c(8,9,10,11),]
  #   # divide into df with no smc and df wth smc
  #   dt_smc = dt_compare_months[received_this_year==TRUE,]
  #   dt_no_smc = dt_compare_months[received_this_year==FALSE,]
  #   # get mean numbers for each DS
  #   dt_smc_mean0 = dt_smc %>% 
  #     group_by(admin_name) %>%
  #     summarise_all(funs(mean(., na.rm=TRUE))) %>%
  #     ungroup()
  #   dt_smc_mean = dt_smc_mean0[(!is.nan(dt_smc_mean0[[u5_dhis_col]])),]
  #   colnames(dt_smc_mean)[colnames(dt_smc_mean) == u5_dhis_col] = 'dhis_u5_smc'
  #   colnames(dt_smc_mean)[colnames(dt_smc_mean) == o5_dhis_col] = 'dhis_o5_smc'
  #   colnames(dt_smc_mean)[colnames(dt_smc_mean) == 'sim_maltreat_u5'] = 'sim_u5_smc'
  #   colnames(dt_smc_mean)[colnames(dt_smc_mean) == 'sim_maltreat_ov5'] = 'sim_o5_smc'
  #   dt_no_smc_mean0 = dt_no_smc %>% 
  #     group_by(admin_name) %>%
  #     summarise_all(funs(mean(., na.rm=TRUE))) %>%
  #     ungroup()
  #   dt_no_smc_mean = dt_no_smc_mean0[(!is.nan(dt_no_smc_mean0[[u5_dhis_col]])),]
  #   colnames(dt_no_smc_mean)[colnames(dt_no_smc_mean) == u5_dhis_col] = 'dhis_u5_no_smc'
  #   colnames(dt_no_smc_mean)[colnames(dt_no_smc_mean) == o5_dhis_col] = 'dhis_o5_no_smc'
  #   colnames(dt_no_smc_mean)[colnames(dt_no_smc_mean) == 'sim_maltreat_u5'] = 'sim_u5_no_smc'
  #   colnames(dt_no_smc_mean)[colnames(dt_no_smc_mean) == 'sim_maltreat_ov5'] = 'sim_o5_no_smc'
  #   
  #   # data table with the mean values for SMC-relevant months for DS that had years with and without SMC
  #   # dt_maltreat = merge(dt_smc_mean[,c('admin_name', 'maltreat_u5_smc', 'maltreat_ov5_smc')], dt_no_smc_mean[,c('admin_name', 'maltreat_u5_no_smc', 'maltreat_ov5_no_smc')])
  #   dt_maltreat = merge(dt_smc_mean, dt_no_smc_mean, by='admin_name')
  #   # dhis2
  #   plot(dt_maltreat$maltreat_u5_no_smc, dt_maltreat$maltreat_u5_smc, 
  #        xlim=c(min(c(dt_maltreat[[paste0(u5_dhis_col, '_no_smc')]], dt_maltreat[[paste0(u5_dhis_col, '_smc')]])), max(c(dt_maltreat[[paste0(u5_dhis_col, '_no_smc')]], dt_maltreat[[paste0(u5_dhis_col, '_smc')]]))),
  #        ylim=c(min(c(dt_maltreat[[paste0(u5_dhis_col, '_no_smc')]], dt_maltreat[[paste0(u5_dhis_col, '_smc')]])), max(c(dt_maltreat[[paste0(u5_dhis_col, '_no_smc')]], dt_maltreat[[paste0(u5_dhis_col, '_smc')]]))))
  #   abline(a=0, b=1, col='grey')
  #   plot(dt_maltreat$maltreat_ov5_no_smc, dt_maltreat$maltreat_ov5_smc, 
  #        xlim=c(min(c(dt_maltreat$maltreat_ov5_no_smc, dt_maltreat$maltreat_ov5_smc)), max(c(dt_maltreat$maltreat_ov5_no_smc, dt_maltreat$maltreat_ov5_smc))),
  #        ylim=c(min(c(dt_maltreat$maltreat_ov5_no_smc, dt_maltreat$maltreat_ov5_smc)), max(c(dt_maltreat$maltreat_ov5_no_smc, dt_maltreat$maltreat_ov5_smc))))
  #   abline(a=0, b=1, col='grey')
  #   plot(dt_maltreat$maltreat_u5_no_smc/dt_maltreat$maltreat_ov5_no_smc, dt_maltreat$maltreat_u5_smc/dt_maltreat$maltreat_ov5_smc, 
  #        xlim=c(0,2.5),
  #        ylim=c(0,2.5))
  #   abline(a=0, b=1, col='grey')
  #   mean( (dt_maltreat$maltreat_u5_smc/dt_maltreat$maltreat_ov5_smc) / (dt_maltreat$maltreat_u5_no_smc/dt_maltreat$maltreat_ov5_no_smc))
  #   # simulations
  #   plot(dt_maltreat$sim_maltreat_u5_no_smc, dt_maltreat$sim_maltreat_u5_smc, main='simulation results')
  #   abline(a=0, b=1, col='grey')
  #   plot(dt_maltreat$sim_maltreat_u5_no_smc/dt_maltreat$sim_maltreat_ov5_no_smc, dt_maltreat$sim_maltreat_u5_smc/dt_maltreat$sim_maltreat_ov5_smc, 
  #        xlim=c(0,2.5),
  #        ylim=c(0,2.5), main='simulation results')
  #   abline(a=0, b=1, col='grey')
  #   mean( (dt_maltreat$sim_maltreat_u5_smc/dt_maltreat$sim_maltreat_ov5_smc) / (dt_maltreat$sim_maltreat_u5_no_smc/dt_maltreat$sim_maltreat_ov5_no_smc))
  #   
  #   # simulations, showing all DS (not just those with DHIS2 in pre- and post-SMC periods)
  #   dt_smc_mean = dt_smc_mean0[(!is.nan(dt_smc_mean0$sim_maltreat_u5)),]
  #   colnames(dt_smc_mean)[colnames(dt_smc_mean) == 'sim_maltreat_u5'] = 'sim_maltreat_u5_smc'
  #   colnames(dt_smc_mean)[colnames(dt_smc_mean) == 'PfPR_U5'] = 'PfPR_U5_smc'
  #   dt_no_smc_mean = dt_no_smc_mean0[(!is.nan(dt_no_smc_mean0$sim_maltreat_u5)),]
  #   colnames(dt_no_smc_mean)[colnames(dt_no_smc_mean) == 'sim_maltreat_u5'] = 'sim_maltreat_u5_no_smc'
  #   colnames(dt_no_smc_mean)[colnames(dt_no_smc_mean) == 'PfPR_U5'] = 'PfPR_U5_no_smc'
  #   dt_maltreat = merge(dt_smc_mean, dt_no_smc_mean, by='admin_name')
  #   plot(dt_maltreat$sim_maltreat_u5_no_smc/dt_maltreat$Pop_U5.x*1000, dt_maltreat$sim_maltreat_u5_smc/dt_maltreat$Pop_U5.x*1000, main='simulation U5 incidence before and after SMC',
  #        xlim=c(120,600), ylim=c(20,180), xlab='no SMC: incidence (per 1000) in U5', ylab='with SMC: incidence (per 1000) in U5')
  #   abline(a=0, b=1, col='grey')
  #   abline(a=0,b=0.2, col='blue', lty=3)
  #   text(x=200,y=50,'80% reduction', col='blue', srt=15)
  #   points(x=c(135,460), y=c(24,55), pch=20, col='blue')
  #   plot(dt_maltreat$PfPR_U5_no_smc, dt_maltreat$PfPR_U5_smc, main='simulation U5 prevalence before and after SMC',
  #        xlim=c(0,1), ylim=c(0,0.4), xlab='no SMC: prevalence in U5', ylab='with SMC: prevalence in U5')
  #   abline(a=0, b=1, col='grey')
  #   abline(a=0,b=0.4, col='blue', lty=3)
  #   text(x=0.16,y=0.083,'60% reduction', col='blue', srt=22)
  #   points(x=c(0.46,0.18, 0.068), y=c(0.18,0.057,0.035), pch=20, col='blue')
  # }
  # 
  # 
  # # Fraction U5 cases in DHIS2/sims by month (boxplot) with spline fit, 2017-2018, districts with SMC
  # ggplot()+
  #   theme_bw()+
  #   geom_boxplot(data=dt[year>2016 & received_this_year==TRUE],
  #                aes(x=date,y=frac_U5_sim_cm_nmf_adjusted,group=date,
  #                    color=factor(month)))+
  #   geom_smooth(data=dt[year>2016 & received_this_year==TRUE],
  #               aes(x=date,y=frac_U5_sim_cm_nmf_adjusted))+
  #   # geom_boxplot(data=dt[year>2016 & received_this_year==TRUE],
  #   #              aes(x=date,y=frac_U5_dhis2,group=date,
  #   #                  color=factor(month)))+
  #   geom_smooth(data=dt[year>2016 & received_this_year==TRUE],
  #               aes(x=date,y=frac_U5_dhis2),color="black")+
  #   ylab('Fraction of malaria cases from U5, CM and NMF adjusted')+
  #   xlab('')+
  #   # ggtitle(paste('SMC multiplier=', smc_coverage_multiplier, '; adherence23 multiplier=', adherence23_multiplier, '; U5 NMF=', num_nmf_month_u5, sep=''))
  #   ggtitle(paste(sim_version_name, '; monthly U5 NMF=', num_nmf_month_u5, sep=''))
  # 
  # # ggsave(paste0(save_plot_filepath,"/compare_sim_dhis2_timecourse_smc",smc_coverage_multiplier*100,"_adh",adherence23_multiplier*100,"_nmf",num_nmf_month_u5*100,".pdf"))
  # ggsave(paste0(save_plot_filepath,"/compare_sim_dhis2_timecourse_", sim_version_name,"_nmf",num_nmf_month_u5*100,".png"))
  # 
  # 
  # # normalized to July
  # # Create column normalizing DHIS2 data to specific month (here, July)
  # base_month = 7
  # norm_frac = dt[month==base_month,.(admin_name,year,month,frac_U5_dhis2)]
  # setnames(norm_frac,"frac_U5_dhis2","temp_frac_U5_dhis2")
  # norm_frac[,month:=NULL]
  # norm_frac = distinct(norm_frac)
  # if(!('temp_frac_U5_dhis2' %in% colnames(dt))) dt = merge(dt, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
  # dt[,norm_frac_U5_dhis2:=frac_U5_dhis2-temp_frac_U5_dhis2]
  # 
  # # Create column normalizing CM-adjusted sims to specific month (here, July)
  # norm_frac = dt[month==base_month,.(admin_name,year,month,frac_U5_sim_cm_nmf_adjusted)]
  # setnames(norm_frac,"frac_U5_sim_cm_nmf_adjusted","temp_frac_U5_sim_cm_nmf_adjusted")
  # norm_frac[,month:=NULL]
  # norm_frac = distinct(norm_frac)
  # if(!('temp_frac_U5_sim_cm_nmf_adjusted' %in% colnames(dt))) dt = merge(dt, norm_frac, by=c("admin_name", "year"), all.x=TRUE)
  # dt[,norm_frac_U5_sim_cm_nmf_adjusted:=frac_U5_sim_cm_nmf_adjusted-temp_frac_U5_sim_cm_nmf_adjusted]
  # 
  # ggplot()+
  #   theme_bw()+
  #   geom_boxplot(data=dt[(year==2018 & received_this_year==TRUE)][month>=base_month],
  #                aes(x=month,y=norm_frac_U5_sim_cm_nmf_adjusted,group=interaction(date)),
  #                color=rgb(0,0.5,0.5,0.5))+
  #   geom_smooth(data=dt[(year==2018 & received_this_year==TRUE)][month>=base_month],
  #               aes(x=month,y=norm_frac_U5_sim_cm_nmf_adjusted,group=received_this_year),
  #               color=rgb(0,0.5,0.5))+
  #   # geom_boxplot(data=dt[(year==2018 & received_this_year==TRUE)][month>6],
  #   #              aes(x=month,y=norm_frac_U5_dhis2,group=interaction(date)), color='black')+
  #   geom_smooth(data=dt[(year==2018 & received_this_year==TRUE)][month>=base_month],
  #               aes(x=month,y=norm_frac_U5_dhis2,group=received_this_year), color="black")+
  #   ylab('Fraction of malaria cases from U5, normalized to July in given year')+
  #   xlab('')+
  #   # ggtitle(paste('SMC multiplier=', smc_coverage_multiplier, '; adherence23 multiplier=', adherence23_multiplier, '; U5 NMF=', num_nmf_month_u5, sep=''))
  #   ggtitle(paste(sim_version_name,'; monthly U5 NMF=', num_nmf_month_u5, sep=''))
  # # show all months of year, not just after July
  # ggplot()+
  #   theme_bw()+
  #   geom_violin(data=dt[(year==2018 & received_this_year==TRUE)],
  #               aes(x=month,y=norm_frac_U5_dhis2,group=interaction(date)), color='black', fill=NA)+
  #   geom_violin(data=dt[(year==2018 & received_this_year==TRUE)],
  #               aes(x=month,y=norm_frac_U5_sim_cm_nmf_adjusted,group=interaction(date)),
  #               color=rgb(0,0.5,0.5,0.5), fill=NA)+
  #   geom_smooth(data=dt[(year==2018 & received_this_year==TRUE)],
  #               aes(x=month,y=norm_frac_U5_dhis2,group=received_this_year), color="black")+
  #   geom_smooth(data=dt[(year==2018 & received_this_year==TRUE)],
  #               aes(x=month,y=norm_frac_U5_sim_cm_nmf_adjusted,group=received_this_year),
  #               color=rgb(0,0.5,0.5))+
  #   
  #   ylab('Fraction of malaria cases from U5, normalized to July in 2018')+
  #   xlab('')+
  #   # ggtitle(paste('SMC multiplier=', smc_coverage_multiplier, '; adherence23 multiplier=', adherence23_multiplier, '; U5 NMF=', num_nmf_month_u5, sep=''))
  #   ggtitle(paste(sim_version_name,'; monthly U5 NMF=', num_nmf_month_u5, sep=''))
  # # ggsave(paste0(save_plot_filepath,"/compare_sim_dhis2_relativeJuly_smc",smc_coverage_multiplier*100,"_adh",adherence23_multiplier*100,"_nmf",num_nmf_month_u5*100,".pdf"))
  # ggsave(paste0(save_plot_filepath,"/compare_sim_dhis2_relativeJuly_",sim_version_name,"_nmf",num_nmf_month_u5*100,".png"))
  # 
  # 
  # 
  # 
  


# plot of number of cases reported in each DS from DHIS2 data - looks at seasonality
seasonality_dhis2 = FALSE
if(seasonality_dhis2){
  plot(merged_df$month+runif(length(merged_df$month), -0.2,0.2), (merged_df$maltreat_u5 + merged_df$maltreat_ov5), pch=20, cex=0.5, ylim=c(0,120000), main='total number of treated malaria', ylab='number treated', xlab='month (jittered)', bty='L')
  # plot(merged_df_yy$month+runif(length(merged_df_yy$month), -0.2,0.2), (merged_df_yy$maltreat_u5 + merged_df_yy$maltreat_ov5), pch=20, cex=0.5, ylim=c(0,80000), main='treated malaria cases 2018', ylab='number treated', xlab='month (jittered)', bty='L')
}

# scatter plots (DHIS2 vs sim) for all DS-month-year values: fraction of cases U5 in each ds-month
scatter_all = FALSE
if(scatter_all){
  plot(merged_df$frac_U5_dhis2, merged_df$frac_U5_sim, cex=0.5, col=rainbow(12)[merged_df$month], pch=c(19,21)[as.factor(merged_df$received_smc)])
}

# scatter plots (DHIS2 vs sim) broken out by year: fraction of cases U5 in each ds-month
scatter_year = FALSE
if(scatter_year){
  pdf(paste(save_plot_filepath, '/compare_smc_sim_dhis2_scatter_year_', dhis_string, '_smcMult', smc_coverage_multiplier*100, '.pdf', sep=''), width=5, height=5, useDingbats=FALSE)
  for (yy in 1:length(years_included)){
    merged_df_yy = merged_df %>% filter(year == years_included[yy])
    plot(merged_df_yy$frac_U5_dhis2, merged_df_yy$frac_U5_sim, cex=0.5, col=rainbow(12)[merged_df_yy$month], pch=c(19,21)[as.factor(merged_df_yy$received_smc)],
         main=years_included[yy], xlab='dhis2 fraction of cases U5', ylab='simulation fraction of cases U5', ylim=c(0,1), xlim=c(0,1), bty='L')
    legend('topright', legend=1:12, title='month', col=rainbow(12), pch=20, bty='n')
  }
  dev.off()
}

# monthly fraction of cases U5 in each ds-month, separated by month; separate plots for DHIS2 and sim. Sim data unadjusted for CM or NMF
month_unadjusted = FALSE
if(month_unadjusted){
  
  pdf(paste(save_plot_filepath, '/compare_smc_sim_dhis2_monthly_', dhis_string, '_smcMult', smc_coverage_multiplier*100, '.pdf', sep=''), width=9, height=5.5, useDingbats=FALSE)
  for (yy in 1:length(years_included)){
    merged_df_yy = merged_df %>% filter(year == years_included[yy])
    par(mfrow=c(1,2))
    plot(NA, ylim=c(0,1), xlim=c(0.5, 12.5), xlab='month', ylab='fraction of cases U5', bty='L', main=paste('dhis2 -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy %>% filter(month == mm)
      values = merged_df_mm$frac_U5_dhis2
      points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
    }
    plot(NA, ylim=c(0,1), xlim=c(0.5, 12.5), xlab='month', ylab='fraction of cases U5', bty='L', main=paste('sim -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy %>% filter(month == mm)
      values = merged_df_mm$frac_U5_sim
      points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
    }
  }
  dev.off()
}


# monthly fraction of cases U5 in each ds-month, separated by month; separate plots for DHIS2 and sim. Sim data adjusted for CM, not adjusted for NMF
month_CMadjusted_NMFunadjusted = FALSE
if(month_CMadjusted_NMFunadjusted){
  pdf(paste(save_plot_filepath, '/compare_smc_sim_dhis2_monthly_', dhis_string, '_smcMult', smc_coverage_multiplier*100, '_CMAdjusted.pdf', sep=''), width=9, height=5.5, useDingbats=FALSE)
  for (yy in 1:length(years_included)){
    merged_df_yy = merged_df %>% filter(year == years_included[yy])
    par(mfrow=c(1,2))
    plot(NA, ylim=c(0,1), xlim=c(0.5, 12.5), xlab='month', ylab='fraction of cases U5', bty='L', main=paste('dhis2 -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy %>% filter(month == mm)
      values = merged_df_mm$frac_U5_dhis2
      points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
    }
    plot(NA, ylim=c(0,1), xlim=c(0.5, 12.5), xlab='month', ylab='fraction of cases U5', bty='L', main=paste('sim -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy %>% filter(month == mm)
      values = merged_df_mm$frac_U5_sim_cm_adjusted
      points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
    }
  }
  dev.off()
}




# number treated in U5 and in all ages relative to June of that year; separate plots for DHIS2 and sim. Sim data adjusted for CM but not for NMF
relative_month_CMadjusted_NMFunadjusted = FALSE
if(relative_month_CMadjusted_NMFunadjusted){

  pdf(paste(save_plot_filepath, '/compare_number_treated_relative_June_sim_dhis2_', dhis_string, '_smcMult', smc_coverage_multiplier*100, '_CMAdjusted.pdf', sep=''), width=9, height=5.5, useDingbats=F)
  for (yy in 1:length(years_included)){
    merged_df_yy = merged_df %>% filter(year == years_included[yy])
    merged_df_yy_jan = merged_df_yy %>% filter(month == 6)
    par(mfrow=c(2,2))
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases U5 relative to June', bty='L', main=paste('dhis2 -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm[[u5_dhis_col]] / merged_df_yy_jan[[u5_dhis_col]]
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases U5 relative to June', bty='L', main=paste('sim -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm$New_clinical_cases_U5 / merged_df_yy_jan$New_clinical_cases_U5
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases O5 relative to June', bty='L', main=paste('dhis2 -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm[[o5_dhis_col]] / merged_df_yy_jan[[o5_dhis_col]]
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases O5 relative to June', bty='L', main=paste('sim -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm$New_Clinical_Cases / merged_df_yy_jan$New_Clinical_Cases
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
  }
  dev.off()
  
  
  
  # only admins with SMC
  pdf(paste(save_plot_filepath, '/compare_number_treated_relative_June_sim_dhis2_', dhis_string, '_smcMult', smc_coverage_multiplier*100, '_CMAdjusted_receivedSMCOnly.pdf', sep=''), width=9, height=5.5, useDingbats=F)
  for (yy in 1:length(years_included)){
    merged_df_yy = merged_df %>% filter(year == years_included[yy])
    merged_df_yy_jan = merged_df_yy[merged_df_yy$received_smc,] %>% filter(month == 6)
    par(mfrow=c(2,2))
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases U5 relative to June', bty='L', main=paste('dhis2 -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy[merged_df_yy$received_smc,] %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm[[u5_dhis_col]] / merged_df_yy_jan[[u5_dhis_col]]
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases U5 relative to June', bty='L', main=paste('sim -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy[merged_df_yy$received_smc,] %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm$New_clinical_cases_U5 / merged_df_yy_jan$New_clinical_cases_U5
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases O5 relative to June', bty='L', main=paste('dhis2 -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy[merged_df_yy$received_smc,] %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm[[o5_dhis_col]] / merged_df_yy_jan[[o5_dhis_col]]
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases O5 relative to June', bty='L', main=paste('sim -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy[merged_df_yy$received_smc,] %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm$New_Clinical_Cases / merged_df_yy_jan$New_Clinical_Cases
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
  }
  dev.off()
  
  
  
  
  # only admins with out SMC
  pdf(paste(save_plot_filepath, '/compare_number_treated_relative_June_sim_dhis2_', dhis_string, '_smcMult', smc_coverage_multiplier*100, '_CMAdjusted_NoSMCOnly.pdf', sep=''), width=9, height=5.5, useDingbats=F)
  for (yy in 1:length(years_included)){
    merged_df_yy = merged_df %>% filter(year == years_included[yy])
    merged_df_yy_jan = merged_df_yy[!(merged_df_yy$received_smc),] %>% filter(month == 6)
    par(mfrow=c(2,2))
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases U5 relative to June', bty='L', main=paste('dhis2 -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy[!(merged_df_yy$received_smc),] %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm[[u5_dhis_col]] / merged_df_yy_jan[[u5_dhis_col]]
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases U5 relative to June', bty='L', main=paste('sim -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy[!(merged_df_yy$received_smc),] %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm$New_clinical_cases_U5 / merged_df_yy_jan$New_clinical_cases_U5
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases O5 relative to June', bty='L', main=paste('dhis2 -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy[!(merged_df_yy$received_smc),] %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm[[o5_dhis_col]] / merged_df_yy_jan[[o5_dhis_col]]
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
    plot(NA, ylim=c(0,20), xlim=c(0.5, 12.5), xlab='month', ylab='number of cases O5 relative to June', bty='L', main=paste('sim -', years_included[yy]))
    for(mm in 1:12){
      merged_df_mm = merged_df_yy[!(merged_df_yy$received_smc),] %>% filter(month == mm)
      # check that the ds are in the same order
      if(all(merged_df_mm$admin_name == merged_df_yy_jan$admin_name)){
        values = merged_df_mm$New_Clinical_Cases / merged_df_yy_jan$New_Clinical_Cases
        points((mm+runif(length(values),-0.3,0.3)), values, col=rainbow(12)[mm], pch=c(19,21)[as.factor(merged_df_mm$received_smc)])
      } else warning('ds names ordering not the same for each month... need to sort')
    }
  }
  dev.off()
}



# timeseries of cases and PfPR and cases in U5 and all ages
plot_sim_timeseries = FALSE
if(plot_sim_timeseries){
  sim_burden = as.data.table(sim_burden)
  sim_burden[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
  
  # PfPR
  ggplot() + geom_line(data = sim_burden, aes(x = date, y = PfPR_U5, color = admin_name)) + 
    theme_bw()+
    theme(legend.position='none') 
  ggsave(paste0(save_plot_filepath,"/PfPR_U5_timeseries_", sim_version_name,".png"))
  
  ggplot() + geom_line(data = sim_burden, aes(x = date, y = PfPR_MiP_adjusted, color = admin_name)) + 
    theme_bw()+
    theme(legend.position='none') 
  ggsave(paste0(save_plot_filepath,"/PfPR_allAge_timeseries_", sim_version_name,".png"))
  
  # Clinical cases
  ggplot() + geom_line(data = sim_burden, aes(x = date, y = New_clinical_cases_U5, color = admin_name)) + 
    theme_bw()+
    ylim(0,600)+
    theme(legend.position='none') 
  ggsave(paste0(save_plot_filepath,"/ClinicalCases_U5_timeseries_", sim_version_name,".png"))
  
  ggplot() + geom_line(data = sim_burden, aes(x = date, y = New_Clinical_Cases, color = admin_name)) + 
    theme_bw()+
    ylim(0,1200)+
    theme(legend.position='none') 
  ggsave(paste0(save_plot_filepath,"/ClinicalCases_allAge_timeseries_", sim_version_name,".png"))
  
  # fraction of cases in U5
  ggplot() + geom_line(data = sim_burden, aes(x = date, y = frac_U5_sim_cm_nmf_adjusted, color = admin_name)) + 
    theme_bw()+
    ylim(0,1)+
    theme(legend.position='none') 
  ggsave(paste0(save_plot_filepath,"/FracU5_ClinicalCases_allAge_timeseries_", sim_version_name,".png"))
}
























###############################################################################################################
# compare old and new simulation output with dhis2
###############################################################################################################


# --- simulation output --- #
num_nmf_month_u5 = 0.2  # estimated monthly probability of NMF for U5
num_nmf_month_o5 = num_nmf_month_u5 / 2  # estimated monthly probability of NMF for O5
rel_prob_cm_o5 = 0.5  # relative to U5, what is the probability someone O5 seeks treatment for fever

sim_output_stem_filepath = paste(box_filepath, "/hbhi_burkina/simulation_output/2010_to_2020/baseline_2010_to_2020/BF 70DS ", sep='')
old_sim_filepath =  paste(sim_output_stem_filepath,  "2010-2020 v11 small/malariaBurden_withAdjustments.csv", sep='')
adherence23_multiplier = 0.8
new_sim_filepath =  paste(sim_output_stem_filepath,  "smcRound_100_90_90_90 adh23_",adherence23_multiplier*100," sulfC50200/malariaBurden_withAdjustments.csv", sep='')

sim_burden_old = get_sim_data(sim_output_filepath=old_sim_filepath, rel_prob_cm_o5=rel_prob_cm_o5, num_nmf_month_u5=num_nmf_month_u5, num_nmf_month_o5=num_nmf_month_o5, years_included=years_included)
sim_burden_new = get_sim_data(sim_output_filepath=new_sim_filepath, rel_prob_cm_o5=rel_prob_cm_o5, num_nmf_month_u5=num_nmf_month_u5, num_nmf_month_o5=num_nmf_month_o5, years_included=years_included)

# --- merge datasets --- #
merged_df_old = merge_smc_datasets(smc_info, dhis2_data_sums, sim_burden_old)
merged_df_new = merge_smc_datasets(smc_info, dhis2_data_sums, sim_burden_new)

merged_df_old$U5_fever_with_malaria_old = (merged_df_old$New_clinical_cases_U5 + merged_df_old$m_NMF_u5)
# merged_df_old$PfPR_U5_old = merged_df_old$PfPR_U5
merged_df_old$Pop_U5_old = merged_df_old$Pop_U5
merged_df_old_2 = merged_df_old %>% dplyr::select(month, year, admin_name, Pop_U5_old, U5_fever_with_malaria_old, maltreat_u5)  #, received_smc)
merged_df_new$U5_fever_with_malaria_new = (merged_df_new$New_clinical_cases_U5 + merged_df_new$m_NMF_u5)
# merged_df_new$PfPR_U5_new = merged_df_new$PfPR_U5
merged_df_new$Pop_U5_new = merged_df_new$Pop_U5
merged_df_new_2 = merged_df_new %>% dplyr::select(month, year, admin_name, Pop_U5_new, U5_fever_with_malaria_new)

merged_df = merge(merged_df_old_2, merged_df_new_2, by=c('month', 'year','admin_name'))
head(merged_df)

# get population size of each DS to calculate incidence for the dhis2 data
ds_pop = fread(pop_arch_filepath)[,c('admin_name', 'population')]
merged_df_2 = merge(merged_df, ds_pop, by='admin_name')
head(merged_df_2)


ds_list = c("Boulsa", "Dafra", "Djibo",
            "Kaya","Nanoro", "Nouna",
            "Sapone", "Sapouy","Ziniare")
# ds_list = unique(dtsum$admin_name)
merged_df_2 = as.data.table(merged_df_2)
merged_df_2[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
merged_df_2[,old_sim_U5_incidence:=U5_fever_with_malaria_old/Pop_U5_old]
merged_df_2[,new_sim_U5_incidence:=U5_fever_with_malaria_new/Pop_U5_new]
merged_df_2[,dhis_U5_incidence:=maltreat_u5/(population * 0.167718)]  # population age distribution from www.populationpyramid.net


ggplot(merged_df_2[admin_name%in%ds_list])+
  geom_line(aes(x=date,y=dhis_U5_incidence), color='black')+theme_bw()+facet_wrap(~admin_name)+
  geom_line(aes(x=date,y=old_sim_U5_incidence), color=rgb(1,0.6, 0.5))+theme_bw()+facet_wrap(~admin_name)+
  geom_line(aes(x=date,y=new_sim_U5_incidence), color=rgb(0.4, 0.8,0.8))+theme_bw()+facet_wrap(~admin_name)+
  # geom_line(data=smc[admin_name%in%ds_list & run==0],
  #           aes(x=date,y=old_sim_U5_incidence,group=cat,color=cat))+theme_bw()+facet_wrap(~admin_name)+
  ylab("incidence in U5")+
  # scale_color_discrete(name="")+
  xlim(c(as.Date("2015-01-01"),NA)) 




# get montly totals across all DS (sum of new cases (U5_fever_with_malaria_old, U5_fever_with_malaria_new and maltreat_U5) and sum of population sizes)
merged_df_3 <- merged_df_2 %>%group_by(year, month) %>%
  summarise(sum(Pop_U5_old), sum(Pop_U5_new),
            sum(U5_fever_with_malaria_old), sum(U5_fever_with_malaria_new),
            sum(maltreat_u5), sum(population)) %>% ungroup()
colnames(merged_df_3) = c('year', 'month', 'Pop_U5_old', 'Pop_U5_new', 'U5_fever_with_malaria_old', 'U5_fever_with_malaria_new', 'maltreat_u5', 'population')
merged_df_3 = as.data.table(merged_df_3)
merged_df_3[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
merged_df_3[,old_sim_U5_incidence:=U5_fever_with_malaria_old/Pop_U5_old]
merged_df_3[,new_sim_U5_incidence:=U5_fever_with_malaria_new/Pop_U5_new]
merged_df_3[,dhis_U5_incidence:=maltreat_u5/(population * 0.167718)]  # population age distribution from www.populationpyramid.net

ggplot(merged_df_3)+
  geom_line(aes(x=date,y=dhis_U5_incidence), color='black')+theme_bw()+
  geom_line(aes(x=date,y=old_sim_U5_incidence), color=rgb(1,0.6, 0.5))+theme_bw()+
  geom_line(aes(x=date,y=new_sim_U5_incidence), color=rgb(0.4, 0.8,0.8))+theme_bw()+
  # geom_line(data=smc[admin_name%in%ds_list & run==0],
  #           aes(x=date,y=old_sim_U5_incidence,group=cat,color=cat))+theme_bw()+facet_wrap(~admin_name)+
  ylab("incidence in U5")+
  # scale_color_discrete(name="")+
  xlim(c(as.Date("2015-01-01"),NA)) 





###############################################################################################################
# look across multiple adjustments for SMC coverage and NMF treatment-seeking rates
###############################################################################################################

# create data table that has  values for all combinations of smc multipliers and nmf rates

smc_coverage_multiplier_vector = c(1, 0.5, 0.3, 0.1, 0.01)  # c(1, 0.5, 0.4, 0.3, 0.2, 0.1, 0.01)
adherence23_multiplier_vector = c(1, 1, 1, 1, 1)
num_nmf_month_u5_vector = c(0, 0.2, 0.4)  # estimated monthly probability of NMF for U5
num_nmf_month_o5_vector = num_nmf_month_u5_vector / 2  # estimated monthly probability of NMF for O5
for(i_smc in 1:length(smc_coverage_multiplier_vector)){
  smc_coverage_multiplier = smc_coverage_multiplier_vector[i_smc]
  adherence23_multiplier = adherence23_multiplier_vector[i_smc]
  for(i_nmf in 1:length(num_nmf_month_u5_vector)){
    num_nmf_month_u5 = num_nmf_month_u5_vector[i_nmf]
    num_nmf_month_o5 = num_nmf_month_o5_vector[i_nmf]
    # simulation output
    sim_output_filepath = paste(sim_output_stem_filepath, smc_coverage_multiplier*100, "_adh", adherence23_multiplier*100, "/malariaBurden_withAdjustments.csv", sep='')
    sim_burden = get_sim_data(sim_output_filepath=sim_output_filepath, rel_prob_cm_o5=rel_prob_cm_o5, num_nmf_month_u5=num_nmf_month_u5, num_nmf_month_o5=num_nmf_month_o5, years_included=years_included)
    # merge datasets
    merged_df = merge_smc_datasets(smc_info, dhis2_data_sums, sim_burden)
    # add columns indicating the smc and nmf scenario
    merged_df$smc_multiplier = smc_coverage_multiplier
    merged_df$NMF_u5 = num_nmf_month_u5
    
    if((i_smc ==1) & (i_nmf == 1)){
      combined_dt = as.data.table(merged_df)
    } else{
      combined_dt = rbindlist(list(combined_dt, as.data.table(merged_df)))
    }
  }
}


dt = combined_dt
# Add some useful columns for plotting
dt[,months_received:=sum(received_smc==TRUE,na.rm=TRUE),by=.(year,admin_name)]
dt[,received_this_year:=months_received>0]
dt[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]




# Fraction U5 cases in DHIS2/sims by month (boxplot) with spline fit, 2017-2018, districts with SMC
ggplot()+
  theme_bw()+
  geom_boxplot(data=dt[year>2016 & received_this_year==TRUE],
               aes(x=date,y=frac_U5_sim_cm_nmf_adjusted,group=date,
                   color=factor(month)))+
  geom_smooth(data=dt[year>2016 & received_this_year==TRUE],
              aes(x=date,y=frac_U5_sim_cm_nmf_adjusted))+
  # geom_boxplot(data=dt[year>2016 & received_this_year==TRUE],
  #              aes(x=date,y=frac_U5_dhis2,group=date,
  #                  color=factor(month)))+
  geom_smooth(data=dt[year>2016 & received_this_year==TRUE],
              aes(x=date,y=frac_U5_dhis2),color="black")+
  facet_grid(smc_multiplier~NMF_u5)+
  ylab('Fraction of malaria cases from U5, CM and NMF adjusted')+
  xlab('')
ggsave(paste0(save_plot_filepath,"/compare_sim_dhis2_timecourse_multipleSMC_multipleNMF.pdf"))


# normalize to July
# Create column normalizing DHIS2 data to specific month (here, July)
norm_frac = dt[month==7,.(admin_name,year,month,smc_multiplier,NMF_u5,frac_U5_dhis2)]
setnames(norm_frac,"frac_U5_dhis2","temp_frac_U5_dhis2")
norm_frac[,month:=NULL]
dt = merge(dt,norm_frac,by=c("admin_name","year", "smc_multiplier", "NMF_u5"))
dt[,norm_frac_U5_dhis2:=frac_U5_dhis2-temp_frac_U5_dhis2]

# Create column normalizing CM-adjusted sims to specific month (here, July)
norm_frac = dt[month==7,.(admin_name,year,month,smc_multiplier,NMF_u5,frac_U5_sim_cm_nmf_adjusted)]
setnames(norm_frac,"frac_U5_sim_cm_nmf_adjusted","temp_frac_U5_sim_cm_nmf_adjusted")
norm_frac[,month:=NULL]
dt = merge(dt,norm_frac,by=c("admin_name","year", "smc_multiplier", "NMF_u5"))
dt[,norm_frac_U5_sim_cm_nmf_adjusted:=frac_U5_sim_cm_nmf_adjusted-temp_frac_U5_sim_cm_nmf_adjusted]

ggplot()+
  theme_bw()+
  geom_boxplot(data=dt[(year==2018 & received_this_year==TRUE)][month>6],
               aes(x=month,y=norm_frac_U5_sim_cm_nmf_adjusted,group=interaction(date)),
               color=rgb(0,0.5,0.5,0.5))+
  geom_smooth(data=dt[(year==2018 & received_this_year==TRUE)][month>6],
              aes(x=month,y=norm_frac_U5_sim_cm_nmf_adjusted,group=received_this_year),
              color=rgb(0,0.5,0.5))+
  # geom_boxplot(data=dt[(year==2018 & received_this_year==TRUE)][month>6],
  #              aes(x=month,y=norm_frac_U5_dhis2,group=interaction(date)), color='black')+
  geom_smooth(data=dt[(year==2018 & received_this_year==TRUE)][month>6],
              aes(x=month,y=norm_frac_U5_dhis2,group=received_this_year), color="black")+
  facet_grid(smc_multiplier~NMF_u5)+
  ylab('Fraction of malaria cases from U5, normalized to July in given year')+
  xlab('')
ggsave(paste0(save_plot_filepath,"/compare_sim_dhis2_relativeJuly_multipleSMC_multipleNMF.pdf"))










