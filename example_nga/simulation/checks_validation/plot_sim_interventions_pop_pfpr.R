# check simulation results: population size, vector abundance, PfPR, clinical cases, interventions

# download results from COMPS: ReportEventCounter.json and Report MalariaFiltered.json and plot key simulation occurrences
library('rjson')
library('ggplot2')

base_filepath = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration/hbhi_nigeria'
json_download_filepath = paste0(base_filepath, '/snt_2022/simulation_output/2010_to_present/NGA_toPresent_allInter_v6/json_download')
cur_sim_folder = 'toPresent_Bindawa'
# cur_sim_folder = 'toPresent_Fakai'


create_sim_checking_plots = function(json_download_filepath, cur_sim_folder){
  
  event_filename = paste0(json_download_filepath, '/',cur_sim_folder, '/ReportEventCounter.json')
  filtered_filename = paste0(json_download_filepath, '/',cur_sim_folder, '/ReportMalariaFiltered.json')
  # interventions delivered and used
  events = fromJSON(file=event_filename)
 # human and vector population sizes, climate, PfPR, clinical cases, etc.
  filtered = fromJSON(file=filtered_filename)  
  
  # pdf(file = paste0(json_download_filepath, '/',cur_sim_folder, '/check_sim_output_plots.pdf'), width=12, height=8, useDingbats=FALSE)
  png(file = paste0(json_download_filepath, '/',cur_sim_folder, '/check_sim_output_plots1.png'), width=12, height=8, units='in', res=300)
  par(mfrow=c(2,3))
  
  # human population
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`Statistical Population`$Data, xlab='simulation day', ylab='count', bty='L', main='human population', type='l')
  # plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$Births$Data, xlab='simulation day', ylab='count', bty='L', main='human births', type='l')

  # interventions delivered and used
  if('Bednet_Got_New_One' %in% names(events$Channels)) {
    if(length(seq(events$Header$Start_Time, events$Header$Timesteps-1, by=events$Header$Simulation_Timestep)) == length(events$Channels$Bednet_Got_New_One$Data)){
      plot(seq(events$Header$Start_Time, events$Header$Timesteps-1, by=events$Header$Simulation_Timestep), events$Channels$Bednet_Got_New_One$Data, xlab='simulation day', ylab='number of events', bty='L', main='received new bednet', type='l')
    } else if(length(seq(events$Header$Start_Time, events$Header$Timesteps, by=events$Header$Simulation_Timestep)) == length(events$Channels$Bednet_Got_New_One$Data)){
      plot(seq(events$Header$Start_Time, events$Header$Timesteps, by=events$Header$Simulation_Timestep), events$Channels$Bednet_Got_New_One$Data, xlab='simulation day', ylab='number of events', bty='L', main='received new bednet', type='l')
    }
  }
  if('Bednet_Using' %in% names(events$Channels)) plot(seq(events$Header$Start_Time, events$Header$Timesteps-1, by=events$Header$Simulation_Timestep), events$Channels$Bednet_Using$Data, xlab='simulation day', ylab='number of events', bty='L', main='using bednet', type='l')
  if('Received_NMF_Treatment' %in% names(events$Channels)) plot(seq(events$Header$Start_Time, events$Header$Timesteps-1, by=events$Header$Simulation_Timestep), events$Channels$Received_NMF_Treatment$Data, xlab='simulation day', ylab='number of events', bty='L', main='received NMF treatment', type='l')
  # plot(seq(events$Header$Start_Time, events$Header$Timesteps-1, by=events$Header$Simulation_Timestep), events$Channels$Received_Severe_Treatment$Data, xlab='simulation day', ylab='number of events', bty='L', main='received severe treatment', type='l')
  if('Received_Treatment' %in% names(events$Channels)) plot(seq(events$Header$Start_Time, events$Header$Timesteps-1, by=events$Header$Simulation_Timestep), events$Channels$Received_Treatment$Data, xlab='simulation day', ylab='number of events', bty='L', main='received treatment', type='l')
  if('Received_IRS' %in% names(events$Channels)) plot(seq(events$Header$Start_Time, events$Header$Timesteps-1, by=events$Header$Simulation_Timestep), events$Channels$Received_IRS$Data, xlab='simulation day', ylab='number of events', bty='L', main='received IRS', type='l')
  if('Received_Campaign_Drugs' %in% names(events$Channels)) plot(seq(events$Header$Start_Time, events$Header$Timesteps-1, by=events$Header$Simulation_Timestep), events$Channels$Received_Campaign_Drugs$Data, xlab='simulation day', ylab='number of events', bty='L', main='received SMC', type='l')
  
  dev.off()
  
  
  png(file = paste0(json_download_filepath, '/',cur_sim_folder, '/check_sim_output_plots2.png'), width=24, height=8, units='in', res=300)
  # climate and vectors
  par(mfcol=c(2,6))
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`Air Temperature`$Data, xlab='simulation day', ylab='measure', bty='L', main='air temperature', type='l')
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`Land Temperature`$Data, xlab='simulation day', ylab='measure', bty='L', main='land temperature', type='l')
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`Relative Humidity`$Data, xlab='simulation day', ylab='measure', bty='L', main='relative humidity', type='l')
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`Rainfall`$Data, xlab='simulation day', ylab='measure', bty='L', main='rainfall', type='l')
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`Adult Vectors`$Data, xlab='simulation day', ylab='count', bty='L', main='adult vectors', type='l')
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`Daily Bites`$Data, xlab='simulation day', ylab='count', bty='L', main='daily bites', type='l')
    
  # malaria
  # par(mfcol=c(2,3))
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`Daily EIR`$Data, xlab='simulation day', ylab='measure', bty='L', main='daily EIR', type='l')
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`True Prevalence`$Data, xlab='simulation day', ylab='measure', bty='L', main='true prevalence', type='l')
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`Blood Smear Parasite Prevalence`$Data, xlab='simulation day', ylab='measure', bty='L', main='prevalence (microscopy)', type='l')
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`PfHRP2 Prevalence`$Data, xlab='simulation day', ylab='measure', bty='L', main='prevalence (HRP2 RDT)', type='l')
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`New Infections`$Data, xlab='simulation day', ylab='count', bty='L', main='new infections', type='l')
  plot(seq(filtered$Header$Start_Time, filtered$Header$Timesteps-1, by=filtered$Header$Simulation_Timestep), filtered$Channels$`New Clinical Cases`$Data, xlab='simulation day', ylab='count', bty='L', main='new clinical cases', type='l')
  
  dev.off()
  par(mfrow=c(1,1))
  
  # # monthly treated clinical cases plus treated non-malaria fevers
  # clinical_daily = events$Channels$Received_Treatment$Data
  # nmf_daily = events$Channels$Received_NMF_Treatment$Data
  # daily_cases = data.frame(simday = 1:length(nmf_daily), obs_cases = (clinical_daily + nmf_daily))
  # daily_cases$date = as.Date(daily_cases$simday, origin='2011-01-01')
  # daily_cases$month = month(daily_cases$date)
  
  
}

count_num_each_year = function(cur_events){
  years_in_events = length(cur_events)/365
  if((years_in_events - floor(years_in_events))>0.05){
    warning('POTENTIAL ISSUE: it appears that only part of a year was included in output, not certain which days correspond to which year...')
  }
  years_in_events = floor(years_in_events)
  sum_events = rep(NA, years_in_events)
  
  for(yy in 1:years_in_events){
    sum_events[yy] = sum(cur_events[((yy-1)*365+1) : (yy*365)])
  }
  return(sum_events)
}

# calculate number of interventions received in each year
total_annual_interventions_cases = function(json_download_filepath, cur_sim_folder){
  
  event_filename = paste0(json_download_filepath, '/',cur_sim_folder, '/ReportEventCounter.json')
  filtered_filename = paste0(json_download_filepath, '/',cur_sim_folder, '/ReportMalariaFiltered.json')
  # interventions delivered and used
  events = fromJSON(file=event_filename)
  # human and vector population sizes, climate, PfPR, clinical cases, etc.
  filtered = fromJSON(file=filtered_filename)  
  
  # pdf(file = paste0(json_download_filepath, '/',cur_sim_folder, '/check_sim_output_plots.pdf'), width=12, height=8, useDingbats=FALSE)
  png(file = paste0(json_download_filepath, '/',cur_sim_folder, '/num_interventions_each_year.png'), width=8, height=8, units='in', res=300)
  par(mfrow=c(2,2))
  
  # interventions delivered
  if('Bednet_Got_New_One' %in% names(events$Channels)){
    cur_events = events$Channels$Bednet_Got_New_One$Data
    sum_events = count_num_each_year(cur_events) 
    plot(c(rep(1:length(sum_events), each=2)[-1], length(sum_events), length(sum_events)+1), c(rep(sum_events, each=2), sum_events[length(sum_events)]), xlab='simulation year', ylab='number of events', bty='L', main='received new bednet', type='l')
  } 
  if('Received_NMF_Treatment' %in% names(events$Channels)){
    cur_events = events$Channels$Received_NMF_Treatment$Data
    sum_events = count_num_each_year(cur_events) 
    plot(c(rep(1:length(sum_events), each=2)[-1], length(sum_events), length(sum_events)+1), c(rep(sum_events, each=2), sum_events[length(sum_events)]), xlab='simulation year', ylab='number of events', bty='L', main='received NMF treatment', type='l')
  } 
  if('Received_Treatment' %in% names(events$Channels)) {
    cur_events = events$Channels$Received_Treatment$Data
    sum_events = count_num_each_year(cur_events) 
    plot(c(rep(1:length(sum_events), each=2)[-1], length(sum_events), length(sum_events)+1), c(rep(sum_events, each=2), sum_events[length(sum_events)]), xlab='simulation year', ylab='number of events', bty='L', main='received treatment', type='l')
  } 
    if('Received_IRS' %in% names(events$Channels)) {
    cur_events = events$Channels$Received_IRS$Data
    sum_events = count_num_each_year(cur_events) 
    plot(c(rep(1:length(sum_events), each=2)[-1], length(sum_events), length(sum_events)+1), c(rep(sum_events, each=2), sum_events[length(sum_events)]), xlab='simulation year', ylab='number of events', bty='L', main='received IRS', type='l')
  } 
  dev.off()
  

  # cases of malaria each year (total infections and clinical cases)
  png(file = paste0(json_download_filepath, '/',cur_sim_folder, '/num_cases_each_year.png'), width=8, height=8, units='in', res=300)
  par(mfrow=c(2,2))
  
  # malaria new infections
  cur_events = filtered$Channels$`New Infections`$Data
  sum_events = count_num_each_year(cur_events) 
  plot(c(rep(1:length(sum_events), each=2)[-1], length(sum_events), length(sum_events)+1), c(rep(sum_events, each=2), sum_events[length(sum_events)]), xlab='simulation year', ylab='count', bty='L', main='new infections', type='l')
  # malaria clinical cases
  cur_events = filtered$Channels$`New Clinical Cases`$Data
  sum_events = count_num_each_year(cur_events) 
  plot(c(rep(1:length(sum_events), each=2)[-1], length(sum_events), length(sum_events)+1), c(rep(sum_events, each=2), sum_events[length(sum_events)]), xlab='simulation year', ylab='count', bty='L', main='clinical cases', type='l')
  
  dev.off()
  par(mfrow=c(1,1))
}




create_sim_checking_plots(json_download_filepath, cur_sim_folder)
total_annual_interventions_cases(json_download_filepath, cur_sim_folder)










# seasonality calibration check
# this is similar to the plotted results from replot_seasonality_best_fit.py, though here the reference dataset isn't shown and we can plot more years. 
# create plot of treated malaria-positive cases across multiple seeds in the seasonality calibration
# (goal = see how long the impact of ITN distribution lasts and whether it's consistent with observed surveillance datasets)
cases_download_filepath = "C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/simulation_output/seasonality_calibration/TEST17blonger_2017_seasonality_calibration_1arch_Gitega_round2_iter0"
sample=0
cases_download_filepath = "C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/simulation_output/seasonality_calibration/seasonality_calibration_1arch_Gitega_2017_round1_iter10"
sample = 26
cases = read.csv(paste0(cases_download_filepath, '/All_Age_monthly_Cases.csv'))
cases = cases[cases$X__sample_index__ == sample,]
cases$treated_cases = cases$Received_NMF_Treatment + cases$Received_Treatment
cases$date = as.Date(cases$date)
ggplot(cases, aes(x=date, y=treated_cases, group=Run_Number)) + 
  geom_line() + 
  geom_vline(xintercept=as.Date('2011-07-15'), color=rgb(197/255,90/255,17/255),size=1.5)+
  geom_vline(xintercept=as.Date('2014-07-15'), color=rgb(197/255,90/255,17/255),size=1.5)+
  theme_classic()



