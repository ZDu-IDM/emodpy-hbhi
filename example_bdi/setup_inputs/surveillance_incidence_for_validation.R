# surveillance_incidence_for_validation.R
# NOTE: this script is specific to the Burundi data extraction

# turn data-grabbed csvs of case counts or incidence into file format needed to compare routine surveillance with simulation output
# output file should have the following columns:
#  - date
#  - year
#  - month
#  - obs_annual_incidence - cases per 1000 people per year, recorded in each month = average weekly incidence * 52 (aggregated to a national level)



library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)

# set whether to use the country-level incidence or archetype-level incidence
use_country_incidence = TRUE
country_name = 'Burundi'
extracted_data_dir_country = "C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/data/Burundi/PMI_MOP_caseSeasonality/all_months"
extracted_data_dir_archetype = "C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/data/Burundi/WHO/Incidence"

base_filepath_raster =  paste0(box_filepath, '/burundi_hbhi/SpatialClustering')

output_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/simulation_inputs/incidence'


itn_distribution_years = c(2010, 2014, 2017, 2019)
itn_distribution_months = c(1, 7, 9, 12)
itn_distribution_dates = as.Date(paste0(itn_distribution_years, '-', itn_distribution_months,'-15'))


# simulation output
hbhi_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
sim_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_to_present/BDI_2010_2020_allInter')
pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')




########################################################################################
#  country-level data-grab from PMI report
#   input files have week-of-year as first column, incidence as second column
#   one file per year
########################################################################################
# from csvs with plot-grabbed values of weekly cases recorded in the country for several years, 
#    1) get mean weekly cases within each month
#    3) multiply by 52/12 to get mean monthly number of reported cases in each month

extracted_data_dir = extracted_data_dir_country

# upper bound (in weeks) for each month
month_days = cumsum(c(31,28,31,30,31,30,31,31,30,31,30,31))
month_weeks = c(0,month_days/7)  # for month mm, week values between month_weeks[mm] and month_weeks[mm+1] will be included in that month

# read in extracted data
input_years = 2013:2017
input_files = paste0('PMI_caseSeason_all_extract_', input_years, '.csv')

# get average weekly cases in each month
monthly_averages = data.frame()
for(ii in 1:length(input_years)){
  extracted_values = read.csv(paste0(extracted_data_dir, '/', input_files[ii]), header=FALSE)
  # extract average values within each month (x-axis units are in weeks)
  monthly_averages_cur = data.frame('year' = rep(input_years[ii], 12), 'month' = 1:12, 'mean_weekly_cases' = rep(NA,12))
  for(mm in 1:12){
    monthly_averages_cur$mean_weekly_cases[mm] = mean(extracted_values[intersect(which(extracted_values[,1]>=month_weeks[mm]), which(extracted_values[,1]<month_weeks[mm+1])),2], na.rm=TRUE)
  }
  if(nrow(monthly_averages) == 0){
    monthly_averages = monthly_averages_cur
  } else{
    monthly_averages = rbind(monthly_averages,  monthly_averages_cur)
  }
}
# add date
monthly_averages$date = as.Date(paste0(monthly_averages$year, '-', monthly_averages$month, '-01'))





########################################################################################
#  archetype-level data-grab 
#   input files have year as first column, number of confirmed RDTs as second column
#   one file per archetype
########################################################################################
# from csvs with plot-grabbed values of monthly cases recorded each archetype for several years, 
#    1) get mean monthly cases within each month (original may have several grabbed data points within a month, 
#       take the one closest to the start of the month where the month's value is recorded to avoid averaging with the previous and following months' values)


extracted_data_dir = extracted_data_dir_archetype

# read in extracted data
input_files = list.files(extracted_data_dir, pattern='RDT_confirmed_extract_arch', full.names=FALSE)
input_files = sort(input_files)

# iterate through archetypes, saving RDT confirmed cases in all months
for(ii in 1:length(input_files)){
  extracted_values = read.csv(paste0(extracted_data_dir, '/', input_files[ii]), header=FALSE)

  # get time values as close as possible to the start of the month when that month's values will have been recorded. Month starts on day 0 and for plot is split evenly into 12 divisions within year
  plot_month_values = seq(min(floor(extracted_values[,1])), max(floor(extracted_values[,1])+11/12), by=1/12)
  
  # save dataframe for montly values across all years
  monthly_values = data.frame('admin_group' = rep(ii, length(plot_month_values)), 'year' = floor(plot_month_values), 'plot_month_value' = plot_month_values)
  monthly_values$month = (monthly_values$plot_month_value - monthly_values$year) * 12 + 1
  # check that data extraction went as planned and the month values are correct
  if(!all(round(monthly_values$month) %in% 1:12)) warning('PROBLEM DETECTED: extracted month values are not correct')
  if(any(abs(round(monthly_values$month) - monthly_values$month)>0.05)) warning('PROBLEM DETECTED: extracted month values are not integers')
  monthly_values$month = as.integer(round(monthly_values$month))
  
  # get the number of RDT-confirmed cases in each month by recording the plot-grabbed value closest to plot_month_value
  monthly_values$matching_extract_row = sapply(monthly_values$plot_month_value, FUN=function(x)which.min(abs(x - extracted_values[,1])))
  monthly_values$RDT_confirmed = extracted_values[monthly_values$matching_extract_row,2]

  if(ii ==1){
    monthly_values_all_arch = monthly_values
  } else{
    monthly_values_all_arch = rbind(monthly_values_all_arch, monthly_values)
  }
}
monthly_values_all_arch$date = as.Date(paste0(monthly_values_all_arch$year, '-', monthly_values_all_arch$month, '-01'))
monthly_values_sum = monthly_values_all_arch %>% group_by(month, year, date) %>%
  summarise(RDT_confirmed = sum(RDT_confirmed))
monthly_values_sum = monthly_values_sum[order(monthly_values_sum$date),]





########################################################################################
#  simulation output
########################################################################################
# from csvs with number of treated cases and NMF treated cases in each admin and month,
#    1) get sum of treated malaria and NMF treated
#    2) rescale to full admin population size
#    3) get mean numbers across runs
#     

# get true population sizes in each admin
pop_sizes = read.csv(pop_filepath)
# get simulation output
sim_treated = read.csv(paste0(sim_output_dir, '/monthly_Event_Count.csv'))
# get simulation population size in each month
sim_pop = read.csv(paste0(sim_output_dir, '/MonthlyUsageLLIN.csv'))
sim_pop$sim_pop_size = sim_pop$Statistical.Population
sim_treated_pop = merge(sim_treated, sim_pop, by=c('admin_name', 'Run_Number', 'date'))

# get total number treated in each month
sim_treated_pop$total_treated = sim_treated_pop$Received_Treatment + sim_treated_pop$Received_Severe_Treatment + sim_treated_pop$Received_NMF_Treatment
sim_treated_pop = merge(sim_treated_pop[,which(colnames(sim_treated_pop) %in% c('admin_name', 'Run_Number', 'total_treated', 'date', 'sim_pop_size'))], pop_sizes[,which(colnames(pop_sizes) %in% c('admin_name', 'pop_size'))], by='admin_name', all.x=TRUE)
sim_treated_pop$rescaled_total_treated = sim_treated_pop$total_treated / sim_treated_pop$sim_pop_size * sim_treated_pop$pop_size

# get average across runs
sim_treated_pop_ave = sim_treated_pop %>% group_by(date, admin_name) %>%
  summarise_all(mean)

# get total across all admins
sim_treated_pop_national = sim_treated_pop_ave[,-which(colnames(sim_treated_pop_ave) == 'admin_name')] %>% group_by(date) %>%
  summarise_all(sum)




##################################################
# plot PMI versus WHO versus simulation
##################################################
pmi_color = rgb(0,0.85,0)
who_color = rgb(0,0.4,1)
sim_color = rgb(0.6,0,0.6)
min_y_val = min(c(monthly_averages$mean_weekly_cases*52/12, monthly_values_sum$RDT_confirmed, sim_treated_pop_national$rescaled_total_treated))
max_y_val = max(c(monthly_averages$mean_weekly_cases*52/12, monthly_values_sum$RDT_confirmed, sim_treated_pop_national$rescaled_total_treated))
plot(monthly_averages$date, monthly_averages$mean_weekly_cases*52/12, xlim=c(as.Date('2013-06-01'), as.Date('2019-12-01')), ylim=c(min_y_val, max_y_val), type='l', ylab='monthly number of reported cases', xlab='date', bty='L', col=pmi_color, lwd=2)
lines(monthly_values_sum$date, monthly_values_sum$RDT_confirmed, type='l', col=who_color, lwd=2)
lines(as.Date(sim_treated_pop_national$date), sim_treated_pop_national$rescaled_total_treated, type='l', col=sim_color, lwd=2)
abline(v=itn_distribution_dates, col=rgb(0.6,0.5,0.5,0.2), lwd=4)
legend('topleft', c('confirmed cases (from PMI report)', 'RDT confirmed cases (from Bea)', 'simulation treated cases'), col=c(pmi_color, who_color, sim_color), lwd=2, bty='n')


if(!dir.exists(paste0(sim_output_dir,'/_plots'))) dir.create(paste0(sim_output_dir,'/_plots'))
# build plot sequence
# legend
png(filename=paste0(sim_output_dir,'/_plots/validate_numCases_timeseries_legend.png'), width=6, height=4.5, units='in', res=900)
plot(NA, axes=FALSE, ylab='', xlab='',ylim=c(0,1), xlim=c(0,1))
legend('center', c('confirmed cases (from PMI report)', 'RDT confirmed cases (from Bea)', 'simulation treated cases'), col=c(pmi_color, who_color, sim_color), lwd=2, bty='n')
dev.off()
# PMI reference data only
png(filename=paste0(sim_output_dir,'/_plots/validate_numCases_timeseries_0.png'), width=6, height=4.5, units='in', res=900)
plot(monthly_averages$date, monthly_averages$mean_weekly_cases*52/12, xlim=c(as.Date('2013-06-01'), as.Date('2019-12-01')), ylim=c(min_y_val, max_y_val), type='l', ylab='monthly number of reported cases', xlab='date', bty='L', col=pmi_color, lwd=2)
abline(v=itn_distribution_dates, col=rgb(0.6,0.5,0.5,0.2), lwd=4)
dev.off()
# WHO and PMI reference data only
png(filename=paste0(sim_output_dir,'/_plots/validate_numCases_timeseries_1.png'), width=6, height=4.5, units='in', res=900)
plot(monthly_averages$date, monthly_averages$mean_weekly_cases*52/12, xlim=c(as.Date('2013-06-01'), as.Date('2019-12-01')), ylim=c(min_y_val, max_y_val), type='l', ylab='monthly number of reported cases', xlab='date', bty='L', col=pmi_color, lwd=2)
lines(monthly_values_sum$date, monthly_values_sum$RDT_confirmed, type='l', col=who_color, lwd=2)
abline(v=itn_distribution_dates, col=rgb(0.6,0.5,0.5,0.2), lwd=4)
dev.off()
# add simulation data
png(filename=paste0(sim_output_dir,'/_plots/validate_numCases_timeseries_2.png'), width=6, height=4.5, units='in', res=900)
plot(monthly_averages$date, monthly_averages$mean_weekly_cases*52/12, xlim=c(as.Date('2013-06-01'), as.Date('2019-12-01')), ylim=c(min_y_val, max_y_val), type='l', ylab='monthly number of reported cases', xlab='date', bty='L', col=pmi_color, lwd=2)
lines(monthly_values_sum$date, monthly_values_sum$RDT_confirmed, type='l', col=who_color, lwd=2)
lines(as.Date(sim_treated_pop_national$date), sim_treated_pop_national$rescaled_total_treated, type='l', col=sim_color, lwd=2)
abline(v=itn_distribution_dates, col=rgb(0.6,0.5,0.5,0.2), lwd=4)
dev.off()



