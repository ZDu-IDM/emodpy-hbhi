# monthly_incidence_for_seasonality.R
# NOTE: this script is specific to the Burundi data extraction

# turn data-grabbed csvs of case counts or incidence into file format needed for seasonality calibration
# output file should have the following columns:
#  - month (values 1-12) - each seasonality archetype should have 12 rows, one for each month
#  - cases - rescaled number of cases per month
#  - seasonality_archetype - name of representative admin for archetype
#  - population - population size (may or may not be rescaled)
#  - incidence = cases/population * 1000
# recommendation: the case counts should be rescaled to give monthly incidence between 0-max_case_rescale (unless there's a reason not to)



library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)

# set whether to use the country-level incidence or archetype-level incidence
use_country_incidence = TRUE
country_name = 'Burundi'
extracted_data_dir_country = "C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/data/Burundi/PMI_MOP_caseSeasonality"
extracted_data_dir_archetype = "C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/data/Burundi/WHO/Incidence"
hbhi_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/snt_2023'

base_filepath_raster =  paste0(hbhi_dir, '/SpatialClustering')
var_weight_string = 'vector1_rain3_tsi2_centroid2_pre2010itn2'

output_dir = paste0(hbhi_dir, '/simulation_inputs/incidence')
if(!dir.exists(output_dir)) dir.create(output_dir)

# rescale monthly cases so that the maximum number of cases in any month is max_case_rescale
max_case_rescale = 100

########################################################################################
#  country-level data-grab 
#   input files have week-of-year as first column, incidence as second column
#   one file per year
########################################################################################
# from csvs with plot-grabbed values of weekly cases recorded in the country for several years, 
#    1) get mean weekly cases within each month
#    2) rescale years to have comprable means then take average across all years
#    3) rescale again so that maximum monthly incidence is max_case_rescale cases per 1000
#    4) save results as csv readable for seasonality calibration

extracted_data_dir = extracted_data_dir_country

# get country-level representative admin
arch_representatives = read.csv(paste0(base_filepath_raster, '/clustering_results/', var_weight_string, '/clara_DS_cluster_representatives.csv'))
# find the column in the archetype assignments dataframe that correspond to the current number of clusters
cur_col_rep = which(colnames(arch_representatives) == paste('representative_DS_1clusters', sep=''))  
archetype_rep = arch_representatives[which(arch_representatives$cluster_id == 1), cur_col_rep]
if(length(archetype_rep) != 1) warning('PROBLEM DETECTED: there should be only one representative admin for the country-level cluster, but more or fewer have been retrieved.')

# upper bound (in weeks) for each month
month_days = cumsum(c(31,28,31,30,31,30,31,31,30,31,30,31))
month_weeks = c(0,month_days/7)  # for month mm, week values between month_weeks[mm] and month_weeks[mm+1] will be included in that month
# week_days = seq(7,365,7)
# max_week_val = sapply(month_days, function(x) max(which(week_days<x)))

# read in extracted data
input_files = list.files(extracted_data_dir, pattern='PMI_caseSeason_extract', full.names=FALSE)
input_files = sort(input_files)
input_years = 2013:2017

monthly_averages = data.frame('month' = 1:12)
for(ii in 1:length(input_files)){
  extracted_values = read.csv(paste0(extracted_data_dir, '/', input_files[ii]), header=FALSE)
  # extract average values within each month (x-axis units are in weeks)
  monthly_averages = cbind(monthly_averages, data.frame('mean_weekly_cases' = rep(NA,12)))
  for(mm in 1:12){
    monthly_averages$mean_weekly_cases[mm] = mean(extracted_values[intersect(which(extracted_values[,1]>=month_weeks[mm]), which(extracted_values[,1]<month_weeks[mm+1])),2], na.rm=TRUE)
  }
  colnames(monthly_averages)[colnames(monthly_averages)=='mean_weekly_cases'] = paste0('mean_weekly_cases_y',ii)
}

# rescale values so that average of first five months is constant across all years (set to be equal to value for second dataset)
rescaled_monthly_averages = monthly_averages
averaging_months = 1:6
ref_mean = mean(monthly_averages$mean_weekly_cases_y2[averaging_months])
rescale_months = c(1,2, 4,5)  # note that ii=3 is 2015, which begins one year after 2014 ITN distribution. we assume that it has the same scaling as 2014 (the reference)
for(ii in rescale_months){
  cur_mean = mean(monthly_averages[[paste0('mean_weekly_cases_y',ii)]][averaging_months])
  rescaled_monthly_averages[[paste0('mean_weekly_cases_y',ii)]] = monthly_averages[[paste0('mean_weekly_cases_y',ii)]] / cur_mean * ref_mean
}


plot(NA, ylim=c(min(rescaled_monthly_averages[,-1], na.rm=TRUE),max(rescaled_monthly_averages[,-1], na.rm=TRUE)), xlim=c(1,12), xlab='month',ylab='rescaled average weekly cases', bty='L', main=c('rescaled cases', 'data grabbed from PMI plot'))
for(ii in 2:ncol(rescaled_monthly_averages)){
  lines(rescaled_monthly_averages[,ii], col=rainbow((length(input_years)+1))[ii], lwd=3)
}

# get averages across years
rescaled_monthly_averages$mean_all_years = rowMeans(rescaled_monthly_averages[,-1], na.rm=TRUE)
lines(rescaled_monthly_averages$mean_all_years, col=rgb(0,0,0,0.5), lwd=10)
legend('bottomleft', legend=c(input_years,'average'), col=c(rainbow((length(input_years)+1))[2:(length(input_years)+1)], rgb(0,0,0,0.5)), lwd=3, bty='n')


# rescale again so that the maximum cases is max_case_rescale
cur_max = max(rescaled_monthly_averages$mean_all_years, na.rm=TRUE)
rescaled_monthly_averages$cases = rescaled_monthly_averages$mean_all_years / cur_max * max_case_rescale


# save to csv in format used for calibration
seasonality_calibration_df = rescaled_monthly_averages[,c('month','cases')]
seasonality_calibration_df$seasonality_archetype = archetype_rep
seasonality_calibration_df$population = 1000
seasonality_calibration_df$incidence = seasonality_calibration_df$cases

if(use_country_incidence){
  write.csv(seasonality_calibration_df, paste0(output_dir, '/archetype_incidence.csv'))
}




########################################################################################
#  archetype-level data-grab 
#   input files have year as first column, number of confirmed RDTs as second column
#   one file per archetype
########################################################################################
# from csvs with plot-grabbed values of monthly cases recorded each archetype for several years, 
#    1) get mean monthly cases within each month (original may have several grabbed data points within a month, 
#       take the one closest to the start of the month where the month's value is recorded to avoid averaging with the previous and following months' values
#    2) remove year following LLIN mass distribution, take average across all remaining years
#    3) rescale again so that maximum monthly incidence is max_case_rescale cases per 1000
#    4) save results as csv readable for seasonality calibration

num_clusters = 4
extracted_data_dir = extracted_data_dir_archetype

itn_year = 2017
itn_month = 10

# read in extracted data
input_files = list.files(extracted_data_dir, pattern='RDT_confirmed_extract_arch', full.names=FALSE)
input_files = sort(input_files)
# get archetype number associated with each file
arch_ids = as.integer(gsub('.csv','',gsub('RDT_confirmed_extract_arch','',input_files)))

# get the archetype representatives for each of the clusters
if(!all(1:num_clusters == arch_ids)) warning('PROBLEM DETECTED: the number of specified clusters does not match the cluster ids in the input files')
arch_representatives = read.csv(paste0(base_filepath_raster, '/clustering_results/', var_weight_string, '/clara_DS_cluster_representatives.csv'))
# find the column in the archetype assignments dataframe that correspond to the current number of clusters
cur_col_rep = which(colnames(arch_representatives) == paste('representative_DS_',max(arch_ids),'clusters', sep=''))  
archetype_reps = arch_representatives[arch_ids, cur_col_rep]
  
# iterate through archetypes, saving RDT confirmed cases in all months
for(ii in 1:length(arch_ids)){
  extracted_values = read.csv(paste0(extracted_data_dir, '/', input_files[ii]), header=FALSE)

  # get time values as close as possible to the start of the month when that month's values will have been recorded. Month starts on day 0 and for plot is split evenly into 12 divisions within year
  plot_month_values = seq(min(floor(extracted_values[,1])), max(floor(extracted_values[,1])+11/12), by=1/12)
  
  # save dataframe for montly values across all years
  monthly_values = data.frame('arch_id' = rep(arch_ids[ii], length(plot_month_values)), 'year' = floor(plot_month_values), 'plot_month_value' = plot_month_values)
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

# plot these values to make sure they match with the original
par(mfrow=c(2,2))
for(ii in 1:length(arch_ids)){
  monthly_values = monthly_values_all_arch[monthly_values_all_arch$arch_id==arch_ids[ii],]
  plot(monthly_values$plot_month_value, monthly_values$RDT_confirmed, type='l', bty='L', xlab='time', ylab='RDT confirmed cases', main=arch_ids[ii])
}
par(mfrow=c(1,1))


      
# remove value for year following ITN distribution
monthly_values_all_arch_noITN = monthly_values_all_arch[-c(intersect(which(monthly_values_all_arch$year == itn_year), which(monthly_values_all_arch$month >= itn_month)),intersect(which(monthly_values_all_arch$year == (itn_year+1)), which(monthly_values_all_arch$month < itn_month))),]

# get average within each (non-ITN) month
monthly_means = monthly_values_all_arch_noITN[,c('month','RDT_confirmed', 'arch_id')] %>%
  group_by(month, arch_id) %>%
  summarise(mean_RDT = mean(RDT_confirmed))

# in each archetype, plot the years on top of one another as well as the averaged monthly values
par(mfrow=c(2,2))
for(ii in 1:length(arch_ids)){
  monthly_values = monthly_values_all_arch_noITN[monthly_values_all_arch_noITN$arch_id==arch_ids[ii],]
  plot(NA, xlim=c(1,12), ylim=c(min(monthly_values$RDT_confirmed),max(monthly_values$RDT_confirmed)), bty='L', xlab='month', ylab='RDT confirmed cases', main=arch_ids[ii])
  for(yy in 1:length(unique(monthly_values$year))){
    monthly_values_yy = monthly_values[monthly_values$year == unique(monthly_values$year)[yy],]
    lines(monthly_values_yy$month, monthly_values_yy$RDT_confirmed, type='l', col=topo.colors(length(unique(monthly_values_all_arch_noITN$year))+3)[yy], lwd=3)
  }
  cur_mean_values = monthly_means[monthly_means$arch_id==arch_ids[ii],]
  lines(cur_mean_values$month, cur_mean_values$mean_RDT, type='l', lwd=10, col=rgb(0,0,0,0.5))
}
par(mfrow=c(1,1))


# rescale so that the maximum cases in a month (within each archetype) is max_case_rescale
monthly_means_rescaled = monthly_means
for(ii in 1:length(arch_ids)){
  max_cases = max(monthly_means$mean_RDT[monthly_means$arch_id==arch_ids[ii]])
  monthly_means_rescaled$mean_RDT[monthly_means$arch_id==arch_ids[ii]] = monthly_means$mean_RDT[monthly_means$arch_id==arch_ids[ii]] / max_cases * max_case_rescale
}


# plot rescaled monthly values
pp = ggplot(data=monthly_means_rescaled, aes(x=month, y=mean_RDT, col=arch_id, group=arch_id)) +
  geom_line()+
  ylab('average monthly confirmed RDT cases (rescaled)')+
  theme_classic() 
pp


# save in a csv formatted for the seasonality calibration
incidence_df = data.frame('month'=monthly_means_rescaled$month, 
                          'cases'=monthly_means_rescaled$mean_RDT, 
                          'seasonality_archetype' = archetype_reps[monthly_means_rescaled$arch_id],
                          'population'=rep(1000, nrow(monthly_means_rescaled)), 
                          'incidence'=(monthly_means_rescaled$mean_RDT))

if(!use_country_incidence){
  write.csv(incidence_df, paste0(output_dir, '/archetype_incidence.csv'))
}




# plot averaged monthly values for archetype-level and for country-level aggregates
plot(NA, xlim=c(1,12), ylim=c(20,max_case_rescale), ylab='rescaled average monthly cases', xlab='month', bty='L', main='compare reference incidence when using archetypes versus national values')
for(ii in 1:length(arch_ids)){
  monthly_values = monthly_means_rescaled[monthly_means_rescaled$arch_id==arch_ids[ii],]
  lines(monthly_values$month, monthly_values$mean_RDT, col=rgb(0,0.5,1), lwd=2)
}
# country-level
lines(seasonality_calibration_df$month, seasonality_calibration_df$cases, col='black',lwd=4)
legend('bottomleft', c('4 seasonality archetypes','1 seasonality archetype'), lwd=c(2,4), col=c(rgb(0,0.5,1), 'black'), bty='n')









########################################################################################
# plot the incidence used for calibrations
########################################################################################
incidence_df = read.csv(paste0(output_dir, '/archetype_incidence.csv'))
month_names = c('Jan',' Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
incidence_df$month_name = month_names[incidence_df$month]

# note: currently only formatted for single archetype... would need to add additional lines for additional archetypes
par(mgp=c(2,0.8,0))
plot(incidence_df$month, incidence_df$incidence, xlab = 'month', ylab='monthly incidence', bty='L', xaxt='n', type='b', pch=20, lwd=3, ylim=c(min(incidence_df$incidence*0.7), max(incidence_df$incidence*1.1)), col=rgb(96/255,142/255,140/255))
axis(side=1, at=seq(1,12,2), labels=month_names[seq(1,12,2)])
par(mgp=c(3,1,0))
