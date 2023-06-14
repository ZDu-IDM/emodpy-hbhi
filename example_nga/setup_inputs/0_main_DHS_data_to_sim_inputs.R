# 0_main_DHS_data_to_sim_inputs.R
# coordinator script for extracting DHS values, reformatting, sampling simulation input values, and saving csv files to be used as simulation inputs
# Before running this script, must 
#   1) download relevant DHS files for current country
#   2) create a csv for each year where there is DHS data describing the codes used for each variable (use the DHS_data_examination.R script to more easily explore the DHS datasets)

library(pals)
library(prettyGraphs) 
library(rgdal)
library(raster)

# ============================================================================ #
# =========================     USER SPECIFIED     =========================== #
# ============================================================================ #
country = 'NGA'  #'SLE'  # 'BDI'
dta_dir = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration'
script_dir = 'C:/Users/moniqueam/Documents/malaria-snt-core'
variables = c('mic','itn_all','itn_u5','itn_5_10','itn_10_15','itn_15_20','itn_o20','iptp','cm','blood_test', 'art_given_antimal')
itn_variables = c('itn_u5','itn_5_10','itn_10_15','itn_15_20','itn_o20')
create_plots = FALSE
num_samples = 0
min_num_total = 30  # minimum number of individuals surveyed in a district before the region value is used
min_num_total_microscopy = 60  # minimum number of individuals surveyed in a district before the region value is used
colors_range_0_to_1 = rev(add.alpha(pals::parula(101), alpha=0.5))  # specify colorscale. To go from a value vv to a color, take colors_range_0_to_1[1+round(vv*100)]
use_NPC_mass_ITN = FALSE
sample_uncertainty = FALSE

if(country =='NGA'){
  hbhi_dir_base = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration/hbhi_nigeria'
  hbhi_dir = paste0(hbhi_dir_base, '/snt_2022')
  years = c(2010, 2013, 2015, 2018, 2021) # DHS/MIS years
  pfpr_dhs_ref_years=c(2010, 2015, 2018, 2021)
  # read in shapefile with admin boundaries
  admin_shape = shapefile(paste0(dta_dir, '/hbhi_nigeria/SpatialClustering/reference_rasters_shapefiles/NGA_DS_clusteringProjection.shp'))
  admin_shape$NAME_1 = admin_shape$NOMREGION  # NOMDEP should be admin level where modeling occurs (e.g., DS, LGA), NOMREGION should be one admin above that, NAME_1 should be one above that
  admin_shape$NOMREGION = admin_shape$State
  admin_shape$NOMDEP = admin_shape$NOMDEP
  sim_start_year = 2010
  last_sim_year=2021
  season_calib_start_year_burn=1967
  season_calib_start_year=2017
  trans_calib_start_year=2010
  quantiles = c(0.5)
  
  
  # --- parameters for general itn-related values --- #
  mortality_df_name = paste0(hbhi_dir,'/simulation_inputs/intermediate_files/insecticide_resistance/permethrin_mortality_admin_estimates.csv')
  median_llin_retention_est = 2.22  # years (from Amelia's retention time table, SLE=1.47(1.31,1.63), BDI=1.31(1.14,1.47), BFA=1.58(1.41,1.76))
  median_llin_retention_lowerCI = 2.0  # years (from Amelia's retention time table, SLE=1.47(1.31,1.63), BDI=1.31(1.14,1.47), BFA=1.58(1.41,1.76))
  itn_lognorm_sigma = 0.8  # parameter for the lognormal distribution for itn retention function
  frac_reassign_feed_survive = 0.9  # fraction of mosquitos that should feed and die that are assigned to feed and survive in the dtk
  indoor_net_protection = 0.75  # fraction of indoor bites occurring during net-protected hours
  U5_use_given_access=1
  create_itn_kill_block_plots=TRUE
  kill_decay_time=1460
  block_decay_time=730
  rel_vector_df_name = paste0(hbhi_dir,'/ento/DS_vector_rel_abundance.csv')
  
  
  # --- parameters for mass-itn distribution extrapolation --- #
  default_first_coverage = 0.1  # default ITN mass distribution coverage if first distribution took place too early for a DHS survey to be informative
  llin_mass_date_filename = paste0(hbhi_dir, '/interventions/NGA_net_distributions_modifiedFromWHO.csv')
  itn_distributions_by_admin_filename=paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_mass_dist_dates_LGA.csv')
  seasonality_monthly_scalar = read.csv(paste0(hbhi_dir, '/simulation_inputs/ITN_use_seasonality.csv'))$itn_use_scalar  # seasonality scalar for net use: gives rate of net use in each month relative to month with maximum use rate
  maximum_coverage_itn = 0.9
  # assumed distribution years and matching DHS survey for seasonality calibration
  itn_distribution_years_calib = c(2018)  # since the 2010-2020 simulation starts at the first day of 2010, consider the 2009 distribution to occur on the first day of 2010 to get those nets in the simulation
  itn_distribution_months_calib = c(1)
  matching_dhs_years = c(2018)  # DHS survey year following mass ITN distributions (must match order of itn_distribution_years)
  matching_dhs_months = c(10)  # chose the median month across all dhs entries, not specific for each admin
  dhs_rescale_multipliers = 1/seasonality_monthly_scalar[matching_dhs_months]  # need to divide by use in the survey month to get seasonality-controlled use rates
  
  
  
  # --- parameters for mass-itn distribution additional past years --- #
  additional_itn_years = c() # we know there were mass distribution during these years, but we have no information on coverage obtained

  
  
  # --- parameters for routine itn input files --- #
  years_anc_nets = 2018:2022
  sim_2019_llin_anc_filename = paste0(hbhi_dir_base, '/simulation_inputs/projection_csvs/2010_2020_LGA_intervention_files/ITN/ITN_ANC_current_coverage.csv')
  anc_itn_access_filename = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_ANC_dates_LGA.csv')  # data formatted to match that from WHO
  max_anc_itn_access = 0.97
  anc_itn_use_given_access = 0.9  # multiply access by this value to get the use rate in a night during the month with the highest use rates (if there is seasonal use)
  calib_anc_itn_year=2018
  include_EPI = FALSE
  epi_itn_use_given_access = 0.9  # multiply access by this value to get the use rate in a night during the month with the highest use rates (if there is seasonal use)
  birthday_ages=c(1,3)  # for EPI ITN distributions
  coverage_each_birthday=c(0.915, 0.732)  # for EPI ITN distributions
  
  
  
  # --- parameters for cm input files --- #
  cm_variable_name = 'effective_treatment'
  adult_multiplier = 1  # multiply U5 coverage by this value to get adult coverage
  severe_multiplier = 2  # multiply U5 coverage by this value to get severe coverage
  severe_minimum = 0.5  # minimum severe coverage
  maximum_coverage_cm = 0.8  #  maximum coverage for any age/severity
  dhs_year_cm_burnin=2010  # case management rates will be used from this DHS year for all calibration burnin years
  dhs_year_cm_calib=2018  # case management rates will be used from this DHS year for all calibration pickup years
  
  # --- parameters for iptp input files --- #
  iptp_variable_name = 'iptp'
  maximum_coverage_iptp = 0.9
  
  # --- parameters for IRS input files --- #
  include_IRS=FALSE
  mean_household_size = 4.8
  irs_who_filename = NA
  irs_first_round_month = 3.5
  irs_second_round_month = 10.5
  max_effective_coverage_irs=0.75
  
  
  # --- parameters for EPI vaccination coverage (used for IPTi) --- #
  vaccine_dhs_year = 2018
  vaccine_variables = c('vacc_dpt1', 'vacc_dpt2', 'vacc_dpt3', 'vacc_measles')
  vaccine_alternate_positive_patterns = c('reported by mother', 'vaccination marked on card')
}

# ============================================================================ #
# ============================================================================ #


# source scripts
source(paste0(script_dir,'/data_processing/1_DHS_data_extraction.R'))
source(paste0(script_dir,'/data_processing/2_sample_coverage_values.R'))
source(paste0(script_dir,'/data_processing/3_sample_itn_mass_distribution_coverages.R'))
source(paste0(script_dir,'/data_processing/4_create_DHS_reference_monthly_pfpr.R'))
source(paste0(script_dir,'/data_processing/setup_inputs/add_past_unobserved_llin_distributions.R'))
source(paste0(script_dir,'/data_processing/setup_inputs/get_routine_itn_coverages.R'))
source(paste0(script_dir,'/data_processing/setup_inputs/add_permethrin_mortality_to_itn_coverage_files.R'))
source(paste0(script_dir,'/data_processing/setup_inputs/add_hut_trial_mort_block.R'))
source(paste0(script_dir,'/data_processing/setup_inputs/create_sim_input_cm_iptp.R'))
source(paste0(script_dir,'/data_processing/setup_inputs/reformat_IPTp_for_postprocessing_inputs.R'))
source(paste0(script_dir,'/data_processing/setup_inputs/get_iptp_doses_from_dhs.R'))
source(paste0(script_dir,'/data_processing/setup_inputs/format_IRS_coverage.R'))
source(paste0(script_dir,'/standardize_admin_names.R'))

# get dataframe with standardized admin names, states, archetype, and population information
ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
archetype_info = read.csv(ds_pop_df_filename)
# standardize shapefile names
admin_shape$NOMDEP = standardize_admin_names_in_vector(target_names=archetype_info$LGA, origin_names=admin_shape$NOMDEP)



#=======================================#
# TODO: check inputs needed for this script are present and in the correct formats
#=======================================#

# make sure the relative vector abundance csv file uses appropriate admin names
if(file.exists(rel_vector_df_name)){
  rel_vector_df = read.csv(rel_vector_df_name)
  if(any(!(rel_vector_df$DS %in% archetype_info$admin_name))){
    rel_vector_df = standardize_admin_names_in_df(target_names_df=archetype_info, origin_names_df=rel_vector_df, target_names_col='admin_name', origin_names_col='DS')
    write.csv(rel_vector_df, rel_vector_df_name)
  }
}

if((!file.exists(itn_distributions_by_admin_filename)) & (country =='NGA')){
  source(paste0(script_dir,'/data_processing/create_NGA_LLIN_mass_dist_dates.R'))
  create_llin_mass_dist_dates(hbhi_dir=hbhi_dir, ds_pop_df_filename=ds_pop_df_filename, llin_mass_date_filename=llin_mass_date_filename, itn_distributions_by_admin_filename=itn_distributions_by_admin_filename, create_plots=create_plots)
}

if((!file.exists(anc_itn_access_filename)) & (country =='NGA')){
  source(paste0(script_dir,'/data_processing/create_NGA_LLIN_ANC_date_coverage.R'))
  modify_llin_anc_date_coverage_from_2019_sim(hbhi_dir=hbhi_dir, ds_pop_df_filename=ds_pop_df_filename, sim_2019_llin_anc_filename=sim_2019_llin_anc_filename, anc_itn_access_filename=anc_itn_access_filename, years_anc_nets=years_anc_nets)
}

if((!file.exists(mortality_df_name)) & (country =='NGA')){
  source(paste0(script_dir,'/example_nga/setup_inputs/create_NGA_bioassay_mortality_input.R'))
  if(!dir.exists(paste0(hbhi_dir, "/simulation_inputs/intermediate_files/insecticide_resistance"))) dir.create(paste0(hbhi_dir, "/simulation_inputs/intermediate_files/insecticide_resistance"))
  create_NGA_bioassay_mortality_input(hbhi_dir_base=hbhi_dir_base, hbhi_dir=hbhi_dir, ds_pop_df_filename=ds_pop_df_filename, mortality_df_name=mortality_df_name)
}


#=======================================#
# sample outputs from DHS surveys
#=======================================#

# extract DHS survey results for each variable and plot maps of the results
extract_DHS_data(hbhi_dir=hbhi_dir, dta_dir=dta_dir, years=years, admin_shape=admin_shape, ds_pop_df_filename=ds_pop_df_filename, min_num_total=min_num_total, variables=variables)
extract_vaccine_DHS_data(hbhi_dir=hbhi_dir, dta_dir=dta_dir, year=vaccine_dhs_year, admin_shape=admin_shape, ds_pop_df_filename=ds_pop_df_filename, min_num_total=min_num_total, vaccine_variables=vaccine_variables, vaccine_alternate_positive_patterns=vaccine_alternate_positive_patterns)
extract_archetype_level_DHS_data(hbhi_dir=hbhi_dir, dta_dir=dta_dir, ds_pop_df_filename=ds_pop_df_filename, years=years)
extract_country_level_DHS_data(hbhi_dir=hbhi_dir, dta_dir=dta_dir, years=years)
#!!! get_relative_itn_use_by_age(hbhi_dir=hbhi_dir, years=years)
plot_extracted_DHS_data(hbhi_dir=hbhi_dir, years=years, admin_shape=admin_shape, min_num_total=min_num_total, variables=variables, colors_range_0_to_1=colors_range_0_to_1, all_years_int_plot_panel=TRUE)
# plot vaccine maps
plot_extracted_DHS_data(hbhi_dir=hbhi_dir, years=vaccine_dhs_year, admin_shape=admin_shape, min_num_total=min_num_total, variables=vaccine_variables, colors_range_0_to_1=colors_range_0_to_1, all_years_int_plot_panel=TRUE, plot_vaccine=TRUE)
# extract the U5 microscopy rates for each admin-month-year combination (this is used as the reference dataset for the transmission intensity calibration)
create_DHS_reference_monthly_pfpr(hbhi_dir=hbhi_dir, dta_dir=dta_dir, admin_shape=admin_shape, ds_pop_df_filename=ds_pop_df_filename, pfpr_dhs_ref_years=pfpr_dhs_ref_years, min_num_total=min_num_total_microscopy)
# sample parameter values from posterior (calculated based on DHS observations), create trajectories of coverage through time, plot results
variables=unique(c(variables, cm_variable_name))
sample_params_from_DHS_posterior(hbhi_dir=hbhi_dir, years=years, variables=variables, num_samples=num_samples, plot_flag=TRUE, sim_start_year=sim_start_year, last_sim_year=last_sim_year)
if(sample_uncertainty){
  # get parameter values from quantiles of the posterior (calculated based on DHS observations), create trajectories of coverage through time, plot results - these sampled values are used for the baseline transmission intensity calibration
  sample_params_from_DHS_posterior(hbhi_dir=hbhi_dir, years=years, variables=variables, num_samples=NA, quantiles=quantiles, sample_quantiles=TRUE, plot_flag=TRUE, sim_start_year=sim_start_year, last_sim_year=last_sim_year)
}

#=======================================#
# setup LLIN inputs
#=======================================#

# sample the net retention distribution parameters
sample_net_longevity_params(hbhi_dir=hbhi_dir, num_samples=num_samples, itn_lognorm_sigma=itn_lognorm_sigma,
                            median_llin_retention_est=median_llin_retention_est,  # years (from Amelia's retention time plot, SLE=1.47(1.31,1.63), BDI=1.31(1.14,1.47), BFA=1.58(1.41,1.76))
                            median_llin_retention_lowerCI=median_llin_retention_lowerCI,  # years (from Amelia's retention time plot, SLE=1.47(1.31,1.63), BDI=1.31(1.14,1.47), BFA=1.58(1.41,1.76))
                            kill_decay_time=kill_decay_time,
                            block_decay_time=block_decay_time)
if(num_samples>0){
  quantile_net_longevity_params(hbhi_dir=hbhi_dir, quantiles=quantiles, itn_lognorm_sigma=itn_lognorm_sigma,
                              median_llin_retention_est=median_llin_retention_est,  # years (from Amelia's retention time plot, SLE=1.47(1.31,1.63), BDI=1.31(1.14,1.47), BFA=1.58(1.41,1.76))
                              median_llin_retention_lowerCI=median_llin_retention_lowerCI,  # years (from Amelia's retention time plot, SLE=1.47(1.31,1.63), BDI=1.31(1.14,1.47), BFA=1.58(1.41,1.76))
                              kill_decay_time=kill_decay_time,
                              block_decay_time=block_decay_time)
}
# which method for generating ITN mass distribution input files
if(use_NPC_mass_ITN){
  # from reported nets distributed per capita for mass LLIN campaigns, create input files
  create_mass_itn_input_from_npc(hbhi_dir=hbhi_dir, ds_pop_df_filename=ds_pop_df_filename, itn_mass_coverage_filepath=itn_mass_coverage_filepath, 
                                 num_samples=num_samples, sim_start_year=sim_start_year, 
                                 season_calib_start_year_burn=season_calib_start_year_burn, season_calib_start_year=season_calib_start_year, exclude_calib_itn_mass_after=exclude_calib_itn_mass_after, 
                                 trans_calib_start_year=trans_calib_start_year,
                                 U5_use_given_access=U5_use_given_access, 
                                 npc_access_param1_vals = npc_access_param1_vals, 
                                 npc_access_param2_vals = npc_access_param2_vals,
                                 age_group_names=age_group_names,
                                 relative_use_by_age=relative_use_by_age)
} else{
  # from observed ITN use at time of surveys, extrapolate use rates back to the time of ITN campaigns and create plots
  create_itn_input_from_DHS_differentDates(hbhi_dir=hbhi_dir, itn_variables=itn_variables, itn_distributions_by_admin_filename=itn_distributions_by_admin_filename, 
                                      sim_start_year=sim_start_year, maximum_coverage=maximum_coverage_itn,
                                      seasonality_monthly_scalar=seasonality_monthly_scalar, years=years, min_num_total=min_num_total, 
                                      default_first_coverage=default_first_coverage, itn_variable_base='itn_u5',  save_timeseries_coverage_plots=TRUE)
  if(length(additional_itn_years)>0){
    add_past_unobserved_llin_distributions(past_itn_mass_coverage_filepath = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_mass_obs/itn_mass_coverages_2010_2020.csv'),
                                         sim_start_year = sim_start_year,
                                         additional_itn_years = additional_itn_years, 
                                         additional_itn_months = additional_itn_months,
                                         copied_itn_coverage_year = copied_itn_coverage_year)
  }
  create_seasonality_calibration_itn_from_DHS(hbhi_dir=hbhi_dir, itn_variables=itn_variables, burn_start_year=season_calib_start_year_burn, calib_start_year=season_calib_start_year, maximum_coverage=maximum_coverage_itn,
                                              itn_distribution_years_calib = itn_distribution_years_calib,
                                              itn_distribution_months_calib = itn_distribution_months_calib,
                                              matching_dhs_years = matching_dhs_years, 
                                              matching_dhs_months = matching_dhs_months, 
                                              dhs_rescale_multipliers=dhs_rescale_multipliers)
  #!!! create_transmission_calibration_itn_from_DHS(hbhi_dir=hbhi_dir, itn_variables=itn_variables, sim_start_year=sim_start_year, maximum_coverage=maximum_coverage_itn,
  #                                              itn_distribution_years=itn_distribution_years, itn_distribution_months=itn_distribution_months,
  #                                              matching_dhs_years=matching_dhs_years, matching_dhs_months=matching_dhs_months,
  #                                              dhs_rescale_multipliers=dhs_rescale_multipliers)
  # plot itn distribution map for main simulations
  plot_itn_campaign_coverage_map(hbhi_dir=hbhi_dir, itn_variables=itn_variables, admin_shape=admin_shape,
                                 colors_range_0_to_1=colors_range_0_to_1, plot_sim_type='main_simulation')
  # # plot itn distributions for calibrations
  # plot_itn_campaign_coverage(hbhi_dir=hbhi_dir, itn_variables=itn_variables, admin_shape=admin_shape, maximum_coverage=maximum_coverage_itn,
  #                            itn_distribution_years=itn_distribution_years_calib, itn_distribution_months=itn_distribution_months_calib,
  #                            matching_dhs_years=matching_dhs_years, matching_dhs_months=matching_dhs_months,
  #                            admin_name_plot=NA, colors_range_0_to_1=colors_range_0_to_1, plot_sim_type='seasonality_calib')
  # plot_itn_campaign_coverage(hbhi_dir=hbhi_dir, itn_variables=itn_variables, admin_shape=admin_shape, maximum_coverage=maximum_coverage_itn,
  #                            itn_distribution_years=itn_distribution_years, itn_distribution_months=itn_distribution_months,
  #                            matching_dhs_years=matching_dhs_years, matching_dhs_months=matching_dhs_months,
  #                            admin_name_plot=NA, colors_range_0_to_1=colors_range_0_to_1, plot_sim_type='transmission_calib')
}

# create ANC ITN input files (for calibration and main simulations) using coverage data from WHO
create_anc_itn_inputs_from_WHO(anc_itn_access_filename=anc_itn_access_filename, hbhi_dir=hbhi_dir, ds_pop_df_filename=ds_pop_df_filename, max_anc_itn_access=max_anc_itn_access, anc_itn_use_given_access=anc_itn_use_given_access, sim_start_year=sim_start_year, calib_anc_itn_year=calib_anc_itn_year, num_samples=num_samples)

if(include_EPI){# create EPI ITN input files (for calibrations and main simulations) using coverage values at specified birthdays (from PMI reports)
  create_epi_itn_inputs(hbhi_dir=hbhi_dir, birthday_ages=birthday_ages, coverage_each_birthday=coverage_each_birthday, epi_itn_use_given_access=epi_itn_use_given_access, 
                                   last_sim_year=last_sim_year, sim_start_year=sim_start_year, season_calib_start_year=season_calib_start_year, trans_calib_start_year=trans_calib_start_year)
}

# add the bioassay permethrin mortality values (from the insecticide resistance values) to the ITN files
add_bioassay_mortality_to_itn_file(hbhi_dir=hbhi_dir, mortality_df_name=mortality_df_name)
# calculate the killing and blocking parameters from the bioassay permetherin mortality (calculated by translating to hut-trial estimates and then calculating dtk params)
add_kill_block_params_to_itn_files(hbhi_dir=hbhi_dir, frac_reassign_feed_survive=frac_reassign_feed_survive, indoor_net_protection=indoor_net_protection)
# create plots showing how bioassay mortality is converted to dtk blocking and killing rates
if(create_itn_kill_block_plots){
  plot_kill_block_function_of_bioassay_mort(hbhi_dir=hbhi_dir, frac_reassign_feed_survive=frac_reassign_feed_survive)
  create_feeding_outcome_plot_for_GR(hbhi_dir=hbhi_dir, frac_reassign_feed_survive=frac_reassign_feed_survive, indoor_net_protection=indoor_net_protection)
}

# copy ITN files into input folders
ifelse(!dir.exists(paste0(hbhi_dir, '/simulation_inputs/interventions_calib')), dir.create(paste0(hbhi_dir, '/simulation_inputs/interventions_calib')), FALSE)
ifelse(!dir.exists(paste0(hbhi_dir, '/simulation_inputs/interventions_2010_toPresent')), dir.create(paste0(hbhi_dir, '/simulation_inputs/interventions_2010_toPresent')), FALSE)
intermediate_folder = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_coverage_block_kill')
calib_files_itn = c('itn_mass_coverages_mort_season_calib_burnin', 'itn_mass_coverages_mort_season_calib_main',  'anc_itn_use_coverages_mort_season_calib','anc_itn_use_coverages_mort_trans_calib') #!!! 'itn_mass_coverages_mort_transmission_calib',
main_sim_files_itn = c('itn_mass_coverages_mort_2010_toPresent', 'anc_itn_use_coverages_mort_2010_toPresent')
if(include_EPI){
  calib_files_itn = c(calib_files_itn, 'epi_itn_use_coverages_mort_season_calib','epi_itn_use_coverages_mort_trans_calib')
  main_sim_files_itn = c(main_sim_files_itn, 'epi_itn_use_coverages_mort_2010_toPresent')
}
for(ff in 1:length(calib_files_itn)){
  write.csv(read.csv(paste0(intermediate_folder, '/', calib_files_itn[ff],'.csv')), paste0(hbhi_dir, '/simulation_inputs/interventions_calib/', calib_files_itn[ff],'.csv'), row.names=FALSE)
}
for(ff in 1:length(main_sim_files_itn)){
  write.csv(read.csv(paste0(intermediate_folder, '/', main_sim_files_itn[ff],'.csv')), paste0(hbhi_dir, '/simulation_inputs/interventions_2010_toPresent/', main_sim_files_itn[ff],'.csv'), row.names=FALSE)
}


#=======================================#
# setup CM, IPTp, IRS inputs
#=======================================#

# covert sampled parameters to simulation input files
create_cm_input_from_DHS(hbhi_dir=hbhi_dir, cm_variable_name=cm_variable_name, sim_start_year=sim_start_year, adult_multiplier=adult_multiplier, severe_multiplier=severe_multiplier, severe_minimum=severe_minimum, maximum_coverage=maximum_coverage_cm)
create_iptp_input_from_DHS(hbhi_dir=hbhi_dir, iptp_variable_name=iptp_variable_name, sim_start_year=sim_start_year, maximum_coverage=maximum_coverage_iptp)
reformat_IPTp_input(hbhi_dir=hbhi_dir, ds_pop_df_filename=ds_pop_df_filename)
# get doses of IPTp taken by individuals who receive IPTp (from DHS, average for entire country)
get_iptp_doses_from_dhs(hbhi_dir=hbhi_dir, dta_dir=dta_dir, years=years, sim_start_year=sim_start_year, last_sim_year=last_sim_year, country=country)
  
# create intervention coverage input csv files for calibration
create_season_calib_cm_input_from_DHS(hbhi_dir=hbhi_dir, cm_variable_name=cm_variable_name, dhs_year_cm_burnin=dhs_year_cm_burnin, dhs_year_cm_calib=dhs_year_cm_calib, adult_multiplier=adult_multiplier, severe_multiplier=severe_multiplier, severe_minimum=severe_minimum, maximum_coverage=maximum_coverage_cm)
#!!! create_trans_calib_cm_input_from_DHS(hbhi_dir=hbhi_dir, cm_variable_name=cm_variable_name, sim_start_year=sim_start_year, adult_multiplier=adult_multiplier, severe_multiplier=severe_multiplier, severe_minimum=severe_minimum, maximum_coverage=maximum_coverage_cm)

if(include_IRS){
  # get IRS inputs from csv provided by WHO
  format_irs_inputs(irs_who_filename=irs_who_filename, ds_pop_df_filename=ds_pop_df_filename,mean_household_size=mean_household_size, irs_first_round_month=irs_first_round_month,irs_second_round_month=irs_second_round_month,  
                    sim_start_year=sim_start_year, burn_start_year=season_calib_start_year_burn, calib_start_year=season_calib_start_year, 
                    max_effective_coverage_irs=max_effective_coverage_irs, num_samples=num_samples, create_plots=FALSE)
}



#=======================================#
# TODO: check inputs needed for simulations are all present and in the correct formats
#=======================================#



