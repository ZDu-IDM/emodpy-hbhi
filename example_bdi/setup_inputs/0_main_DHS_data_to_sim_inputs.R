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
country = 'BDI'  #'SLE'  # 'BDI'
dta_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/data'
script_dir = 'C:/Users/moniqueam/Documents/malaria-snt-core'
variables = c('mic','itn_all','itn_u5','itn_5_10','itn_10_15','itn_15_20','itn_o20','iptp','cm','blood_test')
itn_variables = c('itn_u5','itn_5_10','itn_10_15','itn_15_20','itn_o20')
create_plots = FALSE
num_samples = 50
min_num_total = 30  # minimum number of individuals surveyed in a district before the region value is used
min_num_total_microscopy = 60  # minimum number of individuals surveyed in a district before the region value is used
colors_range_0_to_1 = rev(add.alpha(pals::parula(101), alpha=0.5))  # specify colorscale. To go from a value vv to a color, take colors_range_0_to_1[1+round(vv*100)]
use_NPC_mass_ITN = TRUE
sample_uncertainty = TRUE

if(country =='BDI'){
  hbhi_dir_base = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
  hbhi_dir = paste0(hbhi_dir_base, '/snt_2023')
  years = c(2010, 2012, 2016)
  pfpr_dhs_ref_years=c(2012, 2016)
  # read in shapefile with admin boundaries
  admin_shape = shapefile(paste0(hbhi_dir, '/SpatialClustering/reference_rasters_shapefiles/bdi_adm2.shp'))
  admin_shape$NAME_1 = admin_shape$NOMREGION  # NOMDEP should be admin level where modeling occurs (e.g., DS, LGA), NOMREGION should be one admin above that, NAME_1 should be one above that
  admin_shape$NOMREGION = admin_shape$NOMREGION
  admin_shape$NOMDEP = admin_shape$NOMDEP
  sim_start_year = 2010
  last_sim_year=2021
  season_calib_start_year_burn=1964
  season_calib_start_year=2014
  trans_calib_start_year=2010
  quantiles = c(0.1, 0.3, 0.5, 0.7, 0.9)
  filename_new_admins = file.path(hbhi_dir, 'new_admins_added_2023.csv')  # filename for csv describing split/new admins
  

  # --- parameters for general itn-related values --- #
  mortality_df_name = paste0(hbhi_dir,'/ento/insecticide_resistance_DS_means/insecticide_Permethrin_mortality_DS_means.csv')
  median_llin_retention_est = 1.31  # years (from Amelia's retention time plot, SLE=1.47(1.31,1.63), BDI=1.31(1.14,1.47), BFA=1.58(1.41,1.76))
  median_llin_retention_lowerCI = 1.14  # years (from Amelia's retention time plot, SLE=1.47(1.31,1.63), BDI=1.31(1.14,1.47), BFA=1.58(1.41,1.76))
  itn_lognorm_sigma = 0.8  # parameter for the lognormal distribution for itn retention function
  frac_reassign_feed_survive = 0.9  # fraction of mosquitos that should feed and die that are assigned to feed and survive in the dtk
  indoor_net_protection = 0.75  # fraction of indoor bites occurring during net-protected hours
  U5_use_given_access=1
  create_itn_kill_block_plots=TRUE
  kill_decay_time=1460
  block_decay_time=730
  rel_vector_df_name = paste0(hbhi_dir,'/ento/DS_vector_rel_abundance.csv')
  
  
  # --- parameters for mass-itn distribution extrapolation --- #
  if(use_NPC_mass_ITN){
    itn_mass_coverage_filepath = paste0(dta_dir, '/burundi/WHO/Interventions/LLIN_mass_distribution_records_v3.csv')
    npc_access_param1_vals = c(0.98,0.85, 0.92,0.98,0.99)  # for function converting nets per capita to access: first value is best estimate, then two lower and two upper bounds from lowest to highest
    npc_access_param2_vals = c(3, 1.8, 2.2, 5, 9)  # for function converting nets per capita to access: first value is best estimate, then two lower and two upper bounds from lowest to highest
    # get country-average relative ITN use rates for different age groups
    relative_use_rates = read.csv(paste0(hbhi_dir,'/estimates_from_DHS/DHS_weighted_ITN_use_by_age_relative_to_u5.csv'))
    relative_use_rates = relative_use_rates[1,grepl('use_relative_to', colnames(relative_use_rates))]
    age_group_names=gsub('X','',colnames(relative_use_rates))
    age_group_names=gsub('_use_relative_to_u5','',age_group_names)
    relative_use_by_age = as.numeric(relative_use_rates)
    exclude_calib_itn_mass_after = 2016
  } else{
    itn_distribution_years = c(2010, 2011, 2014)  # years of itn distributions, sorted from earliest to latest  # since the 2010-2020 simulation starts at the first day of 2010, consider the 2009 distribution to occur on the first day of 2010 to get those nets in the simulation
    itn_distribution_months = c(1, 7 ,7)
    itn_distribution_years_calib = c(2009, 2011, 2014)  # if an itn distribution starts before the first day of the main sim, it will be included in the burnin part of calibration on its original day (instead of pretending distribution occurs on first day of sim)
    itn_distribution_months_calib = c(7, 7 ,7)
    matching_dhs_years = c(2010,2012,2016)  # DHS survey year following mass ITN distributions (must match order of itn_distribution_years)
    matching_dhs_months = c(10, 12, 10)  # chose the median month across all dhs entries, not specific for each admin
    seasonality_monthly_scalar = read.csv(paste0(hbhi_dir, '/simulation_inputs/ITN_use_seasonality.csv'))$itn_use_scalar  # seasonality scalar for net use: gives rate of net use in each month relative to month with maximum use rate
    dhs_rescale_multipliers = 1/seasonality_monthly_scalar[matching_dhs_months]  # need to divide by use in the survey month to get seasonality-controlled use rates
    maximum_coverage_itn = 0.9
    admin_name_plot='Cankuzo'  # admin_shape$NOMDEP[1]
    
    # --- parameters for mass-itn distribution additional past years --- #
    additional_itn_years = c(2017, 2020) # we know there were mass distribution during these years, but we have no information on coverage obtained
    additional_itn_months = c(10, 7)
    copied_itn_coverage_year = 2014  # assume coverage for unobserved mass LLIN distribution years is the same as coverage for this LLIN distribution
  }


  
  # --- parameters for routine itn input files --- #
  # anc_itn_access_filename = paste0(dta_dir, '/burundi/WHO/Interventions/LLINS_ANC_Coverage.csv')  # data from WHO
  anc_itn_access_filename = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_ANC_dates_coverages.csv')  # data formatted for later processing
  epi_itn_access_filename = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_EPI_dates_coverages.csv')  # data formatted for later processing
  max_anc_itn_access = 0.97
  anc_itn_use_given_access = 0.9  # multiply access by this value to get the use rate in a night during the month with the highest use rates (if there is seasonal use)
  calib_anc_itn_year=2016
  calib_epi_itn_year=2016
  max_epi_itn_access = 0.97
  epi_itn_use_given_access = 0.9  # multiply access by this value to get the use rate in a night during the month with the highest use rates (if there is seasonal use)
  birthday_ages=c(1,3)  # for EPI ITN distributions
  # coverage_each_birthday=c(0.915, 0.732)  # for EPI ITN distributions
  coverage_age_rel_age1 = c(1, 0.8)  # nets are distributed to children aged birthday_ages[1], but since nets cover multiple children, we assume a second child
                                #    may also benefit. To represent in the simulations, we assume a child of age birthday_ages[2] 'effectively' receives a net
                                #    coverage_age_rel_age1[2] percent of the time the target age does. (Currently only set up for one additional age, but structure can support more)
  
  
  # --- parameters for cm input files --- #
  cm_variable_name = 'blood_test'  # originally, used 'effective_treatment' but there are concerns with the trajectory (from high rates of ACT to low in 2016) that the respondants are not distinguishing between amodiaquine monotherapy and ACT (citation: the DHS 2016 report)
  adult_multiplier = 0.5  # multiply U5 coverage by this value to get adult coverage
  severe_multiplier = 2  # multiply U5 coverage by this value to get severe coverage
  severe_minimum = 0.6  # minimum severe coverage
  maximum_coverage_cm = 0.9  #  maximum coverage for any age/severity
  dhs_year_cm_burnin=2010  # case management rates will be used from this DHS year for all calibration burnin years
  dhs_year_cm_calib=2016  # case management rates will be used from this DHS year for all calibration pickup years
  
  # --- parameters for iptp input files --- #
  rescale_iptp_dose_filename = paste0(dta_dir, '/burundi/WHO/snt_2022/Données TPIg3 et plus 2017-2021.xlsx')
  iptp_variable_name = 'iptp'
  maximum_coverage_iptp = 0.9
  
  # --- parameters for IRS input files --- #
  mean_household_size = 4.7
  irs_who_filename = paste0(dta_dir, '/Burundi/WHO/snt_2022/IRS_2016-2020_plus2023_v3.csv')
  irs_first_round_month = 10
  irs_second_round_month = 4  # if there are two rounds, assume the second one is to protect the 'dry' season (technically happens first)
  max_effective_coverage_irs=0.8
  effective_irs_coverage_multiplier = 1-0.22  # reduces effective individual coverage to account for dwelling modification/washing, migration, incomplete spraying, etc.
  # create IRS input file with different multipliers for different seeds
  irs_kill_seed_multipliers = c(0.3, 0.15, 0.45)  # c(0.3, 0.2, 0.4, 0.1, 0.5)
  
  # --- parameters for EPI vaccination coverage (used for IPTi) --- #
  vaccine_dhs_year = 2016
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
source(paste0(script_dir,'/data_processing/setup_inputs/rescale_IPTp_based_on_dose_trajectory.R'))
source(paste0(script_dir,'/data_processing/add_admin_subdivisions.R'))
source(paste0(script_dir,'/standardize_admin_names.R'))

# get dataframe with standardized admin names, states, archetype, and population information
ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
archetype_info = read.csv(ds_pop_df_filename)
# standardize shapefile names
admin_shape$NOMDEP = standardize_admin_names_in_vector(target_names=archetype_info$admin_name, origin_names=admin_shape$NOMDEP)


#=======================================#
# TODO: check inputs needed for this script are present and in the correct formats
#=======================================#




#=======================================#
# sample outputs from DHS surveys
#=======================================#

# extract DHS survey results for each variable and plot maps of the results
extract_DHS_data(hbhi_dir=hbhi_dir, dta_dir=dta_dir, years=years, admin_shape=admin_shape, ds_pop_df_filename=ds_pop_df_filename, min_num_total=min_num_total, variables=variables)
extract_vaccine_DHS_data(hbhi_dir=hbhi_dir, dta_dir=dta_dir, year=vaccine_dhs_year, admin_shape=admin_shape, ds_pop_df_filename=ds_pop_df_filename, min_num_total=min_num_total, vaccine_variables=vaccine_variables, vaccine_alternate_positive_patterns=vaccine_alternate_positive_patterns)
extract_archetype_level_DHS_data(hbhi_dir=hbhi_dir, dta_dir=dta_dir, ds_pop_df_filename=ds_pop_df_filename, years=years)
extract_country_level_DHS_data(hbhi_dir=hbhi_dir, dta_dir=dta_dir, years=years)
get_relative_itn_use_by_age(hbhi_dir=hbhi_dir, years=years)
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
  intervention_file_admin_name='admin_name'
  file_names = c(paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_coverage/itn_mass_coverages_2010_toPresent.csv'),
    # paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_coverage/itn_mass_coverages_season_calib_burnin.csv'),
    # paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_coverage/itn_mass_coverages_season_calib_main.csv'),
    paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_coverage/itn_mass_coverages_transmission_calib.csv'))
  for(ii in 1:length(file_names)){
    add_rows_for_new_admins(filename_new_admins=filename_new_admins, intervention_filename=file_names[ii], intervention_file_admin_name=intervention_file_admin_name)
  }
  
} else{
  # from observed ITN use at time of surveys, extrapolate use rates back to the time of ITN campaigns and create plots
  create_itn_input_from_DHS(hbhi_dir=hbhi_dir, itn_variables=itn_variables, num_samples=num_samples, sim_start_year=sim_start_year, maximum_coverage=maximum_coverage_itn,
                            itn_distribution_years=itn_distribution_years, itn_distribution_months=itn_distribution_months,
                            matching_dhs_years=matching_dhs_years, matching_dhs_months=matching_dhs_months,
                            dhs_rescale_multipliers=dhs_rescale_multipliers)
  add_past_unobserved_llin_distributions(past_itn_mass_coverage_filepath = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_mass_obs/itn_mass_coverages_2010_2020.csv'),
                                         sim_start_year = sim_start_year,
                                         additional_itn_years = additional_itn_years, 
                                         additional_itn_months = additional_itn_months,
                                         copied_itn_coverage_year = copied_itn_coverage_year)
  create_seasonality_calibration_itn_from_DHS(hbhi_dir=hbhi_dir, itn_variables=itn_variables, burn_start_year=season_calib_start_year_burn, calib_start_year=season_calib_start_year, maximum_coverage=maximum_coverage_itn,
                                              itn_distribution_years_calib = itn_distribution_years_calib,  # since the 2010-2020 simulation starts at the first day of 2010, consider the 2009 distribution to occur on the first day of 2010 to get those nets in the simulation
                                              itn_distribution_months_calib = itn_distribution_months_calib,
                                              matching_dhs_years = matching_dhs_years,  # DHS survey year following mass ITN distributions (must match order of itn_distribution_years)
                                              matching_dhs_months = matching_dhs_months,  # chose the median month across all dhs entries, not specific for each admin
                                              dhs_rescale_multipliers=dhs_rescale_multipliers)
  create_transmission_calibration_itn_from_DHS(hbhi_dir=hbhi_dir, itn_variables=itn_variables, sim_start_year=sim_start_year, maximum_coverage=maximum_coverage_itn,
                                               itn_distribution_years=itn_distribution_years, itn_distribution_months=itn_distribution_months,
                                               matching_dhs_years=matching_dhs_years, matching_dhs_months=matching_dhs_months,
                                               dhs_rescale_multipliers=dhs_rescale_multipliers)
  # plot itn distributions for main simulations
  plot_itn_campaign_coverage(hbhi_dir=hbhi_dir, itn_variables=itn_variables, admin_shape=admin_shape, maximum_coverage=maximum_coverage_itn,
                             itn_distribution_years=itn_distribution_years, itn_distribution_months=itn_distribution_months,
                             matching_dhs_years=matching_dhs_years, matching_dhs_months=matching_dhs_months,
                             admin_name_plot=admin_name_plot, colors_range_0_to_1=colors_range_0_to_1, plot_sim_type='main_simulation')
  # plot itn distributions for calibrations
  plot_itn_campaign_coverage(hbhi_dir=hbhi_dir, itn_variables=itn_variables, admin_shape=admin_shape, maximum_coverage=maximum_coverage_itn,
                             itn_distribution_years=itn_distribution_years_calib, itn_distribution_months=itn_distribution_months_calib,
                             matching_dhs_years=matching_dhs_years, matching_dhs_months=matching_dhs_months,
                             admin_name_plot=NA, colors_range_0_to_1=colors_range_0_to_1, plot_sim_type='seasonality_calib')
  plot_itn_campaign_coverage(hbhi_dir=hbhi_dir, itn_variables=itn_variables, admin_shape=admin_shape, maximum_coverage=maximum_coverage_itn,
                             itn_distribution_years=itn_distribution_years, itn_distribution_months=itn_distribution_months,
                             matching_dhs_years=matching_dhs_years, matching_dhs_months=matching_dhs_months,
                             admin_name_plot=NA, colors_range_0_to_1=colors_range_0_to_1, plot_sim_type='transmission_calib')
}

# create ANC ITN input files (for calibration and main simulations) using coverage data from WHO
  # intervention_file_admin_name='adm2'
  # add_rows_for_new_admins(filename_new_admins=filename_new_admins, intervention_filename=anc_itn_access_filename, intervention_file_admin_name=intervention_file_admin_name)
create_anc_itn_inputs_from_WHO(anc_itn_access_filename=anc_itn_access_filename, hbhi_dir=hbhi_dir, ds_pop_df_filename=ds_pop_df_filename, max_anc_itn_access=max_anc_itn_access, anc_itn_use_given_access=anc_itn_use_given_access, sim_start_year=sim_start_year, calib_anc_itn_year=calib_anc_itn_year, num_samples=num_samples)

# create EPI ITN input files (for calibrations and main simulations) using coverage values and specified birthdays for distributions
create_epi_itn_inputs_from_WHO(epi_itn_access_filename=epi_itn_access_filename, hbhi_dir=hbhi_dir, ds_pop_df_filename=ds_pop_df_filename, birthday_ages=birthday_ages, coverage_age_rel_age1=coverage_age_rel_age1, max_epi_itn_access=max_epi_itn_access, epi_itn_use_given_access=epi_itn_use_given_access, 
                                 last_sim_year=last_sim_year, sim_start_year=sim_start_year, calib_epi_itn_year=calib_epi_itn_year, num_samples=num_samples)
  
# add the bioassay permethrin mortality values (from the insecticide resistance values) to the ITN files
add_bioassay_mortality_to_itn_file(hbhi_dir=hbhi_dir, mortality_df_name=mortality_df_name)
# calculate the killing and blocking parameters from the bioassay permetherin mortality (calculated by translating to hut-trial estimates and then calculating dtk params)
add_kill_block_params_to_itn_files(hbhi_dir=hbhi_dir, frac_reassign_feed_survive=frac_reassign_feed_survive, indoor_net_protection=indoor_net_protection, create_itn_kill_block_plots=create_itn_kill_block_plots)
# create plots showing how bioassay mortality is converted to dtk blocking and killing rates
if(create_itn_kill_block_plots){
  plot_kill_block_function_of_bioassay_mort(hbhi_dir=hbhi_dir, frac_reassign_feed_survive=frac_reassign_feed_survive)
  create_feeding_outcome_plot_for_GR(hbhi_dir=hbhi_dir, frac_reassign_feed_survive=frac_reassign_feed_survive, indoor_net_protection=indoor_net_protection)
}

# copy ITN files into input folders
ifelse(!dir.exists(paste0(hbhi_dir, '/simulation_inputs/interventions_calib')), dir.create(paste0(hbhi_dir, '/simulation_inputs/interventions_calib')), FALSE)
ifelse(!dir.exists(paste0(hbhi_dir, '/simulation_inputs/interventions_2010_toPresent')), dir.create(paste0(hbhi_dir, '/simulation_inputs/interventions_2010_toPresent')), FALSE)
intermediate_folder = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_coverage_block_kill')
calib_files_itn = c('itn_mass_coverages_mort_season_calib_burnin', 'itn_mass_coverages_mort_season_calib_main', 'itn_mass_coverages_mort_transmission_calib', 'anc_itn_use_coverages_mort_season_calib','anc_itn_use_coverages_mort_trans_calib', 'epi_itn_use_coverages_mort_season_calib','epi_itn_use_coverages_mort_trans_calib')
main_sim_files_itn = c('itn_mass_coverages_mort_2010_toPresent', 'anc_itn_use_coverages_mort_2010_toPresent', 'epi_itn_use_coverages_mort_2010_toPresent')
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
# add more recent IPTp years based on data from PNILP
rescale_IPTp_later_years(hbhi_dir=hbhi_dir, script_dir=script_dir, ds_pop_df_filename=ds_pop_df_filename, rescale_iptp_dose_filename=rescale_iptp_dose_filename, maximum_coverage_iptp=maximum_coverage_iptp)
reformat_IPTp_input(hbhi_dir=hbhi_dir, ds_pop_df_filename=ds_pop_df_filename)
# get doses of IPTp taken by individuals who receive IPTp (from DHS, average for entire country)
get_iptp_doses_from_dhs(hbhi_dir=hbhi_dir, dta_dir=dta_dir, years=years, sim_start_year=sim_start_year, last_sim_year=last_sim_year)
  
# create intervention coverage input csv files for calibration
create_season_calib_cm_input_from_DHS(hbhi_dir=hbhi_dir, cm_variable_name=cm_variable_name, dhs_year_cm_burnin=dhs_year_cm_burnin, dhs_year_cm_calib=dhs_year_cm_calib, adult_multiplier=adult_multiplier, severe_multiplier=severe_multiplier, severe_minimum=severe_minimum, maximum_coverage=maximum_coverage_cm)
create_trans_calib_cm_input_from_DHS(hbhi_dir=hbhi_dir, cm_variable_name=cm_variable_name, sim_start_year=sim_start_year, adult_multiplier=adult_multiplier, severe_multiplier=severe_multiplier, severe_minimum=severe_minimum, maximum_coverage=maximum_coverage_cm)

# get IRS inputs from csv provided by WHO
format_irs_inputs(irs_who_filename=irs_who_filename, ds_pop_df_filename=ds_pop_df_filename,mean_household_size=mean_household_size, irs_first_round_month=irs_first_round_month,irs_second_round_month=irs_second_round_month,  
                  sim_start_year=sim_start_year, burn_start_year=season_calib_start_year_burn, calib_start_year=season_calib_start_year, 
                  effective_irs_coverage_multiplier=effective_irs_coverage_multiplier, max_effective_coverage_irs=max_effective_coverage_irs, num_samples=num_samples, irs_kill_seed_multipliers=irs_kill_seed_multipliers, create_plots=FALSE)



