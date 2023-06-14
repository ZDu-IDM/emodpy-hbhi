# merge_outputs_from_mult_experiments
# December 2022
# contact: mambrose
#
# Objective: merge simulation output files from multiple experiments
#   Or overwrite certain admins with an updated version

library(data.table)
library(dplyr)

sim_output_base_filepath = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/snt_2023/simulation_output'

# set source and target filepaths
exp1_filepath = paste0(sim_output_base_filepath, '/transmission_intensity_calibration/PfPR_sweep_main_BDI_v1_originalVersion')
exp2_filepath = paste0(sim_output_base_filepath, '/transmission_intensity_calibration/PfPR_sweep_main_BDI_v1_redoSubset')
merged_filepath = paste0(sim_output_base_filepath, '/transmission_intensity_calibration/PfPR_sweep_main_BDI_v1')

# specify which output files should be included in merge
files_to_merge = c('monthly_U5_PfPR.csv')

merge_files = FALSE
overwrite_files = TRUE

if(merge_files){
  
  # set the column names that identify unique, distinct sets of simulations. These should not be merged
  colnames_for_distinct_simulations = c('month', 'year', 'admin_name', 'Habitat_Multiplier')
  run_number_colname = 'Run_Number'
  
  # check whether new directory already exists. If so, check with user to make sure it's okay to overwrite
  overwrite_existing_dir = FALSE
  if(dir.exists(merged_filepath)){
    warning(paste0('The source directory already exists. Are you sure you want to overwrite it? Set overwrite_existing_dir=TRUE if so. '))
  } else{
    dir.create(merged_filepath)
    
    # iterate through output files, creating and saving merged file 
    for(ff in 1:length(files_to_merge)){
      # read in files
      if(file.exists(paste0(exp1_filepath, '/', files_to_merge[ff])) & file.exists(paste0(exp2_filepath, '/', files_to_merge[ff]))){
        exp1 = fread(paste0(exp1_filepath, '/', files_to_merge[ff]))
        exp2 = fread(paste0(exp2_filepath, '/', files_to_merge[ff]))
        exp1 = as.data.frame(exp1)
        exp2 = as.data.frame(exp2)
        
        # check whether there are any simulations that are present in both experiments and have overlapping run numbers - for these simulations, the run numbers for one set will need to be reassigned
        exp1_identifiers = exp1[c(colnames_for_distinct_simulations, run_number_colname)]
        exp2_identifiers = exp2[,c(colnames_for_distinct_simulations, run_number_colname)]
        exp12_identifiers = rbind(exp1_identifiers, exp2_identifiers)
        exp12_distinct = distinct(exp12_identifiers)
        if(nrow(exp12_identifiers) == nrow(exp12_distinct)){ 
          if(all(sort(colnames(exp1))==sort(colnames(exp2)))){
            # have already checked to make sure there are no duplicates, so can rbind the dataframes
            ordered_colnames = colnames(exp1)
            exp12 = rbind(exp1[,ordered_colnames], exp2[,ordered_colnames])
          } else{
            exp12b = merge(exp1, exp2, all=TRUE)
          }
          if(nrow(exp12) == (nrow(exp1) + nrow(exp2))){
            write.csv(exp12, paste0(merged_filepath, '/', files_to_merge[ff]), row.names=FALSE)
          } else{
            warning(paste0('There has been an issue with merging the simulation output dataframes for ', files_to_merge[ff], '. Please check the code for bugs.'))
          }
        } else{
          warning('Additional runs have been added to existing scenario. Need to add code to handle this (either ignore or update the Run_Number value for one set of simulations)')
        }
      } else warning(paste0('At least one of the specified files to merge does not exist. Files are: ', paste0(exp1_filepath, '/', files_to_merge[ff]), ' and ', paste0(exp2_filepath, '/', files_to_merge[ff])))
    }
  }
  
}else if(overwrite_files){
  # check whether new directory already exists. If so, check with user to make sure it's okay to overwrite
  overwrite_existing_dir = FALSE
  if(dir.exists(merged_filepath)){
    warning(paste0('The source directory already exists. Are you sure you want to overwrite it? Set overwrite_existing_dir=TRUE if so. '))
  } else{
    dir.create(merged_filepath)
    
    # iterate through output files, creating and saving merged file 
    for(ff in 1:length(files_to_merge)){
      # read in files
      if(file.exists(paste0(exp1_filepath, '/', files_to_merge[ff])) & file.exists(paste0(exp2_filepath, '/', files_to_merge[ff]))){
        exp1 = fread(paste0(exp1_filepath, '/', files_to_merge[ff]))
        exp2 = fread(paste0(exp2_filepath, '/', files_to_merge[ff]))
        exp1 = as.data.frame(exp1)
        exp2 = as.data.frame(exp2)
        
        # get the admins from exp2; delete them from exp1; then add the rows from exp2 into exp1
        exp2_admins = unique(exp2$admin_name)
        exp_updated = exp1[-which(exp1$admin_name %in% exp2_admins),]
        exp_updated = merge(exp_updated, exp2, all=TRUE)
        
        if(nrow(exp_updated) == (nrow(exp1))){
          write.csv(exp_updated, paste0(merged_filepath, '/', files_to_merge[ff]), row.names=FALSE)
        } else{
          warning(paste0('There has been an issue with merging the simulation output dataframes for ', files_to_merge[ff], '. Please check the code for bugs.'))
        }
      } else warning(paste0('At least one of the specified files to merge does not exist. Files are: ', paste0(exp1_filepath, '/', files_to_merge[ff]), ' and ', paste0(exp2_filepath, '/', files_to_merge[ff])))
    }
  }
}



