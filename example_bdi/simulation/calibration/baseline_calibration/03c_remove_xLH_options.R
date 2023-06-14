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
exp_filepath = paste0(sim_output_base_filepath, '/transmission_intensity_calibration/PfPR_sweep_main_BDI_v1')

# specify which output files should be included in merge
files_to_subset = c('monthly_U5_PfPR.csv')
max_xLH = 27  # remove all rows with a xLH larger than this
    
# iterate through output files, creating and saving merged file 
for(ff in 1:length(files_to_subset)){
  # read in files
  if(file.exists(paste0(exp_filepath, '/', files_to_subset[ff]))){
    exp1 = fread(paste0(exp_filepath, '/', files_to_subset[ff]))
    exp1 = as.data.frame(exp1)
    
    exp1_subset = exp1[exp1$Habitat_Multiplier <= max_xLH,]

    write.csv(exp1_subset, paste0(exp_filepath, '/', files_to_subset[ff]), row.names=FALSE)

  } else warning(paste0('At least one of the specified files to merge does not exist. Files are: ', paste0(exp1_filepath, '/', files_to_merge[ff]), ' and ', paste0(exp2_filepath, '/', files_to_merge[ff])))
}




