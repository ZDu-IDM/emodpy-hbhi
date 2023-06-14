# combine the results from multiple experiments that use ascending seeds 
#    (i.e., no duplicate Run_Number values. First set of experiments have Run_Number={0,1,...,n} and second set have Run_Number={n+1, n+2,...})

library(data.table)

#################################
# version 1: experiments are stored in different directories under the same subdirectory names
#################################
# set filepaths
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
hbhi_dir = paste0(user_path, '/Dropbox (IDM)/NU_collaboration/hbhi_nigeria/snt_2022')
sim_future_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_future/v3')

# set the directory for one batch of experiments
dir_s0 = paste0(sim_future_output_dir, '/_combine_seeds/s0')
# set the directory for the other batch of experiments
dir_s1 = paste0(sim_future_output_dir, '/_combine_seeds/s1')
# set the directory for where the results of combining the two experiments should go
dir_out = sim_future_output_dir

# set the relevant files to combine
filenames_to_combine = c('malariaBurden_withAdjustments.csv', 'MonthlyUsageLLIN.csv','monthly_Event_Count.csv')

# get name of all experiments that are in both dir_s0 and dir_s1 (to be combined)
s0_exps = list.dirs(path=dir_s0, full.names = FALSE, recursive = FALSE)
s1_exps = list.dirs(path=dir_s1, full.names = FALSE, recursive = FALSE)
combine_exps = intersect(s0_exps, s1_exps)
if(FALSE) combine_exps = s0_exps # for debugging

# iterate through experiments, combining the relevant files and saving in a new output directory
for(ee in 1:length(combine_exps)){
  cur_exp = combine_exps[ee]
  print(paste0('Beginning to combine ', cur_exp))
  # create new directory for combined output
  if(!dir.exists(file.path(dir_out, cur_exp))){
    dir.create(file.path(dir_out, cur_exp))
  }
  for(ff in 1:length(filenames_to_combine)){
    cur_filename = filenames_to_combine[ff]
    if(file.exists(file.path(dir_s0, cur_exp, cur_filename)) & file.exists(file.path(dir_s1, cur_exp, cur_filename))){
      s0_file = fread(file.path(dir_s0, cur_exp, cur_filename))
      s1_file = fread(file.path(dir_s1, cur_exp, cur_filename))
      
      # check that Run_Numbers do not overlap
      s0_runNums = unique(s0_file$Run_Number)
      s1_runNums = unique(s1_file$Run_Number)
      if(!(any(s0_runNums %in% s1_runNums))){
        combined_file = merge(s0_file, s1_file, all=TRUE)
        if(nrow(combined_file) == (nrow(s0_file) + nrow(s1_file))){
          fwrite(combined_file, file=file.path(dir_out, cur_exp, cur_filename))
        } else{
          warning(paste0('PROBLEM: issue detected where combined experiments do not have appropriate number of rows for: ', cur_exp, ' - ', cur_filename))
        }
      } else{
        warning(paste0('PROBLEM: the same Run_Number value appears in both experiments for :', cur_exp))
      }
    } else{
      warning(paste0('PROBLEM: A file does not exist in both of the experiments that should be combined: ', cur_filename))
    }
  }
}






#################################
# version 2: all experiments are stored in the same directories, and they are differentiated with suffixes
#################################
# set filepaths
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
hbhi_dir = paste0(user_path, '/Dropbox (IDM)/NU_collaboration/hbhi_nigeria/snt_2022')
sim_future_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_future/v3')

# set the directory for one batch of experiments
dir_s0 = paste0(sim_future_output_dir, '/_separateSeeds')
# set the suffix that differentiates experiments
suffix = '_seed'
# set the directory for where the results of combining the two experiments should go
dir_out = sim_future_output_dir

# set the relevant files to combine
filenames_to_combine = c('malariaBurden_withAdjustments.csv', 'MonthlyUsageLLIN.csv','monthly_Event_Count.csv')

# get name of all experiments that are in the directory
s0_exps = list.dirs(path=dir_s0, full.names = FALSE, recursive = FALSE)
combine_exps = unique(gsub('_seed[0-9]*', '', s0_exps))

# iterate through experiments, combining the relevant files and saving in a new output directory
for(ee in 1:length(combine_exps)){
  cur_exp = combine_exps[ee]
  relevant_directories = s0_exps[grep(cur_exp,s0_exps)]
  print(paste0('Beginning to combine ', cur_exp))
  # create new directory for combined output - if that directory already exists, send error
  if(!dir.exists(file.path(dir_out, cur_exp))){
    dir.create(file.path(dir_out, cur_exp))
    
    for(ff in 1:length(filenames_to_combine)){
      cur_filename = filenames_to_combine[ff]
      print(paste0('...combining for file ', cur_filename))
      
      # get list of all outputs, checking that the run numbers are all unique
      combined_file = data.frame()
      seeds_so_far = c()
      rows_each = c()
      for(ss in 1:length(relevant_directories)){
        if(file.exists(file.path(dir_s0, relevant_directories[ss], cur_filename))){
          s0_file = fread(file.path(dir_s0, relevant_directories[ss], cur_filename))
          rows_each[ss] = nrow(s0_file)
          # check that Run_Numbers do not overlap
          s0_runNums = unique(s0_file$Run_Number)
          if(!(any(s0_runNums %in% seeds_so_far))){
            if(nrow(combined_file)<1){
              combined_file = s0_file
            } else{
              combined_file = merge(combined_file, s0_file, all=TRUE)
            }
          }else{
            warning(paste0('PROBLEM: the same Run_Number value appears in both experiments for :', cur_exp))
          }
        }else{
          warning(paste0('PROBLEM: A file does not exist in both of the experiments that should be combined: ', cur_filename))
        }
      }
      
      if(nrow(combined_file) == sum(rows_each)){
        fwrite(combined_file, file=file.path(dir_out, cur_exp, cur_filename))
      } else{
        warning(paste0('PROBLEM: issue detected where combined experiments do not have appropriate number of rows for: ', cur_exp, ' - ', cur_filename))
      }
    }
  } else{
    warning(paste0('The output folder already exists in the target destination for experiment ',cur_exp, '. It must be deleted manually before a new one will be created. '))
  }
}



