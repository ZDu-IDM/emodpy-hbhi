# add_BDI_routine_ITN_later_years.R

# From an earlier dataset, we were given coverage information in each DS for ANC and EPI nets distributed for years <=2019. 
#   More recently, we received a dataset of the number of nets distributed through these channels for 2015-2021. 
# We now use the later dataset to calculate coverage through time. 

library(readxl)
library(dplyr)
library(ggplot2)

dta_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/data'
hbhi_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/snt_2023'
script_dir = 'C:/Users/moniqueam/Documents/malaria-snt-core'

source(paste0(script_dir,'/standardize_admin_names.R'))

# more recent data on number of nets distributed through each channel in each year (all year-channel combinations saved as different sheets in excel file)
new_anc_epi_filename = paste0(dta_dir, '/burundi/WHO/snt_2022/CPN_VAR_MIILDA 2015-2021 BURUNDI_formatted.xlsx')
# filepath for newly created ANC and EPI csv files
anc_itn_access_filename = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_ANC_dates_coverages.csv')  # data formatted for later processing
epi_itn_access_filename = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_EPI_dates_coverages.csv')  # data formatted for later processing

# get the names of each sheet. Note that CPN is for ANC and VAR is for EPI. The sheet name also contains the year.
sheet_names = excel_sheets(path=new_anc_epi_filename)
years_included = 2015:2021
# specify which columns are used
colname_admin = 'adm2_ds'
itn_col_substring = 'llin'

epi_attendance_rescale = 0.924  # data are fraction of individuals attending EPI who received net, so need to multiply by fraction attending EPI. This value is national cluster weighted mean for first vaccine as proxy (e.g., DHS_vaccine_admin_min30_2016.csv)
max_epi_net_coverage = 0.97

# # there appear to be some additional / new DS in the csv file that are not in the shapefile and datasets we use currently. These are removed in this analysis.
# #    nevermind - these were newly added DS, which are now included
# unmatched_ds_names = c("Bukinanyana", "Gisuru", "Rutovu")
unmatched_ds_names = c()


# get dataframe with standardized admin names, states, archetype, and population information
ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
archetype_info = read.csv(ds_pop_df_filename)


# create directories if they don't already exist
if(!dir.exists(file.path(hbhi_dir, 'simulation_inputs','intermediate_files'))) dir.create(file.path(hbhi_dir, 'simulation_inputs','intermediate_files'))

# function to extract the coverage in all DS for a given year and routine channel
get_admin_coverage = function(itn_sheet, colname_admin, visit_col_substring, itn_col_substring){

  # get sum of number of visits (across all months and ANC number). Also sum across all health facilities in a DS
  df_visits = itn_sheet[,(grepl(visit_col_substring, colnames(itn_sheet))) | grepl(colname_admin, colnames(itn_sheet))]
  df_visits_sums0 = df_visits %>% 
    group_by_at(all_of(colname_admin)) %>%
    summarise_all(sum, na.rm=TRUE)
  df_visits_sums = data.frame(adm2=df_visits_sums0[[colname_admin]], num_visits=rowSums(df_visits_sums0[,-which(grepl(colname_admin, colnames(df_visits_sums0)))]))
  
  # get sum of number of nets (across all months)
  df_itns = itn_sheet[,(grepl(itn_col_substring, colnames(itn_sheet))) | grepl(colname_admin, colnames(itn_sheet))]
  df_itns_sums0 = df_itns %>% 
    group_by_at(all_of(colname_admin)) %>%
    summarise_all(sum, na.rm=TRUE)
  df_itns_sums = data.frame(adm2=df_itns_sums0[[colname_admin]], num_itns=rowSums(df_itns_sums0[,-which(grepl(colname_admin, colnames(df_itns_sums0)))]))
    # merge dataframes and return fraction of individuals with >=1 visit that received a net
  df_comb = merge(df_visits_sums, df_itns_sums, all=TRUE)
  df_comb$coverage = df_comb$num_itns / df_comb$num_visits

  # remove the 'DS ' from the name and standardize
  df_comb$adm2 = gsub('DS ', '', df_comb$adm2)
  # remove DS that are not part of the set of DS from the WHO or shapefile
  if(any(df_comb$adm2 %in% unmatched_ds_names)) df_comb = df_comb[-which(df_comb$adm2 %in% unmatched_ds_names),]
  # standardize shapefile names
  df_comb$adm2 = standardize_admin_names_in_vector(target_names=archetype_info$admin_name, origin_names=df_comb$adm2)

  return(df_comb[,c('adm2', 'coverage')])
}




# get results for ANC
sheet_type = 'CPN'
visit_col_substring = 'anc1'
anc_coverage = data.frame()
for (yy in years_included){
  itn_sheet_cur = read_excel(path = new_anc_epi_filename, sheet = sheet_names[grepl(sheet_type, sheet_names) & grepl(yy, sheet_names)])
  cur_coverage = get_admin_coverage(itn_sheet=itn_sheet_cur, colname_admin=colname_admin, visit_col_substring=visit_col_substring, itn_col_substring=itn_col_substring)
  cur_coverage$year = yy
  if(nrow(anc_coverage)>0){
    anc_coverage = merge(anc_coverage, cur_coverage, all=TRUE)
  } else{
    anc_coverage = cur_coverage
  }
}
anc_coverage$cov_llins_anc = anc_coverage$coverage  # raw coverage (will set a maximum coverage in later processing script)
ggplot(anc_coverage, aes(x=year, y=coverage, col=adm2))+
  geom_line()
# save net distribution coverage
write.csv(anc_coverage, anc_itn_access_filename, row.names=FALSE)



# get results for EPI
sheet_type = 'VAR'
visit_col_substring = 'epi1'
epi_coverage = data.frame()
for (yy in years_included){
  itn_sheet_cur = read_excel(path = new_anc_epi_filename, sheet = sheet_names[grepl(sheet_type, sheet_names) & grepl(yy, sheet_names)])
  cur_coverage = get_admin_coverage(itn_sheet=itn_sheet_cur, colname_admin=colname_admin, visit_col_substring=visit_col_substring, itn_col_substring=itn_col_substring)
  cur_coverage$year = yy
  if(nrow(epi_coverage)>0){
    epi_coverage = merge(epi_coverage, cur_coverage, all=TRUE)
  } else{
    epi_coverage = cur_coverage
  }
}
# adjust coverage to have maximum rate of individuals at EPI visits receiving ITNs and to account for not everyone having EPI visits
epi_coverage$coverage = sapply(epi_coverage$coverage, min, max_epi_net_coverage)
epi_coverage$coverage = epi_coverage$coverage * epi_attendance_rescale

ggplot(epi_coverage, aes(x=year, y=coverage, col=adm2))+
  geom_line()
# save net distribution coverage
write.csv(epi_coverage, epi_itn_access_filename, row.names=FALSE)
