# DHS/MIS output for Sierra Leone - save results in SierraLeone_hbhi/explore_DHS folder DHS_*year*_files_recodes_for_sims (see existing format) - this will be read in in DHS_data_extraction2.R

library(foreign)
library(haven)
library(dplyr)
library(rgdal)
library(raster)
library(sp)

country = 'BDI'  #'SLE'  # 'BDI'
cur_year = 2016

if(country =='BDI'){
  
  dta_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/data/Burundi/DHS'
  # read in shapefile with admin boundaries
  admin_shape = shapefile('C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/shapefiles_rasterfiles/bdi_adm_igebu_ocha/bdi_admbnda_adm2_igebu_ocha_20171103.shp')
  
  

  
  
  if(cur_year == 2010){
    dta_1 = read.dta(paste0(dta_dir, '/BU_2010_DHS/BUBR61DT/BUBR61FL.DTA'))
    dta_2 = read.dta(paste0(dta_dir, '/BU_2010_DHS/BUCR61DT/BUCR61FL.DTA'))
    dta_3 = read.dta(paste0(dta_dir, '/BU_2010_DHS/BUHR61DT/BUHR61FL.DTA'))
    dta_4 = read.dta(paste0(dta_dir, '/BU_2010_DHS/BUIR61DT/BUIR61FL.DTA'))
    dta_5 = read.dta(paste0(dta_dir, '/BU_2010_DHS/BUKR61DT/BUKR61FL.DTA'))
    dta_6 = read.dta(paste0(dta_dir, '/BU_2010_DHS/BUMR61DT/BUMR61FL.DTA'))
    dta_7 = read.dta(paste0(dta_dir, '/BU_2010_DHS/BUPR61DT/BUPR61FL.DTA'))
    dta_list = list(dta_1, dta_2, dta_3, dta_4, dta_5, dta_6, dta_7)
    locations_shp = shapefile(paste0(dta_dir, '/BU_2010_DHS/BUGE61FL/BUGE61FL.shp'))
    locations = data.frame(clusterid = locations_shp$DHSCLUST, latitude=locations_shp$LATNUM, longitude=locations_shp$LONGNUM)
  } else if(cur_year == 2012){
    dta_1 = read.dta(paste0(dta_dir, '/BU_2012_MIS/BUHR6ADT/BUHR6AFL.DTA'))
    dta_2 = read.dta(paste0(dta_dir, '/BU_2012_MIS/BUIR6ADT/BUIR6AFL.DTA'))
    dta_3 = read.dta(paste0(dta_dir, '/BU_2012_MIS/BUKR6ADT/BUKR6AFL.DTA'))
    dta_4 = read.dta(paste0(dta_dir, '/BU_2012_MIS/BUPR6ADT/BUPR6AFL.DTA'))
    dta_list = list(dta_1, dta_2, dta_3, dta_4)
    locations_shp = shapefile(paste0(dta_dir, '/BU_2012_MIS/BUGE6AFL/BUGE6AFL.shp'))
    locations = data.frame(clusterid = locations_shp$DHSCLUST, latitude=locations_shp$LATNUM, longitude=locations_shp$LONGNUM)
  } else if(cur_year == 2016){
    dta_1 = read.dta(paste0(dta_dir, '/BU_201617_DHS/BUBR71DT/BUBR71FL.DTA'))  # v006=month, v007=year
    dta_2 = read.dta(paste0(dta_dir, '/BU_201617_DHS/BUCR71DT/BUCR71FL.DTA'))
    dta_3 = read.dta(paste0(dta_dir, '/BU_201617_DHS/BUFW71DT/BUFW71FL.DTA'))
    dta_4 = read.dta(paste0(dta_dir, '/BU_201617_DHS/BUHR71DT/BUHR71FL.DTA'))
    dta_5 = read.dta(paste0(dta_dir, '/BU_201617_DHS/BUIR71DT/BUIR71FL.DTA'))
    dta_6 = read.dta(paste0(dta_dir, '/BU_201617_DHS/BUKR71DT/BUKR71FL.DTA'))
    dta_7 = read.dta(paste0(dta_dir, '/BU_201617_DHS/BUMR71DT/BUMR71FL.DTA'))
    dta_8 = read.dta(paste0(dta_dir, '/BU_201617_DHS/BUPR71DT/BUPR71FL.DTA'))
    dta_list = list(dta_1, dta_2, dta_3, dta_4, dta_5, dta_6, dta_7, dta_8)
    locations_shp = shapefile(paste0(dta_dir, '/BU_201617_DHS/BUGE71FL/BUGE71FL.shp'))
    locations = data.frame(clusterid = locations_shp$DHSCLUST, latitude=locations_shp$LATNUM, longitude=locations_shp$LONGNUM)
  } 
  

} else if(country == 'SLE'){
  
  dta_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/data/SierraLeone/DHS'
  # read in shapefile with chiefdom boundaries
  admin_shape = shapefile('C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/SierraLeone_hbhi/shapefiles_rasterfiles/gadm36_SLE_shp/gadm36_SLE_3.shp')
  
  if(cur_year == 2008){
    dta_1 = read.dta(paste0(dta_dir, '/SL_2008_DHS/SLBR51DT/SLBR51FL.DTA'))
    dta_2 = read.dta(paste0(dta_dir, '/SL_2008_DHS/SLCR51DT/SLCR51FL.DTA'))
    dta_3 = read.dta(paste0(dta_dir, '/SL_2008_DHS/SLHR51DT/SLHR51FL.DTA'))
    dta_4 = read.dta(paste0(dta_dir, '/SL_2008_DHS/SLIR51DT/SLIR51FL.DTA'))
    dta_5 = read.dta(paste0(dta_dir, '/SL_2008_DHS/SLKR51DT/SLKR51FL.DTA'))
    dta_6 = read.dta(paste0(dta_dir, '/SL_2008_DHS/SLMR51DT/SLMR51FL.DTA'))
    dta_7 = read.dta(paste0(dta_dir, '/SL_2008_DHS/SLPR51DT/SLPR51FL.DTA'))
    dta_list = list(dta_1, dta_2, dta_3, dta_4, dta_5, dta_6, dta_7)
    locations_shp = shapefile(paste0(dta_dir, '/SL_2008_DHS/SLGE53FL/SLGE53FL.shp'))
    locations = data.frame(clusterid = locations_shp$DHSCLUST, latitude=locations_shp$LATNUM, longitude=locations_shp$LONGNUM)
  } else if(cur_year == 2013){
    dta_1 = read.dta(paste0(dta_dir, '/SL_2013_DHS/SLBR61DT/SLBR61FL.DTA'))
    dta_2 = read.dta(paste0(dta_dir, '/SL_2013_DHS/SLCR61DT/SLCR61FL.DTA'))
    dta_3 = read.dta(paste0(dta_dir, '/SL_2013_DHS/SLHR61DT/SLHR61FL.DTA'))
    dta_4 = read.dta(paste0(dta_dir, '/SL_2013_DHS/SLIR61DT/SLIR61FL.DTA'))
    dta_5 = read.dta(paste0(dta_dir, '/SL_2013_DHS/SLKR61DT/SLKR61FL.DTA'))
    dta_6 = read.dta(paste0(dta_dir, '/SL_2013_DHS/SLMR61DT/SLMR61FL.DTA'))
    dta_7 = read.dta(paste0(dta_dir, '/SL_2013_DHS/SLPR61DT/SLPR61FL.DTA'))
    dta_list = list(dta_1, dta_2, dta_3, dta_4, dta_5, dta_6, dta_7)
    locations_shp = shapefile(paste0(dta_dir, '/SL_2013_DHS/SLGE61FL/SLGE61FL.shp'))
    locations = data.frame(clusterid = locations_shp$DHSCLUST, latitude=locations_shp$LATNUM, longitude=locations_shp$LONGNUM)
    
  } else if(cur_year == 2016){
    # 2016 MIS
    dta_1 = read.dta(paste0(dta_dir, '/MIS2016/SLPR71DT/SLPR71FL.DTA'))
    dta_2 = read.dta(paste0(dta_dir, '/MIS2016/SLKR71DT/SLKR71FL.DTA'))
    dta_3 = read.dta(paste0(dta_dir, '/MIS2016/SLIR71DT/SLIR71FL.DTA'))
    dta_4 = read.dta(paste0(dta_dir, '/MIS2016/SLHR71DT/SLHR71FL.DTA'))
    dta_5 = read.dta(paste0(dta_dir, '/MIS2016/SLFW71DT/SLFW71FL.DTA'))
    dta_list = list(dta_1, dta_2, dta_3, dta_4, dta_5)
    #   # data extracted by Kate/MAP
    #   locations = read.csv('C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/data/SierraLeone/DHS/MIS2016/MIS_2016_FST_byCluster.csv')
    locations_shp = shapefile(paste0(dta_dir, '/MIS2016/SLGE71FL/SLGE71FL.shp'))
    locations = data.frame(clusterid = locations_shp$DHSCLUST, latitude=locations_shp$LATNUM, longitude=locations_shp$LONGNUM)
  } else if(cur_year == 2019){
    dta_1 = read.dta(paste0(dta_dir, '/SL_2019_DHS/SLBR7ADT/SLBR7AFL.DTA'))
    dta_2 = read.dta(paste0(dta_dir, '/SL_2019_DHS/SLCR7ADT/SLCR7AFL.DTA'))
    dta_3 = read.dta(paste0(dta_dir, '/SL_2019_DHS/SLFW7ADT/SLFW7AFL.DTA'))
    dta_4 = read.dta(paste0(dta_dir, '/SL_2019_DHS/SLHR7ADT/SLHR7AFL.DTA'))
    dta_5 = read.dta(paste0(dta_dir, '/SL_2019_DHS/SLIR7ADT/SLIR7AFL.DTA'))
    dta_6 = read.dta(paste0(dta_dir, '/SL_2019_DHS/SLKR7ADT/SLKR7AFL.DTA'))
    dta_7 = read.dta(paste0(dta_dir, '/SL_2019_DHS/SLMR7ADT/SLMR7AFL.DTA'))
    dta_8 = read.dta(paste0(dta_dir, '/SL_2019_DHS/SLPR7ADT/SLPR7AFL.DTA'))
    dta_list = list(dta_1, dta_2, dta_3, dta_4, dta_5, dta_6, dta_7, dta_8)
    locations_shp = shapefile(paste0(dta_dir, '/SL_2019_DHS/SLGE7AFL/SLGE7AFL.shp'))
    locations = data.frame(clusterid = locations_shp$DHSCLUST, latitude=locations_shp$LATNUM, longitude=locations_shp$LONGNUM)
  }

}
# plot(admin_shape)

# # # look at non-na entries of first row
# dta_1[1,which(!is.na(dta_1[1,]))]
# dta_2[1,which(!is.na(dta_2[1,]))]
# dta_3[1,which(!is.na(dta_3[1,]))]
# dta_4[1,which(!is.na(dta_4[1,]))]
# dta_5[1,which(!is.na(dta_5[1,]))]
# dta_6[1,which(!is.na(dta_6[1,]))]
# dta_7[1,which(!is.na(dta_7[1,]))]
# dta_8[1,which(!is.na(dta_8[1,]))]




# function to check whether a particular code is included in each of the datasets and print out
find_code_locations = function(dta_list, code_str){
  
  # determine which columns with the code_str have non-NA values in each dta
  store_candidate_cols = list()
  for(ii in 1:length(dta_list)){
    dta_cur = dta_list[[ii]]
    cur_cols = grep(colnames(dta_cur), pattern = code_str)
    if(length(cur_cols)>0){
      # find columns that contain something other than NA
      can_cols = c()
      for(cc in 1:length(cur_cols)){
        if(any(!is.na(unique(dta_cur[,cur_cols[cc]])))) can_cols = c(can_cols, cur_cols[cc])
      }
      store_candidate_cols[[ii]] = can_cols
    }else{
      store_candidate_cols[[ii]] = NA
    }
  }
  if(length(store_candidate_cols)<length(dta_list)) store_candidate_cols[[length(dta_list)]] = NA
  # # print out head of relevant parts of data frame
  # for(ii in 1:length(dta_list)){
  #   if(length(store_candidate_cols[[ii]])>0){
  #     if(!is.na(store_candidate_cols[[ii]][1])){
  #       dta_cur = dta_list[[ii]]
  #       print(head(dta_cur[,store_candidate_cols[[ii]]]))
  #     }
  #   }
  # }
  # print out summary of relevant parts of data frame
  for(ii in 1:length(dta_list)){
    if(length(store_candidate_cols[[ii]])>0){
      if(!is.na(store_candidate_cols[[ii]][1])){      
        dta_cur = dta_list[[ii]]
        print(paste('ii=',ii,' (num rows=',nrow(dta_cur),'); column names:', colnames(dta_cur)[store_candidate_cols[[ii]]]))
        print(summary(dta_cur[,store_candidate_cols[[ii]]]))
      }
    }
  }
}




########################
# PfPR (microscopy)
########################
find_code_locations(dta_list=dta_list, code_str='hml32')
find_code_locations(dta_list=dta_list, code_str='hml33')
find_code_locations(dta_list=dta_list, code_str='hml35')


########################
# month of survey in cluster
########################
find_code_locations(dta_list=dta_list, code_str='v006')
find_code_locations(dta_list=dta_list, code_str='hv006')


########################
# ITNs
########################
# possible_code_strs = c('hml20','hml19','s508', '460','461')
find_code_locations(dta_list=dta_list, code_str='hml20')
find_code_locations(dta_list=dta_list, code_str='hml19')

######################
# treatment-seeking
#####################
find_code_locations(dta_list=dta_list, code_str='h32z')

############################################
# receive heel prick given seek treatment
############################################
find_code_locations(dta_list=dta_list, code_str='h47')

###################################
# IPTp >=1 dose
###################################
find_code_locations(dta_list=dta_list, code_str='m49a')


###################################
# IPTp number of doses
###################################
find_code_locations(dta_list=dta_list, code_str='ml1')


###################################
# vaccinations (proxy for IPTi coverage)
###################################
find_code_locations(dta_list=dta_list, code_str='h3')  # DPT1 vaccination (1=date shown on health card, 2=no health card, but respondent reported the child received the vaccine, 3=health card has vaccine but not date of vaccine)
find_code_locations(dta_list=dta_list, code_str='h5')  # DPT2 vaccination (1=date shown on health card, 2=no health card, but respondent reported the child received the vaccine, 3=health card has vaccine but not date of vaccine)
find_code_locations(dta_list=dta_list, code_str='h7')  # DPT3 vaccination (1=date shown on health card, 2=no health card, but respondent reported the child received the vaccine, 3=health card has vaccine but not date of vaccine)
find_code_locations(dta_list=dta_list, code_str='h4')  # Polio1 vaccination (1=date shown on health card, 2=no health card, but respondent reported the child received the vaccine, 3=health card has vaccine but not date of vaccine)
find_code_locations(dta_list=dta_list, code_str='h6')  # Polio2 vaccination (1=date shown on health card, 2=no health card, but respondent reported the child received the vaccine, 3=health card has vaccine but not date of vaccine)
find_code_locations(dta_list=dta_list, code_str='h8')  # Polio3 vaccination (1=date shown on health card, 2=no health card, but respondent reported the child received the vaccine, 3=health card has vaccine but not date of vaccine)
find_code_locations(dta_list=dta_list, code_str='h51')  # Penta1 vaccination (1=date shown on health card, 2=no health card, but respondent reported the child received the vaccine, 3=health card has vaccine but not date of vaccine)
find_code_locations(dta_list=dta_list, code_str='h52')  # Penta2 vaccination (1=date shown on health card, 2=no health card, but respondent reported the child received the vaccine, 3=health card has vaccine but not date of vaccine)
find_code_locations(dta_list=dta_list, code_str='h53')  # Penta3 vaccination (1=date shown on health card, 2=no health card, but respondent reported the child received the vaccine, 3=health card has vaccine but not date of vaccine)

find_code_locations(dta_list=dta_list, code_str='q514')  # polio vaccination
find_code_locations(dta_list=dta_list, code_str='q516')  # number of polio vaccinations
find_code_locations(dta_list=dta_list, code_str='q517')  # pentavalent vaccination
find_code_locations(dta_list=dta_list, code_str='q518')  # number of pentavalent vaccinations



###################################
# time between fever onset and ACT
###################################
find_code_locations(dta_list=dta_list, code_str='ml20a')


###################################
# child took combination with artemisinin
###################################
#BASE for ML13A-Z: Children suffering from fever or short rapid breaths or difficulty
# breathing in the last two weeks (H22 = 1 or H31B = 1). In previous recodes the base was
# restricted to children with fever in the last 2 weeks (H22 = 1).
# Questions pertaining to ML14A to ML14Z are no longer part of the DHS VII core
# questionnaire, but the variables are kept in the DHS VII recode
find_code_locations(dta_list=dta_list, code_str='ml13e')



###################################
# months ago household obtained net
###################################
find_code_locations(dta_list=dta_list, code_str='hml4')

###################################
# time since individual received net
###################################
hist(dta_list[[8]]$hml4[dta_list[[8]]$hml4<80], xlab='months ago received net', main='net age at time of 2016 DHS')

length(which(dta_list[[8]]$hml4[dta_list[[8]]$hml4<80] < 12)) / length(which(dta_list[[8]]$hml4[dta_list[[8]]$hml4<80]<80))

length(which(dta_list[[8]]$hml4[dta_list[[8]]$hml4<80] > 14)) / length(which(dta_list[[8]]$hml4[dta_list[[8]]$hml4<80]<80))





###################################
# ANC
###################################
# M14  Number of antenatal visits during the pregnancy. Women who did not see anyone for antenatal care during the pregnancy are coded 0. 







# =============================================================================== #
#     old
# =============================================================================== #



########################
# PfPR (microscopy)
########################

possible_code_strs = c('hml32', 'hml32a', 'hml33', 'hml35')
code_str = possible_code_strs[4]

find_code_locations(dta_list=dta_list, code_str='hml32')
find_code_locations(dta_list=dta_list, code_str='hml33')
find_code_locations(dta_list=dta_list, code_str='hml35')


# it appears that dta_1's 'hml32' entry is the one we want to use for PfPR
# one of the columns of dta_1 is hv020, of which all entries are 'all woman sample,' which I think is okay looking at the recode manual: I think it just indicates how information was recorded for the household surveys
#      unique(dta_1[,grep(colnames(dta_1), pattern = 'hv020')])
# also for dta_1, there is a question on the number of children aged 5 and under, and this takes values from 0-8.  since the microscopy entry is 
#     only positive or negative in each line, does it correspond to just one child given the test per household? or is it asking if any of the children 
#     are positive? ah, okay, hhid gives the household id, then within a household, I believe hvidx gives the index of the individual. 
#     there appears to be one entry per individual in the household, so the micropscopy results are per individual. this is supported by the 'hv105' variable, which is the age of the individual
#        table((dta_1[,grep(colnames(dta_1), pattern = 'hv014')]))
#        dta_1[which(dta_1[,grep(colnames(dta_1), pattern = 'hv014')]==3)[1:10],c(1:19, 160, 262:269)]
#        # subset to children under 5
#        dta_1[intersect(which(dta_1[,grep(colnames(dta_1), pattern = 'hv105')]<=5), which(dta_1[,grep(colnames(dta_1), pattern = 'hv014')]==3))[1:10],c(1:19, 160, 262:269)]
#        table(dta_1[which(dta_1[,grep(colnames(dta_1), pattern = 'hv105')]<=5),262])

# I think 'hv001' gives the cluster id - there appear to be 336 clusters in 2016
#        table((dta_1[,grep(colnames(dta_1), pattern = 'hv001')]))

# comparing dta_4 with dta_1, I think there's a lot of duplicate/overlap in that dta_4 has one row per household, but then breaks out certain entries by individual, so there is hml32_01, hml32_02, etc. for each person in a house. 


# get mean PfPR as well as number of children with reported values for each cluster
# subset to non-na microscopy values
# dta_mic = dta_1[!is.na(dta_1[,grep(colnames(dta_1), pattern = 'hml32')]),]
dta_1$mic_pos = NA
dta_1$mic_pos[which(dta_1[,grep(colnames(dta_1), pattern = 'hml32')] == 'positive')] = 1
dta_1$mic_pos[which(dta_1[,grep(colnames(dta_1), pattern = 'hml32')] == 'negative')] = 0
dta_pfpr_cluster = dta_1  %>%
  filter(!is.na(dta_1$mic_pos)) %>%
  group_by(hv001) %>%
  summarize(pfpr = mean(mic_pos, na.rm = TRUE),
            num_pos = sum(mic_pos),
            mean_age = mean(hv105, na.rm=TRUE),
            num_tested = n()) 

# can we now match each entry with a cluster and a location?  - none of the dtas appear to have gps coordinates associated with entries. perhaps somewhere else in DHS/MIS records?
# well, in the MAP-extracted outputs from Kate, there are GPS coordinates associated with 336 clusters, which seems promising, assuming the cluster ids match up correctly.
# match 'hv001' with 'clusterid'
pfpr_cluster = merge(dta_pfpr_cluster, map, by.x='hv001', by.y='clusterid', all=TRUE)
pfpr_cluster = pfpr_cluster[,c('hv001','pfpr','num_pos','mean_age','num_tested', 'SurveyName','latitude','longitude','urban_rural')]

layout(matrix(c(1,1,1,2, 1,1,1,3),nrow=2, byrow=TRUE))
plot(pfpr_cluster$longitude, pfpr_cluster$latitude, col=rev(rainbow(150, alpha=0.5))[50+round(pfpr_cluster$pfpr*100)], pch=20, cex=pfpr_cluster$num_tested/8, main='PfPR (microscopy, U5)', xlab='longitude', ylab='latitude')
plot(rep(0,100), seq(0,1,length.out=100), col=rev(rainbow(150, alpha=0.5))[50+round(seq(0,1,length.out=100)*100)], pch=15, cex=1.2, ylab='PfPR', axes=FALSE, xlab=''); axis(2)
plot(rep(0,5), seq(round(min(pfpr_cluster$num_tested)), round(max(pfpr_cluster$num_tested)), length.out=5), cex=seq(round(min(pfpr_cluster$num_tested)), round(max(pfpr_cluster$num_tested)), length.out=5)/8, pch=20, axes=FALSE, xlab='', ylab='sample size'); axis(2)


MIS_2016 = pfpr_cluster
colnames(MIS_2016)[colnames(MIS_2016)=='pfpr'] = 'mic_rate'
colnames(MIS_2016)[colnames(MIS_2016)=='num_pos'] = 'mic_num_true'
colnames(MIS_2016)[colnames(MIS_2016)=='num_tested'] = 'mic_num_total'
colnames(MIS_2016)[colnames(MIS_2016)=='mean_age'] = 'mic_mean_age'






########################
# ITNs
########################


possible_code_strs = c('hml20','hml19','s508', '460','461')
code_str = possible_code_strs[1]
code_str='hml20'

find_code_locations(dta_list=dta_list, code_str='hml20')
find_code_locations(dta_list=dta_list, code_str='hml19')


# there is a 's508' in both dta_2 and dta_3. there is also hml19 in dta_1 and dta_4 ('person slept under an ever treated net') <-- this one is good in that it is for household members. there's also hml20, which is "person slept under LLIN net." the hml19 and hml20 numbers appear fairly similar. I'll plan to use hml20
# age distribution of people reporting use of nets
# hist(dta_1$hv105[which(dta_1[,grep(colnames(dta_1), pattern = code_str)] == 'yes')])


# get mean PfPR as well as number of children with reported values for each cluster
# subset to non-na microscopy values
# dta_mic = dta_1[!is.na(dta_1[,grep(colnames(dta_1), pattern = 'hml32')]),]
dta_1$used_itn = NA
dta_1$used_itn[which(dta_1[,grep(colnames(dta_1), pattern = code_str)] == 'yes')] = 1
dta_1$used_itn[which(dta_1[,grep(colnames(dta_1), pattern = code_str)] == 'no')] = 0
dta_itn_cluster = dta_1  %>%
  filter(!is.na(dta_1$used_itn)) %>%
  group_by(hv001) %>%
  summarize(itn_use_rate = mean(used_itn, na.rm = TRUE),
            num_pos = sum(used_itn),
            mean_age = mean(hv105, na.rm=TRUE),
            num_included = n()) 



# can we now match each entry with a cluster and a location?  - none of the dtas appear to have gps coordinates associated with entries. perhaps somewhere else in DHS/MIS records?
# well, in the MAP-extracted outputs from Kate, there are GPS coordinates associated with 336 clusters, which seems promising, assuming the cluster ids match up correctly.
# match 'hv001' with 'clusterid'
itn_cluster = merge(dta_itn_cluster, locations, by.x='hv001', by.y='clusterid', all=TRUE)
itn_cluster = itn_cluster[,c('hv001','itn_use_rate','num_pos','mean_age','num_included', 'SurveyName','latitude','longitude','urban_rural')]
layout(matrix(c(1,1,1,2, 1,1,1,3),nrow=2, byrow=TRUE))
plot(itn_cluster$longitude, itn_cluster$latitude, col=(rainbow(150, alpha=0.5))[1+round(itn_cluster$itn_use_rate*100)], pch=20, cex=itn_cluster$num_included/40, main='ITN use (all age)', xlab='longitude', ylab='latitude')
plot(rep(0,100), seq(0,1,length.out=100), col=(rainbow(150, alpha=0.5))[1+round(seq(0,1,length.out=100)*100)], pch=15, cex=1.2, ylab='rate', axes=FALSE, xlab=''); axis(2)
plot(rep(0,5), seq(round(min(itn_cluster$num_included)), round(max(itn_cluster$num_included)), length.out=5), cex=seq(round(min(itn_cluster$num_included)), round(max(itn_cluster$num_included)), length.out=5)/40, pch=20, axes=FALSE, xlab='', ylab='sample size'); axis(2)


# combine with other MIS data points
MIS_2016 = merge(MIS_2016, itn_cluster, by=c('hv001', 'SurveyName', 'latitude','longitude','urban_rural'))
colnames(MIS_2016)[colnames(MIS_2016)=='itn_use_rate'] = 'itn_all_rate'
colnames(MIS_2016)[colnames(MIS_2016)=='num_pos'] = 'itn_all_num_true'
colnames(MIS_2016)[colnames(MIS_2016)=='num_included'] = 'itn_all_num_total'
colnames(MIS_2016)[colnames(MIS_2016)=='mean_age'] = 'itn_all_mean_age'




# break out U5 and O5 ITN use rates
# U5
dta_1_u5 = dta_1[dta_1$hv105<=5,]
dta_1_u5$used_itn = NA
dta_1_u5$used_itn[which(dta_1_u5[,grep(colnames(dta_1_u5), pattern = code_str)] == 'yes')] = 1
dta_1_u5$used_itn[which(dta_1_u5[,grep(colnames(dta_1_u5), pattern = code_str)] == 'no')] = 0
dta_itn_cluster_u5 = dta_1_u5  %>%
  filter(!is.na(dta_1_u5$used_itn)) %>%
  group_by(hv001) %>%
  summarize(itn_use_rate = mean(used_itn, na.rm = TRUE),
            num_pos = sum(used_itn),
            mean_age = mean(hv105, na.rm=TRUE),
            num_included = n()) 
# can we now match each entry with a cluster and a location?  - none of the dtas appear to have gps coordinates associated with entries. perhaps somewhere else in DHS/MIS records?
# well, in the MAP-extracted outputs from Kate, there are GPS coordinates associated with 336 clusters, which seems promising, assuming the cluster ids match up correctly.
# match 'hv001' with 'clusterid'
# itn_cluster_u5 = merge(dta_itn_cluster_u5, map, by.x='hv001', by.y='clusterid', all=TRUE)
# itn_cluster_u5 = itn_cluster_u5[,c('hv001','itn_use_rate','num_pos','mean_age','num_included', 'SurveyName','latitude','longitude','urban_rural')]
# plot(itn_cluster_u5$longitude, itn_cluster_u5$latitude, col=(rainbow(150, alpha=0.5))[1+round(itn_cluster_u5$itn_use_rate*100)], pch=20, cex=itn_cluster_u5$num_included/20, main='ITN use (U5)', xlab='longitude', ylab='latitude')

# combine with other MIS data points
# MIS_2016 = merge(MIS_2016, itn_cluster_u5, by=c('hv001', 'SurveyName', 'latitude','longitude','urban_rural'))
# colnames(MIS_2016)[colnames(MIS_2016)=='itn_use_rate'] = 'itn_u5_rate'
# colnames(MIS_2016)[colnames(MIS_2016)=='num_pos'] = 'itn_u5_num_true'
# colnames(MIS_2016)[colnames(MIS_2016)=='num_included'] = 'itn_u5_num_total'
# colnames(MIS_2016)[colnames(MIS_2016)=='mean_age'] = 'itn_u5_mean_age'


# O5
dta_1_o5 = dta_1[dta_1$hv105>5,]
dta_1_o5$used_itn = NA
dta_1_o5$used_itn[which(dta_1_o5[,grep(colnames(dta_1_o5), pattern = code_str)] == 'yes')] = 1
dta_1_o5$used_itn[which(dta_1_o5[,grep(colnames(dta_1_o5), pattern = code_str)] == 'no')] = 0
dta_itn_cluster_o5 = dta_1_o5  %>%
  filter(!is.na(dta_1_o5$used_itn)) %>%
  group_by(hv001) %>%
  summarize(itn_use_rate = mean(used_itn, na.rm = TRUE),
            num_pos = sum(used_itn),
            mean_age = mean(hv105, na.rm=TRUE),
            num_included = n()) 
# can we now match each entry with a cluster and a location?  - none of the dtas appear to have gps coordinates associated with entries. perhaps somewhere else in DHS/MIS records?
# well, in the MAP-extracted outputs from Kate, there are GPS coordinates associated with 336 clusters, which seems promising, assuming the cluster ids match up correctly.
# match 'hv001' with 'clusterid'
# itn_cluster_o5 = merge(dta_itn_cluster_o5, locations, by.x='hv001', by.y='clusterid', all=TRUE)
# itn_cluster_o5 = itn_cluster[,c('hv001','itn_use_rate','num_pos','mean_age','num_included', 'SurveyName','latitude','longitude','urban_rural')]
# plot(itn_cluster_o5$longitude, itn_cluster_o5$latitude, col=(rainbow(150, alpha=0.5))[1+round(itn_cluster_o5$itn_use_rate*100)], pch=20, cex=itn_cluster_o5$num_included/80, main='ITN use (O5)', xlab='longitude', ylab='latitude')

# # fairly good linear relationship between I5 and O5 ITN use rates. not quite 1:1, but not that far from it.
# itn_cluster_age = merge(itn_cluster_u5, itn_cluster_o5[,c('hv001','itn_use_rate')], by=c('hv001', 'SurveyName', 'latitude','longitude','urban_rural'))
# plot(itn_cluster_age$itn_use_rate.x, itn_cluster_age$itn_use_rate.y, xlim=c(0,1), ylim=c(0,1), xlab='u5',ylab='o5')
# abline(a=0,b=1)

# combine with other MIS data points
# MIS_2016 = merge(MIS_2016, itn_cluster_o5, by=c('hv001', 'SurveyName', 'latitude','longitude','urban_rural'))
# colnames(MIS_2016)[colnames(MIS_2016)=='itn_use_rate'] = 'itn_o5_rate'
# colnames(MIS_2016)[colnames(MIS_2016)=='num_pos'] = 'itn_o5_num_true'
# colnames(MIS_2016)[colnames(MIS_2016)=='num_included'] = 'itn_o5_num_total'
# colnames(MIS_2016)[colnames(MIS_2016)=='mean_age'] = 'itn_o5_mean_age'



######################
# treatment-seeking
#####################

possible_code_strs = c('h32z')
code_str = possible_code_strs[1]

find_code_locations(dta_list=dta_list, code_str='h32z')


# I'll use the value from dta_2, which seems to have each row correspond to an individual, while dta_3 has separate columns for multiple individuals with fever.
# this variable is for children born in the past five years. asks whether child was taken to a medical facility for treatment of the fever and/or cough, among children who had fever/cough in last two weeks

# subset to non-na
dta_2$treat = NA
dta_2$treat[which(dta_2[,grep(colnames(dta_2), pattern = 'h32z')] == 'yes')] = 1
dta_2$treat[which(dta_2[,grep(colnames(dta_2), pattern = 'h32z')] == 'no')] = 0
dta_treat_cluster = dta_2  %>%
  filter(!is.na(dta_2$treat)) %>%
  group_by(v001) %>%
  summarize(treat_rate = mean(treat, na.rm = TRUE),
            num_treat = sum(treat),
            num_w_fever = n()) 

# can we now match each entry with a cluster and a location?  - none of the dtas appear to have gps coordinates associated with entries. perhaps somewhere else in DHS/MIS records?
# well, in the MAP-extracted outputs from Kate, there are GPS coordinates associated with 336 clusters, which seems promising, assuming the cluster ids match up correctly.
# match 'hv001' with 'clusterid'
treat_cluster = merge(dta_treat_cluster, locations, by.x='v001', by.y='clusterid', all=TRUE)
treat_cluster = treat_cluster[,c('v001','treat_rate','num_treat','num_w_fever', 'SurveyName','latitude','longitude','urban_rural')]
layout(matrix(c(1,1,1,2, 1,1,1,3),nrow=2, byrow=TRUE))
plot(treat_cluster$longitude, treat_cluster$latitude, col=(rainbow(150, alpha=0.5))[1+round(treat_cluster$treat_rate*100)], pch=20, cex=treat_cluster$num_w_fever/2, main='seek treatment for fever (U5)', xlab='longitude', ylab='latitude')
plot(rep(0,100), seq(0,1,length.out=100), col=(rainbow(150, alpha=0.5))[1+round(seq(0,1,length.out=100)*100)], pch=15, cex=1.2, ylab='rate', axes=FALSE, xlab=''); axis(2)
plot(rep(0,5), seq(round(min(treat_cluster$num_w_fever, na.rm=TRUE)), round(max(treat_cluster$num_w_fever, na.rm=TRUE)), length.out=5), cex=seq(round(min(treat_cluster$num_w_fever, na.rm=TRUE)), round(max(treat_cluster$num_w_fever, na.rm=TRUE)), length.out=5)/2, pch=20, axes=FALSE, xlab='', ylab='sample size'); axis(2)

# hist(treat_cluster$treat_rate)
# sum(treat_cluster$treat_rate * treat_cluster$num_w_fever, na.rm=TRUE)/sum(treat_cluster$num_w_fever, na.rm=TRUE)



# combine with other MIS data points
MIS_2016 = merge(MIS_2016, treat_cluster, by.x=c('hv001', 'SurveyName', 'latitude','longitude','urban_rural'), by.y=c('v001', 'SurveyName', 'latitude','longitude','urban_rural'))
colnames(MIS_2016)[colnames(MIS_2016)=='treat_rate'] = 'cm_rate'
colnames(MIS_2016)[colnames(MIS_2016)=='num_treat'] = 'cm_num_true'
colnames(MIS_2016)[colnames(MIS_2016)=='num_w_fever'] = 'cm_num_total'





############################################
# receive heel prick given seek treatment
############################################


possible_code_strs = c('h47')
code_str = possible_code_strs[1]

# grep(colnames(dta_1), pattern = code_str)  # several columns contain hml32
# grep(colnames(dta_2), pattern = code_str)
# grep(colnames(dta_3), pattern = code_str)
# grep(colnames(dta_4), pattern = code_str) # numerous columns contain hml32
# grep(colnames(dta_5), pattern = code_str)
# 
# dta_2[1:5,grep(colnames(dta_2), pattern = code_str)]
# dta_3[1:5,grep(colnames(dta_3), pattern = code_str)]
# 
# # determine which columns with the code_str have non-NA values in each dta
# store_candidate_cols = list()
# for(ii in 1:length(dta_list)){
#   dta_cur = dta_list[[ii]]
#   cur_cols = grep(colnames(dta_cur), pattern = code_str)
#   if(length(cur_cols)>0){
#     # find columns that contain something other than NA
#     can_cols = c()
#     for(cc in 1:length(cur_cols)){
#       if(any(!is.na(unique(dta_cur[,cur_cols[cc]])))) can_cols = c(can_cols, cur_cols[cc])
#     }
#     store_candidate_cols[[ii]] = can_cols
#   }else{
#     store_candidate_cols[[ii]] = NA
#   }
# }
# 
# # # print out head of relevant parts of data frame
# # for(ii in 1:length(dta_list)){
# #   if(!is.na(store_candidate_cols[[ii]][1]) & length(store_candidate_cols[[ii]])>0){
# #     dta_cur = dta_list[[ii]]
# #     print(head(dta_cur[,store_candidate_cols[[ii]]]))
# #   }
# # }
# 
# # print out summary of relevant parts of data frame
# for(ii in 1:length(dta_list)){
#   if(!is.na(store_candidate_cols[[ii]][1]) & length(store_candidate_cols[[ii]])>0){
#     dta_cur = dta_list[[ii]]
#     print(paste('ii=',ii))
#     print(summary(dta_cur[,store_candidate_cols[[ii]]]))
#   }
# }

# I'll use the value from dta_2, which seems to have each row correspond to an individual, while dta_3 has separate columns for multiple individuals with fever.
# this variable is for children born in the past five years. asks whether blood was taken for testing, among children who had fever/cough in last two weeks

# subset to non-na
dta_2$treat = NA
dta_2$treat[which(dta_2[,grep(colnames(dta_2), pattern = 'h47')] == 'yes')] = 1
dta_2$treat[which(dta_2[,grep(colnames(dta_2), pattern = 'h47')] == 'no')] = 0
dta_treat_cluster = dta_2  %>%
  filter(!is.na(dta_2$treat)) %>%
  group_by(v001) %>%
  summarize(blood_test_rate = mean(treat, na.rm = TRUE),
            num_blood_test = sum(treat),
            num_w_fever_blood = n()) 

# can we now match each entry with a cluster and a location?  - none of the dtas appear to have gps coordinates associated with entries. perhaps somewhere else in DHS/MIS records?
# well, in the MAP-extracted outputs from Kate, there are GPS coordinates associated with 336 clusters, which seems promising, assuming the cluster ids match up correctly.
# match 'hv001' with 'clusterid'
treat_cluster = merge(dta_treat_cluster, locations, by.x='v001', by.y='clusterid', all=TRUE)
treat_cluster = treat_cluster[,c('v001','blood_test_rate','num_blood_test','num_w_fever_blood', 'SurveyName','latitude','longitude','urban_rural')]
layout(matrix(c(1,1,1,2, 1,1,1,3),nrow=2, byrow=TRUE))
plot(treat_cluster$longitude, treat_cluster$latitude, col=(rainbow(150, alpha=0.5))[1+round(treat_cluster$blood_test_rate*100)], pch=20, cex=treat_cluster$num_w_fever_blood/2, main='blood sample with fever (U5)', xlab='longitude', ylab='latitude')
plot(rep(0,100), seq(0,1,length.out=100), col=(rainbow(150, alpha=0.5))[1+round(seq(0,1,length.out=100)*100)], pch=15, cex=1.2, ylab='rate', axes=FALSE, xlab=''); axis(2)
plot(rep(0,5), seq(round(min(treat_cluster$num_w_fever_blood, na.rm=TRUE)), round(max(treat_cluster$num_w_fever_blood, na.rm=TRUE)), length.out=5), cex=seq(round(min(treat_cluster$num_w_fever_blood, na.rm=TRUE)), round(max(treat_cluster$num_w_fever_blood, na.rm=TRUE)), length.out=5)/2, pch=20, axes=FALSE, xlab='', ylab='sample size'); axis(2)


# hist(treat_cluster$treat_rate)
# sum(treat_cluster$treat_rate * treat_cluster$num_w_fever, na.rm=TRUE)/sum(treat_cluster$num_w_fever, na.rm=TRUE)


# combine with other MIS data points
MIS_2016 = merge(MIS_2016, treat_cluster, by.x=c('hv001', 'SurveyName', 'latitude','longitude','urban_rural'), by.y=c('v001', 'SurveyName', 'latitude','longitude','urban_rural'))
colnames(MIS_2016)[colnames(MIS_2016)=='num_blood_test'] = 'blood_test_num_true'
colnames(MIS_2016)[colnames(MIS_2016)=='num_w_fever_blood'] = 'blood_test_num_total'





###################################
# IPTp >=1 dose
###################################



possible_code_strs = c('m49a')
code_str = possible_code_strs[1]

# grep(colnames(dta_1), pattern = code_str)  # several columns contain hml32
# grep(colnames(dta_2), pattern = code_str)
# grep(colnames(dta_3), pattern = code_str)
# grep(colnames(dta_4), pattern = code_str) # numerous columns contain hml32
# grep(colnames(dta_5), pattern = code_str)
# 
# dta_2[1:5,grep(colnames(dta_2), pattern = code_str)]
# dta_3[1:5,grep(colnames(dta_3), pattern = code_str)]
# 
# # determine which columns with the code_str have non-NA values in each dta
# store_candidate_cols = list()
# for(ii in 1:length(dta_list)){
#   dta_cur = dta_list[[ii]]
#   cur_cols = grep(colnames(dta_cur), pattern = code_str)
#   if(length(cur_cols)>0){
#     # find columns that contain something other than NA
#     can_cols = c()
#     for(cc in 1:length(cur_cols)){
#       if(any(!is.na(unique(dta_cur[,cur_cols[cc]])))) can_cols = c(can_cols, cur_cols[cc])
#     }
#     store_candidate_cols[[ii]] = can_cols
#   }else{
#     store_candidate_cols[[ii]] = NA
#   }
# }
# 
# # # print out head of relevant parts of data frame
# # for(ii in 1:length(dta_list)){
# #   if(!is.na(store_candidate_cols[[ii]][1]) & length(store_candidate_cols[[ii]])>0){
# #     dta_cur = dta_list[[ii]]
# #     print(head(dta_cur[,store_candidate_cols[[ii]]]))
# #   }
# # }
# 
# # print out summary of relevant parts of data frame
# for(ii in 1:length(dta_list)){
#   if(!is.na(store_candidate_cols[[ii]][1]) & length(store_candidate_cols[[ii]])>0){
#     dta_cur = dta_list[[ii]]
#     print(paste('ii=',ii))
#     print(summary(dta_cur[,store_candidate_cols[[ii]]]))
#   }
# }

# I'll use the value from dta_2, which seems to have each row correspond to an individual, while dta_3 has separate columns for multiple individuals with fever.
# this variable is for children born in the past five years. asks whether blood was taken for testing, among children who had fever/cough in last two weeks

# subset to non-na
dta_2$treat = NA
dta_2$treat[which(dta_2[,grep(colnames(dta_2), pattern = 'm49a')] == 'yes')] = 1
dta_2$treat[which(dta_2[,grep(colnames(dta_2), pattern = 'm49a')] == 'no')] = 0
dta_treat_cluster = dta_2  %>%
  filter(!is.na(dta_2$treat)) %>%
  group_by(v001) %>%
  summarize(iptp_rate = mean(treat, na.rm = TRUE),
            num_iptp = sum(treat),
            num_preg = n()) 

# can we now match each entry with a cluster and a location?  - none of the dtas appear to have gps coordinates associated with entries. perhaps somewhere else in DHS/MIS records?
# well, in the MAP-extracted outputs from Kate, there are GPS coordinates associated with 336 clusters, which seems promising, assuming the cluster ids match up correctly.
# match 'hv001' with 'clusterid'
treat_cluster = merge(dta_treat_cluster, locations, by.x='v001', by.y='clusterid', all=TRUE)
treat_cluster = treat_cluster[,c('v001','iptp_rate','num_iptp','num_preg', 'SurveyName','latitude','longitude','urban_rural')]
plot(treat_cluster$longitude, treat_cluster$latitude, col=(rainbow(150, alpha=0.5))[1+round(treat_cluster$iptp_rate*100)], pch=20, cex=treat_cluster$num_preg/5, main='receive IPTp', xlab='longitude', ylab='latitude')
# plot(seq(0,1,length.out=100),col=(rainbow(150, alpha=0.5))[1+round(seq(0,1,length.out=100)*100)], pch=20)
 
# combine with other MIS data points
MIS_2016 = merge(MIS_2016, treat_cluster, by.x=c('hv001', 'SurveyName', 'latitude','longitude','urban_rural'), by.y=c('v001', 'SurveyName', 'latitude','longitude','urban_rural'))
colnames(MIS_2016)[colnames(MIS_2016)=='num_iptp'] = 'iptp_num_true'
colnames(MIS_2016)[colnames(MIS_2016)=='num_preg'] = 'iptp_num_total'







###################################
# IPTp distribution of number of doses
###################################


possible_code_strs = c('ml1')
code_str = possible_code_strs[1]

colnames(dta_1)[grep(colnames(dta_1), pattern = code_str)]  # several columns contain hml32
colnames(dta_2)[grep(colnames(dta_2), pattern = code_str)]
colnames(dta_3)[grep(colnames(dta_3), pattern = code_str)]
colnames(dta_4)[grep(colnames(dta_4), pattern = code_str)] # numerous columns contain hml32
colnames(dta_5)[grep(colnames(dta_5), pattern = code_str)]

# times took SP/Fansidar during pregnancy, given that it was taken at least once -> 3 now means >=3
table(dta_2$ml1)
dta_2$iptp_num_doses = dta_2$ml1
dta_2$iptp_num_doses[dta_2$iptp_num_doses == 0] = NA
dta_2$iptp_num_doses[dta_2$iptp_num_doses > 15] = NA
dta_2$iptp_num_doses[dta_2$iptp_num_doses > 3] = 3
table(dta_2$iptp_num_doses)
# use same ratio across country due to small numbers when break into clusters













##################################################################################
# determine which clusters are in which chiefdoms
##################################################################################


# turn MIS_2016 into spatial points data frame
# not sure what the spatial projection is... will try with same spatial projection as shapefile
points_crs = crs(admin_shape)
MIS_2016_shape = SpatialPointsDataFrame(MIS_2016[,c('longitude', 'latitude')],
                                        MIS_2016,
                                        proj4string = points_crs)
# find which chiefdom each cluster belongs to
MIS_2016_locations = over(MIS_2016_shape, admin_shape)
if(nrow(MIS_2016_locations) == nrow(MIS_2016_shape)){
  MIS_2016_shape$NAME_3 = MIS_2016_locations$NAME_3
  MIS_2016_shape$NAME_2 = MIS_2016_locations$NAME_2
  MIS_2016_shape$NAME_1 = MIS_2016_locations$NAME_1
}

write.csv(as.data.frame(MIS_2016_shape), 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/SierraLeone_hbhi/explore_DHS/MIS_2016.csv')


par(mfrow=c(3,4))
# for each of the interventions/measures, count number of individuals surveyed in each chiefdom
num_surveyed = as.data.frame(MIS_2016_shape[c('NAME_3','num_mic_test', 'num_use_itn_all', 'num_w_fever', 'num_preg')]) %>% 
  group_by(NAME_3) %>%
  summarise(num_mic_test=sum(num_mic_test, na.rm = TRUE),
            num_use_itn_all=sum(num_use_itn_all, na.rm = TRUE),
            num_w_fever=sum(num_w_fever, na.rm = TRUE),
            num_preg=sum(num_preg, na.rm = TRUE))
num_surveyed = num_surveyed[!is.na(num_surveyed[,1]),]
hist(num_surveyed$num_mic_test,main='microscopy (PfPR)', breaks=seq(0,800, length.out=80))
hist(num_surveyed$num_use_itn_all,  main='ITN', breaks=seq(0,1600, length.out=80))
hist(num_surveyed$num_w_fever, main='fever (CM)', breaks=seq(0,200, length.out=80))
hist(num_surveyed$num_preg, main='preganacies (IPTp)', breaks=seq(0,500, length.out=80))


# look at survey numbers by district
# for each of the interventions/measures, count number of individuals surveyed in each chiefdom
num_surveyed = as.data.frame(MIS_2016_shape[c('NAME_2','num_mic_test', 'num_use_itn_all', 'num_w_fever', 'num_preg')]) %>% 
  group_by(NAME_2) %>%
  summarise(num_mic_test=sum(num_mic_test, na.rm = TRUE),
            num_use_itn_all=sum(num_use_itn_all, na.rm = TRUE),
            num_w_fever=sum(num_w_fever, na.rm = TRUE),
            num_preg=sum(num_preg, na.rm = TRUE))
num_surveyed = num_surveyed[!is.na(num_surveyed[,1]),]
hist(num_surveyed$num_mic_test,main='microscopy (PfPR)', breaks=seq(0,800, length.out=80))
hist(num_surveyed$num_use_itn_all,  main='ITN', breaks=seq(0,1600, length.out=80))
hist(num_surveyed$num_w_fever, main='fever (CM)', breaks=seq(0,200, length.out=80))
hist(num_surveyed$num_preg, main='preganacies (IPTp)', breaks=seq(0,500, length.out=80))

# look at survey numbers by region
# for each of the interventions/measures, count number of individuals surveyed in each chiefdom
num_surveyed = as.data.frame(MIS_2016_shape[c('NAME_1','num_mic_test', 'num_use_itn_all', 'num_w_fever', 'num_preg')]) %>% 
  group_by(NAME_1) %>%
  summarise(num_mic_test=sum(num_mic_test, na.rm = TRUE),
            num_use_itn_all=sum(num_use_itn_all, na.rm = TRUE),
            num_w_fever=sum(num_w_fever, na.rm = TRUE),
            num_preg=sum(num_preg, na.rm = TRUE))
num_surveyed = num_surveyed[!is.na(num_surveyed[,1]),]
hist(num_surveyed$num_mic_test,main='microscopy (PfPR)', breaks=seq(0,3000, length.out=80))
hist(num_surveyed$num_use_itn_all,  main='ITN',breaks=seq(0,6000, length.out=80))
hist(num_surveyed$num_w_fever, main='fever (CM)', breaks=seq(0,700, length.out=80))
hist(num_surveyed$num_preg, main='preganacies (IPTp)', breaks=seq(0,2000, length.out=80))
par(mfrow=c(1,1))




# get weighted means and number tested/positive within each chiefdom for all variables



