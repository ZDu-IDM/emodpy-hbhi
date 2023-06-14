# look at cases tested in various parts of the health system to understand role of CHWs


case_testing = read.csv('C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/data/Burundi/WHO/snt_2022/Copy of Cas de paludisme par secteur et par district 2021 et 2022.csv')

# rows with 'Nouveaux cas de Paludisme traité sans test' should be excluded
case_testing1 = case_testing[!grepl('Nouveaux cas de Paludisme traité sans test', case_testing$dataname),]

case_testing1$rowsum = rowSums(case_testing1[,-c(1:2)], na.rm=TRUE)
case_testing1$chw = as.numeric(grepl('GASC_TDR', case_testing1$dataname))
case_testing1$other = as.numeric(grepl('examens', case_testing1$dataname))
unique(case_testing1$chw+case_testing1$other)  # should only be 1
case_testing1$chw_num = case_testing1$rowsum * case_testing1$chw
case_testing1$other_num = case_testing1$rowsum * case_testing1$other

case_testing2 = case_testing1 %>% group_by(organisationunitname) %>%
  summarise(chw_num = sum(chw_num, na.rm=TRUE),
            other_num = sum(other_num, na.rm=TRUE)) %>%
  ungroup()


# a very small fraction of all tests are reported through CHWs
