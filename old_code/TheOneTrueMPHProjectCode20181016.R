# Oil and Gas Disposal Well Analysis
# Katherine Wolf
# January 2018

# setup
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)

setwd("C:/Rfiles/MPHProjectR/workingdata/")

#### creating master ACS data file ####
# # ACS data from TIGER to use
#
# # create full ACS dataset from individual ArcGIS tables
# # import files
# BG_METADATA_2016<-read.csv("ACSDataExportedFromArcGISAsText/BG_METADATA_2016.txt")
# View(BG_METADATA_2016)
# X00_COUNTS<-read.csv("ACSDataExportedFromArcGISAsText/X00_COUNTS.txt")
# X01_AGE_AND_SEX<-read.csv("ACSDataExportedFromArcGISAsText/X01_AGE_AND_SEX.txt")
# X02_RACE<-read.csv("ACSDataExportedFromArcGISAsText/X02_RACE.txt")
# X03_HISPANC_OR_LATINO_ORIGIN<-read.csv("ACSDataExportedFromArcGISAsText/X03_HISPANC_OR_LATINO_ORIGIN.txt")
# X07_MIGRATION<-read.csv("ACSDataExportedFromArcGISAsText/X07_MIGRATION.txt")
# X08_COMMUTING<-read.csv("ACSDataExportedFromArcGISAsText/X08_COMMUTING.txt")
# X09_CHILDREN_HOUSEHOLD_RELATIONSHIP<-read.csv("ACSDataExportedFromArcGISAsText/X09_CHILDREN_HOUSEHOLD_RELATIONSHIP.txt")
# X11_HOUSEHOLD_FAMILY_SUBFAMILIES<-read.csv("ACSDataExportedFromArcGISAsText/X11_HOUSEHOLD_FAMILY_SUBFAMILIES.txt")
# X12_MARITAL_STATUS_AND_HISTORY<-read.csv("ACSDataExportedFromArcGISAsText/X12_MARITAL_STATUS_AND_HISTORY.txt")
# X14_SCHOOL_ENROLLMENT<-read.csv("ACSDataExportedFromArcGISAsText/X14_SCHOOL_ENROLLMENT.txt")
# X15_EDUCATIONAL_ATTAINMENT<-read.csv("ACSDataExportedFromArcGISAsText/X15_EDUCATIONAL_ATTAINMENT.txt")
# X16_LANGUAGE_SPOKEN_AT_HOME<-read.csv("ACSDataExportedFromArcGISAsText/X16_LANGUAGE_SPOKEN_AT_HOME.txt")
# X17_POVERTY<-read.csv("ACSDataExportedFromArcGISAsText/X17_POVERTY.txt")
# X19_INCOME<-read.csv("ACSDataExportedFromArcGISAsText/X19_INCOME.txt")
# X20_EARNINGS<-read.csv("ACSDataExportedFromArcGISAsText/X20_EARNINGS.txt")
# X21_VETERAN_STATUS<-read.csv("ACSDataExportedFromArcGISAsText/X21_VETERAN_STATUS.txt")
# X22_FOOD_STAMPS<-read.csv("ACSDataExportedFromArcGISAsText/X22_FOOD_STAMPS.txt")
# X23_EMPLOYMENT_STATUS<-read.csv("ACSDataExportedFromArcGISAsText/X23_EMPLOYMENT_STATUS.txt")
# X24_INDUSTRY_OCCUPATION<-read.csv("ACSDataExportedFromArcGISAsText/X24_INDUSTRY_OCCUPATION.txt")
# X25_HOUSING_CHARACTERISTICS<-read.csv("ACSDataExportedFromArcGISAsText/X25_HOUSING_CHARACTERISTICS.txt")
# X27_HEALTH_INSURANCE<-read.csv("ACSDataExportedFromArcGISAsText/X27_HEALTH_INSURANCE.txt")
# X99_IMPUTATION<-read.csv("ACSDataExportedFromArcGISAsText/X99_IMPUTATION.txt")
# ACS_2016_5YR_BG<-read.csv("ACSDataExportedFromArcGISAsText/ACS_2016_5YR_BG.txt")
#
#
# # save ACS files as RData files so that I never have to reload them again
# save(BG_METADATA_2016,file="BG_METADATA_2016.rdata")
# save(X00_COUNTS,file="X00_COUNTS.rdata")
# save(X01_AGE_AND_SEX,file="X01_AGE_AND_SEX.rdata")
# save(X02_RACE,file="X02_RACE.rdata")
# save(X03_HISPANC_OR_LATINO_ORIGIN,file="X03_HISPANC_OR_LATINO_ORIGIN.rdata")
# save(X07_MIGRATION,file="X07_MIGRATION.rdata")
# save(X08_COMMUTING,file="X08_COMMUTING.rdata")
# save(X09_CHILDREN_HOUSEHOLD_RELATIONSHIP,file="X09_CHILDREN_HOUSEHOLD_RELATIONSHIP.rdata")
# save(X11_HOUSEHOLD_FAMILY_SUBFAMILIES,file="X11_HOUSEHOLD_FAMILY_SUBFAMILIES.rdata")
# save(X12_MARITAL_STATUS_AND_HISTORY,file="X12_MARITAL_STATUS_AND_HISTORY.rdata")
# save(X14_SCHOOL_ENROLLMENT,file="X14_SCHOOL_ENROLLMENT.rdata")
# save(X15_EDUCATIONAL_ATTAINMENT,file="X15_EDUCATIONAL_ATTAINMENT.rdata")
# save(X16_LANGUAGE_SPOKEN_AT_HOME,file="X16_LANGUAGE_SPOKEN_AT_HOME.rdata")
# save(X17_POVERTY,file="X17_POVERTY.rdata")
# save(X19_INCOME,file="X19_INCOME.rdata")
# save(X20_EARNINGS,file="X20_EARNINGS.rdata")
# save(X21_VETERAN_STATUS,file="X21_VETERAN_STATUS.rdata")
# save(X22_FOOD_STAMPS,file="X22_FOOD_STAMPS.rdata")
# save(X23_EMPLOYMENT_STATUS,file="X23_EMPLOYMENT_STATUS.rdata")
# save(X24_INDUSTRY_OCCUPATION,file="X24_INDUSTRY_OCCUPATION.rdata")
# save(X25_HOUSING_CHARACTERISTICS,file="X25_HOUSING_CHARACTERISTICS.rdata")
# save(X27_HEALTH_INSURANCE,file="X27_HEALTH_INSURANCE.rdata")
# save(X99_IMPUTATION,file="X99_IMPUTATION.rdata")
# save(ACS_2016_5YR_BG,file="ACS_2016_5YR_BG.rdata")
#
# load rdata files if needed
# load(file="BG_METADATA_2016.rdata")
# load(file="X00_COUNTS.rdata")
# load(file="X01_AGE_AND_SEX.rdata")
# load(file="X02_RACE.rdata")
# load(file="X03_HISPANC_OR_LATINO_ORIGIN.rdata")
# load(file="X07_MIGRATION.rdata")
# load(file="X08_COMMUTING.rdata")
# load(file="X09_CHILDREN_HOUSEHOLD_RELATIONSHIP.rdata")
# load(file="X11_HOUSEHOLD_FAMILY_SUBFAMILIES.rdata")
# load(file="X12_MARITAL_STATUS_AND_HISTORY.rdata")
# load(file="X14_SCHOOL_ENROLLMENT.rdata")
# load(file="X15_EDUCATIONAL_ATTAINMENT.rdata")
# load(file="X16_LANGUAGE_SPOKEN_AT_HOME.rdata")
# load(file="X17_POVERTY.rdata")
# load(file="X19_INCOME.rdata")
# load(file="X20_EARNINGS.rdata")
# load(file="X21_VETERAN_STATUS.rdata")
# load(file="X22_FOOD_STAMPS.rdata")
# load(file="X23_EMPLOYMENT_STATUS.rdata")
# load(file="X24_INDUSTRY_OCCUPATION.rdata")
# load(file="X25_HOUSING_CHARACTERISTICS.rdata")
# load(file="X27_HEALTH_INSURANCE.rdata")
# load(file="X99_IMPUTATION.rdata")
# load(file="ACS_2016_5YR_BG.rdata")
# 
# # convert everything to a data table
# BG_METADATA_2016_data_table<-data.table(BG_METADATA_2016)
# X00_COUNTS_data_table<-data.table(X00_COUNTS)
# X01_AGE_AND_SEX_data_table<-data.table(X01_AGE_AND_SEX)
# X02_RACE_data_table<-data.table(X02_RACE)
# X03_HISPANC_OR_LATINO_ORIGIN_data_table<-data.table(X03_HISPANC_OR_LATINO_ORIGIN)
# X07_MIGRATION_data_table<-data.table(X07_MIGRATION)
# X08_COMMUTING_data_table<-data.table(X08_COMMUTING)
# X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table<-data.table(X09_CHILDREN_HOUSEHOLD_RELATIONSHIP)
# X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table<-data.table(X11_HOUSEHOLD_FAMILY_SUBFAMILIES)
# X12_MARITAL_STATUS_AND_HISTORY_data_table<-data.table(X12_MARITAL_STATUS_AND_HISTORY)
# X14_SCHOOL_ENROLLMENT_data_table<-data.table(X14_SCHOOL_ENROLLMENT)
# X15_EDUCATIONAL_ATTAINMENT_data_table<-data.table(X15_EDUCATIONAL_ATTAINMENT)
# X16_LANGUAGE_SPOKEN_AT_HOME_data_table<-data.table(X16_LANGUAGE_SPOKEN_AT_HOME)
# X17_POVERTY_data_table<-data.table(X17_POVERTY)
# X19_INCOME_data_table<-data.table(X19_INCOME)
# X20_EARNINGS_data_table<-data.table(X20_EARNINGS)
# X21_VETERAN_STATUS_data_table<-data.table(X21_VETERAN_STATUS)
# X22_FOOD_STAMPS_data_table<-data.table(X22_FOOD_STAMPS)
# X23_EMPLOYMENT_STATUS_data_table<-data.table(X23_EMPLOYMENT_STATUS)
# X24_INDUSTRY_OCCUPATION_data_table<-data.table(X24_INDUSTRY_OCCUPATION)
# X25_HOUSING_CHARACTERISTICS_data_table<-data.table(X25_HOUSING_CHARACTERISTICS)
# X27_HEALTH_INSURANCE_data_table<-data.table(X27_HEALTH_INSURANCE)
# X99_IMPUTATION_data_table<-data.table(X99_IMPUTATION)
# ACS_2016_5YR_BG_data_table<-data.table(ACS_2016_5YR_BG)
# 
# # save data tables
# save(BG_METADATA_2016_data_table,file="BG_METADATA_2016_data_table.rdata")
# save(X00_COUNTS_data_table,file="X00_COUNTS_data_table.rdata")
# save(X01_AGE_AND_SEX_data_table,file="X01_AGE_AND_SEX_data_table.rdata")
# save(X02_RACE_data_table,file="X02_RACE_data_table.rdata")
# save(X03_HISPANC_OR_LATINO_ORIGIN_data_table,file="X03_HISPANC_OR_LATINO_ORIGIN_data_table.rdata")
# save(X07_MIGRATION_data_table,file="X07_MIGRATION_data_table.rdata")
# save(X08_COMMUTING_data_table,file="X08_COMMUTING_data_table.rdata")
# save(X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table,file="X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table.rdata")
# save(X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table,file="X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table.rdata")
# save(X12_MARITAL_STATUS_AND_HISTORY_data_table,file="X12_MARITAL_STATUS_AND_HISTORY_data_table.rdata")
# save(X14_SCHOOL_ENROLLMENT_data_table,file="X14_SCHOOL_ENROLLMENT_data_table.rdata")
# save(X15_EDUCATIONAL_ATTAINMENT_data_table,file="X15_EDUCATIONAL_ATTAINMENT_data_table.rdata")
# save(X16_LANGUAGE_SPOKEN_AT_HOME_data_table,file="X16_LANGUAGE_SPOKEN_AT_HOME_data_table.rdata")
# save(X17_POVERTY_data_table,file="X17_POVERTY_data_table.rdata")
# save(X19_INCOME_data_table,file="X19_INCOME_data_table.rdata")
# save(X20_EARNINGS_data_table,file="X20_EARNINGS_data_table.rdata")
# save(X21_VETERAN_STATUS_data_table,file="X21_VETERAN_STATUS_data_table.rdata")
# save(X22_FOOD_STAMPS_data_table,file="X22_FOOD_STAMPS_data_table.rdata")
# save(X23_EMPLOYMENT_STATUS_data_table,file="X23_EMPLOYMENT_STATUS_data_table.rdata")
# save(X24_INDUSTRY_OCCUPATION_data_table,file="X24_INDUSTRY_OCCUPATION_data_table.rdata")
# save(X25_HOUSING_CHARACTERISTICS_data_table,file="X25_HOUSING_CHARACTERISTICS_data_table.rdata")
# save(X27_HEALTH_INSURANCE_data_table,file="X27_HEALTH_INSURANCE_data_table.rdata")
# save(X99_IMPUTATION_data_table,file="X99_IMPUTATION_data_table.rdata")
# save(ACS_2016_5YR_BG_data_table,file="ACS_2016_5YR_BG_data_table.rdata")
# 
# 
# # load data tables
# load(file="BG_METADATA_2016_data_table.rdata")
# load(file="X00_COUNTS_data_table.rdata")
# load(file="X01_AGE_AND_SEX_data_table.rdata")
# load(file="X02_RACE_data_table.rdata")
# load(file="X03_HISPANC_OR_LATINO_ORIGIN_data_table.rdata")
# load(file="X07_MIGRATION_data_table.rdata")
# load(file="X08_COMMUTING_data_table.rdata")
# load(file="X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table.rdata")
# load(file="X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table.rdata")
# load(file="X12_MARITAL_STATUS_AND_HISTORY_data_table.rdata")
# load(file="X14_SCHOOL_ENROLLMENT_data_table.rdata")
# load(file="X15_EDUCATIONAL_ATTAINMENT_data_table.rdata")
# load(file="X16_LANGUAGE_SPOKEN_AT_HOME_data_table.rdata")
# load(file="X17_POVERTY_data_table.rdata")
# load(file="X19_INCOME_data_table.rdata")
# load(file="X20_EARNINGS_data_table.rdata")
# load(file="X21_VETERAN_STATUS_data_table.rdata")
# load(file="X22_FOOD_STAMPS_data_table.rdata")
# load(file="X23_EMPLOYMENT_STATUS_data_table.rdata")
# load(file="X24_INDUSTRY_OCCUPATION_data_table.rdata")
# load(file="X25_HOUSING_CHARACTERISTICS_data_table.rdata")
# load(file="X27_HEALTH_INSURANCE_data_table.rdata")
# load(file="X99_IMPUTATION_data_table.rdata")
# load(file="ACS_2016_5YR_BG_data_table.rdata")
#
# 
# # merge data tables if have infinite memory
# # merge_data_tables<-function(x, y) x[y, on = "GEOID"]
# # ACS_all_data_tables<-Reduce(merge_data_tables,list(X00_COUNTS_data_table,X01_AGE_AND_SEX_data_table,X02_RACE_data_table,X03_HISPANC_OR_LATINO_ORIGIN_data_table,X07_MIGRATION_data_table,X08_COMMUTING_data_table,X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table,X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table,X12_MARITAL_STATUS_AND_HISTORY_data_table,X14_SCHOOL_ENROLLMENT_data_table,X15_EDUCATIONAL_ATTAINMENT_data_table,X16_LANGUAGE_SPOKEN_AT_HOME_data_table,X17_POVERTY_data_table,X19_INCOME_data_table,X20_EARNINGS_data_table,X21_VETERAN_STATUS_data_table,X22_FOOD_STAMPS_data_table,X23_EMPLOYMENT_STATUS_data_table,X24_INDUSTRY_OCCUPATION_data_table,X25_HOUSING_CHARACTERISTICS_data_table,X27_HEALTH_INSURANCE_data_table,X99_IMPUTATION_data_table))
# 
#
# merge data tables individually to watch memory usage
# dtm1 <- merge(X00_COUNTS_data_table,X01_AGE_AND_SEX_data_table)
# dtm2 <- merge(dtm1,X02_RACE_data_table)
# dtm3 <- merge(dtm2,X03_HISPANC_OR_LATINO_ORIGIN_data_table)
# dtm4 <- merge(dtm3,X07_MIGRATION_data_table)
# dtm5 <- merge(dtm4,X08_COMMUTING_data_table)
# dtm6 <- merge(dtm5,X09_CHILDREN_HOUSEHOLD_RELATIONSHIP_data_table)
# dtm7 <- merge(dtm6,X11_HOUSEHOLD_FAMILY_SUBFAMILIES_data_table)
# dtm8 <- merge(dtm7,X12_MARITAL_STATUS_AND_HISTORY_data_table)
# dtm9 <- merge(dtm8,X14_SCHOOL_ENROLLMENT_data_table)
# dtm10 <- merge(dtm9,X15_EDUCATIONAL_ATTAINMENT_data_table)
# dtm11 <- merge(dtm10,X16_LANGUAGE_SPOKEN_AT_HOME_data_table)
# dtm12 <- merge(dtm11,X17_POVERTY_data_table)
# dtm13 <- merge(dtm12,X19_INCOME_data_table)
# dtm14 <- merge(dtm13,X20_EARNINGS_data_table)
# dtm15 <- merge(dtm14,X21_VETERAN_STATUS_data_table)
# dtm16 <- merge(dtm15,X22_FOOD_STAMPS_data_table)
# dtm17 <- merge(dtm16,X23_EMPLOYMENT_STATUS_data_table)
# dtm18 <- merge(dtm17,X24_INDUSTRY_OCCUPATION_data_table)
# dtm19 <- merge(dtm18,X25_HOUSING_CHARACTERISTICS_data_table)
# dtm20 <- merge(dtm19,X27_HEALTH_INSURANCE_data_table)
# dtm21 <- merge(dtm20,X99_IMPUTATION_data_table)
#
#
#
# # fix GEOID_Data in ACS_2016_5YR_BG_data_table to match GEOID in rest of files
#
# # copy to new table to preserve old
# ACS_2016_5YR_BG_data_table_synced<-ACS_2016_5YR_BG_data_table
# 
# # convert factos with different levels to characters
# ACS_2016_5YR_BG_data_table_synced$GEOID_Data<-as.character(ACS_2016_5YR_BG_data_table_synced$GEOID_Data)
# 
# # preserve the old GEOID column for posterity
# ACS_2016_5YR_BG_data_table_synced$GEOID_simple<-ACS_2016_5YR_BG_data_table_synced$GEOID
# 
# # make the GEOID column of ACS_2016_5YR_BG match the GEOID column for the rest of the data
# ACS_2016_5YR_BG_data_table_synced$GEOID<-ACS_2016_5YuR_BG_data_table_synced$GEOID_Data
# 
# # check type
# str(ACS_2016_5YR_BG_data_table_synced$GEOID)
# 
# # do the merge
# dtm22<-merge(dtm21,ACS_2016_5YR_BG_data_table_synced,by="GEOID")
# 
# # final full ACS data file saved here in R format (12.7 GB)
# save(all_ACS_2016_data,file="all_ACS_2016_data.rdata")
#
# # final full ACS data file saved here in portable CSV format
# write.csv(all_ACS_2016_data,file="all_ACS_2016_data.csv")
#
# # save data key in R format under original and new names
# save(BG_METADATA_2016_data_table,file="BG_METADATA_2016_data_table")
# save(BG_METADATA_2016_data_table,file="ACS_metadata.rdata")
# # write data key to portable CSV format with the new name
# write.csv(BG_METADATA_2016_data_table,file="ACS_metadata.csv")
#
# # load final data without regenerating
# load(file="all_ACS_2016_data.rdata")
# load(file="BG_METADATA_2016_data_table.rdata")
# 
# # View variable names
# View(BG_METADATA_2016_data_table)
# 
# #### pull ACS data I need ####
# total_pop<-all_ACS_2016_data[,c("GEOID","B01001e1")]
# sex_cat<-all_ACS_2016_data[,c("GEOID","B01001e2","B01001e26")]
# sex_age_cat<-all_ACS_2016_data[,c("GEOID","B01001e3","B01001e4","B01001e5","B01001e6","B01001e7","B01001e8","B01001e9","B01001e10","B01001e11","B01001e12","B01001e13","B01001e14","B01001e15","B01001e16","B01001e17","B01001e18","B01001e19","B01001e20","B01001e21","B01001e22","B01001e23","B01001e24","B01001e25","B01001e27","B01001e28","B01001e29","B01001e30","B01001e31","B01001e32","B01001e33","B01001e34","B01001e35","B01001e36","B01001e37","B01001e38","B01001e39","B01001e40","B01001e41","B01001e42","B01001e43","B01001e44","B01001e45","B01001e46","B01001e47","B01001e48","B01001e49")]
# age_median<-all_ACS_2016_data[,c("GEOID","B01002e1","B01002e2","B01002e3")]
# race_cat<-all_ACS_2016_data[,c("GEOID","B03002e1","B03002e2","B03002e3","B03002e4","B03002e5","B03002e6","B03002e7","B03002e8","B03002e9","B03002e10","B03002e11","B03002e12","B03002e13","B03002e14","B03002e15","B03002e16","B03002e17","B03002e18","B03002e19","B03002e20","B03002e21")]
# education_cat<-all_ACS_2016_data[,c("GEOID","B15003e1","B15003e2","B15003e3","B15003e4","B15003e5","B15003e6","B15003e7","B15003e8","B15003e9","B15003e10","B15003e11","B15003e12","B15003e13","B15003e14","B15003e15","B15003e16","B15003e17","B15003e18","B15003e19","B15003e20","B15003e21","B15003e22","B15003e23","B15003e24","B15003e25")]
# poverty_ratio_cat<-all_ACS_2016_data[,c("GEOID","C17002e1","C17002e2","C17002e3","C17002e4","C17002e5","C17002e6","C17002e7","C17002e8")]
# income_house_cat<-all_ACS_2016_data[,c("GEOID","B19001e1","B19001e2","B19001e3","B19001e4","B19001e5","B19001e6","B19001e7","B19001e8","B19001e9","B19001e10","B19001e11","B19001e12","B19001e13","B19001e14","B19001e15","B19001e16","B19001e17")]
# income_house_median<-all_ACS_2016_data[,c("GEOID","B19013e1")]
# earnings_sex_cat<-all_ACS_2016_data[,c("GEOID","B20001e1","B20001e2","B20001e3","B20001e4","B20001e5","B20001e6","B20001e7","B20001e8","B20001e9","B20001e10","B20001e11","B20001e12","B20001e13","B20001e14","B20001e15","B20001e16","B20001e17","B20001e18","B20001e19","B20001e20","B20001e21","B20001e22","B20001e23","B20001e24","B20001e25","B20001e26","B20001e27","B20001e28","B20001e29","B20001e30","B20001e31","B20001e32","B20001e33","B20001e34","B20001e35","B20001e36","B20001e37","B20001e38","B20001e39","B20001e40","B20001e41","B20001e42","B20001e43")]
# earnings_median<-all_ACS_2016_data[,c("GEOID","B20002e1","B20002e2","B20002e3")]
# employment_cat<-all_ACS_2016_data[,c("GEOID","B23025e1","B23025e2","B23025e3","B23025e4","B23025e5","B23025e6","B23025e7")]
# home_value_cat<-all_ACS_2016_data[,c("GEOID","B25075e1","B25075e2","B25075e3","B25075e4","B25075e5","B25075e6","B25075e7","B25075e8","B25075e9","B25075e10","B25075e11","B25075e12","B25075e13","B25075e14","B25075e15","B25075e16","B25075e17","B25075e18","B25075e19","B25075e20","B25075e21","B25075e22","B25075e23","B25075e24","B25075e25","B25075e26","B25075e27")]
# health_insurance_cat<-all_ACS_2016_data[,c("GEOID","B27010e1","B27010e2","B27010e3","B27010e4","B27010e5","B27010e6","B27010e7","B27010e8","B27010e9","B27010e10","B27010e11","B27010e12","B27010e13","B27010e14","B27010e15","B27010e16","B27010e17","B27010e18","B27010e19","B27010e20","B27010e21","B27010e22","B27010e23","B27010e24","B27010e25","B27010e27","B27010e28","B27010e29","B27010e30","B27010e31","B27010e32","B27010e33","B27010e34","B27010e35","B27010e36","B27010e37","B27010e38","B27010e39","B27010e40","B27010e41","B27010e42","B27010e43","B27010e44","B27010e45","B27010e46","B27010e47","B27010e48","B27010e49","B27010e50","B27010e51","B27010e52","B27010e53","B27010e54","B27010e55","B27010e56","B27010e57","B27010e58","B27010e59","B27010e60","B27010e61","B27010e62","B27010e63","B27010e64","B27010e65","B27010e66")]
# home_value_median<-all_ACS_2016_data[,c("GEOID","B25077e1")]
# geography_area<-all_ACS_2016_data[,c("GEOID","STATEFP","COUNTYFP","TRACTCE","BLKGRPCE","NAMELSAD","MTFCC","FUNCSTAT","ALAND","AWATER","INTPTLAT","INTPTLON","Shape_Length","Shape_Area","GEOID_simple")]
# 
# merge all of the above together
# ftdm1<-merge(total_pop,sex_cat)
# ftdm2<-merge(ftdm1,sex_age_cat)
# ftdm3<-merge(ftdm2,age_median)
# ftdm4<-merge(ftdm3,race_cat)
# ftdm5<-merge(ftdm4,education_cat)
# ftdm6<-merge(ftdm5,poverty_ratio_cat)
# ftdm7<-merge(ftdm6,income_house_cat)
# ftdm8<-merge(ftdm7,income_house_median)
# ftdm9<-merge(ftdm8,earnings_sex_cat)
# ftdm10<-merge(ftdm9,earnings_median)
# ftdm11<-merge(ftdm10,employment_cat)
# ftdm12<-merge(ftdm11,home_value_cat)
# ftdm13<-merge(ftdm12,home_value_median)
# ftdm14<-merge(ftdm13,health_insurance_cat)
# select_ACS_data<-merge(ftdm14,geography_area)
#  
# # # View resulting dataframe with selected ACS variables
# # View(select_ACS_data)
# 
# # save select_ACS_data
# save(select_ACS_data,file="select_ACS_data.rdata")
#
# # write selected variables to portable CSV format
#
#
# load selected variables
# load(file="select_ACS_data.rdata")
# 
#
#### making variables ####
# # making variables
# # from Silva: median household income (dollars), median household value (dollars), percent of population identifying as White only, population density (population per square mile), percent of population with a high school education/GED or higher, and population median age
# 
# # make set to modify
# ACS_constructed_variables<-select_ACS_data
# 
# # median household inome (renaming)
# ACS_constructed_variables$median_household_income_B19013<-ACS_constructed_variables$B19013e1
# 
# # median household value
# ACS_constructed_variables$median_household_value_B25077<-ACS_constructed_variables$B25077e1
# 
# # percent white
# ACS_constructed_variables$percent_white_B03002<-ACS_constructed_variables$B03002e3/ACS_constructed_variables$B03002e1
# 
# # population density (people per square kilometer, ALAND in square meters)
# ACS_constructed_variables$population_density_B01001_ALAND<-(1000000*ACS_constructed_variables$B01001e1/ACS_constructed_variables$ALAND)
# 
# # percent of population with high school education or more
# ACS_constructed_variables$percent_high_school_plus_B15003<-(
#   (ACS_constructed_variables$B15003e17+ACS_constructed_variables$B15003e18+ACS_constructed_variables$B15003e19+ACS_constructed_variables$B15003e20+ACS_constructed_variables$B15003e21+ACS_constructed_variables$B15003e22+ACS_constructed_variables$B15003e23+ACS_constructed_variables$B15003e24+ACS_constructed_variables$B15003e25)
#   /ACS_constructed_variables$B15003e1)
# 
# # median age
# ACS_constructed_variables$median_age_B01002<-ACS_constructed_variables$B01002e1
# 
# # save constructed variables
# save(ACS_constructed_variables,file="ACS_constructed_variables.rdata")
# 
# # make constructed variables into .csc
# write.csv(ACS_constructed_variables,file="ACS_constructed_variables.csv")
# 
# # load constructed variables to avoid the above
# load(file="ACS_constructed_variables.rdata")
# 
# 
# 
#### well data joining to census block groups ####
# # read and view well data from Drillinginfo
# fullrawwelldata<-read.csv("fullrawwelldatatest.CSV")
# View(fullrawwelldata)
# 
# # save raw well data to disk in R format
# save(fullrawwelldata,file="fullrawwelldata.rdata")
# 
# # load full raw well data
# load(file="fullrawwelldata.rdata")
# 
# # rename well data to not horrid format
# modifiedfullwelldata<-fullrawwelldata
# 
# # convert API14 to character
# modifiedfullwelldata$API14<-as.character(modifiedfullwelldata$API14)
# View(modifiedfullwelldata)
# 
# # remove quotes from API14s with leading zeroes
# modifiedfullwelldata$API14 <- gsub("\"", "", modifiedfullwelldata$API14)
# 
# # remove equals sign from API14s with leading zeroes
# modifiedfullwelldata$API14<-gsub("=","",modifiedfullwelldata$API14)
# View(modifiedfullwelldata)
# 
# # add index column just in case
# modifiedfullwelldata$rowID <- seq.int(nrow(modifiedfullwelldata))
# View(modifiedfullwelldata)
# 
# # move index column to the start
# fullwelldatawithID <- modifiedfullwelldata %>%
#   select(rowID, everything())
# View(fullwelldatawithID)
# 
# # # count unique wells
# length(unique(fullwelldatawithID$API14))
# 
# # save version with ID to file!!!
# save(fullwelldatawithID,file="fullwelldatawithID.rdata")
# load(file="fullwelldatawithID.rdata")
# 
# # excerpting only pieces needed for the join
# welldataforjoin<-fullwelldatawithID[c("rowID","API14","Surface.Hole.Longitude..WGS84.","Surface.Hole.Latitude..WGS84.")]
# 
# # rename columns so ArcGIS doesn't explode
# names(welldataforjoin)[names(welldataforjoin) == 'Surface.Hole.Longitude..WGS84.'] <- 'SHLong'
# names(welldataforjoin)[names(welldataforjoin) == 'Surface.Hole.Latitude..WGS84.'] <- 'SHLat'
# 
# View(welldataforjoin)
# 
# # write data for join to file
# write.csv(welldataforjoin,"welldataforjoin.csv",row.names=FALSE)
# 
# # after sending welldataforjoin to ArcGIS to join to census block groups, read that file back into R
# wellBGjoin<-read.csv("WellDataBGsJoin20180325.txt")
# # save file
# save(wellBGjoin,file="wellBGjoin.rdata")
# 
# # load well data joined to census block groups
# load(file="wellBGjoin.rdata")
# 
# View(wellBGjoin)
# 
# # make new file to edit from wellACSjoin
# wellBGjoinedited<-wellBGjoin
# 
# # rename rowID from the horrors ArcGIS committed
# names(wellBGjoinedited)[names(wellBGjoinedited) == 'rowID_'] <- 'rowID'
# View(wellBGjoinedited)
# 
# # join well data back with spatial join block group data
# wellsandBGs<-merge(x = fullwelldatawithID, y = wellBGjoinedited, by = "rowID", all.x = TRUE)
# View(wellsandBGs)
# 
# # save wells and block groups to disk
# save(wellsandBGs,file="wellsandBGs.rdata")
# 
# # load wells and block groups if needed
# load(file="wellsandBGs.rdata")
# View(wellsandBGs)
# 
# 
# 
# #### well data cleaning ####
# 
# # read well type data
# well_totals_by_type<-read.csv(file="WellTypesDrillinginfo2018-03-08.txt",header = FALSE)
# View(well_totals_by_type)
# sum(well_totals_by_type$V2)
# 
# # make dataset for cleaning
# cleanwellsandBGs<-wellsandBGs
# View(cleanwellsandBGs)
# 
# # remove the API column ArcGIS destroyed
# cleanwellsandBGs$API14.y<-NULL
# View(cleanwellsandBGs)
# 
# # rename API14.x back to API14
# cleanwellsandBGs$API14<-cleanwellsandBGs$API14.x
# cleanwellsandBGs$API14.x<-NULL
# 
# # find duplicated APIs
# # view rows with duplicated APIs
# duplicateAPI<-cleanwellsandBGs[duplicated(cleanwellsandBGs$API14), ]
# View(duplicateAPI[100:104])
# 
# # view rows with duplicated APIs manually
# duplicateAPIs<-subset(cleanwellsandBGs,API14=="47039003780000")
# View(duplicateAPIs[101:104])
# 
# # figure out which block group has other wells
# wellswithduplicate<-subset(cleanwellsandBGs,GEOID=="540390108021" | GEOID=="540390108022")
# View(wellswithduplicate)
# 
# # delete duplicate well in row 22
# cleanwellsandBGs<-cleanwellsandBGs[!cleanwellsandBGs$GEOID == "540390108022", ]
# 
# # add row IDs, I think?
# cleanwellsandBGs <- cleanwellsandBGs %>%
#    select(rowID, everything())
# 
# # view wells with duplicated longitudes and latitudes
# duplonglat<-cleanwellsandBGs[duplicated(cleanwellsandBGs[,c("Surface.Hole.Longitude..WGS84.","Surface.Hole.Latitude..WGS84.")]),]
# View(duplonglat)
# 
# table(cleanwellsandBGs$Production.Type,cleanwellsandBGs$State)
# 
# # pull only disposal, injection, and saltwater injection wells
# DISWI_wells<-subset(cleanwellsandBGs,Production.Type=="DISPOSAL" | Production.Type=="SALTWATER INJECTION" | Production.Type=="INJECTION")
# DISWI_wells$Production.Type<-factor(DISWI_wells$Production.Type)
# View(DISWI_wells)
# 
# #### adding 
# 
# 
# 
# 
# 
# # view well statuses
# levels(DISWI_wells$Well.Status)
# TAwells<-subset(DISWI_wells,Well.Status=="TA")
# View(TAwells)
# cancelledwells<-subset(DISWI_wells,Well.Status=="CANCELLED")
# View(cancelledwells)
# 
# # table of well statuses by well type
# wellstatusesandtypes<-table(DISWI_wells$Production.Type,DISWI_wells$Well.Status)
# View(wellstatusesandtypes)
# 
# # remove cancelled wells and expired permits
# wellsnocancelledorexpired<-DISWI_wells[!(DISWI_wells$Well.Status %in% c("CANCELLED","EXPIRED PERMIT")),]
# 
# View(wellsnocancelledorexpired)
# table(wellsnocancelledorexpired$Well.Status)
# save(wellsnocancelledorexpired,file="wellsnocanceledorexpired.rdata")
# write.csv(wellsnocancelledorexpired,file="cleaned_wells_as_of_2018-04-05.csv")
# 
# # make table of wells by production and state
# wellsbyproductionandstate<-table(wellsnocancelledorexpired$Production.Type,wellsnocancelledorexpired$State)
# View(wellsbyproductionandstate)
# 
# # make table of wells by state
# wellsbystate<-table(wellsnocancelledorexpired$State)
# View(wellsbystate)
# write.csv(wellsbystate,file="wellsbystate.csv")
# write.csv(wellsbyproductionandstate,file="wellsbyproductionandstate.csv")

#### ran to here 2018-04-05 ####


















  










#merge select ACS data with spatial join data
fulldata<-merge(x=ACSwithfullwells,y=selectACSdata,by="GEOID",all.x = TRUE,all.y = TRUE)
save(fulldata,file = "fulldata.rdata")
write.csv(fulldata,file="WolfMPHProjectFullDataset2018.csv")
View(fulldata)

# load(fulldata.rdata)
basicwellcounts<-as.data.frame(table(fulldata$Production.Type,fulldata$State))
View(subset(basicwellcounts,basicwellcounts$Var1=="DISPOSAL"))
exporttableforslide1<-subset(basicwellcounts,basicwellcounts$Var1=="DISPOSAL")
write.csv(exporttableforslide1)

onlywithwells<-subset(fulldata,fulldata$Production.Type %in% c("DISPOSAL","SALTWATER INJECTION"))
View(onlywithwells)

noduplicates<-subset(onlywithwells, !duplicated(onlywithwells$GEOID))
View(noduplicates)

mean(noduplicates$B20002e1,na.rm = TRUE)
str(noduplicates$B20002e1)

selectdataformapping<-onlywithwells[1:110]
View(selectdataformapping)
write.csv(selectdataformapping,file="selectdataformapping.csv")
write.csv(onlywithwells,file="WolfWellDataForMapping2018.csv")
colnames(onlywithwells)


#race
# % white
sum(noduplicates$B03002e3,na.rm=TRUE)
sum(noduplicates$B03002e1,na.rm=TRUE)
# % African American
sum(noduplicates$B03002e4,na.rm=TRUE)/sum(noduplicates$B03002e1,na.rm=TRUE)


38785726/316515021
54232205/316525021

# % Hispanic/Latino
sum(noduplicates$B03002e12,na.rm=TRUE)/sum(noduplicates$B03002e1,na.rm=TRUE)
sum(noduplicates$B)

# % sum(noduplicates$B03002e3,na.rm=TRUE)/sum(noduplicates$B03002e1,na.rm=TRUE)

colnames(noduplicates)

unique(noduplicates$STATEFP)

#population
mean(noduplicates$B01001e1,na.rm=TRUE)
mean(selectACSdata$B01001e1,na.rm=TRUE)

#educational attainment
sum(noduplicates$B15003e1,na.rm=TRUE)
sum(noduplicates$B15003e17,na.rm=TRUE)+sum(noduplicates$B15003e18,na.rm=TRUE)+sum(noduplicates$B15003e19,na.rm=TRUE)+sum(noduplicates$B15003e20,na.rm=TRUE)+sum(noduplicates$B15003e21,na.rm=TRUE)+sum(noduplicates$B15003e22,na.rm=TRUE)+sum(noduplicates$B15003e23,na.rm=TRUE)+sum(noduplicates$B15003e24,na.rm=TRUE)+sum(noduplicates$B15003e25,na.rm=TRUE)


colnames(noduplicates)


sum(is.na(wellACSjoin$GEOID))




#### where the hell does this go?!

# finaldata<-read.csv('WolfMPHProjectFullDataset.csv')

# table(finaldata$Production.Type,finaldata$State)


# 
# 
# 
# sum(is.na(wellACSjoin$GEOID))
# 
# 
# 
# 
# load(file="wellACSjoin.rdata")
# save(ACSdata,file="ACSdata.rdata")
# 
# 
# #permitdata<-read.csv("permitdatabase20170824.csv")
# 
# #View(permitdata)
# 
# #length(unique(permitdata$Permit.Number))
# 
# #permitdatadups<-permitdata[duplicated(permitdata$Permit.Number) | duplicated(permitdata$Permit.Number, fromLast=TRUE),]
# #View(permitdatadups)
# 
# 
# 
# 
# 
# #productiondata<-read.csv("productiondatabase20170824.csv")
#   
# 
#   
#   
# #productiondata<-
# 
# 
# #permitdataforjoin<-perm
# 
# 
# 
# 
# #######OLD CODE
# 
# #import raw permit data 2017-07-06 (Permits, then Well Type of Disposal, Injection, or Saltwater Injection)
# #permitdata<-read.csv("PermitsThenWellTypeDisposalInjectionSWIn.csv", header = TRUE)
# #View(permitdata)
# 
# 
# #count wells per state
# #wellsperstate<-permitdata %>% group_by(State.Province) %>% summarize(count=n())
# #View(wellsperstate)
# 
# 
# #count wells per state per type
# #wellsperstatepertype<-permitdata %>% group_by(State.Province, Well.Type) %>% summarize(count=n())
# #View(wellsperstatepertype)
# 
# 
# #write.table(wellsperstate, file="wellsperstate.csv",sep=",",row.names=F)
# #write.table(wellsperstatepertype, file="wellsperstatepertype.csv",sep=",",row.names=F)
# 
# 
# 
# #import raw permit data 2017-07-06 (Permits, then Well Type of Disposal, Injection, or Saltwater Injection)
# #permitdataTX<-read.csv("TexasMiniDataPermits.csv", header = TRUE)
# #View(permitdataTX)
# 
# 
# 
# 
# #count wells per state per type
# #wellsperstatepertypeTX<-permitdataTX %>% group_by(Well.Type) %>% summarize(count=n())
# #View(wellsperstatepertypeTX)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #TEST census data -- too much memory edition
# 
# # test make a data table for each sequence-state combination, this works but makes one big list
# setwd("C:/Rfiles/MPHProject/workingdata/acsdata/")
# 
# #trying to make sequence lists
# #make test list of sequences
# testfixwidthnoofseq<-c("0001","0002","0003")
# testfixwidthnoofseq
# str(testfixwidthnoofseq)
# 
# #make test list of states
# testlistofstates<-c("ak","al","az")
# 
# #use master list method
# masterlist<-list()
# for(j in testfixwidthnoofseq) {
#    for(i in testlistofstates) {
#        inname <- paste("e20155",i,j,"000.txt",sep="")
#        outname <- paste("seq",j,i,sep="")
#        inread <- read.csv(inname,header=FALSE,sep=",")
#        assign(outname,inread)
#        masterlist[[outname]] <- inread
#    }
# }
# 
# #make a separate list and data frame for each sequence -- final dataframes are "seqXXXX" where XXXX is the sequence number
# for (j in testfixwidthnoofseq){
#   nameofsequence<-paste("listseq",j,sep="")
#   nameofoutgoingdataframe<-paste("seq",j,sep="")
#   interimlist<-masterlist[grep(j, names(masterlist))]
#   assign(nameofsequence,interimlist)
#   interimdataframe <- do.call(rbind, lapply(interimlist, data.frame, stringsAsFactors=FALSE))
#   assign(nameofoutgoingdataframe,interimdataframe)
# }
# 
# #list of names of sequences
# listofnamesofsequencedataframes<-list()
# for (j in testfixwidthnoofseq){
#   addseqtonumber<-paste("seq",j,sep="")
#   listofnamesofsequencedataframes<-c(listofnamesofsequencedataframes,addseqtonumber)
# }
# listofnamesofsequencedataframes
# str(listofnamesofsequencedataframes)
# 
# #goal is one big sheet, rows are block groups, columns are all variables
# 
# #get headers from sequence file
# 
# testnumberofsequences<-1:3
# 
# #get header file read in and labeled
# 
# #convert xls to csv
# library(rio)
# # Create a vector of Excel files to read
# xls <- dir(pattern = "xls")
# created <- mapply(convert, xls, gsub("xls", "csv", xls))
# 
# #make header files into dataframes and make a list of the names of the header files
# listofheadernames<-list()
# for (k in testnumberofsequences){
#   kwithzeroes<-formatC(k, width = 4, format = "d", flag = "0")
#   headerinname<-paste("Seq",k,".csv",sep="")
#   headeroutname<-paste("seq",kwithzeroes,"headers",sep="")
#   headerinread<-read.csv(headerinname,header = TRUE)
#   listofheadernames<-c(listofheadernames,headeroutname)
#   assign(headeroutname,headerinread)
# }
# 
# #take each header file and assign its headers to the actual sequence file
# listoftitledsequencenames<-list()
# listoftitledsequences<-list()
# for (s in listofnamesofsequencedataframes){
#   sequenceheadername<-paste(s,"headers",sep="")
#   titledsequenceoutname<-paste(s,"titled",sep="")
#   pleasebesequenceheader<-get(sequenceheadername)
#   temporaryheaders<-colnames(pleasebesequenceheader)
#   pleasebesequencedataframe<-get(s)
#   rownames(pleasebesequencedataframe)<-NULL
#   colnames(pleasebesequencedataframe)<-temporaryheaders
#   listoftitledsequencenames<-c(listoftitledsequencenames,titledsequenceoutname)
#   assign(titledsequenceoutname,pleasebesequencedataframe)
# }
# 
# #put titled dataframes in a list!
# titledlistofdataframes = mget(ls(pattern = "seq.*titled"))
# View(titledlistofdataframes)
# 
# testacsdatablockgroup <- Reduce(function(x, y) merge(x, y, by=c("FILEID","FILETYPE","STUSAB","CHARITER","LOGRECNO")), titledlistofdataframes)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #TEST edition, trying to reduce memory usage
# 
# # test make a data table for each sequence-state combination, this works but makes one big list
# setwd("C:/Rfiles/MPHProject/workingdata/acsdata/")
# 
# #trying to make sequence lists
# #make test list of sequences
# testfixwidthnoofseq<-c("0001","0002","0003")
# testfixwidthnoofseq
# str(testfixwidthnoofseq)
# 
# #make test list of states
# testlistofstates<-c("ak","al","ar","az")
# 
# #for whole dataset estimates only
# #input_filenames<-list.files(pattern = "e20155.*000\\.txt")
# #masterlist<-lapply(input_filenames,read.csv)
# 
# #to make sample with same master list structure
# #input_filenames<-list.files(pattern = "e20155a.000[1-3]000\\.txt")
# #masterlist<-lapply(input_filenames,read.csv)
# #input_filenames
# 
# #what if I never make the master list?  just little lists for each sequence?
# 
# #make a data frame for each sequence
# listofsequences<-list()
# for (i in testfixwidthnoofseq){
#   input_all_state_filenames<-paste0("e20155",testlistofstates,i,"000.txt")
#   master_all_state_list<-lapply(input_all_state_filenames,read.csv,header=FALSE)
#   names(master_all_state_list) <- gsub("\\.txt$", "", input_all_state_filenames)
#   interimdataframe <- do.call(rbind, lapply(master_all_state_list, data.frame, stringsAsFactors=FALSE))
#   listofsequences[[i]]<-interimdataframe
#   rm(interimdataframe)
# }
# names(master_all_state_list)
# View(master_all_state_list)
# View(listofsequences[[0001]])
# 
# 
# 
# 
# 
# 
# 
# #trying to use big memory options
# 
# setwd("C:/Rfiles/MPHProject/workingdata/acsdata/")
# 
# library(plyr)
# library(dplyr)
# library(data.table)
# library(ggplot2)
# library(acs)
# library(ff)
# library(bigmemory)
# library(ffbase)
# 
# sequenceindex<-1:122
# fixedwidthsequenceindexff<-formatC(sequenceindex, width = 4, format = "d", flag = "0")
# listofstates<-c('ak', 'al', 'ar', 'az', 'ca', 'co', 'ct', 'dc', 'de', 'fl', 'ga', 'hi', 'ia', 'id', 'il', 'in', 'ks', 'ky', 'la', 'ma', 'md', 'me', 'mi', 'mn', 'mo', 'ms', 'mt', 'nc', 'nd', 'ne', 'nh', 'nj', 'nm', 'nv', 'ny', 'oh', 'ok', 'or', 'pa', 'ri', 'sc', 'sd', 'tn', 'tx', 'ut', 'va', 'vt', 'wa', 'wi', 'wv', 'wy','dc','pr')
# 
# 
# #TESTING BIG DATA
# master_list_of_ffdfs<-list()
# for (i in seq_along(input_filenames)){
#   master_list_of_ffdfs[[i]]<-read.csv.ffdf(file=input_filenames[i],header=FALSE)
# }
# names(master_list_of_ffdfs) <- gsub("\\.txt$", "", input_filenames)
# View(master_list_of_ffdfs$e20155ak0001000)
# str(master_list_of_ffdfs)
# #works to here
# 
# 
# #another option for file input -- testing with other data
# input_filenames<-list.files(pattern = "e20155o.*000.*000\\.txt") #change back for whole dataset
# input_filenames
# 
# master_list_of_ffs<-lapply(input_filenames,function(i){
#   read.csv.ffdf(file=i, header=FALSE)
# })
# #started 6:46 for 27 files, ended 6:49
# 
# start.time <- Sys.time()
# input_filenames<-list.files(pattern = "e20155o.*000.*000\\.txt") #change back for whole dataset
# input_filenames
# 
# master_list_of_ffs<-lapply(input_filenames,function(i){
#   read.csv.ffdf(file=i, header=FALSE)
# })
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# 
# 
# #trying fread
# start.time <- Sys.time()
# master_list_of_ffs<-lapply(input_filenames,function(i){
#   fread(file=i, header=FALSE)
# })
# 
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.takenmes(master_list_of_ffdfs) <- gsub("\\.txt$", "", input_filenames)
# View(master_list_of_ffdfs$e20155ak0001000)
# #triedwithfulldatatohere
# 
# 
# 
# 
# listofsequenceffdfs<-list()
# testfixwidthnoofseq<-c("0001","0002","0003","0004","0005","0006","0007","0008","0009")
# 
# #for (j in testfixwidthnoofseq){
# #  interimlistoffdfnames<-master_list_of_ffdfs[grep(j, names(master_list_of_ffdfs))]
# #  interimlist2<-as.ffdf(interimlistoffdfnames[[1]])
# #  for (i in interimlistoffdfnames){
# #    interimlist2<-ffdfappend(interimlist2, i, adjustvmode=F)
# #  }
# #  listofsequenceffdfs[[j]]<-interimlist2
# #}
# #listofsequenceffdfs
# #View(interimlistoffdfnames[[1]])
# #maybe works to here
# 
# for (j in testfixwidthnoofseq){
#   interimlistofffnames<-master_list_of_ffdfs[grep(j, names(master_list_of_ffdfs))]
#   interimseqff <- Reduce(function(x,y) ffdfappend(x,y, adjustvmode=F), interimlistofffnames)
#   listofsequenceffdfs[[j]]<-interimseqff
# }
# 
# listofsequenceffdfs
# 
# 
# 
# interimlist2
# listofsequenceffdfs
# interimlistoffdfnames
# 
# 
# #listofsequencesff<-list()
# #for (i in fixedwidthsequenceindexff){
# #  input_all_state_filenamesff<-paste0("e20155",listofstates,i,"000.txt")
# #}
# 
# #for (i in 1)
# #  mydata <- read.csv.ffdf(file=input_all_state_filenamesff[i], header=FALSE, VERBOSE=TRUE
# #                          , first.rows=100000, next.rows=100000, colClasses=NA)
# 
# 
# 
# 
# 
# listofsequences<-list()
# for (i in fixedwidthsequenceindex){
#   input_all_state_filenames<-paste0("e20155",listofstates,i,"000.txt")
#   master_all_state_list<-lapply(input_all_state_filenames,read.csv,header=FALSE)
#   names(master_all_state_list) <- gsub("\\.txt$", "", input_all_state_filenames)
#   interimdataframe <- do.call(rbind, lapply(master_all_state_list, data.frame, stringsAsFactors=FALSE))
#   listofsequences[[i]]<-interimdataframe
#   rm(interimdataframe)
# }
# View(listofsequences[[0001]])
# 
# 
# 
# 
# #create a vector of the files that I want to load
# temp = list.files(pattern="*.csv")
# 
# #create the first ffdf object for i = 1, this is necessary to establish the ff dataframe to append the rest
# for (i in 1)
#   mydata <- read.csv.ffdf(file=temp[i], header=FALSE, VERBOSE=TRUE
#                           , first.rows=100000, next.rows=100000, colClasses=NA)
# 
# #loop through the remaining objects
# for (i in 2:length(temp))
#   mydata <- read.csv.ffdf(x = mydata, file=temp[i], header=FALSE, VERBOSE=TRUE
#                           , first.rows=100000, next.rows=100000)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #for realsies
# 
# 
# # setup
# 
# setwd("C:/Rfiles/MPHProject/workingdata/acsdata/")
# 
# library(plyr)
# library(dplyr)
# library(data.table)
# library(ggplot2)
# library(acs)
# library(ff)
# library(bigmemory)
# library(ffbase)
# 
# 
# 
# 
# 
# #make a data frame for each sequence
# 
# sequenceindex<-1:122
# fixedwidthsequenceindex<-formatC(sequenceindex, width = 4, format = "d", flag = "0")
# listofstates<-c('ak', 'al', 'ar', 'az', 'ca', 'co', 'ct', 'dc', 'de', 'fl', 'ga', 'hi', 'ia', 'id', 'il', 'in', 'ks', 'ky', 'la', 'ma', 'md', 'me', 'mi', 'mn', 'mo', 'ms', 'mt', 'nc', 'nd', 'ne', 'nh', 'nj', 'nm', 'nv', 'ny', 'oh', 'ok', 'or', 'pa', 'ri', 'sc', 'sd', 'tn', 'tx', 'ut', 'va', 'vt', 'wa', 'wi', 'wv', 'wy','dc','pr')
# 
# 
# listofsequences<-list()
# for (i in fixedwidthsequenceindex){
#   input_all_state_filenames<-paste0("e20155",listofstates,i,"000.txt")
#   master_all_state_list<-lapply(input_all_state_filenames,read.csv,header=FALSE)
#   names(master_all_state_list) <- gsub("\\.txt$", "", input_all_state_filenames)
#   interimdataframe <- do.call(rbind, lapply(master_all_state_list, data.frame, stringsAsFactors=FALSE))
#   listofsequences[[i]]<-interimdataframe
#   rm(interimdataframe)
# }
# View(listofsequences[[0001]])
# 
# 
# 
# 
# 
# 
# #THIS IS THE PART THAT TAKES ALL THE MEMORY:  make a separate list and data frame for each sequence -- final dataframes are "seqXXXX" where XXXX is the sequence number
# for (j in testfixwidthnoofseq){
#   nameofsequence<-paste("listseq",j,sep="")
#   nameofoutgoingdataframe<-paste("seq",j,sep="")
#   interimlist<-masterlist[grep(j, names(masterlist))]
#   assign(nameofsequence,interimlist)
#   interimdataframe <- do.call(rbind, lapply(interimlist, data.frame, stringsAsFactors=FALSE))
#   assign(nameofoutgoingdataframe,interimdataframe)
# }
# 
# #list of names of sequences
# listofnamesofsequencedataframes<-list()
# for (j in testfixwidthnoofseq){
#   addseqtonumber<-paste("seq",j,sep="")
#   listofnamesofsequencedataframes<-c(listofnamesofsequencedataframes,addseqtonumber)
# }
# listofnamesofsequencedataframes
# str(listofnamesofsequencedataframes)
# 
# #goal is one big sheet, rows are block groups, columns are all variables
# 
# #get headers from sequence file
# 
# testnumberofsequences<-1:3
# 
# #get header file read in and labeled
# 
# #convert xls to csv
# library(rio)
# # Create a vector of Excel files to read
# xls <- dir(pattern = "xls")
# created <- mapply(convert, xls, gsub("xls", "csv", xls))
# 
# #make header files into dataframes and make a list of the names of the header files
# listofheadernames<-list()
# for (k in testnumberofsequences){
#   kwithzeroes<-formatC(k, width = 4, format = "d", flag = "0")
#   headerinname<-paste("Seq",k,".csv",sep="")
#   headeroutname<-paste("seq",kwithzeroes,"headers",sep="")
#   headerinread<-read.csv(headerinname,header = TRUE)
#   listofheadernames<-c(listofheadernames,headeroutname)
#   assign(headeroutname,headerinread)
# }
# 
# #take each header file and assign its headers to the actual sequence file
# listoftitledsequencenames<-list()
# listoftitledsequences<-list()
# for (s in listofnamesofsequencedataframes){
#   sequenceheadername<-paste(s,"headers",sep="")
#   titledsequenceoutname<-paste(s,"titled",sep="")
#   pleasebesequenceheader<-get(sequenceheadername)
#   temporaryheaders<-colnames(pleasebesequenceheader)
#   pleasebesequencedataframe<-get(s)
#   rownames(pleasebesequencedataframe)<-NULL
#   colnames(pleasebesequencedataframe)<-temporaryheaders
#   listoftitledsequencenames<-c(listoftitledsequencenames,titledsequenceoutname)
#   assign(titledsequenceoutname,pleasebesequencedataframe)
# }
# 
# #put titled dataframes in a list!
# titledlistofdataframes = mget(ls(pattern = "seq.*titled"))
# View(titledlistofdataframes)
# 
# testacsdatablockgroup <- Reduce(function(x, y) merge(x, y, by=c("FILEID","FILETYPE","STUSAB","CHARITER","LOGRECNO")), titledlistofdataframes)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #list of names of sequences
# listofnamesofsequencedataframes<-list()
# for (j in fixedwidthnumberofsequences){
#   addseqtonumber<-paste("seq",j,sep="")
#   listofnamesofsequencedataframes<-c(listofnamesofsequencedataframes,addseqtonumber)
# }
# listofnamesofsequencedataframes
# str(listofnamesofsequencedataframes)
# 
# #goal is one big sheet, rows are block groups, columns are all variables
# #get headers from sequence file
# #get header file read in and labeled
# 
# #convert xls to csv
# library(rio)
# # Create a vector of Excel files to read
# xls <- dir(pattern = "xls")
# created <- mapply(convert, xls, gsub("xls", "csv", xls))
# 
# #make header files into dataframes and make a list of the names of the header files
# listofheadernames<-list()
# for (k in numberofsequences){
#   kwithzeroes<-formatC(k, width = 4, format = "d", flag = "0")
#   headerinname<-paste("Seq",k,".csv",sep="")
#   headeroutname<-paste("seq",kwithzeroes,"headers",sep="")
#   headerinread<-read.csv(headerinname,header = TRUE)
#   listofheadernames<-c(listofheadernames,headeroutname)
#   assign(headeroutname,headerinread)
# }
# 
# #take each header file and assign its headers to the actual sequence file
# listoftitledsequencenames<-list()
# listoftitledsequences<-list()
# for (s in listofnamesofsequencedataframes){
#   sequenceheadername<-paste(s,"headers",sep="")
#   titledsequenceoutname<-paste(s,"titled",sep="")
#   pleasebesequenceheader<-get(sequenceheadername)
#   temporaryheaders<-colnames(pleasebesequenceheader)
#   pleasebesequencedataframe<-get(s)
#   rownames(pleasebesequencedataframe)<-NULL
#   colnames(pleasebesequencedataframe)<-temporaryheaders
#   listoftitledsequencenames<-c(listoftitledsequencenames,titledsequenceoutname)
#   assign(titledsequenceoutname,pleasebesequencedataframe)
# }
# 
# #put titled dataframes in a list!
# titledlistofdataframes = mget(ls(pattern = "seq.*titled"))
# View(titledlistofdataframes)
# 
# testacsdatablockgroup <- Reduce(function(x, y) merge(x, y, by=c("FILEID","FILETYPE","STUSAB","CHARITER","LOGRECNO")),titledlistofdataframes)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #download census summary files
# 
# #temp <- tempfile()
# #download.file("https://www2.census.gov/census_2010/04-Summary_File_1/Alabama/al2010.sf1.zip",temp)
# #alabamablockgroups <- read.table(unz(temp, "a1.dat"))
# #unlink(temp)
# 
# 
# 
# 
# #NHGIS data
# setwd("C:/Rfiles/MPHProject/workingdata/")
# NHGISraw<-read.csv("./nhgis0010_csv/nhgis0010_ds172_2010_blck_grp.csv", header = TRUE)
# View(NHGISraw)
