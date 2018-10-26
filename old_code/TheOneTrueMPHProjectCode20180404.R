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
load(file="BG_METADATA_2016_data_table.rdata")
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
# write.csv(select_ACS_data,file="select_ACS_data.csv")
#
#
# load selected variables
load(file="select_ACS_data.rdata")


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

# load constructed variables to avoid the above
load(file="ACS_constructed_variables.rdata")



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

# load wells and block groups if needed
load(file="wellsandBGs.rdata")
View(wellsandBGs)



#### well data cleaning ####

# read well type data
well_totals_by_type<-read.csv(file="WellTypesDrillinginfo2018-03-08.txt",header = FALSE)
View(well_totals_by_type)
sum(well_totals_by_type$V2)

# make dataset for cleaning
cleanwellsandBGs<-wellsandBGs
View(cleanwellsandBGs)

# remove the API column ArcGIS destroyed
cleanwellsandBGs$API14.y<-NULL
View(cleanwellsandBGs)

# rename API14.x back to API14
cleanwellsandBGs$API14<-cleanwellsandBGs$API14.x
cleanwellsandBGs$API14.x<-NULL

# find duplicated APIs
# view rows with duplicated APIs
duplicateAPI<-cleanwellsandBGs[duplicated(cleanwellsandBGs$API14), ]
View(duplicateAPI[100:104])

# view rows with duplicated APIs manually
duplicateAPIs<-subset(cleanwellsandBGs,API14=="47039003780000")
View(duplicateAPIs[101:104])

# figure out which block group has other wells
wellswithduplicate<-subset(cleanwellsandBGs,GEOID=="540390108021" | GEOID=="540390108022")
View(wellswithduplicate)

# delete duplicate well in row 22
cleanwellsandBGs<-cleanwellsandBGs[!cleanwellsandBGs$GEOID == "540390108022", ]

# add row IDs, I think?
cleanwellsandBGs <- cleanwellsandBGs %>%
   select(rowID, everything())

# view wells with duplicated longitudes and latitudes
duplonglat<-cleanwellsandBGs[duplicated(cleanwellsandBGs[,c("Surface.Hole.Longitude..WGS84.","Surface.Hole.Latitude..WGS84.")]),]
View(duplonglat)

table(cleanwellsandBGs$Production.Type,cleanwellsandBGs$State)

# pull only disposal, injection, and saltwater injection wells
DISWI_wells<-subset(cleanwellsandBGs,Production.Type=="DISPOSAL" | Production.Type=="SALTWATER INJECTION" | Production.Type=="INJECTION")
DISWI_wells$Production.Type<-factor(DISWI_wells$Production.Type)
View(DISWI_wells)

#### adding 





# view well statuses
levels(DISWI_wells$Well.Status)
TAwells<-subset(DISWI_wells,Well.Status=="TA")
View(TAwells)
cancelledwells<-subset(DISWI_wells,Well.Status=="CANCELLED")
View(cancelledwells)

# table of well statuses by well type
wellstatusesandtypes<-table(DISWI_wells$Production.Type,DISWI_wells$Well.Status)
View(wellstatusesandtypes)

# remove cancelled wells and expired permits
wellsnocancelledorexpired<-DISWI_wells[!(DISWI_wells$Well.Status %in% c("CANCELLED","EXPIRED PERMIT")),]

View(wellsnocancelledorexpired)
table(wellsnocancelledorexpired$Well.Status)
save(wellsnocancelledorexpired,file="wellsnocanceledorexpired.rdata")
write.csv(wellsnocancelledorexpired,file="cleaned_wells_as_of_2018-04-05.csv")

# make table of wells by production and state
wellsbyproductionandstate<-table(wellsnocancelledorexpired$Production.Type,wellsnocancelledorexpired$State)
View(wellsbyproductionandstate)

# make table of wells by state
wellsbystate<-table(wellsnocancelledorexpired$State)
View(wellsbystate)
write.csv(wellsbystate,file="wellsbystate.csv")
write.csv(wellsbyproductionandstate,file="wellsbyproductionandstate.csv")



#### ran to here 2018-04-05 ####






#### Kansas ####

# # import data
# ks_wells<-read.csv(file="ks_wells.txt", stringsAsFactors = FALSE)
# View(ks_wells)
#  
# # save file as an rdata file
# save(ks_wells,file="ks_wells.rdata")

# load rdata file
load(file="ks_wells.rdata")

# # UIC data
# ks_uic<-read.csv(file="KS_UIC_archive.txt")
# save(ks_uic,file="ks_uic.rdata")
load(file="ks_uic.rdata")
# View(ks_uic)
# kids<-unique(ks_uic$KGS_ID)
# kids
# 
# ks_wells_in_uic_data_only <- subset(ks_clean, KID %in% kids)
# View(ks_wells_in_uic_data_only)
# uic_statuses<-table(ks_wells_in_uic_data_only$STATUS)
# View(uic_statuses)
# write.csv(uic_statuses,file="uic_statuses.csv")


# make version for cleaning
ks_clean<-ks_wells
View(ks_clean)

#### formatting changes ####

# column type fixes

# convert dates to dates

# permit date
ks_clean$permit_as_date<-as.Date(ks_clean$PERMIT, "%d-%b-%Y")

# spud date
ks_clean$spud_as_date<-as.Date(ks_clean$SPUD, "%d-%b-%Y")

# completion date
ks_clean$completion_as_date<-as.Date(ks_clean$COMPLETION, "%d-%b-%Y")

# plugging date
ks_clean$plugging_as_date<-as.Date(ks_clean$PLUGGING, "%d-%b-%Y")

# modified date
ks_clean$modified_as_date<-as.Date(ks_clean$MODIFIED, "%d-%b-%Y")

# make API into a character
ks_clean$API_NUMBER<-as.character(ks_clean$API_NUMBER)

# get possible values of well status
unique(ks_clean$STATUS)
unique(ks_clean$STATUS2)

save(ks_clean,file="ks_clean.rdata")

# # count unique APIs
# length(unique(ks_clean$API_NUMBER))
# nrow(ks_clean)
# 
# # view rows with duplicated APIs
# index <- duplicated(ks_clean$API_NUMBER) | duplicated(ks_clean$API_NUMBER, fromLast = TRUE)
# ks_API_dups<-ks_clean[index, ] 
# View(ks_API_dups)
# 
# # view counts of duplicates per API
# ks_dup_API_counts<-table(ks_API_dups$API_NUMBER)
# View(ks_dup_API_counts)
# 
# # count unique KIDs
# length(unique(ks_clean$KID))
# nrow(ks_clean)
# 
# # view duplicates by KID
# index <- duplicated(ks_clean$API_NUMBER) | duplicated(ks_clean$API_NUMBER, fromLast = TRUE)
# ks_API_dups<-ks_clean[index, ] 
# View(ks_API_dups)
# 
# # view counts of duplicates per API
# ks_dup_API_counts<-table(ks_API_dups$API_NUMBER)
# View(ks_dup_API_counts)
# 
# # overall counts by API
# KS_API_counts<-table(ks_clean$API_NUMBER)
# View(ks_api_counts)
# 
# # counts by well status for rows without APIs
# ks_wells_no_API<-ks_clean[which(ks_clean$API_NUMBER == ""),]
# ks_wells_no_API_by_status<-table(ks_wells_no_API$STATUS)
# View(ks_wells_no_API_by_status)
# 
# # checking ambiguous wells for inclusion or exclusion
# # get counts by well status 1
# ks_well_counts_by_status<-table(ks_clean$STATUS)
# View(ks_well_counts_by_status)
# # write.csv(ks_well_counts_by_status, file="ks_well_counts_by_status.csv")
# 
# # get counts by well status 2
# ks_well_counts_by_status_2<-table(ks_clean$STATUS2)
# View(ks_well_counts_by_status_2)
# write.csv(ks_well_counts_by_status_2, file="ks_well_counts_by_status_2.csv")


#### selecting wells for inclusion ####

load(file="ks_clean.rdata")

# broad inclusion criteria
ks_included_status1s<-c("INJ","INJ-P&A","OTHER()","OTHER(1O&1SWD)","OTHER(CBM/SWD)","OTHER(CLASS ONE (OLD))","OTHER(CLASS1)","OTHER(INJ or EOR)","OTHER(NHDW)","OTHER(NULL)","OTHER(OIL,SWD)","OTHER(OTHER)","OTHER(TA)","OTHER(TEMP ABD)","OTHER-P&A()","OTHER-P&A(CLASS ONE (OLD))","OTHER-P&A(INJ OR )","OTHER-P&A(INJ or EOR)","OTHER-P&A(OIL-SWD)","OTHER-P&A(TA)","SWD","SWD-P&A")

# make list of only included wells
ks_select_wells<-ks_clean[which(ks_clean$STATUS %in% ks_included_status1s),]
View(ks_select_wells)

# view statuses of remaining wells
ks_select_statuses<-table(ks_select_wells$STATUS)
View(ks_select_statuses)



#### figuring out which others I should keep
# make vector of other statuses with little other information
ks_statuses_to_investigate_in_group<-c("OTHER()","OTHER-P&A()","OTHER-P&A(TA)","OTHER(OTHER)","OTHER(NULL)","OTHER(TA)","OTHER(TEMP ABD)")

# select those wells in status vector
ks_investigate_others<-ks_select_wells[which(ks_select_wells$STATUS %in% ks_statuses_to_investigate_in_group),]
View(ks_investigate_others)

# table of secondary statuses of ambiguous "other" wells
ks_others_status2s<-table(ks_investigate_others$STATUS2)
View(ks_others_status2s)

# make data frame of comments on others
ks_others_comments<-table(ks_investigate_others$COMMENTS)
ks_others_comments<-as.data.frame(ks_others_comments)
unique(ks_others_comments$Var1)

# vector of comments that had "SWD", "injection", "disposal", "INJ" included to keep (out of 1,777 different comments)
disposal_comments<-c("elog marked in pencil, Collins SWD 1-BRB10/9/2000  quarter call & KB not on elog thus top card/ACO1 not identifiable for well number-BRB10/9/2000",
                     "believed to be same well as Mid-West Oil and Gas drilled in the twenties-also exact footage as a Stanolind #1 SWD drilled to just over 500 feet-LKS",
                     "ACO-1 also lists well as a water injection well - 1/21/04 ABC",
                     "ACO-1 also lists well as water injection - 1/21/04 ABC",
                     "Ground elevation taken from scout card. Well is also a water injection per ACO-1 - 1/14/04 ABC",
                     "SALT WATER DISPOSAL TEST",
                     "WATER INJ WELL")

# keep these STATUS2s
ks_status2s_to_keep_from_investigation<-c("Authorized Injection Well","Injection Authorization Terminated","Injection Well Split to Another Dkt","Unplugged Former Injection Well")

# define "not-in" function
`%notin%` <- function(x,y) !(x %in% y)

# select wells to drop based on status 2 and comments
wells_to_drop <- subset(ks_investigate_others, STATUS2 %notin% ks_status2s_to_keep_from_investigation & COMMENTS %notin% disposal_comments)
View(wells_to_drop)

# get well IDs alone
kids_of_wells_to_drop<-wells_to_drop$KID
save(kids_of_wells_to_drop,file="kids_of_wells_to_drop.rdata")
save(ks_select_wells,file="ks_select_wells.rdata")



#### begin final well list ####
# load necessary files if starting from here
load(file="ks_select_wells.rdata")
load(file="kids_of_wells_to_drop.rdata")
load(file="ks_uic.rdata")

# define "not-in" function
`%notin%` <- function(x,y) !(x %in% y)

ks_final_wells<-ks_select_wells

# drop the droppables
ks_final_wells<-ks_final_wells[which(ks_final_wells$KID %notin% kids_of_wells_to_drop),]
ks_final_well_statuses<-table(ks_final_wells$STATUS)
View(ks_final_well_statuses)

# add my own categories
ks_final_wells$drilled_post_2006<-NA
ks_final_wells$active_post_2006<-NA
ks_final_wells$swd_inj_ci<-NA
ks_final_wells$activity<-NA
ks_final_wells$in_uic<-NA
ks_final_wells$has_api<-NA
ks_final_wells$other<-NA

# # load UIC volume data
# load(file="ks_uic.rdata")
# View(ks_uic)

# # putting views together so they don't run every time
# #### OTHER-(P&A(INJ OR )): Two wells assumed to be INJ, plugged and abandoned
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER-P&A(INJ OR )"),])
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER-P&A(INJ OR )" | ks_final_wells$API_NUMBER=="15-009-00009"),])
# #### OTHER(1O&1SWD)
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(1O&1SWD)"),])
# 
# #### OTHER-P&A(OIL-SWD)
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER-P&A(OIL-SWD)"),])
# 
# #### OTHER(CBM/SWD)
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(CBM/SWD)"),])
# 
# #### OTHER(NHDW)
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(NHDW)"),])
# 
# #### OTHER(INJ or EOR)
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(INJ or EOR)"),])
# View(ks_final_wells[which(ks_final_wells$API_NUMBER=="15-159-19310"),])
# 
# #### OTHER(OIL,SWD)
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(OIL,SWD)"),])
#
# #### OTHER()
# ks_other<-ks_final_wells[which(ks_final_wells$STATUS=="OTHER()"),]
# View(ks_other)
# 
# #### OTHER-P&A(TA)
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER-P&A(TA)"),])
# 
# #### OTHER(TA)
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(TA)"),])
# 
# #### OTHER-P&A(INJ or EOR)
# View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER-P&A(INJ or EOR)"),])




# #### updating my database and checking little categories
# 
# #### OTHER-(P&A(INJ OR )): Two wells assumed to be INJ, plugged and abandoned
# 
# # update my classifications
# # in uic
# ks_final_wells <- within(ks_final_wells, in_uic[STATUS == "OTHER-P&A(INJ OR )"] <- 'no')
# # has API
# ks_final_wells <- within(ks_final_wells, has_api[STATUS == "OTHER-P&A(INJ OR )"] <- 'yes')
# # active post 2006 (based on uic)
# ks_final_wells <- within(ks_final_wells, active_post_2006[STATUS == "OTHER-P&A(INJ OR )"] <- 'no')
# # drilled post 2006
# ks_final_wells <- within(ks_final_wells, drilled_post_2006[STATUS == "OTHER-P&A(INJ OR )"] <- 'no')
# # active or PA
# ks_final_wells <- within(ks_final_wells, activity[STATUS == "OTHER-P&A(INJ OR )"] <- 'pa')
# # swd_inj_ci
# ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "OTHER-P&A(INJ OR )"] <- 'inj')
# #active or PA
# ks_final_wells <- within(ks_final_wells, activity[STATUS == "OTHER-P&A(INJ OR )"] <- 'pa')
# # other flag
# ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER-P&A(INJ OR )"] <- 'yes')
# 
# 
# #### OTHER(1O&1SWD)
# 
# # update my classifications
# # in uic
# ks_final_wells <- within(ks_final_wells, in_uic[API_NUMBER == "15-191-21652"] <- 'yes')
# ks_final_wells <- within(ks_final_wells, in_uic[API_NUMBER == "15-079-00332-0001"] <- 'no')
# # has API
# ks_final_wells <- within(ks_final_wells, has_api[STATUS == "OTHER(1O&1SWD)"] <- 'yes')
# # active post 2006 (based on uic)
# ks_final_wells <- within(ks_final_wells, active_post_2006[API_NUMBER == "15-191-21652"] <- 'yes')
# ks_final_wells <- within(ks_final_wells, active_post_2006[API_NUMBER == "15-079-00332-0001"] <- 'no_uic_no_plug')
# # drilled post 2006
# ks_final_wells <- within(ks_final_wells, drilled_post_2006[STATUS == "OTHER(1O&1SWD)"] <- 'no')
# # activity
# ks_final_wells <- within(ks_final_wells, activity[API_NUMBER == "15-191-21652"] <- 'active')
# ks_final_wells <- within(ks_final_wells, activity[API_NUMBER == "15-079-00332-0001"] <- 'no_plug')
# # swd_inj_ci
# ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "OTHER(1O&1SWD)"] <- 'swd')
# # other flag
# ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER(1O&1SWD)"] <- 'yes')
# 
# 
# #### OTHER-P&A(OIL-SWD)
# 
# # update my classifications
# # active post 2006 (based on uic)
# ks_final_wells <- within(ks_final_wells, active_post_2006[STATUS == "OTHER-P&A(OIL-SWD)"] <- 'no')
# # drilled post 2006
# ks_final_wells <- within(ks_final_wells, drilled_post_2006[STATUS == "OTHER-P&A(OIL-SWD)"] <- 'no')
# # activity
# ks_final_wells <- within(ks_final_wells, activity[STATUS == "OTHER-P&A(OIL-SWD)"] <- 'pa')
# # swd_inj_ci
# ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "OTHER-P&A(OIL-SWD)"] <- 'swd')
# # other flag
# ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER-P&A(OIL-SWD)"] <- 'yes')
# 
# 
# #### OTHER(CBM/SWD)
# 
# # update my classifications
# # in uic
# ks_final_wells <- within(ks_final_wells, in_uic[API_NUMBER == "15-019-26584"] <- 'yes')
# # active post 2006 (based on uic)
# ks_final_wells <- within(ks_final_wells, active_post_2006[API_NUMBER == "15-019-26584"] <- 'yes')
# # drilled post 2006
# ks_final_wells <- within(ks_final_wells, drilled_post_2006[API_NUMBER == "15-019-26584"] <- 'no')
# # activity
# ks_final_wells <- within(ks_final_wells, activity[API_NUMBER == "15-019-26584"] <- 'no_plug')
# # swd_inj_ci
# ks_final_wells <- within(ks_final_wells, swd_inj_ci[API_NUMBER == "15-019-26584"] <- 'swd')
# # other flag
# ks_final_wells <- within(ks_final_wells, other[API_NUMBER == "15-019-26584"] <- 'yes')
# 
# 
# #### OTHER(INJ or EOR)
# 
# # sole well removed from dataset because it was recompleted as a better-classified SWD PA
# ks_final_wells<-ks_final_wells[!(ks_final_wells$KID=="1046071032"),]
# 
# 
# #### OTHER(NHDW)
# 
# # update my classifications
# # has API
# ks_final_wells <- within(ks_final_wells, has_api[KID == "1043994238"] <- 'no')
# # in uic
# ks_final_wells <- within(ks_final_wells, in_uic[KID == "1043994238"] <- 'no')
# # active post 2006 (based on uic)
# ks_final_wells <- within(ks_final_wells, active_post_2006[KID == "1043994238"] <- 'yes')
# # drilled post 2006
# ks_final_wells <- within(ks_final_wells, drilled_post_2006[KID == "1043994238"] <- 'yes')
# # activity
# ks_final_wells <- within(ks_final_wells, activity[KID == "1043994238"] <- 'permit')
# # swd_inj_ci
# ks_final_wells <- within(ks_final_wells, swd_inj_ci[KID == "1043994238"] <- 'c1')
# # other flag
# ks_final_wells <- within(ks_final_wells, other[KID == "1043994238"] <- 'yes')
# 
# 
# #### OTHER(OIL,SWD)
# 
# # update my classifications 
# # has API
# ks_final_wells <- within(ks_final_wells, has_api[KID == "1006137923"] <- 'yes')
# # in uic
# ks_final_wells <- within(ks_final_wells, in_uic[KID == "1006137923"] <- 'yes')
# # active post 2006 (based on uic)
# ks_final_wells <- within(ks_final_wells, active_post_2006[KID == "1006137923"] <- 'yes')
# # drilled post 2006
# ks_final_wells <- within(ks_final_wells, drilled_post_2006[KID == "1006137923"] <- 'no')
# # activity
# ks_final_wells <- within(ks_final_wells, activity[KID == "1006137923"] <- 'active')
# # swd_inj_ci
# ks_final_wells <- within(ks_final_wells, swd_inj_ci[KID == "1006137923"] <- 'swd')
# # other flag
# ks_final_wells <- within(ks_final_wells, other[KID == "1006137923"] <- 'yes')
# 
# 
# #### OTHER()
# # swd_inj_ci injection wells by STATUS2
# ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER()" & STATUS2 %in% c("Authorized Injection Well", "Injection Authorization Terminated", "Unplugged Former Injection Well","Injection Well Split to Another Dkt")] <- 'inj')
# # swd_inj_ci injection wells by COMMENTS
# ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER()" & COMMENTS == "WATER INJ WELL"] <- 'inj')
# # swd_inj_ci SWD wells by COMMENTS 
# ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER()" & COMMENTS %in% c("elog marked in pencil, Collins SWD 1-BRB10/9/2000  quarter call & KB not on elog thus top card/ACO1 not identifiable for well number-BRB10/9/2000","believed to be same well as Mid-West Oil and Gas drilled in the twenties-also exact footage as a Stanolind #1 SWD drilled to just over 500 feet-LKS","SALT WATER DISPOSAL TEST")] <- 'swd')
# 
# # other flag
# ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER()"] <- 'yes')
# 
# 
# #### OTHER-P&A(TA)
# 
# # swd_inj_ci
# ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER-P&A(TA)"] <- 'inj')
# 
# # other flag
# ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER-P&A(TA)"] <- 'yes')
# 
# 
# #### OTHER(TA)
# 
# # swd_inj_ci
# ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER(TA)"] <- 'inj')
# 
# # other flag
# ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER(TA)"] <- 'yes')
# 
# 
# #### OTHER-P&A(INJ or EOR)
# 
# # swd_inj_ci
# ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER-P&A(INJ or EOR)"] <- 'inj')
# 
# # other flag
# ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER-P&A(INJ or EOR)"] <- 'yes')
# 
# 
# #### conditional group assignments
# # assign has api
# ks_final_wells$has_api <- ifelse(ks_final_wells$API_NUMBER == "", "no", "yes")
# 
# # assign in uic
# kids_in_uic<-unique(ks_uic$KGS_ID)
# ks_final_wells$in_uic <- ifelse(ks_final_wells$KID %in% kids_in_uic, "yes", "no")
# 
# # assign statuses
# #### class ones
# 
# # swd_inj_ci
# ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "OTHER(CLASS1)" | STATUS == "OTHER(CLASS1)" | STATUS == "OTHER(CLASS ONE (OLD))" | STATUS == "OTHER-P&A(CLASS ONE (OLD))"] <- 'c1')
# 
# # other flag
# ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER(CLASS1)" | STATUS == "OTHER(CLASS1)" | STATUS == "OTHER(CLASS ONE (OLD))" | STATUS == "OTHER-P&A(CLASS ONE (OLD))"] <- 'yes')
# 
# # SWDs
# ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "SWD" | STATUS == "SWD-P&A"] <- 'swd')
# 
# # INJs
# ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "INJ" | STATUS == "INJ-P&A"] <- 'inj')
# 
# # assign not-others to "no"
# ks_final_wells$other[is.na(ks_final_wells$other)] <- "no"
# View(ks)
# 
# save(ks_final_wells,file="ks_final_wells.rdata")
# 
# # make dataset without wells without apis
# ks_only_apis<-ks_final_wells
# ks_only_apis<-ks_final_wells[!(is.na(ks_final_wells$API_NUMBER) | ks_final_wells$API_NUMBER==""),]
# View(ks_only_apis)
# 
# # find duplicate APIs
# ks_final_index <- duplicated(ks_only_apis$API_NUMBER) | duplicated(ks_only_apis$API_NUMBER, fromLast = TRUE)
# ks_API_dups<-ks_only_apis[ks_final_index, ]
# View(ks_API_dups)
# 
# # remove duplicate APIs, keeping those last modified
# ks_bye_dup_API<-subset(ks_only_apis, ave(modified_as_date, API_NUMBER, FUN = max) == modified_as_date)
# 
# # find any duplicate KIDs
# ks_KID_index <- duplicated(ks_only_apis$KID) | duplicated(ks_only_apis$KID, fromLast = TRUE)
# ks_KID_dups<-ks_only_apis[ks_KID_index, ]
# View(ks_KID_dups) # there were none
# 
# View(ks_bye_dup_API)
# 
# # find duplicated latitudes and longitudes
# ks_lat_long_dup_index<-duplicated(ks_bye_dup_API[c("LATITUDE","LONGITUDE")]) | duplicated(ks_bye_dup_API[c("LATITUDE","LONGITUDE")], fromLast = TRUE)
# ks_lat_long_dups<-ks_bye_dup_API[ks_lat_long_dup_index, ]
# View(ks_lat_long_dups)
# 
# 
# #### dealing with API duplication based on 4-digit activity code
# 
# # delete extraneous four digits
# ks_bye_dup_API$API_NUMBER_SIMPLE <- substr(ks_bye_dup_API$API_NUMBER, 0, 12)
# View(ks_bye_dup_API)
# 
# # find duplicate APIs without extraneous four digits
# ks_simple_API_index <- duplicated(ks_bye_dup_API$API_NUMBER_SIMPLE) | duplicated(ks_bye_dup_API$API_NUMBER_SIMPLE, fromLast = TRUE)
# ks_simple_API_dups<-ks_bye_dup_API[ks_simple_API_index, ]
# View(ks_simple_API_dups)
# 
# # remove duplicate APIs, keeping those last modified
# ks_bye_dup_API_simple<-subset(ks_bye_dup_API, ave(modified_as_date, API_NUMBER_SIMPLE, FUN = max) == modified_as_date)
# 
# # calling it the final thesis data
# ks_final_thesis_data<-ks_bye_dup_API_simple
# save(ks_final_thesis_data,file="ks_final_thesis_data.rdata")
# View(ks_final_thesis_data)
# 
# table(ks_final_thesis_data$other,ks_final_thesis_data$swd_inj_ci)
# 
# ks_set_for_mapping<-ks_final_thesis_data[,c("KID","API_NUMBER_SIMPLE","LATITUDE","LONGITUDE","swd_inj_ci")]
# View(ks_set_for_mapping)
# write.csv2(ks_set_for_mapping,file="ks_set_for_mapping.csv")
# 
# # import mapping results back
# ks_join_results<-read.csv(file="KansasWellJoinResults20180415.txt",stringsAsFactors = FALSE)
# View(ks_join_results)
# save(ks_join_results,file="ks_join_results.rdata")

# load needed files if starting from here
load(file="ks_final_thesis_data.rdata")
load(file="ks_join_results.rdata")

ks_join_to_edit<-ks_join_results
ks_join_to_edit$API_NUMBER<-NULL
ks_join_to_edit$LATITUDE<-NULL
ks_join_to_edit$LONGITUDE<-NULL
ks_join_to_edit$swd_inj_ci<-NULL
ks_join_to_edit$Join_Count<-NULL
ks_join_to_edit$OBJECTID_1<-NULL
ks_join_to_edit$TARGET_FID<-NULL

ks_wells_and_block_groups<-merge(x = ks_final_thesis_data, y = ks_join_to_edit, by = "KID", all.x = TRUE)

# assigning years

# go for the date limit
ks_wells_and_block_groups$drilled_post_2006<-NULL
ks_wells_and_block_groups$active_post_2006<-NULL

# 2000 year
ks_wells_and_block_groups$permit_after_2000 <- NA
ks_wells_and_block_groups$spud_after_2000 <- NA
ks_wells_and_block_groups$completed_after_2000 <- NA
ks_wells_and_block_groups$active_after_2000 <- NA

# 2007 year
ks_wells_and_block_groups$permit_after_2007 <- NA
ks_wells_and_block_groups$spud_after_2007 <- NA
ks_wells_and_block_groups$completed_after_2007 <- NA
ks_wells_and_block_groups$active_after_2007 <- NA

# 2010 year
ks_wells_and_block_groups$permit_after_2010 <- NA
ks_wells_and_block_groups$spud_after_2010 <- NA
ks_wells_and_block_groups$completed_after_2010 <- NA
ks_wells_and_block_groups$active_after_2010 <- NA


# 2000 permit
ks_wells_and_block_groups<-within(ks_wells_and_block_groups, permit_after_2000[permit_as_date >= "2000-01-01"] <- 'yes')
ks_wells_and_block_groups$permit_after_2000[is.na(ks_wells_and_block_groups$permit_after_2000)] <- "no"
# 2000 spud
ks_wells_and_block_groups<-within(ks_wells_and_block_groups, spud_after_2000[spud_as_date >= "2000-01-01"] <- 'yes')
ks_wells_and_block_groups$spud_after_2000[is.na(ks_wells_and_block_groups$spud_after_2000)] <- "no"
# 2000 completed
ks_wells_and_block_groups<-within(ks_wells_and_block_groups, completed_after_2000[completion_as_date >= "2000-01-01"] <- 'yes')
ks_wells_and_block_groups$completed_after_2000[is.na(ks_wells_and_block_groups$completed_after_2000)] <- "no"

# 2007 permit
ks_wells_and_block_groups<-within(ks_wells_and_block_groups, permit_after_2007[permit_as_date >= "2007-01-01"] <- 'yes')
ks_wells_and_block_groups$permit_after_2007[is.na(ks_wells_and_block_groups$permit_after_2007)] <- "no"
# 2007 spud
ks_wells_and_block_groups<-within(ks_wells_and_block_groups, spud_after_2007[spud_as_date >= "2007-01-01"] <- 'yes')
ks_wells_and_block_groups$spud_after_2007[is.na(ks_wells_and_block_groups$spud_after_2007)] <- "no"
# 2007 completed
ks_wells_and_block_groups<-within(ks_wells_and_block_groups, completed_after_2007[completion_as_date >= "2007-01-01"] <- 'yes')
ks_wells_and_block_groups$completed_after_2007[is.na(ks_wells_and_block_groups$completed_after_2007)] <- "no"

# 2010 permit
ks_wells_and_block_groups<-within(ks_wells_and_block_groups, permit_after_2010[permit_as_date >= "2010-01-01"] <- 'yes')
ks_wells_and_block_groups$permit_after_2010[is.na(ks_wells_and_block_groups$permit_after_2010)] <- "no"
# 2010 spud
ks_wells_and_block_groups<-within(ks_wells_and_block_groups, spud_after_2010[spud_as_date >= "2010-01-01"] <- 'yes')
ks_wells_and_block_groups$spud_after_2010[is.na(ks_wells_and_block_groups$spud_after_2010)] <- "no"
# 2010 completed
ks_wells_and_block_groups<-within(ks_wells_and_block_groups, completed_after_2010[completion_as_date >= "2010-01-01"] <- 'yes')
ks_wells_and_block_groups$completed_after_2010[is.na(ks_wells_and_block_groups$completed_after_2010)] <- "no"

# single fix based on comments and duplicate API
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_inj_ci[KID == "1001233161"] <- 'swd')

# currently active classification
ks_wells_and_block_groups$current_active<-NA

# use status
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, current_active[STATUS == "INJ-P&A" | STATUS == "OTHER-P&A(CLASS ONE (OLD))" | STATUS == " OTHER-P&A(INJ OR )" | STATUS == " OTHER-P&A(INJ or EOR)" | STATUS == "OTHER-P&A(OIL-SWD)" | STATUS == " OTHER-P&A(TA)" | STATUS == "SWD-P&A"] <- 'plugged_abandoned')

# use presence of plugged date
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, current_active[!is.na(plugging_as_date)] <- 'plugged_abandoned')

# make rest active
ks_wells_and_block_groups$current_active[is.na(ks_wells_and_block_groups$current_active)] <- "presumed_active"

# combine permit or spud after 2000
ks_wells_and_block_groups$permit_spud_post_2000<-NA
ks_wells_and_block_groups$permit_spud_post_2007<-NA
ks_wells_and_block_groups$permit_spud_post_2010<-NA

# post 2000
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, permit_spud_post_2000[permit_after_2000 == "yes" | spud_after_2000 == "yes"] <- 'yes')
ks_wells_and_block_groups$permit_spud_post_2000[is.na(ks_wells_and_block_groups$permit_spud_post_2000)] <- "no"

# post 2007
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, permit_spud_post_2007[permit_after_2007 == "yes" | spud_after_2007 == "yes"] <- 'yes')
ks_wells_and_block_groups$permit_spud_post_2007[is.na(ks_wells_and_block_groups$permit_spud_post_2007)] <- "no"

# post 2010
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, permit_spud_post_2010[permit_after_2010 == "yes" | spud_after_2010 == "yes"] <- 'yes')
ks_wells_and_block_groups$permit_spud_post_2010[is.na(ks_wells_and_block_groups$permit_spud_post_2010)] <- "no"


# categories by which to subset: swd_inj_ci, other, permit or spud after 2000, current_active

# make categories manually
ks_wells_and_block_groups$swd_norm_post2000_active<-NA
ks_wells_and_block_groups$swd_norm_post2000_pa<-NA
ks_wells_and_block_groups$swd_norm_pre2000_active<-NA
ks_wells_and_block_groups$swd_norm_pre2000_pa<-NA

ks_wells_and_block_groups$swd_other_post2000_active<-NA
ks_wells_and_block_groups$swd_other_post2000_pa<-NA
ks_wells_and_block_groups$swd_other_pre2000_active<-NA
ks_wells_and_block_groups$swd_other_pre2000_pa<-NA

ks_wells_and_block_groups$inj_norm_post2000_active<-NA
ks_wells_and_block_groups$inj_norm_post2000_pa<-NA
ks_wells_and_block_groups$inj_norm_pre2000_active<-NA
ks_wells_and_block_groups$inj_norm_pre2000_pa<-NA
ks_wells_and_block_groups$inj_other_post2000_active<-NA
ks_wells_and_block_groups$inj_other_post2000_pa<-NA
ks_wells_and_block_groups$inj_other_pre2000_active<-NA
ks_wells_and_block_groups$inj_other_pre2000_pa<-NA

ks_wells_and_block_groups$ci_other_post2000_active<-NA
ks_wells_and_block_groups$ci_other_post2000_pa<-NA
ks_wells_and_block_groups$ci_other_pre2000_active<-NA
ks_wells_and_block_groups$ci_other_pre2000_pa<-NA


ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_norm_post2000_active[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active']<-'yes')
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_norm_post2000_pa[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned']<-'yes')
# ks_wells_and_block_groups$swd_norm_pre2000_active
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_norm_pre2000_active[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active']<-'yes')
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_norm_pre2000_pa[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned']<-'yes')


ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_other_post2000_active[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active']<-'yes')
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_other_post2000_pa[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned']<-'yes')
# ks_wells_and_block_groups$swd_norm_pre2000_active
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_other_pre2000_active[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active']<-'yes')
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_other_pre2000_pa[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned']<-'yes')


ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_norm_post2000_active[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active']<-'yes')
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_norm_post2000_pa[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned']<-'yes')
# ks_wells_and_block_groups$inj_norm_pre2000_active
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_norm_pre2000_active[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active']<-'yes')
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_norm_pre2000_pa[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned']<-'yes')


ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_other_post2000_active[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active']<-'yes')
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_other_post2000_pa[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned']<-'yes')
# ks_wells_and_block_groups$inj_norm_pre2000_active
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_other_pre2000_active[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active']<-'yes')
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_other_pre2000_pa[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned']<-'yes')


ks_wells_and_block_groups <- within(ks_wells_and_block_groups, ci_other_post2000_active[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active']<-'yes')
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, ci_other_post2000_pa[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned']<-'yes')
# ks_wells_and_block_groups$swd_norm_pre2000_active
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, ci_other_pre2000_active[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active']<-'yes')
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, ci_other_pre2000_pa[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned']<-'yes')

View(ks_wells_and_block_groups)


swd_norm_post2000_active<-aggregate(swd_norm_post2000_active~GEOID_Data,ks_wells_and_block_groups,length)
swd_norm_post2000_pa<-aggregate(swd_norm_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
swd_norm_pre2000_active<-aggregate(swd_norm_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
swd_norm_pre2000_pa<-aggregate(swd_norm_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)

swd_other_post2000_active<-aggregate(swd_other_post2000_active~GEOID_Data,ks_wells_and_block_groups,length)
swd_other_post2000_pa<-aggregate(swd_other_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length) # no data in this one
swd_other_pre2000_active<-aggregate(swd_other_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
swd_other_pre2000_pa<-aggregate(swd_other_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)

inj_norm_post2000_active<-aggregate(inj_norm_post2000_active~GEOID_Data,ks_wells_and_block_groups,length)
inj_norm_post2000_pa<-aggregate(inj_norm_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
inj_norm_pre2000_active<-aggregate(inj_norm_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
inj_norm_pre2000_pa<-aggregate(inj_norm_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
inj_other_post2000_active<-aggregate(inj_other_post2000_active~GEOID_Data,ks_wells_and_block_groups,length) # no data in this one
inj_other_post2000_pa<-aggregate(inj_other_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length) # no data in this one
inj_other_pre2000_active<-aggregate(inj_other_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
inj_other_pre2000_pa<-aggregate(inj_other_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)

ci_other_post2000_active<-aggregate(ci_other_post2000_active~GEOID_Data,ks_wells_and_block_groups,length) # no data
ci_other_post2000_pa<-aggregate(ci_other_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length) # no data
ci_other_pre2000_active<-aggregate(ci_other_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
ci_other_pre2000_pa<-aggregate(ci_other_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)

# merge them all
ks_well_counts<-Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "GEOID_Data", all = TRUE),
       list(
         swd_norm_post2000_active,
         swd_norm_post2000_pa,
         swd_norm_pre2000_active,
         swd_norm_pre2000_pa,
         
         swd_other_post2000_active,
         swd_other_pre2000_active,
         swd_other_pre2000_pa,
         
         inj_norm_post2000_active,
         inj_norm_post2000_pa,
         inj_norm_pre2000_active,
         inj_norm_pre2000_pa,
         inj_other_pre2000_active,
         inj_other_pre2000_pa,
         
         ci_other_pre2000_active,
         ci_other_pre2000_pa   
       ))
View(ks_well_counts)

sum(colSums(ks_well_counts[2:16],na.rm = TRUE)) # equaled 19,995 by some miracle

# rename GEOID_Data to GEOID in well data
ks_well_counts$GEOID<-ks_well_counts$GEOID_Data
save(ks_well_counts,file="ks_well_counts.rdata")
load(file="ks_well_counts.rdata")

# load ACS variables
load(file="ACS_constructed_variables.rdata")
View(ACS_constructed_variables[,201:291])

# limit ks block groups to ks
ks_acs<-ACS_constructed_variables[which(ACS_constructed_variables$STATEFP=='20'),]
save(ks_acs,file="ks_acs.rdata")
load(file="ks_acs.rdata")

# merge well counts and acs!  hey-o!
KS_FINAL_DATASET<-merge(ks_well_counts, ks_acs, by = "GEOID", all = TRUE)
View(KS_FINAL_DATASET)
save(KS_FINAL_DATASET,file="KS_FINAL_DATASET.rdata")
write.csv(KS_FINAL_DATASET,file="KS_FINAL_DATASET.csv")
load(file="KS_FINAL_DATASET.rdata")

#### begin analyses
ks_analysis_dataset<-KS_FINAL_DATASET

# swd block groups only

# make variable for sum of swd wells per block group
ks_analysis_dataset$any_swd_sum<-NA
ks_analysis_dataset$any_swd_sum<-rowSums(ks_analysis_dataset[,c("swd_norm_post2000_active","swd_norm_post2000_pa","swd_norm_pre2000_active","swd_norm_pre2000_pa","swd_other_post2000_active","swd_other_pre2000_active","swd_other_pre2000_pa")], na.rm=TRUE)

# make new binary variable for have versus not have swd
ks_analysis_dataset$any_swd_binary<-NA
ks_analysis_dataset<-within(ks_analysis_dataset, any_swd_binary[any_swd_sum == 0]<-'no')
ks_analysis_dataset<-within(ks_analysis_dataset, any_swd_binary[any_swd_sum > 0]<-'yes')
ks_analysis_dataset$any_swd_binary

names(ks_analysis_dataset)

#### analysis by presence and absence of swd ####
# n
table(ks_analysis_dataset$any_swd_binary)

## median household income
# median
aggregate(median_household_income_B19013~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$median_household_income_B19013, na.rm = TRUE)

# interquartile range
aggregate(median_household_income_B19013~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$median_household_income_B19013, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$median_household_income_B19013))
aggregate(median_household_income_B19013~any_swd_binary, data=ks_analysis_dataset, FUN=length)


## median household value
# median
aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$median_household_income_B19013, na.rm = TRUE)

# interquartile range
aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$median_household_income_B19013, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$median_household_value_B25077))
aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=length)


## percent white
# median
aggregate(percent_white_B03002~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$percent_white_B03002, na.rm = TRUE)

# interquartile range
aggregate(percent_white_B03002~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$percent_white_B03002, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$percent_white_B03002))
aggregate(percent_white_B03002~any_swd_binary, data=ks_analysis_dataset, FUN=length)


## population density
# median
aggregate(population_density_B01001_ALAND~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)

# interquartile range
aggregate(population_density_B01001_ALAND~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$population_density_B01001_ALAND))


## percent high school or more
# median
aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(percent_high_school_plus_B15003, na.rm = TRUE)
hsttest<-lm(any_swd_binary~percent_high_school_plus_B15003, data=ks_analysis_dataset)
summary(hsttest)

# interquartile range
aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$percent_high_school_plus_B15003, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$percent_high_school_plus_B15003))
aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=length)


## median age
# median
aggregate(median_age_B01002~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$median_age_B01002, na.rm = TRUE)

# interquartile range
aggregate(median_age_B01002~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$median_age_B01002, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$median_age_B01002))
aggregate(median_age_B01002~any_swd_binary, data=ks_analysis_dataset, FUN=length)










                                    
permit_spud_post_2010[permit_after_2010 == "yes" | spud_after_2010 == "yes"] <- 'yes'
View(ks_wells_by_block_group_count)



View(ks_wells_and_block_groups)







table(ks_bye_dup_API_simple$swd_inj_ci)






View(ks_final_wells[which(is.na(ks_final_wells$swd_inj_ci)),])
View(ks_final_wells)


table(ks_final_wells$swd_inj_ci,ks_final_wells$in_uic)
table(ks_final_wells$STATUS,ks_final_wells$has_api)







kids_other_pa_inj_or<-ks_final_wells[ks_final_wells$STATUS == "OTHER-P&A(INJ OR )", "KID"]
View(kids_other_pa_inj_or)

View(ks_uic[which(ks_uic$KGS_ID %in% kids_other_pa_inj_or),]) # neither was present in UIC database

# update my classification columns





View(ks_final_wells[which(ks_final_wells$STATUS == "OTHER-P&A(INJ OR )"),])











# wells for investigation
ks_clean_other_unspec<-ks_clean[which(ks_clean$STATUS == "OTHER()" | ks_clean$STATUS == "OTHER(OTHER)"),]

# view other well types
ks_other_well_types<-table(ks_clean_other_unspec$STATUS2)
View(ks_other_well_types)
write.csv(ks_other_well_types,file="ks_other_well_types.csv")

ks_oil_gas_inj_wells<-ks_clean[which(ks_clean$STATUS == "OTHER(OIL&GAS-INJ)"),]
View(ks_oil_gas_inj_wells)

View(ks_clean_other_unspec)

View(ks_clean[which(ks_clean$STATUS=="INJ"),])

  










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



2996446/3493171





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
