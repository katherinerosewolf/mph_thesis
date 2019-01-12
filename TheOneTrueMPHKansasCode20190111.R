# Oil and Gas Disposal Well Analysis for Kansas
# Katherine Wolf
# December 2018



#### file requirements ####
# to run this code straight through as currently commented, you will need
# these files in the same folder:
# ks_wells_2018_11_01.rdata   # well data
# ks_uic_2018_09_04.rdata   # underground injection control data
# rows_requiring_comment_investigation_simple_2018_11_29_back_to_r.csv



#### check working directory ####

getwd() # this directory should be the one with your data files in it!
# if not, use "setwd()" to set the working directory



#### load libraries ####

library(data.table)
library(extrafont)
library(ggplot2)
library(plyr)
library(dplyr)
library(psych)
library(tcltk2)
library(reshape2)
library(totalcensus)
library(tidyverse)
library(geoRglm)
library(spatstat)
library(tidyr)



#### load fonts ####
# font_import()
# loadfonts(device = "win")



#### create necessary functions ####

# notin function
`%notin%`  <-  function(x,y) !(x %in% y) # define "not-in" function

# function to force R to stop
my_wait <- function() {
  tt <- tktoplevel()
  tkpack(tkbutton(
    tt, 
    text = 'continue', 
    command = 
      function()tkdestroy(tt)),
    side='bottom')
  tkbind(tt, '<Key>', 
         function()tkdestroy(tt))
  tkwait.window(tt)
}



# # #### raw acs data import ####
# # 
# # variable_names <-
# #   readLines(
# #     "acs_2013_2017_data_supplements/comma_separated_variables.csv")
# # variable_names
# # 
# # variable_names_to_include <-
# #   readLines(
# #     "acs_2013_2017_data_supplements/variable_names_to_include.csv")
# # variable_names_to_include
# # 
# # variable_names_to_exclude <-
# #   readLines("acs_2013_2017_data_supplements/variable_names_to_exclude.csv"
# #             )
# # variable_names_to_exclude
# # 
# # true_variable_candidates <-
# #   variable_names[
# #     grepl(
# #       paste(
# #         variable_names_to_include,
# #         collapse="|"),
# #       variable_names)]
# # 
# # true_variable_candidates
# # 
# # variable_names_for_totalcensus <-
# #   true_variable_candidates[
# #     !grepl(
# #       paste(
# #         variable_names_to_exclude,
# #         collapse="|"),
# #       true_variable_candidates)]
# # 
# # additional_exclusions <-
# #   c("\\.",
# #     "B15002A",
# #     "B15002B",
# #     "B15002C",
# #     "B15002D",
# #     "B15002E",
# #     "B15002F",
# #     "B15002G",
# #     "B15002H",
# #     "B15002I",
# #     "B21001A",
# #     "B21001B",
# #     "B21001C",
# #     "B21001D",
# #     "B21001E",
# #     "B21001F",
# #     "B21001G",
# #     "B21001H",
# #     "B21001I")
# # 
# # final_variable_names_for_totalcensus <-
# #   variable_names_for_totalcensus[
# #     !grepl(
# #       paste(
# #         additional_exclusions,
# #         collapse="|"),
# #       variable_names_for_totalcensus
# #       )
# #     ]
# # 
# # final_variable_names_for_totalcensus
# # 
# # # set_path_to_census("acs_2013_2017_data")
# # 
# # acs_data_2013_2017_via_totalcensus <- 
# #   read_acs5year(
# #     year = 2017, 
# #     states = "KS", 
# #     table_contents = 
# #       final_variable_names_for_totalcensus, 
# #     summary_level = "block group", 
# #     with_margin = TRUE
# # )
# # 
# # # View(acs_data_2013_2017_via_totalcensus)
# # 
# # save(
# #   acs_data_2013_2017_via_totalcensus,
# #   file = "acs_data_2013_2017_via_totalcensus.rdata"
# #   )
# # 
# # write.csv(
# #   acs_data_2013_2017_via_totalcensus,
# #   file = "acs_data_2013_2017_via_totalcensus.csv"
# #   )
# #
# #
# #
# 
# 
# # #### raw well data import ####
# # 
# # # import raw data
# # ks_wells_2018_11_01 <- 
# #   fread(file = "ks_wells_2018_11_01.txt", 
# #         stringsAsFactors = FALSE, 
# #         colClasses = list(character = 'API_NUM_NODASH'))
# # 
# # # save as an rdata file
# # save(ks_wells_2018_11_01, 
# #      file = "ks_wells_2018_11_01.rdata") 
# 
# 
# 
# # #### UIC data import ####
# # 
# # # import raw data
# # ks_uic_2018_09_04 <- 
# #   fread(file = "KS_UIC_archive_2018_09_04.txt") 
# # 
# # # save as an rdata file
# # save(ks_uic_2018_09_04, 
# #      file = "ks_uic_2018_09_04.rdata")
# 
# 
# 
# # #### ks blockgroup from geodatabase shapefile import ####
# # 
# # ks_tiger_table <- # import raw data
# #   fread(
# #     "ks_2017_bg_gdb_table_export.txt",
# #     colClasses =
# #       list(character = 'GEOID'
# #       )
# #     )
# # 
# # View(ks_tiger_table)
# # 
# # # fix GEOID to match ACS data
# # ks_tiger_table$GEOID <-
# #   paste0('15000US',
# #          ks_tiger_table$GEOID
# #          )
# # 
# # # save as .rdata file
# # save(ks_tiger_table,
# #      file = "ks_tiger_table.rdata"
# #      )
# #
# #
# #
# # #### horizontal well data load ####
# # horizontal_wells_raw <-
# #   fread(file = "horizontal_wells_2019_01_09.txt")
# # 
# # save(horizontal_wells_raw,
# #      file = "horizontal_wells_raw.rdata")
# # 
# # 
# # 
# # #### mississippi lime data import ####
# # 
# # # import raw data
# # lime_data_import <-
# #   fread(file = "shale_to_block_groups.txt", 
# #         colClasses = list(character = 'GEOID'))
# # View(lime_data_import)
# # save(lime_data_import,
# #      file = "lime_data_import.rdata")
# 
# 
# 
# #### load necessary files to start here ####
# 
# # load well data
# load(file = "ks_wells_2018_11_01.rdata")
# 
# # load UIC data
# load(file = "ks_uic_2018_09_04.rdata")
# 
# # load ACS shapefile data from its attribute table
# load(file = "ks_tiger_table.rdata")
# 
# # load raw acs data
# load(file = "acs_data_2013_2017_via_totalcensus.rdata") 
# 
# # load horizontal well data
# load(file = "horizontal_wells_raw.rdata")
# 
# # load lime data
# load(file = "lime_data_import.rdata")
# 
# 
# 
# #### lime data work ####
# # make working dataset
# working_lime_data <- 
#   lime_data_import
# # View(working_lime_data)
# 
# # change shale presence to be meaningful
# colnames(working_lime_data)[which(
#   names(working_lime_data) == "Id")] <- "shale_presence"
# 
# # delete unneeded columns
# working_lime_data$Join_Count <- NULL
# working_lime_data$OBJECTID <- NULL
# working_lime_data$TARGET_FID <- NULL
# working_lime_data$gridcode <- NULL
# working_lime_data$SHAPE_Length <- NULL
# working_lime_data$SHAPE_Area <- NULL
# 
# # change NAs in shale presence to 0
# working_lime_data <- 
#   within(working_lime_data, 
#          shale_presence[is.na(shale_presence)] <- 0)
# # View(working_lime_data)
# 
# # # number of block groups with shale
# # sum(working_lime_data$shale_presence)
# # 
# # # table of with versus without shale
# # table(working_lime_data$shale_presence)
# 
# 
# 
# #### horizontal well data work ####
# 
# horizontal_wells <-   # make working dataset
#   horizontal_wells_raw
# 
# # convert dates to dates
# horizontal_wells$permit_as_date  <-
#   as.Date(horizontal_wells$PERMIT, "%d-%b-%Y") # permit date
# horizontal_wells$spud_as_date  <-
#   as.Date(horizontal_wells$SPUD, "%d-%b-%Y") # spud date
# horizontal_wells$completion_as_date  <-
#   as.Date(horizontal_wells$COMPLETION, "%d-%b-%Y") # completion date
# horizontal_wells$plugging_as_date  <-
#   as.Date(horizontal_wells$PLUGGING, "%d-%b-%Y") # plugging date
# horizontal_wells$modified_as_date  <-
#   as.Date(horizontal_wells$MODIFIED, "%d-%b-%Y") # modified date
# horizontal_wells$API_NUMBER  <-
#   as.character(horizontal_wells$API_NUMBER) # make API into a character
# 
# # isolate wells from 2010 onward
# 
# # make variables
# horizontal_wells$permit_2010_plus <- NA
# horizontal_wells$spud_2010_plus <- NA
# horizontal_wells$completion_2010_plus <- NA
# horizontal_wells$in_2010_plus <- NA
# 
# # 2010 permit, spud, and completion dates
# horizontal_wells <- 
#   within(horizontal_wells, 
#          permit_2010_plus[permit_as_date >= "2010-01-01"] <- 'yes')
# horizontal_wells <- 
#   within(horizontal_wells, 
#          spud_2010_plus[spud_as_date >= "2010-01-01"] <- 'yes')
# horizontal_wells <- 
#   within(horizontal_wells, 
#          completion_2010_plus[completion_as_date >= "2010-01-01"] <- 'yes')
# 
# # make ONE VARIABLE TO RULE THEM ALL called 'in_2010_plus'
# # (I deny watching Lord of the Rings while writing this code.)
# horizontal_wells <- 
#   within(horizontal_wells, 
#          in_2010_plus[permit_2010_plus == 'yes' |
#                        spud_2010_plus == 'yes' |
#                         completion_2010_plus == 'yes'] <- 'yes')
# 
# # View(horizontal_wells)
# 
# horizontal_wells_from_2010 <-   # make dataframe with only 2010 and later wells
#   horizontal_wells[which(horizontal_wells$in_2010_plus == 'yes'),]
# 
# # View(horizontal_wells_from_2010)
# 
# # exclude horizontal disposal wells (?!) (probably failed fracking wells)
# horizontal_wells_from_2010_exclusions <- 
#   horizontal_wells_from_2010[
#     which(
#       horizontal_wells_from_2010$STATUS %notin% 
#         c("SWD", "SWD-P&A")),]
# 
# # exclude well permitted after study period 
# horizontal_wells_from_2010_exclusions <- 
#   horizontal_wells_from_2010_exclusions[
#     which(
#       horizontal_wells_from_2010_exclusions$KID != 
#         1046919471),]
# # View(horizontal_wells_from_2010_exclusions)
# # table(horizontal_wells_from_2010$STATUS)
# 
# # make simple dataset for fast spatial joining
# horizontal_wells_for_map <- 
#   horizontal_wells_from_2010_exclusions[,c("KID", 
#                                           "LATITUDE", 
#                                           "LONGITUDE", 
#                                           "STATUS")]
# 
# # View(horizontal_wells_for_map)
# 
# write.csv(horizontal_wells_for_map, 
#           file = "horizontal_wells_for_map.csv")
# write.csv(horizontal_wells_from_2010_exclusions, 
#           file = "horizontal_wells_from_2010_exclusions.csv")
# 
# # HERE WE PORT THE HORIZONTAL WELL DATA TO ARCGIS FOR MAPPING FUN.  THE END.
# 
# 
# 
# #### UIC data work ####
# 
# # View(ks_uic_2018_09_04)
# 
# uic_small <- 
#   ks_uic_2018_09_04[,c("KGS_ID", 
#                        "API", 
#                        "LATITUDE", 
#                        "LONGITUDE", 
#                        "YEAR", 
#                        "MONTH", 
#                        "FLUID_INJECTED", 
#                        "GAS_INJECTED", 
#                        "TOTAL_GAS_YEAR", 
#                        "TOTAL_FLUID_YEAR", 
#                        "INJECTION_ZONE")]
# 
# # figure out why some of the rows have no year total but do have monthly totals
# check_weird_rows <- 
#   uic_small[which(FLUID_INJECTED > 0 & TOTAL_FLUID_YEAR == 0),]
# 
# # View(check_weird_rows)
# 
# # sum by KID
# fix_fluid_year <- aggregate(FLUID_INJECTED ~ KGS_ID, 
#                         check_weird_rows, 
#                         sum)
# 
# names(fix_fluid_year)[2] <- "FIX_TOTAL_FLUID_YEAR"
# 
# # View(fix_fluid_year)
# 
# fixed_weird_rows <- 
#   merge(x = check_weird_rows, 
#         y = fix_fluid_year, 
#         by = "KGS_ID", 
#         all = TRUE)
# 
# # View(fixed_weird_rows)
# 
# # fix the five manually
# uic_small <- 
#   within(uic_small, 
#          TOTAL_FLUID_YEAR[KGS_ID == 1033661327 & 
#                             YEAR == 2015] <- 6935)
# uic_small <- 
#   within(uic_small, 
#          TOTAL_FLUID_YEAR[KGS_ID == 1037335071 & 
#                             YEAR == 2017] <- 40028)
# uic_small <- 
#   within(uic_small, 
#          TOTAL_FLUID_YEAR[KGS_ID == 1042538729 & 
#                             YEAR == 2016] <- 40028)
# uic_small <- 
#   within(uic_small, 
#          TOTAL_FLUID_YEAR[KGS_ID == 1045066288 & 
#                             YEAR == 2016] <- 40028)
# uic_small <- 
#   within(uic_small, 
#          TOTAL_FLUID_YEAR[KGS_ID == 1045109583 & 
#                             YEAR == 2016] <- 40028)
# 
# recheck_weird_rows <- 
#   uic_small[which(FLUID_INJECTED > 0 & TOTAL_FLUID_YEAR == 0),]
# # View(recheck_weird_rows)
# 
# # View(uic_small)
# 
# uic_year_only <-   # remove months
#   uic_small[,c("KGS_ID", 
#                "API", 
#                "LATITUDE", 
#                "LONGITUDE", 
#                "YEAR", 
#                "TOTAL_GAS_YEAR", 
#                "TOTAL_FLUID_YEAR", 
#                "INJECTION_ZONE")]
# 
# uic_year_only <-   # remove duplicates, leaving only relevant months and years
#   unique(uic_year_only)
# 
# uic_2010_onward_only <- 
#   uic_year_only[which(YEAR %in% 
#                         c(2010, 
#                           2011, 
#                           2012, 
#                           2013, 
#                           2014, 
#                           2015,
#                           2016,
#                           2017)),]
# 
# # View(uic_2010_onward_only)
# 
# # sum lifetime fluid per KID
# total_lifetime_fluid_injected_by_well <- 
#   aggregate(TOTAL_FLUID_YEAR ~ KGS_ID, 
#             uic_year_only, 
#             sum)
# 
# names(total_lifetime_fluid_injected_by_well) <- 
#   c("KID", "total_lifetime_fluid_injected")
# 
# # sum 2010 onward fluid per KID
# from_2010_fluid_injected_by_well <- 
#   aggregate(TOTAL_FLUID_YEAR ~ KGS_ID, 
#             uic_2010_onward_only, 
#             sum)
# 
# names(from_2010_fluid_injected_by_well) <- 
#   c("KID", "from_2010_fluid_injected")
# 
# # merge lifetime and since 2010 totals
# fluid_totals_by_well <- 
#   merge(total_lifetime_fluid_injected_by_well, 
#         from_2010_fluid_injected_by_well, 
#         by = "KID", 
#         all = TRUE)
# 
# # View(fluid_totals_by_well)
# 
# well_data_from_master_set <- 
#   ks_wells_2018_11_01[,c("KID", 
#                          "STATUS", 
#                          "STATUS2", 
#                          "API_NUMBER", 
#                          "LATITUDE", 
#                          "LONGITUDE")]
# 
# fluid_totals_by_well <-   # 
#   merge(fluid_totals_by_well, 
#         well_data_from_master_set, 
#         by = "KID",
#         all.x = TRUE, 
#         all.y = FALSE)
# 
# # View(fluid_totals_by_well)
# 
# table(fluid_totals_by_well$STATUS)
# 
# swd_fluid_totals_by_well <- 
#   fluid_totals_by_well[which(
#     fluid_totals_by_well$STATUS %in% c("SWD", 
#                                        "SWD-P&A")
#   ),]
#   
# # View(swd_fluid_totals_by_well)
# 
# # make dataset for ArcGIS
# write.csv(swd_fluid_totals_by_well, 
#           file = "swd_fluid_totals_by_well.csv")
# 
# 
# 
# #### ACS DATA ####
# 
# # make working acs
# working_acs <-
#   acs_data_2013_2017_via_totalcensus
# 
# # convert from dataframe to data table
# working_acs <-
#   as.data.frame(working_acs)
# 
# # pull data from various categories
# total_pop <-
#   working_acs[
#     ,c(
#       "GEOID",
#       "B01001_001",
#       "B01001_001_margin"
#     )
#     ]
# sex_cat <- working_acs[
#   ,c(
#     "GEOID",
#     "B01001_002",
#     "B01001_002_margin",
#     "B01001_026",
#     "B01001_026_margin"
#   )
#   ]
# sex_age_cat <- working_acs[
#   ,c(
#     "GEOID",
#     "B01001_003",
#     "B01001_003_margin",
#     "B01001_004",
#     "B01001_004_margin",
#     "B01001_005",
#     "B01001_005_margin",
#     "B01001_006",
#     "B01001_006_margin",
#     "B01001_007",
#     "B01001_007_margin",
#     "B01001_008",
#     "B01001_008_margin",
#     "B01001_009",
#     "B01001_009_margin",
#     "B01001_010",
#     "B01001_010_margin",
#     "B01001_011",
#     "B01001_011_margin",
#     "B01001_012",
#     "B01001_012_margin",
#     "B01001_013",
#     "B01001_013_margin",
#     "B01001_014",
#     "B01001_014_margin",
#     "B01001_015",
#     "B01001_015_margin",
#     "B01001_016",
#     "B01001_016_margin",
#     "B01001_017",
#     "B01001_017_margin",
#     "B01001_018",
#     "B01001_018_margin",
#     "B01001_019",
#     "B01001_019_margin",
#     "B01001_020",
#     "B01001_020_margin",
#     "B01001_021",
#     "B01001_021_margin",
#     "B01001_022",
#     "B01001_022_margin",
#     "B01001_023",
#     "B01001_023_margin",
#     "B01001_024",
#     "B01001_024_margin",
#     "B01001_025",
#     "B01001_025_margin",
#     "B01001_027", # 26 is total population of women and omitted
#     "B01001_027_margin",
#     "B01001_028",
#     "B01001_028_margin",
#     "B01001_029",
#     "B01001_029_margin",
#     "B01001_030",
#     "B01001_030_margin",
#     "B01001_031",
#     "B01001_031_margin",
#     "B01001_032",
#     "B01001_032_margin",
#     "B01001_033",
#     "B01001_033_margin",
#     "B01001_034",
#     "B01001_034_margin",
#     "B01001_035",
#     "B01001_035_margin",
#     "B01001_036",
#     "B01001_036_margin",
#     "B01001_037",
#     "B01001_037_margin",
#     "B01001_038",
#     "B01001_038_margin",
#     "B01001_039",
#     "B01001_039_margin",
#     "B01001_040",
#     "B01001_040_margin",
#     "B01001_041",
#     "B01001_041_margin",
#     "B01001_042",
#     "B01001_042_margin",
#     "B01001_043",
#     "B01001_043_margin",
#     "B01001_044",
#     "B01001_044_margin",
#     "B01001_045",
#     "B01001_045_margin",
#     "B01001_046",
#     "B01001_046_margin",
#     "B01001_047",
#     "B01001_047_margin",
#     "B01001_048",
#     "B01001_048_margin",
#     "B01001_049",
#     "B01001_049_margin"
#   )
#   ]
# 
# age_median <- working_acs[
#   ,c(
#     "GEOID",
#     "B01002_001",
#     "B01002_001_margin",
#     "B01002_002",
#     "B01002_002_margin",
#     "B01002_003",
#     "B01002_003_margin"
#   )
#   ]
# 
# race_cat <- working_acs[
#   ,c(
#     "GEOID",
#     "B03002_001",
#     "B03002_001_margin",
#     "B03002_002",
#     "B03002_002_margin",
#     "B03002_003",
#     "B03002_003_margin",
#     "B03002_004",
#     "B03002_004_margin",
#     "B03002_005",
#     "B03002_005_margin",
#     "B03002_006",
#     "B03002_006_margin",
#     "B03002_007",
#     "B03002_007_margin",
#     "B03002_008",
#     "B03002_008_margin",
#     "B03002_009",
#     "B03002_009_margin",
#     "B03002_010",
#     "B03002_010_margin",
#     "B03002_011",
#     "B03002_011_margin",
#     "B03002_012",
#     "B03002_012_margin",
#     "B03002_013",
#     "B03002_013_margin",
#     "B03002_014",
#     "B03002_014_margin",
#     "B03002_015",
#     "B03002_015_margin",
#     "B03002_016",
#     "B03002_016_margin",
#     "B03002_017",
#     "B03002_017_margin",
#     "B03002_018",
#     "B03002_018_margin",
#     "B03002_019",
#     "B03002_019_margin",
#     "B03002_020",
#     "B03002_020_margin",
#     "B03002_021",
#     "B03002_021_margin")]
# education_cat <- working_acs[
#   ,c(
#     "GEOID",
#     "B15003_001",
#     "B15003_001_margin",
#     "B15003_002",
#     "B15003_002_margin",
#     "B15003_003",
#     "B15003_003_margin",
#     "B15003_004",
#     "B15003_004_margin",
#     "B15003_005",
#     "B15003_005_margin",
#     "B15003_006",
#     "B15003_006_margin",
#     "B15003_007",
#     "B15003_007_margin",
#     "B15003_008",
#     "B15003_008_margin",
#     "B15003_009",
#     "B15003_009_margin",
#     "B15003_010",
#     "B15003_010_margin",
#     "B15003_011",
#     "B15003_011_margin",
#     "B15003_012",
#     "B15003_012_margin",
#     "B15003_013",
#     "B15003_013_margin",
#     "B15003_014",
#     "B15003_014_margin",
#     "B15003_015",
#     "B15003_015_margin",
#     "B15003_016",
#     "B15003_016_margin",
#     "B15003_017",
#     "B15003_017_margin",
#     "B15003_018",
#     "B15003_018_margin",
#     "B15003_019",
#     "B15003_019_margin",
#     "B15003_020",
#     "B15003_020_margin",
#     "B15003_021",
#     "B15003_021_margin",
#     "B15003_022",
#     "B15003_022_margin",
#     "B15003_023",
#     "B15003_023_margin",
#     "B15003_024",
#     "B15003_024_margin",
#     "B15003_025",
#     "B15003_025_margin"
#   )
#   ]
# poverty_ratio_cat <-
#   working_acs[
#   ,c(
#     "GEOID",
#     "C17002_001",
#     "C17002_001_margin",
#     "C17002_002",
#     "C17002_002_margin",
#     "C17002_003",
#     "C17002_003_margin",
#     "C17002_004",
#     "C17002_004_margin",
#     "C17002_005",
#     "C17002_005_margin",
#     "C17002_006",
#     "C17002_006_margin",
#     "C17002_007",
#     "C17002_007_margin",
#     "C17002_008",
#     "C17002_008_margin"
#   )
#   ]
# income_house_cat <-
#   working_acs[
#     ,c(
#       "GEOID",
#       "B19001_001",
#       "B19001_001_margin",
#       "B19001_002",
#       "B19001_002_margin",
#       "B19001_003",
#       "B19001_003_margin",
#       "B19001_004",
#       "B19001_004_margin",
#       "B19001_005",
#       "B19001_005_margin",
#       "B19001_006",
#       "B19001_006_margin",
#       "B19001_007",
#       "B19001_007_margin",
#       "B19001_008",
#       "B19001_008_margin",
#       "B19001_009",
#       "B19001_009_margin",
#       "B19001_010",
#       "B19001_010_margin",
#       "B19001_011",
#       "B19001_011_margin",
#       "B19001_012",
#       "B19001_012_margin",
#       "B19001_013",
#       "B19001_013_margin",
#       "B19001_014",
#       "B19001_014_margin",
#       "B19001_015",
#       "B19001_015_margin",
#       "B19001_016",
#       "B19001_016_margin",
#       "B19001_017",
#       "B19001_017_margin"
#     )
#     ]
# income_house_median<-working_acs[
#   ,c(
#     "GEOID",
#     "B19013_001",
#     "B19013_001_margin"
#   )
#   ]
# earnings_sex_cat <-
#   working_acs[
#     ,c(
#       "GEOID",
#       "B20001_001",
#       "B20001_001_margin",
#       "B20001_002",
#       "B20001_002_margin",
#       "B20001_003",
#       "B20001_003_margin",
#       "B20001_004",
#       "B20001_004_margin",
#       "B20001_005",
#       "B20001_005_margin",
#       "B20001_006",
#       "B20001_006_margin",
#       "B20001_007",
#       "B20001_007_margin",
#       "B20001_008",
#       "B20001_008_margin",
#       "B20001_009",
#       "B20001_009_margin",
#       "B20001_010",
#       "B20001_010_margin",
#       "B20001_011",
#       "B20001_011_margin",
#       "B20001_012",
#       "B20001_012_margin",
#       "B20001_013",
#       "B20001_013_margin",
#       "B20001_014",
#       "B20001_014_margin",
#       "B20001_015",
#       "B20001_015_margin",
#       "B20001_016",
#       "B20001_016_margin",
#       "B20001_017",
#       "B20001_017_margin",
#       "B20001_018",
#       "B20001_018_margin",
#       "B20001_019",
#       "B20001_019_margin",
#       "B20001_020",
#       "B20001_020_margin",
#       "B20001_021",
#       "B20001_021_margin",
#       "B20001_022",
#       "B20001_022_margin",
#       "B20001_023",
#       "B20001_023_margin",
#       "B20001_024",
#       "B20001_024_margin",
#       "B20001_025",
#       "B20001_025_margin",
#       "B20001_026",
#       "B20001_026_margin",
#       "B20001_027",
#       "B20001_027_margin",
#       "B20001_028",
#       "B20001_028_margin",
#       "B20001_029",
#       "B20001_029_margin",
#       "B20001_030",
#       "B20001_030_margin",
#       "B20001_031",
#       "B20001_031_margin",
#       "B20001_032",
#       "B20001_032_margin",
#       "B20001_033",
#       "B20001_033_margin",
#       "B20001_034",
#       "B20001_034_margin",
#       "B20001_035",
#       "B20001_035_margin",
#       "B20001_036",
#       "B20001_036_margin",
#       "B20001_037",
#       "B20001_037_margin",
#       "B20001_038",
#       "B20001_038_margin",
#       "B20001_039",
#       "B20001_039_margin",
#       "B20001_040",
#       "B20001_040_margin",
#       "B20001_041",
#       "B20001_041_margin",
#       "B20001_042",
#       "B20001_042_margin",
#       "B20001_043",
#       "B20001_043_margin"
#     )
#     ]
# earnings_median <-
#   working_acs[
#     ,c(
#       "GEOID",
#       "B20002_001",
#       "B20002_001_margin",
#       "B20002_002",
#       "B20002_002_margin",
#       "B20002_003",
#       "B20002_003_margin"
#     )
#     ]
# employment_cat <- # divide 5 by 3
#   working_acs[
#     ,c(
#       "GEOID",
#       "B23025_001",
#       "B23025_001_margin",
#       "B23025_002",
#       "B23025_002_margin",
#       "B23025_003",
#       "B23025_003_margin",
#       "B23025_004",
#       "B23025_004_margin",
#       "B23025_005",
#       "B23025_005_margin",
#       "B23025_006",
#       "B23025_006_margin",
#       "B23025_007",
#       "B23025_007_margin"
#     )
#     ]
# home_value_cat <-
#   working_acs[
#     ,c(
#       "GEOID",
#       "B25075_001",
#       "B25075_001_margin",
#       "B25075_002",
#       "B25075_002_margin",
#       "B25075_003",
#       "B25075_003_margin",
#       "B25075_004",
#       "B25075_004_margin",
#       "B25075_005",
#       "B25075_005_margin",
#       "B25075_006",
#       "B25075_006_margin",
#       "B25075_007",
#       "B25075_007_margin",
#       "B25075_008",
#       "B25075_008_margin",
#       "B25075_009",
#       "B25075_009_margin",
#       "B25075_010",
#       "B25075_010_margin",
#       "B25075_011",
#       "B25075_011_margin",
#       "B25075_012",
#       "B25075_012_margin",
#       "B25075_013",
#       "B25075_013_margin",
#       "B25075_014",
#       "B25075_014_margin",
#       "B25075_015",
#       "B25075_015_margin",
#       "B25075_016",
#       "B25075_016_margin",
#       "B25075_017",
#       "B25075_017_margin",
#       "B25075_018",
#       "B25075_018_margin",
#       "B25075_019",
#       "B25075_019_margin",
#       "B25075_020",
#       "B25075_020_margin",
#       "B25075_021",
#       "B25075_021_margin",
#       "B25075_022",
#       "B25075_022_margin",
#       "B25075_023",
#       "B25075_023_margin",
#       "B25075_024",
#       "B25075_024_margin",
#       "B25075_025",
#       "B25075_025_margin",
#       "B25075_026",
#       "B25075_026_margin",
#       "B25075_027",
#       "B25075_027_margin"
#     )
#     ]
# health_insurance_cat <-
#   working_acs[
#     ,c(
#       "GEOID",
#       "B27010_001",
#       "B27010_001_margin",
#       "B27010_002",
#       "B27010_002_margin",
#       "B27010_003",
#       "B27010_003_margin",
#       "B27010_004",
#       "B27010_004_margin",
#       "B27010_005",
#       "B27010_005_margin",
#       "B27010_006",
#       "B27010_006_margin",
#       "B27010_007",
#       "B27010_007_margin",
#       "B27010_008",
#       "B27010_008_margin",
#       "B27010_009",
#       "B27010_009_margin",
#       "B27010_010",
#       "B27010_010_margin",
#       "B27010_011",
#       "B27010_011_margin",
#       "B27010_012",
#       "B27010_012_margin",
#       "B27010_013",
#       "B27010_013_margin",
#       "B27010_014",
#       "B27010_014_margin",
#       "B27010_015",
#       "B27010_015_margin",
#       "B27010_016",
#       "B27010_016_margin",
#       "B27010_017",
#       "B27010_017_margin",
#       "B27010_018",
#       "B27010_018_margin",
#       "B27010_019",
#       "B27010_019_margin",
#       "B27010_020",
#       "B27010_020_margin",
#       "B27010_021",
#       "B27010_021_margin",
#       "B27010_022",
#       "B27010_022_margin",
#       "B27010_023",
#       "B27010_023_margin",
#       "B27010_024",
#       "B27010_024_margin",
#       "B27010_025",
#       "B27010_025_margin",
#       "B27010_027",
#       "B27010_027_margin",
#       "B27010_028",
#       "B27010_028_margin",
#       "B27010_029",
#       "B27010_029_margin",
#       "B27010_030",
#       "B27010_030_margin",
#       "B27010_031",
#       "B27010_031_margin",
#       "B27010_032",
#       "B27010_032_margin",
#       "B27010_033",
#       "B27010_033_margin",
#       "B27010_034",
#       "B27010_034_margin",
#       "B27010_035",
#       "B27010_035_margin",
#       "B27010_036",
#       "B27010_036_margin",
#       "B27010_037",
#       "B27010_037_margin",
#       "B27010_038",
#       "B27010_038_margin",
#       "B27010_039",
#       "B27010_039_margin",
#       "B27010_040",
#       "B27010_040_margin",
#       "B27010_041",
#       "B27010_041_margin",
#       "B27010_042",
#       "B27010_042_margin",
#       "B27010_043",
#       "B27010_043_margin",
#       "B27010_044",
#       "B27010_044_margin",
#       "B27010_045",
#       "B27010_045_margin",
#       "B27010_046",
#       "B27010_046_margin",
#       "B27010_047",
#       "B27010_047_margin",
#       "B27010_048",
#       "B27010_048_margin",
#       "B27010_049",
#       "B27010_049_margin",
#       "B27010_050",
#       "B27010_050_margin",
#       "B27010_051",
#       "B27010_051_margin",
#       "B27010_052",
#       "B27010_052_margin",
#       "B27010_053",
#       "B27010_053_margin",
#       "B27010_054",
#       "B27010_054_margin",
#       "B27010_055",
#       "B27010_055_margin",
#       "B27010_056",
#       "B27010_056_margin",
#       "B27010_057",
#       "B27010_057_margin",
#       "B27010_058",
#       "B27010_058_margin",
#       "B27010_059",
#       "B27010_059_margin",
#       "B27010_060",
#       "B27010_060_margin",
#       "B27010_061",
#       "B27010_061_margin",
#       "B27010_062",
#       "B27010_062_margin",
#       "B27010_063",
#       "B27010_063_margin",
#       "B27010_064",
#       "B27010_064_margin",
#       "B27010_065",
#       "B27010_065_margin",
#       "B27010_066",
#       "B27010_066_margin"
#     )
#     ]
# home_value_median <-
#   working_acs[
#     ,c(
#       "GEOID",
#       "B25077_001",
#       "B25077_001_margin"
#     )
#     ]
# ling_iso <-
#   working_acs[
#     ,c(
#       "GEOID",
#       "C16002_001",
#       "C16002_001_margin",
#       "C16002_002",
#       "C16002_002_margin",
#       "C16002_003",
#       "C16002_003_margin",
#       "C16002_004",
#       "C16002_004_margin",
#       "C16002_005",
#       "C16002_005_margin",
#       "C16002_006",
#       "C16002_006_margin",
#       "C16002_007",
#       "C16002_007_margin",
#       "C16002_008",
#       "C16002_008_margin",
#       "C16002_009",
#       "C16002_009_margin",
#       "C16002_010",
#       "C16002_010_margin",
#       "C16002_011",
#       "C16002_011_margin",
#       "C16002_012",
#       "C16002_012_margin",
#       "C16002_013",
#       "C16002_013_margin",
#       "C16002_014",
#       "C16002_014_margin"
#     )
#     ]
# housing_tenure <-
#   working_acs[
#     ,c(
#       "GEOID",
#       "B25003_001",
#       "B25003_001_margin",
#       "B25003_002",
#       "B25003_002_margin",
#       "B25003_003",
#       "B25003_003_margin"
#     )
#     ]
# 
# # make list of all categories
# variables_of_interest <-
#   list(total_pop,
#        sex_cat,
#        sex_age_cat,
#        age_median,
#        race_cat,
#        education_cat,
#        poverty_ratio_cat,
#        income_house_cat,
#        income_house_median,
#        earnings_sex_cat,
#        earnings_median,
#        employment_cat,
#        home_value_cat,
#        home_value_median,
#        health_insurance_cat,
#        ling_iso,
#        housing_tenure)
# 
# 
# # merge different subset data together
# 
# # define merge function
# merge_dataframes <-
#   function(x, y) full_join(x, y, by = "GEOID")
# 
# # perform the merge function en masse
# acs_select <-
#   Reduce(
#     merge_dataframes,
#     variables_of_interest
#   )
# 
# # save select acs data
# save(acs_select,
#      file = "acs_select.rdata")
# 
# # merge select acs data with geography
# acs_select_geo <-
#   full_join(ks_tiger_table,
#             acs_select,
#             by = "GEOID")
# # View(acs_select_geo)
# 
# # save acs_select_geo
# save(acs_select_geo,
#      file = "acs_select_geo.rdata")
# 
# write.csv(acs_select_geo, file = "acs_select_geo.csv")
# 
# 
# # make data for sorting into categories
# acs_geo_cats <-
#   acs_select_geo
# 
# 
# 
# #### CONSTRUCT NECESSARY VARIABLES ####
# 
# # total population
# acs_geo_cats$pop_tot_B01001_001 <-
#   acs_geo_cats$B01001_001
# 
# # land area in km2
# acs_geo_cats$ALAND_KM <-
#   acs_geo_cats$ALAND/1000000
# 
# # population density (people/km2, ALAND in m2)
# acs_geo_cats$pop_dense_B01001_001_ALAND <-
#   (acs_geo_cats$B01001_001/
#      acs_geo_cats$ALAND_KM)
# 
# # median household income
# acs_geo_cats$income_house_median_B19013_001 <-
#   acs_geo_cats$B19013_001
# 
# # median age
# acs_geo_cats$age_median_B01002_001 <-
#   acs_geo_cats$B01002_001
# 
# # ages
# acs_geo_cats$age_under_5_B01001_003_027 <-
#   acs_geo_cats$B01001_003 + acs_geo_cats$B01001_027
# acs_geo_cats$age_5_to_9_B01001_004_028 <-
#   acs_geo_cats$B01001_004 + acs_geo_cats$B01001_028
# acs_geo_cats$age_10_to_14_B01001_005_029 <-
#   acs_geo_cats$B01001_005 + acs_geo_cats$B01001_029
# acs_geo_cats$age_15_to_17_B01001_006_030 <-
#   acs_geo_cats$B01001_006 + acs_geo_cats$B01001_030
# acs_geo_cats$age_18_to_19_B01001_007_031 <-
#   acs_geo_cats$B01001_007 + acs_geo_cats$B01001_031
# acs_geo_cats$age_20_B01001_008_032 <-
#   acs_geo_cats$B01001_008 + acs_geo_cats$B01001_032
# acs_geo_cats$age_21_B01001_009_033 <-
#   acs_geo_cats$B01001_009 + acs_geo_cats$B01001_033
# acs_geo_cats$age_22_to_24_B01001_010_034 <-
#   acs_geo_cats$B01001_010 + acs_geo_cats$B01001_034
# acs_geo_cats$age_25_to_29_B01001_011_035 <-
#   acs_geo_cats$B01001_011 + acs_geo_cats$B01001_035
# acs_geo_cats$age_30_to_34_B01001_012_036 <-
#   acs_geo_cats$B01001_012 + acs_geo_cats$B01001_036
# acs_geo_cats$age_35_to_39_B01001_013_037 <-
#   acs_geo_cats$B01001_013 + acs_geo_cats$B01001_037
# acs_geo_cats$age_40_to_44_B01001_014_038 <-
#   acs_geo_cats$B01001_014 + acs_geo_cats$B01001_038
# acs_geo_cats$age_45_to_49_B01001_015_039 <-
#   acs_geo_cats$B01001_015 + acs_geo_cats$B01001_039
# acs_geo_cats$age_50_to_54_B01001_016_040 <-
#   acs_geo_cats$B01001_016 + acs_geo_cats$B01001_040
# acs_geo_cats$age_55_to_59_B01001_017_041 <-
#   acs_geo_cats$B01001_017 + acs_geo_cats$B01001_041
# acs_geo_cats$age_60_to_61_B01001_018_042 <-
#   acs_geo_cats$B01001_018 + acs_geo_cats$B01001_042
# acs_geo_cats$age_62_to_64_B01001_019_043 <-
#   acs_geo_cats$B01001_019 + acs_geo_cats$B01001_043
# acs_geo_cats$age_65_to_66_B01001_020_044 <-
#   acs_geo_cats$B01001_020 + acs_geo_cats$B01001_044
# acs_geo_cats$age_67_to_69_B01001_021_045 <-
#   acs_geo_cats$B01001_021 + acs_geo_cats$B01001_045
# acs_geo_cats$age_70_to_74_B01001_022_046 <-
#   acs_geo_cats$B01001_022 + acs_geo_cats$B01001_046
# acs_geo_cats$age_75_to_79_B01001_023_047 <-
#   acs_geo_cats$B01001_023 + acs_geo_cats$B01001_047
# acs_geo_cats$age_80_to_84_B01001_024_048 <-
#   acs_geo_cats$B01001_024 + acs_geo_cats$B01001_048
# acs_geo_cats$age_85_to_inf_B01001_025_049 <-
#   acs_geo_cats$B01001_025 + acs_geo_cats$B01001_049
# 
# acs_geo_cats$age_0_to_14 <-
#   acs_geo_cats$age_under_5_B01001_003_027 +
#   acs_geo_cats$age_5_to_9_B01001_004_028 +
#   acs_geo_cats$age_10_to_14_B01001_005_029
# 
# acs_geo_cats$age_0_to_17 <-
#   acs_geo_cats$age_under_5_B01001_003_027 +
#   acs_geo_cats$age_5_to_9_B01001_004_028 +
#   acs_geo_cats$age_10_to_14_B01001_005_029 +
#   acs_geo_cats$age_15_to_17_B01001_006_030
# 
# acs_geo_cats$age_0_to_19 <-
#   acs_geo_cats$age_under_5_B01001_003_027 +
#   acs_geo_cats$age_5_to_9_B01001_004_028 +
#   acs_geo_cats$age_10_to_14_B01001_005_029 +
#   acs_geo_cats$age_15_to_17_B01001_006_030 +
#   acs_geo_cats$age_18_to_19_B01001_007_031
# 
# acs_geo_cats$age_5_to_14 <-
#   acs_geo_cats$age_5_to_9_B01001_004_028 +
#   acs_geo_cats$age_10_to_14_B01001_005_029
# 
# acs_geo_cats$age_5_to_17 <-
#   acs_geo_cats$age_5_to_9_B01001_004_028 +
#   acs_geo_cats$age_10_to_14_B01001_005_029 +
#   acs_geo_cats$age_15_to_17_B01001_006_030
# 
# acs_geo_cats$age_5_to_19 <-
#   acs_geo_cats$age_5_to_9_B01001_004_028 +
#   acs_geo_cats$age_10_to_14_B01001_005_029 +
#   acs_geo_cats$age_15_to_17_B01001_006_030 +
#   acs_geo_cats$age_18_to_19_B01001_007_031
# 
# acs_geo_cats$age_10_to_17 <-
#   acs_geo_cats$age_10_to_14_B01001_005_029 +
#   acs_geo_cats$age_15_to_17_B01001_006_030
# 
# acs_geo_cats$age_15_to_64 <-
#   acs_geo_cats$age_15_to_17_B01001_006_030 +
#   acs_geo_cats$age_18_to_19_B01001_007_031 +
#   acs_geo_cats$age_20_B01001_008_032 +
#   acs_geo_cats$age_21_B01001_009_033 +
#   acs_geo_cats$age_22_to_24_B01001_010_034 +
#   acs_geo_cats$age_25_to_29_B01001_011_035 +
#   acs_geo_cats$age_30_to_34_B01001_012_036 +
#   acs_geo_cats$age_35_to_39_B01001_013_037 +
#   acs_geo_cats$age_40_to_44_B01001_014_038 +
#   acs_geo_cats$age_45_to_49_B01001_015_039 +
#   acs_geo_cats$age_50_to_54_B01001_016_040 +
#   acs_geo_cats$age_55_to_59_B01001_017_041 +
#   acs_geo_cats$age_60_to_61_B01001_018_042 +
#   acs_geo_cats$age_62_to_64_B01001_019_043
# 
# acs_geo_cats$age_18_to_64 <-
#   acs_geo_cats$age_18_to_19_B01001_007_031 +
#   acs_geo_cats$age_20_B01001_008_032 +
#   acs_geo_cats$age_21_B01001_009_033 +
#   acs_geo_cats$age_22_to_24_B01001_010_034 +
#   acs_geo_cats$age_25_to_29_B01001_011_035 +
#   acs_geo_cats$age_30_to_34_B01001_012_036 +
#   acs_geo_cats$age_35_to_39_B01001_013_037 +
#   acs_geo_cats$age_40_to_44_B01001_014_038 +
#   acs_geo_cats$age_45_to_49_B01001_015_039 +
#   acs_geo_cats$age_50_to_54_B01001_016_040 +
#   acs_geo_cats$age_55_to_59_B01001_017_041 +
#   acs_geo_cats$age_60_to_61_B01001_018_042 +
#   acs_geo_cats$age_62_to_64_B01001_019_043
# 
# acs_geo_cats$age_20_to_64 <-
#   acs_geo_cats$age_20_B01001_008_032 +
#   acs_geo_cats$age_21_B01001_009_033 +
#   acs_geo_cats$age_22_to_24_B01001_010_034 +
#   acs_geo_cats$age_25_to_29_B01001_011_035 +
#   acs_geo_cats$age_30_to_34_B01001_012_036 +
#   acs_geo_cats$age_35_to_39_B01001_013_037 +
#   acs_geo_cats$age_40_to_44_B01001_014_038 +
#   acs_geo_cats$age_45_to_49_B01001_015_039 +
#   acs_geo_cats$age_50_to_54_B01001_016_040 +
#   acs_geo_cats$age_55_to_59_B01001_017_041 +
#   acs_geo_cats$age_60_to_61_B01001_018_042 +
#   acs_geo_cats$age_62_to_64_B01001_019_043
# 
# acs_geo_cats$age_65_plus <-
#   acs_geo_cats$age_65_to_66_B01001_020_044 +
#   acs_geo_cats$age_67_to_69_B01001_021_045 +
#   acs_geo_cats$age_70_to_74_B01001_022_046 +
#   acs_geo_cats$age_75_to_79_B01001_023_047 +
#   acs_geo_cats$age_80_to_84_B01001_024_048 +
#   acs_geo_cats$age_85_to_inf_B01001_025_049
# 
# # age percents
# acs_geo_cats$age_under_5_percent_B01001 <-
#   acs_geo_cats$age_under_5_B01001_003_027 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_0_to_14_percent_B01001 <-
#   acs_geo_cats$age_0_to_14 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_0_to_17_percent_B01001 <-
#   acs_geo_cats$age_0_to_17 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_0_to_19_percent_B01001 <-
#   acs_geo_cats$age_0_to_19 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_5_to_14_percent_B01001 <-
#   acs_geo_cats$age_5_to_14 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_5_to_17_percent_B01001 <-
#   acs_geo_cats$age_5_to_17 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_5_to_19_percent_B01001 <-
#   acs_geo_cats$age_5_to_19 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_10_to_17_percent_B01001 <-
#   acs_geo_cats$age_10_to_17 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_15_to_64_percent_B01001 <-
#   acs_geo_cats$age_15_to_64 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_18_to_64_percent_B01001 <-
#   acs_geo_cats$age_18_to_64 /
# 
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_20_to_64_percent_B01001 <-
#   acs_geo_cats$age_20_to_64 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# acs_geo_cats$age_65_plus_percent_B01001 <-
#   acs_geo_cats$age_65_plus /
#   acs_geo_cats$pop_tot_B01001_001
# 
# # unemployment rate
# acs_geo_cats$employment_tot_16_over_B23025_001 <-
#   acs_geo_cats$B23025_001
# acs_geo_cats$labor_force_tot_B23025_002 <-
#   acs_geo_cats$B23025_002
# acs_geo_cats$civie_labor_force_B23025_003 <-
#   acs_geo_cats$B23025_003
# acs_geo_cats$civie_employed_B23025_004 <-
#   acs_geo_cats$B23025_004
# acs_geo_cats$civie_unemployed_B23025_005 <-
#   acs_geo_cats$B23025_005
# acs_geo_cats$armed_forces_B23025_006 <-
#   acs_geo_cats$B23025_006
# 
# acs_geo_cats$unemployed_percent_B23025_005_003 <-
#   acs_geo_cats$B23025_005/
#   acs_geo_cats$B23025_003
# 
# # median household value
# acs_geo_cats$house_value_median_B25077_001 <-
#   acs_geo_cats$B25077_001
# 
# # percent non_Hispanic_white
# acs_geo_cats$white_non_hisp_lat_percent_B03002_003_001 <-
#   acs_geo_cats$B03002_003 /
#   acs_geo_cats$B03002_001
# 
# # percent Native (includes Hispanic/Latinx)
# acs_geo_cats$aian_percent_B03002_005_015_001 <-
#   (acs_geo_cats$B03002_005 +
#      acs_geo_cats$B03002_015) /
#   acs_geo_cats$B03002_001
# 
# # percent Hispanic/Latino
# acs_geo_cats$hisp_lat_percent_B03002_012_001 <- 
#   (acs_geo_cats$B03002_012 / 
#      acs_geo_cats$B03002_001)
# 
# # percent of population with high school education or more
# acs_geo_cats$education_high_school_plus_percent_B15003_017_to_025 <-
#   (
#     (
#       acs_geo_cats$B15003_017 +
#         acs_geo_cats$B15003_018 +
#         acs_geo_cats$B15003_019 +
#         acs_geo_cats$B15003_020 +
#         acs_geo_cats$B15003_021 +
#         acs_geo_cats$B15003_022 +
#         acs_geo_cats$B15003_023 +
#         acs_geo_cats$B15003_024 +
#         acs_geo_cats$B15003_025
#     )
#     /acs_geo_cats$B15003_001
#   )
# 
# # linguistic isolation
# acs_geo_cats$ling_iso_total_households_C16002_001 <-
#   acs_geo_cats$C16002_001
# acs_geo_cats <-
#   acs_geo_cats %>%
#   mutate(
#     limited_english_percent_C16002_004_007_010_013_001 =
#       (C16002_004 +
#          C16002_007 +
#          C16002_010 +
#          C16002_013) /
#       C16002_001)
# 
# # housing tenure
# acs_geo_cats$housing_occupied_total_B25003_001 <-
#   acs_geo_cats$B25003_001
# acs_geo_cats$housing_owner_occupied_B25003_002 <-
#   acs_geo_cats$B25003_002
# acs_geo_cats$housing_renter_occupied_B25003_003 <-
#   acs_geo_cats$B25003_003
# acs_geo_cats$owner_occupy_percent_B25003_002_001 <-
#   acs_geo_cats$B25003_002 /
#   acs_geo_cats$B25003_001
# acs_geo_cats$renter_occupy_percent_B25003_003_001 <-
#   acs_geo_cats$B25003_003 /
#   acs_geo_cats$B25003_001
# 
# # percent below poverty
# acs_geo_cats$pop_for_pov_C17002_001 <-
#   acs_geo_cats$C17002_001
# acs_geo_cats$number_below_0p5_pov_C17002_002 <-
#   acs_geo_cats$C17002_002
# acs_geo_cats$number_0p5_to_0p99_pov_C17002_003 <-
#   acs_geo_cats$C17002_003
# acs_geo_cats$number_1p0_to_1p24_pov_C17002_004 <-
#   acs_geo_cats$C17002_004
# acs_geo_cats$number_1p25_to_1p49_pov_C17002_005 <-
#   acs_geo_cats$C17002_005
# acs_geo_cats$number_1p5_to_1p84_pov_C17002_006 <-
#   acs_geo_cats$C17002_006
# acs_geo_cats$number_1p85_to_1p99_pov_C17002_007 <-
#   acs_geo_cats$C17002_007
# acs_geo_cats$number_2p0_plus_pov_C17002_008 <-
#   acs_geo_cats$C17002_008
# 
# acs_geo_cats$poverty_below_100_percent_C17002_002_003_001 <-
#   (acs_geo_cats$C17002_002 +
#      acs_geo_cats$C17002_003) /
#   acs_geo_cats$pop_for_pov_C17002_001
# 
# acs_geo_cats$poverty_below_150_percent_C17002_002_to_005_001 <-
#   (acs_geo_cats$C17002_002 +
#      acs_geo_cats$C17002_003 +
#      acs_geo_cats$C17002_004 +
#      acs_geo_cats$C17002_005) /
#   acs_geo_cats$pop_for_pov_C17002_001
# 
# acs_geo_cats$poverty_below_200_percent__C17002_002_to_007_001 <-
#   (acs_geo_cats$C17002_002 +
#      acs_geo_cats$C17002_003 +
#      acs_geo_cats$C17002_004 +
#      acs_geo_cats$C17002_005 +
#      acs_geo_cats$C17002_006 +
#      acs_geo_cats$C17002_007) /
#   acs_geo_cats$pop_for_pov_C17002_001
# 
# # percent female
# acs_geo_cats$female_number_B01001_026 <-
#   acs_geo_cats$B01001_026
# acs_geo_cats$male_number_B01001_002 <-
#   acs_geo_cats$B01001_002
# acs_geo_cats$female_percent_B01001_026_001 <-
#   acs_geo_cats$female_number_B01001_026 /
#   acs_geo_cats$pop_tot_B01001_001
# 
# # percent health insurance
# acs_geo_cats$pop_for_health_insurance_B27010_001 <-
#   acs_geo_cats$B27010_001
# acs_geo_cats$no_health_insurance_percent_B27010_017_033_050_066 <-
#   (acs_geo_cats$B27010_017 +
#      acs_geo_cats$B27010_033 +
#      acs_geo_cats$B27010_050 +
#      acs_geo_cats$B27010_066) /
#   acs_geo_cats$pop_for_health_insurance_B27010_001
# 
# # earnings median
# acs_geo_cats$earnings_median_B20002_001 <-
#   acs_geo_cats$B20002_001
# 
# # earnings categories
# acs_geo_cats$earnings_1_to_2499_003_0024 <-
#   acs_geo_cats$B20001_003 + acs_geo_cats$B20001_024
# acs_geo_cats$earnings_2500_to_4999_004_025 <-
#   acs_geo_cats$B20001_004 + acs_geo_cats$B20001_025
# acs_geo_cats$earnings_5000_to_7499_005_026 <-
#   acs_geo_cats$B20001_005 + acs_geo_cats$B20001_026
# acs_geo_cats$earnings_7500_to_9999_006_027 <-
#   acs_geo_cats$B20001_006 + acs_geo_cats$B20001_027
# acs_geo_cats$earnings_10K_to_12499_007_028 <-
#   acs_geo_cats$B20001_007 + acs_geo_cats$B20001_028
# acs_geo_cats$earnings_12500_to_14999_008_029 <-
#   acs_geo_cats$B20001_008 + acs_geo_cats$B20001_029
# acs_geo_cats$earnings_15K_to_17499_009_030 <-
#   acs_geo_cats$B20001_009 + acs_geo_cats$B20001_030
# acs_geo_cats$earnings_17500_to_19999_010_031 <-
#   acs_geo_cats$B20001_010 + acs_geo_cats$B20001_031
# acs_geo_cats$earnings_20K_to_22499_011_032 <-
#   acs_geo_cats$B20001_011 + acs_geo_cats$B20001_032
# acs_geo_cats$earnings_22500_to_24999_012_033 <-
#   acs_geo_cats$B20001_012 + acs_geo_cats$B20001_033
# acs_geo_cats$earnings_25K_to_29999_013_034 <-
#   acs_geo_cats$B20001_013 + acs_geo_cats$B20001_034
# acs_geo_cats$earnings_30K_to_34999_014_035 <-
#   acs_geo_cats$B20001_014 + acs_geo_cats$B20001_035
# acs_geo_cats$earnings_35K_to_39999_015_036 <-
#   acs_geo_cats$B20001_015 + acs_geo_cats$B20001_036
# acs_geo_cats$earnings_40K_to_44999_016_037 <-
#   acs_geo_cats$B20001_016 + acs_geo_cats$B20001_037
# acs_geo_cats$earnings_45K_to_49999_017_038 <-
#   acs_geo_cats$B20001_017 + acs_geo_cats$B20001_038
# acs_geo_cats$earnings_50K_to_54999_018_039 <-
#   acs_geo_cats$B20001_018 + acs_geo_cats$B20001_039
# acs_geo_cats$earnings_55K_to_64999_019_040 <-
#   acs_geo_cats$B20001_019 + acs_geo_cats$B20001_040
# acs_geo_cats$earnings_65K_to_74999_020_041 <-
#   acs_geo_cats$B20001_020 + acs_geo_cats$B20001_041
# acs_geo_cats$earnings_75K_to_99999_021_042 <-
#   acs_geo_cats$B20001_021 + acs_geo_cats$B20001_042
# acs_geo_cats$earnings_100K_to_infinity_022_043 <-
#   acs_geo_cats$B20001_022 + acs_geo_cats$B20001_043
# 
# # save constructed variables
# save(acs_geo_cats,
#      file = "acs_geo_cats.rdata")
# 
# # make constructed variables into .csv
# write.csv(acs_geo_cats,
#           file = "acs_geo_cats.csv")
# 
# # # load constructed variables to avoid the above
# # load(file = "acs_geo_cats.rdata")
# 
# 
# 
# # #### WELL DATA ####
# # 
# # # make well data file for cleaning
# # ks_clean <-
# #   ks_wells_2018_11_01
# # 
# # 
# #
# # #### formatting changes in main well data ####
# # 
# # # convert dates to dates
# # ks_clean$permit_as_date  <-
# #   as.Date(ks_clean$PERMIT, "%d-%b-%Y") # permit date
# # ks_clean$spud_as_date  <-
# #   as.Date(ks_clean$SPUD, "%d-%b-%Y") # spud date
# # ks_clean$completion_as_date  <-
# #   as.Date(ks_clean$COMPLETION, "%d-%b-%Y") # completion date
# # ks_clean$plugging_as_date  <-
# #   as.Date(ks_clean$PLUGGING, "%d-%b-%Y") # plugging date
# # ks_clean$modified_as_date  <-
# #   as.Date(ks_clean$MODIFIED, "%d-%b-%Y") # modified date
# # ks_clean$API_NUMBER  <-
# #   as.character(ks_clean$API_NUMBER) # make API into a character
# # 
# # # make NAs for blank apis
# # ks_clean$API_NUMBER[ks_clean$API_NUMBER == ""] <- NA
# # 
# # # separate well api into event codes
# # ks_clean <-
# #   ks_clean %>%
# #   separate(API_NUMBER,
# #            into = c("API_NUMBER_SIMPLE", "EVENT"),
# #            sep = 12,
# #            remove = F)
# # 
# # # remove extraneous dashes from event codes
# # ks_clean$EVENT <-
# #   substring(ks_clean$EVENT, 2)
# # 
# # # make NAs for blank dates
# # ks_clean$PERMIT[
# #   ks_clean$PERMIT == ""
# #   ] <- NA
# # ks_clean$SPUD[
# #   ks_clean$SPUD == ""
# #   ] <- NA
# # ks_clean$COMPLETION[
# #   ks_clean$COMPLETION == ""
# #   ] <- NA
# # ks_clean$PLUGGING[
# #   ks_clean$PLUGGING == ""
# #   ] <- NA
# # ks_clean$MODIFIED[
# #   ks_clean$MODIFIED == ""
# #   ] <- NA
# # 
# # 
# # 
# # #### add analysis categories ####
# # 
# # # categorizing the well as SWD ('swd'), INJ ('inj'), or Class I ('ci')
# # ks_clean$well_type <- NA
# # 
# # # categorizing the well as in the uic database ('yes') or no ('no')
# # ks_clean$uic <- NA
# # 
# # # categorizing the well as active ('active'), inactive ('inactive'),
# # # drilled ('drill'), future ('future'), never drilled or used as a
# # # saltwater disposal well ('ab_loc'), or canceled midway through drilling
# # # or conversion or hole lost ('midway')
# # ks_clean$activity <- NA
# # 
# # # categorizing the well as plugged ('has_plug_status')
# # # or unknown ('no_plug_status') by STATUS or STATUS2
# # ks_clean$plug_status <- NA
# # 
# # # categorizing the well as plugged or not by plugging_as_date
# # ks_clean$plug_date_binary <- NA
# # 
# # # categorizing as plugged or not by the above two categories
# # ks_clean$plug_overall <- NA
# # 
# # # classifying as having or not having an api ('yes' or 'no')
# # ks_clean$has_api <- NA
# # 
# # # whether the well was classified via either
# # # (1) a STATUS of "SWD" or "SWD-P&A" ('status');
# # # (2) a status2 of "Converted to SWD Well" ('status2'); or
# # # (3) manual comment review ('comments')
# # ks_clean$assignment_source <- NA
# # 
# # # classifies the detailed well type (see data documentation for possible values)
# # ks_clean$detailed_well_type <- NA
# # 
# # # save results of all these conversions
# # save(ks_clean,
# #      file = "ks_clean.rdata")
# # 
# # # load ks_clean so you don't have to do all this again
# # load(file = "ks_clean.rdata")
# # 
# # # make working file
# # ks_working <- ks_clean
# # 
# # # order wells by API
# # ks_working <-
# # ks_working[
# #    order(ks_working$API_NUMBER),
# #    ]
# #
# # # save the working file
# # save(ks_working, file = "ks_working.rdata")
# #
# # # load the working file
# # load(file = "ks_working.rdata")
# # # View(ks_working[1:200,])
# #
# # # make set for spatial joining in ArcGIS
# # ks_all_wells_for_map <-
# #   ks_working[,c("KID",
# #                 "API_NUMBER",
# #                 "LATITUDE",
# #                 "LONGITUDE")]
# #
# # # export mapping data
# # write.csv(ks_all_wells_for_map,
# #           file = "ks_all_wells_for_map.csv")
# #
# #
# #
# # #### HERE MUST DO SPATIAL JOIN IN ARCGIS ####
# #
# # # import spatial join (within!) results back
# # ks_join_wells_block_groups <-
# #   fread(file = "ks_join_wells_block_groups_for_r_2019_01_06.txt",
# #         stringsAsFactors = FALSE, 
# #         colClasses = list(character = 'GEOID'))
# # 
# # # join data with GEOID back to full data
# # ks_working_with_block_groups <-
# #   join(ks_working,
# #        ks_join_wells_block_groups,
# #        by = "KID",
# #        type = "full")
# # 
# # # View(ks_working_with_block_groups)
# # 
# # save(ks_working_with_block_groups,
# #      file = "ks_working_with_block_groups.rdata")
# # write.csv(ks_working_with_block_groups,
# #      file = "ks_working_with_block_groups.csv")
# 
# # load file to start from here
# load(file = "ks_working_with_block_groups.rdata")
# 
# 
# 
# # #### investigating rows based on comments ####
# # ks_potential_disposal_comments <-   # make vector of just comments
# #   ks_working_with_block_groups$COMMENTS
# # 
# # # below identifies rows with comments containing the strings 
# # # "swd", "disp", "salt", "class", or "waste"
# # positions_of_possible_comments_to_include <-
# #   grep("swd|disp|salt|class|waste", 
# #        ks_potential_disposal_comments, 
# #        ignore.case = TRUE)
# # 
# # comments_to_review <-   # makes vector of identified comments
# #   ks_working_with_block_groups$COMMENTS[
# #     positions_of_possible_comments_to_include
# #     ]
# # 
# # # makes .csv file of identified comments
# # write.csv(comments_to_review,   # makes .csv file of identified comments
# #           file = "comments_to_review.csv")
# # 
# # # makes dataframe of entire rows matching comments
# # rows_requiring_comment_investigation <-
# #   ks_working_with_block_groups[which(ks_working_with_block_groups$COMMENTS %in% 
# #                                        comments_to_review),]
# # 
# # # make dataframe of most relevant columns of entire rows matching comments
# # rows_requiring_comment_investigation_simple <- 
# #   rows_requiring_comment_investigation[
# #     ,c("KID",
# #        "API_NUMBER",
# #        "activity",
# #        "well_type",
# #        "assignment_source",
# #        "detailed_well_type",
# #        "assignment_notes",
# #        "comments_examined",
# #        "kgs_available_documents_verified",
# #        "STATUS",
# #        "STATUS2",
# #        "COMMENTS"
# #     )
# #     ]
# # 
# # # make .csv files of rows matching comments and simple rows matching comments
# # write.csv(rows_requiring_comment_investigation,
# #           file = "rows_requiring_comment_investigation.csv")
# # write.csv(rows_requiring_comment_investigation_simple, 
# #           file = "rows_requiring_comment_investigation_simple.csv")
# 
# 
# 
# # #### manual well assignment space ####
# # cat("Please assign the ambiguous wells manually in an Excel file.  
# # Afterward, find the window that just opened and click on the 'continue' 
# # button to continue the program.  The next prompt asks you to choose the file 
# # with the well assignments.")
# # my_wait() # runs function to force the program to wait for input
# # 
# # # import manual assignments
# # manual_well_assignment_csv_file <- # asks user to choose the correct .csv file
# #   file.choose()
# # 
# # raw_manual_well_assignments_dataframe <- # converts above .csv to a dataframe
# #   read.csv(manual_well_assignment_csv_file)
# # 
# # raw_manual_well_assignments_dataframe <- # converts above .csv to a dataframe
# #   read.csv(
# #     file =
# #       "rows_requiring_comment_investigation_simple_2019_01_04_back_to_r.csv")
# # 
# # # View(raw_manual_well_assignments_dataframe) # view the import
# # save(raw_manual_well_assignments_dataframe,   # save the dataframe
# #      file = 'raw_manual_well_assignments_dataframe.rdata')
# 
# # load the dataframe to start from here
# load(file = "raw_manual_well_assignments_dataframe.rdata")
# 
# 
# 
# #### creating the semi-final well list #### 
# 
# # make vector of status1s that mean swd well
# swd_status1s <-   
#   sort(c("OTHER(1O&1SWD)",
#          "OTHER(CBM/SWD)",
#          "OTHER(OIL,SWD)",
#          "OTHER(SWD-P&A)",
#          "OTHER-P&A(OIL-SWD)",
#          "SWD",
#          "SWD-P&A"))
# 
# # make dataframe of wells selected due to status1
# ks_swd_statii_one <- 
#   ks_working_with_block_groups[
#     which(
#       ks_working_with_block_groups$STATUS %in%
#         swd_status1s
#     ),
#     ]
# # View(ks_swd_statii_one)
# 
# # make dataframe of wells selected due to status2
# ks_swd_statii_two <- 
#   ks_working_with_block_groups[
#     which(
#       ks_working_with_block_groups$STATUS2 == "Converted to SWD Well" 
#       & ks_working_with_block_groups$STATUS %notin% swd_status1s),
#     ]
# 
# # View(ks_swd_statii_two)
# # table(ks_swd_statii_two$STATUS)
# 
# # pull the manual assignments 
# # View(raw_manual_well_assignments_dataframe)
# raw_assignments_exclude_swd <- 
#   raw_manual_well_assignments_dataframe[
#     which(raw_manual_well_assignments_dataframe$STATUS %notin% 
#             swd_status1s & 
#             raw_manual_well_assignments_dataframe$STATUS2 != 
#             "Converted to SWD Well"),
#     ]
# 
# # View(raw_assignments_exclude_swd)
# 
# 
# 
# #### make big dataframe of all wells pulled for at least one reason ####
# # list all the kids
# kids_of_everything <- 
#   c(ks_swd_statii_one$KID, 
#          ks_swd_statii_two$KID, 
#          raw_assignments_exclude_swd$KID) 
# 
# # nrow(ks_swd_statii_one)
# # nrow(ks_swd_statii_two)
# # nrow(raw_assignments_exclude_swd)
# # length(kids_of_everything)
# 
# # View(ks_swd_statii_one$KID)
# # View(ks_swd_statii_two$KID)
# # View(raw_assignments_exclude_swd$KID)
# 
# # pull the wells
# top_of_the_flowchart <- 
#   ks_working_with_block_groups[which(
#     ks_working_with_block_groups$KID %in% 
#       kids_of_everything),]
# # View(top_of_the_flowchart)
# 
# # pull those manually assigned as swd
# raw_assignments_man_swd <-   
#   raw_assignments_exclude_swd[
#     which(raw_assignments_exclude_swd$man_well_type_swd == "yes"),]
# # nrow(raw_assignments_man_swd)   # n = 108
# 
# raw_assignments_man_not_swd <- 
#   raw_assignments_exclude_swd[
#     which(raw_assignments_exclude_swd$man_well_type_swd != "yes"),]
# # View(raw_assignments_man_swd)
# 
# # separate well api into event codes
# raw_assignments_man_swd <- 
#   raw_assignments_man_swd %>% 
#   separate(API_NUMBER, 
#            into = c("API_NUMBER_SIMPLE", 
#                     "EVENT"), 
#            sep = 12, 
#            remove = F)
# 
# # remove extraneous dashes from event codes
# raw_assignments_man_swd$EVENT <- 
#   substring(raw_assignments_man_swd$EVENT, 2)
# # View(raw_assignments_man_swd)
# 
# 
# 
# #### find APIs of status II in status I and delete those
# # APIs of statusIs
# status1_apis <- 
#   as.character(unique(ks_swd_statii_one$API_NUMBER_SIMPLE))
# 
# status2_apis <- 
#   as.character(unique(ks_swd_statii_two$API_NUMBER_SIMPLE))
# 
# # View(status1_apis)
# 
# # View(status2_apis)
# 
# 
# # make true/false vector to match status2 apis to status1 apis
# s2s_in_s1s_T_F <- status2_apis %in% status1_apis
# 
# # list of status2 apis in status1 apis
# s2_apis_in_s1_apis <- status2_apis[s2s_in_s1s_T_F]
# 
# ks_swd_statii_two_not_in_statii_one <- 
#   ks_swd_statii_two[which(
#     ks_swd_statii_two$API_NUMBER_SIMPLE %notin% 
#       s2_apis_in_s1_apis),]
# 
# # make vector of kids removed at this step for later analysis (n = 3157)
# kids_of_removed_s2_well_apis_overlap_with_s1 <- 
#   setdiff(ks_swd_statii_two$KID, 
#           ks_swd_statii_two_not_in_statii_one$KID)
# 
# 
# 
# # View(ks_swd_statii_two_not_in_statii_one)
# 
# 
# 
# #### find APIs of manual assignments in status Is and delete those
# # pull manual assignment apis
# raw_assignment_apis <- 
#   as.character(unique(raw_assignments_man_swd$API_NUMBER_SIMPLE))
# 
# # make true/false vector to match manual apis to status1 apis
# raws_in_s1s_T_F <- 
#   raw_assignment_apis %in% status1_apis
# 
# # list of manual assignment apis in status1 apis
# raw_apis_in_s1_apis <- 
#   raw_assignment_apis[raws_in_s1s_T_F]
# 
# ks_swd_manual_not_in_statii_one <- 
#   raw_assignments_man_swd[which(
#     raw_assignments_man_swd$API_NUMBER_SIMPLE %notin% 
#       raw_apis_in_s1_apis),]
# 
# # kids of removed wells (n = 37)
# kids_of_removed_man_well_apis_overlap_with_s1 <- 
#   setdiff(raw_assignments_man_swd$KID, 
#           ks_swd_manual_not_in_statii_one$KID)
# 
# 
# # View(ks_swd_manual_not_in_statii_one)
# 
# 
# 
# #### find APIs of manual assignments in status2s and delete those
# # pull manual assignment apis
# ks_swd_manual_not_in_statii_one_apis <- 
#   as.character(unique(ks_swd_manual_not_in_statii_one$API_NUMBER_SIMPLE))
# 
# # make true/false vector to match manual apis to status2 apis
# raws_in_s2s_T_F <- 
#   ks_swd_manual_not_in_statii_one_apis %in% status2_apis
# 
# # list of manual assignment apis in status2 apis
# raw_apis_in_s2_apis <- 
#   ks_swd_manual_not_in_statii_one_apis[raws_in_s2s_T_F]
# # View(raw_apis_in_s1_apis)
# 
# ks_swd_manual_not_in_statii_two <- 
#   ks_swd_manual_not_in_statii_one[which(
#     ks_swd_manual_not_in_statii_one$API_NUMBER_SIMPLE %notin% 
#       raw_apis_in_s2_apis),]
# 
# # View(ks_swd_manual_not_in_statii_two)   # DON'T USE THIS KEEP MANUAL WELL!
# 
# # manually examine sole well in common
# # View(ks_working[which(ks_working$KID %in% c(1030278155, 1002886382)),])
# # want manual well, 1030278155
# 
# # delete unwanted well 1002886382
# ks_swd_statii_two_not_in_statii_one_or_man <- 
#   ks_swd_statii_two_not_in_statii_one[which(
#     ks_swd_statii_two_not_in_statii_one$KID != 
#       "1002886382"
#   ),]
# # View(ks_swd_statii_two_not_in_statii_one_or_man)
# 
# # ks_swd_s1_kids_for_master
# ks_swd_s1_kids_for_master_review <- 
#   ks_swd_statii_one$KID
# length(ks_swd_s1_kids_for_master_review)
# 
# # ks_swd_s2_kids_for_master
# ks_swd_s2_kids_for_master_review <- 
#   ks_swd_statii_two_not_in_statii_one_or_man$KID
# length(ks_swd_s2_kids_for_master_review)
# 
# # ks_swd_man_kids_for_master
# ks_swd_man_kids_for_master_review <- 
#   ks_swd_manual_not_in_statii_one$KID
# length(ks_swd_man_kids_for_master_review)
# 
# # kids for master list
# kids_for_master_list_review <- 
#   c(ks_swd_s1_kids_for_master_review, 
#     ks_swd_s2_kids_for_master_review, 
#     ks_swd_man_kids_for_master_review)
# 
# # View(kids_for_master_list_review)
# 
# # pull rows corresponding to above apis
# ks_swd_master_dataframe_for_review <- 
#   ks_working_with_block_groups[
#     which(
#       ks_working_with_block_groups$KID %in% 
#         kids_for_master_list_review
#     ),
#     ]
# 
# # View(ks_swd_master_dataframe_for_review)
# 
# # label rows by origin
# ks_swd_master_dataframe_for_review$origin <- NA
# 
# ks_swd_master_dataframe_for_review <-   # label s1s
#   within(ks_swd_master_dataframe_for_review, 
#          origin[KID %in% 
#                   ks_swd_s1_kids_for_master_review] <- "s1")
# 
# ks_swd_master_dataframe_for_review <-   # label s2s
#   within(ks_swd_master_dataframe_for_review, 
#          origin[KID %in% 
#                   ks_swd_s2_kids_for_master_review] <- "s2")
# 
# ks_swd_master_dataframe_for_review <-   # label manuals
#   within(ks_swd_master_dataframe_for_review, 
#          origin[KID %in% 
#                   ks_swd_man_kids_for_master_review] <- "man")
# 
# # View(ks_swd_master_dataframe_for_review)
# 
# 
# 
# #### find duplicates by latitude and longitude ####
# ks_swd_master_lat_long_dup_index <- 
#   duplicated(ks_swd_master_dataframe_for_review[c("LATITUDE","LONGITUDE")]) | 
#   duplicated(ks_swd_master_dataframe_for_review[c("LATITUDE","LONGITUDE")], 
#              fromLast = TRUE)
# 
# ks_swd_master_lat_long_dup <-   # make dataframe of the duplicates
#   ks_swd_master_dataframe_for_review[ks_swd_master_lat_long_dup_index, ]
# 
# # View(ks_swd_master_lat_long_dup)
# 
# # pull lat/long dup rows with s2 or manual origins
# ks_swd_lat_long_dup_s2_man <- 
#   ks_swd_master_lat_long_dup[which(
#     ks_swd_master_lat_long_dup$origin %in% c("s2", "man")
#   ),]
# 
# # pull lat/long dup rows with s1 origins
# ks_swd_lat_long_dup_s1 <- 
#   ks_swd_master_lat_long_dup[which(
#     ks_swd_master_lat_long_dup$origin == 's1'
#   ),]
# 
# # make s2/man dataframe simpler, pulling bare minimum of columns
# ks_swd_lat_long_s2_man_simple <- 
#   ks_swd_lat_long_dup_s2_man[,c("KID", "LATITUDE", "LONGITUDE", "origin")]
# colnames(ks_swd_lat_long_s2_man_simple)[1] <- "KID_S2_MAN"
# 
# # make s1 dataframe simpler, pulling bare minimum of columns
# ks_swd_lat_long_s1_simple <- 
#   ks_swd_lat_long_dup_s1[,c("KID", "LATITUDE", "LONGITUDE", "origin")]
# colnames(ks_swd_lat_long_s1_simple)[1] <- "KID_S1"
# 
# # merge s1 and s2/man kids into one dataframe by lat/long
# lat_long_duplicates_of_weirdos_only <- 
#   merge(ks_swd_lat_long_s2_man_simple,
#         ks_swd_lat_long_s1_simple,
#         by = c("LATITUDE", "LONGITUDE"))
# 
# # kids of rows to pull for duplicate evaluation
# kids_of_rows_to_pull_for_duplicate_evaluation <- 
#   c(lat_long_duplicates_of_weirdos_only$KID_S2_MAN, 
#     lat_long_duplicates_of_weirdos_only$KID_S1)
# # View(kids_of_rows_to_pull_for_duplicate_evaluation)
# 
# # make dataframe of duplicates of s1 wells by s2/manual wells by lat/long
# weirdo_duplicate_rows <- 
#   ks_swd_master_dataframe_for_review[which(
#     ks_swd_master_dataframe_for_review$KID %in% 
#       kids_of_rows_to_pull_for_duplicate_evaluation
#   ),]
# 
# # View(weirdo_duplicate_rows)
# # kid 1030570876 is swd but no api, 
# # where dup has api and more dates, API 15-009-07176
# # 
# # manual check of sole weird well
# # View(ks_working_with_block_groups[which(
# #   ks_working_with_block_groups$API_NUMBER_SIMPLE == "15-009-07176"
# # ),])
# 
# # kids of wells to keep (drop 6 of them)
# kids_keep_lat_long <- 
#   weirdo_duplicate_rows$KID[which(weirdo_duplicate_rows$origin == 's1')]
# 
# # kids of wells to drop (drop 6 of them)
# kids_drop_lat_long <- 
#   weirdo_duplicate_rows$KID[which(weirdo_duplicate_rows$origin != 's1')]
# 
# # save the master dataframe
# save(ks_swd_master_dataframe_for_review, 
#      file = "ks_swd_master_dataframe_for_review.rdata")
# 
# # remove 6 duplicates!
# ks_swd_master_no_weirdo_dups <- 
#   ks_swd_master_dataframe_for_review[which(
#     ks_swd_master_dataframe_for_review$KID %notin% 
#       kids_drop_lat_long
#   ),]
# 
# # View(ks_swd_master_no_weirdo_dups)
# str(ks_swd_master_no_weirdo_dups$API_NUMBER_SIMPLE)
# 
# # fix that one api missing from its well
# ks_swd_master_no_weirdo_dups <-   # fix big one
#   within(ks_swd_master_no_weirdo_dups, 
#          API_NUMBER[KID == "1030570876"] <- "15-009-07176")
# ks_swd_master_no_weirdo_dups <-   # fix simple one
#   within(ks_swd_master_no_weirdo_dups, 
#          API_NUMBER_SIMPLE[KID == "1030570876"] <- "15-009-07176")
# 
# # ks_s2_man_remaining <- 
# #   filter(ks_swd_master_no_weirdo_dups, 
# #          origin == 's2' | origin == 'man')
# 
# 
# #### remove wells without APIs ####
# ks_swd_master_with_api <- 
#   filter(ks_swd_master_no_weirdo_dups, 
#          !is.na(API_NUMBER))
# 
# nrow(ks_swd_master_no_weirdo_dups)
# 
# # View(ks_swd_master_with_api)  # count 14,865
# 
# 
# 
# #### check full API duplicates, match to list ####
# # view rows with duplicated APIs
# ks_swd_master_api_dup_index <-   # make the index to get the rows
#   duplicated(ks_swd_master_with_api$API_NUMBER) | 
#   duplicated(ks_swd_master_with_api$API_NUMBER, 
#              fromLast = TRUE)
# 
# ks_swd_api_full_dups <-   # pull the duplicated rows
#   ks_swd_master_with_api[ks_swd_master_api_dup_index,]
# save(ks_swd_api_full_dups, 
#      file = "ks_swd_api_full_dups.rdata")
# 
# # View(ks_swd_api_full_dups) # count of 274, all s1s
# 
# 
# 
# #### check simple API duplicates, now not deleting SWD wells ####
# 
# # keep last modified
# ks_swd_api_full_dups <-   # order by modification date
#   ks_swd_api_full_dups[order(ks_swd_api_full_dups$modified_as_date),]
# # View(ks_swd_api_full_dups)
# 
# # marks all but latest api for each well as TRUE
# ks_swd_full_api_drop_index <-    
#   duplicated(ks_swd_api_full_dups$API_NUMBER, 
#              fromLast = TRUE)
# 
# # makes dataframe of rows of wells to drop
# ks_swd_full_api_drop_wells <-    
#   ks_swd_api_full_dups[ks_swd_full_api_drop_index, ]
# 
# # makes list of just the kids
# kids_full_api_drops <-   
#   ks_swd_full_api_drop_wells$KID
# 
# # make a drop dup column
# ks_swd_master_with_api$drop_dup <- NA
# 
# # flag all full api dups
# ks_swd_master_with_api <-    
#   within(ks_swd_master_with_api, 
#          drop_dup[KID %in% ks_swd_api_full_dups$KID] <- 
#            'keep_dup_full_api')
# 
# # flag full api drops
# ks_swd_master_with_api <-    
#   within(ks_swd_master_with_api, 
#          drop_dup[KID %in% kids_full_api_drops] <- 
#            'drop_dup_full_api')
# # View(ks_swd_master_with_api[which(!is.na(ks_swd_master_with_api$drop_dup)),])
# 
# # make dataframe without full api duplicates
# ks_swd_master_for_small_api <-   
#   filter(ks_swd_master_with_api, 
#          is.na(drop_dup) |
#            drop_dup != "drop_dup_full_api")
# # nrow(ks_swd_master_with_api)
# # nrow(ks_swd_master_for_small_api)
# 
# 
# 
# #### DROP PARTIAL API DUPLICATES HERE #### 
# # view rows with duplicated APIs
# ks_swd_master_small_dup_index <-   # make the index to get the rows
#   duplicated(ks_swd_master_for_small_api$API_NUMBER_SIMPLE) | 
#   duplicated(ks_swd_master_for_small_api$API_NUMBER_SIMPLE, 
#              fromLast = TRUE)
# 
# # make dataframe of duplicated rows
# ks_swd_api_small_dups <-   
#   ks_swd_master_for_small_api[ks_swd_master_small_dup_index,]
# save(ks_swd_api_small_dups, 
#      file = "ks_swd_api_small_dups.rdata")
# # View(ks_swd_api_small_dups) # count of 2,218, 2 s2, 2,216 s1s
# 
# # order dataframe by full api number (latest last)
# ks_swd_api_small_dups <-   
#   ks_swd_api_small_dups[order(ks_swd_api_small_dups$API_NUMBER),]
# # View(ks_swd_master_for_small_api)
# 
# # marks all but last api for each well as TRUE
# ks_swd_api_small_drop_index <-    
#   duplicated(ks_swd_api_small_dups$API_NUMBER_SIMPLE, 
#              fromLast = TRUE)
# 
# # makes dataframe of rows of wells to drop
# ks_swd_api_small_drops <-    
#   ks_swd_api_small_dups[ks_swd_api_small_drop_index, ]
# 
# # makes list of just the kids
# kids_api_small_drops <-   
#   ks_swd_api_small_drops$KID
# 
# # flag all full api dups
# # (note: in full dataset!  need to drop full and small again)
# ks_swd_master_with_api <-    
#   within(ks_swd_master_with_api, 
#          drop_dup[KID %in% ks_swd_api_small_dups$KID] <- 
#            'keep_dup_small_api')
# 
# # flag full api drops
# ks_swd_master_with_api <-    
#   within(ks_swd_master_with_api, 
#          drop_dup[KID %in% kids_api_small_drops] <- 
#            'drop_dup_small_api')
# length(kids_api_small_drops) # 1152
# # View(ks_swd_master_with_api[which(!is.na(ks_swd_master_with_api$drop_dup)),])
# 
# # drop full and small duplicates
# ks_swd_master_dropped_all_apis <- 
#   filter(ks_swd_master_with_api, 
#          is.na(drop_dup) |
#            drop_dup == "keep_dup_full_api" | 
#            drop_dup == "keep_dup_small_api")
# 
# # table(ks_swd_master_with_api$drop_dup, ks_swd_master_with_api$origin)
# nrow(ks_swd_master_dropped_all_apis)
# 
# 
# #### deal with lat/longs!  nearly done! ####
# # find duplicates by latitude and longitude 
# ks_swd_last_lat_long_dup_index <- 
#   duplicated(ks_swd_master_dropped_all_apis[c("LATITUDE","LONGITUDE")]) | 
#   duplicated(ks_swd_master_dropped_all_apis[c("LATITUDE","LONGITUDE")], 
#              fromLast = TRUE)
# 
# # make dataframe of the duplicates
# ks_swd_last_lat_long_dups <-   
#   ks_swd_master_dropped_all_apis[ks_swd_last_lat_long_dup_index, ]
# 
# # View(ks_swd_last_lat_long_dups)
# 
# # order by modification date
# ks_swd_last_lat_long_dups <-   
#   ks_swd_last_lat_long_dups[order(ks_swd_last_lat_long_dups$modified_as_date),]
# # View(ks_swd_last_lat_long_dups)
# # table(ks_swd_last_lat_long_dups$origin)
# 
# # marks all but latest lat/long for each well as TRUE
# ks_swd_last_lat_long_drop_index <-    
#   duplicated(ks_swd_last_lat_long_dups[c("LATITUDE","LONGITUDE")], 
#              fromLast = TRUE)
# 
# # makes dataframe of rows of wells to drop
# ks_swd_last_lat_long_drops <-    
#   ks_swd_last_lat_long_dups[ks_swd_last_lat_long_drop_index, ]
# nrow(ks_swd_last_lat_long_drops)
# 
# # makes list of just the kids
# kids_last_lat_long_drops <-   
#   ks_swd_last_lat_long_drops$KID
# 
# # flag all lat-long dups
# ks_swd_master_with_api <-    
#   within(ks_swd_master_with_api, 
#          drop_dup[KID %in% ks_swd_last_lat_long_dups$KID] <- 
#            'keep_dup_lat_long')
# 
# # flag all lat-long drops
# ks_swd_master_with_api <-    
#   within(ks_swd_master_with_api, 
#          drop_dup[KID %in% kids_last_lat_long_drops] <- 
#            'drop_dup_lat_long')
# 
# # View(ks_swd_master_with_api[which(
# # !is.na(ks_swd_master_with_api$drop_dup))
# # ,])
# 
# # View(ks_swd_master_with_api[which(!is.na(ks_swd_master_with_api$drop_dup)),])
# 
# # make final dataset of wells
# ks_swd_without_any_dups <-   
#   filter(ks_swd_master_with_api, 
#          is.na(drop_dup) |
#            drop_dup %in% c("keep_dup_full_api", 
#                            "keep_dup_small_api", 
#                            "keep_dup_lat_long"))
# # View(ks_swd_without_any_dups)
# # View(ks_swd_without_any_dups)
# 
# # save and load chunk
# save(ks_swd_without_any_dups,
#      file = "ks_swd_without_any_dups.rdata")
# write.csv(ks_swd_without_any_dups,
#           file = "ks_swd_without_any_dups.csv")
# load(file = "ks_swd_without_any_dups.rdata")
# 
# 
# ks_swd_working <-   # make working file
#   ks_swd_without_any_dups
# # View(ks_swd_working)
# # table(ks_swd_working$STATUS2)
# # table(ks_swd_working$STATUS)
# # nrow(ks_swd_working)
# 
# 
# #### CATEGORY ASSIGNMENTS ####
# 
# #### assign activity
# 
# # assign inactive statuses
# inactive_status1s <-   # make vector of pa status1s
#   sort(c("EOR-P&A",
#          "GAS-P&A",
#          "INJ-P&A",
#          "O&G-P&A",
#          "OIL-P&A",
#          "OTHER-P&A(CLASS ONE (OLD))",
#          "OTHER-P&A(LH)",
#          "OTHER-P&A(OIL-SWD)",
#          "OTHER-P&A(OIL&GAS-INJ)",
#          "OTHER-P&A(STRAT)",
#          "OTHER-P&A(TA)",
#          "OTHER-P&A(WATER)",
#          "OTHER(SWD-P&A)",
#          "SWD-P&A",
#          "D&A",
#          "OTHER(LH)",
#          "OTHER(TA)"))
# 
# inactive_status2s <-   # make vector of pa status2s
#   sort(c("Approved for Plugging - CP-1 Received",
#          "Expired Plugging Application (CP-1)",
#          "Inactive Well",
#          "Injection Authorization Terminated",
#          "Injection Authorization Terminated - INACTIVE CODE",
#          "KCC Fee Fund Plugging",
#          "Plugged and Abandoned",
#          "Re-Plugged (non Fee-Fund)",
#          "Unplugged Former Injection Well"))
# 
# ks_swd_working <-   # assign inactive wells
#   within(ks_swd_working, 
#          activity[STATUS %in% inactive_status1s |
#                     STATUS2 %in% inactive_status2s] <- 'inactive')
# 
# # assign future statuses
# future_status2s <-   # make vector of future status2s
#   c("Approved Intent to Drill",
#     "DEVELOPMENT",
#     "ON LIST",
#     "Pending Injection Application")
# 
# ks_swd_working <-   # assign future wells
#   within(ks_swd_working, 
#          activity[STATUS2 %in% future_status2s] <- 'future')
# 
# # assign cancel statuses
# cancel_status2s <-   # make vector of cancel status2s
#   c("Cancelled API Number",
#     "Expired Intent to Drill (C-1)",
#     "UIC Application Denied",
#     "UIC Application Dismissed",
#     "UIC Application Withdrawn")
# 
# ks_swd_working <-   # assign cancel wells
#   within(ks_swd_working, 
#          activity[STATUS2 %in% cancel_status2s] <- 'ab_loc')
# 
# # assign drill statuses
# drill_status2s <-   # make vector of drill status2s
#   sort(c("Spudded",
#          "Well Drilled"))
# 
# ks_swd_working <-   # assign drill wells
#   within(ks_swd_working, 
#          activity[STATUS2 %in% drill_status2s] <- 'drill')
# 
# # table(ks_swd_working$activity)
# 
# 
# 
# #### assign plug
# 
# # assign plug_status
# plug_status1s <-   # make vector of plug status1s
#   sort(c("EOR-P&A",
#          "GAS-P&A",
#          "INJ-P&A",
#          "O&G-P&A",
#          "OIL-P&A",
#          "OTHER-P&A(CLASS ONE (OLD))",
#          "OTHER-P&A(LH)",
#          "OTHER-P&A(OIL-SWD)",
#          "OTHER-P&A(OIL&GAS-INJ)",
#          "OTHER-P&A(STRAT)",
#          "OTHER-P&A(TA)",
#          "OTHER-P&A(WATER)",
#          "OTHER(SWD-P&A)",
#          "SWD-P&A"))
# 
# plug_status2s <-   # make vector of plug status2s
#   sort(c("Plugged and Abandoned",
#          "Re-Plugged (non Fee-Fund)"))
# 
# ks_swd_working <-   # assign plugged by status
#   within(ks_swd_working,
#          plug_status[STATUS %in% plug_status1s | 
#                        STATUS2 %in% plug_status2s] <-
#            'has_plug_status')
# 
# ks_swd_working <-   # assign rest to not status
#   within(ks_swd_working,
#          plug_status[is.na(plug_status)] <-
#            'no_plug_status')
# 
# 
# 
# #### assign UIC status ####
# 
# uic_kids <-   # pull KIDs of uic wells in UIC database
#   unique(ks_uic_2018_09_04$KGS_ID)
# 
# # assign wells as in or not in the uic
# ks_swd_working$uic <-   
#   ifelse(ks_swd_working$KID %in% uic_kids,
#          'in_uic',
#          'not_in_uic')
# # View(ks_swd_working)
# 
# 
# #### deal with comments ####
# 
# # isolate those wells from the comments that will actually help
# ks_manual_wells_useful_columns <- 
#   raw_manual_well_assignments_dataframe[
#     ,c("KID", 
#        "man_activity", 
#        "man_plug_status", 
#        "man_well_type",	
#        "man_assignment_source",	
#        "man_detailed_well_type",	
#        "man_assignment_notes", 
#        "man_comments_examined",	
#        "man_kgs_available_documents_verified", 
#        "man_reason_for_flowchart", 
#        "man_well_type_swd"
#        )
#   ]
# 
# # View(ks_swd_working)
# ks_swd_working <- # merge manual assignments with large dataset
#   left_join(ks_swd_working, 
#             ks_manual_wells_useful_columns, 
#             by = "KID")
# # View(ks_swd_working)
# 
# 
# 
# # assigning overall plug status
# 
# # assign plug_date
# ks_swd_working <-   # assign plugged by date
#   within(ks_swd_working, 
#          plug_date_binary[!is.na(plugging_as_date)] <-
#            'has_plug_date')
# 
# ks_swd_working <-   # assign rest to no plug date
#   within(ks_swd_working,
#          plug_date_binary[is.na(plug_date_binary)] <-
#            'no_plug_date')
# 
# # assign overall plugged
# ks_swd_working <-   # assign overall plug
#   within(ks_swd_working,
#          plug_overall[plug_status == 
#                         "has_plug_status" | 
#                         plug_date_binary == "has_plug_date"] <-
#            'plugged')
# 
# ks_swd_working <-   # assign overall plug
#   within(ks_swd_working,
#          plug_overall[is.na(plug_overall)] <-
#            'not_plugged')
# 
# 
# 
# # assign manual activity statuses
# ks_swd_working$man_activity <-   # force man_activity to character
#   as.character(ks_swd_working$man_activity)
# 
# # apply manual activity assignments to wells that have them
# ks_swd_working$activity <-   
#   with(ks_swd_working, 
#        ifelse(!is.na(man_activity), man_activity, activity))
# 
# # fix rest of activities
# ks_swd_working$activity <-   # make unplugged NAs active
#   with(ks_swd_working, 
#        ifelse(is.na(activity) & 
#                 plug_overall == 'not_plugged', 'active', activity))
# 
# ks_swd_working$activity <-   # make plugged NAs inactive
#   with(ks_swd_working, 
#        ifelse(is.na(activity) & 
#                 plug_overall == 'plugged', 'inactive', activity))
# 
# ks_swd_working$activity <-   # make unplugged unknowns active
#   with(ks_swd_working, 
#        ifelse(activity == 'unknown' & 
#                 plug_overall == 'not_plugged', 'active', activity))
# 
# ks_swd_working$activity <-   # make plugged unknowns inactive
#   with(ks_swd_working, 
#        ifelse(activity == 'unknown' & 
#                 plug_overall == 'plugged', 'inactive', activity))
# 
# # View(ks_swd_working)
# table(ks_swd_working$activity)
# 
# 
# 
# #### assigning years ####
# 
# # 2010 year make variable
# ks_swd_working$permit_from_2010 <- NA
# ks_swd_working$spud_from_2010 <- NA
# ks_swd_working$completed_from_2010 <- NA
# ks_swd_working$active_from_2010 <- NA
# ks_swd_working$any_from_2010 <- NA
# # View(ks_swd_working)
# 
# # 2010 permit
# ks_swd_working <- 
#   within(ks_swd_working, 
#          permit_from_2010[permit_as_date >= "2010-01-01"] <- 'yes')
# ks_swd_working$permit_from_2010[
#   is.na(ks_swd_working$permit_from_2010)] <- "no"
# 
# # 2010 spud
# ks_swd_working <- 
#   within(ks_swd_working, 
#          spud_from_2010[spud_as_date >= "2010-01-01"] <- 'yes')
# ks_swd_working$spud_from_2010[
#   is.na(ks_swd_working$spud_from_2010)] <- "no"
# 
# # 2010 completed
# ks_swd_working <- 
#   within(ks_swd_working, 
#          completed_from_2010[completion_as_date >= "2010-01-01"] <- 'yes')
# ks_swd_working$completed_from_2010[
#   is.na(ks_swd_working$completed_from_2010)] <- "no"
# 
# # ONE VARIABLE TO RULE THEM ALL! (the year variables)
# ks_swd_working <-   # assign yeses
#   within(ks_swd_working, 
#          any_from_2010[permit_from_2010 == 'yes' |
#                         spud_from_2010 == 'yes' |
#                         completed_from_2010 == 'yes'] <- 'yes')
# ks_swd_working <-   # assign nos
#   within(ks_swd_working, 
#          any_from_2010[is.na(any_from_2010)] <- 'no')
# 
# # make variable for true SWD well
# ks_swd_working$all_swd <- 1
# nrow(ks_swd_working)
# 
# # make variable for wells excluding midways and abandoned locations
# ks_swd_working$extant_swd <- 1
# 
# # exclude abandoned locations and midways from total SWD list
# ks_swd_working <- 
#   within(ks_swd_working, 
#          extant_swd[activity %in% 
#                       c("midway", 
#                         "ab_loc")] <- 0)
# 
# # make numeric variable for wells started or completed in 2010 or later
# ks_swd_working$all_from_2010 <- 0
# 
# # assign the post-2010 variable
# ks_swd_working <- 
#   within(ks_swd_working, 
#          all_from_2010[any_from_2010 == 'yes'] <- 1)
# 
# # make variable for extant from 2010
# ks_swd_working$extant_from_2010 <- 0
# 
# # assign it
# ks_swd_working <- 
#   within(ks_swd_working, 
#          extant_from_2010[any_from_2010 == 'yes' &
#                             extant_swd == 1] <- 1)
# 
# # View(ks_swd_working)
# 
# sum(ks_swd_working$all_swd)
# sum(ks_swd_working$extant_swd)
# sum(ks_swd_working$all_from_2010)
# sum(ks_swd_working$extant_from_2010)
# 
# 
# 
# #### BEGIN BLOCK GROUP COUNTS ####
# 
# # pull KIDs and GEOIDs from master list of wells
# kids_and_geoids <- 
#   ks_working_with_block_groups[,c("KID", "GEOID")]
# # View(kids_and_geoids)
# 
# 
# 
# #### counting fluid by block group ####
# ks_working_with_block_groups$GEOID <- 
#   as.character(ks_working_with_block_groups$GEOID)
# # View(kids_and_geoids)
# 
# # join them with the fluid data
# swd_fluid_totals_well_block_group <- 
#   full_join(kids_and_geoids, swd_fluid_totals_by_well, by = "KID")
# # View(swd_fluid_totals_well_block_group)
# 
# # aggregate by block group
# fluid_totals_block_group <- 
#   aggregate(cbind(total_lifetime_fluid_injected, 
#                   from_2010_fluid_injected) ~ GEOID, 
#             data = swd_fluid_totals_well_block_group, 
#             sum)
# fluid_totals_block_group$GEOID <-   # convert GEOID to character
#   as.character(fluid_totals_block_group$GEOID)
# 
# # change column names for analysis
# colnames(fluid_totals_block_group) <- c("GEOID", 
#                                         "total_fluid", 
#                                         "from_2010_fluid")
# View(fluid_totals_block_group)
# 
# 
# #### counting wells by block group ####
# 
# # extant wells
# ks_well_counts <- 
#   aggregate(
#     cbind(
#       all_swd, 
#       extant_swd, 
#       all_from_2010, 
#       extant_from_2010) ~ GEOID,
#   data = ks_swd_working,
#   sum)
# # View(ks_well_counts)
# 
# # rename well count variables
# colnames(ks_well_counts) <- c("GEOID", 
#                               "all_swd_count", 
#                               "extant_swd_count", 
#                               "all_swd_from_2010_count", 
#                               "extant_swd_from_2010_count")
# # View(ks_well_counts)
# 
# 
# 
# #### horizontal well data counts by block group ####
# # merge with GEOIDs
# horizontal_block_groups_no_swd <- 
#   full_join(kids_and_geoids, 
#             horizontal_wells_from_2010_exclusions, 
#             by = "KID")
# 
# # add unit column
# horizontal_block_groups_no_swd$horizontal_count <- 1
# 
# # make rows without wells 0
# horizontal_block_groups_no_swd <- 
#   within(horizontal_block_groups_no_swd, 
#          horizontal_count[is.na(API_NUMBER)] <- 0)
# # View(horizontal_block_groups_no_swd)
# 
# # make the counts
# horizontal_counts <- 
#   aggregate(horizontal_count ~ GEOID,
#     data = horizontal_block_groups_no_swd,
#     sum)
# # View(horizontal_counts)
# 
# # remove blank row
# horizontal_counts <- 
#   horizontal_counts[which(
#     horizontal_counts$GEOID != ""
#   ),]
# 
# 
# 
# #### shale data sums ####
# # already summed, just don't forget it
# 
# 
# 
# #### merge well counts, fluid counts, horizontal counts, and shale presence
# lime_and_fluid <-   # merge lime and fluid counts
#   full_join(working_lime_data, fluid_totals_block_group, by = "GEOID")
# # View(lime_and_fluid)
# 
# lime_fluid_swd <- 
#   full_join(lime_and_fluid, ks_well_counts, by = "GEOID")
# # View(lime_fluid_swd)
# 
# lime_fluid_swd_horiz <- 
#   full_join(lime_fluid_swd, horizontal_counts, by = "GEOID")
# # View(lime_fluid_swd_horiz)
# 
# 
# # load ACS variables
# # load(file = "acs_geo_cats.rdata")
# # View(acs_geo_cats)
# 
# # convert acs_geo_cats GEOID to the one in ks_well_counts
# # preserve the original GEOID
# acs_geo_cats$GEOID_alphanum <- 
#   acs_geo_cats$GEOID 
# acs_geo_cats$GEOID <-  # fix the new one 
#   gsub("15000US", "", acs_geo_cats$GEOID)
# 
# 
# 
# #### make the full dataset ####
# # merge everything with acs!  hey-o!
# lime_fluid_swd_horiz_acs <- merge(lime_fluid_swd_horiz,
#                                   acs_geo_cats,
#                                   by = "GEOID",
#                                   all = TRUE)
# # View(lime_fluid_swd_horiz_acs)
# 
# 
# 
# #### replace NAs with 0s ####
# # shale presence
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          shale_presence[is.na(
#            lime_fluid_swd_horiz_acs$shale_presence)] <- 0)
# 
# # total lifetime fluid injected
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          total_fluid[is.na(
#            lime_fluid_swd_horiz_acs$total_fluid)] <- 0)
# 
# # from 2010 fluid injected
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          from_2010_fluid[is.na(
#            lime_fluid_swd_horiz_acs$from_2010_fluid)] <- 0)
# 
# # all swd well count
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          all_swd_count[is.na(
#            lime_fluid_swd_horiz_acs$all_swd_count)] <- 0)
# 
# # extant swd well count
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          extant_swd_count[is.na(
#            lime_fluid_swd_horiz_acs$extant_swd_count)] <- 0)
# 
# # all swd wells from 2010 count
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          all_swd_from_2010_count[is.na(
#            lime_fluid_swd_horiz_acs$all_swd_from_2010_count)] <- 0)
# 
# # extant swd wells from 2010 count
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          extant_swd_from_2010_count[is.na(
#            lime_fluid_swd_horiz_acs$extant_swd_from_2010_count)] <- 0)
# 
# # horizontal well count
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          horizontal_count[is.na(
#            lime_fluid_swd_horiz_acs$horizontal_count)] <- 0)
# # View(lime_fluid_swd_horiz_acs)
# 
# 
# 
# #### construct the binaries! ####
# 
# # make the variables
# lime_fluid_swd_horiz_acs$total_fluid_binary <- NA
# lime_fluid_swd_horiz_acs$from_2010_fluid_binary <- NA
# lime_fluid_swd_horiz_acs$horizontal_binary <- NA
# lime_fluid_swd_horiz_acs$all_swd_binary <- NA
# lime_fluid_swd_horiz_acs$extant_swd_binary <- NA
# lime_fluid_swd_horiz_acs$all_swd_from_2010_binary <- NA
# lime_fluid_swd_horiz_acs$extant_swd_from_2010_binary <- NA
# 
# # total fluid binary
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          total_fluid_binary[total_fluid == 0] <- 0)
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          total_fluid_binary[total_fluid > 0] <- 1)
# 
# # from 2010 fluid binary
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          from_2010_fluid_binary[from_2010_fluid == 0] <- 0)
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          from_2010_fluid_binary[from_2010_fluid > 0] <- 1)
# 
# # horizontal binary
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          horizontal_binary[horizontal_count == 0] <- 0)
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          horizontal_binary[horizontal_count > 0] <- 1)
# 
# # all swd binary
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          all_swd_binary[all_swd_count == 0] <- 0)
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          all_swd_binary[all_swd_count > 0] <- 1)
# 
# # all swd from 2010 binary
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          all_swd_from_2010_binary[all_swd_from_2010_count == 0] <- 0)
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          all_swd_from_2010_binary[all_swd_from_2010_count > 0] <- 1)
# 
# # extant swd binary
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          extant_swd_binary[extant_swd_count == 0] <- 0)
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          extant_swd_binary[extant_swd_count > 0] <- 1)
# 
# # extant swd from 2010 binary
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          extant_swd_from_2010_binary[extant_swd_from_2010_count == 0] <- 0)
# lime_fluid_swd_horiz_acs <- 
#   within(lime_fluid_swd_horiz_acs, 
#          extant_swd_from_2010_binary[extant_swd_from_2010_count > 0] <- 1)
# 
# # View(head(lime_fluid_swd_horiz_acs))
# 
# 
# 
# # save/write/load chunk
# save(lime_fluid_swd_horiz_acs, 
#      file = "lime_fluid_swd_horiz_acs.rdata")
# write.csv(lime_fluid_swd_horiz_acs, 
#           file = "lime_fluid_swd_horiz_acs.csv")
# load(file = "lime_fluid_swd_horiz_acs.rdata")
# 
# 
# 
# # simplifying to only the variables that I really, really need
# lime_fluid_swd_horiz_acs_analysis <- 
#   lime_fluid_swd_horiz_acs[,c(
#     "GEOID",
#     "all_swd_count", 
#     "extant_swd_count", 
#     "all_swd_from_2010_count", 
#     "extant_swd_from_2010_count", 
#     "horizontal_count", 
#     "total_fluid",
#     "from_2010_fluid", 
#     "all_swd_binary", 
#     "extant_swd_binary", 
#     "all_swd_from_2010_binary", 
#     "extant_swd_from_2010_binary", 
#     "horizontal_binary",
#     "total_fluid_binary",
#     "from_2010_fluid_binary", 
#     "shale_presence", 
#     "ALAND_KM", 
#     "AWATER", 
#     "pop_tot_B01001_001", 
#     "pop_dense_B01001_001_ALAND", 
#     "age_0_to_17_percent_B01001", 
#     "age_65_plus_percent_B01001", 
#     "white_non_hisp_lat_percent_B03002_003_001", 
#     "hisp_lat_percent_B03002_012_001", 
#     "aian_percent_B03002_005_015_001",
#     "education_high_school_plus_percent_B15003_017_to_025", 
#     "renter_occupy_percent_B25003_003_001", 
#     "limited_english_percent_C16002_004_007_010_013_001", 
#     "earnings_median_B20002_001", 
#     "income_house_median_B19013_001", 
#     "house_value_median_B25077_001", 
#     "no_health_insurance_percent_B27010_017_033_050_066", 
#     "poverty_below_100_percent_C17002_002_003_001", 
#     "unemployed_percent_B23025_005_003"
#     )]
# # View(lime_fluid_swd_horiz_acs_analysis)
# 
# # save/write/load chunk
# save(lime_fluid_swd_horiz_acs_analysis, 
#      file = "lime_fluid_swd_horiz_acs_analysis.rdata")
# write.csv(lime_fluid_swd_horiz_acs_analysis, 
#           file = "lime_fluid_swd_horiz_acs_analysis.csv")
load(file = "lime_fluid_swd_horiz_acs_analysis.rdata")



#### BEGIN ANALYSES!!! ####

# make working dataset
ks_analysis_dataset <-   
  lime_fluid_swd_horiz_acs_analysis
# View(ks_analysis_dataset)

# save write load block
save(ks_analysis_dataset, 
     file = "ks_analysis_dataset.rdata")
write.csv(ks_analysis_dataset, 
          file = "ks_analysis_dataset.csv")
load(file = "ks_analysis_dataset.rdata")

# remove block groups with no poulation (n = 12, none with any wells or data)
ks_analysis_populated <- 
  ks_analysis_dataset[which(
    ks_analysis_dataset$pop_tot_B01001_001 != 0),]
# View(ks_analysis_populated)   # 2339 rows

# save write load block
save(ks_analysis_populated, 
     file = "ks_analysis_populated.rdata")
write.csv(ks_analysis_populated, 
          file = "ks_analysis_populated.csv")
load(file = "ks_analysis_populated.rdata")

# count NAs in ks_analysis_populated to total the missing values
missing_value_totals <- 
  colSums(is.na(ks_analysis_populated))
# View(missing_value_totals)

# make dataframe of rows with NAs in them
na_rows_ks_analysis_populated <- 
  ks_analysis_populated[
    !complete.cases(ks_analysis_populated), ]
# View(na_rows_ks_analysis_populated)

# save and write chunk
save(missing_value_totals, 
     file = "missing_value_totals.rdata")
write.csv(missing_value_totals, 
          file = "missing_value_totals.csv")



#### CORRELATION TABLE ####

# make dataframe for the correlation dataset
correlation_matrix_data  <-  
  ks_analysis_populated[,c(
    "GEOID",
    "ALAND_KM", 
    "pop_tot_B01001_001",
    "pop_dense_B01001_001_ALAND", 
    "age_0_to_17_percent_B01001", 
    "age_65_plus_percent_B01001", 
    "white_non_hisp_lat_percent_B03002_003_001", 
    "hisp_lat_percent_B03002_012_001", 
    "aian_percent_B03002_005_015_001",
    "education_high_school_plus_percent_B15003_017_to_025", 
    "renter_occupy_percent_B25003_003_001", 
    "limited_english_percent_C16002_004_007_010_013_001", 
    "earnings_median_B20002_001", 
    "income_house_median_B19013_001", 
    "house_value_median_B25077_001", 
    "no_health_insurance_percent_B27010_017_033_050_066", 
    "poverty_below_100_percent_C17002_002_003_001", 
    "unemployed_percent_B23025_005_003", 
    "shale_presence", 
    "horizontal_count")]

# fix names to make them prettier on the heatmap
colnames(correlation_matrix_data) <- c("GEOID", 
                                       "Land area", 
                                       "Total population", 
                                       "Population density", 
                                       "% age <18", 
                                       "% age >65", 
                                       "% non-Hispanic white", 
                                       "% Hispanic or Latinx",
                                       "% Native American", 
                                       "% high school education", 
                                       "% renter-occupied households", 
                                       "% limited-English households", 
                                       "Median earnings", 
                                       "Median household income", 
                                       "Median household value", 
                                       "% with no health insurance", 
                                       "% in poverty", 
                                       "% unemployed", 
                                       "Mississippi Lime Play", 
                                       "Horizontal well count")

# reorder the columns
correlation_matrix_data <- 
  correlation_matrix_data[c("GEOID", 
                            "Horizontal well count",
                            "Mississippi Lime Play",
                            "Median household value", 
                            "Median earnings", 
                            "Median household income", 
                            "% with no health insurance", 
                            "% limited-English households", 
                            "% renter-occupied households", 
                            "% unemployed", 
                            "% in poverty", 
                            "% high school education", 
                            "% age >65", 
                            "% age <18", 
                            "% non-Hispanic white", 
                            "% Hispanic or Latinx", 
                            "% Native American", 
                            "Population density", 
                            "Total population",
                            "Land area")]

View(correlation_matrix_data)

# make dataframe of rows with NAs in them
na_rows_correlation_matrix_data <- 
  correlation_matrix_data[
    !complete.cases(correlation_matrix_data), ]

# make populated correlation matrix data
correlation_matrix_data_populated <- 
  correlation_matrix_data[which(
    correlation_matrix_data$pop_tot_B01001_001 != 0),]

# View(na_rows_correlation_matrix_data)

# make dataframe with only complete observations
correlation_matrix_data_complete <- 
  correlation_matrix_data[
    complete.cases(correlation_matrix_data), ]

# View(correlation_matrix_data_complete)

# create the correlation matrix with all observations
# rounds to 2 decimal places
correlation_matrix  <-  
  round(cor(correlation_matrix_data[,2:20], 
            method = "spearman", 
            use = "pairwise.complete.obs"), 
        2) 

# View(correlation_matrix)

# view and write to file
# View(correlation_matrix)
write.csv(correlation_matrix, file = "correlation_matrix.csv")

# count observations going into the correlation matrix
counts_pairwise_correlations  <-  
  count.pairwise(correlation_matrix_data_complete[,2:20], 
                 y = NULL, 
                 diagonal=TRUE)

# view and write to file
# View(counts_pairwise_correlations)
write.csv(counts_pairwise_correlations, 
          file = "counts_pairwise_correlations.csv")



#### make pretty heatmap ####
cormat <- 
  correlation_matrix

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()

# get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# heatmap
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Spearman\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust = 1, 
                                   family = "Times New Roman")) +
  coord_fixed()


# reorder_cormat <- function(cormat){
#   # Use correlation between variables as distance
#   dd <- as.dist((1-cormat)/2)
#   hc <- hclust(dd)
#   cormat <-cormat[hc$order, hc$order]
# }
# 
# # reorder the correlation matrix
# cormat <- reorder_cormat(cormat)
# upper_tri <- get_upper_tri(cormat)
# 
# # melt the correlation matrix
# melted_cormat <- melt(upper_tri, na.rm = TRUE)

# create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Spearman\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1, 
                                   family = "Times New Roman"), 
        axis.text.y = element_text(family = "Times New Roman", 
                                   size = 12))+
  coord_fixed()

print(ggheatmap)   # print the heatmap

ggheatmap +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4, 
            family = "Times New Roman") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal", 
    text=element_text(family = "Times New Roman"))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

ggheatmap +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal", 
    text = element_text(family = "Times New Roman"))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
print(ggheatmap)



#### make post-correlation dataset excluding land area, % Hisp/Lat, median earnings, % in poverty, and household value

ks_analysis_pop_indep <- 
  subset(ks_analysis_populated, 
         select = -c(
           ALAND_KM, 
           hisp_lat_percent_B03002_012_001, 
           earnings_median_B20002_001, 
           house_value_median_B25077_001, 
           poverty_below_100_percent_C17002_002_003_001))

# View(ks_analysis_pop_indep)

# check for NAs
# count NAs in ks_analysis_populated to total the missing values
missing_value_totals_indep <- 
  colSums(is.na(ks_analysis_pop_indep))
View(missing_value_totals_indep)

# make dataframe of rows with NAs in them (n = 46, all due to missing income)
na_rows_ks_analysis_pop_indep <- 
  ks_analysis_pop_indep[
    !complete.cases(ks_analysis_pop_indep), ]
View(na_rows_ks_analysis_pop_indep)
  









































# #### Analyses! ####
# 
# save(KS_FINAL_DATASET, 
#      file = "KS_FINAL_DATASET.rdata")
# write.csv(KS_FINAL_DATASET, 
#           file = "KS_FINAL_DATASET.csv")
# 
# # load files if starting from here
# # load(file = "KS_FINAL_DATASET.rdata")
# ks_analysis_dataset <- KS_FINAL_DATASET
# # View(ks_analysis_dataset)









# #### checking normality ####
# 
# par(mfrow=c(2,2))
# 
# # income
# density_hh_income <- 
#   density(ks_analysis_dataset$median_household_income_B19013, 
#           na.rm = TRUE)
# plot(density_hh_income, main = "Median Household Income Density Plot")
# 
# density_household_value <- 
#   density(ks_analysis_dataset$median_household_value_B25077, na.rm = TRUE)
# plot(density_household_value, 
#      main = "Median Household Value Density Plot")
# 
# density_percent_white <- 
#   density(ks_analysis_dataset$percent_white_B03002, na.rm = TRUE)
# plot(density_percent_white, 
#      main = "Percent White Density Plot")
# 
# density_pop_density <- 
#   density(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)
# plot(density_pop_density, 
#      main = "Population Density Density Plot")
# 
# density_percent_high_school <- 
#   density(ks_analysis_dataset$percent_high_school_plus_B15003, na.rm = TRUE)
# plot(density_percent_high_school, 
#      main = "Percent Completed High School Density Plot")
# 
# density_median_age <- 
#   density(ks_analysis_dataset$median_age_B01002, na.rm = TRUE)
# plot(density_median_age, main = "Median Age Density Plot")
# 
# density_swd <- 
#   density(ks_analysis_dataset$any_swd_sum, na.rm = TRUE)
# plot(density_swd, 
#      main = "Number of Disposal Wells per Block Group Density Plot")
# 
# 
# 
# #### analysis by presence and absence of swd ####
# # number of block groups with and without swd wells
# table(ks_analysis_dataset$any_swd_binary)
#
#
#
# #### HAVE NOT RUN PAST HERE WITH THE CURRENT DATA 2018-12-15 ####
# 
# ## median household income
# # median
# aggregate(median_household_income_B19013~any_swd_binary, data=ks_analysis_dataset, FUN=median)
# median(ks_analysis_dataset$median_household_income_B19013, na.rm = TRUE)
# 
# # interquartile range
# aggregate(median_household_income_B19013~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
# quantile(ks_analysis_dataset$median_household_income_B19013, na.rm = TRUE)
# 
# # missings
# table(is.na(ks_analysis_dataset$median_household_income_B19013))
# aggregate(median_household_income_B19013~any_swd_binary, data=ks_analysis_dataset, FUN=length)
# 
# # t test
# t.test(ks_analysis_dataset$median_household_income_B19013[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$median_household_income_B19013[ks_analysis_dataset$any_swd_binary=="no"])
# 
# 
# ## median household value
# # median
# aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=median)
# median(ks_analysis_dataset$median_household_value_B25077, na.rm = TRUE)
# 
# # interquartile range
# aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
# quantile(ks_analysis_dataset$median_household_value_B25077, na.rm = TRUE)
# 
# # missings
# table(is.na(ks_analysis_dataset$median_household_value_B25077))
# aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=length)
# 
# # t test
# t.test(ks_analysis_dataset$median_household_value_B25077[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$median_household_value_B25077[ks_analysis_dataset$any_swd_binary=="no"])
# 
# 
# ## percent white
# # median
# aggregate(percent_white_B03002~any_swd_binary, data=ks_analysis_dataset, FUN=median)
# median(ks_analysis_dataset$percent_white_B03002, na.rm = TRUE)
# 
# # interquartile range
# aggregate(percent_white_B03002~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
# quantile(ks_analysis_dataset$percent_white_B03002, na.rm = TRUE)
# 
# # missings
# table(is.na(ks_analysis_dataset$percent_white_B03002))
# aggregate(percent_white_B03002~any_swd_binary, data=ks_analysis_dataset, FUN=length)
# 
# # t test
# t.test(ks_analysis_dataset$percent_white_B03002[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$percent_white_B03002[ks_analysis_dataset$any_swd_binary=="no"])
# 
# 
# ## population density
# # median
# aggregate(population_density_B01001_ALAND~any_swd_binary, data=ks_analysis_dataset, FUN=median)
# median(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)
# 
# # interquartile range
# aggregate(population_density_B01001_ALAND~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
# quantile(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)
# 
# # missings
# table(is.na(ks_analysis_dataset$population_density_B01001_ALAND))
# 
# # t test
# t.test(ks_analysis_dataset$population_density_B01001_ALAND[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$population_density_B01001_ALAND[ks_analysis_dataset$any_swd_binary=="no"])
# 
# 
# ## percent high school or more
# # median
# aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=median)
# median(ks_analysis_dataset$percent_high_school_plus_B15003, na.rm = TRUE)
# 
# # interquartile range
# aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
# quantile(ks_analysis_dataset$percent_high_school_plus_B15003, na.rm = TRUE)
# 
# # missings
# table(is.na(ks_analysis_dataset$percent_high_school_plus_B15003))
# aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=length)
# 
# # t test
# t.test(ks_analysis_dataset$percent_high_school_plus_B15003[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$percent_high_school_plus_B15003[ks_analysis_dataset$any_swd_binary=="no"])
# 
# 
# ## median age
# # median
# aggregate(median_age_B01002~any_swd_binary, data=ks_analysis_dataset, FUN=median)
# median(ks_analysis_dataset$median_age_B01002, na.rm = TRUE)
# 
# # interquartile range
# aggregate(median_age_B01002~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
# quantile(ks_analysis_dataset$median_age_B01002, na.rm = TRUE)
# 
# # missings
# table(is.na(ks_analysis_dataset$median_age_B01002))
# aggregate(median_age_B01002~any_swd_binary, data=ks_analysis_dataset, FUN=length)
# 
# # t test
# t.test(ks_analysis_dataset$median_age_B01002[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$median_age_B01002[ks_analysis_dataset$any_swd_binary=="no"])
# 
# 
# 
# #### Poisson
# poisson_model <- glm(formula = any_swd_sum ~ median_household_value_B25077 + percent_white_B03002 + population_density_B01001_ALAND + percent_high_school_plus_B15003 + median_age_B01002, family = "poisson", data = ks_analysis_dataset)
# summary(poisson_model)
# 
# poisson_model <- glm(formula = any_swd_sum ~ median_household_income_B19013 + median_household_value_B25077 + percent_white_B03002 + population_density_B01001_ALAND + percent_high_school_plus_B15003 + median_age_B01002, family = "poisson", data = ks_analysis_dataset)
# summary(poisson_model)
# confint(poisson_model)
# 
# ks_analysis_dataset <- within(ks_analysis_dataset, any_swd_binary[ks_analysis_dataset$any_swd_binary == "yes"]  <-  1)
# ks_analysis_dataset <- within(ks_analysis_dataset, any_swd_binary[ks_analysis_dataset$any_swd_binary == "no"]  <-  0)
# View(ks_analysis_dataset$any_swd_binary)
# ks_analysis_dataset$any_swd_binary <- as.numeric(ks_analysis_dataset$any_swd_binary)
# View(ks_analysis_dataset$any_swd_binary)
# 
# #### logistic model r
# logistic_model <- glm(formula = any_swd_binary ~ median_household_income_B19013 + median_household_value_B25077 + percent_white_B03002 + population_density_B01001_ALAND + percent_high_school_plus_B15003 + median_age_B01002, family = "binomial", data = ks_analysis_dataset)
# summary(logistic_model)
# confint(logistic_model)























#### ABANDON HOPE ALL YE WHO ENTER HERE, WHERE OLD CODE GOES TO DIE ####



# # This section combines all the manually assigned wells as well as the wells
# # pulled due solely due to their STATUS or STATUS2.
# 
# # pull well KIDs identified manually or by status1 or status2
# kids_from_manual_assignments <-   # KIDs of manually added wells
#   ks_working_with_block_groups$KID[which(ks_working_with_block_groups$KID %in% 
#                                            raw_manual_well_assignments_dataframe$KID)]
# 
# kids_status1s <-   # KIDs of wells IDed via status1
#   ks_working_with_block_groups$KID[which(ks_working_with_block_groups$STATUS %in% 
#                                            ks_definitely_include_status1s)]
# 
# kids_status2s <-   # KIDs of wells IDed via status2
#   ks_working_with_block_groups$KID[which(ks_working_with_block_groups$STATUS2 %in% 
#                                            ks_potential_disposal_include_status2s)]



#### HERE BEGINS THE REVISED WELL ASSIGNMENT PROCEDURE ####

# #### assignments for categories now moot 
# # make variable for is/is not swd
# ks_swd_working$is_swd <- NA 
# 
# # make variable for is/is not class1
# ks_swd_working$is_class1 <- NA
# 
# # identify non-manually assigned swd wells
# ks_swd_working <-   
#   within(ks_swd_working, 
#          is_swd[well_type == 'swd'] <- 'swd')
# 
# # identify manually assigned swd wells for 'is_swd' variable
# ks_swd_working <-   
#   within(ks_swd_working, 
#          is_swd[man_well_type 
#                 %in% 
#                   c('swd', 
#                     'prob_swd', 
#                     'prob_swd_prob_class1', 
#                     'prob_swd_may_class1', 
#                     'prob_class1_to_prob_swd', 
#                     'def_class1_to_prob_swd', 
#                     'prob_swd_to_prob_class1', 
#                     'may_class1_to_prob_swd', 
#                     'may_swd', 
#                     'test_swd'
#                   )
#                 ] <- 
#            'swd'
#   )
# 
# # identify manually assigned swd wells for 'well_type' variable
# ks_swd_working <-   
#   within(ks_swd_working, 
#          well_type[man_well_type 
#                    %in% 
#                      c('swd', 
#                        'prob_swd', 
#                        'prob_swd_prob_class1', 
#                        'prob_swd_may_class1', 
#                        'prob_class1_to_prob_swd', 
#                        'def_class1_to_prob_swd', 
#                        'prob_swd_to_prob_class1', 
#                        'may_class1_to_prob_swd', 
#                        'may_swd', 
#                        'test_swd'
#                      )
#                    ] <- 
#            'swd'
#   )
# 
# # identify non-manually assigned class1 wells
# ks_swd_working <-   
#   within(ks_swd_working, 
#          is_class1[well_type == 'class1'] <- 'class1')
# 
# # identify manually assigned class1 wells
# ks_swd_working <-   
#   within(ks_swd_working, 
#          is_class1[man_well_type 
#                    %in% 
#                      c('class1',
#                        'prob_swd_prob_class1',
#                        'prob_class1_to_prob_swd', 
#                        'def_class1_to_prob_swd', 
#                        'prob_swd_to_prob_class1'
#                      )
#                    ] <- 
#            'class1'
#   )
# 
# write.csv(ks_swd_working, 
#           file = "ks_Swd_working.csv")

# #### variable assignments no longer necessary
# # raw notes on manual well assignments
# ks_clean$assignment_notes <- NA
# 
# # if comments on the intial well have already been reviewed ('yes', 'NA')
# ks_clean$comments_examined <- NA
# 
# # whether I reviewed documents from the Kansas Geological Society ('yes', 'no')
# ks_clean$kgs_available_documents_verified <- NA




# # make vectors of status1s for further investigation or not
# ks_status1_check_further <- 
#   sort(c("INTENT",
#          "OTHER()",
#          "OTHER(NULL)",
#          "OTHER(OTHER)",
#          "OTHER(TA)",
#          "OTHER(TEMP ABD)",
#          "OTHER-P&A()",
#          "OTHER-P&A(TA)"))
# 
# ks_potential_disposal_include_status1s <- 
#   sort(c("OTHER()",
#          "OTHER(1O&1SWD)",
#          "OTHER(CBM/SWD)",
#          "OTHER(CLASS ONE (OLD))",
#          "OTHER(CLASS1)",
#          "OTHER(NHDW)",
#          "OTHER(NULL)",
#          "OTHER(OIL,SWD)",
#          "OTHER(OTHER)",
#          "OTHER(SWD-P&A)",
#          "OTHER(TA)",
#          "OTHER(TEMP ABD)",
#          "OTHER-P&A()",
#          "OTHER-P&A(CLASS ONE (OLD))",
#          "OTHER-P&A(OIL-SWD)",
#          "OTHER-P&A(TA)",
#          "SWD",
#          "SWD-P&A"))
# 
# ks_definitely_include_status1s <- 
#   sort(c("OTHER(1O&1SWD)",
#          "OTHER(CBM/SWD)",
#          "OTHER(CLASS ONE (OLD))",
#          "OTHER(CLASS1)",
#          "OTHER(NHDW)",
#          "OTHER(OIL,SWD)",
#          "OTHER(SWD-P&A)",
#          "OTHER-P&A(CLASS ONE (OLD))",
#          "OTHER-P&A(OIL-SWD)",
#          "SWD",
#          "SWD-P&A"))
# 
# ks_potential_disposal_exclude_status1s <- 
#   sort(c("INTENT",
#          "INJ",
#          "INJ-P&A",
#          "OTHER(INJ or EOR)",
#          "OTHER-P&A(INJ OR )",
#          "OTHER-P&A(INJ or EOR)",
#          "CBM",
#          "CBM-P&A",
#          "D&A",
#          "EOR",
#          "EOR-P&A",
#          "GAS",
#          "GAS-P&A",
#          "LOC",
#          "O&G",
#          "O&G-P&A",
#          "OIL",
#          "OIL-P&A",
#          "OTHER-P&A(2 OIL)",
#          "OTHER-P&A(CATH)",
#          "OTHER-P&A(COREHOLE)",
#          "OTHER-P&A(GAS-INJ)",
#          "OTHER-P&A(GAS-STG)",
#          "OTHER-P&A(GSW)",
#          "OTHER-P&A(LH)",
#          "OTHER-P&A(OBS)",
#          "OTHER-P&A(OIL&GAS-INJ)",
#          "OTHER-P&A(SHUT-IN)",
#          "OTHER-P&A(STRAT)",
#          "OTHER-P&A(WATER)",
#          "OTHER(2OIL)",
#          "OTHER(ABD LOC)",
#          "OTHER(CATH)",
#          "OTHER(COREHOLE)",
#          "OTHER(GAS-INJ)",
#          "OTHER(GAS-STG)",
#          "OTHER(GAS INJ)",
#          "OTHER(GAS SHUT-IN)",
#          "OTHER(GSW)",
#          "OTHER(HELIUM)",
#          "OTHER(LH)",
#          "OTHER(Monitor)",
#          "OTHER(MONITOR)",
#          "OTHER(OBS)",
#          "OTHER(OBSERVATION)",
#          "OTHER(OIL&GAS-INJ)",
#          "OTHER(Oil)",
#          "OTHER(OIL/GAS)",
#          "OTHER(SHUT-IN)",
#          "OTHER(STRAT)",
#          "OTHER(WATER)"))
# 
# # check lengths of vectors
# length(ks_potential_disposal_include_status1s) # count included statii
# length(ks_potential_disposal_exclude_status1s) # count excluded statii
# length(ks_all_status1s) # above two numbers should sum to this number
# 
# 
# 
# #### selecting rows based on status2
# # make vector of status2s to include regardless of status1
# ks_potential_disposal_include_status2s <- 
#   sort(c("Converted to SWD Well")) 
# 
# # vector of status2s that don't guarantee inclusion
# ks_potential_disposal_exclude_status2s <- 
#   setdiff(ks_all_status2s,
#           ks_potential_disposal_include_status2s)
# ks_potential_disposal_exclude_status2s



# #### checking ambiguous wells for inclusion or exclusion
# 
# # # get counts by well status1 (main status)
# # ks_well_counts_by_status1 <- 
# #   table(ks_working_with_block_groups$STATUS) # get counts
# # ks_well_counts_by_status1 <- 
# #   as.data.frame(ks_well_counts_by_status1) # convert to dataframe
# # save(ks_well_counts_by_status1, 
# #      file = "ks_well_counts_by_status1.rdata")
# # write.csv(ks_well_counts_by_status1, 
# #           file = "ks_well_counts_by_status1.csv") # write for excel file
# # 
# # # get counts by well status2
# # ks_well_counts_by_status2 <- 
# #   table(ks_working_with_block_groups$STATUS2)
# # ks_well_counts_by_status2 <- 
# #   as.data.frame(ks_well_counts_by_status2) # convert to dataframe
# # save(ks_well_counts_by_status2, 
# #      file = "ks_well_counts_by_status2.rdata")
# # write.csv(ks_well_counts_by_status2, 
# #           file = "ks_well_counts_by_status2.csv")
# 
# # make vectors of status1s and status2s
# ks_all_status1s <- 
#   sort(unique(ks_working_with_block_groups$STATUS)) # vector of all STATUS values
# ks_all_status2s <- 
#   sort(unique(ks_working_with_block_groups$STATUS2)) # vector of all STATUS2 values
# save(ks_all_status1s, 
#      file = "ks_all_status1s.rdata")
# save(ks_all_status2s, # save status1s to file
#      file = "ks_all_status2s.rdata") # save status2s to file
# # write.csv(ks_all_status1s, 
# #           file = "ks_all_status1s.csv") # csv of status1s for excel
# # write.csv(ks_all_status2s, 
# #           file = "ks_all_status2s.csv") # csv of status2s for excel




# #### old date propogation
#
# # categories for propogated dates
# ks_clean$permit_propogate <-
#   ks_clean$permit_as_date
# ks_clean$spud_propogate <-
#   ks_clean$spud_as_date
# ks_clean$completion_propogate <-
#   ks_clean$completion_as_date
# ks_clean$plugging_propogate <-
#   ks_clean$plugging_as_date
# ks_clean$modified_propogate <-
#   ks_clean$modified_as_date
# 
# # 
# # # propogate dates (BEWARE, THIS STEP TAKE ABOUT 20 MINUTES!)
# # ks_working <- 
# #   ks_working %>% 
# #   group_by(API_NUMBER_SIMPLE) %>% 
# #   fill(
# #     permit_propogate, 
# #     spud_propogate, 
# #     completion_propogate, 
# #     plugging_propogate, 
# #     modified_propogate
# #   ) %>% 
# #   ungroup()
# # 



# #### sidenote -- wells in the mississippi 
# mississippian_wells <- 
#   ks_wells_2018_11_01[ks_wells_2018_11_01$PRODUCE_FORM 
#                       %like% "Mississippian",]
# 
# View(mississippian_wells)

#### whatnot about UIC
# View(ks_uic)
# kids<-unique(ks_uic$KGS_ID)
# kids
#
# ks_wells_in_uic_data_only <- subset(ks_swd_working, KID %in% kids)
# View(ks_wells_in_uic_data_only)
# uic_statuses<-table(ks_wells_in_uic_data_only$STATUS)
# View(uic_statuses)
# write.csv(uic_statuses,file="uic_statuses.csv")




# #### assign has api
# 
# # assign it
# ks_swd_working$has_api <-
#   ifelse(ks_swd_working$API_NUMBER == "", "no", "yes")
# # View(ks_swd_working)
#
# #### update master dataframe to make later analysis easier
# ks_swd_top_of_the_flowchart <-   # start the dataframe
#   top_of_the_flowchart
# 
# ks_swd_top_of_the_flowchart$origin <- NA   # start the origin column
# 
# ks_swd_top_of_the_flowchart <-   # assign s1 origins
#   within(ks_swd_top_of_the_flowchart, 
#          origin[KID %in% ks_swd_statii_one$KID] <- 's1')
# 
# ks_swd_top_of_the_flowchart <-   # assign s2 origins
#   within(ks_swd_top_of_the_flowchart, 
#          origin[KID %in% ks_swd_statii_two$KID] <- 's2')
# 
# ks_swd_top_of_the_flowchart <-   # assign 'other' origins
#   within(ks_swd_top_of_the_flowchart, 
#          origin[KID %in% raw_assignments_exclude_swd$KID] <- 'man')
# 
# 
# 
# # View(ks_swd_statii_one$KID)
# # View(ks_swd_statii_two$KID)
# # View(raw_assignments_exclude_swd$KID)
# 
# # fix that one api
# ks_swd_top_of_the_flowchart <-   # fix big one
#   within(ks_swd_top_of_the_flowchart, 
#          API_NUMBER[KID == "1030570876"] <- "15-009-07176")
# ks_swd_top_of_the_flowchart <-   # fix simple one
#   within(ks_swd_top_of_the_flowchart, 
#          API_NUMBER_SIMPLE[KID == "1030570876"] <- "15-009-07176")
# 
# # kids of manual wells with no api (n = 2 after manual review)
# 
# # View(raw_assignments_exclude_swd)   # exclude those not swd, leaves 354
# 
# raw_assignments_api_only <-   # exclude those without API, leaves 173
#   raw_assignments_exclude_swd[which(raw_assignments_exclude_swd$API_NUMBER != ""),]
# 
# raw_assignments_api_and_swd <-   # those classed as swd with apis, leaves 106
#   raw_assignments_api_only[which(raw_assignments_api_only$man_well_type_swd == 'yes'),]
# View(raw_assignments_api_and_swd) 
# 
# # kids of non-swd wells dropped per manual review
# View(raw_assignments_man_not_swd$KID)
# 
# # kids of s2 apis that overlapped with s1 apis
# View(kids_of_removed_s2_well_apis_overlap_with_s1)
# 
# # kids of man apis that overlapped with s1 apis
# View(kids_of_removed_man_well_apis_overlap_with_s1)
# 
# # kids of s2 apis that overlapped with man apis
# kids_of_s2_apis_that_overlapped_with_s1_apis <- c("1002886382")
# 
# # assign drop_dup values and reasons
# ks_swd_top_of_the_flowchart$drop_dup <- NA
# 
# # assign dropped wells among lat/long s2 to not s2 dups
# kids_drop_lat_long
# 
# # assign wells dropped due to missing API
# 
# # assign wells dropped due to full API duplicates
# 
# # assign wells dropped due to partial API duplicates
# 
# # assign final wells dropped due to lat_long



# 
# write.csv(ks_analysis_dataset, file = "ks_analysis_dataset.csv")
# 
# HERE BE ANCIENT CODE FROM THE OLD ASSIGNMENT METHODOLOGY
# 
# # length(kids_from_manual_assignments) # count of manual KIDs
# # length(kids_status1s)  # count of status1 KIDs
# # length(kids_status2s)  # count of status2 KIDs
# 
# # combine the lists
# kids_semi_final_list <- c(kids_from_manual_assignments, 
#                           kids_status1s,
#                           kids_status2s)
# 
# # convert the list of three vectors into just one vector
# kids_semi_final_list <- 
#   unlist(kids_semi_final_list)
# 
# kids_semi_final_list <-   # delete duplicates
#   unique(kids_semi_final_list)   
# 
# ks_semi_final_wells <-   # make dataframe of data from selected wells
#   ks_working_with_block_groups[which(ks_working_with_block_groups$KID %in% 
#                    kids_semi_final_list),]
# 
# # View(ks_semi_final_wells)
# 
# # save data to disk
# save(ks_semi_final_wells, file = "ks_semi_final_wells.rdata")
# write.csv(ks_semi_final_wells, file = "ks_semi_final_wells.csv")
# 
# 
# 
# #### create working semi-final dataset
# 
# ks_semi_final_wells_working <-   # make safe working dataset
#   ks_semi_final_wells
# 
# # status1s
# 
# ks_semi_final_status1s <-   # make vector of status1s in semi-final dataset
#   sort(unique(ks_semi_final_wells$STATUS))
# 
# # View(ks_semi_final_status1s)   # view vector of status1s
# 
# # View(table(ks_semi_final_wells_working$STATUS))   # view table of status1s
# 
# write.csv(ks_semi_final_status1s,   # write .csv of status1s
#           file = "ks_semi_final_status1s.csv")
# 
# # status2s
# 
# ks_semi_final_status2s <-   # make vector of status2s in semi-final dataset
#   sort(unique(ks_semi_final_wells$STATUS2))
# 
# # View(ks_semi_final_status2s)   # view vector of status2s
# 
# # View(table(ks_semi_final_wells_working$STATUS2))   # view table of status2s
# 
# write.csv(ks_semi_final_status2s,   # write .csv of status2s
#           file = "ks_semi_final_status2s.csv")
#
#
# #### HERE BE ANCIENT CODE FROM WHEN WE THOUGHT WE'D LOOK AT CLASS I TOO
# # make class1 dataset
# ks_class1_only <- 
#   subset(ks_semi_final_wells_working, 
#          is_class1 == 'class1')
# # View(ks_class1_only)
# 
# # fix duplicated latitudes and longitudes #
# ks_class1_dup_index <- 
#   duplicated(ks_class1_only[c("LATITUDE","LONGITUDE")]) | 
#   duplicated(ks_class1_only[c("LATITUDE","LONGITUDE")], 
#              fromLast = TRUE)
# ks_class1_dup <- 
#   ks_class1_only[ks_class1_dup_index, ]
# # View(ks_class1_dup)
# 
# # KIDs of all class1 duplicates
# kids_all_class1_duplicates <- 
#   ks_class1_dup$KID
# 
# # order by latitude, UIC status, and date propogated
# ks_class1_dup <- 
#   ks_class1_dup[order(ks_class1_dup$LATITUDE, 
#                         ks_class1_dup$LONGITUDE, 
#                         ks_class1_dup$uic, 
#                         ks_class1_dup$modified_propogate),]
# 
# # View(ks_class1_dup)
# 
# # propogate dates
# ks_class1_dup <-
#   ks_class1_dup %>%
#   group_by(LATITUDE, LONGITUDE) %>%
#   fill(
#     permit_propogate,
#     spud_propogate,
#     completion_propogate,
#     plugging_propogate,
#     modified_propogate
#   ) %>%
#   ungroup()
# 
# # View(ks_class1_dup)
# 
# # vector marking all but last well with same lat/long as TRUE
# ks_class1_drop_index <-   
#   duplicated(ks_class1_dup[c("LATITUDE","LONGITUDE")], 
#              fromLast = TRUE)
# 
# # makes dataframe of rows of wells to drop
# class1_wells_to_drop <-    
#   ks_class1_dup[ks_class1_drop_index, ]
# 
# # makes list of just the kids of the wells to drop
# kids_of_class1_wells_to_drop <-   
#   class1_wells_to_drop$KID
# 
# # make drop dup category
# ks_class1_only$drop_dup <- NA
# 
# # flag all duplicates
# ks_class1_only <- 
#   within(ks_class1_only, 
#          drop_dup[KID %in% 
#                     kids_all_class1_duplicates] <- 
#            'keep_dup_lat_long') 
# 
# # flag drops 
# ks_class1_only <- 
#   within(ks_class1_only, 
#          drop_dup[KID %in% 
#                     kids_of_class1_wells_to_drop] <- 
#            'drop_dup_lat_long') 
# 
# # replace with propogated dates
# ks_class1_only$permit_propogate[
#   match(ks_class1_dup$KID, 
#         ks_class1_only$KID)] <- 
#   ks_class1_dup$permit_propogate
# 
# ks_class1_only$spud_propogate[
#   match(ks_class1_dup$KID, 
#         ks_class1_only$KID)] <- 
#   ks_class1_dup$spud_propogate
# 
# ks_class1_only$completion_propogate[
#   match(ks_class1_dup$KID, 
#         ks_class1_only$KID)] <- 
#   ks_class1_dup$completion_propogate
# 
# ks_class1_only$plugging_propogate[
#   match(ks_class1_dup$KID, 
#         ks_class1_only$KID)] <- 
#   ks_class1_dup$plugging_propogate
# 
# ks_class1_only$modified_propogate[
#   match(ks_class1_dup$KID, 
#         ks_class1_only$KID)] <- 
#   ks_class1_dup$modified_propogate
# 
# View(ks_class1_only)
# 
# write.csv(ks_class1_only, 
#           file = "ks_class1_only.csv")
#
#
#
# # HERE BE MORE CODE FROM THE OLD ASSIGNMENT METHODS #
# # assign well_type 
#
# # View(table(ks_semi_final_wells_working$STATUS)) view possible status1s
#
# ks_definitely_include_status1s <- # reminder of status1s chosen to keep
#   sort(c("OTHER(1O&1SWD)",
#          "OTHER(CBM/SWD)",
#          "OTHER(CLASS ONE (OLD))",
#          "OTHER(CLASS1)",
#          "OTHER(NHDW)",
#          "OTHER(OIL,SWD)",
#          "OTHER(SWD-P&A)",
#          "OTHER-P&A(CLASS ONE (OLD))",
#          "OTHER-P&A(OIL-SWD)",
#          "SWD",
#          "SWD-P&A"))
# 
# swd_status1s <-   # make vector of status1s that mean swd well
#   sort(c("OTHER(1O&1SWD)",
#          "OTHER(CBM/SWD)",
#          "OTHER(OIL,SWD)",
#          "OTHER(SWD-P&A)",
#          "OTHER-P&A(OIL-SWD)",
#          "SWD",
#          "SWD-P&A"))
# 
# class1_status1s <-   # make vector of status1s that mean class1 well
#   sort(c("OTHER(CLASS ONE (OLD))",
#          "OTHER(CLASS1)",
#          "OTHER(NHDW)",
#          "OTHER-P&A(CLASS ONE (OLD))"))
# 
# swd_status2s <-   # make vector of status2s that mean swd well
#   sort(c("Converted to SWD Well"))
# 
# ks_semi_final_wells_working <-   # assign class1
#   within(ks_semi_final_wells_working, 
#          well_type[STATUS %in% class1_status1s] <- 'class1')
# 
# ks_semi_final_wells_working <-   # assign swd
#   within(ks_semi_final_wells_working, 
#          well_type[STATUS %in% swd_status1s | 
#                      STATUS2 %in% swd_status2s] <- 'swd')
# 
# # remove non_swds
# ks_swd_only <- 
#   subset(ks_semi_final_wells_working, 
#          is_swd == "swd")
# 
# nrow(ks_swd_only)
# 
# # deleting those with duplicate apis
# # make dataset without wells without apis
# ks_only_apis <- 
#   ks_swd_only[!(
#     is.na(ks_swd_only$API_NUMBER) | 
#       ks_swd_only$API_NUMBER==""
#     ),]
# 
# table(ks_only_apis$is_swd)
# table(ks_only_apis$is_class1)
# nrow(ks_only_apis)
# 
# # find duplicate APIs
# ks_final_index  <-   # make index of API duplicates
#   duplicated(ks_only_apis$API_NUMBER) | 
#   duplicated(ks_only_apis$API_NUMBER, 
#              fromLast = TRUE)
# 
# # make dataframe of duplicates
# ks_api_dups <-   
#   ks_only_apis[ks_final_index, ]
# # View(ks_api_dups)
# 
# # pull kids of dups
# kids_ks_api_dups <- 
#   ks_api_dups$KID
# 
# save(ks_api_dups, 
#      file = "ks_api_dups.rdata")
# 
# write.csv(ks_api_dups, 
#           file = "ks_api_dups.csv")
# 
# #### review duplicate APIs manually here
# 
# ks_api_dups_assigned <-   # import manual assignments
#   fread("ks_api_dups_2018_12_28_back_to_r.csv")
# 
# # View(ks_api_dups_assigned)
# 
# ks_api_dups_rows_to_drop <-   # make dataframe of rows to drop
#   filter(ks_api_dups_assigned, 
#          KEEP == 'n')
# 
# kids_of_dup_api_rows_to_drop <-  # make list of kids of rows to drop
#   ks_api_dups_rows_to_drop$KID
# 
# # View(ks_api_dups_rows_to_drop)
# 
# # make column for drop for api
# ks_only_apis$drop_dup <- NA
# 
# # flag all api dups
# ks_only_apis <-   
#   within(ks_only_apis, 
#          drop_dup[KID %in% kids_ks_api_dups] <- 
#            'keep_dup_api')
# 
# # flag drops for api
# ks_only_apis <-   
#   within(ks_only_apis, 
#          drop_dup[KID %in% kids_of_dup_api_rows_to_drop] <- 
#            'drop_dup_api')
# 
# # order ks_only_apis by drop or keep
# ks_only_apis <- 
#   ks_only_apis[order(ks_only_apis$drop_dup),]
# 
# # View(ks_only_apis)
# 
# # # propogate dates (BEWARE, THIS STEP TAKE ABOUT 20 MINUTES!)
# # ks_only_apis <-
# #   ks_only_apis %>%
# #   group_by(API_NUMBER) %>%
# #   fill(
# #     permit_propogate,
# #     spud_propogate,
# #     completion_propogate,
# #     plugging_propogate,
# #     modified_propogate
# #   ) %>%
# #   ungroup()
# 
# # View(ks_only_apis)
# 
# # remove duplicate APIs, keeping those last modified
# ks_bye_dup_api <- 
#   filter(ks_only_apis, 
#          KID %notin% kids_of_dup_api_rows_to_drop)
# 
# nrow(ks_bye_dup_api) # get number for flowchart
# 
# 
# 
# #### dealing with API duplication based on 4-digit activity code
# 
# # find duplicate APIs without extraneous four digits
# ks_simple_api_index <- 
#   duplicated(ks_bye_dup_api$API_NUMBER_SIMPLE) | 
#   duplicated(ks_bye_dup_api$API_NUMBER_SIMPLE, 
#              fromLast = TRUE)
# 
# ks_simple_api_dups <- 
#   ks_bye_dup_api[ks_simple_api_index, ]
# 
# # pull kids of simple api dups
# kids_simple_api_dups <- 
#   ks_simple_api_dups$KID
# 
# write.csv(ks_simple_api_dups, 
#           file = "ks_simple_api_dups.csv")
# 
# # View(ks_simple_api_dups)
# 
# # put duplicates of core API code in order
# ks_simple_api_dups_order <-   
#   ks_simple_api_dups[
#     order(ks_simple_api_dups$API_NUMBER),
#     ]
# 
# # View(ks_simple_api_dups_order)
# 
# # marks all but last api for each well as TRUE
# simple_api_drop_index <-    
#   duplicated(ks_simple_api_dups_order$API_NUMBER_SIMPLE, 
#               fromLast = TRUE)
# 
# # makes dataframe of rows of wells to drop
# wells_to_drop_due_to_api <-    
#   ks_simple_api_dups_order[simple_api_drop_index, ]
# 
# kids_of_simple_wells_to_drop <-   # makes list of just the kids
#   wells_to_drop_due_to_api$KID
# 
# # flag all simple api dups
# ks_only_apis <-    
#   within(ks_only_apis, 
#          drop_dup[KID %in% kids_simple_api_dups] <- 
#            'keep_dup_simple_api')
# 
# ks_only_apis <-    # flag drops for api
#   within(ks_only_apis, 
#          drop_dup[KID %in% kids_of_simple_wells_to_drop] <- 
#            'drop_dup_simple_api')
# # View(ks_only_apis)
# 
# ks_api_dups_gone <-   # remove rows marked as duplicates!
#   subset(ks_only_apis, 
#          ks_only_apis$drop_dup %notin% 
#            c('drop_dup_simple_api', 
#              'drop_dup_api'))
# # View(ks_api_dups_gone)
# 
# 
# 
# #### fix duplicated latitudes and longitudes
# ks_lat_long_dup_index <- 
#   duplicated(ks_api_dups_gone[c("LATITUDE","LONGITUDE")]) | 
#   duplicated(ks_api_dups_gone[c("LATITUDE","LONGITUDE")], 
#              fromLast = TRUE)
# ks_lat_long_dup <- 
#   ks_api_dups_gone[ks_lat_long_dup_index, ]
# # View(ks_lat_long_dup)
# 
# write.csv(ks_lat_long_dup, 
#           file = "ks_lat_long_dup.csv")
# 
# # order by latitude, UIC status, and date propogated
# ks_lat_long_dup <- 
#   ks_lat_long_dup[order(ks_lat_long_dup$LATITUDE, 
#                         ks_lat_long_dup$LONGITUDE, 
#                         -rank(ks_lat_long_dup$uic), 
#                         ks_lat_long_dup$modified_propogate),]
# 
# table(ks_lat_long_dup$uic)
# 
# # # propogate dates
# # ks_lat_long_dup <-
# #   ks_lat_long_dup %>%
# #   group_by(LATITUDE, LONGITUDE) %>%
# #   fill(
# #     permit_propogate,
# #     spud_propogate,
# #     completion_propogate,
# #     plugging_propogate,
# #     modified_propogate
# #   ) %>%
# #   ungroup()
# 
# 
# 
# # View(ks_lat_long_dup)
# 
# # vector marking all but last well with same lat/long as TRUE
# ks_lat_long_drop_index <-   
#   duplicated(ks_lat_long_dup[c("LATITUDE","LONGITUDE")], 
#              fromLast = TRUE)
# 
# # makes dataframe of rows of wells to drop
# wells_to_drop_due_to_lat_long <-    
#   ks_lat_long_dup[ks_lat_long_drop_index, ]
# 
# table(wells_to_drop_due_to_lat_long$uic)
# # View(wells_to_drop_due_to_lat_long)
# 
# # makes list of just the kids
# kids_of_wells_to_drop_due_to_lat_long <-   
#   wells_to_drop_due_to_lat_long$KID
# 
# # flag drops for lat-long
# ks_only_apis <- 
#   within(ks_only_apis, 
#          drop_dup[KID %in% 
#                     kids_of_wells_to_drop_due_to_lat_long] <- 
#            'drop_dup_lat_long') 
#   
# # replace with propogated dates
# ks_only_apis$permit_propogate[
#   match(ks_lat_long_dup$KID, 
#         ks_only_apis$KID)] <- 
#   ks_lat_long_dup$permit_propogate
# 
# ks_only_apis$spud_propogate[
#   match(ks_lat_long_dup$KID, 
#         ks_only_apis$KID)] <- 
#   ks_lat_long_dup$spud_propogate
# 
# ks_only_apis$completion_propogate[
#   match(ks_lat_long_dup$KID, 
#         ks_only_apis$KID)] <- 
#   ks_lat_long_dup$completion_propogate
# 
# ks_only_apis$plugging_propogate[
#   match(ks_lat_long_dup$KID, 
#         ks_only_apis$KID)] <- 
#   ks_lat_long_dup$plugging_propogate
# 
# ks_only_apis$modified_propogate[
#   match(ks_lat_long_dup$KID, 
#         ks_only_apis$KID)] <- 
#   ks_lat_long_dup$modified_propogate
# 
# # View(ks_only_apis)
# 
# # remove all rows marked as duplicates
# ks_dups_gone <-   
#   ks_only_apis[which(ks_only_apis$drop_dup %notin% 
#                        c('drop_dup_lat_long', 
#                          'drop_dup_simple_api', 
#                          'drop_dup_api')),]
# 
# # nrow(ks_dups_gone) # get number left after removing duplicate lat/longs
# 
# ks_swd_full_for_map <- # make mapping dataset
#   ks_dups_gone
# 
# # save dataset for mapping
# save(ks_swd_full_for_map, 
#      file = "ks_swd_full_for_map.rdata")
# # View(ks_swd_full_for_map)
# 
# # table(ks_swd_full_for_map$activity)
# # table(ks_swd_full_for_map$STATUS)
# 
# # what do we need?!
# 
# ks_wells_and_block_groups <- 
#   ks_swd_full_for_map
# # View(ks_wells_and_block_groups)
#
#
# 
# # more old code
# # ks_semi_final_wells_working <- within(ks_semi_final_wells_working, well_type[man_well_type == 'class1'] <- 'class1')
# # ks_semi_final_wells_working <- within(ks_semi_final_wells_working, well_type[man_well_type == 'swd_class1'] <- 'swd_class1')
# # ks_semi_final_wells_working <- within(ks_semi_final_wells_working, activity[man_activity == 'pa'] <- 'pa')
# # ks_semi_final_wells_working <- within(ks_semi_final_wells_working, activity[man_activity == 'never_completed'] <- 'never_completed')
# # ks_semi_final_wells_working <- within(ks_semi_final_wells_working, other[STATUS %in% c("SWD","SWD-P&A")] <- 'no')
# # 
# # # assign not-others to "yes"
# # ks_semi_final_wells_working$other[is.na(ks_semi_final_wells_working$other)]  <-  "yes"
# # ks_semi_final_wells_working$activity[is.na(ks_semi_final_wells_working$activity)]  <-  "not_pa"
# # ks_semi_final_wells_working$manual[is.na(ks_semi_final_wells_working$man_well_type)] <- "no"
# # ks_semi_final_wells_working$manual[is.na(ks_semi_final_wells_working$manual)] <- "yes"
# # 
# # View(ks_semi_final_wells_working)
# # 
# # View(table(ks_semi_final_wells_working$well_type,ks_semi_final_wells_working$has_api))
# # 
# # save(ks_semi_final_wells_working, file = "ks_semi_final_wells_working.rdata")
# 
# 
# 
# # # drop never-completeds
# # no_never_complete <- subset(true_no_dup_API, !(activity == "never_completed"))
# # View(no_never_complete)
# 
# 
# 
# 
# ##### counting duplicates
#
# # count unique APIs
# number_of_unique_APIs <- length(unique(ks_working_with_block_groups$API_NUMBER))
# save(number_of_unique_APIs, file = "number_of_unique_APIs.rdata")
# 
# # raw well count
# raw_well_count <- nrow(ks_working_with_block_groups)
# save(raw_well_count, file = "raw_well_count.rdata")
# 
# # overall counts by API
# ks_by_API_count <- table(ks_working_with_block_groups$API_NUMBER)
# save(ks_by_API_count, file = "ks_by_API_count.rdata")
# 
# # view rows with duplicated APIs
# index <- 
#   duplicated(ks_working_with_block_groups$API_NUMBER) | duplicated(ks_working_with_block_groups$API_NUMBER, 
#                                                  fromLast = TRUE)
# ks_api_dups_all_wells <- ks_working_with_block_groups[index,]
# save(ks_api_dups_all_wells, file = "ks_api_dups_all_wells.rdata")
# 
# # view counts of duplicates per API
# ks_api_dup_counts <- table(ks_api_dups_all_wells$API_NUMBER)
# save(ks_api_dup_counts, file = "ks_dup_API_count.rdata")
# 
# # count unique KIDs
# unique_KID_count <- length(unique(ks_working_with_block_groups$KID))
# save(unique_KID_count, file = "unique_KID_count.rdata")
# 
# # counts by well status for rows without APIs
# ks_wells_no_API <- 
#   ks_working_with_block_groups[which(ks_working_with_block_groups$API_NUMBER == ""),] # isolate rows for wells sans APIs
# save(ks_wells_no_API, 
#      file = "ks_wells_no_API.rdata") # save the above
# ks_wells_no_API_by_status <- 
#   table(ks_wells_no_API$STATUS) # table of counts of wells sans API by status
# save(ks_wells_no_API_by_status, 
#      file = "ks_wells_no_API_by_status.rdata") # save table of counts
# 
#
#
# # informal comparisons of old and new methods of duplicate removal #
# table(ks_swd_final_better_dups$STATUS)
# table(ks_working_with_block_groups$STATUS)
#
# View(ks_swd_final_better_dups)
# 
# nrow(ks_swd_final_better_dups)
# 
# View(ks_swd_full_for_map)
# 
# little_new_dataframe <- 
#   ks_swd_final_better_dups[,c("KID","API_NUMBER_SIMPLE")]
# 
# little_old_dataframe <- 
#   ks_swd_full_for_map[,c("KID","API_NUMBER_SIMPLE")]
# 
# kids_of_differences_1 <- 
#   setdiff(little_new_dataframe$KID, 
#           little_old_dataframe$KID)
# 
# kids_of_differences_2 <- 
#   setdiff(little_old_dataframe$KID, 
#         little_new_dataframe$KID)
# 
# differences <- c(kids_of_differences_1, kids_of_differences_2)
# 
# dataframe_of_difference <- 
#   filter(ks_working_with_block_groups, 
#          KID %in% differences)
# 
# View(dataframe_of_difference)
# 
# dataframe_of_difference$old_or_new <- NA
# 
# dataframe_of_difference <- 
#   within(dataframe_of_difference, 
#          old_or_new[KID %in% kids_of_differences_1] <- "new")
# dataframe_of_difference <- 
#   within(dataframe_of_difference, 
#          old_or_new[KID %in% kids_of_differences_2] <- "old")
# 
# View(dataframe_of_difference)
# 
# write.csv(dataframe_of_difference, 
#           file = "dataframe_of_difference.csv")
# 
# # the one in the old without a matching in the new is 
# # KID 1002916703, API 15-073-20645
# 
# figure_out <- 
#   ks_working_with_block_groups[which(
#     ks_working_with_block_groups$API_NUMBER_SIMPLE == "15-073-20645"),]
# 
# View(ks_working_with_block_groups)
# 
# figure_out_lat <- 
#   ks_working_with_block_groups[which(
#     ks_working_with_block_groups$LATITUDE == 38.03855),]
# 
# View(figure_out_lat)
# 
# # pair of weird one is 1036244004, typo in API between them but same lat/long
# 
# View(ks_swd_full_for_map[which(
#   ks_swd_full_for_map$KID %in% c("1036244004", "1002916703")),])
# 
# View(ks_swd_final_better_dups[which(
#   ks_swd_final_better_dups$KID %in% c("1036244004", "1002916703", "1044234469")),])
# 
# table(ks_swd_final_better_dups$STATUS)
#
#
#
# # view duplicates for api
# ks_all_api_index <- 
#   duplicated(ks_only_apis$API_NUMBER_SIMPLE) | 
#   duplicated(ks_only_apis$API_NUMBER_SIMPLE, 
#              fromLast = TRUE)
# ks_all_api_duplicates <- 
#   ks_only_apis[ks_all_api_index, ]
# 
# View(ks_all_api_duplicates)
# 
# # put all duplicates in API number order
# ks_all_api_duplicates_order <- 
#   ks_all_api_duplicates[
#     order(ks_all_api_duplicates$API_NUMBER),
#     ]
#
#
# #### well selection code
# nrow(ks_wells_2018_11_01) # gives the total number of well entries
# table(ks_wells_2018_11_01$STATUS) # gives total by STATUS2
# table(ks_wells_2018_11_01$STATUS2) # gives total by STATUS2
# 
# # gives total excluding overlapping STATUS and STATUS2
# table(ks_wells_2018_11_01$STATUS, ks_wells_2018_11_01$STATUS2) 
# 
# table(rows_requiring_comment_investigation$STATUS)
# table(rows_requiring_comment_investigation$STATUS2)
# table(rows_requiring_comment_investigation$STATUS, 
#       rows_requiring_comment_investigation$STATUS2 == "Converted to SWD Well")
# table(raw_manual_well_assignments_dataframe$man_detailed_well_type)
# 
# manual_assignments_minus_swd_swdpa_convertedswd <-
#   filter(raw_manual_well_assignments_dataframe, 
#          STATUS %notin% c("SWD", "SWD-P&A") &
#            STATUS2 != "Converted to SWD Well")
# 
# nrow(manual_assignments_minus_swd_swdpa_convertedswd)
# # View(manual_assignments_minus_swd_swdpa_convertedswd)
# 
# man_well_type_table <- 
#   table(manual_assignments_minus_swd_swdpa_convertedswd$man_well_type)
# 
# # View(man_well_type_table)
# 
# three_type_table <- 
#   table(manual_assignments_minus_swd_swdpa_convertedswd$man_well_type, 
#         manual_assignments_minus_swd_swdpa_convertedswd$man_reason_for_flowchart, 
#         manual_assignments_minus_swd_swdpa_convertedswd$man_well_type_swd)
# 
# three_type_table <- as.data.frame(three_type_table)
# 
# three_type_table <- 
#   filter(three_type_table, Freq > 0)
# 
# # View(three_type_table)
# 
# reasons_for_decision <- 
#   three_type_table %>% 
#   group_by(Var2, Var3) %>% 
#   summarise(sum_thing = sum(Freq))
# 
# # View(reasons_for_decision)
# 
# decisions <- 
#   three_type_table %>% 
#   group_by(Var3) %>% 
#   summarise(sum_thing = sum(Freq))
# 
# # View(decisions)
# 
# write.csv(reasons_for_decision, 
#           file = "reasons_for_decision.csv")
# 
# 
# nrow(rows_requiring_comment_investigation)
# 
# 
# # View(ks_swd_working)
# 
# table(ks_swd_working$STATUS)
# 
# # count well entries included for further evaluation based on 
# # status 1, status 2, and manual review
# wells_for_further_evaluation <-   # make dataframe of just those wells
#   subset(ks_swd_working, is_swd == "swd")
# nrow(wells_for_further_evaluation) # get the count
# 
# # count well entries excluded for no apis out of the above dataset
# no_apis <- 
#   subset(wells_for_further_evaluation, is.na(API_NUMBER))
# nrow(no_apis) # used to count number of wells removed for not having an API
# 
# 
# 
# # count well entries 
# analysis_without_dup_apis <- 
#   subset(ks_bye_dup_api, is_swd == "swd")
# 
# # View(ks_bye_dup_api)
# 
# 
# class_1_wells <- 
#   subset(ks_swd_working, 
#          ks_swd_working$STATUS %in% c("OTHER(CLASS1)", 
#                                       "OTHER(CLASS ONE (OLD))"))
# 
# # View(class_1_wells)
