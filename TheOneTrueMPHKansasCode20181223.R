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
library(ggplot2)
library(plyr)
library(dplyr)
library(psych)
library(tcltk2)
library(bit64)
library(reshape2)
library(totalcensus)



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


# #### raw acs data import ####
# 
# variable_names <-
#   readLines(
#     "acs_2013_2017_data_supplements/comma_separated_variables.csv")
# variable_names
# 
# variable_names_to_include <-
#   readLines(
#     "acs_2013_2017_data_supplements/variable_names_to_include.csv")
# variable_names_to_include
# 
# variable_names_to_exclude <-
#   readLines("acs_2013_2017_data_supplements/variable_names_to_exclude.csv"
#             )
# variable_names_to_exclude
# 
# true_variable_candidates <-
#   variable_names[
#     grepl(
#       paste(
#         variable_names_to_include,
#         collapse="|"),
#       variable_names)]
# 
# true_variable_candidates
# 
# variable_names_for_totalcensus <-
#   true_variable_candidates[
#     !grepl(
#       paste(
#         variable_names_to_exclude,
#         collapse="|"),
#       true_variable_candidates)]
# 
# additional_exclusions <-
#   c("\\.",
#     "B15002A",
#     "B15002B",
#     "B15002C",
#     "B15002D",
#     "B15002E",
#     "B15002F",
#     "B15002G",
#     "B15002H",
#     "B15002I",
#     "B21001A",
#     "B21001B",
#     "B21001C",
#     "B21001D",
#     "B21001E",
#     "B21001F",
#     "B21001G",
#     "B21001H",
#     "B21001I")
# 
# final_variable_names_for_totalcensus <-
#   variable_names_for_totalcensus[
#     !grepl(
#       paste(
#         additional_exclusions,
#         collapse="|"),
#       variable_names_for_totalcensus
#       )
#     ]
# 
# final_variable_names_for_totalcensus
# 
# # set_path_to_census("acs_2013_2017_data")
# 
# acs_data_2013_2017_via_totalcensus <- 
#   read_acs5year(
#     year = 2017, 
#     states = "KS", 
#     table_contents = 
#       final_variable_names_for_totalcensus, 
#     summary_level = "block group", 
#     with_margin = TRUE
# )
# 
# # View(acs_data_2013_2017_via_totalcensus)
# 
# save(
#   acs_data_2013_2017_via_totalcensus,
#   file = "acs_data_2013_2017_via_totalcensus.rdata"
#   )
# 
# write.csv(
#   acs_data_2013_2017_via_totalcensus,
#   file = "acs_data_2013_2017_via_totalcensus.csv"
#   )
#
#
#
# #### raw well data import ####
# 
# ks_wells_2018_11_01 <- read.csv(file = "ks_wells_2018_11_01.txt", stringsAsFactors = FALSE) # import raw data
# save(ks_wells_2018_11_01, file = "ks_wells_2018_11_01.rdata") # save as an rdata file
# 
#
#
# #### UIC data import ####
#
# ks_uic_2018_09_04 <- fread(file = "KS_UIC_archive_2018_09_04.txt") # import raw data
# save(ks_uic_2018_09_04, file = "ks_uic_2018_09_04.rdata") # save as an rdata file
#
#
#
# #### ks blockgroup from geodatabase shapefile import ####
# 
# ks_tiger_table <- # import raw data
#   fread(
#     "ks_2017_bg_gdb_table_export.txt",
#     colClasses =
#       list(character = 'GEOID'
#       )
#     )
# 
# View(ks_tiger_table)
# 
# # fix GEOID to match ACS data
# ks_tiger_table$GEOID <-
#   paste0('15000US',
#          ks_tiger_table$GEOID
#          )
# 
# # save as .rdata file
# save(ks_tiger_table,
#      file = "ks_tiger_table.rdata"
#      )



#### load necessary files to start here ####

# load well data
load(file = "ks_wells_2018_11_01.rdata")

# load UIC data
load(file = "ks_uic_2018_09_04.rdata")

# load ACS shapefile data from its attribute table
load(file = "ks_tiger_table.rdata")

# load raw acs data
load(file = "acs_data_2013_2017_via_totalcensus.rdata") 



#### pull ACS data I need ####

# make working acs
working_acs <- 
  acs_data_2013_2017_via_totalcensus

# convert from dataframe to data table
working_acs <- 
  as.data.frame(working_acs)

# pull data from various categories
total_pop <- 
  working_acs[
    ,c(
      "GEOID", 
      "B01001_001",
      "B01001_001_margin"
    )
    ]
sex_cat <- working_acs[
  ,c(
    "GEOID", 
    "B01001_002", 
    "B01001_002_margin",
    "B01001_026", 
    "B01001_026_margin"
  )
  ]
sex_age_cat <- working_acs[
  ,c(
    "GEOID",
    "B01001_003",
    "B01001_003_margin",
    "B01001_004",
    "B01001_004_margin",
    "B01001_005",
    "B01001_005_margin",
    "B01001_006",
    "B01001_006_margin",    
    "B01001_007",
    "B01001_007_margin",
    "B01001_008",
    "B01001_008_margin",
    "B01001_009",
    "B01001_009_margin",
    "B01001_010",
    "B01001_010_margin",
    "B01001_011",
    "B01001_011_margin",
    "B01001_012",
    "B01001_012_margin",
    "B01001_013",
    "B01001_013_margin",
    "B01001_014",
    "B01001_014_margin",
    "B01001_015",
    "B01001_015_margin",
    "B01001_016",
    "B01001_016_margin",
    "B01001_017",
    "B01001_017_margin",
    "B01001_018",
    "B01001_018_margin",
    "B01001_019",
    "B01001_019_margin",
    "B01001_020",
    "B01001_020_margin",
    "B01001_021",
    "B01001_021_margin",
    "B01001_022",
    "B01001_022_margin",
    "B01001_023",
    "B01001_023_margin",
    "B01001_024",
    "B01001_024_margin",
    "B01001_025",
    "B01001_025_margin",
    "B01001_027", # 26 is total population of women and omitted
    "B01001_027_margin",
    "B01001_028",
    "B01001_028_margin",
    "B01001_029",
    "B01001_029_margin",
    "B01001_030",
    "B01001_030_margin",
    "B01001_031",
    "B01001_031_margin",
    "B01001_032",
    "B01001_032_margin",
    "B01001_033",
    "B01001_033_margin",
    "B01001_034",
    "B01001_034_margin",
    "B01001_035",
    "B01001_035_margin",
    "B01001_036",
    "B01001_036_margin",
    "B01001_037",
    "B01001_037_margin",
    "B01001_038",
    "B01001_038_margin",
    "B01001_039",
    "B01001_039_margin",
    "B01001_040",
    "B01001_040_margin",
    "B01001_041",
    "B01001_041_margin",
    "B01001_042",
    "B01001_042_margin",
    "B01001_043",
    "B01001_043_margin",
    "B01001_044",
    "B01001_044_margin",
    "B01001_045",
    "B01001_045_margin",
    "B01001_046",
    "B01001_046_margin",
    "B01001_047",
    "B01001_047_margin",
    "B01001_048",
    "B01001_048_margin",
    "B01001_049",
    "B01001_049_margin"
  )
  ]

age_median <- working_acs[
  ,c(
    "GEOID",
    "B01002_001",
    "B01002_001_margin",
    "B01002_002",
    "B01002_002_margin",
    "B01002_003",
    "B01002_003_margin"
  )
  ]

race_cat <- working_acs[
  ,c(
    "GEOID",
    "B03002_001",
    "B03002_001_margin",
    "B03002_002",
    "B03002_002_margin",
    "B03002_003",
    "B03002_003_margin",
    "B03002_004",
    "B03002_004_margin",
    "B03002_005",
    "B03002_005_margin",
    "B03002_006",
    "B03002_006_margin",
    "B03002_007",
    "B03002_007_margin",
    "B03002_008",
    "B03002_008_margin",
    "B03002_009",
    "B03002_009_margin",
    "B03002_010",
    "B03002_010_margin",
    "B03002_011",
    "B03002_011_margin",
    "B03002_012",
    "B03002_012_margin",
    "B03002_013",
    "B03002_013_margin",
    "B03002_014",
    "B03002_014_margin",
    "B03002_015",
    "B03002_015_margin",
    "B03002_016",
    "B03002_016_margin",
    "B03002_017",
    "B03002_017_margin",
    "B03002_018",
    "B03002_018_margin",
    "B03002_019",
    "B03002_019_margin",
    "B03002_020",
    "B03002_020_margin",
    "B03002_021",
    "B03002_021_margin")]
education_cat <- working_acs[
  ,c(
    "GEOID", 
    "B15003_001",
    "B15003_001_margin", 
    "B15003_002", 
    "B15003_002_margin", 
    "B15003_003", 
    "B15003_003_margin", 
    "B15003_004", 
    "B15003_004_margin", 
    "B15003_005", 
    "B15003_005_margin", 
    "B15003_006", 
    "B15003_006_margin", 
    "B15003_007", 
    "B15003_007_margin", 
    "B15003_008",  
    "B15003_008_margin",
    "B15003_009",
    "B15003_009_margin", 
    "B15003_010", 
    "B15003_010_margin", 
    "B15003_011", 
    "B15003_011_margin", 
    "B15003_012", 
    "B15003_012_margin", 
    "B15003_013", 
    "B15003_013_margin", 
    "B15003_014", 
    "B15003_014_margin", 
    "B15003_015", 
    "B15003_015_margin", 
    "B15003_016", 
    "B15003_016_margin", 
    "B15003_017", 
    "B15003_017_margin", 
    "B15003_018", 
    "B15003_018_margin", 
    "B15003_019", 
    "B15003_019_margin", 
    "B15003_020", 
    "B15003_020_margin", 
    "B15003_021", 
    "B15003_021_margin", 
    "B15003_022", 
    "B15003_022_margin", 
    "B15003_023", 
    "B15003_023_margin", 
    "B15003_024", 
    "B15003_024_margin", 
    "B15003_025", 
    "B15003_025_margin"
  )
  ]
poverty_ratio_cat <- 
  working_acs[
  ,c(
    "GEOID",
    "C17002_001",
    "C17002_001_margin", 
    "C17002_002", 
    "C17002_002_margin", 
    "C17002_003", 
    "C17002_003_margin", 
    "C17002_004", 
    "C17002_004_margin", 
    "C17002_005", 
    "C17002_005_margin", 
    "C17002_006", 
    "C17002_006_margin", 
    "C17002_007", 
    "C17002_007_margin", 
    "C17002_008", 
    "C17002_008_margin"
  )
  ]
income_house_cat <- 
  working_acs[
    ,c(
      "GEOID", 
      "B19001_001",
      "B19001_001_margin", 
      "B19001_002", 
      "B19001_002_margin", 
      "B19001_003", 
      "B19001_003_margin", 
      "B19001_004", 
      "B19001_004_margin", 
      "B19001_005", 
      "B19001_005_margin", 
      "B19001_006", 
      "B19001_006_margin", 
      "B19001_007", 
      "B19001_007_margin", 
      "B19001_008", 
      "B19001_008_margin", 
      "B19001_009", 
      "B19001_009_margin", 
      "B19001_010", 
      "B19001_010_margin", 
      "B19001_011", 
      "B19001_011_margin", 
      "B19001_012",  
      "B19001_012_margin",
      "B19001_013",
      "B19001_013_margin", 
      "B19001_014",  
      "B19001_014_margin",
      "B19001_015",
      "B19001_015_margin", 
      "B19001_016", 
      "B19001_016_margin", 
      "B19001_017", 
      "B19001_017_margin"
    )
    ]
income_house_median<-working_acs[
  ,c(
    "GEOID", 
    "B19013_001", 
    "B19013_001_margin"
  )
  ]
earnings_sex_cat <- 
  working_acs[
    ,c(
      "GEOID", 
      "B20001_001",
      "B20001_001_margin", 
      "B20001_002", 
      "B20001_002_margin", 
      "B20001_003", 
      "B20001_003_margin", 
      "B20001_004", 
      "B20001_004_margin", 
      "B20001_005", 
      "B20001_005_margin", 
      "B20001_006", 
      "B20001_006_margin", 
      "B20001_007", 
      "B20001_007_margin", 
      "B20001_008", 
      "B20001_008_margin", 
      "B20001_009", 
      "B20001_009_margin", 
      "B20001_010", 
      "B20001_010_margin", 
      "B20001_011", 
      "B20001_011_margin", 
      "B20001_012", 
      "B20001_012_margin", 
      "B20001_013", 
      "B20001_013_margin", 
      "B20001_014", 
      "B20001_014_margin", 
      "B20001_015", 
      "B20001_015_margin", 
      "B20001_016", 
      "B20001_016_margin", 
      "B20001_017", 
      "B20001_017_margin", 
      "B20001_018", 
      "B20001_018_margin", 
      "B20001_019", 
      "B20001_019_margin", 
      "B20001_020", 
      "B20001_020_margin", 
      "B20001_021", 
      "B20001_021_margin", 
      "B20001_022", 
      "B20001_022_margin", 
      "B20001_023", 
      "B20001_023_margin", 
      "B20001_024", 
      "B20001_024_margin",
      "B20001_025",
      "B20001_025_margin", 
      "B20001_026",
      "B20001_026_margin",
      "B20001_027",
      "B20001_027_margin", 
      "B20001_028", 
      "B20001_028_margin", 
      "B20001_029", 
      "B20001_029_margin", 
      "B20001_030", 
      "B20001_030_margin", 
      "B20001_031", 
      "B20001_031_margin", 
      "B20001_032", 
      "B20001_032_margin", 
      "B20001_033", 
      "B20001_033_margin", 
      "B20001_034", 
      "B20001_034_margin", 
      "B20001_035", 
      "B20001_035_margin", 
      "B20001_036", 
      "B20001_036_margin", 
      "B20001_037", 
      "B20001_037_margin", 
      "B20001_038", 
      "B20001_038_margin", 
      "B20001_039", 
      "B20001_039_margin", 
      "B20001_040", 
      "B20001_040_margin", 
      "B20001_041", 
      "B20001_041_margin", 
      "B20001_042", 
      "B20001_042_margin", 
      "B20001_043", 
      "B20001_043_margin"
    )
    ]
earnings_median <- 
  working_acs[
    ,c(
      "GEOID", 
      "B20002_001", 
      "B20002_001_margin", 
      "B20002_002", 
      "B20002_002_margin", 
      "B20002_003",
      "B20002_003_margin" 
    )
    ]
employment_cat <- # divide 5 by 3
  working_acs[
    ,c(
      "GEOID",
      "B23025_001",
      "B23025_001_margin",
      "B23025_002",
      "B23025_002_margin",
      "B23025_003",
      "B23025_003_margin",
      "B23025_004",
      "B23025_004_margin",
      "B23025_005",
      "B23025_005_margin",
      "B23025_006",
      "B23025_006_margin",
      "B23025_007",
      "B23025_007_margin"
    )
    ]
home_value_cat <- 
  working_acs[
    ,c(
      "GEOID", 
      "B25075_001",
      "B25075_001_margin", 
      "B25075_002", 
      "B25075_002_margin", 
      "B25075_003", 
      "B25075_003_margin", 
      "B25075_004", 
      "B25075_004_margin", 
      "B25075_005", 
      "B25075_005_margin", 
      "B25075_006", 
      "B25075_006_margin", 
      "B25075_007", 
      "B25075_007_margin", 
      "B25075_008", 
      "B25075_008_margin", 
      "B25075_009", 
      "B25075_009_margin", 
      "B25075_010", 
      "B25075_010_margin", 
      "B25075_011", 
      "B25075_011_margin", 
      "B25075_012", 
      "B25075_012_margin", 
      "B25075_013", 
      "B25075_013_margin", 
      "B25075_014", 
      "B25075_014_margin", 
      "B25075_015", 
      "B25075_015_margin", 
      "B25075_016", 
      "B25075_016_margin", 
      "B25075_017", 
      "B25075_017_margin", 
      "B25075_018", 
      "B25075_018_margin", 
      "B25075_019", 
      "B25075_019_margin", 
      "B25075_020", 
      "B25075_020_margin", 
      "B25075_021", 
      "B25075_021_margin", 
      "B25075_022", 
      "B25075_022_margin", 
      "B25075_023", 
      "B25075_023_margin", 
      "B25075_024", 
      "B25075_024_margin", 
      "B25075_025", 
      "B25075_025_margin", 
      "B25075_026", 
      "B25075_026_margin", 
      "B25075_027", 
      "B25075_027_margin"
    )
    ]
health_insurance_cat <- 
  working_acs[
    ,c(
      "GEOID", 
      "B27010_001", 
      "B27010_001_margin", 
      "B27010_002", 
      "B27010_002_margin", 
      "B27010_003", 
      "B27010_003_margin", 
      "B27010_004", 
      "B27010_004_margin", 
      "B27010_005", 
      "B27010_005_margin", 
      "B27010_006", 
      "B27010_006_margin", 
      "B27010_007", 
      "B27010_007_margin", 
      "B27010_008", 
      "B27010_008_margin", 
      "B27010_009", 
      "B27010_009_margin", 
      "B27010_010", 
      "B27010_010_margin", 
      "B27010_011", 
      "B27010_011_margin", 
      "B27010_012", 
      "B27010_012_margin", 
      "B27010_013", 
      "B27010_013_margin", 
      "B27010_014", 
      "B27010_014_margin", 
      "B27010_015", 
      "B27010_015_margin", 
      "B27010_016", 
      "B27010_016_margin", 
      "B27010_017",
      "B27010_017_margin", 
      "B27010_018", 
      "B27010_018_margin", 
      "B27010_019", 
      "B27010_019_margin", 
      "B27010_020", 
      "B27010_020_margin", 
      "B27010_021", 
      "B27010_021_margin", 
      "B27010_022", 
      "B27010_022_margin", 
      "B27010_023", 
      "B27010_023_margin", 
      "B27010_024", 
      "B27010_024_margin", 
      "B27010_025", 
      "B27010_025_margin", 
      "B27010_027", 
      "B27010_027_margin", 
      "B27010_028", 
      "B27010_028_margin", 
      "B27010_029", 
      "B27010_029_margin", 
      "B27010_030", 
      "B27010_030_margin", 
      "B27010_031", 
      "B27010_031_margin", 
      "B27010_032", 
      "B27010_032_margin", 
      "B27010_033", 
      "B27010_033_margin", 
      "B27010_034", 
      "B27010_034_margin", 
      "B27010_035", 
      "B27010_035_margin", 
      "B27010_036", 
      "B27010_036_margin", 
      "B27010_037", 
      "B27010_037_margin", 
      "B27010_038", 
      "B27010_038_margin", 
      "B27010_039", 
      "B27010_039_margin", 
      "B27010_040", 
      "B27010_040_margin", 
      "B27010_041", 
      "B27010_041_margin", 
      "B27010_042", 
      "B27010_042_margin", 
      "B27010_043", 
      "B27010_043_margin", 
      "B27010_044", 
      "B27010_044_margin", 
      "B27010_045",  
      "B27010_045_margin",
      "B27010_046",
      "B27010_046_margin", 
      "B27010_047",  
      "B27010_047_margin",
      "B27010_048",
      "B27010_048_margin", 
      "B27010_049", 
      "B27010_049_margin", 
      "B27010_050", 
      "B27010_050_margin", 
      "B27010_051", 
      "B27010_051_margin", 
      "B27010_052", 
      "B27010_052_margin", 
      "B27010_053", 
      "B27010_053_margin", 
      "B27010_054", 
      "B27010_054_margin", 
      "B27010_055", 
      "B27010_055_margin", 
      "B27010_056", 
      "B27010_056_margin", 
      "B27010_057", 
      "B27010_057_margin", 
      "B27010_058", 
      "B27010_058_margin", 
      "B27010_059", 
      "B27010_059_margin", 
      "B27010_060",  
      "B27010_060_margin",
      "B27010_061", 
      "B27010_061_margin",
      "B27010_062",
      "B27010_062_margin", 
      "B27010_063", 
      "B27010_063_margin", 
      "B27010_064", 
      "B27010_064_margin", 
      "B27010_065", 
      "B27010_065_margin", 
      "B27010_066", 
      "B27010_066_margin"
    )
    ]
home_value_median <- 
  working_acs[
    ,c(
      "GEOID", 
      "B25077_001", 
      "B25077_001_margin"
    )
    ]
ling_iso <- 
  working_acs[
    ,c(
      "GEOID", 
      "C16002_001", 
      "C16002_001_margin", 
      "C16002_002", 
      "C16002_002_margin", 
      "C16002_003", 
      "C16002_003_margin", 
      "C16002_004", 
      "C16002_004_margin", 
      "C16002_005", 
      "C16002_005_margin", 
      "C16002_006", 
      "C16002_006_margin", 
      "C16002_007", 
      "C16002_007_margin", 
      "C16002_008", 
      "C16002_008_margin", 
      "C16002_009", 
      "C16002_009_margin", 
      "C16002_010", 
      "C16002_010_margin", 
      "C16002_011", 
      "C16002_011_margin", 
      "C16002_012", 
      "C16002_012_margin", 
      "C16002_013", 
      "C16002_013_margin", 
      "C16002_014", 
      "C16002_014_margin"
    )
    ]
housing_tenure <- 
  working_acs[
    ,c(
      "GEOID", 
      "B25003_001", 
      "B25003_001_margin", 
      "B25003_002", 
      "B25003_002_margin", 
      "B25003_003", 
      "B25003_003_margin"
    )
    ]

# make list of all categories
variables_of_interest <- 
  list(total_pop, 
       sex_cat, 
       sex_age_cat, 
       age_median, 
       race_cat, 
       education_cat, 
       poverty_ratio_cat, 
       income_house_cat, 
       income_house_median, 
       earnings_sex_cat, 
       earnings_median, 
       employment_cat, 
       home_value_cat, 
       home_value_median, 
       health_insurance_cat, 
       ling_iso, 
       housing_tenure)

#### merge different subset data together ####

# define merge function
merge_dataframes <- 
  function(x, y) full_join(x, y, by = "GEOID")

# perform the merge function en masse
acs_select <- 
  Reduce(
    merge_dataframes, 
    variables_of_interest
  )

# save select acs data
save(acs_select, 
     file = "acs_select.rdata")

# merge select acs data with geography
acs_select_geo <- 
  full_join(ks_tiger_table, 
            acs_select, 
            by = "GEOID")
# View(acs_select_geo)

# save acs_select_geo
save(acs_select_geo, 
     file = "acs_select_geo.rdata")

write.csv(acs_select_geo, file = "acs_select_geo.csv")



#### START HERE FIX CATEGORIES ####
acs_geo_cats <-   # make data for sorting into categories
  acs_select_geo

### making variables ####
# making variables
# from Silva:
# median household income (dollars), 
# median household value (dollars), 
# percent of population identifying as White only, 
# population density (population per square mile), 
# percent of population with a high school education/GED or higher, and 
# population median age
# from RMF:  age categories


# total population
acs_geo_cats$pop_tot_B01001_001 <-
  acs_geo_cats$B01001_001

# population density (people/km2, ALAND in m2)
acs_geo_cats$pop_dense_B01001_001_ALAND <- 
  (1000000*
     acs_geo_cats$B01001_001/
     acs_geo_cats$ALAND)

# median household income
acs_geo_cats$income_house_median_B19013_001 <-
  acs_geo_cats$B19013_001

# median age
acs_geo_cats$age_median_B01002_001 <- 
  acs_geo_cats$B01002_001

# ages
acs_geo_cats$age_under_5_B01001_003_027 <- 
  acs_geo_cats$B01001_003 + acs_geo_cats$B01001_027
acs_geo_cats$age_5_to_9_B01001_004_028 <- 
  acs_geo_cats$B01001_004 + acs_geo_cats$B01001_028
acs_geo_cats$age_10_to_14_B01001_005_029 <- 
  acs_geo_cats$B01001_005 + acs_geo_cats$B01001_029 
acs_geo_cats$age_15_to_17_B01001_006_030 <- 
  acs_geo_cats$B01001_006 + acs_geo_cats$B01001_030 
acs_geo_cats$age_18_to_19_B01001_007_031 <- 
  acs_geo_cats$B01001_007 + acs_geo_cats$B01001_031 
acs_geo_cats$age_20_B01001_008_032 <- 
  acs_geo_cats$B01001_008 + acs_geo_cats$B01001_032 
acs_geo_cats$age_21_B01001_009_033 <- 
  acs_geo_cats$B01001_009 + acs_geo_cats$B01001_033 
acs_geo_cats$age_22_to_24_B01001_010_034 <- 
  acs_geo_cats$B01001_010 + acs_geo_cats$B01001_034 
acs_geo_cats$age_25_to_29_B01001_011_035 <- 
  acs_geo_cats$B01001_011 + acs_geo_cats$B01001_035 
acs_geo_cats$age_30_to_34_B01001_012_036 <- 
  acs_geo_cats$B01001_012 + acs_geo_cats$B01001_036 
acs_geo_cats$age_35_to_39_B01001_013_037 <- 
  acs_geo_cats$B01001_013 + acs_geo_cats$B01001_037 
acs_geo_cats$age_40_to_44_B01001_014_038 <- 
  acs_geo_cats$B01001_014 + acs_geo_cats$B01001_038 
acs_geo_cats$age_45_to_49_B01001_015_039 <- 
  acs_geo_cats$B01001_015 + acs_geo_cats$B01001_039 
acs_geo_cats$age_50_to_54_B01001_016_040 <- 
  acs_geo_cats$B01001_016 + acs_geo_cats$B01001_040 
acs_geo_cats$age_55_to_59_B01001_017_041 <- 
  acs_geo_cats$B01001_017 + acs_geo_cats$B01001_041 
acs_geo_cats$age_60_to_61_B01001_018_042 <- 
  acs_geo_cats$B01001_018 + acs_geo_cats$B01001_042 
acs_geo_cats$age_62_to_64_B01001_019_043 <- 
  acs_geo_cats$B01001_019 + acs_geo_cats$B01001_043
acs_geo_cats$age_65_to_66_B01001_020_044 <- 
  acs_geo_cats$B01001_020 + acs_geo_cats$B01001_044 
acs_geo_cats$age_67_to_69_B01001_021_045 <- 
  acs_geo_cats$B01001_021 + acs_geo_cats$B01001_045 
acs_geo_cats$age_70_to_74_B01001_022_046 <- 
  acs_geo_cats$B01001_022 + acs_geo_cats$B01001_046 
acs_geo_cats$age_75_to_79_B01001_023_047 <- 
  acs_geo_cats$B01001_023 + acs_geo_cats$B01001_047 
acs_geo_cats$age_80_to_84_B01001_024_048 <- 
  acs_geo_cats$B01001_024 + acs_geo_cats$B01001_048 
acs_geo_cats$age_85_to_inf_B01001_025_049 <- 
  acs_geo_cats$B01001_025 + acs_geo_cats$B01001_049 

acs_geo_cats$age_0_to_14 <- 
  acs_geo_cats$age_under_5_B01001_003_027 + 
  acs_geo_cats$age_5_to_9_B01001_004_028 + 
  acs_geo_cats$age_10_to_14_B01001_005_029

acs_geo_cats$age_0_to_17 <- 
  acs_geo_cats$age_under_5_B01001_003_027 + 
  acs_geo_cats$age_5_to_9_B01001_004_028 + 
  acs_geo_cats$age_10_to_14_B01001_005_029 + 
  acs_geo_cats$age_15_to_17_B01001_006_030

acs_geo_cats$age_0_to_19 <- 
  acs_geo_cats$age_under_5_B01001_003_027 + 
  acs_geo_cats$age_5_to_9_B01001_004_028 + 
  acs_geo_cats$age_10_to_14_B01001_005_029 + 
  acs_geo_cats$age_15_to_17_B01001_006_030 +
  acs_geo_cats$age_18_to_19_B01001_007_031 

acs_geo_cats$age_5_to_14 <- 
  acs_geo_cats$age_5_to_9_B01001_004_028 + 
  acs_geo_cats$age_10_to_14_B01001_005_029

acs_geo_cats$age_5_to_17 <- 
  acs_geo_cats$age_5_to_9_B01001_004_028 + 
  acs_geo_cats$age_10_to_14_B01001_005_029 + 
  acs_geo_cats$age_15_to_17_B01001_006_030

acs_geo_cats$age_5_to_19 <- 
  acs_geo_cats$age_5_to_9_B01001_004_028 + 
  acs_geo_cats$age_10_to_14_B01001_005_029 + 
  acs_geo_cats$age_15_to_17_B01001_006_030 +
  acs_geo_cats$age_18_to_19_B01001_007_031 

acs_geo_cats$age_10_to_17 <- 
  acs_geo_cats$age_10_to_14_B01001_005_029 + 
  acs_geo_cats$age_15_to_17_B01001_006_030
  
acs_geo_cats$age_15_to_64 <- 
  acs_geo_cats$age_15_to_17_B01001_006_030 +
  acs_geo_cats$age_18_to_19_B01001_007_031 + 
  acs_geo_cats$age_20_B01001_008_032 + 
  acs_geo_cats$age_21_B01001_009_033 + 
  acs_geo_cats$age_22_to_24_B01001_010_034 + 
  acs_geo_cats$age_25_to_29_B01001_011_035 + 
  acs_geo_cats$age_30_to_34_B01001_012_036 + 
  acs_geo_cats$age_35_to_39_B01001_013_037 + 
  acs_geo_cats$age_40_to_44_B01001_014_038 + 
  acs_geo_cats$age_45_to_49_B01001_015_039 + 
  acs_geo_cats$age_50_to_54_B01001_016_040 + 
  acs_geo_cats$age_55_to_59_B01001_017_041 + 
  acs_geo_cats$age_60_to_61_B01001_018_042 + 
  acs_geo_cats$age_62_to_64_B01001_019_043
  
acs_geo_cats$age_18_to_64 <- 
  acs_geo_cats$age_18_to_19_B01001_007_031 + 
  acs_geo_cats$age_20_B01001_008_032 + 
  acs_geo_cats$age_21_B01001_009_033 + 
  acs_geo_cats$age_22_to_24_B01001_010_034 + 
  acs_geo_cats$age_25_to_29_B01001_011_035 + 
  acs_geo_cats$age_30_to_34_B01001_012_036 + 
  acs_geo_cats$age_35_to_39_B01001_013_037 + 
  acs_geo_cats$age_40_to_44_B01001_014_038 + 
  acs_geo_cats$age_45_to_49_B01001_015_039 + 
  acs_geo_cats$age_50_to_54_B01001_016_040 + 
  acs_geo_cats$age_55_to_59_B01001_017_041 + 
  acs_geo_cats$age_60_to_61_B01001_018_042 + 
  acs_geo_cats$age_62_to_64_B01001_019_043

acs_geo_cats$age_20_to_64 <- 
  acs_geo_cats$age_20_B01001_008_032 + 
  acs_geo_cats$age_21_B01001_009_033 + 
  acs_geo_cats$age_22_to_24_B01001_010_034 + 
  acs_geo_cats$age_25_to_29_B01001_011_035 + 
  acs_geo_cats$age_30_to_34_B01001_012_036 + 
  acs_geo_cats$age_35_to_39_B01001_013_037 + 
  acs_geo_cats$age_40_to_44_B01001_014_038 + 
  acs_geo_cats$age_45_to_49_B01001_015_039 + 
  acs_geo_cats$age_50_to_54_B01001_016_040 + 
  acs_geo_cats$age_55_to_59_B01001_017_041 + 
  acs_geo_cats$age_60_to_61_B01001_018_042 + 
  acs_geo_cats$age_62_to_64_B01001_019_043

acs_geo_cats$age_65_plus <- 
  acs_geo_cats$age_65_to_66_B01001_020_044 + 
  acs_geo_cats$age_67_to_69_B01001_021_045 + 
  acs_geo_cats$age_70_to_74_B01001_022_046 + 
  acs_geo_cats$age_75_to_79_B01001_023_047 + 
  acs_geo_cats$age_80_to_84_B01001_024_048 + 
  acs_geo_cats$age_85_to_inf_B01001_025_049

# age percents
acs_geo_cats$age_under_5_percent <- 
  acs_geo_cats$age_under_5_B01001_003_027 / 
  acs_geo_cats$pop_tot_B01001_001

acs_geo_cats$age_0_to_14_percent <- 
  acs_geo_cats$age_0_to_14 / 
  acs_geo_cats$pop_tot_B01001_001

acs_geo_cats$age_0_to_17_percent <- 
  acs_geo_cats$age_0_to_17 / 
  acs_geo_cats$pop_tot_B01001_001

acs_geo_cats$age_0_to_19_percent <- 
  acs_geo_cats$age_0_to_19 / 
  acs_geo_cats$pop_tot_B01001_001

acs_geo_cats$age_5_to_14_percent <- 
  acs_geo_cats$age_5_to_14 / 
  acs_geo_cats$pop_tot_B01001_001

acs_geo_cats$age_5_to_17_percent <- 
  acs_geo_cats$age_5_to_17 / 
  acs_geo_cats$pop_tot_B01001_001

acs_geo_cats$age_5_to_19_percent <- 
  acs_geo_cats$age_5_to_19 / 
  acs_geo_cats$pop_tot_B01001_001

acs_geo_cats$age_10_to_17_percent <- 
  acs_geo_cats$age_10_to_17 / 
  acs_geo_cats$pop_tot_B01001_001

acs_geo_cats$age_15_to_64_percent <- 
  acs_geo_cats$age_15_to_64 / 
  acs_geo_cats$pop_tot_B01001_001 

acs_geo_cats$age_18_to_64_percent <- 
  acs_geo_cats$age_18_to_64 / 
  
  acs_geo_cats$pop_tot_B01001_001

acs_geo_cats$age_20_to_64_percent <- 
  acs_geo_cats$age_20_to_64 / 
  acs_geo_cats$pop_tot_B01001_001

acs_geo_cats$age_65_plus_percent <- 
  acs_geo_cats$age_65_plus / 
  acs_geo_cats$pop_tot_B01001_001

# unemployment rate
acs_geo_cats$employment_tot_16_over_B23025_001 <- 
  acs_geo_cats$B23025_001
acs_geo_cats$labor_force_tot_B23025_002 <- 
  acs_geo_cats$B23025_002 
acs_geo_cats$civie_labor_force_B23025_003 <- 
  acs_geo_cats$B23025_003 
acs_geo_cats$civie_employed_B23025_004 <- 
  acs_geo_cats$B23025_004
acs_geo_cats$civie_unemployed_B23025_005 <- 
  acs_geo_cats$B23025_005
acs_geo_cats$armed_forces_B23025_006 <- 
  acs_geo_cats$B23025_006

acs_geo_cats$unemployed_percent_B23025_005_003 <- 
  acs_geo_cats$B23025_005/
  acs_geo_cats$B23025_003

# median household value
acs_geo_cats$house_value_median_B25077_001 <- 
  acs_geo_cats$B25077_001

# percent white
acs_geo_cats$white_non_hisp_lat_percent_B03002_003_001 <- 
  acs_geo_cats$B03002_003/
  acs_geo_cats$B03002_001

# percent Native (includes Hispanic/Latinx)
acs_geo_cats$aian_percent_B03002_005_015_001 <- 
  (acs_geo_cats$B03002_005 + 
     acs_geo_cats$B03002_015) /
  acs_geo_cats$B03002_001

# percent of population with high school education or more
acs_geo_cats$education_high_school_plus_percent_B15003_017_to_025 <- 
  (
    (
      acs_geo_cats$B15003_017 + 
        acs_geo_cats$B15003_018 + 
        acs_geo_cats$B15003_019 + 
        acs_geo_cats$B15003_020 + 
        acs_geo_cats$B15003_021 + 
        acs_geo_cats$B15003_022 + 
        acs_geo_cats$B15003_023 + 
        acs_geo_cats$B15003_024 + 
        acs_geo_cats$B15003_025
    )
    /acs_geo_cats$B15003_001
  )

# linguistic isolation
acs_geo_cats$ling_iso_total_households_C16002_001 <-
  acs_geo_cats$C16002_001
acs_geo_cats <- 
  acs_geo_cats %>% 
  mutate(
    limited_english_percent_C16002_004_007_010_013_001 <- 
      (C16002_004 + 
         C16002_007 + 
         C16002_010 + 
         C16002_013) / 
      C16002_001)

# housing tenure
acs_geo_cats$housing_occupied_total_B25003_001 <- 
  acs_geo_cats$B25003_001
acs_geo_cats$housing_owner_occupied_B25003_002 <-
  acs_geo_cats$B25003_002
acs_geo_cats$housing_renter_occupied_B25003_003 <-
  acs_geo_cats$B25003_003
acs_geo_cats$owner_occupy_percent_B25003_002_001 <-
  acs_geo_cats$B25003_002 / 
  acs_geo_cats$B25003_001
acs_geo_cats$renter_occupy_percent_B25003_003_001 <-
  acs_geo_cats$B25003_003 / 
  acs_geo_cats$B25003_001

# percent below poverty
acs_geo_cats$pop_for_pov_C17002_001 <- 
  acs_geo_cats$C17002_001
acs_geo_cats$number_below_0p5_pov_C17002_002 <- 
  acs_geo_cats$C17002_002
acs_geo_cats$number_0p5_to_0p99_pov_C17002_003 <- 
  acs_geo_cats$C17002_003
acs_geo_cats$number_1p0_to_1p24_pov_C17002_004 <- 
  acs_geo_cats$C17002_004
acs_geo_cats$number_1p25_to_1p49_pov_C17002_005 <- 
  acs_geo_cats$C17002_005
acs_geo_cats$number_1p5_to_1p84_pov_C17002_006 <- 
  acs_geo_cats$C17002_006
acs_geo_cats$number_1p85_to_1p99_pov_C17002_007 <- 
  acs_geo_cats$C17002_007
acs_geo_cats$number_2p0_plus_pov_C17002_008 <- 
  acs_geo_cats$C17002_008

acs_geo_cats$poverty_below_100_percent_C17002_002_003_001 <- 
  (acs_geo_cats$C17002_002 + 
     acs_geo_cats$C17002_003) / 
  acs_geo_cats$pop_for_pov_C17002_001

acs_geo_cats$poverty_below_150_percent_C17002_002_to_005_001 <- 
  (acs_geo_cats$C17002_002 + 
     acs_geo_cats$C17002_003 + 
     acs_geo_cats$C17002_004 + 
     acs_geo_cats$C17002_005) / 
  acs_geo_cats$pop_for_pov_C17002_001

acs_geo_cats$poverty_below_200_percent__C17002_002_to_007_001 <- 
  (acs_geo_cats$C17002_002 + 
     acs_geo_cats$C17002_003 + 
     acs_geo_cats$C17002_004 + 
     acs_geo_cats$C17002_005 + 
     acs_geo_cats$C17002_006 + 
     acs_geo_cats$C17002_007) / 
  acs_geo_cats$pop_for_pov_C17002_001

# percent female
acs_geo_cats$female_number_B01001_026 <- 
  acs_geo_cats$B01001_026
acs_geo_cats$male_number_B01001_002 <-
  acs_geo_cats$B01001_002
acs_geo_cats$female_percent <- 
  acs_geo_cats$female_number_B01001_026 /
  acs_geo_cats$pop_tot_B01001_001

# percent health insurance
acs_geo_cats$pop_for_health_insurance_B27010_001 <- 
  acs_geo_cats$B27010_001
acs_geo_cats$no_health_insurance_percent_B27010_017_033_050_066 <- 
  (acs_geo_cats$B27010_017 + 
     acs_geo_cats$B27010_033 + 
     acs_geo_cats$B27010_050 + 
     acs_geo_cats$B27010_066) / 
  acs_geo_cats$pop_for_health_insurance_B27010_001

# earnings median
acs_geo_cats$earnings_median_B20002_001 <- 
  acs_geo_cats$B20002_001

# earnings categories
acs_geo_cats$earnings_1_to_2499_003_0024 <- 
  acs_geo_cats$B20001_003 + acs_geo_cats$B20001_024
acs_geo_cats$earnings_2500_to_4999_004_025 <- 
  acs_geo_cats$B20001_004 + acs_geo_cats$B20001_025
acs_geo_cats$earnings_5000_to_7499_005_026 <- 
  acs_geo_cats$B20001_005 + acs_geo_cats$B20001_026
acs_geo_cats$earnings_7500_to_9999_006_027 <- 
  acs_geo_cats$B20001_006 + acs_geo_cats$B20001_027
acs_geo_cats$earnings_10K_to_12499_007_028 <- 
  acs_geo_cats$B20001_007 + acs_geo_cats$B20001_028
acs_geo_cats$earnings_12500_to_14999_008_029 <- 
  acs_geo_cats$B20001_008 + acs_geo_cats$B20001_029
acs_geo_cats$earnings_15K_to_17499_009_030 <- 
  acs_geo_cats$B20001_009 + acs_geo_cats$B20001_030
acs_geo_cats$earnings_17500_to_19999_010_031 <- 
  acs_geo_cats$B20001_010 + acs_geo_cats$B20001_031
acs_geo_cats$earnings_20K_to_22499_011_032 <- 
  acs_geo_cats$B20001_011 + acs_geo_cats$B20001_032
acs_geo_cats$earnings_22500_to_24999_012_033 <- 
  acs_geo_cats$B20001_012 + acs_geo_cats$B20001_033
acs_geo_cats$earnings_25K_to_29999_013_034 <- 
  acs_geo_cats$B20001_013 + acs_geo_cats$B20001_034
acs_geo_cats$earnings_30K_to_34999_014_035 <- 
  acs_geo_cats$B20001_014 + acs_geo_cats$B20001_035
acs_geo_cats$earnings_35K_to_39999_015_036 <- 
  acs_geo_cats$B20001_015 + acs_geo_cats$B20001_036
acs_geo_cats$earnings_40K_to_44999_016_037 <- 
  acs_geo_cats$B20001_016 + acs_geo_cats$B20001_037
acs_geo_cats$earnings_45K_to_49999_017_038 <- 
  acs_geo_cats$B20001_017 + acs_geo_cats$B20001_038
acs_geo_cats$earnings_50K_to_54999_018_039 <- 
  acs_geo_cats$B20001_018 + acs_geo_cats$B20001_039
acs_geo_cats$earnings_55K_to_64999_019_040 <- 
  acs_geo_cats$B20001_019 + acs_geo_cats$B20001_040
acs_geo_cats$earnings_65K_to_74999_020_041 <- 
  acs_geo_cats$B20001_020 + acs_geo_cats$B20001_041
acs_geo_cats$earnings_75K_to_99999_021_042 <- 
  acs_geo_cats$B20001_021 + acs_geo_cats$B20001_042
acs_geo_cats$earnings_100K_to_infinity_022_043 <- 
  acs_geo_cats$B20001_022 + acs_geo_cats$B20001_043

#### RUNS TO HERE 2018-12-26

# save constructed variables
save(acs_geo_cats, 
     file = "acs_geo_cats.rdata")

# make constructed variables into .csv
write.csv(acs_geo_cats, 
          file = "acs_geo_cats.csv")

# # load constructed variables to avoid the above
# load(file = "acs_geo_cats.rdata")
# # View(acs_geo_cats)



# what data to isolate?
# active versus inactive
# any
# spudded after 2010
# post 2010 and 2000 cutoff years



#### well data ####
ks_clean <- 
  ks_wells_2018_11_01 # make well data file for cleaning



#### formatting changes in main well data ####

# convert dates to dates
ks_clean$permit_as_date  <-  
  as.Date(ks_clean$PERMIT, "%d-%b-%Y") # permit date
ks_clean$spud_as_date  <-  
  as.Date(ks_clean$SPUD, "%d-%b-%Y") # spud date
ks_clean$completion_as_date  <-  
  as.Date(ks_clean$COMPLETION, "%d-%b-%Y") # completion date
ks_clean$plugging_as_date  <-  
  as.Date(ks_clean$PLUGGING, "%d-%b-%Y") # plugging date
ks_clean$modified_as_date  <-  
  as.Date(ks_clean$MODIFIED, "%d-%b-%Y") # modified date
ks_clean$API_NUMBER  <-  
  as.character(ks_clean$API_NUMBER) # make API into a character



#### add analysis categories ####

# categorizing the well as SWD ('swd'), INJ ('inj'), or Class I ('ci')
ks_clean$well_type <- NA

# categorizing the well as in the uic database ('yes') or no ('no')
ks_clean$uic <- NA

# categorizing the well as active ('active'), inactive ('inactive'), 
# drilled ('drill'), future ('future'), never drilled or used as a 
# saltwater disposal well ('ab_loc'), or canceled midway through drilling
# or conversion or hole lost ('midway')
ks_clean$activity <- NA

# categorizing the well as plugged ('has_plug_status') 
# or unknown ('no_plug_status') by STATUS or STATUS2
ks_clean$plug_status <- NA

# categorizing the well as plugged or not by plugging_as_date
ks_clean$plug_date_binary <- NA

# categorizing as plugged or not by the above two categories
ks_clean$plug_overall <- NA

# classifying as having or not having an api ('yes' or 'no')
ks_clean$has_api <- NA

# whether the well was classified via either
# (1) a STATUS of "SWD" or "SWD-P&A" ('status');
# (2) a status2 of "Converted to SWD Well" ('status2'); or
# (3) manual comment review ('comments')
ks_clean$assignment_source <- NA

# classifying the detailed well type (see data documentation for possible values)
ks_clean$detailed_well_type <- NA

# raw notes on manual well assignments
ks_clean$assignment_notes <- NA

# whether the comments of the intial well have already been reviewed ('yes', 'NA')
ks_clean$comments_examined <- NA

# whether I reviewed documents from the Kansas Geological Society ('yes', 'no')
ks_clean$kgs_available_documents_verified <- NA

save(ks_clean, 
     file = "ks_clean.rdata") # save results of all these conversions



##### counting duplicates #####

# count unique APIs
number_of_unique_APIs <- length(unique(ks_clean$API_NUMBER))
save(number_of_unique_APIs, file = "number_of_unique_APIs.rdata")

# raw well count
raw_well_count <- nrow(ks_clean)
save(raw_well_count, file = "raw_well_count.rdata")

# overall counts by API
ks_by_API_count <- table(ks_clean$API_NUMBER)
save(ks_by_API_count, file = "ks_by_API_count.rdata")

# view rows with duplicated APIs
index <- 
  duplicated(ks_clean$API_NUMBER) | duplicated(ks_clean$API_NUMBER, 
                                               fromLast = TRUE)
ks_api_dups_all_wells <- ks_clean[index,]
save(ks_api_dups_all_wells, file = "ks_api_dups_all_wells.rdata")

# view counts of duplicates per API
ks_api_dup_counts <- table(ks_api_dups_all_wells$API_NUMBER)
save(ks_api_dup_counts, file = "ks_dup_API_count.rdata")

# count unique KIDs
unique_KID_count <- length(unique(ks_clean$KID))
save(unique_KID_count, file = "unique_KID_count.rdata")

# counts by well status for rows without APIs
ks_wells_no_API <- 
  ks_clean[which(ks_clean$API_NUMBER == ""),] # isolate rows for wells sans APIs
save(ks_wells_no_API, 
     file = "ks_wells_no_API.rdata") # save the above
ks_wells_no_API_by_status <- 
  table(ks_wells_no_API$STATUS) # table of counts of wells sans API by status
save(ks_wells_no_API_by_status, 
     file = "ks_wells_no_API_by_status.rdata") # save table of counts



##### checking ambiguous wells for inclusion or exclusion #####

# get counts by well status1 (main status)
ks_well_counts_by_status1 <- 
  table(ks_clean$STATUS) # get counts
ks_well_counts_by_status1 <- 
  as.data.frame(ks_well_counts_by_status1) # convert to dataframe
save(ks_well_counts_by_status1, 
     file = "ks_well_counts_by_status1.rdata")
write.csv(ks_well_counts_by_status1, 
          file = "ks_well_counts_by_status1.csv") # write for excel file

# get counts by well status2
ks_well_counts_by_status2 <- 
  table(ks_clean$STATUS2)
ks_well_counts_by_status2 <- 
  as.data.frame(ks_well_counts_by_status2) # convert to dataframe
save(ks_well_counts_by_status2, 
     file = "ks_well_counts_by_status2.rdata")
write.csv(ks_well_counts_by_status2, 
          file = "ks_well_counts_by_status2.csv")

# make vectors of status1s and status2s
ks_all_status1s <- 
  sort(unique(ks_clean$STATUS)) # vector of all STATUS values
ks_all_status2s <- 
  sort(unique(ks_clean$STATUS2)) # vector of all STATUS2 values
save(ks_all_status1s, 
     file = "ks_all_status1s.rdata")
save(ks_all_status2s, # save status1s to file
     file = "ks_all_status2s.rdata") # save status2s to file
write.csv(ks_all_status1s, 
          file = "ks_all_status1s.csv") # csv of status1s for excel
write.csv(ks_all_status2s, 
          file = "ks_all_status2s.csv") # csv of status2s for excel

# make vectors of status1s for further investigation or not
ks_status1_check_further <- 
  sort(c("INTENT",
         "OTHER()",
         "OTHER(NULL)",
         "OTHER(OTHER)",
         "OTHER(TA)",
         "OTHER(TEMP ABD)",
         "OTHER-P&A()",
         "OTHER-P&A(TA)"))

ks_potential_disposal_include_status1s <- 
  sort(c("OTHER()",
         "OTHER(1O&1SWD)",
         "OTHER(CBM/SWD)",
         "OTHER(CLASS ONE (OLD))",
         "OTHER(CLASS1)",
         "OTHER(NHDW)",
         "OTHER(NULL)",
         "OTHER(OIL,SWD)",
         "OTHER(OTHER)",
         "OTHER(SWD-P&A)",
         "OTHER(TA)",
         "OTHER(TEMP ABD)",
         "OTHER-P&A()",
         "OTHER-P&A(CLASS ONE (OLD))",
         "OTHER-P&A(OIL-SWD)",
         "OTHER-P&A(TA)",
         "SWD",
         "SWD-P&A"))

ks_definitely_include_status1s <- 
  sort(c("OTHER(1O&1SWD)",
         "OTHER(CBM/SWD)",
         "OTHER(CLASS ONE (OLD))",
         "OTHER(CLASS1)",
         "OTHER(NHDW)",
         "OTHER(OIL,SWD)",
         "OTHER(SWD-P&A)",
         "OTHER-P&A(CLASS ONE (OLD))",
         "OTHER-P&A(OIL-SWD)",
         "SWD",
         "SWD-P&A"))

ks_potential_disposal_exclude_status1s <- 
  sort(c("INTENT",
         "INJ",
         "INJ-P&A",
         "OTHER(INJ or EOR)",
         "OTHER-P&A(INJ OR )",
         "OTHER-P&A(INJ or EOR)",
         "CBM",
         "CBM-P&A",
         "D&A",
         "EOR",
         "EOR-P&A",
         "GAS",
         "GAS-P&A",
         "LOC",
         "O&G",
         "O&G-P&A",
         "OIL",
         "OIL-P&A",
         "OTHER-P&A(2 OIL)",
         "OTHER-P&A(CATH)",
         "OTHER-P&A(COREHOLE)",
         "OTHER-P&A(GAS-INJ)",
         "OTHER-P&A(GAS-STG)",
         "OTHER-P&A(GSW)",
         "OTHER-P&A(LH)",
         "OTHER-P&A(OBS)",
         "OTHER-P&A(OIL&GAS-INJ)",
         "OTHER-P&A(SHUT-IN)",
         "OTHER-P&A(STRAT)",
         "OTHER-P&A(WATER)",
         "OTHER(2OIL)",
         "OTHER(ABD LOC)",
         "OTHER(CATH)",
         "OTHER(COREHOLE)",
         "OTHER(GAS-INJ)",
         "OTHER(GAS-STG)",
         "OTHER(GAS INJ)",
         "OTHER(GAS SHUT-IN)",
         "OTHER(GSW)",
         "OTHER(HELIUM)",
         "OTHER(LH)",
         "OTHER(Monitor)",
         "OTHER(MONITOR)",
         "OTHER(OBS)",
         "OTHER(OBSERVATION)",
         "OTHER(OIL&GAS-INJ)",
         "OTHER(Oil)",
         "OTHER(OIL/GAS)",
         "OTHER(SHUT-IN)",
         "OTHER(STRAT)",
         "OTHER(WATER)"))

# check lengths of vectors
length(ks_potential_disposal_include_status1s) # count included statii
length(ks_potential_disposal_exclude_status1s) # count excluded statii
length(ks_all_status1s) # above two numbers should sum to this number



#### selecting rows based on status2 ####
# make vector of status2s to include regardless of status1
ks_potential_disposal_include_status2s <- 
  sort(c("Converted to SWD Well")) 

# vector of status2s that don't guarantee inclusion
ks_potential_disposal_exclude_status2s <- 
  setdiff(ks_all_status2s,
          ks_potential_disposal_include_status2s)
ks_potential_disposal_exclude_status2s



#### investigating rows based on comments ####
ks_potential_disposal_comments <-   # make vector of just comments
  ks_clean$COMMENTS

# below identifies rows with comments containing the strings 
# "swd", "disp", "salt", "class", or "waste"
positions_of_possible_comments_to_include <-
  grep("swd|disp|salt|class|waste", 
       ks_potential_disposal_comments, 
       ignore.case = TRUE)

comments_to_review <-   # makes vector of identified comments
  ks_clean$COMMENTS[positions_of_possible_comments_to_include]

# makes .csv file of identified comments
write.csv(comments_to_review,   # makes .csv file of identified comments
          file = "comments_to_review.csv")

# makes dataframe of entire rows matching comments
rows_requiring_comment_investigation <-
  ks_clean[which(ks_clean$COMMENTS %in% comments_to_review),]

# make dataframe of most relevant columns of entire rows matching comments
rows_requiring_comment_investigation_simple <- 
  rows_requiring_comment_investigation[
    ,c("KID",
       "API_NUMBER",
       "activity",
       "well_type",
       "assignment_source",
       "detailed_well_type",
       "assignment_notes",
       "comments_examined",
       "kgs_available_documents_verified",
       "STATUS",
       "STATUS2",
       "COMMENTS"
    )
    ]

# make .csv files of rows matching comments and simple rows matching comments
write.csv(rows_requiring_comment_investigation,
          file = "rows_requiring_comment_investigation.csv")
write.csv(rows_requiring_comment_investigation_simple, 
          file = "rows_requiring_comment_investigation_simple.csv")



#### manual well assignment space ####
# cat("Please assign the ambiguous wells manually in an Excel file.  Afterward, find the window that just opened and click on the 'continue' button to continue the program.  The next prompt asks you to choose the file with the well assignments.")
# my_wait() # runs function to force the program to wait for input

# # import manual assignments
# manual_well_assignment_csv_file <- # asks user to choose the correct .csv file
#   file.choose()
# 
# raw_manual_well_assignments_dataframe <- # converts above .csv to a dataframe
#   read.csv(manual_well_assignment_csv_file)

raw_manual_well_assignments_dataframe <- # converts above .csv to a dataframe
  read.csv(
    file = 
      "rows_requiring_comment_investigation_simple_2018_12_26_back_to_r.csv")

# View(raw_manual_well_assignments_dataframe) # view the import



#### creating the semi-final well list #### 
# This section combines all the manually assigned wells as well as the wells
# pulled due solely due to their STATUS or STATUS2.

# pull well KIDs identified manually or by status1 or status2
kids_from_manual_assignments <-   # KIDs of manually added wells
  ks_clean$KID[which(ks_clean$KID %in% 
                       raw_manual_well_assignments_dataframe$KID)]

kids_status1s <-   # KIDs of wells IDed via status1
  ks_clean$KID[which(ks_clean$STATUS %in% 
                       ks_definitely_include_status1s)]

kids_status2s <-   # KIDs of wells IDed via status2
  ks_clean$KID[which(ks_clean$STATUS2 %in% 
                       ks_potential_disposal_include_status2s)]

# length(kids_from_manual_assignments) # count of manual KIDs
# length(kids_status1s)  # count of status1 KIDs
# length(kids_status2s)  # count of status2 KIDs

# combine the lists
kids_semi_final_list <- c(kids_from_manual_assignments, 
                          kids_status1s,
                          kids_status2s)

# convert the list of three vectors into just one vector
kids_semi_final_list <- 
  unlist(kids_semi_final_list)

kids_semi_final_list <-   # delete duplicates
  unique(kids_semi_final_list)   

ks_semi_final_wells <-   # make dataframe of data from selected wells
  ks_clean[which(ks_clean$KID %in% 
                   kids_semi_final_list),]

# View(ks_semi_final_wells)

# save data to disk
save(ks_semi_final_wells, file = "ks_semi_final_wells.rdata")
write.csv(ks_semi_final_wells, file = "ks_semi_final_wells.csv")



#### create working semi-final dataset ####

ks_semi_final_wells_working <-   # make safe working dataset
  ks_semi_final_wells

# status1s

ks_semi_final_status1s <-   # make vector of status1s in semi-final dataset
  sort(unique(ks_semi_final_wells$STATUS))

# View(ks_semi_final_status1s)   # view vector of status1s

# View(table(ks_semi_final_wells_working$STATUS))   # view table of status1s

write.csv(ks_semi_final_status1s,   # write .csv of status1s
          file = "ks_semi_final_status1s.csv")


# status2s

ks_semi_final_status2s <-   # make vector of status2s in semi-final dataset
  sort(unique(ks_semi_final_wells$STATUS2))

# View(ks_semi_final_status2s)   # view vector of status2s

# View(table(ks_semi_final_wells_working$STATUS2))   # view table of status2s

write.csv(ks_semi_final_status2s,   # write .csv of status2s
          file = "ks_semi_final_status2s.csv")



#### CATEGORY ASSIGNMENTS IN SEMI-FINAL DATASET ####

#### assign has api ####
# assign it
ks_semi_final_wells_working$has_api <-
  ifelse(ks_semi_final_wells_working$API_NUMBER == "", "no", "yes")

# View(ks_semi_final_wells_working)



#### assign well_type ####

# View(table(ks_semi_final_wells_working$STATUS)) view possible status1s

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

swd_status1s <-   # make vector of status1s that mean swd well
  sort(c("OTHER(1O&1SWD)",
         "OTHER(CBM/SWD)",
         "OTHER(OIL,SWD)",
         "OTHER(SWD-P&A)",
         "OTHER-P&A(OIL-SWD)",
         "SWD",
         "SWD-P&A"))

class1_status1s <-   # make vector of status1s that mean class1 well
  sort(c("OTHER(CLASS ONE (OLD))",
         "OTHER(CLASS1)",
         "OTHER(NHDW)",
         "OTHER-P&A(CLASS ONE (OLD))"))

swd_status2s <-   # make vector of status2s that mean swd well
  sort(c("Converted to SWD Well"))

ks_semi_final_wells_working <-   # assign class1
  within(ks_semi_final_wells_working, 
         well_type[STATUS %in% class1_status1s] <- 'class1')

ks_semi_final_wells_working <-   # assign swd
  within(ks_semi_final_wells_working, 
         well_type[STATUS %in% swd_status1s | 
                     STATUS2 %in% swd_status2s] <- 'swd')



#### assign activity ####

# assign inactive statuses
inactive_status1s <-   # make vector of pa status1s
  sort(c("EOR-P&A",
         "GAS-P&A",
         "INJ-P&A",
         "O&G-P&A",
         "OIL-P&A",
         "OTHER-P&A(CLASS ONE (OLD))",
         "OTHER-P&A(LH)",
         "OTHER-P&A(OIL-SWD)",
         "OTHER-P&A(OIL&GAS-INJ)",
         "OTHER-P&A(STRAT)",
         "OTHER-P&A(TA)",
         "OTHER-P&A(WATER)",
         "OTHER(SWD-P&A)",
         "SWD-P&A",
         "D&A",
         "OTHER(LH)",
         "OTHER(TA)"))

inactive_status2s <-   # make vector of pa status2s
  sort(c("Approved for Plugging - CP-1 Received",
         "Expired Plugging Application (CP-1)",
         "Inactive Well",
         "Injection Authorization Terminated",
         "Injection Authorization Terminated - INACTIVE CODE",
         "KCC Fee Fund Plugging",
         "Plugged and Abandoned",
         "Re-Plugged (non Fee-Fund)",
         "Unplugged Former Injection Well"))

ks_semi_final_wells_working <-   # assign inactive wells
  within(ks_semi_final_wells_working, 
         activity[STATUS %in% inactive_status1s |
                    STATUS2 %in% inactive_status2s] <- 'inactive')

# assign future statuses
future_status2s <-   # make vector of future status2s
  sort(c("Approved Intent to Drill",
         "DEVELOPMENT",
         "ON LIST",
         "Pending Injection Application"))

ks_semi_final_wells_working <-   # assign inactive wells
  within(ks_semi_final_wells_working, 
         activity[STATUS %in% future_status2s] <- 'future')

# assign cancel statuses
cancel_status2s <-   # make vector of cancel status2s
  sort(c("Cancelled API Number",
         "Expired Intent to Drill (C-1)",
         "UIC Application Denied",
         "UIC Application Dismissed",
         "UIC Application Withdrawn"))

ks_semi_final_wells_working <-   # assign cancel wells
  within(ks_semi_final_wells_working, 
         activity[STATUS %in% cancel_status2s] <- 'ab_loc')

# assign drill statuses
drill_status2s <-   # make vector of drill status2s
  sort(c("Spudded",
         "Well Drilled"))

ks_semi_final_wells_working <-   # assign drill wells
  within(ks_semi_final_wells_working, 
         activity[STATUS %in% drill_status2s] <- 'drill')



#### assign plug ####

# assign plug_status
plug_status1s <-   # make vector of plug status1s
  sort(c("EOR-P&A",
         "GAS-P&A",
         "INJ-P&A",
         "O&G-P&A",
         "OIL-P&A",
         "OTHER-P&A(CLASS ONE (OLD))",
         "OTHER-P&A(LH)",
         "OTHER-P&A(OIL-SWD)",
         "OTHER-P&A(OIL&GAS-INJ)",
         "OTHER-P&A(STRAT)",
         "OTHER-P&A(TA)",
         "OTHER-P&A(WATER)",
         "OTHER(SWD-P&A)",
         "SWD-P&A"))

plug_status2s <-   # make vector of plug status2s
  sort(c("Plugged and Abandoned",
         "Re-Plugged (non Fee-Fund)"))

ks_semi_final_wells_working <-   # assign plugged by status
  within(ks_semi_final_wells_working,
         plug_status[STATUS %in% plug_status1s | 
                       STATUS2 %in% plug_status2s] <-
           'has_plug_status')

ks_semi_final_wells_working <-   # assign rest to not status
  within(ks_semi_final_wells_working,
         plug_status[is.na(plug_status)] <-
           'no_plug_status')

# assign plug_date
ks_semi_final_wells_working <-   # assign plugged by date
  within(ks_semi_final_wells_working, 
         plug_date_binary[!is.na(plugging_as_date)] <-
           'has_plug_date')

ks_semi_final_wells_working <-   # assign rest to no plug date
  within(ks_semi_final_wells_working,
         plug_date_binary[is.na(plug_date_binary)] <-
           'no_plug_date')

# assign overall plugged
ks_semi_final_wells_working <-   # assign overall plug
  within(ks_semi_final_wells_working,
         plug_overall[plug_status == 
                        "has_plug_status" | 
                        plug_date_binary == "has_plug_date"] <-
           'plugged')

ks_semi_final_wells_working <-   # assign overall plug
  within(ks_semi_final_wells_working,
         plug_overall[is.na(plug_overall)] <-
           'not_plugged')



#### assign UIC status ####

uic_kids <-   # pull KIDs of uic wells in UIC database
  unique(ks_uic_2018_09_04$KGS_ID)

# assign wells as in or not in the uic
ks_semi_final_wells_working$uic <-   
  ifelse(ks_semi_final_wells_working$KID %in% uic_kids,
         'in_uic',
         'not_in_uic')

# View(ks_uic)
# kids<-unique(ks_uic$KGS_ID)
# kids
#
# ks_wells_in_uic_data_only <- subset(ks_clean, KID %in% kids)
# View(ks_wells_in_uic_data_only)
# uic_statuses<-table(ks_wells_in_uic_data_only$STATUS)
# View(uic_statuses)
# write.csv(uic_statuses,file="uic_statuses.csv")



#### deal with comments ####

# isolate those wells from the comments that will actually help
ks_manual_wells_useful_columns <- 
  raw_manual_well_assignments_dataframe[
    ,c("KID", 
       "man_activity", 
       "man_plug_status", 
       "man_well_type",	
       "man_assignment_source",	
       "man_detailed_well_type",	
       "man_assignment_notes", 
       "man_comments_examined",	
       "man_kgs_available_documents_verified"
       )
  ]




ks_semi_final_wells_working <- # merge manual assignments with large dataset
  merge(ks_semi_final_wells_working, 
        ks_manual_wells_useful_columns, 
        by = "KID", 
        all.x = TRUE, 
        all.y = TRUE)

#### CHECK INJECTION AUTHORIZATION TERMINATED!!!

ks_semi_final_wells_working$is_swd <-   # make variable for is/is not swd
  NA 

ks_semi_final_wells_working <-   # identify manually assigned swd wells
  within(ks_semi_final_wells_working, 
         is_swd[well_type == 'swd'] <- 'swd')

ks_semi_final_wells_working <-   # identify manually assigned swd wells
  within(ks_semi_final_wells_working, 
         is_swd[man_well_type 
                %in% 
                  c('swd', 
                    'prob_swd', 
                    'prob_swd_prob_class1', 
                    'prob_swd_may_class1', 
                    'prob_c1_to_prob_swd', 
                    'def_c1_to_prob_swd', 
                    'prob_swd_to_prob_c1', 
                    'may_c1_to_prob_swd'
                    )
                ] <- 
           'swd'
         )

# drop non-swd wells
ks_swd_only <- 
  subset(ks_semi_final_wells_working, 
         is_swd == 'swd')

# assign manual activity statuses
ks_swd_only$man_activity <-   # force man_activity to character
  as.character(ks_swd_only$man_activity)

ks_swd_only$activity <-   # apply manual activity assignments
  with(ks_swd_only, 
       ifelse(!is.na(man_activity), man_activity, activity))

ks_swd_only$activity <-   # make NAs active
  with(ks_swd_only, 
       ifelse(is.na(activity), 'active', activity))

ks_swd_only$activity <-   # make unknowns active
  with(ks_swd_only, 
       ifelse(activity == 'unknown', 'active', activity))

#### handling duplicates ####

#### deleting those with duplicate APIs ####

#### start here 2018-12-27 ####

# make dataset without wells without apis
ks_only_apis <- 
  ks_swd_only[!(is.na(ks_swd_only$API_NUMBER) | 
                  ks_swd_only$API_NUMBER==""),]

# find duplicate APIs
ks_final_index  <-   # make index of API duplicates
  duplicated(ks_only_apis$API_NUMBER) | 
  duplicated(ks_only_apis$API_NUMBER, 
             fromLast = TRUE)

ks_api_swd_dups <-   # make dataframe of duplicates
  ks_only_apis[ks_final_index, ]
View(ks_api_swd_dups)

save(ks_api_swd_dups, 
     file = "ks_api_swd_dups.rdata")

write.csv(ks_api_swd_dups, 
          file = "ks_api_swd_dups.csv")

#### review duplicate APIs manually here ####

kids_duplicate_apis_to_keep <- 
  c("",   # reason
    "", 
    "",)

# remove duplicate APIs, keeping those last modified
ks_bye_dup_API <- 
  subset(ks_only_apis, 
         ave(modified_as_date, API_NUMBER, FUN = max) == modified_as_date)
# View(ks_bye_dup_API)


# find duplicated latitudes and longitudes
ks_lat_long_dup_index <- 
  duplicated(ks_bye_dup_API[c("LATITUDE","LONGITUDE")]) | 
  duplicated(ks_bye_dup_API[c("LATITUDE","LONGITUDE")], fromLast = TRUE)
ks_lat_long_dup <- ks_bye_dup_API[ks_lat_long_dup_index, ]
# View(ks_lat_long_dup)

# fix statuses DO THIS EARLIER ON CODE CLEANUP
ks_bye_dup_API$STATUS <- 
  ks_bye_dup_API$STATUS.x

ks_bye_dup_API$STATUS2 <-
  ks_bye_dup_API$STATUS.x

# order rows in a hierarchy by which ones I think are true # fix later
order_of_STATUSes <- 
  c("SWD", 
    "SWD-P&A", 
    "OTHER(OIL,SWD)", 
    "OTHER(CBM/SWD)", 
    "OTHER(1O&1SWD)", 
    "OTHER-P&A(OIL-SWD)", 
    "OTHER(SWD-P&A)", 
    "OTHER-P&A(LH)", 
    "OTHER(LH)", 
    "LOC", 
    "GAS-P&A", 
    "OIL-P&A", 
    "GAS", 
    "OIL", 
    "D&A", 
    "O&G", 
    "OTHER(CLASS1)", 
    "OTHER(TA)", 
    "O&G-P&A", 
    "EOR", 
    "INJ-P&A", 
    "INJ", 
    "OTHER-P&A(WATER)", 
    "CBM", 
    "OTHER-P&A(OIL&GAS-INJ)", 
    "OTHER(STRAT)", 
    "OTHER()", 
    "OTHER(OIL&GAS-INJ)",
    "OTHER(WATER)", 
    "OTHER-P&A(TA)", 
    "OTHER(OBS)", 
    "OTHER(GAS-INJ)", 
    "EOR-P&A" 
  )

View(unique(ks_bye_dup_API$STATUS))

# order dataframe by statuses
ks_ordered <- 
  ks_bye_dup_API[order(factor(ks_bye_dup_API$STATUS, 
                              levels = order_of_STATUSes)),]

# View(ks_ordered)

# remove duplicates
# check whether I should even do this
ks_data_for_counting <- 
  ks_bye_dup_API[!duplicated(ks_ordered[,c("LATITUDE","LONGITUDE")]),]

# View(ks_data_for_counting)


# calling it the final thesis data
ks_final_thesis_data <- 
  ks_data_for_counting
save(ks_final_thesis_data, 
     file = "ks_final_thesis_data.rdata")
# View(ks_final_thesis_data)

# get number of wells by activity status
table(ks_final_thesis_data$well_type, 
      ks_final_thesis_data$activity)

# get number of wells by type (not useful for quick and dirty)
table(ks_final_thesis_data$well_type)

ks_set_for_mapping <- 
  ks_final_thesis_data[,c("KID", 
                          "API_NUMBER", 
                          "LATITUDE", 
                          "LONGITUDE", 
                          "well_type")]
# View(ks_set_for_mapping)
write.csv2(ks_set_for_mapping, file = "ks_set_for_mapping.csv")

#### HERE PAUSE AND DO SPATIAL JOIN IN ARCGIS ####

# import mapping results back
ks_join_results <- read.csv(file = "well_join_2018_12_15.txt", 
                            stringsAsFactors = FALSE)

# View(ks_join_results)
save(ks_join_results, 
     file = "ks_join_results.rdata")

# # load needed files if starting from here
load(file = 
       "ks_final_thesis_data.rdata")
load(file = 
       "ks_join_results.rdata")


ks_join_to_edit <- ks_join_results

# remove columns not needed in the final analysis dataset
ks_join_to_edit$API_NUMBER <- NULL
ks_join_to_edit$LATITUDE <- NULL
ks_join_to_edit$LONGITUDE <- NULL
ks_join_to_edit$well_type <- NULL
ks_join_to_edit$Join_Count <- NULL
ks_join_to_edit$OBJECTID_1 <- NULL
ks_join_to_edit$TARGET_FID <- NULL

ks_wells_and_block_groups <- merge(x = ks_final_thesis_data, 
                                   y = ks_join_to_edit, by = "KID", all.x = TRUE)
View(ks_wells_and_block_groups)

ks_wells_and_block_groups$swd <- 1

ks_well_counts <- aggregate(
  ks_wells_and_block_groups$swd ~ GEOID_Data, 
  ks_wells_and_block_groups, 
  sum)
View(ks_well_counts)

# rename GEOID_Data to GEOID in well data
ks_well_counts$GEOID <- 
  ks_well_counts$GEOID_Data
save(ks_well_counts, 
     file = "ks_well_counts.rdata")
load(file = "ks_well_counts.rdata")

# load ACS variables
load(file = 
       "acs_geo_cats.rdata")

# merge well counts and acs!  hey-o!
KS_FINAL_DATASET <- merge(ks_well_counts, 
                          acs_geo_cats, 
                          by = "GEOID", 
                          all = TRUE)
View(KS_FINAL_DATASET)
save(KS_FINAL_DATASET, 
     file = "KS_FINAL_DATASET.rdata")
write.csv(KS_FINAL_DATASET, 
          file = "KS_FINAL_DATASET.csv")



#### Analyses! ####

# load files if starting from here
# load(file = "KS_FINAL_DATASET.rdata")
ks_analysis_dataset <- KS_FINAL_DATASET
# View(ks_analysis_dataset)



#### swd well block groups only ####

# make variable for sum of swd wells per block group
ks_analysis_dataset$any_swd_sum <- 
  ks_analysis_dataset[,c("ks_wells_and_block_groups$swd")]
str(ks_analysis_dataset$any_swd_sum)
ks_analysis_dataset <- 
  within(ks_analysis_dataset,any_swd_sum[is.na(ks_analysis_dataset$any_swd_sum)] 
         <- 0)
ks_analysis_dataset[,c("ks_wells_and_block_groups$swd")] <- NULL
# View(ks_analysis_dataset)

# make new binary variable for have versus not have swd
ks_analysis_dataset$any_swd_binary <- NA
ks_analysis_dataset <- within(ks_analysis_dataset, any_swd_binary[any_swd_sum == 0] <- 'no')
ks_analysis_dataset <- within(ks_analysis_dataset, any_swd_binary[any_swd_sum > 0] <- 'yes')
table(ks_analysis_dataset$any_swd_binary)

names(ks_analysis_dataset)

# drop 12 without people
ks_analysis_dataset <- ks_analysis_dataset %>%
  filter(population_density_B01001_ALAND > 0)

#### correlation matrix of ACS variables ####
names(ks_analysis_dataset) # get variable names

# make dataframe for the correlation dataset
correlation_matrix_data  <-  ks_analysis_dataset[,c("median_household_income_B19013","median_household_value_B25077","percent_white_B03002","population_density_B01001_ALAND","percent_high_school_plus_B15003","median_age_B01002","any_swd_sum","any_swd_binary","GEOID_simple")]

# create the correlation matrix
correlation_matrix  <-  round(cor(correlation_matrix_data[,1:6], use = "pairwise.complete.obs"),2) # rounds to 2 decimal places

# view and write to file
View(correlation_matrix)
write.csv(correlation_matrix, file = "correlation_matrix.csv")

# count observations going into the correlation matrix
counts_pairwise_correlations  <-  count.pairwise(correlation_matrix_data[,1:6], y = NULL,diagonal=TRUE)

# view and write to file
View(counts_pairwise_correlations)
write.csv(counts_pairwise_correlations, file = "counts_pairwise_correlations.csv")



#### make pretty heatmap ####
cormat <- correlation_matrix

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
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
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

print(ggheatmap)












#### checking normality ####

par(mfrow=c(2,2))

# income
density_hh_income <- 
  density(ks_analysis_dataset$median_household_income_B19013, 
          na.rm = TRUE)
plot(density_hh_income, main = "Median Household Income Density Plot")

density_household_value <- 
  density(ks_analysis_dataset$median_household_value_B25077, na.rm = TRUE)
plot(density_household_value, 
     main = "Median Household Value Density Plot")

density_percent_white <- 
  density(ks_analysis_dataset$percent_white_B03002, na.rm = TRUE)
plot(density_percent_white, 
     main = "Percent White Density Plot")

density_pop_density <- 
  density(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)
plot(density_pop_density, 
     main = "Population Density Density Plot")

density_percent_high_school <- 
  density(ks_analysis_dataset$percent_high_school_plus_B15003, na.rm = TRUE)
plot(density_percent_high_school, 
     main = "Percent Completed High School Density Plot")

density_median_age <- 
  density(ks_analysis_dataset$median_age_B01002, na.rm = TRUE)
plot(density_median_age, main = "Median Age Density Plot")

density_swd <- 
  density(ks_analysis_dataset$any_swd_sum, na.rm = TRUE)
plot(density_swd, 
     main = "Number of Disposal Wells per Block Group Density Plot")



#### analysis by presence and absence of swd ####
# number of block groups with and without swd wells
table(ks_analysis_dataset$any_swd_binary)



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
# 
# 
# 
# 
# write.csv(ks_analysis_dataset, file = "ks_analysis_dataset.csv")
# 
# #### old code removed that I might need later ####
# 
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
# 
# # # drop never-completeds
# # no_never_complete <- subset(true_no_dup_API, !(activity == "never_completed"))
# # View(no_never_complete)
# 
# 
