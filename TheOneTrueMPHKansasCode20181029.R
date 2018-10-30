# Oil and Gas Disposal Well Analysis for Kansas
# Katherine Wolf
# October 2018

# setup
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(psych)

getwd()

# functions
`%notin%`  <-  function(x,y) !(x %in% y) # define "not-in" function

################

#### import main well data ####

# # import well data
# ks_wells_2018_09_28 <- read.csv(file="ks_wells_2018_09_28.txt", stringsAsFactors = FALSE)
# View(ks_wells_2018_09_28)
# 
# # save file as an rdata file
# save(ks_wells_2018_09_28,file="ks_wells_2018_09_28.rdata")

# load rdata file
load(file="ks_wells_2018_09_28.rdata")

# make version for cleaning
ks_clean <- ks_wells_2018_09_28
# View(ks_clean)



#### import UIC data ####

# import UIC data
# ks_uic_2018_09_04 <- read.csv(file="KS_UIC_archive_2018_09_04.txt")
# save(ks_uic_2018_09_04,file="ks_uic_2018_09_04.rdata")
load(file="ks_uic_2018_09_04.rdata")


#### data analysis in UIC data ####

# isolate individual well IDs present in UIC data
kids <- unique(ks_uic_2018_09_04$KGS_ID)

# make table of KS wells in UIC data
ks_wells_in_uic_data_only  <-  subset(ks_clean, KID %in% kids)
save(ks_wells_in_uic_data_only,file="ks_wells_in_uic_data_only.rdata")

# make table of statuses in UIC data
uic_statuses <- table(ks_wells_in_uic_data_only$STATUS)
write.csv(uic_statuses,file="uic_statuses.csv")


#### formatting changes in main well data ####

# convert dates to dates
ks_clean$permit_as_date  <-  as.Date(ks_clean$PERMIT, "%d-%b-%Y") # permit date
ks_clean$spud_as_date  <-  as.Date(ks_clean$SPUD, "%d-%b-%Y") # spud date
ks_clean$completion_as_date  <-  as.Date(ks_clean$COMPLETION, "%d-%b-%Y") # completion date
ks_clean$plugging_as_date  <-  as.Date(ks_clean$PLUGGING, "%d-%b-%Y") # plugging date
ks_clean$modified_as_date  <-  as.Date(ks_clean$MODIFIED, "%d-%b-%Y") # modified date
ks_clean$API_NUMBER  <-  as.character(ks_clean$API_NUMBER) # make API into a character

# get possible values of well status
unique(ks_clean$STATUS)
unique(ks_clean$STATUS2)

# view and save results of all these conversions
# View(ks_clean)
save(ks_clean,file="ks_clean.rdata")



##### counting duplicates #####

# count unique APIs
number_of_unique_APIs <- length(unique(ks_clean$API_NUMBER))
number_of_unique_APIs
save(number_of_unique_APIs,file="number_of_unique_APIs.rdata")

# raw well count
raw_well_count <- nrow(ks_clean)
raw_well_count
save(raw_well_count,file="raw_well_count.rdata")

# overall counts by API
ks_by_API_count <- table(ks_clean$API_NUMBER)
save(ks_by_API_count,file="ks_by_API_count.rdata")

# view rows with duplicated APIs
index <- duplicated(ks_clean$API_NUMBER) | duplicated(ks_clean$API_NUMBER, fromLast = TRUE)
ks_API_dups <- ks_clean[index,]
save(ks_API_dups,file="ks_API_dups.rdata")

# view counts of duplicates per API
ks_API_dup_counts <- table(ks_API_dups$API_NUMBER)
save(ks_API_dup_counts,file="ks_dup_API_count.rdata")

# count unique KIDs
unique_KID_count <- length(unique(ks_clean$KID))
unique_KID_count
save(unique_KID_count,file="unique_KID_count.rdata")

# counts by well status for rows without APIs
ks_wells_no_API <- ks_clean[which(ks_clean$API_NUMBER == ""),]
save(ks_wells_no_API,file="ks_wells_no_API.rdata")

ks_wells_no_API_by_status <- table(ks_wells_no_API$STATUS)
save(ks_wells_no_API_by_status,file="ks_wells_no_API_by_status.rdata")


##### checking ambiguous wells for inclusion or exclusion #####

# load(file="ks_clean.rdata")

# get counts by well status (main status)
ks_well_counts_by_status <- table(ks_clean$STATUS)
ks_well_counts_by_status <- as.data.frame(ks_well_counts_by_status) # convert to dataframe
save(ks_well_counts_by_status,file="ks_well_counts_by_status.rdata")
write.csv(ks_well_counts_by_status, file="ks_well_counts_by_status.csv")

# get counts by well status 2
ks_well_counts_by_status_2 <- table(ks_clean$STATUS2)
ks_well_counts_by_status_2 <- as.data.frame(ks_well_counts_by_status_2) # convert to dataframe
save(ks_well_counts_by_status_2,file="ks_well_counts_by_status_2.rdata")
write.csv(ks_well_counts_by_status_2, file="ks_well_counts_by_status_2.csv")

ks_all_status1s <- sort(unique(ks_clean$STATUS)) # vector of all STATUS values
ks_all_status2s <- sort(unique(ks_clean$STATUS2)) # vector of all STATUS2 values

ks_all_status2s

ks_status1_check_further <- c("INTENT","OTHER()","OTHER(NULL)","OTHER(OTHER)","OTHER(TA)","OTHER(TEMP ABD)","OTHER-P&A()","OTHER-P&A(TA)")

#### THIS SECTION SHALL ONLY INCLUDE SWD WELLS
ks_potential_disposal_include_status1s <- sort(c("OTHER()","OTHER(1O&1SWD)","OTHER(CBM/SWD)","OTHER(CLASS ONE (OLD))","OTHER(CLASS1)","OTHER(NHDW)","OTHER(NULL)","OTHER(OIL,SWD)","OTHER(OTHER)","OTHER(SWD-P&A)","OTHER(TA)","OTHER(TEMP ABD)","OTHER-P&A()","OTHER-P&A(CLASS ONE (OLD))","OTHER-P&A(OIL-SWD)","OTHER-P&A(TA)","SWD","SWD-P&A"))

ks_definitely_include_status1s <- sort(c("OTHER(1O&1SWD)","OTHER(CBM/SWD)","OTHER(CLASS ONE (OLD))","OTHER(CLASS1)","OTHER(NHDW)","OTHER(OIL,SWD)","OTHER(SWD-P&A)","OTHER-P&A(CLASS ONE (OLD))","OTHER-P&A(OIL-SWD)","SWD","SWD-P&A"))

ks_potential_disposal_exclude_status1s <- sort(c("INTENT","INJ","INJ-P&A","OTHER(INJ or EOR)","OTHER-P&A(INJ OR )","OTHER-P&A(INJ or EOR)","CBM","CBM-P&A","D&A","EOR","EOR-P&A","GAS","GAS-P&A","LOC","O&G","O&G-P&A","OIL","OIL-P&A","OTHER-P&A(2 OIL)","OTHER-P&A(CATH)","OTHER-P&A(COREHOLE)","OTHER-P&A(GAS-INJ)","OTHER-P&A(GAS-STG)","OTHER-P&A(GSW)","OTHER-P&A(LH)","OTHER-P&A(OBS)","OTHER-P&A(OIL&GAS-INJ)","OTHER-P&A(SHUT-IN)","OTHER-P&A(STRAT)","OTHER-P&A(WATER)","OTHER(2OIL)","OTHER(ABD LOC)","OTHER(CATH)","OTHER(COREHOLE)","OTHER(GAS-INJ)","OTHER(GAS-STG)","OTHER(GAS INJ)","OTHER(GAS SHUT-IN)","OTHER(GSW)","OTHER(HELIUM)","OTHER(LH)","OTHER(Monitor)","OTHER(MONITOR)","OTHER(OBS)","OTHER(OBSERVATION)","OTHER(OIL&GAS-INJ)","OTHER(Oil)","OTHER(OIL/GAS)","OTHER(SHUT-IN)","OTHER(STRAT)","OTHER(WATER)"))

length(ks_potential_disposal_include_status1s) # count included statii
length(ks_potential_disposal_exclude_status1s) # count excluded statii
length(ks_all_status1s)

ks_potential_disposal_include_status2s <- # status2s to include regardless of status1
  sort(c("Converted to SWD Well"))
ks_potential_disposal_exclude_status2s <- # status2s that don't mean inclusion
  setdiff(ks_all_status2s, 
          ks_potential_disposal_include_status2s)
ks_potential_disposal_exclude_status2s

# dealing with comments!
ks_potential_disposal_comments <- ks_clean$COMMENTS
positions_of_possible_comments_to_include <- grep("swd|disp|class",ks_potential_disposal_comments,ignore.case = TRUE)
positions_of_possible_comments_to_include
comments_to_review <- ks_clean$COMMENTS[positions_of_possible_comments_to_include]
comments_to_review
write.csv(comments_to_review, file = "comments_to_review.csv")
rows_requiring_comment_investigation <- ks_clean[which(ks_clean$COMMENTS %in% comments_to_review),]
rows_requiring_comment_investigation_simple <- rows_requiring_comment_investigation[,c("KID","API_NUMBER","STATUS","STATUS2","COMMENTS")]
View(rows_requiring_comment_investigation)
write.csv(rows_requiring_comment_investigation, file="rows_requiring_comment_investigation.csv")
write.csv(rows_requiring_comment_investigation_simple, file="rows_requiring_comment_investigation_simple.csv")

raw_manual_well_assignments <- read.csv(file="manual_well_assignments_comments.csv")

manual_without_drops <- raw_manual_well_assignments %>%
  filter(swd_inj_ci != "drop")

View(manual_without_drops)

core_manual <- manual_without_drops[,c("KID","swd_inj_ci","pa")]
names(core_manual) <- c("KID","man_swd_inj_ci","man_activity")
View(core_manual)

# final well group
# pull the KIDs of the wells selected from at least one of the groups
kids_comments_for_the_final_list <- ks_clean$KID[which(ks_clean$KID %in% manual_without_drops$KID)]
kids_status1s <- ks_clean$KID[which(ks_clean$STATUS %in% ks_definitely_include_status1s)]
kids_status2s <- ks_clean$KID[which(ks_clean$STATUS2 %in% ks_potential_disposal_include_status2s)]

length(kids_comments_for_the_final_list)
length(kids_status1s)
length(kids_status2s)

kid_list <- c(kids_comments_for_the_final_list, kids_status1s, kids_status2s)
kids_for_final_list <- unlist(kid_list)
length(kids_for_final_list)
kids_for_final_list <- unique(kids_for_final_list)
length(kids_for_final_list)

ks_final_wells <- ks_clean[which(ks_clean$KID %in% kids_for_final_list),]
View(ks_final_wells)

# add my own categories
ks_final_wells$swd_inj_ci <- NA # categorizing the well as SWD ('swd'), INJ ('inj'), or Class I ('ci')
ks_final_wells$activity <- NA # categorizing the well as plugged and abandoned ('pa') or not ('not_pa')
ks_final_wells$has_api <- NA # classifying as having or not having an api ('yes' or 'no')
ks_final_wells$other <- NA # classifying whether the well had a STATUS of "INJ", "INJ-P&A", "SWD", or "SWD-P&A" ('no') or a STATUS of some other type ('yes')






#### category assignments ####

#### assign has api ####
ks_final_wells$has_api  <-  ifelse(ks_final_wells$API_NUMBER == "", "no", "yes")
View(ks_final_wells)

#### swd versus not ####
View(table(ks_final_wells$STATUS))

ks_definitely_include_status1s <- sort(c("OTHER(1O&1SWD)","OTHER(CBM/SWD)","OTHER(CLASS ONE (OLD))","OTHER(CLASS1)","OTHER(NHDW)","OTHER(OIL,SWD)","OTHER(SWD-P&A)","OTHER-P&A(CLASS ONE (OLD))","OTHER-P&A(OIL-SWD)","SWD","SWD-P&A"))

swd_statii <- sort(c("OTHER(1O&1SWD)","OTHER(CBM/SWD)","OTHER(OIL,SWD)","OTHER(SWD-P&A)","OTHER-P&A(OIL-SWD)","SWD","SWD-P&A"))
class1_statii <- sort(c("OTHER(CLASS ONE (OLD))","OTHER(CLASS1)","OTHER(NHDW)","OTHER-P&A(CLASS ONE (OLD))"))
swd_statii2 <- sort(c("Converted to SWD Well"))

ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS %in% class1_statii] <-  'class1')
ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS %in% swd_statii | STATUS2 %in% swd_statii2]  <-  'swd')

#### pa versus not ####
pa_statii <- sort(c("OTHER(SWD-P&A)","OTHER-P&A(CLASS ONE (OLD))","OTHER-P&A(OIL-SWD)","SWD-P&A"))
ks_final_wells <- within(ks_final_wells, activity[STATUS %in% pa_statii] <-  'pa')

#### deal with comments ####
ks_final_wells <- merge(ks_final_wells, core_manual, by = "KID", all.x = TRUE, all.y = TRUE)
ks_final_wells <- within(ks_final_wells, swd_inj_ci[man_swd_inj_ci == 'swd'] <- 'swd')
ks_final_wells <- within(ks_final_wells, swd_inj_ci[man_swd_inj_ci == 'class1'] <- 'class1')
ks_final_wells <- within(ks_final_wells, swd_inj_ci[man_swd_inj_ci == 'swd_class1'] <- 'swd_class1')
ks_final_wells <- within(ks_final_wells, activity[man_activity == 'pa'] <- 'pa')
ks_final_wells <- within(ks_final_wells, other[STATUS %in% c("SWD","SWD-P&A")] <- 'no')

# assign not-others to "yes"
ks_final_wells$other[is.na(ks_final_wells$other)]  <-  "yes"
ks_final_wells$activity[is.na(ks_final_wells$activity)]  <-  "not_pa"
ks_final_wells$manual[is.na(ks_final_wells$man_swd_inj_ci)] <- "no"
ks_final_wells$manual[is.na(ks_final_wells$manual)] <- "yes"

View(ks_final_wells)

save(ks_final_wells,file="ks_final_wells.rdata")



#### deleting those with duplicate APIs
# make dataset without wells without apis
ks_only_apis <- ks_final_wells
ks_only_apis <- ks_final_wells[!(is.na(ks_final_wells$API_NUMBER) | ks_final_wells$API_NUMBER==""),]
# View(ks_only_apis)

# find duplicate APIs
ks_final_index  <-  duplicated(ks_only_apis$API_NUMBER) | duplicated(ks_only_apis$API_NUMBER, fromLast = TRUE)
ks_API_dups <- ks_only_apis[ks_final_index, ]
# View(ks_API_dups)


# remove duplicate APIs, keeping those last modified
ks_bye_dup_API <- subset(ks_only_apis, ave(modified_as_date, API_NUMBER, FUN = max) == modified_as_date)

# View(ks_bye_dup_API)


#### dealing with API duplication based on 4-digit activity code

# delete extraneous four digits
ks_bye_dup_API$API_NUMBER_SIMPLE  <-  substr(ks_bye_dup_API$API_NUMBER, 0, 12)
# View(ks_bye_dup_API)

# find duplicate APIs without extraneous four digits
ks_simple_API_index  <-  duplicated(ks_bye_dup_API$API_NUMBER_SIMPLE) | duplicated(ks_bye_dup_API$API_NUMBER_SIMPLE, fromLast = TRUE)
ks_simple_API_dups <- ks_bye_dup_API[ks_simple_API_index, ]
# View(ks_simple_API_dups)

# remove duplicate APIs, keeping those last modified
ks_bye_dup_API_simple <- subset(ks_bye_dup_API, ave(modified_as_date, API_NUMBER_SIMPLE, FUN = max) == modified_as_date)

# View(ks_bye_dup_API_simple)

# find duplicated latitudes and longitudes
ks_lat_long_dup_simple_index <- duplicated(ks_bye_dup_API_simple[c("LATITUDE","LONGITUDE")]) | duplicated(ks_bye_dup_API_simple[c("LATITUDE","LONGITUDE")], fromLast = TRUE)
ks_lat_long_dups_simple <- ks_bye_dup_API_simple[ks_lat_long_dup_simple_index, ]
# View(ks_lat_long_dups_simple)

# calling it the final thesis data
ks_final_thesis_data <- ks_bye_dup_API_simple
save(ks_final_thesis_data,file="ks_final_thesis_data.rdata")
# View(ks_final_thesis_data)


table(ks_final_thesis_data$other,ks_final_thesis_data$swd_inj_ci)
 
ks_set_for_mapping <- ks_final_thesis_data[,c("KID","API_NUMBER_SIMPLE","LATITUDE","LONGITUDE","swd_inj_ci")]
# View(ks_set_for_mapping)
write.csv2(ks_set_for_mapping,file="ks_set_for_mapping.csv")



# import mapping results back
ks_join_results <- read.csv(file="well_join_2018_10_25.txt",stringsAsFactors = FALSE)
# View(ks_join_results)
save(ks_join_results,file="ks_join_results.rdata")
#
# # load needed files if starting from here
# load(file="ks_final_thesis_data.rdata")
# load(file="ks_join_results.rdata")
# 
ks_join_to_edit <- ks_join_results
ks_join_to_edit$API_NUMBER <- NULL
ks_join_to_edit$LATITUDE <- NULL
ks_join_to_edit$LONGITUDE <- NULL
ks_join_to_edit$swd_inj_ci <- NULL
ks_join_to_edit$Join_Count <- NULL
ks_join_to_edit$OBJECTID_1 <- NULL
ks_join_to_edit$TARGET_FID <- NULL
 
ks_wells_and_block_groups <- merge(x = ks_final_thesis_data, y = ks_join_to_edit, by = "KID", all.x = TRUE)
View(ks_wells_and_block_groups)

# assigning years

# go for the date limit
ks_wells_and_block_groups$drilled_post_2006 <- NULL
ks_wells_and_block_groups$active_post_2006 <- NULL

# 2000 year
ks_wells_and_block_groups$permit_after_2000  <-  NA
ks_wells_and_block_groups$spud_after_2000  <-  NA
ks_wells_and_block_groups$completed_after_2000  <-  NA
ks_wells_and_block_groups$active_after_2000  <-  NA

# 2007 year
ks_wells_and_block_groups$permit_after_2007  <-  NA
ks_wells_and_block_groups$spud_after_2007  <-  NA
ks_wells_and_block_groups$completed_after_2007  <-  NA
ks_wells_and_block_groups$active_after_2007  <-  NA

# 2010 year
ks_wells_and_block_groups$permit_after_2010  <-  NA
ks_wells_and_block_groups$spud_after_2010  <-  NA
ks_wells_and_block_groups$completed_after_2010  <-  NA
ks_wells_and_block_groups$active_after_2010  <-  NA


# 2000 permit
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, permit_after_2000[permit_as_date >= "2000-01-01"]  <-  'yes')
ks_wells_and_block_groups$permit_after_2000[is.na(ks_wells_and_block_groups$permit_after_2000)]  <-  "no"
# 2000 spud
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, spud_after_2000[spud_as_date >= "2000-01-01"]  <-  'yes')
ks_wells_and_block_groups$spud_after_2000[is.na(ks_wells_and_block_groups$spud_after_2000)]  <-  "no"
# 2000 completed
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, completed_after_2000[completion_as_date >= "2000-01-01"]  <-  'yes')
ks_wells_and_block_groups$completed_after_2000[is.na(ks_wells_and_block_groups$completed_after_2000)]  <-  "no"

# 2007 permit
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, permit_after_2007[permit_as_date >= "2007-01-01"]  <-  'yes')
ks_wells_and_block_groups$permit_after_2007[is.na(ks_wells_and_block_groups$permit_after_2007)]  <-  "no"
# 2007 spud
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, spud_after_2007[spud_as_date >= "2007-01-01"]  <-  'yes')
ks_wells_and_block_groups$spud_after_2007[is.na(ks_wells_and_block_groups$spud_after_2007)]  <-  "no"
# 2007 completed
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, completed_after_2007[completion_as_date >= "2007-01-01"]  <-  'yes')
ks_wells_and_block_groups$completed_after_2007[is.na(ks_wells_and_block_groups$completed_after_2007)]  <-  "no"

# 2010 permit
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, permit_after_2010[permit_as_date >= "2010-01-01"]  <-  'yes')
ks_wells_and_block_groups$permit_after_2010[is.na(ks_wells_and_block_groups$permit_after_2010)]  <-  "no"
# 2010 spud
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, spud_after_2010[spud_as_date >= "2010-01-01"]  <-  'yes')
ks_wells_and_block_groups$spud_after_2010[is.na(ks_wells_and_block_groups$spud_after_2010)]  <-  "no"
# 2010 completed
ks_wells_and_block_groups <- within(ks_wells_and_block_groups, completed_after_2010[completion_as_date >= "2010-01-01"]  <-  'yes')
ks_wells_and_block_groups$completed_after_2010[is.na(ks_wells_and_block_groups$completed_after_2010)]  <-  "no"

# single fix based on comments and duplicate API
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, swd_inj_ci[KID == "1001233161"]  <-  'swd')

# currently active classification
ks_wells_and_block_groups$current_active <- NA

# use status
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, current_active[STATUS == "INJ-P&A" | STATUS == "OTHER-P&A(CLASS ONE (OLD))" | STATUS == " OTHER-P&A(INJ OR )" | STATUS == " OTHER-P&A(INJ or EOR)" | STATUS == "OTHER-P&A(OIL-SWD)" | STATUS == " OTHER-P&A(TA)" | STATUS == "SWD-P&A"]  <-  'plugged_abandoned')

# use presence of plugged date
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, current_active[!is.na(plugging_as_date)]  <-  'plugged_abandoned')

# make rest active
ks_wells_and_block_groups$current_active[is.na(ks_wells_and_block_groups$current_active)]  <-  "presumed_active"

# combine permit or spud after 2000
ks_wells_and_block_groups$permit_spud_post_2000 <- NA
ks_wells_and_block_groups$permit_spud_post_2007 <- NA
ks_wells_and_block_groups$permit_spud_post_2010 <- NA

# post 2000
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, permit_spud_post_2000[permit_after_2000 == "yes" | spud_after_2000 == "yes"]  <-  'yes')
ks_wells_and_block_groups$permit_spud_post_2000[is.na(ks_wells_and_block_groups$permit_spud_post_2000)]  <-  "no"

# post 2007
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, permit_spud_post_2007[permit_after_2007 == "yes" | spud_after_2007 == "yes"]  <-  'yes')
ks_wells_and_block_groups$permit_spud_post_2007[is.na(ks_wells_and_block_groups$permit_spud_post_2007)]  <-  "no"

# post 2010
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, permit_spud_post_2010[permit_after_2010 == "yes" | spud_after_2010 == "yes"]  <-  'yes')
ks_wells_and_block_groups$permit_spud_post_2010[is.na(ks_wells_and_block_groups$permit_spud_post_2010)]  <-  "no"


# categories by which to subset: swd_inj_ci, other, permit or spud after 2000, current_active

# make categories manually
ks_wells_and_block_groups$swd_norm_post2000_active <- NA
ks_wells_and_block_groups$swd_norm_post2000_pa <- NA
ks_wells_and_block_groups$swd_norm_pre2000_active <- NA
ks_wells_and_block_groups$swd_norm_pre2000_pa <- NA

ks_wells_and_block_groups$swd_other_post2000_active <- NA
ks_wells_and_block_groups$swd_other_post2000_pa <- NA
ks_wells_and_block_groups$swd_other_pre2000_active <- NA
ks_wells_and_block_groups$swd_other_pre2000_pa <- NA

ks_wells_and_block_groups$inj_norm_post2000_active <- NA
ks_wells_and_block_groups$inj_norm_post2000_pa <- NA
ks_wells_and_block_groups$inj_norm_pre2000_active <- NA
ks_wells_and_block_groups$inj_norm_pre2000_pa <- NA
ks_wells_and_block_groups$inj_other_post2000_active <- NA
ks_wells_and_block_groups$inj_other_post2000_pa <- NA
ks_wells_and_block_groups$inj_other_pre2000_active <- NA
ks_wells_and_block_groups$inj_other_pre2000_pa <- NA

ks_wells_and_block_groups$ci_other_post2000_active <- NA
ks_wells_and_block_groups$ci_other_post2000_pa <- NA
ks_wells_and_block_groups$ci_other_pre2000_active <- NA
ks_wells_and_block_groups$ci_other_pre2000_pa <- NA


ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, swd_norm_post2000_active[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active'] <- 'yes')
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, swd_norm_post2000_pa[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned'] <- 'yes')
# ks_wells_and_block_groups$swd_norm_pre2000_active
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, swd_norm_pre2000_active[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active'] <- 'yes')
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, swd_norm_pre2000_pa[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned'] <- 'yes')


ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, swd_other_post2000_active[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active'] <- 'yes')
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, swd_other_post2000_pa[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned'] <- 'yes')
# ks_wells_and_block_groups$swd_norm_pre2000_active
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, swd_other_pre2000_active[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active'] <- 'yes')
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, swd_other_pre2000_pa[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned'] <- 'yes')


ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, inj_norm_post2000_active[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active'] <- 'yes')
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, inj_norm_post2000_pa[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned'] <- 'yes')
# ks_wells_and_block_groups$inj_norm_pre2000_active
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, inj_norm_pre2000_active[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active'] <- 'yes')
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, inj_norm_pre2000_pa[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned'] <- 'yes')


ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, inj_other_post2000_active[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active'] <- 'yes')
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, inj_other_post2000_pa[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned'] <- 'yes')
# ks_wells_and_block_groups$inj_norm_pre2000_active
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, inj_other_pre2000_active[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active'] <- 'yes')
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, inj_other_pre2000_pa[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned'] <- 'yes')


ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, ci_other_post2000_active[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active'] <- 'yes')
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, ci_other_post2000_pa[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned'] <- 'yes')
# ks_wells_and_block_groups$swd_norm_pre2000_active
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, ci_other_pre2000_active[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active'] <- 'yes')
ks_wells_and_block_groups  <-  within(ks_wells_and_block_groups, ci_other_pre2000_pa[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned'] <- 'yes')

View(ks_wells_and_block_groups)


swd_norm_post2000_active <- aggregate(swd_norm_post2000_active~GEOID_Data,ks_wells_and_block_groups,length)
swd_norm_post2000_pa <- aggregate(swd_norm_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
swd_norm_pre2000_active <- aggregate(swd_norm_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
swd_norm_pre2000_pa <- aggregate(swd_norm_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)

swd_other_post2000_active <- aggregate(swd_other_post2000_active~GEOID_Data,ks_wells_and_block_groups,length)
swd_other_post2000_pa <- aggregate(swd_other_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length) # no data in this one
swd_other_pre2000_active <- aggregate(swd_other_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
swd_other_pre2000_pa <- aggregate(swd_other_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)

inj_norm_post2000_active <- aggregate(inj_norm_post2000_active~GEOID_Data,ks_wells_and_block_groups,length)
inj_norm_post2000_pa <- aggregate(inj_norm_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
inj_norm_pre2000_active <- aggregate(inj_norm_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
inj_norm_pre2000_pa <- aggregate(inj_norm_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
inj_other_post2000_active <- aggregate(inj_other_post2000_active~GEOID_Data,ks_wells_and_block_groups,length) # no data in this one
inj_other_post2000_pa <- aggregate(inj_other_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length) # no data in this one
inj_other_pre2000_active <- aggregate(inj_other_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
inj_other_pre2000_pa <- aggregate(inj_other_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)

ci_other_post2000_active <- aggregate(ci_other_post2000_active~GEOID_Data,ks_wells_and_block_groups,length) # no data
ci_other_post2000_pa <- aggregate(ci_other_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length) # no data
ci_other_pre2000_active <- aggregate(ci_other_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
ci_other_pre2000_pa <- aggregate(ci_other_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)

# merge them all
ks_well_counts <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "GEOID_Data", all = TRUE),
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
# View(ks_well_counts)

sum(colSums(ks_well_counts[2:16],na.rm = TRUE))

# rename GEOID_Data to GEOID in well data
ks_well_counts$GEOID <- ks_well_counts$GEOID_Data
save(ks_well_counts,file="ks_well_counts.rdata")
# load(file="ks_well_counts.rdata")

# load ACS variables
load(file="ACS_constructed_variables.rdata")
View(ACS_constructed_variables[,201:291])

# limit ks block groups to ks
ks_acs <- ACS_constructed_variables[which(ACS_constructed_variables$STATEFP=='20'),]
save(ks_acs,file="ks_acs.rdata")
load(file="ks_acs.rdata")

# merge well counts and acs!  hey-o!
KS_FINAL_DATASET <- merge(ks_well_counts, ks_acs, by = "GEOID", all = TRUE)
View(KS_FINAL_DATASET)
save(KS_FINAL_DATASET,file="KS_FINAL_DATASET.rdata")
write.csv(KS_FINAL_DATASET,file="KS_FINAL_DATASET.csv")




#### Analyses! ####

# load files if starting from here
load(file = "KS_FINAL_DATASET.rdata")
ks_analysis_dataset <- KS_FINAL_DATASET
View(ks_analysis_dataset)

#### swd well block groups only ####

# make variable for sum of swd wells per block group
ks_analysis_dataset$any_swd_sum <- NA
ks_analysis_dataset$any_swd_sum <- rowSums(ks_analysis_dataset[,c("swd_norm_post2000_active","swd_norm_post2000_pa","swd_norm_pre2000_active","swd_norm_pre2000_pa","swd_other_post2000_active","swd_other_pre2000_active","swd_other_pre2000_pa")], na.rm=TRUE)

# make new binary variable for have versus not have swd
ks_analysis_dataset$any_swd_binary <- NA
ks_analysis_dataset <- within(ks_analysis_dataset, any_swd_binary[any_swd_sum == 0] <- 'no')
ks_analysis_dataset <- within(ks_analysis_dataset, any_swd_binary[any_swd_sum > 0] <- 'yes')
ks_analysis_dataset$any_swd_binary

names(ks_analysis_dataset)


#### correlation matrix of ACS variables ####
names(ks_analysis_dataset) # get variable names

# make dataframe for the correlation dataset
correlation_matrix_data  <-  ks_analysis_dataset[,c("median_household_income_B19013","median_household_value_B25077","percent_white_B03002","population_density_B01001_ALAND","percent_high_school_plus_B15003","median_age_B01002","any_swd_sum","any_swd_binary","GEOID_simple")]

# create the correlation matrix
correlation_matrix  <-  round(cor(correlation_matrix_data[,1:6], use = "pairwise.complete.obs"),2) # rounds to 2 decimal places

# view and write to file
View(correlation_matrix)
write.csv(correlation_matrix,file="correlation_matrix.csv")

# count observations going into the correlation matrix
counts_pairwise_correlations  <-  count.pairwise(correlation_matrix_data[,1:6], y = NULL,diagonal=TRUE)

# view and write to file
View(counts_pairwise_correlations)
write.csv(counts_pairwise_correlations,file="counts_pairwise_correlations.csv")






#### analysis by presence and absence of swd ####
# number of block groups with and without swd wells
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

# t test
t.test(ks_analysis_dataset$median_household_income_B19013[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$median_household_income_B19013[ks_analysis_dataset$any_swd_binary=="no"])


## median household value
# median
aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$median_household_value_B25077, na.rm = TRUE)

# interquartile range
aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$median_household_value_B25077, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$median_household_value_B25077))
aggregate(median_household_value_B25077~any_swd_binary, data=ks_analysis_dataset, FUN=length)

# t test
t.test(ks_analysis_dataset$median_household_value_B25077[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$median_household_value_B25077[ks_analysis_dataset$any_swd_binary=="no"])


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

# t test
t.test(ks_analysis_dataset$percent_white_B03002[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$percent_white_B03002[ks_analysis_dataset$any_swd_binary=="no"])


## population density
# median
aggregate(population_density_B01001_ALAND~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)

# interquartile range
aggregate(population_density_B01001_ALAND~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$population_density_B01001_ALAND, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$population_density_B01001_ALAND))

# t test
t.test(ks_analysis_dataset$population_density_B01001_ALAND[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$population_density_B01001_ALAND[ks_analysis_dataset$any_swd_binary=="no"])


## percent high school or more
# median
aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=median)
median(ks_analysis_dataset$percent_high_school_plus_B15003, na.rm = TRUE)

# interquartile range
aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=quantile)
quantile(ks_analysis_dataset$percent_high_school_plus_B15003, na.rm = TRUE)

# missings
table(is.na(ks_analysis_dataset$percent_high_school_plus_B15003))
aggregate(percent_high_school_plus_B15003~any_swd_binary, data=ks_analysis_dataset, FUN=length)

# t test
t.test(ks_analysis_dataset$percent_high_school_plus_B15003[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$percent_high_school_plus_B15003[ks_analysis_dataset$any_swd_binary=="no"])


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

# t test
t.test(ks_analysis_dataset$median_age_B01002[ks_analysis_dataset$any_swd_binary=="yes"], ks_analysis_dataset$median_age_B01002[ks_analysis_dataset$any_swd_binary=="no"])

#### Poisson
poisson_model <- glm(formula = any_swd_sum ~ median_household_income_B19013 + median_household_value_B25077 + percent_white_B03002 + population_density_B01001_ALAND + percent_high_school_plus_B15003 + median_age_B01002, family = "poisson", data = ks_analysis_dataset)
summary(poisson_model)


write.csv(ks_analysis_dataset,file="ks_analysis_dataset.csv")