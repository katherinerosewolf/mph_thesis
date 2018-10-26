# Oil and Gas Disposal Well Analysis for Kansas
# Katherine Wolf
# July 2018

# setup
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)

setwd("C:/Rfiles/MPHProjectR/workingdata/")

################

#### Kansas ####

# # import well data
# ks_wells_2018_07_02<-read.csv(file="./raw_files_as_imported_into_r/ks_wells_2018_07_02.txt", stringsAsFactors = FALSE)
# View(ks_wells_2018_07_02)
# 
# # save file as an rdata file
# save(ks_wells_2018_07_02,file="ks_wells_2018_07_02.rdata")

# load rdata file
load(file="ks_wells_2018_07_02.rdata")

# make version for cleaning
ks_clean<-ks_wells_2018_07_02
View(ks_clean)

##### UIC data

# import UIC data
ks_uic_2018_06_28<-read.csv(file="./raw_files_as_imported_into_r/KS_UIC_archive_2018_06_28.txt")
save(ks_uic_2018_06_28,file="ks_uic_2018_06_28.rdata")
load(file="ks_uic_2018_06_28.rdata")
View(ks_uic_2018_06_28)

# isolate individual well IDs present in UIC data
kids<-unique(ks_uic_2018_06_28$KGS_ID)
kids

# make table of KS wells in UIC data
ks_wells_in_uic_data_only <- subset(ks_clean, KID %in% kids)
View(ks_wells_in_uic_data_only)
save(ks_wells_in_uic_data_only,file="ks_wells_in_uic_data_only.rdata")

# make table of statuses in UIC data
uic_statuses<-table(ks_wells_in_uic_data_only$STATUS)
View(uic_statuses)
write.csv(uic_statuses,file="uic_statuses.csv")

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

# view and save results of all these conversions
View(ks_clean)
save(ks_clean,file="ks_clean.rdata")

##### eliminating duplicates #####

# count unique APIs
number_of_unique_APIs<-length(unique(ks_clean$API_NUMBER))
number_of_unique_APIs
save(number_of_unique_APIs,file="number_of_unique_APIs.rdata")

# raw well count
raw_well_count<-nrow(ks_clean)
raw_well_count
save(raw_well_count,file="raw_well_count.rdata")

# overall counts by API
ks_by_API_count<-table(ks_clean$API_NUMBER)
save(ks_by_API_count,file="ks_by_API_count.rdata")
View(ks_by_API_count)

# view rows with duplicated APIs
index<-duplicated(ks_clean$API_NUMBER) | duplicated(ks_clean$API_NUMBER, fromLast = TRUE)
ks_API_dups<-ks_clean[index,]
View(ks_API_dups)
save(ks_API_dups,file="ks_API_dups.rdata")

# view counts of duplicates per API
ks_API_dup_counts<-table(ks_API_dups$API_NUMBER)
View(ks_API_dup_counts)
save(ks_API_dup_counts,file="ks_dup_API_count.rdata")

# count unique KIDs
unique_KID_count<-length(unique(ks_clean$KID))
save(unique_KID_count,file="unique_KID_count.rdata")

# counts by well status for rows without APIs
ks_wells_no_API<-ks_clean[which(ks_clean$API_NUMBER == ""),]
View(ks_wells_no_API)
save(ks_wells_no_API,file="ks_wells_no_API.rdata")

ks_wells_no_API_by_status<-table(ks_wells_no_API$STATUS)
View(ks_wells_no_API_by_status)
save(ks_wells_no_API_by_status,file="ks_wells_no_API_by_status.rdata")




##### checking ambiguous wells for inclusion or exclusion #####

load(file="ks_clean.rdata")

# get counts by well status (main status)
ks_well_counts_by_status<-table(ks_clean$STATUS)
save(ks_well_counts_by_status,file="ks_well_counts_by_status.rdata")
View(ks_well_counts_by_status)
write.csv(ks_well_counts_by_status, file="ks_well_counts_by_status.csv")

# get counts by well status 2
ks_well_counts_by_status_2<-table(ks_clean$STATUS2)
save(ks_well_counts_by_status_2,file="ks_well_counts_by_status_2.rdata")
View(ks_well_counts_by_status_2)
write.csv(ks_well_counts_by_status_2, file="ks_well_counts_by_status_2.csv")

View(ks_well_counts_by_status_2)

# broad inclusion criteria
ks_included_status1s<-c("INJ","INJ-P&A","INTENT","OTHER()","OTHER(1O&1SWD)","OTHER(CBM/SWD)","OTHER(CLASS ONE (OLD))","OTHER(CLASS1)","OTHER(INJ or EOR)","OTHER(NHDW)","OTHER(NULL)","OTHER(OIL,SWD)","OTHER(OTHER)","OTHER(SWD-P&A)","OTHER(TA)","OTHER(TEMP ABD)","OTHER-P&A()","OTHER-P&A(CLASS ONE (OLD))","OTHER-P&A(INJ OR )","OTHER-P&A(INJ or EOR)","OTHER-P&A(OIL-SWD)","OTHER-P&A(TA)","SWD","SWD-P&A")

# make table of counts of all well status 1s for PowerPoint
ks_well_counts_by_status<-as.data.frame(ks_well_counts_by_status) # convert to dataframe
View(ks_well_counts_by_status) # check well counts

# make table of included wells statuses for PowerPoint
ks_included_status1_well_table<-ks_well_counts_by_status[
  which(ks_well_counts_by_status$Var1 
        %in% ks_included_status1s),] # make table of included well statuses
View(ks_included_status1_well_table)
save(ks_included_status1_well_table,file="ks_included_status1_well_table.rdata")
write.csv(ks_included_status1_well_table,file="ks_included_status1_well_table.csv")

# make table of excluded wells for PowerPoint
`%notin%` <- function(x,y) !(x %in% y) # define "not-in" function

ks_excluded_status1_well_table<-ks_well_counts_by_status[
  which(ks_well_counts_by_status$Var1 
        %notin% ks_included_status1s),] # make table of excluded wells

View(ks_excluded_status1_well_table)
save(ks_excluded_status1_well_table,file="ks_excluded_status1_well_table.rdata")
write.csv(ks_excluded_status1_well_table,file="ks_excluded_status1_well_table.csv")

# make dataframe of only included wells
ks_select_wells<-ks_clean[which(ks_clean$STATUS %in% ks_included_status1s),]
View(ks_select_wells)



#### figuring out which others I should keep ####

# make vector of other statuses with little other information (20 or more wells)
ks_statuses_to_investigate_in_group<-ks_included_status1_well_table[ks_included_status1_well_table$Freq >= 20, "Var1"] # select those statuses with equal to or greater than 20 wells

ks_statuses_to_investigate_in_group<-as.character(ks_statuses_to_investigate_in_group) # convert result of the above to a character vector

View(ks_statuses_to_investigate_in_group)

# character vector of statuses to remove
remove <- c("INJ", "INJ-P&A", "SWD", "SWD-P&A", "OTHER(CLASS1)", "OTHER(CLASS ONE (OLD))")
# remainder includes "INTENT", "OTHER-P&A()","OTHER-P&A(INJ or EOR)","OTHER-P&A(TA)","OTHER()","OTHER(TA)"

ks_statuses_to_investigate_in_group <- setdiff(ks_statuses_to_investigate_in_group, remove)

# select those wells with a status in the big group investigation
ks_wells_to_investigate_in_group<-ks_select_wells[which(ks_select_wells$STATUS %in% ks_statuses_to_investigate_in_group),]
View(ks_wells_to_investigate_in_group)

# make frequency table of wells in the big group investigation
ks_well_counts_by_status_big_group_investigation<-table(ks_wells_to_investigate_in_group$STATUS)

# table of secondary statuses of ambiguous "other" wells
ks_group_investigation_status2s<-table(ks_wells_to_investigate_in_group$STATUS2)
View(ks_group_investigation_status2s)
write.csv(ks_group_investigation_status2s,file="ks_group_investigation_status2s.csv")

# make data frame of comments on others
ks_group_investigation_comments<-table(ks_wells_to_investigate_in_group$COMMENTS)
ks_group_investigation_comments<-as.data.frame(ks_group_investigation_comments)
write.csv(ks_group_investigation_comments,file="ks_group_investigation_comments.csv")
View(ks_group_investigation_comments)
unique(ks_group_investigation_comments$Var1)

# vector of comments that had "swd", "inj", "disposal", "class I", "salt water disposal" included to keep (out of 1,777 different comments)
disposal_comments<-c(
  "elog marked in pencil, Collins SWD 1-BRB10/9/2000  quarter call & KB not on elog thus top card/ACO1 not identifiable for well number-BRB10/9/2000",
  "believed to be same well as Mid-West Oil and Gas drilled in the twenties-also exact footage as a Stanolind #1 SWD drilled to just over 500 feet-LKS",
  "ACO-1 also lists well as a water injection well - 1/21/04 ABC",
  "ACO-1 also lists well as water injection - 1/21/04 ABC",
  "Ground elevation taken from scout card. Well is also a water injection per ACO-1 - 1/14/04 ABC",
  "SALT WATER DISPOSAL TEST",
  "WATER INJ WELL","Well may match Class I well with KID 1002876875 (DRL 5-4-2012)",
  "injection into Arbuckle?",
  "ACO-1 also lists well as a water injection well - 1/21/04 ABC")

# keep these STATUS2s
ks_status2s_to_keep_from_investigation<-c(
  "Authorized Injection Well",
  "Injection Authorization Terminated",
  "Injection Authorization Terminated - INACTIVE CODE",
  "Unplugged Former Injection Well",
  "Converted to SWD Well",
  "Injection Well Split to Another Dkt")

# define "not-in" function
`%notin%` <- function(x,y) !(x %in% y)

# select wells to drop based on status 2 and comments
wells_to_drop <- subset(ks_wells_to_investigate_in_group, STATUS2 %notin% ks_status2s_to_keep_from_investigation & COMMENTS %notin% disposal_comments)

View(wells_to_drop)

# get well IDs alone
kids_of_wells_to_drop<-wells_to_drop$KID
save(kids_of_wells_to_drop,file="kids_of_wells_to_drop.rdata")
save(ks_select_wells,file="ks_select_wells.rdata")

View(ks_select_wells)





#### little category well review ####

#### OTHER-(P&A(INJ OR )): 2 wells assumed to be INJ, plugged and abandoned
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER-P&A(INJ OR )"),])
# 2 wells assumed to be INJ: KIDs 1033706748 1035942341

#### OTHER-P&A(INJ or EOR) (1 well)
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER-P&A(INJ or EOR)"),])
# assumed to be INJ based on comment "Injection into Arbuckle?": KID 1006054097

#### OTHER-P&A(OIL-SWD) (1 well)
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER-P&A(OIL-SWD)"),])
# assumed to be SWD: KID 1006164600

#### OTHER-P&A(TA) (3 wells)
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER-P&A(TA)"),])
# KID 1006072336 assumed to be SWD due to STATUS2 "Converted to SWD Well"
# KID 1006112943 assumed to be SWD due to STATUS2 "Injection Authorization Terminated"
# KID 1031374643 assumed to be SWD due to STATUS2 "Authorized Injection Well"

#### OTHER(1O&1SWD) (2 wells)
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER(1O&1SWD)"),])
# KID 1006162621 assumed to be SWD despite STATUS2 of "Authorized Injection Well"
# KID 1037043342 assumed to be SWD despite no STATUS2

#### OTHER(CBM/SWD) (1 well)
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER(CBM/SWD)"),])
# KID 1028007900 assumed to be SWD despite STATUS2 of "Converted to Producing Well"

#### OTHER(INJ or EOR) (1 well) (NO RELEVANT WELLS)
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER(INJ or EOR)"),])
# KID 1046071032 assumed to be INJ b/c STATUS2 is "Recompleted"

#### OTHER(NHDW) (1 well)
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER(NHDW)"),])
ks_select_wells[which(ks_select_wells$STATUS=="OTHER(NHDW)"),]
# KID 1043994238 assumed to be Class I due to STATUS2 of "Non Hazardous Class 1 Disposal Well KDHE Permit - KS-01-159-008. MTD, WWSL, 09-06-2011"

#### OTHER(NULL) (3 wells) NO RELEVANT WELLS
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER(NULL)"),])
# 1037651940, 1038098730, 1038098731 all should be excluded

#### OTHER(OIL,SWD) (1 well)
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER(OIL,SWD)"),])
# KID 1006137923 assumed to be SWD despite STATUS2 of "Authorized Injection Well"

#### OTHER(OTHER) (12 wells) NO RELEVANT WELLS
ks_other<-ks_select_wells[which(ks_select_wells$STATUS=="OTHER(OTHER)"),]
View(ks_other)
# all not injections due to no STATUS2 or comments indicating relevance

#### OTHER(SWD-P&A) (1 well)
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER(SWD-P&A)"),])
# 1046799842 assumed SWD-P&A due to STATUS

#### OTHER(TEMP ABD) (3 wells)
View(ks_select_wells[which(ks_select_wells$STATUS=="OTHER(TEMP ABD)"),])
# none included because no STATUS2 or relevant comments

# list of statuses for rows with no relevant wells
no_relevant_wells<-c("OTHER(INJ or EOR)","OTHER(NULL)","OTHER(OTHER)","OTHER(TEMP ABD)")
str(no_relevant_wells)

#### begin final well list ####

# load necessary files if starting from here
load(file="ks_select_wells.rdata")
load(file="kids_of_wells_to_drop.rdata")
load(file="ks_uic_2018_06_28.rdata")

# define "not-in" function
`%notin%` <- function(x,y) !(x %in% y)

ks_final_wells<-ks_select_wells # make data set for mucking

# drop the droppables
ks_final_wells<-ks_final_wells[which(ks_final_wells$KID %notin% kids_of_wells_to_drop),]
ks_finalish_well_statuses<-table(ks_final_wells$STATUS)
View(ks_finalish_well_statuses)
write.csv(ks_finalish_well_statuses,file="ks_finalish_well_statuses.csv")

# drop small well categories with no relevant wells
ks_final_wells<-ks_final_wells[which(ks_final_wells$STATUS %notin% no_relevant_wells),]
ks_finalish2_well_statuses<-table(ks_final_wells$STATUS)
View(ks_finalish2_well_statuses)

# add my own categories
ks_final_wells$swd_inj_ci<-NA
ks_final_wells$activity<-NA
ks_final_wells$has_api<-NA
ks_final_wells$other<-NA

# assign has api
ks_final_wells$has_api <- ifelse(ks_final_wells$API_NUMBER == "", "no", "yes")
View(ks_final_wells)




#### updating the little categories
View(table(ks_final_wells$STATUS))



#### OTHER-P&A(INJ OR ) DONE

# active or PA
ks_final_wells <- within(ks_final_wells, activity[STATUS == "OTHER-P&A(INJ OR )"] <- 'pa')
# swd_inj_ci
ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "OTHER-P&A(INJ OR )"] <- 'inj')
# other flag
ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER-P&A(INJ OR )"] <- 'yes')



#### OTHER-P&A(INJ or EOR) DONE

# active or PA
ks_final_wells <- within(ks_final_wells, activity[STATUS == "OTHER-P&A(INJ or EOR)"] <- 'pa')
# swd_inj_ci
ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER-P&A(INJ or EOR)"] <- 'inj')
# other flag
ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER-P&A(INJ or EOR)"] <- 'yes')



#### OTHER-P&A(OIL-SWD) DONE

# activity
ks_final_wells <- within(ks_final_wells, activity[STATUS == "OTHER-P&A(OIL-SWD)"] <- 'pa')
# swd_inj_ci
ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "OTHER-P&A(OIL-SWD)"] <- 'swd')
# other flag
ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER-P&A(OIL-SWD)"] <- 'yes')


#### OTHER-P&A(TA) DONE

View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER-P&A(TA)"),])

# activity
ks_final_wells <- within(ks_final_wells, activity[STATUS == "OTHER-P&A(TA)"] <- 'pa')
# swd_inj_ci
ks_final_wells<-within(ks_final_wells, swd_inj_ci[KID == 1006072336] <- 'swd')
ks_final_wells<-within(ks_final_wells, swd_inj_ci[KID == 1006112943 | KID == 1031374643] <- 'inj')
# other flag
ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER-P&A(TA)"] <- 'yes')



#### OTHER(1O&1SWD) DONE

View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(1O&1SWD)"),])

# activity
ks_final_wells <- within(ks_final_wells, activity[API_NUMBER == "15-191-21652"] <- 'not_pa')
ks_final_wells <- within(ks_final_wells, activity[API_NUMBER == "15-079-00332-0001"] <- 'no_plug')
# swd_inj_ci
ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "OTHER(1O&1SWD)"] <- 'swd')
# other flag
ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER(1O&1SWD)"] <- 'yes')



#### OTHER(CBM/SWD) DONE

View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(CBM/SWD)"),])

# activity
ks_final_wells <- within(ks_final_wells, activity[API_NUMBER == "15-019-26584"] <- 'not_pa')
# swd_inj_ci
ks_final_wells <- within(ks_final_wells, swd_inj_ci[API_NUMBER == "15-019-26584"] <- 'swd')
# other flag
ks_final_wells <- within(ks_final_wells, other[API_NUMBER == "15-019-26584"] <- 'yes')



#### OTHER(NHDW) DONE

View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(NHDW)"),])

# activity
ks_final_wells <- within(ks_final_wells, activity[KID == "1043994238"] <- 'not_pa')
# swd_inj_ci
ks_final_wells <- within(ks_final_wells, swd_inj_ci[KID == "1043994238"] <- 'c1')
# other flag
ks_final_wells <- within(ks_final_wells, other[KID == "1043994238"] <- 'yes')


#### OTHER(OIL,SWD) DONE

View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(OIL,SWD)"),])

# activity
ks_final_wells <- within(ks_final_wells, activity[KID == "1006137923"] <- 'not_pa')
# swd_inj_ci
ks_final_wells <- within(ks_final_wells, swd_inj_ci[KID == "1006137923"] <- 'swd')
# other flag
ks_final_wells <- within(ks_final_wells, other[KID == "1006137923"] <- 'yes')



#### OTHER(SWD-P&A) DONE

View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(SWD-P&A)"),])

# activity
ks_final_wells <- within(ks_final_wells, activity[STATUS == "OTHER(SWD-P&A)"] <- 'pa')
# swd_inj_ci
ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "OTHER(SWD-P&A)"] <- 'swd')
# other flag
ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER(SWD-P&A)"] <- 'yes')



#### OTHER() DONE
# swd_inj_ci INJ wells by STATUS2
ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER()" & STATUS2 %in% c(
  "Authorized Injection Well",
  "Injection Authorization Terminated",
  "Injection Authorization Terminated - INACTIVE CODE",
  "Unplugged Former Injection Well",
  "Injection Well Split to Another Dkt")] <- 'inj')
# swd_inj_ci INJ wells by COMMENTS
ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER()" & COMMENTS %in% c(
  "WATER INJ WELL",
  "ACO-1 also lists well as a water injection well - 1/21/04 ABC",
  "ACO-1 also lists well as water injection - 1/21/04 ABC",
  "Ground elevation taken from scout card. Well is also a water injection per ACO-1 - 1/14/04 ABC",
  "injection into Arbuckle?")]<-'inj')

# swd_inj_ci SWD wells by STATUS2
ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER()" & STATUS2 == "Converted to SWD Well"]<-'swd')

# swd_inj_ci SWD wells by COMMENTS
ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER()" & COMMENTS %in% c("elog marked in pencil, Collins SWD 1-BRB10/9/2000  quarter call & KB not on elog thus top card/ACO1 not identifiable for well number-BRB10/9/2000","believed to be same well as Mid-West Oil and Gas drilled in the twenties-also exact footage as a Stanolind #1 SWD drilled to just over 500 feet-LKS","SALT WATER DISPOSAL TEST")] <- 'swd')

# other flag
ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER()"] <- 'yes')

write.csv(ks_final_wells,file="ks_final_wells.csv")

#### OTHER(TA)


View(ks_final_wells[which(ks_final_wells$STATUS=="OTHER(TA)"),])

# swd_inj_ci
ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER(TA)" & STATUS2 == "Converted to SWD Well"]<-'swd')

ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER(TA)" & STATUS2 == "Authorized Injection Well"]<-'inj')

ks_final_wells<-within(ks_final_wells, swd_inj_ci[STATUS == "OTHER(TA)" & STATUS2 == "Producing"] <- 'inj')

# other flag
ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER(TA)"] <- 'yes')



#### class ones

# swd_inj_ci
ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "OTHER(CLASS1)" | STATUS == "OTHER(CLASS1)" | STATUS == "OTHER(CLASS ONE (OLD))" | STATUS == "OTHER-P&A(CLASS ONE (OLD))" | STATUS == "OTHER(NHDW)"] <- 'c1')

ks_final_wells <- within(ks_final_wells, swd_inj_ci[COMMENTS == "Well may match Class I well with KID 1002876875 (DRL 5-4-2012)"] <- 'c1')

# other flag
ks_final_wells <- within(ks_final_wells, other[STATUS == "OTHER(CLASS1)" | STATUS == "OTHER(CLASS1)" | STATUS == "OTHER(CLASS ONE (OLD))" | STATUS == "OTHER-P&A(CLASS ONE (OLD))" | COMMENTS == "Well may match Class I well with KID 1002876875 (DRL 5-4-2012)"] <- 'yes')

# activity
ks_final_wells <- within(ks_final_wells, activity[STATUS == "OTHER-P&A(CLASS ONE (OLD))"] <- 'pa')






#### conditional group assignments

# assign statuses

# SWDs
ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "SWD" | STATUS == "SWD-P&A"] <- 'swd')
ks_final_wells <- within(ks_final_wells, activity[STATUS == "SWD"] <- 'not_pa')
ks_final_wells <- within(ks_final_wells, activity[STATUS == "SWD-P&A"] <- 'pa')

# INJs
ks_final_wells <- within(ks_final_wells, swd_inj_ci[STATUS == "INJ" | STATUS == "INJ-P&A"] <- 'inj')
ks_final_wells <- within(ks_final_wells, activity[STATUS == "INJ"] <- 'not_pa')
ks_final_wells <- within(ks_final_wells, activity[STATUS == "INJ-P&A"] <- 'pa')

# assign not-others to "no"
ks_final_wells$other[is.na(ks_final_wells$other)] <- "no"
ks_final_wells$activity[is.na(ks_final_wells$activity)] <- "not_pa"
View(ks_final_wells)

save(ks_final_wells,file="ks_final_wells.rdata")



#### deleting those with duplicate APIs
# make dataset without wells without apis
ks_only_apis<-ks_final_wells
ks_only_apis<-ks_final_wells[!(is.na(ks_final_wells$API_NUMBER) | ks_final_wells$API_NUMBER==""),]
View(ks_only_apis)

# find duplicate APIs
ks_final_index <- duplicated(ks_only_apis$API_NUMBER) | duplicated(ks_only_apis$API_NUMBER, fromLast = TRUE)
ks_API_dups<-ks_only_apis[ks_final_index, ]
View(ks_API_dups)


# remove duplicate APIs, keeping those last modified
ks_bye_dup_API<-subset(ks_only_apis, ave(modified_as_date, API_NUMBER, FUN = max) == modified_as_date)

View(ks_bye_dup_API)


#### dealing with API duplication based on 4-digit activity code

# delete extraneous four digits
ks_bye_dup_API$API_NUMBER_SIMPLE <- substr(ks_bye_dup_API$API_NUMBER, 0, 12)
View(ks_bye_dup_API)

# find duplicate APIs without extraneous four digits
ks_simple_API_index <- duplicated(ks_bye_dup_API$API_NUMBER_SIMPLE) | duplicated(ks_bye_dup_API$API_NUMBER_SIMPLE, fromLast = TRUE)
ks_simple_API_dups<-ks_bye_dup_API[ks_simple_API_index, ]
View(ks_simple_API_dups)

# remove duplicate APIs, keeping those last modified
ks_bye_dup_API_simple<-subset(ks_bye_dup_API, ave(modified_as_date, API_NUMBER_SIMPLE, FUN = max) == modified_as_date)

View(ks_bye_dup_API_simple)

# find duplicated latitudes and longitudes
ks_lat_long_dup_simple_index<-duplicated(ks_bye_dup_API_simple[c("LATITUDE","LONGITUDE")]) | duplicated(ks_bye_dup_API_simple[c("LATITUDE","LONGITUDE")], fromLast = TRUE)
ks_lat_long_dups_simple<-ks_bye_dup_API_simple[ks_lat_long_dup_simple_index, ]
View(ks_lat_long_dups_simple)








# calling it the final thesis data
ks_final_thesis_data<-ks_bye_dup_API_simple
save(ks_final_thesis_data,file="ks_final_thesis_data.rdata")
View(ks_final_thesis_data)

table(ks_final_thesis_data$other,ks_final_thesis_data$swd_inj_ci)
 
ks_set_for_mapping<-ks_final_thesis_data[,c("KID","API_NUMBER_SIMPLE","LATITUDE","LONGITUDE","swd_inj_ci")]
View(ks_set_for_mapping)
write.csv2(ks_set_for_mapping,file="ks_set_for_mapping.csv")



# # import mapping results back
# ks_join_results<-read.csv(file="KansasWellJoinResults20180415.txt",stringsAsFactors = FALSE)
# View(ks_join_results)
# save(ks_join_results,file="ks_join_results.rdata")
#
# # load needed files if starting from here
# load(file="ks_final_thesis_data.rdata")
# load(file="ks_join_results.rdata")
# 
# ks_join_to_edit<-ks_join_results
# ks_join_to_edit$API_NUMBER<-NULL
# ks_join_to_edit$LATITUDE<-NULL
# ks_join_to_edit$LONGITUDE<-NULL
# ks_join_to_edit$swd_inj_ci<-NULL
# ks_join_to_edit$Join_Count<-NULL
# ks_join_to_edit$OBJECTID_1<-NULL
# ks_join_to_edit$TARGET_FID<-NULL
# 
# ks_wells_and_block_groups<-merge(x = ks_final_thesis_data, y = ks_join_to_edit, by = "KID", all.x = TRUE)
# 
# # assigning years
# 
# # go for the date limit
# ks_wells_and_block_groups$drilled_post_2006<-NULL
# ks_wells_and_block_groups$active_post_2006<-NULL
# 
# # 2000 year
# ks_wells_and_block_groups$permit_after_2000 <- NA
# ks_wells_and_block_groups$spud_after_2000 <- NA
# ks_wells_and_block_groups$completed_after_2000 <- NA
# ks_wells_and_block_groups$active_after_2000 <- NA
# 
# # 2007 year
# ks_wells_and_block_groups$permit_after_2007 <- NA
# ks_wells_and_block_groups$spud_after_2007 <- NA
# ks_wells_and_block_groups$completed_after_2007 <- NA
# ks_wells_and_block_groups$active_after_2007 <- NA
# 
# # 2010 year
# ks_wells_and_block_groups$permit_after_2010 <- NA
# ks_wells_and_block_groups$spud_after_2010 <- NA
# ks_wells_and_block_groups$completed_after_2010 <- NA
# ks_wells_and_block_groups$active_after_2010 <- NA
# 
# 
# # 2000 permit
# ks_wells_and_block_groups<-within(ks_wells_and_block_groups, permit_after_2000[permit_as_date >= "2000-01-01"] <- 'yes')
# ks_wells_and_block_groups$permit_after_2000[is.na(ks_wells_and_block_groups$permit_after_2000)] <- "no"
# # 2000 spud
# ks_wells_and_block_groups<-within(ks_wells_and_block_groups, spud_after_2000[spud_as_date >= "2000-01-01"] <- 'yes')
# ks_wells_and_block_groups$spud_after_2000[is.na(ks_wells_and_block_groups$spud_after_2000)] <- "no"
# # 2000 completed
# ks_wells_and_block_groups<-within(ks_wells_and_block_groups, completed_after_2000[completion_as_date >= "2000-01-01"] <- 'yes')
# ks_wells_and_block_groups$completed_after_2000[is.na(ks_wells_and_block_groups$completed_after_2000)] <- "no"
# 
# # 2007 permit
# ks_wells_and_block_groups<-within(ks_wells_and_block_groups, permit_after_2007[permit_as_date >= "2007-01-01"] <- 'yes')
# ks_wells_and_block_groups$permit_after_2007[is.na(ks_wells_and_block_groups$permit_after_2007)] <- "no"
# # 2007 spud
# ks_wells_and_block_groups<-within(ks_wells_and_block_groups, spud_after_2007[spud_as_date >= "2007-01-01"] <- 'yes')
# ks_wells_and_block_groups$spud_after_2007[is.na(ks_wells_and_block_groups$spud_after_2007)] <- "no"
# # 2007 completed
# ks_wells_and_block_groups<-within(ks_wells_and_block_groups, completed_after_2007[completion_as_date >= "2007-01-01"] <- 'yes')
# ks_wells_and_block_groups$completed_after_2007[is.na(ks_wells_and_block_groups$completed_after_2007)] <- "no"
# 
# # 2010 permit
# ks_wells_and_block_groups<-within(ks_wells_and_block_groups, permit_after_2010[permit_as_date >= "2010-01-01"] <- 'yes')
# ks_wells_and_block_groups$permit_after_2010[is.na(ks_wells_and_block_groups$permit_after_2010)] <- "no"
# # 2010 spud
# ks_wells_and_block_groups<-within(ks_wells_and_block_groups, spud_after_2010[spud_as_date >= "2010-01-01"] <- 'yes')
# ks_wells_and_block_groups$spud_after_2010[is.na(ks_wells_and_block_groups$spud_after_2010)] <- "no"
# # 2010 completed
# ks_wells_and_block_groups<-within(ks_wells_and_block_groups, completed_after_2010[completion_as_date >= "2010-01-01"] <- 'yes')
# ks_wells_and_block_groups$completed_after_2010[is.na(ks_wells_and_block_groups$completed_after_2010)] <- "no"
# 
# # single fix based on comments and duplicate API
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_inj_ci[KID == "1001233161"] <- 'swd')
# 
# # currently active classification
# ks_wells_and_block_groups$current_active<-NA
# 
# # use status
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, current_active[STATUS == "INJ-P&A" | STATUS == "OTHER-P&A(CLASS ONE (OLD))" | STATUS == " OTHER-P&A(INJ OR )" | STATUS == " OTHER-P&A(INJ or EOR)" | STATUS == "OTHER-P&A(OIL-SWD)" | STATUS == " OTHER-P&A(TA)" | STATUS == "SWD-P&A"] <- 'plugged_abandoned')
# 
# # use presence of plugged date
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, current_active[!is.na(plugging_as_date)] <- 'plugged_abandoned')
# 
# # make rest active
# ks_wells_and_block_groups$current_active[is.na(ks_wells_and_block_groups$current_active)] <- "presumed_active"
# 
# # combine permit or spud after 2000
# ks_wells_and_block_groups$permit_spud_post_2000<-NA
# ks_wells_and_block_groups$permit_spud_post_2007<-NA
# ks_wells_and_block_groups$permit_spud_post_2010<-NA
# 
# # post 2000
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, permit_spud_post_2000[permit_after_2000 == "yes" | spud_after_2000 == "yes"] <- 'yes')
# ks_wells_and_block_groups$permit_spud_post_2000[is.na(ks_wells_and_block_groups$permit_spud_post_2000)] <- "no"
# 
# # post 2007
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, permit_spud_post_2007[permit_after_2007 == "yes" | spud_after_2007 == "yes"] <- 'yes')
# ks_wells_and_block_groups$permit_spud_post_2007[is.na(ks_wells_and_block_groups$permit_spud_post_2007)] <- "no"
# 
# # post 2010
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, permit_spud_post_2010[permit_after_2010 == "yes" | spud_after_2010 == "yes"] <- 'yes')
# ks_wells_and_block_groups$permit_spud_post_2010[is.na(ks_wells_and_block_groups$permit_spud_post_2010)] <- "no"
# 
# 
# # categories by which to subset: swd_inj_ci, other, permit or spud after 2000, current_active
# 
# # make categories manually
# ks_wells_and_block_groups$swd_norm_post2000_active<-NA
# ks_wells_and_block_groups$swd_norm_post2000_pa<-NA
# ks_wells_and_block_groups$swd_norm_pre2000_active<-NA
# ks_wells_and_block_groups$swd_norm_pre2000_pa<-NA
# 
# ks_wells_and_block_groups$swd_other_post2000_active<-NA
# ks_wells_and_block_groups$swd_other_post2000_pa<-NA
# ks_wells_and_block_groups$swd_other_pre2000_active<-NA
# ks_wells_and_block_groups$swd_other_pre2000_pa<-NA
# 
# ks_wells_and_block_groups$inj_norm_post2000_active<-NA
# ks_wells_and_block_groups$inj_norm_post2000_pa<-NA
# ks_wells_and_block_groups$inj_norm_pre2000_active<-NA
# ks_wells_and_block_groups$inj_norm_pre2000_pa<-NA
# ks_wells_and_block_groups$inj_other_post2000_active<-NA
# ks_wells_and_block_groups$inj_other_post2000_pa<-NA
# ks_wells_and_block_groups$inj_other_pre2000_active<-NA
# ks_wells_and_block_groups$inj_other_pre2000_pa<-NA
# 
# ks_wells_and_block_groups$ci_other_post2000_active<-NA
# ks_wells_and_block_groups$ci_other_post2000_pa<-NA
# ks_wells_and_block_groups$ci_other_pre2000_active<-NA
# ks_wells_and_block_groups$ci_other_pre2000_pa<-NA
# 
# 
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_norm_post2000_active[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active']<-'yes')
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_norm_post2000_pa[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned']<-'yes')
# # ks_wells_and_block_groups$swd_norm_pre2000_active
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_norm_pre2000_active[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active']<-'yes')
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_norm_pre2000_pa[swd_inj_ci == 'swd' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned']<-'yes')
# 
# 
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_other_post2000_active[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active']<-'yes')
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_other_post2000_pa[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned']<-'yes')
# # ks_wells_and_block_groups$swd_norm_pre2000_active
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_other_pre2000_active[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active']<-'yes')
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, swd_other_pre2000_pa[swd_inj_ci == 'swd' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned']<-'yes')
# 
# 
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_norm_post2000_active[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active']<-'yes')
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_norm_post2000_pa[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned']<-'yes')
# # ks_wells_and_block_groups$inj_norm_pre2000_active
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_norm_pre2000_active[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active']<-'yes')
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_norm_pre2000_pa[swd_inj_ci == 'inj' & other == 'no' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned']<-'yes')
# 
# 
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_other_post2000_active[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active']<-'yes')
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_other_post2000_pa[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned']<-'yes')
# # ks_wells_and_block_groups$inj_norm_pre2000_active
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_other_pre2000_active[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active']<-'yes')
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, inj_other_pre2000_pa[swd_inj_ci == 'inj' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned']<-'yes')
# 
# 
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, ci_other_post2000_active[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'presumed_active']<-'yes')
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, ci_other_post2000_pa[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'yes' & current_active == 'plugged_abandoned']<-'yes')
# # ks_wells_and_block_groups$swd_norm_pre2000_active
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, ci_other_pre2000_active[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'presumed_active']<-'yes')
# ks_wells_and_block_groups <- within(ks_wells_and_block_groups, ci_other_pre2000_pa[swd_inj_ci == 'c1' & other == 'yes' & permit_spud_post_2000 == 'no' & current_active == 'plugged_abandoned']<-'yes')
# 
# View(ks_wells_and_block_groups)
# 
# 
# swd_norm_post2000_active<-aggregate(swd_norm_post2000_active~GEOID_Data,ks_wells_and_block_groups,length)
# swd_norm_post2000_pa<-aggregate(swd_norm_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
# swd_norm_pre2000_active<-aggregate(swd_norm_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
# swd_norm_pre2000_pa<-aggregate(swd_norm_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
# 
# swd_other_post2000_active<-aggregate(swd_other_post2000_active~GEOID_Data,ks_wells_and_block_groups,length)
# swd_other_post2000_pa<-aggregate(swd_other_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length) # no data in this one
# swd_other_pre2000_active<-aggregate(swd_other_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
# swd_other_pre2000_pa<-aggregate(swd_other_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
# 
# inj_norm_post2000_active<-aggregate(inj_norm_post2000_active~GEOID_Data,ks_wells_and_block_groups,length)
# inj_norm_post2000_pa<-aggregate(inj_norm_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
# inj_norm_pre2000_active<-aggregate(inj_norm_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
# inj_norm_pre2000_pa<-aggregate(inj_norm_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
# inj_other_post2000_active<-aggregate(inj_other_post2000_active~GEOID_Data,ks_wells_and_block_groups,length) # no data in this one
# inj_other_post2000_pa<-aggregate(inj_other_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length) # no data in this one
# inj_other_pre2000_active<-aggregate(inj_other_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
# inj_other_pre2000_pa<-aggregate(inj_other_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
# 
# ci_other_post2000_active<-aggregate(ci_other_post2000_active~GEOID_Data,ks_wells_and_block_groups,length) # no data
# ci_other_post2000_pa<-aggregate(ci_other_post2000_pa~GEOID_Data,ks_wells_and_block_groups,length) # no data
# ci_other_pre2000_active<-aggregate(ci_other_pre2000_active~GEOID_Data,ks_wells_and_block_groups,length)
# ci_other_pre2000_pa<-aggregate(ci_other_pre2000_pa~GEOID_Data,ks_wells_and_block_groups,length)
# 
# # merge them all
# ks_well_counts<-Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "GEOID_Data", all = TRUE),
#        list(
#          swd_norm_post2000_active,
#          swd_norm_post2000_pa,
#          swd_norm_pre2000_active,
#          swd_norm_pre2000_pa,
#          
#          swd_other_post2000_active,
#          swd_other_pre2000_active,
#          swd_other_pre2000_pa,
#          
#          inj_norm_post2000_active,
#          inj_norm_post2000_pa,
#          inj_norm_pre2000_active,
#          inj_norm_pre2000_pa,
#          inj_other_pre2000_active,
#          inj_other_pre2000_pa,
#          
#          ci_other_pre2000_active,
#          ci_other_pre2000_pa   
#        ))
# View(ks_well_counts)
# 
# sum(colSums(ks_well_counts[2:16],na.rm = TRUE)) # equaled 19,995 by some miracle
# 
# # rename GEOID_Data to GEOID in well data
# ks_well_counts$GEOID<-ks_well_counts$GEOID_Data
# save(ks_well_counts,file="ks_well_counts.rdata")
# load(file="ks_well_counts.rdata")
# 
# # load ACS variables
# load(file="ACS_constructed_variables.rdata")
# View(ACS_constructed_variables[,201:291])
# 
# # limit ks block groups to ks
# ks_acs<-ACS_constructed_variables[which(ACS_constructed_variables$STATEFP=='20'),]
# save(ks_acs,file="ks_acs.rdata")
# load(file="ks_acs.rdata")
# 
# # merge well counts and acs!  hey-o!
# KS_FINAL_DATASET<-merge(ks_well_counts, ks_acs, by = "GEOID", all = TRUE)
# View(KS_FINAL_DATASET)
# save(KS_FINAL_DATASET,file="KS_FINAL_DATASET.rdata")
# write.csv(KS_FINAL_DATASET,file="KS_FINAL_DATASET.csv")




#### Analyses! ####

# # load files if starting from here
# load(file = "KS_FINAL_DATASET.rdata")
# ks_analysis_dataset<-KS_FINAL_DATASET
# View(ks_analysis_dataset)
# 
# #### swd well block groups only ####
# 
# # make variable for sum of swd wells per block group
# ks_analysis_dataset$any_swd_sum<-NA
# ks_analysis_dataset$any_swd_sum<-rowSums(ks_analysis_dataset[,c("swd_norm_post2000_active","swd_norm_post2000_pa","swd_norm_pre2000_active","swd_norm_pre2000_pa","swd_other_post2000_active","swd_other_pre2000_active","swd_other_pre2000_pa")], na.rm=TRUE)
# 
# # make new binary variable for have versus not have swd
# ks_analysis_dataset$any_swd_binary<-NA
# ks_analysis_dataset<-within(ks_analysis_dataset, any_swd_binary[any_swd_sum == 0]<-'no')
# ks_analysis_dataset<-within(ks_analysis_dataset, any_swd_binary[any_swd_sum > 0]<-'yes')
# ks_analysis_dataset$any_swd_binary
# 
# names(ks_analysis_dataset)
# 
# #### analysis by presence and absence of swd ####
# # number of block groups with and without swd wells
# table(ks_analysis_dataset$any_swd_binary)
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
# #### Poisson
# poisson_model<-glm(formula = any_swd_sum ~ median_household_income_B19013 + median_household_value_B25077 + percent_white_B03002 + population_density_B01001_ALAND + percent_high_school_plus_B15003 + median_age_B01002, family = "poisson", data = ks_analysis_dataset)
# summary(poisson_model)
# 
# 
# write.csv(ks_analysis_dataset,file="ks_analysis_dataset.csv")