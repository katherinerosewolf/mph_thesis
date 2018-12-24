#Oil and Gas Disposal Well Analysis
#Katherine Wolf
#January 2018

# setup
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)

setwd("C:/Rfiles/MPHProjectR/workingdata/")

# finaldata<-read.csv('WolfMPHProjectFullDataset.csv')

# table(finaldata$Production.Type,finaldata$State)


welldata<-read.csv("WellsTableDisposalInjectionSWInjection.csv")
View(welldata)
length(unique(welldata$API14))
welldataforjoin<-welldata[c("API14","Surface.Hole.Longitude..WGS84.","Surface.Hole.Latitude..WGS84.")]
View(welldataforjoin)
write.csv(welldataforjoin,"welldataforjoin.csv")


wellACSjoin<-read.csv("updatedwelldatajoinedtoACS2015.txt")
ACSdata<-read.csv("ACS2015dataexport.txt")
ACSmetadata<-read.csv("ACS2015metadata.txt")
welldataprejoin<-read.csv("WellsTableDisposalInjectionSWInjection.csv")
welldataforjoin<-read.csv("welldataforjoin.csv")

#length(unique(welldataforjoin$API14))
#length(unique(welldataprejoin$API14))

#test<-merge(x=welldataforjoin,y=welldataprejoin,by="API14")

save(wellACSjoin,file="wellACSjoin.rdata")
save(ACSdata,file="ACSdata.rdata")
save(ACSmetadata,file="ACSmetadata.rdata")

#load(file="wellACSjoin.rdata")
#load(file="ACSdata.rdata")
#load(file="ACSmetadata.rdata")

View(ACSmetadata)
View(ACSdata)

#colnames(ACSdata[1:18])

#Variables to pull from ACS Data
selectACSdata<-ACSdata[c("OBJECTID","STATEFP","COUNTYFP","TRACTCE","BLKGRPCE","GEOID","NAMELSAD","MTFCC","FUNCSTAT","ALAND","AWATER","INTPTLAT","INTPTLON","Shape_Length","Shape_Area","GEOID_Data","B00001e1","B03002e1","B03002e2","B03002e3","B03002e4","B03002e5","B03002e6","B03002e7","B03002e8","B03002e9","B03002e10","B03002e11","B03002e12","B03002e13","B03002e14","B03002e15","B03002e16","B03002e17","B03002e18","B03002e19","B03002e20","B03002e21","B01001e1","B01001e2","B01001e3","B01001e4","B01001e5","B01001e6","B01001e7","B01001e8","B01001e9","B01001e10","B01001e11","B01001e12","B01001e13","B01001e14","B01001e15","B01001e16","B01001e17","B01001e18","B01001e19","B01001e20","B01001e21","B01001e22","B01001e23","B01001e24","B01001e25","B01001e26","B01001e27","B01001e28","B01001e29","B01001e30","B01001e31","B01001e32","B01001e33","B01001e34","B01001e35","B01001e36","B01001e37","B01001e38","B01001e39","B01001e40","B01001e41","B01001e42","B01001e43","B01001e44","B01001e45","B01001e46","B01001e47","B01001e48","B01001e49","B01002e1","B01002e2","B01002e3","B00001e1","B03002e1","B03002e2","B03002e3","B03002e4","B03002e5","B03002e6","B03002e7","B03002e8","B03002e9","B03002e10","B03002e11","B03002e12","B03002e13","B03002e14","B03002e15","B03002e16","B03002e17","B03002e18","B03002e19","B03002e20","B03002e21","B15003e1","B15003e2","B15003e3","B15003e4","B15003e5","B15003e6","B15003e7","B15003e8","B15003e9","B15003e10","B15003e11","B15003e12","B15003e13","B15003e14","B15003e15","B15003e16","B15003e17","B15003e18","B15003e19","B15003e20","B15003e21","B15003e22","B15003e23","B15003e24","B15003e25","B25077e1","B00001e1","B03002e1","B03002e2","B03002e3","B03002e4","B03002e5","B03002e6","B03002e7","B03002e8","B03002e9","B03002e10","B03002e11","B03002e12","B03002e13","B03002e14","B03002e15","B03002e16","B03002e17","B03002e18","B03002e19","B03002e20","B03002e21","B25075e1","B25075e2","B25075e3","B25075e4","B25075e5","B25075e6","B25075e7","B25075e8","B25075e9","B25075e10","B25075e11","B25075e12","B25075e13","B25075e14","B25075e15","B25075e16","B25075e17","B25075e18","B25075e19","B25075e20","B25075e21","B25075e22","B25075e23","B25075e24","B25075e25","B25075e26","B25075e27","B00001e1","B03002e1","B03002e2","B03002e3","B03002e4","B03002e5","B03002e6","B03002e7","B03002e8","B03002e9","B03002e10","B03002e11","B03002e12","B03002e13","B03002e14","B03002e15","B03002e16","B03002e17","B03002e18","B03002e19","B03002e20","B03002e21","B20001e1","B20001e2","B20001e3","B20001e4","B20001e5","B20001e6","B20001e7","B20001e8","B20001e9","B20001e10","B20001e11","B20001e12","B20001e13","B20001e14","B20001e15","B20001e16","B20001e17","B20001e18","B20001e19","B20001e20","B20001e21","B20001e22","B20001e23","B20001e24","B20001e25","B20001e26","B20001e27","B20001e28","B20001e29","B20001e30","B20001e31","B20001e32","B20001e33","B20001e34","B20001e35","B20001e36","B20001e37","B20001e38","B20001e39","B20001e40","B20001e41","B20001e42","B20001e43","B20002e1","B20002e2","B20002e3","B19013e1","B19301e1","C17002e1","C17002e2","C17002e3","C17002e4","C17002e5","C17002e6","C17002e7","C17002e8","B23025e1","B23025e1","B23025e2","B23025e3","B23025e4","B23025e5","B23025e6","B23025e7")]

#View resulting dataframe with selected ACS variables
View(selectACSdata)

#join well data back with spatial join data
ACSwithfullwells<-merge(x = welldataprejoin, y = wellACSjoin, by = "API14", all.x = TRUE)

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
