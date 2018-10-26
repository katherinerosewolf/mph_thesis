#trying to use big memory options

setwd("C:/Rfiles/MPHProject/workingdata/acsdata/")

library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(acs)
library(ff)
library(bigmemory)
library(ffbase)
library(data.table)

sequenceindex<-1:122
fixedwidthsequenceindexff<-formatC(sequenceindex, width = 4, format = "d", flag = "0")
listofstates<-c('ak', 'al', 'ar', 'az', 'ca', 'co', 'ct', 'dc', 'de', 'fl', 'ga', 'hi', 'ia', 'id', 'il', 'in', 'ks', 'ky', 'la', 'ma', 'md', 'me', 'mi', 'mn', 'mo', 'ms', 'mt', 'nc', 'nd', 'ne', 'nh', 'nj', 'nm', 'nv', 'ny', 'oh', 'ok', 'or', 'pa', 'pr', 'ri', 'sc', 'sd', 'tn', 'tx', 'ut', 'va', 'vt', 'wa', 'wi', 'wv', 'wy')


#TESTING BIG DATA
#making master list of ffdfs from csv files
input_filenames<-list.files(pattern = ".*\\.txt")

#remove the us file
input_filenames<-input_filenames[!grepl("us", input_filenames)]
input_filenames

#trying ffdf
start.time <- Sys.time()
master_list_of_ffs<-lapply(input_filenames,function(i){
  read.csv.ffdf(file=i, header=FALSE)
})

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#running to here

names(master_list_of_ffdfs) <- gsub("\\.txt$", "", input_filenames)




#making one file for each sequence and putting the sequence files in a list
listofsequenceffdfs<-list()
for (j in testfixwidthnoofseq){
  interimlistofffnames<-master_list_of_ffdfs[grep(j, names(master_list_of_ffdfs))]
  interimseqff <- Reduce(function(x,y) ffdfappend(x,y, adjustvmode=F), interimlistofffnames)
  listofsequenceffdfs[[j]]<-interimseqff
}

listofsequenceffdfs