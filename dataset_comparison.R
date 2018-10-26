# quick data comparison R
# Katherine Wolf
# October 26, 2018



# setup
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(psych)
library(dataCompareR)

getwd()

# functions
`%notin%` <- function(x,y) !(x %in% y) # define "not-in" function

#### data import ####

load(file="ks_wells_2018_09_28.rdata") # import new data
load(file="ks_wells_2018_07_02.rdata") # import old data

ks_wells_for_comparison_2018_09_28 <- ks_wells_2018_09_28
ks_wells_for_comparison_2018_07_02 <- ks_wells_2018_07_02

compare_them <- rCompare(ks_wells_for_comparison_2018_09_28, ks_wells_for_comparison_2018_07_02, keys = c("KID"))
summary(compare_them)
