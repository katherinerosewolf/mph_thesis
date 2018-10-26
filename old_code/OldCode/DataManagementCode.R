#Data Management Class
#Katherine Wolf
#July 2017

# setup
library(dplyr)
library(data.table)
library(ggplot2)

setwd("C:/Rfiles/DataManagement")




#import raw permit data 2017-07-06 (Permits, then Well Type of Disposal, Injection, or Saltwater Injection)
permitdata<-read.csv("PermitsThenWellTypeDisposalInjectionSWIn.csv", header = TRUE)
View(permitdata)


#count wells per state
wellsperstate<-permitdata %>% group_by(State.Province) %>% summarize(count=n())
View(wellsperstate)


#count wells per state per type
wellsperstatepertype<-permitdata %>% group_by(State.Province, Well.Type) %>% summarize(count=n())
View(wellsperstatepertype)


write.table(wellsperstate, file="wellsperstate.csv",sep=",",row.names=F)
write.table(wellsperstatepertype, file="wellsperstatepertype.csv",sep=",",row.names=F)



#import raw permit data 2017-07-06 (Permits, then Well Type of Disposal, Injection, or Saltwater Injection)
permitdataTX<-read.csv("TexasMiniDataPermits.csv", header = TRUE)
View(permitdataTX)




#count wells per state per type
wellsperstatepertypeTX<-permitdataTX %>% group_by(Well.Type) %>% summarize(count=n())
View(wellsperstatepertypeTX)
