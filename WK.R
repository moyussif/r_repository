
##---------------------------------------------------##
rm(list=ls())
objects()
ls()
search()
##---------------------------------------------------##
library(XLConnect)
library(lubridate)
library(joineR)
library(lattice)
library(latticeExtra)
library(zscorer)
library(DescTools)
library(tidyverse)
library(ggpubr)
library(COUNT)
library(reshape2)
search()

library(reshape2)
library(ggExtra)
library(systemfonts)
library(forcats)
search()
objects(grep("forcats", search()))
library(finalfit)


Sys.time()
Sys.setenv(TZ='GMT')
Sys.time()



###########################################################################################################################
##-----------------------------------------------------------------------------------------------------------------------##
##                                   
##-----------------------------------------------------------------------------------------------------------------------##
###########################################################################################################################

dir()
data.read<-loadWorkbook(file.choose(), create=T) ## 
data.read

DATA<-readWorksheet(data.read, sheet = "Sheet1")
str(DATA)

str(Data_For_Training)
##-----------------------------------------------------------------------------------------------------------------------##
DataTraining <- read_excel("C:/Users/User/OneDrive - University of Ghana/moyussif@NMIMR/DeskDown/repos/Mentorship/Data For Training.xlsx")
str(DataTraining)




str(DATA)
colSums(is.na(DATA))

data.frame(table(DATA[,"BARCODES"]))
summary(DATA[,"Age"])
hist(DATA[,"Age"], main="", xlab="AGE", col="grey45")
hist(DATA[,"Age"], main="", xlab="AGE", col="red2", breaks=1705)


data.frame(table(DataTraining[,"Blood Mf"]))

data.frame(table(Data_For_Training[,"DDTD Oncho biplex...7"]))

subset(Data_For_Training, is.na(Sex), select=c("BARCODES","Age","Sex","District"))



DataTraining <- read_excel("C:/Users/User/OneDrive - University of Ghana/moyussif@NMIMR/DeskDown/repos/Mentorship/Data For Training.xlsx")
str(DataTraining)

names(DataTraining)[6]<- "GADX_Oncho"
names(DataTraining)[7]<- "DDTD_Ocho"

subset(DataTraining,GADX_Oncho=="Negative"&DDTD_Ocho=="Negative", select=c("BARCODES","Age","Sex","District","DDTD_Ocho","GADX_Oncho")) %>% 
 print()

