rm(list=ls())
gc(reset = TRUE)
#------------------
library(readxl)
library(readr)
library(writexl)
library(tidyREDCap)
library(redcapAPI)
library(pak)

#Connect to Import Instrument---------------------------------------------------
pak::pak("keyring")
keyring::key_set("lfstudy_key")

tidyREDCap::import_instruments(
  url = "https://redcap.noguchi.ug.edu.gh/api/", 
  token = keyring::key_get("lfstudy_key")
)

#To enter your computer’s password to access the key.---------------------------
rcon <- redcapAPI::redcapConnection(
  url = 'https://redcap.noguchi.ug.edu.gh/api/', 
  token = keyring::key_get("lfstudy_key")
)

redcap <- redcapAPI::exportRecords(rcon)
#
saveRDS(redcap, file = "LF_study.rds")
write_xlsx(redcap, "LF_study.xlsx")
###
#===============================================================================
setwd("C:/Users/User/OneDrive - University of Ghana/moyussif@NMIMR/NMIMR/4NMIMRdocumnt/mod_r/modibo")
LFparticipants <- read_excel("LF_study.xlsx")
###
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(psych)
library(car)
library(lessR)
library(plotrix)
library(ggfortify)
##
str(LFparticipants)
print(LFparticipants)

#Barchart
consent <-ggplot(data = LFparticipants,
                 mapping = aes(x = consent))+
  geom_bar()+
  theme_classic()
consent

#stacked Barchart
kee_malariaTested <-ggplot(data = LFparticipants,
                           mapping = aes(x = keea_facilities, fill = malaria_t_results))+
  geom_bar()+
  theme_classic()
kee_malariaTested
##
Gomoa_malariaTested <-ggplot(data = LFparticipants,
                             mapping = aes(x = gomoa_west_facilities, fill = malaria_t_results))+
  geom_bar()+
  theme_classic()
Gomoa_malariaTested
#Histogram
Age_MalariaPositive <-ggplot(data = LFparticipants,
                             mapping = aes(x = age, fill = malaria_t_results))+
  geom_histogram(bins = 10, position = "dodge")+
  theme_classic()  
Age_MalariaPositive
#Boxplot
Age_gender <- ggplot(data = LFparticipants,
                     mapping = aes(x = sex, y = age,fill = malaria_t_results))+
  geom_boxplot()+
  theme_classic()
Age_gender

#Histogram with dodge
Age_QFATPositive <-ggplot(data = LFparticipants,
            mapping = aes(x = age, fill = qfat_test_results))+
  geom_histogram(bins = 10, position = "dodge")+
theme_classic()  
Age_QFATPositive








#=======================================
library(rsconnect)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
#===============================

source("app.R")
