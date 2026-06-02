rm(list=ls())
gc(reset = TRUE)

#-------------------------------------------------------------------------------
library(pak)
library(tidyREDCap)
library(redcapAPI)
library(readxl)
library(readr)
library(writexl)

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

#save RDS(redcap, file = "LF_study.rds")
write_xlsx(redcap, "LF_study.xlsx")

#Set Working Directorate--------------------------------------------------------
setwd("C:/Users/User/OneDrive - University of Ghana/moyussif@NMIMR/NMIMR/4NMIMRdocumnt/mod_r/modibo")

LFdata <- read_excel("LF_study.xlsx")

###