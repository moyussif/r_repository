rm(list=ls())
gc(reset = TRUE)
updateR()
library(installr)
#-----------------------

library(tidyREDCap)
library(pak)
pak::pak("keyring")

keyring::key_set("lfstudy_key")

tidyREDCap::import_instruments(
  url = "https://redcap.noguchi.ug.edu.gh/api/", 
  token = keyring::key_get("lfstudy_key")
)


#In the future, your operating system may ask you to enter your computer’s password to access the key.
library(redcapAPI)
rcon <- redcapAPI::redcapConnection(
  url = 'https://redcap.noguchi.ug.edu.gh/api/', 
  token = keyring::key_get("lfstudy_key")
)

redcap <- redcapAPI::exportRecords(rcon)

saveRDS(redcap, file = "lfstudy.rds")