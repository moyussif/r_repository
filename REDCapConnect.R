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
library(readxl)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(psych)
library(car)
library(lessR)
library(plotrix)
library(ggfortify)
library(plotly)
##
LFdata <- read_excel("LF_participant.xlsx")

print(LFdata)
#------------------------------------------------------------------------------
#Donut chart
plot_ly(LFdata, labels = ~consent, values = ~record_id, type = "pie",
        hole = 0.5)%>%
  layout(title = " Consented  Vs Not consented")

#Pie chart
plot_ly(LFdata, labels = ~sex, values = ~record_id, type = "pie",
        hole = 0.0)%>%
  layout(title = " Male  Vs Female")

#------------------------------------------------------------------------------
  
#------------------------------------------------------------------------------



#Barchart
consent <-ggplot(data = LFparticipants,
                 mapping = aes(x = consent))+
  geom_bar()+
  theme_classic()
consent

#stacked Barchart
kee_malariaTested <-ggplot(data = LFdata,
                           mapping = aes(x = keea_facilities, fill = malaria_t_results),na.rm = TRUE)+
  geom_bar()+
  theme_classic()+
  coord_flip()
kee_malariaTested

##
Gomoa_malariaTested <-ggplot(data = LFdata,
                             mapping = aes(x = gomoa_west_facilities, fill = malaria_t_results),na.rm = TRUE)+
  geom_bar()+
  theme_classic()+
  coord_flip()
Gomoa_malariaTested
#Histogram
Age_MalariaPositive <-ggplot(data = LFdata,
                             mapping = aes(x = age, fill = malaria_t_results))+
  geom_histogram(bins = 10, position = "dodge")+
  theme_classic()  
Age_MalariaPositive
#Boxplot
Age_gender <- ggplot(data = LFdata,
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






setwd("C:/Users/User/OneDrive - University of Ghana/moyussif@NMIMR/NMIMR/4NMIMRdocumnt/mod_r/modibo/LF_Dashboard")

#=======================================
library(readxl)
library(rsconnect)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
library(gt)
library(flexdashboard)
#===============================
install.packages("knitr")
install.packages("rpivotTable")
install.packages("openintro")
install.packages("gtsummary")

# Force-compile the latest stable repository build
devtools::install_github("smartinsightsfromdata/rpivotTable", force = TRUE)
library(rpivotTable)

pkgbuild::has_build_tools(debug = TRUE)

install.packages("devtools")

devtools::install_github(
  "smartinsightsfromdata/rpivotTable",
  force = TRUE
)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5555
---
  title: "yussif dashboard"
output: 
  flexdashboard::flex_dashboard:
  theme:
  bg: "#101010"
fg: "#FDF7F7" 
primary: "#ED79F9"
base_font:
  google: Prompt
code_font:
  google: JetBrains Mono
orientation: columns
vertical_layout: fill
---
  
  
  ```{r setup, include=FALSE}
library(flexdashboard)
# Install thematic and un-comment for themed static plots (i.e., ggplot2)
# thematic::thematic_rmd()
#-------------------------------------------------------------------------------
#Import Packages
library(flexdashboard)
library(knitr)
library(rsconnect)
library(readxl)
library(readr)
library(rpivotTable)
library(ggplot2)
library(plotly)
library(dplyr)
library(openintro)
library(highcharter)
library(DT)
library(gt)
#
```

```{r}
#Import Data

LF_participant <- read_excel("LF_participant.xlsx")

```

Row 
---------------------------------
  ##Test by Community and Facility
  
  ```{r}
library(gtsummary)
# KEEA_table


KEEA_table <- 
  LF_participant %>%
  tbl_summary(include = c(consent, site, keea_communities, keea_facilities, malaria_t_results, qfat_test_result)) %>%   
  # add table captions
  as_gt() %>%
  gt::tab_header(title = "Table 1. Test Results by Community and CHPs",
                 subtitle = " Since May,2026")

KEEA_table

```


```{r}
#GomoaWest_table

GomoaWest_table <- 
  LF_participant %>%
  tbl_summary(include = c(gomoa_west_communities, gomoa_west_facilities, malaria_t_results, qfat_test_result)) %>%   
  # add table captions
  as_gt() %>%
  gt::tab_header(title = "Table 1. Test Results by Community and CHPs",
                 subtitle = " Since May,2026")

GomoaWest_table

```


Row
-----------------------------------
  ### Basic Distribution
  
  ```{r}
#Donut chart

plot_ly(LF_participant, labels = ~consent, values = ~record_id, type = "pie",
        hole = 0.5)%>%
  layout(title = " Consented  Vs Not consented")


#Pie chart

plot_ly(LF_participant, labels = ~sex, values = ~record_id, type = "pie",
        hole = 0.0)%>%
  layout(title = " Male  Vs Female")

```


Row
-------------------------------------------------------------------------------
  ### Malaria Test Results 
  
  ```{r}
#KEEA_Malaria Results
kee_Malaria <- ggplot(
  data = LF_participant,
  aes(
    x = keea_facilities,
    fill = malaria_t_results
  )
) +
  geom_bar(na.rm = TRUE) +
  scale_fill_manual(
    values = c("#4682B4", "#EE7600")
  ) +
  theme_classic() +
  coord_flip()

kee_Malaria

#GomoaWest_malaria

GomoaWest_malaria <- ggplot(
  data = LF_participant,
  aes(
    x = gomoa_west_facilities,
    fill = malaria_t_results
  )
) +
  geom_bar(na.rm = TRUE) +
  scale_fill_manual(
    values = c("#4682B4", "#EE7600")
  ) +
  theme_classic() +
  coord_flip()

GomoaWest_malaria



```        


Row
------------------------------------------------------------------------------
  ### QFAT Test Results
  
  ```{r}
#KEEA_QFAT
kee_QFAT <- ggplot(
  data = LF_participant,
  aes(
    x = keea_facilities,
    fill = qfat_test_result
  )
) +
  geom_bar(na.rm = TRUE) +
  scale_fill_manual(
    values = c("#4682B4", "#EE7600")
  ) +
  theme_classic() +
  coord_flip()

kee_QFAT


#GomoaWest_QFAT

GomoaWest_QFAT <- ggplot(
  data = LF_participant,
  aes(
    x = gomoa_west_facilities,
    fill = qfat_test_result
  )
) +
  geom_bar(na.rm = TRUE) +
  scale_fill_manual(
    values = c("#4682B4", "#EE7600")
  ) +
  theme_classic() +
  coord_flip()

GomoaWest_QFAT

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  install.packages("Rcpp")

packageVersion("Rcpp")
library(Rcpp)
  
install.packages("ggiraph")
library(ggiraph)
  # create interactive bar chart
  library(rCharts)
GW_QFAT = subset(as.data.frame(HairEyeColor), 
                       Sex == "Male")
n1 <- nPlot(Freq ~ Hair, 
            group = 'Eye', 
            data = hair_eye_male, 
            type = 'multiBarChart'
)
n1$set(width = 600)
n1$show('iframesrc', cdn=TRUE)  

```
install.packages("ggiraph", dependencies = TRUE)

install.packages("fontLiberation")
library(ggiraph)

devtools::install_github('davidgohel/ggiraph')



