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
#---------------------------------------------------
#Import Packages
library(flexdashboard)
library(knitr)
library(rsconnect)
library(readxl)
library(readr)
library(tidyverse)
library(rpivotTable)
library(ggplot2)
library(plotly)
library(dplyr)
library(patchwork)
library(openintro)
library(highcharter)
library(gtsummary)
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
  tbl_summary(include = c(consent,site, sex, keea_communities, keea_facilities, malaria_t_results, fts_test_result)) %>%   
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
  tbl_summary(include = c(gomoa_west_communities, gomoa_west_facilities, malaria_t_results, fts_test_result)) %>%   
  # add table captions
  as_gt() %>%
  gt::tab_header(title = "Table 1. Test Results by Community and CHPs",
                 subtitle = " Since May,2026")

GomoaWest_table

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
    fill = fts_test_result
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
    fill = fts_test_result
  )
) +
  geom_bar(na.rm = TRUE) +
  scale_fill_manual(
    values = c("#4682B4", "#EE7600")
  ) +
  theme_classic() +
  coord_flip()

GomoaWest_QFAT

```