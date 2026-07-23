#
rm(list=ls())
gc(reset = TRUE)
#................
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
library(writexl)
#Import Data
setwd("C:/Users/User/Desktop")
LFdata <- read_excel("LF_July_22.xlsx")
str(LFdata)

##.............................
df <- LFdata %>% mutate(Age_group = case_when(
  age < 10 ~ "<10",
  age >= 10 & age <= 19 ~ "10-19",
  age >= 20 & age <= 29 ~ "20-29",
  age >= 30 & age <= 40 ~ "30-40",
  age > 40 ~ ">40",
  TRUE ~ NA_character_))
#----
prev <- df %>% group_by(site, sex, Age_group,keea_communities, gomoa_west_communities) %>%
  summarise( Prev = mean(fts_test_result== "Positive", na.rm = TRUE) * 100,
    .groups = "drop")

#----
prev_sex_age_plot <- prev %>% filter(!is.na(Age_group),!is.na(sex),!is.na(site),!is.na(Prev))


#      sink("Prevalence_results.doc")


# Plot--------------------Keea_communities--------------------------------------
df <- LFdata %>% 
  mutate(
    Age_group = case_when(
      age < 10 ~ "<10",
      age >= 10 & age <= 19 ~ "10-19",
      age >= 20 & age <= 29 ~ "20-29",
      age >= 30 & age <= 40 ~ "30-40",
      age > 40 ~ ">40",
      TRUE ~ NA_character_
    )
  )
prev_sex_age_plot <- prev %>% 
  filter(
    !is.na(Age_group),
    !is.na(sex),
    !is.na(keea_communities),
    !is.na(Prev),
    Prev > 0
  )
#
p1 <- ggplot(prev_sex_age_plot,
       aes(Age_group, Prev, fill = sex)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = sprintf("%.1f%%", Prev)),
    position = position_dodge(width = 0.8),
    vjust = 1.0,
    size = 3.5
  ) +
  facet_wrap(~keea_communities) +
  scale_fill_manual(
    values = c(
      "Male" = "#1F77B4",
      "Female" = "#E377C2"
    )
  ) +
  labs(
    x = "Age group",
    y = "Prevalence (%)",
    fill = "Sex"
  ) +
  theme_grey()

# Plot--------------------Gomoa_West_communities--------------------------------
df <- LFdata %>% 
  mutate(
    Age_group = case_when(
      age < 10 ~ "<10",
      age >= 10 & age <= 19 ~ "10-19",
      age >= 20 & age <= 29 ~ "20-29",
      age >= 30 & age <= 40 ~ "30-40",
      age > 40 ~ ">40",
      TRUE ~ NA_character_
    )
  )
prev_sex_age_plot <- prev %>% 
  filter(
    !is.na(Age_group),
    !is.na(sex),
    !is.na(gomoa_west_communities),
    !is.na(Prev),
    Prev > 0
  )
#
p2 <- ggplot(prev_sex_age_plot,
       aes(Age_group, Prev, fill = sex)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = sprintf("%.1f%%", Prev)),
    position = position_dodge(width = 0.8),
    vjust = 1.0,
    size = 3.5
  ) +
  facet_wrap(~gomoa_west_communities) +
  scale_fill_manual(
    values = c(
      "Male" = "#1F77B4",
      "Female" = "#E377C2"
    )
  ) +
  labs(
    x = "Age group",
    y = "Prevalence (%)",
    fill = "Sex"
  ) +
  theme_grey()
#

dIp <-p1 / p2
ggsave("dIp.png", dIp, width = 8, height = 6)


library(patchwork)

dIp <- (p1 / p2) 

dIp

#==========================================================================================

plot_prev <- function(data, facet_var) {
  data %>%
    filter(
      !is.na(Age_group),
      !is.na(sex),
      !is.na(.data[[facet_var]]),
      !is.na(Prev),
      Prev > 0
    ) %>%
    ggplot(aes(
      x = Age_group,
      y = Prev,
      fill = sex
    )) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_text(
      aes(label = sprintf("%.1f%%", Prev)),
      position = position_dodge(width = 0.8),
      vjust = 1,
      size = 3.5
    ) +
    facet_wrap(as.formula(paste("~", facet_var))) +
    scale_fill_manual(
      values = c(
        "Male" = "#1F77B4",
        "Female" = "#E377C2"
      )
    ) +
    labs(
      x = "Age group",
      y = "Prevalence (%)",
      fill = "Sex"
    ) +
    theme_grey()
}

p1 <- plot_prev(prev, "keea_communities")
p2 <- plot_prev(prev, "gomoa_west_communities")

p1 / p2

# # #


#============================================================================================
## KEEA_Municipal - Malaria & QFAT {width = 100%}
#KEEA_Municipal
D

## Gomoa.West_District-Malaria & QFAT {width = 100%}
#Gomoa.West_District
community_table <- LFdata %>%
  filter(site != "KEEA-Municipal") %>%
  group_by(site, gomoa_west_communities) %>%
  summarise(
    Malaria_Tested = n(),
    Malaria_Positives = sum(malaria_t_results == "Positive", na.rm = TRUE),
    Malaria_prevalence = paste0(round(100 * Malaria_Positives / Malaria_Tested, 1), "%"),
    FTS_Tested = sum(!is.na(fts_test_result)),
    FTS_Positives = sum(fts_test_result == "Positive", na.rm = TRUE),
    FTS_prevalence = paste0(round(100 * FTS_Positives / FTS_Tested, 1), "%"),
    .groups = "drop"
  ) %>%
  select(
    site,
    gomoa_west_communities,
    Malaria_Tested,
    Malaria_Positives,
    Malaria_prevalence,
    FTS_Tested,
    FTS_Positives,
    FTS_prevalence
  ) %>%
  arrange(desc(FTS_Positives))

kable(community_table, align = "c")

ggsave("community_table.png", community_table)
# Stop sending output to the document
#     sink()
