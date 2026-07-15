#
rm(list=ls())
gc(reset = TRUE)
#-------------------------------------------------------------------------------
........................ Analysis of Flu data .................................
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(psych)
library(car)
library(lessR)
library(FSA)
library(Hmisc)
library(stats)
library(epitools)
library(gtsummary)
library(lubridate)
#  
FLU100 <- read_excel("C:/Users/User/Desktop/FLU100.xlsx")
str(FLU100)  
#
--------------------------------------------------------------------------------
                          Create Age group
--------------------------------------------------------------------------------
#
  df <- FLU100 %>%
  mutate(age_group = case_when(
    AGE < 5 ~ "0-4",
    AGE < 11 ~ "5-10",
    AGE < 18 ~ "11-17",
    AGE < 35 ~ "18-34",
    AGE < 50 ~ "35-49",
    AGE < 65 ~ "50-64",
    TRUE ~ "65+"
  ))

str(df)
print(df)
#
--------------------------------------------------------------------------------
                          Data Conversion  
--------------------------------------------------------------------------------
#
df$DATE_ONSET <- as.Date(df$DATE_ONSET, format = "%Y/%m/%d")
#
df$WEEK <- isoweek(df$DATE_ONSET)#weeks start on Monday
df$WEEKdiff <- as.numeric(difftime(df$DATE_ONSET,min(df$DATE_ONSET, na.rm = TRUE),
                                   units = "weeks"))#Number of weeks since a reference date
#
df$SEX <- as.factor(df$SEX)
#df$AGE <-as.integer(df$AGE)# For whole Numbers 
#df$age_group <- factor(df$age_group, exclude=NULL)
df$AGE <-as.numeric(df$AGE)# For Decimal Numbers
df$age_group <- as.factor(df$age_group)
df$REGION <- as.factor(df$REGION)
df$District <- as.factor(df$District)  
df$ILI_SARI <- as.factor(df$ILI_SARI) 
df$FEVER <- as.factor(df$FEVER)  
df$COUGH <- as.factor(df$COUGH)  
df$THROAT <- as.factor(df$THROAT)
df$CORYZA <- as.factor(df$CORYZA) 
df$MYALGIA <- as.factor(df$MYALGIA)
df$HEADACHE <- as.factor(df$HEADACHE)
df$BREATH_DIFFICULTY <- as.factor(df$BREATH_DIFFICULTY)
df$FLUMATRIX <- as.factor(df$FLUMATRIX)
# 
str(df)
print(df)
print(df, n = Inf, width = Inf)
#
summary(df)
describe.by(df)
#
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--------------------------------------------------------------------------------
######################## Table - Descriptive Statistics  #######################
table1 <- df %>%
  select(ILI_SARI, AGE, SEX, age_group, REGION, District, FLUMATRIX) %>%
  tbl_summary(
    by = ILI_SARI,
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "ifany"
  ) %>%
  add_p() %>%          # Adds p-values comparing groups
  add_overall() %>%    # Adds an Overall column
  bold_labels()

table1
#
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
########################## PREVALENCE BY AGE GROUP #############################
plot_data <- df %>%
  filter(!is.na(FLUMATRIX), !FLUMATRIX %in% c("SARI", "NEG")) %>%
  count(SEX, age_group, FLUMATRIX) %>%
  group_by(SEX, age_group) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(age_group = fct_reorder(age_group, n, .fun = sum, .desc = TRUE))

ggplot(plot_data, aes(x = age_group, y = n, fill = FLUMATRIX)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = paste0(n, "\n(", sprintf("%.1f%%", percent), ")")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3.5
  ) +
  facet_wrap(~ SEX) +
  labs(
    title = "ILI Cases by Age Group and Sex",
    x = "Age Group",
    y = "Number of Cases",
    fill = "Case Type"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  expand_limits(y = max(plot_data$n) + 5)

# # # 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############################ PREVALENCE BY REGION ##############################
plot_data <- df %>%
  filter(!is.na(FLUMATRIX), !FLUMATRIX %in% c("SARI", "NEG")) %>%
  count(SEX, REGION, FLUMATRIX) %>%
  group_by(SEX, REGION) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(REGION = fct_reorder(REGION, n, .fun = sum, .desc = TRUE))

ggplot(plot_data, aes(x = REGION, y = n, fill = FLUMATRIX)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = paste0(n, "\n(", sprintf("%.1f%%", percent), ")")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3.5
  ) +
  facet_wrap(~ SEX) +
  labs(
    title = "ILI Cases by Region and Sex",
    x = "Region",
    y = "Number of Cases",
    fill = "Case Type"
  ) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  expand_limits(y = max(plot_data$n) + 5)

# # # 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ############################## Weekly trends ###################################
##----------------#######-----------------------------
plot_data <- df %>%
  filter(!is.na(FLUMATRIX), !FLUMATRIX %in% c("SARI", "NEG")) %>%
  count(SEX, WEEK, FLUMATRIX) %>%
  group_by(SEX, WEEK) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()

# Calculate total weekly cases for trend line
trend_data <- plot_data %>%
  group_by(SEX, WEEK,FLUMATRIX) %>%
  summarise(total_cases = sum(n), .groups = "drop")

ggplot(plot_data, aes(x = WEEK, y = n, fill = FLUMATRIX)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_line(
    data = trend_data,
    aes(x = WEEK, y = total_cases, group = SEX),
    color = "black",
    linewidth = 1
  ) +
  geom_point(
    data = trend_data,
    aes(x = WEEK, y = total_cases),
    color = "black",
    size = 2
  ) +
  geom_text(
    aes(label = paste0(n, "\n(", sprintf("%.1f%%", percent), ")")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3
  ) +
  facet_wrap(~ SEX) +
  labs(
    title = "Weekly ILI Cases by Sex with Trend Line",
    x = "Epidemiological Week",
    y = "Number of Cases",
    fill = "Case Type"
  ) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(breaks = unique(plot_data$WEEK)) +
  expand_limits(y = max(trend_data$total_cases) + 5)
########$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
plot_data <- df %>%
  filter(!is.na(FLUMATRIX), !FLUMATRIX %in% c("SARI", "NEG")) %>%
  count(SEX, WEEK, FLUMATRIX) %>%
  group_by(SEX, WEEK) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()

ggplot(plot_data, aes(x = WEEK, y = n, color = FLUMATRIX, group = FLUMATRIX)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ SEX) +
  labs(
    title = "Weekly ILI Cases Trend by Sex",
    x = "Epidemiological Week",
    y = "Number of Cases",
    color = "Case Type"
  ) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(breaks = unique(plot_data$WEEK))



# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                         Time Series Analysis                                 +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse)
library(ggplot2)
library(tseries)
library(forecast)
library(readxl)
library(readr)
#-------------------------------------------------------------------------------

Converting date to time series
#STEP1====tdata$Date = as.Date(tdata$Date, format = "%Y/%m/%d") 
#STEP2====hhdata = ts(tdata$attendance,start = min(tdata$Date), end = max(tdata$Date),frequency = 1)
#STEP3====class(hhdata)  
#-------------------------------------------------------------------------------

Monthly data
#1-month_data=ts(tdata$attendance, start = min(tdata$Date),end = max(tdata$Date),frequency = 12)
#2-monthly <- ts(tdata$attendance, start = 2015, frequency = 12)
#3-monthly = ts(tdata$attendance, start = c(2015,3),end = c(2022, 12),frequency = 12)
Quarterly data 
#1-qtr_data=ts(tdata$attendance, start = min(tdata$Date),end = max(tdata$Date),frequency = 4)
#2-quarterly <- ts(ttdata$registrants, start = 2015, frequency = 4)
#3-qtrly = ts(tdata$attendance, start = c(2015,3),end = c(2022, 12),frequency = 4)
Yearly data 
#1-yr_data=ts(tdata$attendance, start = min(tdata$Date),end = max(tdata$Date),frequency = 1)
#2-yearly <- ts(ttdata$registrants, start = 2015, frequency = 1)
#3-yrly = ts(tdata$attendance, start = c(2015,3),end = c(2022, 12),frequency = 1)
--------------------------------------------------------------------------------
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
tdata <- read_excel("CTrends.xlsx")
cnfm <- read_csv("cnfm.csv")
View(cnfm)
class(cnfm)
boxplot(ParasitePresence~Date, data = cnfm)
--------------------------------------------------------------------------------
  #To control//make the variance Equal
  log(cnfm$ParasitePresence)
plot(log(tdata$attendance))  
#To control//make the mean Equal
plot(diff(log(tdata$attendance)))

#convert data to time series
tsdata=ts(tdata$attendance, start = min(tdata$Date),end = max(tdata$Date),frequency = 1)
class(tsdata)
view(tsdata)
plot(tsdata)

#check to determine stationarity of data 
acf(tsdata)      #step1----autocorrelation
pacf(tsdata)     #step2----partial autocorrelation
adf.test(tsdata) #step3----augmented Dickey-fuller test

#Convert Non_stationary to Stationary---------(seasonal arima model)
tsdata_model=auto.arima(tsdata, ic = "aic", trace = TRUE)
tsdata_model
tsdisplay(residuals(tsdata_model), lag.max = 45, main = "(0,0,0) Model residuals" )
#Check for stationary again
acf(ts(tsdata_model$residuals))
pacf(ts(tsdata_model$residuals))

#Now perform forecast for stationary data-----(seasonal arima model)
mydataforecast=forecast(tsdata_model, level = c(95),h=5*4)
mydataforecast
plot(mydataforecast)
autoplot(mydataforecast)

#Non seasonal ARIMA-------------------------------------------------
nsdata_model=auto.arima(tsdata, seasonal = FALSE)
nsdata_model
tsdisplay(residuals(nsdata_model), lag.max = 45, main = "(0,0,0) Model residuals" )
non_seasonal = forecast(nsdata_model)
plot(non_seasonal)

#Now perform forecast for stationary data-----(seasonal arima model)
mysecondforecast=forecast(nsdata_model, level = c(95),h=5*4)
mysecondforecast
plot(mysecondforecast)


#Evaluate model (seasonal model)
Box.test(tsdata_model$residuals, lag = 5,type = "Ljung-Box")
Box.test(tsdata_model$residuals, lag = 15,type = "Ljung-Box")
Box.test(tsdata_model$residuals, lag = 30,type = "Ljung-Box")
#alternate the lag values until the P.values is > 0.05  ---- indicate No further autocorrelation



# # #

=========================== Time series Using ggplot2 ==========================
#------------------------------------------------------------------------------
library(readxl)
library(scales)
library(ggplot2)
library(ggpmisc)
#------------------------------------------------------------------------------
setwd("C:/Users/User/OneDrive - University of Ghana/myComputer@space/repos")
tdata <- read_excel("CTrends.xlsx")
View(tdata)
#convert date to time series
tdata$Date = as.Date(tdata$Date, format = "%Y/%m/%d")  

# line and Points
ggplot(tdata, aes(x = Date, y = attendance)) +
  geom_line()

#peaks
ggplot(tdata, aes(x = Date, y = attendance)) +
  geom_line()+
  stat_peaks(geom = "point", span = 15, color = "steelblue3", size = 2) +  
  stat_peaks(geom = "label", span = 15, color = "steelblue3", angle = 0,
             hjust = -0.1, x.label.fmt = "%Y-%m-%d") +
  stat_peaks(geom = "rug", span = 15, color = "blue", sides = "b")

#Valleys  
ggplot(tdata, aes(x = Date, y = attendance)) +
  geom_line()+
  stat_valleys(geom = "point", span = 11, color = "red", size = 2)+   
  stat_valleys(geom = "label", span = 11, color = "red", angle = 0,
               hjust = -0.1, x.label.fmt = "%Y-%m-%d")+
  stat_valleys(geom = "rug", span = 11, color = "red", sides = "b")

#break midpoint
ggplot(tdata, aes(x = Date, y = attendance)) +  
  geom_point()+
  geom_vline(xintercept = as.Date(tdata$Date, format = "%Y/%m/%d"),
             linetype = 2, color = 2, linewidth = 1)

# Facet wrap for multiple lines  
fw <-ggplot(tdata, aes(x = Date, y = attendance)) +  
  geom_line() +
  facet_wrap(~Date)
fw

# Facet grid for multiple lines
fg <-ggplot(tdata, aes(x = Date, y = attendance)) +  
  geom_line() +
  facet_grid(attendance~Date)
fg


  
  
  