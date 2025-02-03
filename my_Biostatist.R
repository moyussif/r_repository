rm(list=ls())
gc(reset = TRUE)
#--------------------------- required packages _________________________________
library(readxl)
library(readr)
library(skimr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(dbplyr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(stringr)
library(gtools)
library(gganimate)
library(zoo)
library(mvtnorm)
library(devtools)
library(rvest)
library(dslabs)
library(weights)
library(VennDiagram)
library(rafalib)
library(MASS)
library(broom)
library(matrixStats)
library(gridExtra)
library(randomForest)
library(tree)
library(splitstackshape)
library(egg)
library(rpart)
library(rpart.plot)
library(testthat)
library(scales)
library(htmlwidgets)
library(pdftools)
library(survival)
library(epitools)
library(epiDisplay)

getwd()
#------------------------Import functions------------------------------------
# import .csv file
child_data1 <- read.csv(file = 'children_data.csv') 
# import .csv file with dec = "," and sep = ";"
child_data2 <- read.csv2(file = 'children_data.csv')
#correct by introducing sep = ","
child_data22 <- read.csv2(file = 'children_data.csv', sep = ",")
# import tab delim file with sep = "\t"
child_data3 <- read.delim(file = 'children_data.txt') 

#If you want your variables to be factors (categorical) instead, 
#you can use the argument `stringsAsFactors = TRUE
  child_data1 <- read.csv("children_data.csv", stringsAsFactors = TRUE)
  
str(child_data1)

child<-read.csv(file.choose())
#------------------------Other import options--------------------------------
library(readr)
# import white space delimited files
all_data1 <- read_table(file = 'children_data.txt', col_names = TRUE)
# import comma delimited files
all_data2 <- read_csv(file = 'children_data.csv')
# import tab delimited files
all_data3 <- read_delim(file = 'children_data.txt', delim = "\t")
# or use
all_data4 <- read_tsv(file = 'children_data.txt' )

# Computing summary statistics
health_data<-read.csv("health_data.csv")
head(health_data
summary(health_data)
mean(health_data$age)
mean(health_data$BMI)
median(health_data$age)
median(health_data$BMI)
min(health_data$age)
max(health_data$age)
sd(health_data$age)
var(health_data$age)
quantile(health_data$age)
IQR(health_data$age)

### Data Wrangling
The processes of *transforming* or *manipulating* raw data into a useful format

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
head(murders)

### Adding a column with `mutate`
murders <- mutate(murders, murder_rate = total / population * 100000)
### Subsetting with `filter`
filter(murders, murder_rate <= 0.71)
### Selecting columns with `select`
new_table <- select(murders, state, region, murder_rate)
filter(new_table, murder_rate <= 0.71)

### The pipe: `%>%`
murders %>% select(state, region, murder_rate) %>% filter(murder_rate <= 0.71)

### Summarize
murders %>% summarize(avg = mean(murder_rate))

#To compute the country average murder rate using the `summarize`function, 
#we can do the following: 
us_murder_rate <- murders %>% 
  summarize(murder_rate = sum(total) / sum(population) * 100000)

us_murder_rate

### Using the dot to access the piped data 
us_murder_rate <- murders %>% 
  summarize( murder_rate = sum(total) / sum(population) * 100000) %>%
  .$murder_rate

us_murder_rate

### Group then summarize
murders %>% 
  group_by(region) %>%
  summarize(median_rate = median(murder_rate))

#we get a table with the median murder rate for each of the four regions.

### Sorting data tables
murders %>% 
  arrange(population) %>% 
  head()
#states by murder rate, from smallest to largest, we arrange by `murder_rate` instead:
  murders %>% 
  arrange(murder_rate) %>% 
  head()

#if we want to sort the table in descending order we can type
murders %>% 
  arrange(desc(murder_rate)) %>% 
  head()

# Nested Sorting
#Here we order by `region then within region we order by murder rate:
  murders %>% 
  arrange(region, murder_rate) %>% 
  head()
#### Rename
names(murders)

murders <- murders %>% rename(abbreviation = abb)
names(murders)

#The syntax inside the `rename()` function is new name`=`old name.

#Calculate some descriptive statistics using the `dplyr` functions.

stats<-health_data%>%group_by(sex)%>%
  summarise(mean_age=mean(age), 
            mean_bmi=mean(BMI), 
            mean_heart.rate=mean(heart_rate),
            sd_age=sd(age), sd_bmi=sd(BMI), 
            sd_heart.rate=sd(heart_rate) )
stats

stats<-health_data%>%group_by(sex)%>%
  summarise(mean_age=mean(age), 
            mean_bmi=mean(BMI), 
            mean_heart.rate=mean(heart_rate),
            sd_age=sd(age), sd_bmi=sd(BMI), 
            sd_heart.rate=sd(heart_rate) )%>%
  .$mean_age

stats

## Basic plots

## Scatter plots
health_sub<-health_data%>%filter(age>25)
age25<-health_sub$age
cholesterol25<-health_sub$cholesterol
bmi25<-health_sub$BMI
plot(age25, cholesterol25)

#For a quick plot that avoids accessing variables twice, we can use the `with` function
with(health_sub, plot(age, cholesterol))
# Histograms
hist(health_data$exercise_score_hrs)

# Boxplot
boxplot(exercise_score_hrs~ sex, data = health_sub)

#==============================================================================#
#---------------------------------ggplot2 -------------------------------------#

# Data Visualization and Exploratory Data Analysis

library(tidyverse)
library(dslabs)
library(ggthemes)
library(ggrepel)
data(murders)
head(murders)

r <- murders %>% 
  summarize(pop = sum(population), tot = sum(total)) %>% 
  mutate(murder_rate = tot/pop*10^6) %>% .$murder_rate

murders %>% ggplot(aes(x = population/10^6, y = total, label = abb)) +  
  geom_abline(intercept = log10(r), lty=2, col="darkgrey") +
  geom_point(aes(color=region), size = 3) +
  geom_text_repel() + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

We are reminded of the saying "a picture is worth a thousand words".
Data visualization provides a powerful way to communicate a data-driven finding. 

#One limitation is that ggplot is - 
# designed to work exclusively with data tables in which rows are obs & columns are variables.

```{r}
library(dslabs)
data(murders)
```

```{r  ggplot-example-plot, echo=FALSE}
library(ggthemes)
library(ggrepel)

r <- murders %>% 
  summarize(pop=sum(population), tot=sum(total)) %>% 
  mutate(murder_rate= tot/pop*10^6) %>% .$murder_rate

murders %>% ggplot(aes(x = population/10^6, y = total, label = abb)) +  
  geom_abline(intercept = log10(r), lty=2, col="darkgrey") +
  geom_point(aes(color=region), size = 3) +
  geom_text_repel() + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name="Region") +
  theme_economist()
```

#==============================================================================#
#----------------------------Data Visualization--------------------------------#

# Data Visualization Principles

```{r piechart, echo=FALSE}
library(ggthemes)
p1 <- browsers %>% ggplot(aes(x = "", y = Percentage, fill = Browser)) +
  geom_bar(width = 1, stat = "identity", col = "black")  + coord_polar(theta = "y") +
  theme_excel() + xlab("") + ylab("") +
  theme(axis.text=element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid  = element_blank()) +
  facet_grid(.~Year)
p1
```
#represent quantities with both areas and angles is proportional to the quantity it represents. 

#The donut chart is an example of a plot that uses only area: 

```{r donutchart, echo=FALSE}
browsers %>% ggplot(aes(x = 2, y = Percentage, fill = Browser)) +
  geom_bar(width = 1, stat = "identity", col = "black")  + 
  scale_x_continuous(limits=c(0.5,2.5)) + coord_polar(theta = "y") +
  theme_excel() + xlab("") + ylab("") +
  theme(axis.text=element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid  = element_blank()) +
  facet_grid(.~Year)

```
  
#"Note: Pie charts are a very bad way of displaying information. 
#The eye is good at judging linear measures and bad at judging relative areas. 
#A bar chart or dot chart is a preferable way of displaying this type of data."

p2 <-browsers %>%
  ggplot(aes(Browser, Percentage)) + 
  geom_bar(stat = "identity", width=0.5, fill=4, col = 1) +
  ylab("Percent using the Browser") +
  facet_grid(.~Year)
grid.arrange(p1, p2, nrow = 2)

  ```{r, eaco = FALSE, warning = FALSE, message=FALSE, echo=FALSE}

  library(scales)
browsers <- filter(browsers, Year == 2015)
at <- with(browsers, 100 - cumsum(c(0,Percentage[-length(Percentage)])) - 0.5*Percentage)  
label <- percent(browsers$Percentage/100)
browsers %>% ggplot(aes(x = "", y = Percentage, fill = Browser)) +
  geom_bar(width = 1, stat = "identity", col = "black")  + coord_polar(theta = "y") +
  theme_excel() + xlab("") + ylab("") + ggtitle("2015") +
  theme(axis.text=element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid  = element_blank()) +
  annotate(geom = "text", 
           x = 1.62, 
           y =  at, 
           label = label, size=4)
```

### Know when to include 0

When using barplots it is dishonest not to start the bars at 0. 
This is because, by using a barplot, we are implying the length is proportional to the quantities being displayed. 
By avoiding 0, relatively small differences can be made to look much bigger than they actually are. 
This approach is often used by politicians or media organizations trying to exaggerate a difference.

Here is the appropriate plot: 
  
  ```{r, echo=FALSE, fig.height=4}
data.frame(date = c("Now", "Jan 1, 2013"), tax_rate = c(35, 39.6)) %>%
  mutate(date = reorder(date, tax_rate)) %>%
  ggplot(aes(date, tax_rate)) + ylab("") + xlab("") +
  geom_bar(stat = "identity", fill = "yellow", col = "black", width = 0.5) + 
  ggtitle("Top Tax Rate If Bush Tax Cut Expires")
```

When using position rather than length, it is **not** necessary to include 0. 
e.g when we want to compare differences between groups relative to the variability seen within the groups.

### Do not distort quantities
  
  ```{r, echo = FALSE, fig.height=4.5}
gdp <- c(14.6, 5.7, 5.3, 3.3, 2.5)
gdp_data <- data.frame(Country = rep(c("United States", "China", "Japan", "Germany", "France"),2),
                       y = factor(rep(c("Radius","Area"),each=5), levels = c("Radius", "Area")),
                       GDP= c(gdp^2/min(gdp^2), gdp/min(gdp))) %>% 
  mutate(Country = reorder(Country, GDP))
gdp_data %>% 
  ggplot(aes(Country, y, size = GDP)) + 
  geom_point(show.legend = FALSE, color = "blue") + 
  scale_size(range = c(2,30)) +
  coord_flip() + ylab("") + xlab("")
```

### Order by a meaningful value

To appreciate how the right order can help convey a message, 
suppose we want to create a plot to compare the murder rate across states. 
We are particularly interested in the most dangerous and safest states. 
Note the difference when we order alphabetically (the default) versus when we order by the actual rate: 
  
  ```{r, eval=FALSE}
data(murders)
p1 <- murders %>% mutate(murder_rate = total / population * 100000) %>%
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("")

p2 <- murders %>% mutate(murder_rate = total / population * 100000) %>%
  mutate(state = reorder(state, murder_rate)) %>%
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("")
grid.arrange(p1, p2, ncol = 2)
```

```{r, echo=FALSE}
data(murders)
p1 <- murders %>% mutate(murder_rate = total / population * 100000) %>%
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("")

p2 <- murders %>% mutate(murder_rate = total / population * 100000) %>%
  mutate(state = reorder(state, murder_rate)) %>%
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("")
grid.arrange(p1, p2, ncol = 2)
```

Note that the `reorder` function lets us reorder groups as well. 


### Show the data


### Consider transformations
We have motivated the use the log transformation in cases where the changes are multiplicative. 
Population size was an example in which we found a log transformation to yield a more informative transformation.
The combination of incorrectly using barplots when a log transformation is merited can be particularly distorting.

As an example, consider this barplot showing the average population sizes for each continent in 2015:

```{r, echo=FALSE, fig.height=4.5, message=FALSE}
data(gapminder)
p1 <- gapminder %>% filter(year == 2015) %>%
  group_by(continent) %>% summarize(population = mean(population)) %>%
  mutate(continent = reorder(continent, population)) %>%
  ggplot(aes(continent, population/10^6)) + 
  geom_bar(stat = "identity", width=0.5, fill=4) +
  theme_excel() + 
  ylab("Population in Millions") +
  xlab("Continent")
p1
```


Other transformations you should consider are the logistic transformation, 
useful to better see fold changes in odds, and the square root transformation, useful for count data.

### Adjacent visual cues


### Think of the color blind

About 10% of the population is color blind. 
Unfortunately, the default colors used in ggplot are not optimal for this group. 
However, ggplot does make it easy to change the color palette used in plots. 

```{r, fig.height=3.5}
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", 
                               "#009E73", "#F0E442", "#0072B2", 
                               "#D55E00", "#CC79A7")
p1 <- data.frame(x=1:8, y=1:8, col = as.character(1:8)) %>% 
  ggplot(aes(x, y, color = col)) + geom_point(size=5)
p1 + scale_color_manual(values=color_blind_friendly_cols)
```
### Slope charts

One exception where another type of plot may be more informative is
when you are comparing variables of the same type 
but at different time points and for a relatively small number of comparisons.
For example, comparing life expectancy between 2010 and 2015. 
In this case we might recommend a **slope chart**. 
There is not a geometry for slope chart in ggplot2 but we can construct one using `geom_lines`. 
We need to do some tinkering to add labels.

Here is a comparison for large western countries: 

```{r,eval=FALSE}
west <- c("Western Europe","Northern Europe","Southern Europe",
          "Northern America","Australia and New Zealand")
dat <- gapminder %>% 
  filter(year%in% c(2010, 2015) & region %in% west & 
           !is.na(life_expectancy) & population > 10^7) 
dat %>%
  mutate(location = ifelse(year == 2010, 1, 2), 
         location = ifelse(year == 2015 & 
         country%in%c("United Kingdom","Portugal"), 
         location+0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), 
            show.legend = FALSE) +
  xlab("") + ylab("Life Expectancy")
```


```{r,echo=FALSE}
west <- c("Western Europe","Northern Europe","Southern Europe",
          "Northern America","Australia and New Zealand")
dat <- gapminder %>% 
  filter(year%in% c(2010, 2015) & region %in% west & 
           !is.na(life_expectancy) & population > 10^7) 
dat %>%
  mutate(location = ifelse(year == 2010, 1, 2), 
         location = ifelse(year == 2015 & 
         country%in%c("United Kingdom","Portugal"), 
         location+0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), 
            show.legend = FALSE) +
  xlab("") + ylab("Life Expectancy")
```

An advantage of the slope chart is that it permits us to quickly get an idea of changes based on the slope of the lines.
Note that we are using **angle** as the visual cue. But we also have position to determine the exact values.
Comparing the improvements is a bit harder with a scatter plot:

```{r, echo=FALSE}
library(ggrepel)
west <- c("Western Europe","Northern Europe","Southern Europe",
          "Northern America","Australia and New Zealand")

dat <- gapminder %>% 
  filter(year%in% c(2010, 2015) & region %in% west & 
           !is.na(life_expectancy) & population > 10^7) 

dat %>% 
   mutate(year = paste0("life_expectancy_", year)) %>%
   select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>% 
   ggplot(aes(x=life_expectancy_2010,y=life_expectancy_2015, label = country)) + geom_point() + geom_text_repel() +
  scale_x_continuous(limits=c(78.5, 83)) +
  scale_y_continuous(limits=c(78.5, 83)) +
  geom_abline(lty = 2) +
  xlab("2010") + ylab("2015")
```

### Use common charts

Note that in the scatter plot we have followed the principle _use common axes_ 
since we are comparing these before and after. 
However, if we have many points the slope charts stop being useful as it becomes hard to see each line.

### Bland-Altman plot

Since we are interested in the difference, it makes sense to dedicate one of our axes to it.
The Bland-Altman plot, also known as the Tukey mean-difference plot and the MA-plot, 
shows the difference versus the average: 

```{r, echo=FALSE}
library(ggrepel)
dat %>% 
   mutate(year = paste0("life_expectancy_", year)) %>%
   select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>% 
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) + 
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") + ylab("Difference between 2015 and 2010")
```

Here we quicky see which countries have improved the most as it is represented by the y-axis.
We also get an idea of the overall value from the x-axis.

### Encoding a third variable

We previously showed a scatter plot showing the relationship between infant survival and average income.
Here is a version of this plot where we encode three variables: OPEC membership, region, and population: 

```{r, echo=FALSE}
present_year <- 2010

dat <- gapminder %>%
  mutate(region = case_when(
    region %in% west ~ "The West",
    region %in% "Northern Africa" ~ "Northern Africa",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region == "Southern Asia"~ "Southern Asia",
    region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"),
    dollars_per_day = gdp / population / 365) %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(region) ) %>%
  mutate(OPEC = ifelse(country %in% opec, "Yes", "No")) 

dat %>% 
  ggplot(aes(dollars_per_day, 1 - infant_mortality/1000, 
             col = region, size = population/10^6,
             shape =  OPEC)) +
  scale_x_continuous(trans = "log2", limits=c(0.25, 150)) +
  scale_y_continuous(trans = "logit",limit=c(0.875, .9981),
                     breaks=c(.85,.90,.95,.99,.995,.998)) + 
  geom_point(alpha = 0.5) 

```


Note that we encode categorical variables with color, hue, and shape. 
The shape can be controlled with the `shape` argument. 
Below are the shapes available for use in R. Note that for the last five, the color goes inside. 


```{r, echo=FALSE}
dat2=data.frame(x=c(0:25))
ggplot() +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
scale_shape_identity() + scale_y_reverse() +
geom_point(dat2, mapping=aes(x%%9, x%/%9, shape=x), size=10, fill="blue") +
geom_text(dat2, mapping=aes(x%%9, x%/%9+0.25, label=x), size=6) 
```

The default shape values are a circle and a triangle for OPEC membership. 
We can manually customize these by adding the layer `scale_shape_manual(values = c(8, 10))`, 
where 8 and 10 are the numbers of the desired shapes from the list above.

```{r}
dat %>% 
  ggplot(aes(dollars_per_day, 1 - infant_mortality/1000, 
             col = region, size = population/10^6,
             shape =  OPEC)) +
  scale_x_continuous(trans = "log2", limits=c(0.25, 150)) +
  scale_y_continuous(trans = "logit",limit=c(0.875, .9981),
                     breaks=c(.85,.90,.95,.99,.995,.998)) + 
  geom_point(alpha = 0.5) +
  scale_shape_manual(values = c(8, 10))
```

For continuous variables we can use color, intensity or size. 
We now show an example of how we do this with a case study.

## Case Study: Vaccines 

Vaccines have helped save millions of lives. 
In the 19th century, before herd immunization was achieved through vaccination programs, 
deaths from infectious diseases, like smallpox and polio, were common. However, today, 
despite all the scientific evidence for their importance, vaccination programs have become somewhat controversial.

```{r}
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)
```

We create a temporary object `dat` that stores only the Measles data, includes a per 100,000 rate, 
orders states by average value of disease and removes Alaska and Hawaii since they only became states in the late 1950s. 

```{r}
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
       filter(!state %in% c("Hawaii","Alaska") & disease == the_disease) %>%
       mutate(rate = (count / weeks_reporting) * 52 / (population / 100000)) 
```


Can we show data for all states in one plot? We have three variables to show: year, state and rate. 

When choosing colors to quantify a numeric variable we chose between two options: *sequential* and *diverging*.
Sequential colors are suited for data that goes from high to low. High values are clearly distinguished from low values.
Here are some examples offered by the package `RColorBrewer`:

```{r}
library(RColorBrewer)
display.brewer.all(type = "seq")
```

Diverging colors are used to represent values that diverge from a center. 
We put equal emphasis on both ends of the data range: higher than the center and lower than the center.

```{r}
library(RColorBrewer)
display.brewer.all(type="div")
```

#==============================================================================#
#-----------------------Hypothesis Testing ------------------------------------#


## Hypothesis Testing with Behavioral Risk Factor Surveillance System data(BRFSS)
As example data (*2020-brfss-survey-responses.csv*), from the CDC BRFSS.
#is a system of health-related telephone surveys that collect state data about U.S. residents 
#regarding their health-related risk behaviors, chronic health conditions, and use of preventive services(CDC, 2019).

Guidance on how to download and process this data directly from the CDC website
[CDC website](https://www.cdc.gov/brfss/annual_data/annual_2020.html)

*Import 2020-brfss-survey-responses.csv Data, *
  ```{r}
brfss2020<-read.csv("2020-brfss-survey-responses.csv")
```

Let explored the data
```{r}
str(brfss2020)
```

## Comparing Two Central Tendencies: Tests with Continuous / Discrete Data
### One Sample T-Test (Two-Sided)
The one-sample t-test tests the significance of the difference between the mean of a sample and an expected mean.

* Data: A continuous or discrete sampled variable and a single expected mean (μ)
* Parametric (normal distributions)
* R Function: t.test()
#* Null hypothesis (H0): The means of the sampled distribution matches the expected mean.

```{r}
weight.illinois <-brfss2020[brfss2020$WEIGHT2 %in% 50:776, ]

weight.illinois <- weight.illinois[weight.illinois$ST %in% "IL", ]

plot(density(weight.illinois$WEIGHT2), col="navy", lwd=3)

abline(v = 178, col="red", lwd=3)
```
Density plot of 2020 weight in Illinois vs. 2005 estimated North American mean.

We then performed the one sample t-test using `t.test()` function  from r base function. 
```{r}
weight.test = t.test(weight.illinois$WEIGHT2, mu=178)

print(weight.test)
```

### One Sample T-Test (One-Sided)
Because we were expecting an increase, we can modify our hypothesis that the mean weight in 2020 
is higher than the continental weight in 2005. We can perform a one-sided t-test using the _alternative="greater"_ parameter.

```{r}
weight.test = t.test(weight.illinois$WEIGHT2, mu=178, alternative="greater")

print(weight.test)
```
### Two-Sample T-Test
When comparing means of values from two different groups in your sample, a two-sample t-test is in order.

The two-sample t-test tests the significance of the difference between the means of two different samples.

1. Data:
  * Two normally-distributed, continuous or discrete sampled variables, OR
* A normally-distributed continuous or sampled variable and a parallel dichotomous variable indicating 
what group each of the values in the first variable belong to

2. Parametric (normal distributions)
3. R Function: t.test()
4. Null hypothesis (H0): The means of the two sampled distributions are equal.

Example, given the low incomes and delicious foods prevalent in Mississippi, 
we might presume that average weight in *Mississippi* would be higher than in *Illinois*.

```{r}
weight <- brfss2020[brfss2020$WEIGHT2 %in% 50:776,]

weight.illinois <- weight[weight$ST %in% "IL",]

weight.mississippi <- weight[weight$ST %in% "MS",]

plot(density(weight.illinois$WEIGHT2), col="navy", lwd=3)

lines(density(weight.mississippi$WEIGHT2), col="red", lwd=3)

legend("topright", legend=c("Illinois", "Mississippi"), col=c("navy", "red"), lwd=3)
```

#Density plots for weight in Mississippi (red) vs. Illinois (blue).

#We test a hypothesis that the mean weight in Illinois in 2020 is less than the 2020 mean weight in Mississippi.

weight.test = t.test(weight.illinois$WEIGHT2, weight.mississippi$WEIGHT2, 
                     alternative="less")
print(weight.test)

### Wilcoxen Rank Sum Test (Mann-Whitney U-Test)
The Wilcoxen rank sum test tests the significance of the difference between the means of two different samples.

1. Data: Two continuous sampled variables
2. Non-parametric (non-normal distributions)
3. R Function: wilcox.test()


```{r}
drinking <- brfss2020[brfss2020$AVEDRNK3 %in% 1:88, ]

drinking$AVEDRNK3[drinking$AVEDRNK3 == 88] <- 0

hist(drinking$AVEDRNK3)
```

Continuing the comparison of Illinois and Mississippi from above, 
we might presume that with all that warm weather and excellent food in Mississippi, 
they might be inclined to drink more.

```{r}
drinking.illlinois <- drinking[drinking$ST %in% "IL",]

drinking.mississippi <- drinking[drinking$ST %in% "MS",]

print(mean(drinking.illlinois$AVEDRNK3))

print(mean(drinking.mississippi$AVEDRNK3))
```

The means of average number of drinks per month seem to suggest that Mississippians do drink more than Illinoians.

#We can use `wilcox.test()` to test a hypothesis 
#that the average amount of drinking in Illinois is different than in Mississippi. 
#Like the t-test, the alternative can be specified as two-sided or one-sided, and 
#for this example we will test whether the sampled Illinois value is indeed less than the Mississippi value.

```{r}
drinking.test = wilcox.test(drinking.illlinois$AVEDRNK3, drinking.mississippi$AVEDRNK3, 
                            alternative="less")
print(drinking.test)
```

### Weighted Two-Sample T-Test
The downloadable BRFSS data is raw, a
nonymized survey data that is biased by uneven geographic coverage of survey administration (noncoverage) 
and lack of responsiveness from some segments of the population (nonresponse). 
The X_LLCPWT field (landline, cellphone weighting) is a weighting factor added by the CDC 
that can be assigned to each response to compensate for these biases.

The `wtd.t.test()` function from the _weights_ library has a weights parameter 
that can be used to include a weighting factor as part of the `t-test`.


```{r}
library(weights)

drinking = brfss2020[brfss2020$AVEDRNK3 %in% 1:88, ]

drinking$AVEDRNK3[drinking$AVEDRNK3 == 88] = 0

drinking.illinois = drinking[drinking$ST %in% "IL",]

drinking.mississippi = drinking[drinking$ST %in% "MS",]

drinking.test = wtd.t.test(x = drinking.illinois$AVEDRNK3, 
                           y = drinking.mississippi$AVEDRNK3,
                           weight=drinking.illinois$X_LLCPWT, 
                           weighty = drinking.mississippi$X_LLCPWT)

print(drinking.test)
```

## Comparing Proportions: Tests with Categorical Data

### Chi-Squared Goodness of Fit
Tests the significance of the difference between sampled frequencies of different values and expected frequencies of those values

* Data: A categorical sampled variable and a table of expected frequencies for each of the categories
* R Function: `chisq.test()`
* Null hypothesis (H0): The relative proportions of categories in one variable are different from the expected proportions


Example, we want to test a hypothesis that smoking rates changed between 2000 and 2020.

In 2000, the estimated rate of adult smoking in Illinois was 22.3% (Illinois Department of Public Health 2004).

#The variable we will use is *SMOKDAY2*: Do you now smoke cigarettes every day, some days, or not at all?
  
* 1: Current smoker - now smokes every day
* 2: Current smoker - now smokes some days
* 3: Not at all
* 7: Dont know
* 9: Refused
* NA: Not asked or missing - NA is used for people who have never smoked


#We will subset only yes/no responses in Illinois and convert into a dummy variable (yes = 1, no = 0).

```{r}
smoking <- brfss2020

smoking$SMOKDAY2 = ifelse(smoking$SMOKDAY2 %in% 1:2, 1, 0)

smoking.illinois = table(smoking[smoking$ST %in% "IL", "SMOKDAY2"])

print(smoking.illinois * 100 / sum(smoking.illinois))
```
#The listing of the table as percentages indicates that smoking rates were halved between 2000 and 2020, 
but since this is sampled data, 
#we need to run a chi-squared test to make sure the difference cant be explained by the randomness of sampling.

```{r}
smoking.test = chisq.test(smoking.illinois, p=c(0.777, 0.223))

print(smoking.test)
```

### Chi-Squared Contingency Analysis / Test of Independence
Tests the significance of the difference between frequencies between two different groups

* Data: Two categorical sampled variables
* R Function: `chisq.test()`
* Null hypothesis (H0): The relative proportions of one variable are independent of the second variable.

We can also compare categorical proportions between two sets of sampled categorical variables.

The *chi-squared test* is used to determine if two categorical variables are independent. 
What is passed as the parameter is a contingency table created with the `table()` function 
that cross-classifies the number of rows that are in the categories specified by the two categorical variables.

The null hypothesis with this test is that the two categories are independent. 
The alternative hypothesis is that there is some dependency between the two categories.

Example, we can compare the three categories of smokers (daily = 1, occasionally = 2, never = 3) 
across the two categories of states (Illinois and Mississippi).

```{r}
smoking <- brfss2020[brfss2020$SMOKDAY2 %in% c(1,2,3,NA),]

smoking$SMOKDAY2[is.na(smoking$SMOKDAY2)] <- 3

smoking.table = table(smoking[smoking$ST %in% c("IL", "MS"), c("ST", "SMOKDAY2")])

print(smoking.table)

```

```{r}
plot(smoking.table)
```
Plot of the contingency table comparing smoking responses between IL and MS.

```{r}
smoking.test = chisq.test(smoking.table)

print(smoking.test)
```

### Weighted Chi-Squared Contingency Analysis
As with the weighted t-test above, the weights library contains the `wtd.chi.sq()`
function for incorporating weighting into chi-squared contingency analysis. 

we will use the variable *X_LLCPWT* as the weights for the test.

```{r}
library(weights)

smoking <- brfss2020[brfss2020$SMOKDAY2 %in% c(1,2,3,NA),]

smoking$SMOKDAY2[is.na(smoking$SMOKDAY2)] <- 3

smoking <- smoking[smoking$ST %in% c("IL", "MS"),]

smoking.test <- wtd.chi.sq(var1 = smoking$ST, var2 = smoking$SMOKDAY2, 
                           weight = smoking$X_LLCPWT)

print(smoking.test)
```
## Comparing Categorical and Continuous Variables
### Analysis of Variation (ANOVA)
Analysis of Variance (ANOVA) is a test that you can use when you have a categorical variable and a continuous variable.
It is a test that considers variability between means for different categories
as well as the variability of observations within groups.

* Data: One or more categorical (independent) variables and one continuous (dependent) sampled variable
* Parametric (normal distributions)
* R Function: `aov()`
* Null hypothesis (H0): There is no difference in means of the groups defined by each level of the categorical (independent) variable

Example, we look at the continuous weight variable (WEIGHT2) split into groups by the eight income categories in INCOME2: 
Is your annual household income from all sources?
  
* 1: Less than $10,000
* 2: $10,000 to less than $15,000
* 3: $15,000 to less than $20,000
* 4: $20,000 to less than $25,000
* 5: $25,000 to less than $35,000
* 6: $35,000 to less than $50,000
* 7: $50,000 to less than $75,000)
* 8: $75,000 or more
* 77: Don’t know/Not sure
* 99: Refused
* NA: Not asked or Missing

The `barplot()` of means does show variation among groups, 
although there is no clear linear relationship between income and weight.
*Mean weight by income category in Illinois*
  ```{r}
weight.illinois <- brfss2020[(brfss2020$WEIGHT2 %in% 50:776) & 
                               (brfss2020$INCOME2 %in% 1:8) & 
                               (brfss2020$ST %in% "IL"),]

weight.list<- aggregate(weight.illinois$WEIGHT2, by=list(weight.illinois$INCOME2), FUN=mean)

x <- barplot(weight.list$x, names.arg=weight.list$Group.1, las=1)

text(x, 20, round(weight.list$x), cex=0.8)
```
To test whether this variation could be explained by randomness in the sample, we run the ANOVA test.

```{r}
model <- aov(WEIGHT2 ~ INCOME2, data = weight.illinois)

summary(model)
```



However, it gives us no clear model for describing that relationship and 
offers no insights into why income would affect weight, especially in such a nonlinear manner.

### Kruskal-Wallis One-Way Analysis of Variance
A somewhat simpler test is the *Kruskal-Wallis test* 
which is a nonparametric analogue to ANOVA for testing the significance of differences between three or more groups.

* Data: One or more categorical (independent) variables and one continuous (dependent) sampled variable
* Non-parametric (normal or non-normal distributions)
* R Function: `kruskal.test()`    
* Null hypothesis (H0): The samples come from the same distribution.

For this example, we will investigate whether mean weight varies between the three major US urban states: 
  New York, Illinois, and California.

```{r}
weight.urban = brfss2020[(brfss2020$WEIGHT2 %in% 50:776) &
                           (brfss2020$ST %in% c("NY","IL","CA")),]

boxplot(WEIGHT2 ~ ST, data = weight.urban)
```
To test whether this variation could be explained by randomness in the sample, we run the Kruskal-Wallis test.

```{r}
model_kw <- kruskal.test(WEIGHT2 ~ ST, data = weight.urban)

print(model_kw)
```
The low p-value leads us to reject the null hypothesis that the samples come from the same distribution. 
This corroborates the alternative hypothesis that mean weights differ based on state.

#==============================================================================#
#-----------------------Linear regression -------------------------------------#
Linear regression is one of the most common statistical analyses in medical and health sciences. 
Linear regression models the linear (i.e. straight line) relationship between:
  
- **outcome**: numerical variable (e.g. blood pressure, BMI, cholesterol level).
- **predictors/independent variables**: numerical variables and categorical variables (e.g. gender, race, education level).

In simple words, we might be interested in knowing the relationship between the cholesterol level and its associated factors, 
for example gender, age, BMI and lifestyle. 
This can be explored by a Linear regression\index{Linear regression} analysis.

## Linear regression models

Linear regression\index{Linear regression} 
is a type of Generalized linear models (GLMs)\index{Generalized linear model}\index{GLM}, 
which also includes other outcome types, for example categorical and count.
In subsequent chapters, 
we will cover these outcome types in form of logistic regression\index{Logistic regression} and
Poisson regression\index{Poisson regression}. 
Basically, the relationship between the outcome and 
predictors in a linear regression\index{Linear regression} is structured as follows,

$$\begin{aligned}
numerical\ outcome = &\ numerical\ predictors \\
& + categorical\ predictors
\end{aligned}$$
  
More appropriate forms of this relationship will explained later under simple and multiple linear regressions sections.

## Prepare R Environment for Analysis

### Libraries
For this chapter, we will be using the following packages:
  
- **foreign**\index{foreign}: for reading SPSS\index{SPSS} and STATA\index{STATA} datasets
- **tidyverse**\index{tidyverse}: a general and powerful package for data transformation
- **psych**\index{psych}: for descriptive statistics
- **gtsummary**\index{gtsummary}: for coming up with nice tables for results and plotting the graphs
- **ggplot2**\index{ggplot2}, **ggpubr**\index{ggpubr}, **GGally**\index{GGally}: for plotting the graphs
- **rsq**\index{rsq}: for getting $R^2$ value from a GLM model
- **broom**\index{broom}: for tidying up the results
- **car**\index{car}: for `vif()` function

These are loaded as follows using the function `library()`,

```{r, message=FALSE, error=FALSE, warning=FALSE}
library(foreign)
library(tidyverse)
library(psych)
library(gtsummary)
library(ggplot2)
library(ggpubr)
library(GGally)
library(rsq)
library(broom)
library(car)
```

### Dataset
We will use the `coronary.dta` dataset in STATA\index{STATA} format. 
The dataset contains the total cholesterol level, 
their individual characteristics and intervention groups in a hypothetical clinical trial. 
The dataset contains 200 observations for nine variables:
  
#1. _id_: Subjects' ID.
2. _cad_: Coronary artery disease status (categorical) {no cad, cad}.
3. _sbp_ : Systolic blood pressure in mmHg (numerical).
4. _dbp_ : Diastolic blood pressure in mmHg (numerical).
5. _chol_: Total cholesterol level in mmol/L (numerical).
6. _age_: Age in years (numerical).
7. _bmi_: Body mass index (numerical).
8. _race_: Race of the subjects (categorical) {malay, chinese, indian}.
#9. _gender_: Gender of the subjects (categorical) {woman, man}.

The dataset is loaded as follows,

```{r}
coronary = read.dta("coronary.dta")
head(coronary)
```

We then look at the basic structure of the dataset,
```{r}
str(coronary)
```

## Simple Linear Regression (SLR)

### About Simple Linear Regression

Simple linear regression (SLR) models _linear_ (straight line) relationship between:

- **outcome**: numerical variable.
- **ONE predictor**: numerical/categorical variable.
    
_Note_: When the predictor is a categorical variable, this is typically analyzed by one-way ANOVA.
However, SLR can also handle a categorical variable in the GLM framework.

We may formally represent SLR in form of an equation as follows,

$$numerical\ outcome = intercept + coefficient \times predictor$$
or in a shorter form using mathematical notations,

$$\hat y = b_0 + b_1x_1$$
where $\hat y$ (pronounced y hat) is the predicted value of the outcome y.

### Data exploration

Let say, for the SLR we are interested in knowing 
whether diastolic blood pressure *(predictor)* is associated with the cholesterol level *(outcome)*.
We explore the variables by obtaining the descriptive statistics and plotting the data distribution.

Let obtain the descriptive statistics of the variables,

```{r, message=FALSE}
coronary %>% select(chol, dbp) %>% describe()
```

and the histograms and box-and-whiskers plots,

{r, fig.cap="Histograms and box-and-whiskers plots for `chol` and `dbp`.", message=FALSE}
hist_chol = ggplot(coronary, aes(chol)) + 
  geom_histogram(color = "black", fill = "white")
hist_dbp = ggplot(coronary, aes(dbp)) + 
  geom_histogram(color = "black", fill = "white")
bplot_chol = ggplot(coronary, aes(chol)) + 
  geom_boxplot()
bplot_dbp = ggplot(coronary, aes(dbp)) + 
  geom_boxplot()
ggarrange(hist_chol, bplot_chol, hist_dbp, bplot_dbp)


# Univariable analysis

For the analysis, we fit the *SLR model*, which consists of only one predictor (univariable). 
Here, `chol` is specified as the outcome, and `dbp` as the predictor.
In `glm`, the formula is specified as `outcome ~ predictor`. 
Here, we specify `chol ~ dbp` as the formula in `glm`.

We fit and view the summary information of the model as,
```{r}
slr_chol = glm(chol ~ dbp, data = coronary)
summary(slr_chol)


We can tidy up the *glm* output and obtain the 95% confidence interval (CI) using `tidy()` from the `broom` package.
```{r}
tidy(slr_chol, conf.int = TRUE)
```

From the output above, we pay attention at these results:

- coefficients, $b$ -- column `estimate`.
- 95% CI -- columns `conf.low` and `conf.high`.
- _P_-value -- column `p.value`.

### Model fit assessment

It is important to assess to what extend the SLR model reflects the data. First, we can assess this by $r^2$,
which is the percentage of the variance for the outcome that is explained by the predictor. 
In simpler words, to what extend the variation in the values of the outcome is caused/explained by the predictor.
This ranges from 0% (the predictor does not explain the outcome at all) to 100% (the predictor explains the outcome perfectly).
Here, we obtain the $r^2$ values using `rsq()`

```{r}
rsq(slr_chol)
```
  
Next, we can assess the model fit by a scatter plot,

```{r, fig.cap="Scatter plot of `chol` (outcome) vs `dbp` (predictor)., message=FALSE, eval=FALSE, }

plot_slr <- ggplot(coronary, aes(x = dbp, y = chol)) + 
  geom_point() + geom_smooth(method = lm)
plot_slr
```

```{r, fig.cap="Scatter plot of `chol` (outcome) vs `dbp` (predictor)., message=FALSE, echo=FALSE}

plot_slr <- ggplot(coronary, aes(x = dbp, y = chol)) + 
  geom_point() + geom_smooth(method = lm, color = "black")
plot_slr
```

### Presentation and interpretation

To present the result, we can use `tbl_regression()` to come up with a nice table. 
We use `slr_chol` of the `glm` output with `tbl_regression()` in the `gtsummary` package.

```{r, error=FALSE, message=FALSE, eval=FALSE}
tbl_regression(slr_chol) 
```

```{r, error=FALSE, message=FALSE, echo=FALSE}
tbl_regression(slr_chol, intercept = TRUE) %>% as_gt()
```

<!--Here, we use `intercept = TRUE` to include the intercept value in the table. 
By default, this is omitted by the `tbl_regression()`.-->

It is also very informative to present the model equation,

$$chol = 3.0 + 0.04\times dbp$$
where we obtain the intercept value from `summary(slr_chol)`.

Based on the $r^2$ (which was `r round(rsq(slr_chol), 3)`), table and model equation, 
we may interpret the results as follows:

- 1mmHg increase in DBP causes 0.04mmol/L increase in cholesterol level.
- DBP explains `r round(rsq(slr_chol)*100, 1)`% of the variance in cholesterol level.

## Multiple Linear Regression (MLR)

### About Multiple Linear Regression (MLR)

Multiple linear regression (MLR) models _linear_ relationship between:

- **outcome**: numerical variable.
- **MORE than one predictors**: numerical and categorical variables.

We may formally represent MLR in form of an equation,

$$\begin{aligned}
numerical\ outcome = &\ intercept \\
& + coefficients \times numerical\ predictors \\
& + coefficients \times categorical\ predictors
\end{aligned}$$
or in a shorter form,

$$\hat y = b_0 + b_1x_1 + b_2x_2 + ... + b_px_p$$
where we have _p_ predictors.

Whenever the predictor is a categorical variable with more than two levels, we use dummy variable(s). 
There is no issue with binary categorical variable. For the variable with more than two levels, 
the number of dummy variables (i.e. once turned into several binary variables) equals number of levels minus one. 
For example, whenever we have four levels, we will obtain three dummy (binary) variables. 
As we will see later, `glm` will automatically do this for `factor` variable and
provide separate estimates for each dummy variable.

### Data exploration
Now, for the MLR we are no longer restricted to one predictor. Lets say, 
we are interested in knowing the relationship between blood pressure (SBP and DBP), age, BMI, race 
and render as the predictors and the cholesterol level (outcome). 
As before, we explore the variables by the descriptive statistics,

```{r, message=FALSE}
# numerical
coronary %>% select(-c(id, cad, race, gender)) %>% describe()
```

```{r, message=FALSE, eval=FALSE}
# categorical
coronary %>% dplyr::select(race, gender) %>% tbl_summary() 
```
```{r, message=FALSE, echo=FALSE}
# categorical
coronary %>% dplyr::select(race, gender) %>% tbl_summary() %>% as_gt()
```

and the pairs plot, where we focus on the distribution of the data by histograms and box-and-whiskers plots.
The pairs plot also includes information on the bivariate correlation statistics between the numerical variables.

```{r, fig.cap="Pairs plot for all variables.", message=FALSE}
coronary %>% select(-c(id, cad)) %>% ggpairs()
```

### Univariable analysis
For the univariable analysis in the context of MLR, 
we aim to select variables that are worthwhile to be included in the multivariable model.
In the context of **exploratory research**, 
we want to choose only variables with _P_-values < 0.25 to be included in MLR. To obtain the _P_-values, 
you may perform separate SLRs for each of the predictors (on your own). 
However, obtaining _P_-value for each predictor is easy by `add1()` function. 
Here, we use likelihood ratio test (LRT) using `test = "LRT"` 
option to obtain the _P_-values. 
We start with an intercept only model `slr_chol0` using `chol ~ 1` 
formula specification in the `glm` followed by `add1()`. `add1()` will test each predictor one by one.

```{r}
slr_chol0 <- glm(chol ~ 1, data = coronary)  # intercept only model
add1(slr_chol0, scope = ~ sbp + dbp + age + bmi + race + gender, test = "LRT")
```

From the output, all variables are important with _P_ < 0.25 except `gender`. 
These variables, excluding `gender`, are candidates in this variable selection step.

However, please keep in mind that in the context of **confirmatory research**, 
the variables that we want to include are not merely based on _P_-values alone. 
It is important to consider expert judgement as well.

### Multivariable analysis

Multivariable analysis involves more than one predictors. 
In the univariable variable selection, we decided on several potential predictors. 
For MLR, we (judiciously) included these variables in an MLR model. In the present dataset, 
we have the following considerations:
  
  - including both SBP and DBP is redundant, because both represent the blood pressure.
These variables are also highly correlated. 
This is indicated by the correlation value, _r_ = 0.828 and
scatter plot for the SBP-DBP pair in the pairs plot in the data exploration step.
- `gender` was not sigficant, thus we may exclude the variable.
- let say, as advised by experts in the field, we should exclude `age` in the modelling.

Now, given these considerations, we perform MLR with the selected variables,

```{r}
mlr_chol = glm(chol ~ dbp + bmi + race, data = coronary)
summary(mlr_chol)
```

From the output above, for each variable, we focus these results:
  
  - coefficients, $b$s -- column `estimate`.
- 95% CIs -- columns `conf.low` and `conf.high`.
- _P_-values -- column `p.value`.

Note that for a categorical variable with more than two categories, 
the estimates are obtained for each dummy variable. In our case, `race` consists of *Malay*, *Chinese* and *Indian*.
From the output, the dummy variables are `racechinese` representing Chinese vs Malay 
and `raceindian` representing Indian vs Malay dummy variables, where Malay is set as the baseline comparison group.

We also notice that some variables are not significant at significance level of 0.05, namely `bmi` and `racechinese`.
As for `racechinese` dummy variable, because this forms part of the `race` variable,
we accept the variable because it is marginally insignificant (0.0512 vs 0.05) and
the other dummy variable `raceindian` is significant.

#### Stepwise automatic variable selection

We noted that not all variables included in the model are significant. 
In our case, we may remove `bmi` because it is not statistically significant. 
But in exploratory research where we have hundreds of variables, 
it is impossible to select variables by eye-ball judgement. 
So, in this case, how to perform the variable selection? 
  To explore the significance of the variables, we may perform stepwise automatic selection.
It is important to know stepwise selection is meant for exploratory research. For confirmatory analysis,
it is important to rely on expert opinion for the variable selection. We may perform forward, 
backward or both forward and backward selection combined.

_Forward selection_ starts with an intercept only or empty model without variable. 
It proceeds by adding one variable after another.
In R, Akaike information criterion (AIC) is used as the comparative goodness-of-fit measure and model quality.
In the stepwise selection, it seeks to find the model with the lowest AIC iteratively and the steps are shown in the output.
To perform *forward selection* we will use the `step()` of r base and add the parameter _direction="forward"_. 

```{r}
# forward
mlr_chol_stepforward <- step(slr_chol0, scope = ~ dbp + bmi + race, direction = "forward")
```

_Backward selection_ starts with a model containing all variables.
Then, 
it proceeds by removing one variable after another, of which it aims to find the model with the lowest AIC.

```{r}
# backward
mlr_chol_stepback <- step(mlr_chol, direction = "backward")
```

_Bidirectional selection_ as implemented in R, starts as with the model with all variables.
Then it proceeds with removing or adding variables, which combines both forward and backward selection methods. 
It stops once it finds the model with the lowest AIC.

```{r}
# both
mlr_chol_stepboth = step(mlr_chol, direction = "both")
```

#### Preliminary model {-}

Let say, after considering the _P_-value, stepwise selection (in exploratory research) and expert opinion, 
we decided that our preliminary model is,

`chol ~ dbp + race`

and we fit the model again to view basic information of the model,

```{r, message=FALSE}
mlr_chol_sel = glm(chol ~ dbp + race, data = coronary)
summary(mlr_chol_sel)
rsq(mlr_chol_sel)
```

### Interaction
Interaction is the combination of predictors that requires interpretation of their regression coefficients separately 
based on the levels of the predictor. 
For example, we need separate interpretation of the coefficient for `dbp` depending on `race` group: Malay, Chinese or Indian. 
This makes interpreting our analysis complicated as we can no longer interpret each coefficient on its own. So, most of the time,
we pray not to have interaction in our regression model. We fit the model with a two-way interaction term,

```{r}
summary(glm(chol ~ dbp * race, data = coronary))
```

From the output, 
there is no evidence that suggests the presence of interaction because the included interaction term was insignificant. 
In R, it is easy to fit interaction by `*`, e.g. `dbp * race` will automatically includes all variables involved. 
This is equal to specifiying `glm(chol ~ dbp + race + dbp:race, data = coronary)`, 
where we can use `:` to include the interaction.

### Model fit assessment
For MLR, we assess the model fit by $r^2$ and histogram and scatter plots of residuals.
Residuals, in simple term, are the discrepancies between the observed values (dots) 
and the predicted values (by the fit MLR model).
So, the lesser the discrepancies, the better is the model fit.

#### Percentage of variance explained, $r^2$ 

First, we obtain the $r^2$ values. In comparison to the $r^2$ obtained for the SLR,
we include `adj = TRUE` here to obtain an adjusted $r^2$. The adjusted $r^2$ 
here is the $r^2$ with penalty for the number of predictors _p_. 
This discourages including too many variables, which might be unnecessary.

```{r}
rsq(mlr_chol_sel, adj = TRUE)
```

#### Histogram and box-and-whiskers plot 
Second, 
we plot a histogram and a box-and-whiskers plot to assess the normality of raw/unstandardized residuals of the MLR model.
We expect normally distributed residuals to indicate a good fit of the MLR model. 
Here, we have a normally distributed residuals.
The `resid()` will be use to compute the residuals of the `glm` model.

```{r, fig.cap="Histogram of raw residuals."}
rraw_chol <- resid(mlr_chol_sel)
hist(rraw_chol)
```

```{r, fig.cap="Box-and-whiskers plot of raw residuals."}
boxplot(rraw_chol)
```

#### Scatter plots
Third, we plot a standardized residuals (Y-axis) vs standardized predicted values (X-axis).
Similar to the one for SLR, this plot allows assessment of normality, linearity and equal variance assumptions.
The dots should form elliptical/oval shape (normality) and scattered roughly equal above and below the zero line (equal variance).
Both these indicate linearity. Our plot below shows that the assumptions are met.
The standardized residuals are computed using `rstandard()` 
function and standardized predicted values are computed using `scale()` function.

```{r, fig.cap="Scatter plot of standardized residuals vs standardized predicted values"}
rstd_chol <- rstandard(mlr_chol_sel)  # standardized residuals
pstd_chol <- scale(predict(mlr_chol_sel))  # standardized predicted values
plot(rstd_chol ~ pstd_chol, xlab = "Std predicted", ylab = "Std residuals")
abline(0, 0)  # normal, linear, equal variance
```

In addition to the standardized residuals vs standardized predicted values plot, 
for numerical predictors, we assess the linear relationship between the raw residuals and 
the observed values of the numerical predictors. We plot the raw residuals vs numerical predictor below. 
The plot interpreted in similar way to the standardized residuals vs standardized predicted values plot. 
The plot shows good linearity between the residuals and the numerical predictor.

```{r, fig.cap="Scatter plot of raw residuals vs DBP (numerical predictor)."}
plot(rraw_chol ~ coronary$dbp, xlab = "DBP", ylab = "Raw Residuals")
abline(0, 0)
```

### Presentation and interpretation
After passing all the assumption checks, we may now decide on our final model. 
We may rename the preliminary model `mlr_chol_sel` to `mlr_chol_final` for easier reference. 

```{r}
mlr_chol_final <- mlr_chol_sel
```

Similar to SLR, we use `tbl_regression()` to come up with a nice table to present the results.

```{r, message=FALSE, eval=FALSE}
tbl_regression(mlr_chol_final) 
```
```{r, message=FALSE, echo=FALSE}
tbl_regression(mlr_chol_final) %>% as_gt()
```

It will be useful to be able to save the output in the spreadsheet format for later use.
We can use `tidy()` function in this case and export it to a `.csv` file,

```{r, eval=FALSE}
tib_mlr <- tidy(mlr_chol_final, conf.int = TRUE)
write.csv(tib_mlr, "mlr_final.csv")
```

Then, we present the model equation. Cholesterol level in mmol/L can be predicted by its predictors as given by,

$$chol = 3.30 + 0.03\times dbp + 0.36\times race\ (chinese) + 0.71\times race\ (indian)$$
  Based on the adjusted $r^2$, table and model equation, we may interpret the results as follows:
  
- 1mmHg increase in DBP causes 0.03mmol/L increase in cholesterol, while controlling for the effect of race.
- Likewise, 10mmHg increase in DBP causes 0.03 x 10 = 0.3mmol/L increase in cholesterol, while controlling for the effect of race.
- Being Chinese causes 0.36mmol/L increase in cholesterol in comparison to Malay, while controlling for the effect of DBP.
- Being Indian causes 0.71mmol/L increase in cholesterol in comparison to Malay, while controlling for the effect of DBP.
- DBP and race explains 22.3% variance in cholesterol.

For each of this interpretation, 
please keep in mind to also consider the 95% CI of each of coefficient. 
For example, being Indian causes 0.71mmol/L increase in cholesterol in comparison to Malay, 
where this may range from 0.34mmol/L to 1.1mmol/L based on the 95% CI.

## Prediction
In some situations, it is useful to use the SLR/MLR model for prediction. 
For example, we may want to predict the cholesterol level of a patient given some clinical characteristics. 
We can use the final model above for prediction. For starter, let us view the predicted values for our sample,

```{r}
coronary$pred_chol <- predict(mlr_chol_final)
head(coronary)
```

Compare the predicted values with the observed cholesterol level. 
Recall that we already checked this for the model fit assessment before.

It is more useful to predict for newly observed data. 
Let us try predicting the cholesterol level for an Indian patient with DBP = 90mmHg,

```{r}
predict(mlr_chol_final, list(dbp = 90, race = "indian"))
```

Now, we also do so the data with many more patients,

```{r}
new_data = data.frame(dbp = c(90, 90, 90), 
                      race = c("malay", "chinese", "indian"))
predict(mlr_chol_final, new_data)
new_data$pred_chol = predict(mlr_chol_final, new_data)
new_data
```
#==============================================================================#
#-------------------------Logistic regression ---------------------------------#

# Binary Logistic Regression

## Introduction 
A binary variable is a categorical outcome that has two categories or levels. 
In medical and health research, the binary outcome variable is very common. 
Some examples where the outcome is binary include:
  
- survival status when the status of cancer patients at the end of treatment are coded as either alive or dead
- relapse status when the status of a patient is coded as either relapse or not relapse
- satisfaction level when patients who come to clinics are asked if they are satisfied or not satisfied with the service 
- glucose control when patients were categorized as either good control or poor control based on Hba1c 

In statistics, the logistic model (or logit model) is a statistical model 
that models the probability of an event taking place by having the log-odds for 
the event be a linear combination of one or more independent variables. 
In a binary logistic regression model, the dependent variable has two levels (categorical). 

## Logistic regression model
The logistic model (or logit model) is used to model the probability of a particular class or event existing, 
such as pass or fail, win or lose, alive or dead or healthy or sick. More specifically, 
binary logistic regression is used to model the relationship between a covariate or 
a set of covariates and an outcome variable which is a binary variable.


## Dataset
We will use a dataset named `stroke.dta` which in *STATA* format. 
These data come from a study of hospitalized stroke patients. 
The original dataset contains 12 variables, but our main variables of interest are:
  
  - status : Status of patient during hospitalization (alive or dead)
- gcs : Glasgow Coma Scale on admission (range from 3 to 15)
- stroke_type : IS (Ischaemic Stroke) or HS (Haemorrhagic Stroke)
- sex : female or male
- dm : History of Diabetes (yes or no)
- sbp : Systolic Blood Pressure (mmHg)
- age : age of patient on admission

The outcome variable is variable status. It is labelled as either dead or alive, 
which is the outcome of each patient during hospitalization.   

## Logit and logistic models
The simple binary logit and logistic models refer to a a model with only one covariate (also known as independent variable).
For example, if the covariate is gcs (Glasgow Coma Scale), the simple logit model is written as:
  
  $$\hat{g}(x)= ln\left[ \frac{\hat\pi(x)}{1 - {\hat\pi(x)}} \right]$$
  
where $\hat{g}(x)$ is the log odds for death for a given value of gcs. 
And the odds for death for a given value of GCS is written as 
$$  = \hat\beta_0 + \hat\beta_1(gcs)$$
  
And the simple logistic model is also written as:
 $$\hat{\pi}(x) = \frac{exp^{\hat{\beta}_{0} + \hat{\beta}_{1}{gcs}}}{1 + exp^{\hat{\beta}_{0} + \hat{\beta}_{1}{gcs}}}$$
  The $\pi(x) = E(Y|x)$ represents the conditional mean of $Y$ given $x$ 
  when the logistic distribution is used.

This is also simply known as the predicted probability of death for given value of *gcs*. 

If we have decided (based on our clinical expertise and literature review) 
that a model that could explain death consists of *gcs*, *stroke type*, *sex*, *dm*, *age* and *sbp*, 
then the logit model can be expanded to:
  
$$\hat{g}(x)  = \hat\beta_0 + \hat\beta_1(gcs) + \hat\beta_2(stroke type) + \hat\beta_3(sex)+
  \hat\beta_4(dm) + \hat\beta_5(sbp) +  \hat\beta_6(age)$$
  
These are the odds for death given certain gcs, sbp and age values and specific categories of stroke type, sex and diabetes. 
While the probability of death is 

$$\hat{\pi}(x) = \frac{exp^{\hat\beta_0 + \hat\beta_1(gcs) + \hat\beta_2(stroke type) + \hat\beta_3(sex)+ 
  \hat\beta_4(dm) + \hat\beta_5(sbp) + \hat\beta_6(age)})}{1 + exp^{\hat\beta_0 + \hat\beta_1(gcs) +
    \hat\beta_2(stroke type) + \hat\beta_3(sex)+ \hat\beta_4(dm) + \hat\beta_5(sbp) + \hat\beta_6(age)}}$$
  
In many datasets, some independent variables are either discrete or nominal scale variables. 
Such variables include race, sex, treatment group, and age categories. 
Including them in the model is inappropriate as if they were interval scale variables is inappropriate. 
In some statistical software, these variables are represented by numbers. However, be careful; 
these numbers are used merely as identifiers or labels for the groups. 


In this situation, we will use a method called design variables (or dummy variables). 
Suppose, for example, assuming that one of the independent variables is obesity type, 
which is now coded as “Class 1”, “Class 2” and “Class 3”. In this case, there are 3 levels or categories, 
hence two design variables ($D - 1$) are necessary, let say D1 and D2. 
One possible coding strategy is that when the patient is in “Class 1” 
then the two design variables, for D1 and D2 would both be set equal to zero. 
In this example, "Class 1" is the reference category. 
When the patient is in “Class 2”, then D1 is set as 1 and D2 as 0; 
when the patient is in "Class 3", the we will set D1 as 0 and D2 and 1. 
All these coding assignments can be done automatically in the software. 
But to interpret, we must know which category is the reference.  

### Loading libraries
1. the built in **stat** package - to run Generalized Linear Model.loaded by default.
2. **haven** - to read SPSS, STATA and SAS dataset
3. **tidyverse** - to perform data transformation 
4. **gtsummary** - to provide nice results in a table  
4. **broom** - to tidy up the results 
5. **pROC** - to do model assessment
6. **here** - to ensure proper directory     

To load these packages, we will use the function `library()`:

```{r}
library(haven)
library(tidyverse)
library(gtsummary)
library(broom)
library(pROC)
library(here)
```

## Read data

WE will read data in the working directory into our R environment. 
The example dataset comes from a study among stroke inpatients. 
The dataset is in the STATA format `stroke.dta`.  

```{r}
fatal <- read_dta(here('stroke.dta'))
```

Take a peek at data to check for 

- variable names
- variable types 
```{r}
head(fatal)
```


```{r}
glimpse(fatal)
#str(fatal)
```

## Explore data
Variables sex, status, dm and stroke type are labelled variables though they are coded as numbers.
The numbers represent the groups or categories or levels of the variables. 
They are categorical variables and not real numbers.

We will transform all of labelled variables to factor variables using `mutate()`. 
And to transform all labelled variables, we can quickly achieve that by using the function `across()`. 
See the codes below to transform all labelled variables in the dataset to factor variables:

```{r}
fatal <- fatal %>%
  mutate(across(where(is.labelled), as_factor))
```

```{r}
glimpse(fatal)
```

Now, examine the summary statistics:

```{r}
fatal %>%
  tbl_summary() %>%
  as_gt()
```

If we want to get summary statistics based on the status of patients at discharge:

```{r}
fatal %>%
  tbl_summary(by = status) %>%
  as_gt()
```

## Estimate the regression parameters
As we assume the outcome variable (status) follows binomial distribution, 
we will perform binary logistic regression. 
Logistic regression allow us to estimate the regression parameters $\hat\beta_s$ or 
the log odds is dataset where the outcome follows binomial or bernoulli distribution. 

To achieve the objective above, we do this in two steps:

- The simple binary logistic regression or the univariable logistic regression: 
In this analysis, there is only one independent variable or covariate in the model.
This is also known as the crude or unadjusted analysis. 
- The multiple binary logistic regression or the multivariable logistic regression:
Here, we expand our model and include two or more independent variables (covariates).
The multiple binary logistic regression model is an adjusted model, and 
we can obtain the estimate of a particular covariate independent of the other covariates in the model.  

## Simple binary logistic regression
Simple binary logistic regression model has a dependent variable and only one independent (covariate) variable. 

In our dataset, for example, we are interested to model a simple binary logistic regression using   

- status as the dependent variable.
- gcs as the independent variable. 

The independent variable can be a numerical or a categorical variable.

To estimate the log odds (the regression parameters, $\beta$) for the covariate Glasgow Coma Scale (GCS), 
we can write the logit model as: 

$$log\frac{p(status = dead)}{1 - p(status = dead)}  = \hat\beta_0 + \hat\beta_1(gcs)$$


In R, we use the `glm()` function to estimate the regression parameters and other parameters of interest. 
Let run the model with gcs as the covariate and name the model as `fatal_glm_1`

```{r}
fatal_glm_1 <- glm(status ~ gcs, data = fatal, 
                   family = binomial(link = 'logit'))
```

To get the summarized result of the model `fatal_glm_1`, we will use the `summary()` function:
  
  ```{r}
summary(fatal_glm_1)
```

To get the model summary in a data frame format, so we can edit more easily,
we can use the `tidy()` function from the **broom** package. 
The package also contains other functions to provide other parameters useful for us later. 

The function `conf.int()` will provide the confidence intervals (CI). 
The default is set at the $95%$ level:
  
  ```{r}
tidy(fatal_glm_1, conf.int = TRUE)
```

The estimates here are the log odds for death for a given value of gcs. 
In this example, each unit increase in gcs, the crude or 
unadjusted log odds for death due to stroke change by a factor
$-0.388$ with $95%$ CI ranges from $-0.497  and 	-0.292$.    

Now, let use another covariate, `stroke_type`. Stroke type has 2 levels or categories; 
Haemorrhagic Stroke (HS) and Ischaemic Stroke (IS). 
HS is known to cause a higher risk for deaths in stroke. We will model stroke type (`stroke_type`), 
name the model as `fatal_glm_2` and show the result using `tidy()`

```{r}
fatal_glm_2 <- 
  glm(status ~ stroke_type, 
      data = fatal, 
      family = binomial(link = 'logit'))
tidy(fatal_glm_2, conf.int = TRUE)
```

The simple binary logistic regression models shows 
that patients with Haemorrhagic Stroke (HS) had a higher log odds for death during admission (by a factor $2.02$) 
as compared to patients with Ischaemic Stroke (IS).  

## Multiple binary logistic regression
There are multiple factors that can contribute to the outcome of stroke. 
Hence, there is a strong motivation to include other independent variables or covariates in the model.
For example, in the case of stroke: 

- It is unlikely that only one variable (gcs or stroke type) is related with stroke. 
Stroke like other cardiovascular diseases has many factors affecting the outcome. 
It makes more sense to consider adding other independent variables that we believe
are important independentvariables for stroke outcome in the model. 
- by adding more covariates in the model, we can estimate the adjusted log odds. 
These log odds indicate the relationship of a particular covariate independent of other covariates in the model.
In epidemiology, we always can this as adjustment. An adjustment is important particularly 
when we have confounding effects from other independent variables. 
- interaction term can be generated (the product of two covariates) and added to the model to be estimated.

To add or not to add variables is a big subject on its own. 
Usually it is governed by clinical experience, subject matter experts and some preliminary analysis. 

Let expand our model and include gcs, stroke_type, sex, dm, sbp and age in the model. 
We will name this model as `fatal_mv`. As we have more than one independent variables in the model, 
we will call this as multiple binary logistic regression. 

To estimates the multiple logistic regression model in R: 
  
  ```{r}
fatal_mv1 <- 
  glm(status ~ gcs + stroke_type + sex + dm + sbp + age, 
      data = fatal, 
      family = binomial(link = 'logit'))
summary(fatal_mv1)
```

We could get a cleaner result in a data frame format (and you can edit in spreadsheet easily) 
of the multivariable model by using `tidy()` function:
  
  ```{r}
log_odds <- tidy(fatal_mv1, 
                 conf.int = TRUE)
log_odds
```

We could see that the multivariable model that we named as `fatal_mv1`, can be interpreted as below: 
  
- with one unit increase in Glasgow Coma Scale (GCS), 
the log odds for death during hospitalization equals to $-0.328$, adjusting for other covariates.
- patients with HS have $1.266$ times the log odds for death as compared to patients with IS,
adjusting for other covariates.
- female patients have $0.430$ times the log odds for death as compared to male patients, 
adjusting for other covariates.
- patients with diabetes mellitus have $0.474$ times the log odds for deaths as compared 
to patients with no diabetes mellitus.
- With one mmHg increase in systolic blood pressure, the log odds for deaths change by 
a factor of $0.00086$, when adjusting for other variables.  
- with an increase in one year of age, the log odds for deaths change by a factor of $0.024$, 
when adjusting for other variables.

## Convert the log odds to odds ratio
Lay person has difficulty to interpret log odds from logistic regression.
That why, it is more common to interpret the logistic regression models using odds ratio. 
To obtain the odds ratios, we set the argument `exponentiate = TRUE` in the `tidy()` function. 
Actually, odds ratio can be easily calculate by $\exp^{\beta_i}$

```{r}
odds_ratio <- tidy(fatal_mv1,
                   exponentiate = TRUE,  
                   conf.int = TRUE)
odds_ratio
```

## Making inference 
Let us rename the table appropriately so we can combine the results from the log odds and the odds ratio later.

```{r}
tab_logistic <- bind_cols(log_odds, odds_ratio) 
tab_logistic %>% 
  select(term...1, estimate...2, std.error...3, 
         estimate...9, conf.low...13, conf.high...14 ,p.value...5) %>%
  rename(covariate = term...1, 
         log_odds = estimate...2,
         SE = std.error...3,
         odds_ratio = estimate...9,
         lower_OR = conf.low...13, 
         upper_OR = conf.high...14,
         p.val = p.value...5) 
```

In the model, we can interpret the estimates as below:

- if **gcs** increases by 1 unit (when *stroke type* is adjusted),
the log odds for death changes by a factor $-0.32$ or the odds for death changes by 
a factor $0.72$ (odds for death reduces for $28\%$). 
The $95\%CI$ are between $21\%,36\%$, adjusting for other covariates.
- patients with HS have $3.55\%$ times higher odds for stroke deaths - with $95\%CI : 17\%, 85\%$ 
- as compared to patients with HS, adjusting for other independent variables.
- female patients have $53\%$ higher odds for death as compared 
to female patients ($p = 0.154$), adjusting for other covariates.
- patients with diabetes mellitus have $60.6\%$ higher odds for deaths compared 
to patients with no diabetes mellitus though the p value is above $5\%$ ($p = 0.642\%$).
- With one mmHg increase in systolic blood pressure, the odds for death change by 
a factor $1.00086$, when adjusting for other variables. The p value is also larger than $5\%$.  
- with an increase in one year of age, the odds for deaths increase by a factor of $1.025$, 
when adjusting for other variables. However, the p value is $0.115$  


## Models comparison
The importance of independent variables in the models should not be based on their p-values or the Wald statistics alone.
It is recommended to use likelihood ratio to compare models. 
The difference in the likelihood ratio between models can guide us on choosing a better model.  

For example, when we compare model 1 (`fatal_mv`) and model 2 (`fatal_glm_1`), 
could we say that they are different? One approach is to to see if both models are different statistically. 
This comparison can be done by setting the level of significance at $5\%$. 

```{r}
anova( fatal_glm_1, fatal_mv1, test = 'Chisq')
```

Both models are different statistically (at $5\%$ level). 
Hence, we prefer to keep model `fatal_mv1` because the model makes more sense (more parsimonious). 

Now, let be economical, and just keep variables such as gcs, stroke_type and age in the model. 
We will name this multivariable logistic model as `fatal_mv2`:
  
  ```{r}
fatal_mv2 <- 
  glm(status ~ gcs + stroke_type + age, 
      data = fatal,
      family = binomial(link = 'logit'))
```

And we will perform model comparison again:
  
  ```{r}
anova( fatal_mv1, 
       fatal_mv2, test = 'Chisq')
```

The p-value is above the threshold of $5\%$ set by us. 
Hence, we do not want to reject the null hypothesis 
(null hypothesis says that both models are not statistically different).
This approach also agrees with Occam razor principle; always choose simpler model. 
In this case, `fatal_mv2` is simpler and deserves further exploration. 


## Adding an interaction term
Interaction effect occurs when the effect of one variable depends on the value of another variable 
(in the case of two interacting variables).
Interaction effect is common in regression analysis, ANOVA, and in designed experiments. 

Two way interaction term involves two risk factors and their effect on one disease outcome. 
If the effect of one risk factor is the same within strata defined by the other, 
then there is no interaction. 
When the effect of one risk factor is different within strata defined by the other,
then there is an interaction; this can be considered as a biological interaction. 

Statistical interaction in the regression model can be measured based on the ways that risks are calculated (modeling). 
The presence of statistical interaction may not reflect true biological interaction.

Let add an interaction term between stroke type and gcs:
  
  ```{r}
fatal_mv2_ia <- 
  glm(status ~ gcs + stroke_type + stroke_type:gcs + age, 
      data = fatal, 
      family = binomial(link = 'logit'))
tidy(fatal_mv2_ia)
```


$$\hat{g}(x)  = \hat\beta_0 + \hat\beta_1(gcs) + \hat\beta_2(stroke type) + \hat\beta_3(age)+
  \hat\beta_4(gcs \times stroke_type)$$
  
To decide if we should keep an interaction term in the model, 
we should consider if the interaction term indicates both biological and statistical significance. 
If we believe that the interaction reflects both, then we should keep the interaction term in the model. 

Using our data, we can see that:
  
- the coefficient for the interaction term for stroke type and gcs is not significant 
at the level of significance of $5\%$ that we set.
- stroke experts also believe that the effect of gcs on stroke death is not largely different 
between different stroke type

Using both reasons, we decide not to keep the two-way interaction between gcs and 
stroke type in our multivariable logistic model. 


## Prediction from binary logistic regression

The **broom** package has a function called `augment()` which can calculate:
  
  1. estimated log odds 
2. probabilities
2. residuals
3. hat values
4. Cooks distance
5. standardized residuals

### Predict the log odds
To obtain the `.fitted` column for the estimated log odds for death of each patient in the stroke data, 
we can run: 
  
  ```{r}
log_odds_mv2 <- augment(fatal_mv2)
log_odds_mv2 %>%
  slice(1:10)
```
The `slice()` gives the snapshot of the data. In this case, we choose the first 10 patients. 

### Predict the probabilities
To obtain the `.fitted` column for the estimated probabilities for death of each patient, 
we specify `type.predict = "response"`: 
  
  
  ```{r}
prob_mv2 <- 
  augment(fatal_mv2, 
          type.predict = "response")
prob_mv2 %>%
  slice(1:10)
```

## Model fitness
The basic logistic regression model assessment includes the measurement of overall model fitness. 
To do this, we check

- Area under the curve (AUC)
- Receiver Operating Characteristic (ROC)

Area under the curve (AUC) for a logistic regression model in R, 
can be computed using the `pROC` or `ROCR` packages. 
AUC is a performance metric for classification models that summarizes the trade-off between 
the true positive rate (sensitivity) and the false positive rate (1-specificity).

The ROC curve is a graphical representation of a classifiers diagnostic ability across various thresholds,
plotting the true positive rate (sensitivity) against the false positive rate (1-specificity). 
It is widely used in evaluating the performance of binary classification models.

```{r}
roc_curve <- roc(fatal$status,prob_mv2$.fitted )

auc(roc_curve)

# Plot the ROC Curve
plot(roc_curve, col = "blue", main = "ROC Curve")
```

Our model shows that:

- the area under the curve is $88.35\%$. The values of above 80 are considered to have good discriminating effect. 


## Presentation of logistic regression model

The **gtsummary** package has a useful function `tbld_regression()` 
which can be used to produce a formatted table suitable for publication. 
E.g.To generate a table for adjusted log odds ratio derived from our multivariable logistic regression model `fatal_mv2`, 

we can use the codes below:

```{r}
tbl_regression(fatal_mv2) %>%
  as_gt()
```

Next, to generate the adjusted odds ratios table:

```{r}
tbl_regression(fatal_mv2, exponentiate = TRUE) %>%
  as_gt()
```
#===========================Logistic regression 2-=============================#
#------------------------------------------------------------------------------#

# Multinomial Logistic Regression

## Introduction
Some data come with multinomial outcomes in which case, the outcome variable is a nominal or
polychotomous variable with more than two levels. In multinomial outcome data, 
the outcome has no natural ordering. if it has, then it is best treated as a ordinal outcome data. 

Variables with more than two levels are known as either:

1.  multinomial data
2.  polychotomous data
3.  polytomous data

If we employ logistic regression to such data, then the analysis is known as polytomous or
multinomial logistic regression. Again, the polychotomous outcome does not have any natural order. 
When the categories of the outcome variable do have a natural order, ordinal logistic regression is preferred. 

## Examples of multinomial outcome variables
Examples of data with polychotomous outcome variables (with more than two levels) include:

- disease symptoms that have been classified by subjects as being absent, mild, moderate,or severe,
- tumour invasiveness  classified as in situ, locally invasive, ormetastatic, or
- patient preferred treatment regimen,selected from among three or 
more options for example oral medication only, oral medication plus 
injection medication or injection only.

A numerical outcome can be categorized based on different cut-off points. 
The newly created categorical variable is now either treated as a nominal or 
polychotomous or multinomial outcome  or as a ordinal outcome. 
That justifies the use of multinomial logistic regression. 

## Models for multinomial outcome data
With a multinomial outcome data, an extension of logistic regression known as multinomial logit or 
multinomial logistic regression can be performed. 
In multinomial logistic regression, one of the categories of the outcome variable is designated 
as the reference category and each of the other levels is compared with this reference.

The choice of reference category can be arbitrary and is at the discretion of the researcher. 
Most software set the first category as the reference (also known as the baseline category) by default.

Other models that can analyze data with polychotomous outcome include:

1. Stereotype logistic regression - each independent variable has one value for each individual
2. Alternative-specific variables  


# Estimation for Multinomial logit model
Remember, interpreting and assessing the significance of the estimated coefficients 
are the main objectives in regression analysis. In multinomial logistic regression, 
we would like to model the relationship between covariates with the outcome variable 
that has more than two categories but without ordering or ranking. 

The actual values taken by the dependent variable are irrelevant. 
In Stata, the **exponentiated beta** $\exp^{\beta}$ 
will generate the so-called the **relative-risk ratio**. The dependent variable again, 
is a discrete variable and the model is estimated using maximum likelihood.

In multinomial logistic regression for example in data with three categories of the outcome, 
the sum of the probabilities for the three outcome categories must be equal to 1 (the total probability).
The comparison is done between two categories each time. 
Because of that, each comparison considers only two probabilities, 
and the probabilities in the ratio do not sum to 1. 
Thus, the two odds-like expressions in multinomial logistic regression are not true odds.

Multinomial logistic regression can be thought as of simultenously fitting binary logits 
for all comparisons among the alternatives.

### Log Odds and Odds Ratios
In the special case where the covariate is binary, coded 0 or 1, 
we simplify the notation to $OR_j = OR_j(1,0)$.
Let use an example where data have 3 categories of outcome; 0,1 and 2. 

Let say we have a dataset with the outcome variable, $Y$, and is coded as $0, 1,$ or $2$.

In practice one should check that the software package 
that is going to be used allows a $0$ code as the smallest category. 
We have used packages that require that the codes begin with $1$.

So the logit functions (log odds) when the outcome is a D variable with (0,1 and 2 values) 
are as below
1.  the log odds for comparison between 0 and 1 is
$$g_1(x) = ln\frac{P(D=1|X_1)} {P(D=0|X_1)}=\alpha_1 + (\beta_{11}X_1)$$
  
2.  and, the log odds for comparison between 0 and 2 is
$$g_2(x) =  ln\frac{P(D=2|X_1)} {P(D=0|X_1)}=\alpha_2 + (\beta_{21}X_1)$$
  
If for example, we assume that the outcome labelled with $Y=0$ is the reference outcome.

The subscript on the odds ratio indicates which outcome is being compared to the reference category outcome. 
The odds ratio of outcome $Y = j$ versus $Y = 0$ for the covariate values of $x = a$ versus $x = b$ is:
  
  $$OR_{j}(a,b)= \frac{Pr(Y=j|x=a)/(Pr(Y=0|x=a)} {Pr(Y=j|x=b)/Pr(Y=0|x=b)}$$
  
  Each odds ratio is calculated in a manner similar to that used in standard logistic regression. 
That is:
  
  $$OR_1(X=1,X=0)= \frac{Pr(D=1|X=1)/(Pr(D=0|X=1)} {Pr(Y=1|X=0)/Pr(D=0|X=0)}=\exp^{\beta_{11}}$$
  
  $$OR_2(X=2,X=0)= \frac{Pr(D=2|X=1)/(Pr(D=0|X=1)} {Pr(D=2|X=0)/Pr(D=0|X=0)}=\exp^{\beta_{21}}$$
  
  ### Conditional probabilities
  
  The conditional probabilities for the multinomial logit model are: 
  
  $$Pr(D = 0 | x) = \frac{1}{1+e^{g_1(x)} + e^{g_2(x)}}$$
  
  $$Pr(D = 1 | x) = \frac{e^{g_1(x)}}{1+e^{g_1(x)} + e^{g_2(x)}}$$
  
  $$Pr(D = 2 | x) = \frac{e^{g_2(x)}}{1+e^{g_1(x)} + e^{g_2(x)}}$$
  
  ### Load libraries
- **here -------- file referencing
- **tidyverse---- data wrangling and plotting 
- **haven---------read data in various statistical formats
- **gtsummary-----produce statistical tables 
- **VGAM----------perform multinomial logistic regression
- **kableExtra----produce nice tables for the results 


```{r, warning=FALSE}
library(here)
library(tidyverse)
library(haven)
library(gtsummary)
library(VGAM)
library(kableExtra)
```

### Dataset
The datasets contains all variables of our interest. For the purpose of this assignment, 
Explore association of hypertension status, weight and total cholesterol with the result of screening FBS. 
The variables in the datasets as follow:
  
1.  fbs : Fasting Blood Sugar (mmol/L). The data ranges from 2.51 mmol/L to 28.01 mmol/L.
2.  totchol : Total Cholesterol (mmol/L). The data ranges from 0.18 mmol/L to 23.14 mmol/L.
3.  hpt : Hypertension Status. Coded as Yes and No.
4.  weight : Body weight measures in kilogram. The data ranges between 30kg to 187.8kg.

### Read data
We will use `haven::read_dta()` function to read stata `.dta` data into the memory.   

Let read `metabolic_syndrome.dta` as the dataset into our R memory. 

```{r}
ms <- read_dta('metabolic_syndrome.dta')
```
Quickly glance at the number of observations and the type of variables in the `ms` dataset. 

```{r}
glimpse(ms)
```

### Data Wrangling
We will

- select only variable `fbs`, `totchol`, `hpt` and `weight`
- convert all character variables to factor variables 
- then take a glance of the updated `ms` dataset again


```{r}
ms <- ms %>% 
  select(fbs, totchol, hpt , weight) %>% 
  mutate(across(where(is.character), as_factor))
glimpse(ms)
```

### Create new categorical variable from fbs
Let us create a categorical (also known as a factor variable) based on this classification: 
  
1. Normal if fbs is less than 6.1 mmol/L
2. Impaired Fasting Glucose (IFG)  if fbs is between 6.1 mmol/L to 6.9 mmol/L
3. Diabetis Mellitus (DM) if fbs is 7.00 mmol/L or higher 

```{r}
ms <- ms %>%
  mutate(cat_fbs = cut(fbs, 
                       breaks = c(2.50 , 6.10 , 6.90 , 28.01 ),
                       labels = c("Normal","IFG", "DM")))
ms %>% 
  count(cat_fbs)
```

We notice that there were 251 data has no values recorded as `NA`. Thus, 
we decide to remove observations when there are missing values for variable `cat_fbs`.


```{r}
ms <- ms %>%
  filter(!is.na(cat_fbs)) 
ms %>%
  count(cat_fbs)


### Exploratory data analysis
Next, is to return the table of summary statistics 

ms %>%
  tbl_summary(by = cat_fbs,
              statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
              type = list(where(is.logical) ~ "categorical"),
              label = list(fbs ~ "Fasting Blood Sugar (mmol/L)", 
                           totchol ~ "Total Cholesterol (mmol/L)", hpt~"Hypertension", weight ~ "Weight (kg)"),
              missing_text = "Missing") %>% 
  modify_caption("**Table 1. Survey Participant Characteristic**")  %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Glycemic Control Status**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (SD) or Frequency (%)") %>%
  bold_labels() %>%
  as_gt()

### Confirm the order of cat_fbs
```{r}
levels(ms$cat_fbs)
```
However, we would like the DM as the smallest category. 
To do that we will use the `fct_relevel()` function. 

```{r}
ms <- ms %>% 
  mutate(cat_fbs = fct_relevel(cat_fbs, 
                               c("DM", 'IFG', 'Normal')))
levels(ms$cat_fbs)
```
## Estimation
Our intention to investigate the relationship between totchol, hpt and weight 
with the outcome variables `cat_fbs`. Thus, 
we will perform multinomial logistic regression model to estimate the relation for 2 models:
  
- Model 1: DM vs Normal
- Model 2: IFG vs Normal

In both models, the reference group is Normal

### Single Independent Variable 
For independent variable totchol 

```{r}
log_chol <- vglm(cat_fbs ~ totchol, 
                 multinomial, data = ms)
summary(log_chol)
```
This is the model where hpt is the independent variable

```{r}
log_hpt <- vglm(cat_fbs ~ hpt, 
                multinomial, data = ms)
summary(log_hpt)
```

And lastly, the independent variable is weight

```{r}
log_wt <- vglm(cat_fbs ~ weight, 
               multinomial, data = ms)
summary(log_wt)
```

### Multiple Independent Variables
We feel that totchol, hpt and weight are all important independent variables. 
Hence, we want to fit a model with the three independent variables as the covariates. 

```{r}
mlog <- vglm(cat_fbs ~ totchol + hpt + weight, 
             multinomial, data = ms)
summary(mlog)
```
### Model with interaction term between independent variables 
Then, we hypothesize that there could be a significant interaction between totchol and weight. 
And to test the hypothesis, 
we extend the multivariable logistic regression model by adding an interaction term. 
This interaction term is a product between variable weight and totchol.

```{r}
mlogi <- vglm(cat_fbs ~ totchol + hpt + weight + totchol*weight, 
              multinomial, data = ms)
summary(mlogi)
```

The interaction term in our model showed p-values above 0.05 (p-values of 0.80 and  0.07, respectively). 
As the p-value is bigger than the level of significance at $5\%$ and 
the value of regression parameters for the interaction terms are likely not clinically meaningful, 
we have decided not to use the model with an interaction term. 

## Inferences
For the inference, we will
- calculate the $95\%$ CI (interval estimates)
- calculate the p-values (hypothesis testing)

There is no facility inside the`broom::tidy()` 
function to generate confidence intervals for object with class `vglm`.
Because of that we will use the `coef()`, `confint()` and `cind()` 
functions to produce a rather nice table of inferences. 

We are going to follow these steps:
  
- set the number of digits equal to 2 to limit the decimal numbers
- return the regression coefficents for all $\hat\beta$ as an object named `b_fitmlog2`
- return the the confidence intervals for all $\hat\beta$ as an object named `ci_fitmlog2` 
- combine the $\hat\beta$ and the corresponding $95\%$ CIs


```{r, warning=FALSE}
b_mlog <- coef(mlog)
ci_mlog <- confint(mlog) 
b_ci_mlog <- data.frame(b_mlog,ci_mlog) %>%
  rename("log odds" = b_mlog, "Lower CI" = X2.5.., "Upper CI" = X97.5..)
b_ci_mlog %>% 
  kbl(digits = 2, booktabs = T, caption = "Log odds from multinomial logistic regression") %>%
  kable_styling(position = "center")

Afterwards, we will *exponentiate* the coefficients to obtain the **relative-risk ratio**.
We then combine the results to the previous table. 
Finally, we will name the columns of the object `tab_fitmlog2`. 

{r, warning=FALSE}
rrr_mlog <- exp(b_ci_mlog)
tab_mlog <- cbind(b_ci_mlog, rrr_mlog)
colnames(tab_mlog) <- c('b', 'lower b', 'upper b',
                        'RRR', 'lower RRR', 'upper RRR')
tab_mlog %>%
  kbl(digits = 2, booktabs = T, caption = "Log odds and RRR from multinomial logistic regression") %>%
  kable_styling(position = "center")

## Interpretation
The result from our multivariable logistic regression models can be interpreted as below: 
  
-Every increment 1 mmol/L of totchol (Total cholesterol) when controlling for hypertension status and weight; a) 
the odds of being in DM group (in comparison to Normal) change by 
0.277 (Adjusted RRR = 1.32, $95\%$ CI : 1.232, 1.413, p-value \<0.001) and b) 
the odds of being in IFG group (in comparison to being in Normal) change by 
0.239 (Adjusted RRR = 1.27, $95\%$ CI : 1.169,1.381, p-value \<0.001).
    
-Every increment 1 kg of weight when controlling for hypertension status and total cholesterol; a) 
the odds of being in DM group (in comparison to being in Normal) increase by
0.023 (Adjusted RRR = 1.02, $95\%$ CI : 1.017,1.029, p-value \<0.001), and b) 
the odds of being in IFG group (in comparison being in Normal) increase by 
0.022 (Adjusted RRR = 1.02, $95\%$ CI : 1.015,1.030, p-value \<0.001). 

-In the population with hypertension (as compared with participant without hypertension) 
when controlling for weight and total cholesterol; a) 
their odds of being in DM group (in comparison to being in Normal) change by
0.900 (Adjusted RRR = 2.5, $95\%$ CI : 1.942, 3.114, p-value \<0.001).

When repeated research on this population, the odds ranging between reduced the odds by 
0.481 to 0.677, and b) and their odds of being in IFG group (in comparison to being Normal) 
change by 0.867 (Adjusted RRR = 2.38, 95% CI : 1.789, 3.166, p-value \<0.001).  
    
## Prediction
We can calculate the predicted probability of each category of outcome by using 
the `predict()` function. Below are the result for the top 10 observations. 

predict.vgam(mlog, type = 'response') %>% 
head(10)

# Presentation of multinomial regression model  
We can make a *better* table using `knitr` and `kableExtra` packages, or 
we can save the results as a `.csv` file so we can edit it using spreadsheets.  
    
tab_mlog %>%
  write_csv("results_multinomial.csv")

    #==========================================================================#
    #--------------------------------------------------------------------------#







                                                                                                                                                                                                                                                                              