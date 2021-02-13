#########################################################################################
# DA3 Assignment 3
# Data Analysis Script
# 
# GOAL:
# Use the ABQ swimming pool data
# Find all the open air pools and combine sales
# Clean and filter as you see fit -- create a daily data
# Build a daily predictive model that can forecast 12 months ahead
# Build three different models and compare performance
# Write a short report on your steps and result. 
#
# Data Source
# Website: 
# File: 
#########################################################################################


# Initialize environment --------------------------------------------------
# CLEAR MEMORY
rm(list=ls())

# General
library(tidyverse)
library(Hmisc)
# Modeling
library(timeDate)
library(caret)
library(lubridate)
library(prophet)
# Parallel Computation
library(parallel)
library(doParallel)
# Visualization
library(stargazer)
library(ggplot2)
library(cowplot)
library(viridis)
library(party)
library(GGally)
library(pROC)
library(knitr)

source("codes/helper.R")

options(digits=3)

#import data
daily_agg<-read.csv("data/clean/swim_work.csv") %>% 
  mutate(date = as.Date(date))

adm_types<-colnames(daily_agg %>% select(matches("adm_.*")))

# dow: 1=Monday, weekend: Sat and Sun.
daily_agg <- daily_agg %>%
  mutate(year = year(date),
         quarter = quarter(date),
         month = factor(month(date)),
         week= factor(week(date)),
         day = day(date)) %>%
  mutate(dow = factor(wday(date, week_start = getOption("lubridate.week.start", 1)))) %>%
  mutate(weekend = factor(as.integer(dow %in% c(6,7))))

daily_agg <- daily_agg %>% 
  mutate(school_off = ((day>15 & month==5 & day <=30) | (month==6 |  month==7) |
                         (day<15 & month==8) | (day>20 & month==12) ))

daily_agg <- daily_agg %>% 
  mutate(trend = c(1:dim(daily_agg)[1]))

summary(daily_agg[,adm_types])

# Get holiday calendar ----------------------------------

holidays <-  as.Date(holidayNYSE(2010:2017))

daily_agg <- daily_agg %>% 
  mutate(isHoliday = ifelse(date %in% holidays,1,0))

describe(daily_agg)

# Define vars for analysis ----------------------------------

daily_agg <- daily_agg %>% 
  mutate(adm_core_mod = ifelse(adm_core<1, 1, adm_core),
         ln_adm_core = log(adm_core_mod))

daily_agg <- 
  daily_agg %>% 
  group_by(month) %>% 
  mutate(adm_core_m = mean(adm_core)) %>% 
  ungroup()

adm_types_w<-paste(adm_types,"_w",sep="",collapse =NULL)

daily_agg <- 
  daily_agg %>% 
  group_by(week) %>% 
  mutate_at(vars(adm_types), funs("w"=mean(.))) %>% 
  ungroup() %>% 
  mutate_at(vars(all_of(adm_types_w)), funs("lag"=Lag(.,365)))

daily_agg <- 
  daily_agg %>% 
  group_by(month, dow) %>% 
  mutate(adm_core_md = mean(adm_core),
         ln_adm_core_md = log(adm_core_md)) %>% 
  ungroup()

################################
# Descriptive graphs ----------
#################################

# named date vars for graphs
mydays <- c("Mon","Tue","Wed",
            "Thu","Fri","Sat",
            "Sun")
daily_agg$dow_abb   <- factor(   mydays[daily_agg$dow],  levels=mydays)
daily_agg$month_abb <- factor(month.abb[daily_agg$month],levels=month.abb)

# Graphs
g1 <-ggplot(data=daily_agg[daily_agg$year==2015,], aes(x=date, y=adm_com)) +
  geom_line(size=0.4, color=color[1]) +
  scale_x_date(breaks = as.Date(c("2015-01-01","2015-04-01","2015-07-01","2015-10-01","2016-01-01")),
               labels = date_format("%d%b%Y"),
               date_minor_breaks = "1 month" ) +
  labs(title= "Number of tickets sold in 2015 (Core)", x = "Day", y="Number of tickets sold (Core, Daily)" ) +
  scale_color_discrete(name = "")
g1
# ggsave("out/swimming_pools_2015_admissions_core.png", dpi = 1200)

g2<-ggplot(data=daily_agg[(daily_agg$year>=2010) & (daily_agg$year<=2014),], aes(x=date, y=adm_core)) +
  geom_line(size=0.2, color=color[1]) +
  scale_x_date(breaks = as.Date(c("2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01")),
               labels = date_format("%d%b%Y"),
               minor_breaks = "3 months") +
  labs(title= "Number of tickets sold 2010-2014 (Core)", x = "Day", y="Number of tickets sold (Core, Daily)" ) +
  scale_color_discrete(name = "")
g2
# ggsave("out/swimming_pools_2010_2014_admissions_core.png", dpi = 1200)

g3<-ggplot(data=daily_agg, aes(x=month_abb, y=adm_core)) +
  labs(title= "Number of tickets sold by month (Core)", x = "Month", y="Number of tickets sold (Core)" ) +
  geom_boxplot(color=color[1],outlier.color = color[4], outlier.alpha = 0.6, outlier.size = 0.6)
g3
# ggsave("out/swimming_pools_monthly_admissions_core.png", dpi = 1200)

g4<-ggplot(data=daily_agg, aes(x=dow_abb, y=adm_core)) +
  labs(title= "Number of tickets sold by day of the week (Core)", x = "Day of the week", y="Number of tickets sold (Core)" ) +
  geom_boxplot(color=color[1],outlier.color = color[4], outlier.alpha = 0.6, outlier.size = 0.4)
#geom_boxplot(color=color[1], outlier.shape = NA)
g4
ggsave("out/swimming_pools_dow_admissions_core.png", dpi = 1200)

# to check for interactions, look at the heatmap
swim_heatmap <- 
  ggplot(daily_agg, aes(x = dow_abb, y = month_abb, fill = adm_core_md)) +
  geom_tile(colour = "white") +
  labs(x = 'Day of the week', y = 'Month') +
  scale_fill_viridis(alpha = 0.7, begin = 1, end = 0.2, direction = 1, option = "A") +
  theme(legend.position = "right",
        legend.text = element_text(size=6),
        legend.title =element_text(size=6)
  )
swim_heatmap

ggsave("out/swimming_pools_md_heatmap_admissions_core.png", dpi = 1200)

# not in book
swim_heatmap_log <-
  ggplot(daily_agg, aes(x = dow_abb, y = month_abb, fill = ln_adm_core_md)) +
  geom_tile(colour = "white") +
  labs(x = 'Day of week', y = 'Month ') +
  scale_fill_viridis(alpha = 0.7, begin = 1, end = 0.2, direction = 1, option = "A")
swim_heatmap_log
ggsave("out/swimming_pools_md_heatmap_admissions_core.png", dpi = 1200)

#####################################
# PREDICTION  ----------
#####################################

# Create train/houldout data
# Last year of data
data_holdout<- daily_agg %>%
  filter(year==2016)

# Rest of data for training
data_train <- daily_agg %>%
  filter(year<2016)

# Prepare for cross-validation
data_train <- data_train %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.integer(rowname))

test_index_list <- data_train %>% 
  split(f = factor(data_train$year)) %>% 
  lapply(FUN = function(x){x$rowname})

train_index_list <- test_index_list %>% 
  lapply(FUN = function(x){setdiff(data_train$rowname, x)})

train_control <- trainControl(
  method = "cv",
  index = train_index_list, #index of train data for each fold
  # indexOut = index of test data for each fold, complement of index by default
  # indexFinal = index of data to use to train final model, whole train data by default
  savePredictions = TRUE
)

# Fit models ---------------------------------------------------------

#Model 1 linear trend + monthly seasonality
model1 <- as.formula(adm_core ~ 1 + trend + month)
reg1 <- train(
  model1,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 2 linear trend + monthly seasonality + days of week seasonality 
model2 <- as.formula(adm_core ~ 1 + trend + month + dow)
reg2 <- train(
  model2,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 3 linear trend + monthly seasonality + days of week  seasonality + holidays 
model3 <- as.formula(adm_core ~ 1 + trend + month + dow + isHoliday)
reg3 <- train(
  model3,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 4 linear trend + monthly seasonality + days of week  seasonality + holidays + sch*dow
model4 <- as.formula(adm_core ~ 1 + trend + month + dow + isHoliday + school_off*dow)
reg4 <- train(
  model4,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 5 linear trend + monthly seasonality + days of week  seasonality + holidays + interactions
model5 <- as.formula(adm_core ~ 1 + trend + month + dow + isHoliday + isHoliday*dow + school_off*dow + weekend*month)
reg5 <- train(
  model5,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 6 =  multiplicative trend and seasonality (ie take logs, predict log values and transform back with correction term)
model6 <- as.formula(ln_adm_core ~ 1 + trend + month + dow + isHoliday + school_off*dow)
reg6 <- train(
  model6,
  method = "lm",
  data = data_train,
  trControl = train_control
)

