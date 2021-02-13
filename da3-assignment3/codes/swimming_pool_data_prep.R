#########################################################################################
# DA3 Assignment 3
# Data Analysis Script
# 
# GOAL:
# Use the ABQ swimming pool data
# Also shared in google drive we used in DA2
# Find all the open air pools and combine sales
# Clean and filter as you see fit -- create a daily data
# Build a daily predictive model that can forecast 12 months ahead
# Build three different models and compare performance
# Write a short report on your steps and result. 
#
# Data Source
# Website: https://osf.io/s3ng2/
# File: SwimmingPoolAdmissionsCABQ-en-us.csv
#########################################################################################


# Initialize environment --------------------------------------------------
# CLEAR MEMORY
rm(list=ls())

# Import libraries ---------------------------------------------------
# General
library(tidyverse)
library(Hmisc)
library(janitor)
# Date handling
library(lubridate)
library(timeDate)
# Modeling
library(caret)
# Viz
library(ggplot2)

source("codes/helper.R")

options(digits=3)

#############################################
# Data Cleaning & Wrangling
#############################################

# Load raw data ------------------------------------------------------

raw <- as.data.frame(read.table("data/raw/SwimmingPoolAdmissionsCABQ-en-us.csv",
                                sep = "\t",
                                header = TRUE,
                                fileEncoding = "UCS-2LE",
                                strip.white = TRUE))


# Filter data, create workfile --------------------------------------------------------

# Outdoor pools
outdoor_pools<- c("AQEJ01","AQEI01","AQMP01","AQRG01","AQSP01","AQSV01","AQWP01")

data <- raw %>% 
  clean_names()

data <- data %>%
  filter(location %in% outdoor_pools) %>% #from https://www.cabq.gov/parksandrecreation/recreation/swimming/indoor-pools
  filter(category %in% c("ADMISTIER1","ADMISTIER2",
                         "PROMOTIONS","LESSONS","SCHOOL","SPEC EVENT","SWIM TEAM","COMMUNITY")) %>%
  mutate(date = as.Date(date_time, format = "%Y-%m-%d")) 
Hmisc::describe(data$item)

# Item counts by categories
cc_df<-count(data,category,item)
cc_df[order(cc_df$category,-cc_df$n),]
cc_df<-cc_df %>% group_by(category) %>%  slice_max(order_by = n, n = 5)

agg_df1<-data %>% group_by(category, item) %>% summarise(
  quantity=sum(quantity)
)

agg_df2<-data %>% group_by(category, item) %>% summarise(
  quantity=sum(quantity)
) %>%  slice_max(order_by = quantity, n = 5)
names(table(agg_df2[agg_df2$category=="LESSONS","item"]))

ggplot(agg_df2, aes(y=quantity, x=reorder(item,quantity), fill=item)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  facet_wrap(. ~ category ,scales = 'free')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")

ggplot(cc_df, aes(y=n, x=reorder(item,n), fill=item)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  facet_wrap(. ~ category ,scales = 'free')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")

ggplot(agg_df1 %>% filter(category=="ADMISTIER2"), aes(y=quantity, x=reorder(item,-quantity), fill=item)) + 
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")

data <- data %>%
  mutate(adm_core =  (category %in% c("ADMISTIER1","ADMISTIER2") & 
                              item %in%  c("ADULT" , "SENIOR" ,"TEEN" ,"CHILD", "TOT", "SPECTATOR","CHILD PM","ADULT PM","SENIOR PM", "TOT PM", "TEEN PM")),
         adm_com =   (category %in% c("COMMUNITY") &
                        item %in%  names(table(agg_df2[agg_df2$category=="COMMUNITY","item"]))),
         adm_less =  (category %in% c("LESSONS") &
                              item %in%  names(table(agg_df2[agg_df2$category=="LESSONS","item"]))),
         adm_prom =  (category %in% c("PROMOTIONS") &
                              item %in%  names(table(agg_df2[agg_df2$category=="PROMOTIONS","item"]))),
         adm_sch =   (category %in% c("SCHOOL") &
                              item %in%  names(table(agg_df2[agg_df2$category=="SCHOOL","item"]))),
         adm_spec =  (category %in% c("SPEC EVENT") &
                              item %in%  names(table(agg_df2[agg_df2$category=="SPEC EVENT","item"]))),
         adm_swim =  (category %in% c("SWIM TEAM") &
                              item %in%  names(table(agg_df2[agg_df2$category=="SWIM TEAM","item"])))) %>% 
  filter(adm_core | adm_com | adm_less | adm_prom | adm_sch | adm_spec | adm_swim)

adm_types<-c("adm_core","adm_com","adm_less","adm_prom","adm_sch","adm_spec","adm_swim")

data <- data %>%
  mutate_at(vars(adm_types), funs("q"=ifelse(.,quantity,0)))

adm_types_q<-colnames(data %>% select(matches("*._q")))

# Aggregate date to daily freq --------------------------------------
daily_agg <- aggregate(x = data[adm_types_q],
                       FUN = sum,
                       by = list(date = data$date))

# replace missing days with 0 
daily_agg <- daily_agg %>% 
  merge(data.frame(date = seq(from = min(daily_agg[,"date"]), to = max(daily_agg[,"date"]), by = 1)),
        all = TRUE)

daily_agg[is.na(daily_agg)] <- 0

# Create date/time variables ----------------------------------------

# 2010-2016 only full years used. 
daily_agg <- daily_agg %>%
  filter(date >= as.Date("2010-01-01")) %>%
  filter(date < as.Date("2017-01-01"))
Hmisc::describe(daily_agg)

colnames(daily_agg)<-c("date",adm_types)

# Save work file
write.csv(daily_agg,"data/clean/swim_work.csv", row.names = FALSE) 
