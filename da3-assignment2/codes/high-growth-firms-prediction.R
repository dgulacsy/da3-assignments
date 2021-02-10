#########################################################################################
# DA3 Assignment 2 
# Analysis/Prediction Script
# 
# GOAL:of high-growth firms
# Build a model that assigns a probability of fast growth of a firm in the next two years. 
# The target variable is profit_loss_year
# Build a price prediction model.
# Maintain profits over two-year period. 
# Only count positive profits.
# 2010-2012 train and test with CV with a holdout sample 
# 2013-2015 external validity
# 
#
# Data Source
# Website: https://osf.io/b2ft9/
# File: cs_bisnode_panel.csv
#########################################################################################

# Initialize environment --------------------------------------------------
# CLEAR MEMORY
rm(list=ls())

# General
library(tidyverse)
library(Hmisc)
# Modelling

# Visualization
library(ggplot2)
library(stargazer)
library(xtable)
library(knitr)
library(skimr)

source("codes/helper.R")

options(digits=3)

# Import data -------------------------------------------------------------
df<-read_csv("data/raw/cs_bisnode_panel.csv")
skim(df)

# Create sample to experiment
#df<-df[sample(nrow(df),2000),]

# Variable Selection -------------------------------------------
# Create Variable Selection Schema File
vss(df)

# Import file containing variable selection strategy
vss <- read_csv("data/variable_selection_schema.csv")

# Select variables
df<-df[ , (names(df) %in% vss[vss$use==1,"vars"]$vars)]

# Rename columns
names(df)[names(df) == 'profit_loss_year'] <- 'profit'

# Label Engineering -------------------------------------------------------
# add all missing year and comp_id combinations -
df <- df %>%
  complete(year, comp_id)

# generate dummy var is_fg (fast growth); if sales larger than zero and not-NA, then firm is alive
df  <- df %>%
  mutate(status_alive = sales > 0 & !is.na(sales) %>%
           as.numeric(.))

# generate dummy var is_fg (fast growth)
df <- df %>%
  group_by(comp_id) %>%
  mutate(is_fg = ((status_alive == 1) & (lead(status_alive, 2) == 1) & (lead(profit, 2)-profit)/abs(profit)>0.1) %>%
           as.numeric(.)) %>%
  ungroup()

# Examples
# The company grows large sometimes
df[df$comp_id=="1014183",c("year","profit","is_fg","status_alive","sales")]
# The company goes bankrupt losses are decreasing but without any sales
df[df$comp_id=="1001034",c("year","profit","is_fg","status_alive","sales")]

# Filter the data where we have an outcome variable
df <- df %>%
  filter(year <=2013)

# Feature Engineering Part 1 -----------------------------------------------------
# Add different transformations of sales variable
df <- df %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales = ifelse(sales > 0, log(sales), 0),
         sales_mil=sales/1000000,
         sales_mil_log = ifelse(sales > 0, log(sales_mil), 0))

# Add log sales difference variable
df <- df %>%
  group_by(comp_id) %>%
  mutate(d1_sales_mil_log = sales_mil_log - Lag(sales_mil_log, 1) ) %>%
  ungroup()

# replace w 0 for new firms + add dummy to capture it
df <- df %>%
  mutate(age = (year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .),
         d1_sales_mil_log = ifelse(new == 1, 0, d1_sales_mil_log),
         new = ifelse(is.na(d1_sales_mil_log), 1, new),
         d1_sales_mil_log = ifelse(is.na(d1_sales_mil_log), 0, d1_sales_mil_log))

# Sample Design -----------------------------------------------------
df <- df %>%
  filter((year == 2012) & (status_alive == 1)) %>%
  # look at firms below 10m euro revenues and above 1000 euros
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.001))

describe(df$is_fg)

# Feature Engineering Part 2 -----------------------------------------------------

# General Firm Characteristics --------------------------------------------
# Aggregate Industry codes into categories
# change some industry category codes
df <- df %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )

# Firm characteristics
df <- df %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))



# Financial indicators, ratios --------------------------------------------
# assets can't be negative. Change them to 0 and add a flag.
df <-df  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(df$flag_asset_problem)

# replace negative asset values with 0
df <- df %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

# generate total assets
df <- df %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(df$total_assets_bs)


# Generate Ratios
pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# divide all pl_names elements by sales and create new column for it
df <- df %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
df <- df %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))



# Creating flags, and winsorizing tails -----------------------------------

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

df <- df %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))

# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_pl", "share_eq_bs")

df <- df %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))

# dropping flags with no variation
variances<- df %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

df <- df %>%
  select(-one_of(names(variances)[variances]))


# Extra Imputed Features ----------------------------------------------------------

# CEO age
df <- df %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

df <- df %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# create factors
df <- df %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(df$ind2_cat))))

df <- df %>%
  mutate(f_is_fg = factor(is_fg, levels = c(0,1)) %>%
           recode(., `0` = 'no fast growth', `1` = "fast growth"))



# export final df ---------------------------------------------------------

write_csv(df, "data/clean/bisnode_clean.csv")
