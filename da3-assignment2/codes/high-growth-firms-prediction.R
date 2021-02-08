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
# Visualization
library(ggplot2)
library(stargazer)
library(xtable)
library(knitr)
library(skimr)

options(digits=3)

# Import data -------------------------------------------------------------
df<-read_csv("data/raw/cs_bisnode_panel.csv")
skim(df)

# Feature Selection -------------------------------------------
fss_df<-data.frame(vars=colnames(df),example=as.character(df[1,]),use=NA,notes=NA)
write_csv(fss_df,"data/raw/feature_selection_schema.csv",na = "")

# Import file containing feature selection strategy
fss <- read_csv(paste0(data_dir,"raw/feature_selection_strat.csv"))

