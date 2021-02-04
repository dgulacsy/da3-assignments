#########################################################################################
# DA3 Assignment 2 
# Analysis/Prediction Script
# 
# GOAL:of high-growth firms
# Task is to help a company operating small and mid-size apartments hosting 2-6 guests. 
# The company is set to price their new apartments not on the market. 
# Build a price prediction model.
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

