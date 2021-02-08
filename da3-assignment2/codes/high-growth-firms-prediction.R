#########################################################################################
# DA3 Assignment 2 
# Analysis/Prediction Script
# 
# GOAL:
# of high-growth firms
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

source("codes/helper.R")

options(digits=3)

# Import data -------------------------------------------------------------
df<-read_csv("data/raw/cs_bisnode_panel.csv")
skim(df)

# Feature Selection -------------------------------------------
# Create Feature Selection Schema File
fss(df)

# Import file containing feature selection strategy
fss <- read_csv("data/raw/feature_selection_schema.csv")

# Import file containing feature selection strategy
fss <- read_csv(paste0(data_dir,"raw/feature_selection_strat.csv"))
unique(df$year)
table(df$year)
