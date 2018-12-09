rm(list =ls())

suppressMessages(library(reshape2))
suppressMessages(library(stringr))
suppressMessages(library(dplyr))
suppressMessages(library(zoo))

# Inputs ====================================================================
# Optimisation period
# annual <- 52; quaterly <- 12; monthly <- 4
# opt_period <- annual

dt <- read.csv('data/dummy_input.csv')
# ROI <- dt$Current.Return / dt$Current.Spend

total_budget <- 30000000

# Defaults
increment <- 1000

# ==================
