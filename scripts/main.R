rm(list =ls())

source('scripts/functions.R')

# Load packages ===============================================================================================
suppressMessages(library(reshape2))
suppressMessages(library(stringr))
suppressMessages(library(dplyr))
suppressMessages(library(zoo))

# Inputs =======================================================================================================
# Optimisation period
# annual <- 52; quaterly <- 12; monthly <- 4
# opt_period <- annual

total_budget <- 30000000

# Read in products, their current spend and return, diminishing returns, max and min budgets
dt <- read.csv('data/dummy_input.csv', stringsAsFactors = FALSE)
# ROI <- dt$Current.Return / dt$Current.Spend

# May add in total constraints for Type 1 and Type 2

# Defaults
# CHANGEEEEEEEEEEEEEE
increment <- 1000

# DIMINISHING RETURNSSSSSSS

# Response curves ===============================================================================================
dt <- cbind(Investment=paste0(dt$Type1, dt$Type2), dt, stringsAsFactors = FALSE)

# First calculate the multiplier that will stretch the curve so that it crosses our current spend and return point
dt$Beta <- dt$Current.Return / (1-exp(-dt$Current.Spend/dt$Current.Spend))

Curves <- apply(dt, 1, function(x) createCurves(x["Minimum.Budget"], total_budget, increment, x["Current.Spend"], x["Beta"]))

cumRet <- data.frame(lapply(1:length(Curves), function(x) do.call("cbind", Curves[[x]]["cumRet"])))
colnames(cumRet) <- dt$Investment

marRet <- data.frame(lapply(1:length(Curves), function(x) do.call("cbind", Curves[[x]]["marRet"])))
colnames(marRet) <- dt$Investment
