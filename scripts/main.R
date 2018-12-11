rm(list =ls())

source('scripts/functions.R')

# disable the scientific formatting of numbers
options(scipen = 999)

# Load packages ===============================================================================================
suppressMessages(library(reshape2))
suppressMessages(library(stringr))
suppressMessages(library(dplyr)) # uninstall
suppressMessages(library(zoo))

# graphing libraries
suppressMessages(library(ggplot2))
suppressMessages(library(directlabels)) # uninstall
suppressMessages(library(ggrepel))
suppressMessages(library(RColorBrewer))
suppressMessages(library(plotly))

# Inputs =======================================================================================================
total_budget <- 5000000

# increment <- 1000 #optional input
  
# read in products, their current spend and return, diminishing returns, max and min budgets
dt <- read.csv('data/dummy_input_small.csv', stringsAsFactors = FALSE)
# ROI <- dt$Current.Return / dt$Current.Spend


# Response curves ===============================================================================================

# define a unique "Investment" name - a concatenation of Category1 and Category2
dt <- cbind(Investment=paste0(dt$Category1, dt$Category2), dt, stringsAsFactors = FALSE)

# define the alpha that will determine the steepness of the curve (i.e. how quickly we hit dimishing returns)
# usually this would be the current spend, unless user specifies the diminishing returns value
dt <- transform(dt, Alpha = ifelse(is.na(Diminishing.Return), 
                                   Current.Return, 
                                   Diminishing.Return))

# calculate the multiplier that will stretch the curve so that it crosses our current spend and return point
dt$Beta <- dt$Current.Return / (1-exp(-dt$Current.Spend/dt$Alpha))

# run function that will generate output that contains both the cumulative response and marginal response 
Curves <- apply(dt, 1, function(x) createCurves(x["Minimum.Budget"], total_budget, x["Alpha"], x["Beta"]))

# compile all cumulative response curves
cumRet <- data.frame(lapply(1:length(Curves), function(x) do.call("cbind", Curves[[x]]["cumRet"])))
colnames(cumRet) <- dt$Investment

# # Plot curves - atm there are too many curves, may have to run this on smaller dataset
# cumRet_melt <- cbind(spend=seq(0, total_budget, increment), cumRet)
# cumRet_melt <- melt(cumRet_melt, id.vars = "spend")
# ggplot(cumRet_melt, aes(x=spend, y=value)) +
#   geom_line(aes(group=variable, color=variable), show.legend = FALSE) +
#   theme_minimal()

# compile all marginal response curves
marRet <- data.frame(lapply(1:length(Curves), function(x) do.call("cbind", Curves[[x]]["marRet"])))
colnames(marRet) <- dt$Investment


# spend allocation =======================================================================================

# if there is no budget constraint for investment, assign the total budget as its maximum budget
dt$Maximum.Budget[is.na(dt$Maximum.Budget)] <- total_budget

# allocate budget
allocation <- budgetAllocation(dt, marRet, total_budget)
# budgetAllocation(dt, marRet, total_budget)
# system.time(budgetAllocation(dt, marRet, total_budget, increment))
spend_iterations <- allocation$spend
spend_final <- tail(spend_iterations[-1], 1)


# charting ===============================================================================================

# melt data to be used for ggplot
spend_iterations_melt <- melt(spend_iterations, id.vars = "totalSpend")
# colnames(spend_iterations_melt) <- c("Total Spend", "Investment", "Spend")

# percentage of spend for investment relative to the total (for chart labels)
spend_iterations_melt$pc <- paste0('(', 
                                   paste0(round((spend_iterations_melt$value / (spend_iterations_melt$totalSpend+sum(dt$Minimum.Budget)))*100), 
                                   '%)'))

theme_colours <-  brewer.pal(12,name="Paired")
n_colours <- colorRampPalette(theme_colours)
colours <- n_colours(nrow(dt))

plot_ly(spend_iterations_melt, x = ~totalSpend, y = ~value, type = 'bar',
        name = ~variable, color = ~variable, colors = colours, showlegend=FALSE, 
        text = ~paste(prettyNum(value, big.mark=','), pc), hoverinfo='x+text+name') %>%
  layout(yaxis = list(title = 'Spend', spikecolor='grey', spikethickness=0.1, spikedash='solid'), 
         xaxis = list(title = 'Spend', spikecolor='grey', spikethickness=0.1, spikedash='solid'), 
         barmode = 'stack', 
         hovermode = 'compare', hoverlabel = list(bordercolor='white', namelength=-1))