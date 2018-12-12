rm(list =ls())

source('scripts/functions.R')

# disable the scientific formatting of numbers
options(scipen = 999)

# 1. Load packages ===============================================================================================
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

# 2. Inputs =======================================================================================================
total_budget <- 5000000

# increment <- 1000 #optional input
  
# read in products, their current spend and return, diminishing returns, max and min budgets
dt <- read.csv('data/dummy_input_small.csv', stringsAsFactors = FALSE)
# ROI <- dt$Current.Return / dt$Current.Spend

# define an increment that determines how many iterations to run
# increment will be set between 0.1% and 1% of budget (going by nearest power of 10), unless specified
increment <-  increment <- 10^floor(log10(total_budget)) * 0.01
# increment <- 1000

# 3. Response curves ===============================================================================================

# define a unique "Investment" name - a concatenation of Category1 and Category2
dt <- cbind(Investment=paste(dt$Category1, dt$Category2, sep='_'), dt, stringsAsFactors = FALSE)

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

# Plot curves - atm there are too many curves, may have to run this on smaller dataset
cumRet_melt <- cbind(spend=seq(0, total_budget, increment), cumRet)
cumRet_melt <- melt(cumRet_melt, id.vars = "spend")
ggplot(cumRet_melt, aes(x=spend, y=value)) +
  geom_line(aes(group=variable, color=variable), show.legend = FALSE) +
  theme_minimal()

# compile all marginal response curves
marRet <- data.frame(lapply(1:length(Curves), function(x) do.call("cbind", Curves[[x]]["marRet"])))
colnames(marRet) <- dt$Investment


# 4. spend allocation =======================================================================================

# if there is no budget constraint for investment, assign the total budget as its maximum budget
dt$Maximum.Budget[is.na(dt$Maximum.Budget)] <- total_budget

# allocate budget
allocation <- budgetAllocation(dt, marRet, total_budget)
# budgetAllocation(dt, marRet, total_budget)
# system.time(budgetAllocation(dt, marRet, total_budget, increment))
spend_iterations <- allocation$spend
spend_final <- tail(spend_iterations[-1], 1)


# 5. charting ===============================================================================================
# 5.1. clean data for ch ====================================================================================
# melt data to be used for ggplot
spend_iterations_melt <- melt(spend_iterations, id.vars = "totalSpend")

# rename colnames appropriately
colnames(spend_iterations_melt) <- c('totalSpend', 'investment', 'investmentSpend')

# add category1 and category2
spend_iterations_melt$cat1 <- dt$Category1[match(spend_iterations_melt$investment, dt$Investment)]
spend_iterations_melt$cat2 <- dt$Category2[match(spend_iterations_melt$investment, dt$Investment)]

spend_iterations_melt$investment <- gsub('_', "-", spend_iterations_melt$investment)

# percentage of spend for investment relative to the total (for chart labels)
cat1_sum <- aggregate(data=spend_iterations_melt, investmentSpend~totalSpend+cat1, FUN=sum)
cat2_sum <- aggregate(data=spend_iterations_melt, investmentSpend~totalSpend+cat2, FUN=sum)
colnames(cat1_sum)[3] <- "pc_cat1"
colnames(cat2_sum)[3] <- "pc_cat2"

spend_iterations_melt$pc <- spend_iterations_melt$investmentSpend

spend_iterations_melt <- merge(spend_iterations_melt, cat1_sum, by=c("totalSpend", "cat1"))
spend_iterations_melt <- merge(spend_iterations_melt, cat2_sum, by=c("totalSpend", "cat2"))

new_pc <- sapply(spend_iterations_melt[c('pc', 'pc_cat1', 'pc_cat2')], function(x) {
  do.call("paste0", c('(',
                      list(round((x / (spend_iterations_melt$totalSpend+sum(dt$Minimum.Budget)))*100)),
                      '%)'))
  })

spend_iterations_melt[c('pc', 'pc_cat1', 'pc_cat2')] <- new_pc

nInvestments <- length(unique(dt$Investment))
nInvestments_cat1 <- length(unique(dt$Category1))
nInvestments_cat2 <- length(unique(dt$Category2))

# 5.2. charts ===============================================================================================

# define colour palette
theme_colours <-  brewer.pal(8,name="Set2")
n_colours <- colorRampPalette(theme_colours)
colours <- n_colours(nInvestments)

menu <- list(
  type = "buttons",
  direction = "down",
  xanchor = 'right',
  yanchor = "top",
  bgcolor='white',
  font=list(color='grey'),
  pad = list('r'= 0, 't'= 10, 'b' = 10),
  x = 0.2,
  y = 0.969,
  buttons = list(
    
    list(method = "restyle",
         args = list("visible", c(rep(list(T), nInvestments),
                                  rep(list(F), nInvestments_cat1),
                                  rep(list(F), nInvestments_cat2))),
         label = "All"),
    
    list(method = "restyle",
         args = list("visible", c(rep(list(F), nInvestments),
                                  rep(list(T), nInvestments_cat1),
                                  rep(list(F), nInvestments_cat2))),
         label = "Main Category"),
    
    list(method = "restyle",
         args = list("visible", c(rep(list(F), nInvestments),
                                  rep(list(F), nInvestments_cat1),
                                  rep(list(T), nInvestments_cat2))),
         label = "Subcategory")
  ))

a <- list(
  x = m$wt,
  y = m$mpg,
  text = rownames(m),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

plot_ly(spend_iterations_melt, x = ~totalSpend, y = ~investmentSpend, type = 'bar',
        # name = ~investment,
        color = ~investment, colors = colours,
        text = ~paste(prettyNum(investmentSpend, big.mark=','), pc),
        hoverinfo='x+text+name', showlegend=F,
        visible=T) %>%
  add_trace(spend_iterations_melt, x = ~totalSpend, y = ~investmentSpend, type = 'bar',
            # name = ~investment,
            color = ~cat1, colors = colours,
            text = ~paste(prettyNum(investmentSpend, big.mark=','), pc_cat1),
            hoverinfo='x+text+name', showlegend=F,
            visible=F) %>%
  add_trace(spend_iterations_melt, x = ~totalSpend, y = ~investmentSpend, type = 'bar',
            # name = ~investment,
            color = ~cat2, colors = colours,
            text = ~paste(prettyNum(investmentSpend, big.mark=','), pc_cat2),
            hoverinfo='x+text+name', showlegend=F,
            visible=F) %>%
  layout(title = 'Optimal Budget Allocation across Spend',
         yaxis = list(title = 'Spend', spikecolor='grey', spikethickness=0.1, spikedash='solid', showgrid=F),
         xaxis = list(title = 'Spend', spikecolor='grey', spikethickness=0.1, spikedash='solid'),
         barmode = 'stack',
         hovermode = 'compare', hoverlabel = list(bordercolor='white', namelength=-1),
         legend = list(orientation = 'h'),
         updatemenus = list(menu))
