rm(list =ls())

source('scripts/functions.R')

# disable the scientific formatting of numbers
options(scipen = 999)

# 1. load packages ===============================================================================================
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

suppressMessages(library(shiny))

# 2. inputs =======================================================================================================
total_budget <- 10000000

# increment <- 1000 #optional input
  
# read in products, their current spend and return, diminishing returns, max and min budgets
dt <- read.csv('data/dummy_input_small.csv', stringsAsFactors = F)
# ROI <- dt$Current.Return / dt$Current.Spend

# increment will be set to generate 1000 iterations by default, unless specified
increment <- (total_budget - sum(dt$Minimum.Budget))/1000
# increment <- 1000

# 3. response curves ===============================================================================================

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
# system.time(budgetAllocation(dt, marRet, total_budget, increment))

spend_iterations <- allocation$spend


# 5. charting ===============================================================================================
# 5.1. colour palette =======================================================================================

# number of investments
nInvestments <- length(unique(dt$Investment))
nInvestments_cat1 <- length(unique(dt$Category1))
nInvestments_cat2 <- length(unique(dt$Category2))

colours <- genColours("Set2", nInvestments)
colours_cat1 <- genColours("Set1", nInvestments_cat1)
colours_cat2 <- genColours("Set3", nInvestments_cat2)


# 5.2. clean data for ch ====================================================================================
# 5.2.1. calcs for stacked ch ===============================================================================
spend_iterations_melt <- melt(spend_iterations, id.vars = "totalSpend")

# rename colnames appropriately
colnames(spend_iterations_melt) <- c('totalSpend', 'investment', 'investmentSpend')

# add category1 and category2 to spend_iterations_melt
spend_iterations_melt$cat1 <- dt$Category1[match(spend_iterations_melt$investment, dt$Investment)]
spend_iterations_melt$cat2 <- dt$Category2[match(spend_iterations_melt$investment, dt$Investment)]

# assign levels to investment/category names going by original order (i.e. in dt)
spend_iterations_melt$investment <- factor(spend_iterations_melt$investment, levels=dt$Investment)
spend_iterations_melt$cat1 <- factor(spend_iterations_melt$cat1, levels=unique(dt$Category1))
spend_iterations_melt$cat2 <- factor(spend_iterations_melt$cat2, levels=unique(dt$Category2))

# replace '_' with '-' for better labelling
levels(spend_iterations_melt$investment) <- gsub('_', "-", levels(spend_iterations_melt$investment), fixed=T)

# calculate percentage of investment spend relative to the total (for chart labels)
# create placeholder columns for these percentages by setting it equal to spend
cat1_sum <- aggregate(data=spend_iterations_melt, investmentSpend~totalSpend+cat1, FUN=sum)
cat2_sum <- aggregate(data=spend_iterations_melt, investmentSpend~totalSpend+cat2, FUN=sum)
colnames(cat1_sum)[3] <- "pc_cat1"
colnames(cat2_sum)[3] <- "pc_cat2"
spend_iterations_melt$pc <- spend_iterations_melt$investmentSpend

# introduce pc columns for cat1 & cat2
spend_iterations_melt <- merge(spend_iterations_melt, cat1_sum, by=c("totalSpend", "cat1"))
spend_iterations_melt <- merge(spend_iterations_melt, cat2_sum, by=c("totalSpend", "cat2"))

# calculate percentages
new_pc <- sapply(spend_iterations_melt[c('pc', 'pc_cat1', 'pc_cat2')], function(x) {
  do.call("paste0", c('(',
                      list(round((x / (spend_iterations_melt$totalSpend))*100, digits=1)),
                      '%)'))
  })

# replace pc columns with percentages
spend_iterations_melt[c('pc', 'pc_cat1', 'pc_cat2')] <- new_pc


# 5.2.2. calcs for pie ch ===================================================================================
# take the tranpose of spend_final to put in long format
spend_final <- spend_iterations_melt[spend_iterations_melt$totalSpend == total_budget, ]
spend_final <- spend_final[order(spend_final$investment), ]

# assign cat1 and cat2 colours for each row so colours can be displayed correctly
pieColours_cat1 <- colours_cat1
names(pieColours_cat1) <- levels(spend_final$cat1)
spend_final$colours_cat1 <- pieColours_cat1[match(spend_final$cat1, names(pieColours_cat1))]

pieColours_cat2 <- colours_cat2
names(pieColours_cat2) <- levels(spend_final$cat2)
spend_final$colours_cat2 <- pieColours_cat2[match(spend_final$cat2, names(pieColours_cat2))]

# use separate data for cat1 and cat2, order by category level
spend_final_cat1 <- aggregate(data=spend_final, investmentSpend~cat1+pc_cat1, FUN=sum)
spend_final_cat1 <- spend_final_cat1[order(spend_final_cat1$cat1), ]

spend_final_cat2 <- aggregate(data=spend_final, investmentSpend~cat2+pc_cat2, FUN=sum)
spend_final_cat2 <- spend_final_cat2[order(spend_final_cat2$cat2), ]

# 5.3. plot charts ==========================================================================================
# 5.3.1. stacked charts =====================================================================================
# menu where user can navigate across categories
menu_stacked <- list(
  type = "buttons",
  direction = "down",
  xanchor = 'right',
  yanchor = "top",
  bgcolor='white',
  font=list(color='grey'),
  pad = list('r'= 0, 't'= 10, 'b' = 10),
  x = 0.1,
  y = 0.969,
  buttons = list(
    
    list(method = "restyle",
         args = list("visible", c(rep(list(T), nInvestments),
                                  rep(list(F), nInvestments_cat1),
                                  rep(list(F), nInvestments_cat2))),
         label = "All Categories"),
    
    list(method = "restyle",
         args = list("visible", c(rep(list(F), nInvestments),
                                  rep(list(T), nInvestments_cat1),
                                  rep(list(F), nInvestments_cat2))),
         label = "Subcategory 1"),
    
    list(method = "restyle",
         args = list("visible", c(rep(list(F), nInvestments),
                                  rep(list(F), nInvestments_cat1),
                                  rep(list(T), nInvestments_cat2))),
         label = "Subcategory 2")
  ))

# add a diagonal watermark (serves as title too)
watermark <- list(
  x = 0.5*total_budget,
  y = 0.6*total_budget,
  text = 'Optimal Spend Combinations',
  xref = "x",
  yref = "y",
  showarrow = F,
  font = list(color='#DCDCDC', size=48),
  opacity = 0.8,
  textangle = -45
  # -atan(total_budget/(total_budget-sum(dt$Minimum.Budget)))*(180/pi)
)

# create set of stacked charts
stacked_ch <- plot_ly(spend_iterations_melt, x = ~totalSpend, y = ~investmentSpend, type = 'bar',
                      # name = ~investment,
                      color = ~investment, colors = c(colours, colours_cat1, colours_cat2),
                      text = ~paste(prettyNum(investmentSpend, big.mark=','), pc),
                      hoverinfo='x+text+name', showlegend=F,
                      visible=T) %>%
  add_trace(spend_iterations_melt, x = ~totalSpend, y = ~investmentSpend, type = 'bar',
            color = ~cat1,
            text = ~paste(prettyNum(investmentSpend, big.mark=','), pc_cat1),
            hoverinfo='x+text+name', showlegend=F,
            visible=F) %>%
  add_trace(spend_iterations_melt, x = ~totalSpend, y = ~investmentSpend, type = 'bar',
            color = ~cat2,
            text = ~paste(prettyNum(investmentSpend, big.mark=','), pc_cat2),
            hoverinfo='x+text+name', showlegend=F,
            visible=F) %>%
  layout(yaxis = list(title='', spikecolor='grey', spikethickness=0.1, spikedash='solid', showgrid=F, showline=F, showticklabels=F),
         xaxis = list(title = '', spikecolor='grey', spikethickness=0.1, spikedash='solid', tickfont=list(size=18, color='grey')),
         barmode = 'stack',
         hovermode = 'compare', hoverlabel = list(bordercolor='white', namelength=-1),
         legend = list(orientation = 'h'),
         annotations = list(watermark),
         updatemenus = list(menu_stacked))

stacked_ch

# 5.2.2. pie charts ======================================================================================
# menu where user can navigate across categories
menu_pie <- list(
  type = "buttons",
  direction = "down",
  xanchor = 'right',
  yanchor = "top",
  bgcolor='white',
  font=list(color='grey'),
  pad = list('r'= 0, 't'= 10, 'b' = 10),
  x = 0.1,
  y = 0.969,
  buttons = list(
    
    list(method = "restyle",
         args = list("visible", list(T,F,F)),
         label = "All Categories"),
    
    list(method = "restyle",
         args = list("visible", list(F,T,F)),
         label = "Subcategory 1"),
    
    list(method = "restyle",
         args = list("visible", list(F,F,T)),
         label = "Subcategory 2")
  ))

# custom title
title_pie <- list(
  x = '',
  y = '',
  text = paste('Optimal allocation for your budget of', prettyNum(total_budget, big.mark=',')),
  textposition = 'outside',
  xref = "paper",
  yref = "paper",
  showarrow = F,
  font = list(color='grey', size=29.5)
)

# create set of stacked charts
pie_ch <- plot_ly(spend_final, labels = ~investment, values = ~investmentSpend, type = 'pie',
                  textposition = 'inside',
                  text = ~paste(paste(prettyNum(investmentSpend, big.mark=','), '<br>'), pc),
                  textinfo = 'text+label',
                  insidetextfont = list(color='#FFFFFF'),
                  hoverinfo = 'text+label',
                  marker = list(colors=c(colours),
                                line=list(color='#FFFFFF', width=1)),
                  showlegend = F,
                  visible = T) %>% 
  add_trace(spend_final_cat1, labels = ~cat1, values = ~investmentSpend, type = 'pie',
            textposition = 'inside',
            text = ~paste(paste(prettyNum(investmentSpend, big.mark=','), '<br>'), pc_cat1),
            textinfo = 'text+label',
            insidetextfont = list(color='#FFFFFF'),
            hoverinfo = 'text+label',
            marker = list(
              colors=spend_final$colours_cat1,
              line=list(color='#FFFFFF', width=1)),
            showlegend = F,
            visible = F) %>% 
  add_trace(spend_final_cat2, labels = ~cat2, values = ~investmentSpend, type = 'pie',
            textposition = 'inside',
            text = ~paste(paste(prettyNum(investmentSpend, big.mark=','), '<br>'), pc_cat2),
            textinfo = 'text+label',
            insidetextfont = list(color='#FFFFFF'),
            hoverinfo = 'text+label',
            marker = list(
              colors=spend_final$colours_cat2,
              line=list(color='#FFFFFF', width=1)),
            showlegend = F,
            visible = F) %>% 
  layout(title = '',
         titlefont = list(color='grey'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         hoverlabel = list(bordercolor='white', namelength=-1),
         annotations = title_pie,
         updatemenus = list(menu_pie))

pie_ch