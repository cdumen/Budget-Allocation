start_time <- Sys.time()

setwd('/Users/leduong/Documents/Git/Budget-Allocation/app')

source('calcs/functions.R')

# disable the scientific formatting of numbers
options(scipen = 999)

# load packages ----
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
suppressMessages(library(formattable))

# suppressMessages(library(shiny))


# clean input ----

# make column names referenceable
colnames(dt) <- c("Investment", "Category1", "Category2" , "Current.Spend", "Current.Return", 
                  "Weeks","Diminishing.Return", "Minimum.Budget", "Maximum.Budget" )

# if there is no budget constraint for investment, assign the total budget as its maximum budget
dt$Minimum.Budget[is.na(dt$Minimum.Budget)] <- 0
dt$Maximum.Budget[is.na(dt$Maximum.Budget)] <- total_budget

# if sum of individual constraints is smaller than the total budget, set this as total budget
# this really shouldn't happen unless the user misunderstands the purpose of the app!
if(total_budget > sum(dt$Maximum.Budget)) {
  total_budget <- sum(dt$Maximum.Budget)
}

# increment will be set to generate 1000 iterations by default
increment <- (total_budget - sum(dt$Minimum.Budget))/1000

# if no group names given, label as 'Ungrouped'
suppressWarnings(dt$Category1[dt$Category1 == ''] <- 'Ungrouped')
suppressWarnings(dt$Category2[dt$Category2 == ''] <- 'Ungrouped')


# response curves ----

# define the alpha that will determine the steepness of the curve (i.e. how quickly we hit dimishing returns)
# usually this would be (spend/week)*weeks_covered, unless user specifies the diminishing returns spend
dt <- transform(dt, Alpha = ifelse(is.na(Diminishing.Return),
                                   (Current.Spend/Weeks) * weeks_covered,
                                   Diminishing.Return))

# define beta that will stretch the curve so that it crosses our current spend and return point
# beta is also the value that the curves is capped at
dt$Beta <- dt$Current.Return / (1-exp(-dt$Current.Spend/dt$Alpha))

# determine where we want the curves to cut off
max_curve <- (max(total_budget, 
                  dt$Current.Spend,
                  aggregate(data=dt, Current.Spend~Category1, FUN=sum)$Current.Spend, 
                  aggregate(data=dt, Current.Spend~Category2, FUN=sum)$Current.Spend)) + increment

# run function that will generate output that contains both the cumulative response and marginal response
Curves <- apply(dt, 1, function(x) createCurves(x["Minimum.Budget"], x["Alpha"], x["Beta"], increment, max_curve))

# compile all cumulative response curves
cumRet <- data.frame(lapply(1:length(Curves), function(x) do.call("cbind", Curves[[x]]["cumRet"])))
colnames(cumRet) <- dt$Investment

cumRet <- round(cumRet, digits=2)

# melt data for graphing purposes
cumRet_melt <- cbind(spend=round(seq(0, max_curve, increment), digits=2), cumRet)
cumRet_melt <- melt(cumRet_melt, id.vars = "spend")

# add minimum spend
for(i in 1:nrow(dt)) {
  spend_i <- cumRet_melt[cumRet_melt$variable == unique(dt$Investment)[i], 'spend']
  cumRet_melt[cumRet_melt$variable == unique(dt$Investment)[i], 'spend'] <- spend_i + dt[i, 'Minimum.Budget']
}

# compile all marginal response curves
marRet <- data.frame(lapply(1:length(Curves), function(x) do.call("cbind", Curves[[x]]["marRet"])))
colnames(marRet) <- dt$Investment

# melt data
marRet_melt <- cbind(spend=round(seq(increment, max_curve, increment), digits=2), marRet)
marRet_melt <- melt(marRet_melt, id.vars = "spend")


# spend allocation ----

# allocate budget
allocation <- budgetAllocation(dt, marRet, cumRet, total_budget, increment)

spend_iterations <- round(allocation$spend, digits=2)


# charting: colour palette ----

# number of investments
nInvestments <- length(unique(dt$Investment))
nInvestments_cat1 <- length(unique(dt$Category1))
nInvestments_cat2 <- length(unique(dt$Category2))

colours <- genColours("Set2", nInvestments)
colours_cat1 <- genColours("Set1", nInvestments_cat1)
colours_cat2 <- genColours("Set3", nInvestments_cat2)


# charting: stacked ch calcs ----
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


# charting: stacked ch plots ----

# menu where user can navigate across categories
menu_stacked <- list(
  type = "buttons",
  direction = "right",
  xanchor = 'center',
  yanchor = "top",
  bgcolor='white',
  font=list(color='#A9A9A9'),
  pad = list('r'= 0, 't'= 10, 'b' = 10),
  x = 0.5,
  y = 1.01,
  buttons = list(

    list(method = "restyle",
         args = list("visible", c(rep(list(T), nInvestments),
                                  rep(list(F), nInvestments_cat1),
                                  rep(list(F), nInvestments_cat2))),
         label = "All Investments"),

    list(method = "restyle",
         args = list("visible", c(rep(list(F), nInvestments),
                                  rep(list(T), nInvestments_cat1),
                                  rep(list(F), nInvestments_cat2))),
         label = "Group 1"),

    list(method = "restyle",
         args = list("visible", c(rep(list(F), nInvestments),
                                  rep(list(F), nInvestments_cat1),
                                  rep(list(T), nInvestments_cat2))),
         label = "Group 2")
  ))

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
         xaxis = list(title = 'Spend', spikecolor='grey', spikethickness=0.1, spikedash='solid'),
         barmode = 'stack',
         hovermode = 'compare', hoverlabel = list(bordercolor='white', namelength=-1),
         legend = list(orientation = 'h'),
         # annotations = list(watermark),
         updatemenus = list(menu_stacked),
         height=600, width=850,
         margin = list(r = 20)
         )

stacked_ch


# summary table -----

# take the tranpose of spend_final to put in long format
spend_final <- spend_iterations_melt[spend_iterations_melt$totalSpend == total_budget, ]
spend_final <- spend_final[order(spend_final$investment), ]

# take key columns for summary table
summary_table <- spend_final[c('investment','cat1','cat2', 'investmentSpend')]

# look up new return
summary_table <- merge(summary_table, cumRet_melt, by.x=c('investment','investmentSpend'), by.y=c('variable','spend'), all.x = TRUE)

# order back to orginal
summary_table <- summary_table[order(summary_table$investment), ]

# add current spend and return
summary_table <- cbind(summary_table, currentSpend=dt$Current.Spend, currentReturn=dt$Current.Return)

# add optimal spend
summary_table$spendUpTo <- sapply(1:nrow(summary_table), function(x) {
  approx(marRet[, summary_table$investment[x]], unique(marRet_melt$spend), xout=0)$y
})

# convert to factors to charcters for binding
summary_table$investment <- as.character(summary_table$investment)
summary_table$cat1 <- as.character(summary_table$cat1)
summary_table$cat2 <- as.character(summary_table$cat2)

# list of total values
total <- list()
for(i in 1:ncol(summary_table)) {
  if(sapply(summary_table, is.numeric)[i]) {
    total[[i]] <- sum(summary_table[i], na.rm=T)
  } else {
    total[[i]] <- 'Total'
  }
}

# add total as a row
total <- data.frame(total, stringsAsFactors = F)
colnames(total) <- colnames(summary_table)
summary_table <- rbind(summary_table, total)

# add ROIs
summary_table$currentROI <- summary_table$currentReturn / summary_table$currentSpend
summary_table$newROI <- summary_table$value / summary_table$investmentSpend

# add % change
summary_table$spendChange <- (summary_table$investmentSpend - summary_table$currentSpend) / summary_table$currentSpend
summary_table$returnChange <- (summary_table$value - summary_table$currentReturn) / summary_table$currentReturn
summary_table$ROIChange <- (summary_table$newROI - summary_table$currentROI) / summary_table$currentROI

# order columns
summary_table <- summary_table[c('investment', 'cat1', 'cat2',
                                 'currentSpend', 'investmentSpend', 'spendChange',
                                 'currentReturn', 'value', 'returnChange',
                                 'currentROI','newROI','ROIChange','spendUpTo')]

# rename columns
colnames(summary_table) <- c('Investment', 'Group 1', 'Group 2',
                             'Current Spend', 'New Spend', '% Spend Change',
                             'Current Return', 'New Return', '% Return Change',
                             'Current ROI', 'New ROI', '% ROI Change', 'Spend Up To')

# remove rownames
rownames(summary_table) <- NULL


# summary table - all ----
summary_table_all <- summary_table
summary_table_all[c('Group 1', 'Group 2')] <- NULL

# convert NAs in 'Spend Up To' column to 0
summary_table_all[is.na(summary_table_all$`Spend Up To`), 'Spend Up To'] <- 0


# summary table - group 1 ----
# convert to data.table for easier manipulation
summary_table_cat1 <- data.table(summary_table)

# list of columns we want to keep after aggregating
keep_cols <- colnames(summary_table)[4:ncol(summary_table)]

# aggregate by 'Group 1'
summary_table_cat1 <- summary_table_cat1[, lapply(.SD, sum, na.rm=T), by=`Group 1`, .SDcols = keep_cols]

# correct ROIs
summary_table_cat1[, `Current ROI` := `Current Return` / `Current Spend`]
summary_table_cat1[, `New ROI` := `New Return` / `New Spend`]

# correct the percentages
summary_table_cat1[, `% Spend Change` := (`New Spend` - `Current Spend`) / `Current Spend`]
summary_table_cat1[, `% Return Change` := (`New Return` - `Current Return`) / `Current Return`]
summary_table_cat1[, `% ROI Change` := (`New ROI` - `Current ROI`) / `Current ROI`]

# convert back to data.frame
summary_table_cat1 <- data.frame(summary_table_cat1, check.names = F)


# summary table - group 2 ----
# convert to data.table for easier manipulation
summary_table_cat2 <- data.table(summary_table)

# list of columns we want to keep after aggregating
keep_cols <- colnames(summary_table)[4:ncol(summary_table)]

# aggregate by 'Group 2'
summary_table_cat2 <- summary_table_cat2[, lapply(.SD, sum, na.rm=T), by=`Group 2`, .SDcols = keep_cols]

# correct ROIs
summary_table_cat2[, `Current ROI` := `Current Return` / `Current Spend`]
summary_table_cat2[, `New ROI` := `New Return` / `New Spend`]

# correct the percentages
summary_table_cat2[, `% Spend Change` := (`New Spend` - `Current Spend`) / `Current Spend`]
summary_table_cat2[, `% Return Change` := (`New Return` - `Current Return`) / `Current Return`]
summary_table_cat2[, `% ROI Change` := (`New ROI` - `Current ROI`) / `Current ROI`]

# convert back to data.frame
summary_table_cat2 <- data.frame(summary_table_cat2, check.names = F)

# drop summary_table dataframe
rm(summary_table)

# charting: response curves calcs continued ----

# create response curves for cat1 and cat2
cumRet_melt_cat1 <- agg_rspCurves(dt, 'Category1', max_curve, increment)
cumRet_melt_cat2 <- agg_rspCurves(dt, 'Category2', max_curve, increment)

# add these to cumRet_melt
cumRet_melt <- rbind(rbind(cumRet_melt, cumRet_melt_cat1), cumRet_melt_cat2)
rm(cumRet_melt_cat1, cumRet_melt_cat2)

# list of summary tables we want to combine current spend & return
summary_table_names = c('summary_table_all', 'summary_table_cat1', 'summary_table_cat2')

# list to store copies of the summary tables
summary_table_copy <- list()

# loop through each summary table
for(i in 1:length(summary_table_names)) {
  
  # make a copy
  summary_table_copy[[i]] <- eval(parse(text=summary_table_names[i]))
  
  # make colnames the same as first summary table
  colnames(summary_table_copy[[i]]) <- colnames(summary_table_copy[[1]])
  
  # remove total row
  summary_table_copy[[i]] <- summary_table_copy[[i]][-nrow(summary_table_copy[[i]]), ]
  
  # convert group to factor
  summary_table_copy[[i]][, 1] <- factor(summary_table_copy[[i]][, 1], levels=summary_table_copy[[i]][, 1])
  
}

# bind all all the summary tables together
current <- do.call(rbind, summary_table_copy)

# extract key columns
current <- current[c('Investment', 'Current Spend', 'Current Return')]

# remove list
rm(summary_table_copy)


# charting: response curves plots ----
# menu where user can navigate across categories
menu_line <- list(
  type = "buttons",
  direction = "right",
  xanchor = 'center',
  yanchor = "top",
  bgcolor='white',
  font=list(color='#A9A9A9'),
  pad = list('r'= 0, 't'= 10, 'b' = 10),
  x = 0.5,
  y = 1.07,
  buttons = list(

    list(method = "restyle",
         args = list("visible", c(rep(list(T), nInvestments),
                                  rep(list(F), nInvestments_cat1),
                                  rep(list(F), nInvestments_cat2),
                                  rep(list(T), nInvestments),
                                  rep(list(F), nInvestments_cat1),
                                  rep(list(F), nInvestments_cat2),
                                  rep(list(F), nInvestments),
                                  rep(list(F), nInvestments))),
         label = "All Investments"),

    list(method = "restyle",
         args = list("visible", c(rep(list(F), nInvestments),
                                  rep(list(T), nInvestments_cat1),
                                  rep(list(F), nInvestments_cat2),
                                  rep(list(F), nInvestments),
                                  rep(list(T), nInvestments_cat1),
                                  rep(list(F), nInvestments_cat2),
                                  rep(list(F), nInvestments),
                                  rep(list(F), nInvestments))),
         label = "Group 1"),

    list(method = "restyle",
         args = list("visible", c(rep(list(F), nInvestments),
                                  rep(list(F), nInvestments_cat1),
                                  rep(list(T), nInvestments_cat2),
                                  rep(list(F), nInvestments),
                                  rep(list(F), nInvestments_cat1),
                                  rep(list(T), nInvestments_cat2),
                                  rep(list(F), nInvestments),
                                  rep(list(F), nInvestments))),
         label = "Group 2")
  ))

# current dot annotation on chart
current_dot <- list(
  x = 0.1,
  y = 1,
  text = '<span style="font-size:20px;">•</span> shows current\n return / spend',
  xref = "paper",
  yref = "paper",
  showarrow = F,
  font = list(color='#A9A9A9', size=12),
  opacity = 0.5
)

# create set of line charts
response_curves <- plot_ly(data = cumRet_melt, x = ~spend, y= ~value, type='scatter', mode='lines',
                           color = ~variable, colors=c(colours, colours_cat1, colours_cat2),
                           visible=F) %>%
  add_trace(data = current, x = ~`Current Spend`, y = ~`Current Return`, mode='markers',
            marker = list(size = 9, line = list(color = 'white', width = 1)),
            color = ~Investment, showlegend=F) %>%
  # for some reason defeault view only displays correctly if first layer is added again (not sure if there is a more elegant solution)
  add_trace(data = cumRet_melt[cumRet_melt$variable %in% dt$Investment, ], x = ~spend, y= ~value, type='scatter', mode='lines',
            color = ~variable, colors=c(colours, colours_cat1, colours_cat2),
            visible=T) %>%
  add_trace(data = current[current$Investment %in% dt$Investment, ], x = ~`Current Spend`, y = ~`Current Return`, mode='markers',
            marker = list(size = 9, line = list(color = 'white', width = 1)),
            color = ~Investment, showlegend=F,
            visible=T) %>%
  layout(yaxis = list(title='Return', spikecolor='grey', spikethickness=0.1, spikedash='solid', showgrid=T, showline=F, hoverformat=',f'),
         xaxis = list(title='Spend', spikecolor='grey', spikethickness=0.1, spikedash='solid', showgrid=T, showline=F, hoverformat=',f'),
         hoverlabel = list(bordercolor='white', font=list(color='white'), namelength=-1),
         updatemenus = list(menu_line),
         annotations = list(current_dot),
         height = 600)


# total response ----
# retrieve record of cumulative return from earlier allocation
total_cumRet <- allocation$cumRet

# create total return column
total_cumRet$totalReturn <- rowSums(total_cumRet[-1])

# take only spend and cumulative return
total_cumRet <- total_cumRet[c('totalSpend', 'totalReturn')]

# net return
total_cumRet$netReturn <- total_cumRet$totalReturn - total_cumRet$totalSpend

# new total spend and return
total_new <- summary_table_all[summary_table_all$Investment == 'Total', c('New Spend', 'New Return')]

# add new net return
total_new$`New Net Return` <- total_new$`New Return` - total_new$`New Spend`

# plot chart
total_response <- plot_ly(data = total_cumRet, x = ~totalSpend, y = ~totalReturn, type = 'scatter', mode = 'lines', 
                          name = 'Total Return', line = list(color = '#2d4f7b')) %>% 
  add_trace(x = total_new[1,1], y = total_new[1,2], mode = 'markers',
            line = list(color = 'white'),
            marker = list(size = 11, color = '#DEB887', line = list(color = 'white', width = 1)),
            name = 'New Return') %>% 
  add_trace(data = total_cumRet, x = ~totalSpend, y = ~netReturn, type = 'scatter', mode = 'lines', 
            name = 'Total Net Return', line = list(color = '#2d4f7b', dash='dash')) %>% 
  add_trace(x = total_new[1,1], y = total_new[1,3], mode = 'markers',
            line = list(color = 'white'),
            marker = list(size = 9, color = 'white', line = list(color = '#DEB887', width = 2)),
            name = 'New Net Return') %>% 
  layout(yaxis = list(title='Return', spikecolor='grey', spikethickness=0.1, spikedash='solid', showspikes=T, showgrid=T, showline=F, hoverformat=',f'), 
         xaxis = list(title='Spend', spikecolor='grey', spikethickness=0.1, spikedash='solid', showspikes=T, showgrid=T, showline=F, hoverformat=',f'),
         height = 600)



end_time <- Sys.time()
print(paste('runtime:',end_time - start_time))