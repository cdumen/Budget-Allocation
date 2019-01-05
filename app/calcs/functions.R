# custom functions that are too long / used too often to incorporate into the main script


# create curves ----
# function to generate:
# 1. cumulative returns
# 2. marginal returns
# for a series of given spend

# {product_minSpend}: a vector minimum spends for investments
# {alpha}: a vector of values that determines how quickly diminishing return is reached
#         (i.e. the 'steepness' of the curve)
# {beta}: a vector of values to ensure that response values correspond to current returns
#         (i.e. a scalar that 'stretches' curve)
# {increment}: a scalar that determines how much the spend goes up by
# {max_curve}: a scalar that determines where the curves cut off (x-axis value)

createCurves <- function(product_minSpend, alpha, beta, increment, max_curve) {
  
  output <- list()
  
  product_minSpend <- as.numeric(product_minSpend)
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)
  
  # Generate a chain of spends going up in "increment"
  spend <- seq(from = product_minSpend, max_curve, by = increment)
  
  # Calculate cumulative return for each spend
  output[["cumRet"]] <- beta*(1 - exp(-(spend/alpha)))
  
  # Calculate marginal return for each spend
  output[["marRet"]] <- (output[["cumRet"]] - lag(output[["cumRet"]]) - increment) / increment
  # Remove first value of NA in marRet
  output[["marRet"]] <- output[["marRet"]][-1]
  
  return(output)
}


# budget allocation ----
# function to allocate budget across multiple investment based on their returns
# method used here is a step by step allocation in increments
# at each step, we check which investment gives the highest marginal return
# and allocate an increment of the spend to it
# process continues until we reach the total_budget

# {df}: a dataframe which must contain: 
  # "Investment" (ID of investments)
  # "Category1" (category of investment)
  # "Category2" (sub-category of investment)
  # "Minimum.Budget"
  # "Maximum.Budget"
# {marRet}: a dataframe of marginal returns of investments
  # (columns as investment and rows as spend)
# {cumRet}: a dataframe of cumulative returns of investments
  # (columns as investment and rows as spend)
# {total_budget}: an integer which specifies how much to spend in total
# {increment}: a scalar that determines how much the spend goes up by

budgetAllocation <- function(df, marRet, cumRet, total_budget, increment) {
  
  # current marginal return
  cur_marRet <- as.numeric(marRet[1, ])
  names(cur_marRet) <- df$Investment
  
  # current cumulative return
  cur_cumRet <- rep(0, length(cur_marRet))
  names(cur_cumRet) <- df$Investment
  
  # row index of current marginal return
  cur_marRet_rowIndex <- rep(1, length(cur_marRet))
  # set the current max as 0
  cur_marRet_rowIndex[which.max(cur_marRet)] <- 0
  
  # current spend - starting spend contains the minimum budget for all investments
  cur_spend <- as.numeric(df$Minimum.Budget)
  names(cur_spend) <- df$Investment
  
  # constraints (maximum spend for all investments)
  max_spend <- as.numeric(df$Maximum.Budget)
  
  # number of iterations
  n_iterations <- (total_budget - sum(cur_spend))/increment
  
  # number of investment choices
  n_investments <- nrow(df)
  
  # record marginal return, cumulative return and spend across all iterations
  cur_marRet_iterations <- list()
  cur_cumRet_iterations <- list()
  cur_spend_iterations <- list()
  
  # run allocation until we reach the total_budget
  for(i in 1:n_iterations) {
    
    # sort the current margin in descending order
    cur_marRet_ordered <- sort(cur_marRet, method='radix', decreasing = T)
    
    # keep looking for an investment that we can spend on, given constraints
    for(j in 1:n_investments) {
      
      # select the investmnent with the jth highest marginal return
      cur_investment <- which(df$Investment == names(cur_marRet_ordered)[j])
      
      # check constraints to see if we can still spend on this
      if(cur_spend[cur_investment] + increment <= max_spend[cur_investment]) { # if yes,
        
        # if not, update current spend
        cur_spend[cur_investment] <- cur_spend[cur_investment] + increment
        
        # move marRet row index along one for the selected investment
        cur_marRet_rowIndex[cur_investment] <- cur_marRet_rowIndex[cur_investment] + 1
        rowIndex <- cur_marRet_rowIndex[cur_investment]
        
        # update the current marginal and cumulative return
        cur_marRet[cur_investment] <- marRet[rowIndex, cur_investment]
        cur_cumRet[cur_investment] <- cumRet[rowIndex, cur_investment]
        
        # if investment spend has now reached maximum, assign -Inf to all its marRet values
        if(cur_spend[cur_investment] == max_spend[cur_investment]) {cur_marRet[cur_investment] <- -Inf}
        
        break
      }
    }
    # store each iteration's marginal return, cumulative return and spend in the relevant lists
    cur_marRet_iterations[[i]] <- cur_marRet
    cur_cumRet_iterations[[i]] <- cur_cumRet
    cur_spend_iterations[[i]] <- cur_spend
  }
  
  totalSpend <- round(seq(increment+sum(df$Minimum.Budget), total_budget, by=increment), digits=2)
  
  # convert lists to dataframe
  cur_marRet_iterations <- cbind(totalSpend, data.frame(do.call("rbind", cur_marRet_iterations)))
  cur_cumRet_iterations <- cbind(totalSpend, data.frame(do.call("rbind", cur_cumRet_iterations)))
  cur_spend_iterations <- cbind(totalSpend, data.frame(do.call("rbind", cur_spend_iterations)))
  
  output <- list(marRet=cur_marRet_iterations, cumRet=cur_cumRet_iterations, spend=cur_spend_iterations)
  return(output)
}


# colour palette ----
# RColorBrewer limits the number of distinct colours allowed for a given colour palette
# function generates additional colours complimentary to selected palette

# {theme}: name (in string) of RColorBrewer palette
# n: number of distinct colours we want to generate

genColours <- function(theme, n) {
  theme_colours <-  brewer.pal(8,name=theme)
  n_colours <- colorRampPalette(theme_colours)
  colours <- n_colours(n)
  return(colours)
}


# round dataframe ----
# function picks out all numeric columns in a dataframe
# and rounds them up to the number of decimal places given

# {df}: dataframe of interest
# {digits}: integer which indicates no. of decimal places

round_df <- function(df, digits) {
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- round(df[numeric_cols], digits)
  df
}


# aggregate response curves ----
# function that creates aggregated response curves by groups through recalibrating alpha

# {df}: a dataframe which must contain: 
  # "Current.Spend"
  # "Current.Return"
  # "Weeks"
  # "Minimum.Budget"
  # "Maximum.Budget"
  # "Alpha"
  # "Beta"
  # columns containing groups of interest (e.g. "Category1" and "Category2")
# {group}: a string containing the name of column we want to group by
# {max_curve}: a scalar that determines where the curves cut off (x-axis value)
# {increment}: a scalar that determines how much the spend goes up by

agg_rspCurves <- function(df, group, max_curve, increment) {
  
  # make copy of df and convert to datatable
  df_agg <- data.table(df)
  
  # aggregate key metrics
  df_agg <- df_agg[, .(sum(Current.Spend), sum(Current.Return), sum(Weeks), sum(Minimum.Budget), sum(Minimum.Budget), sum(Alpha), sum(Beta), .N), by=eval(group)]
  
  # assign names again to columns
  colnames(df_agg) <- c('Group', 'Current.Spend', 'Current.Return', 'Weeks', 'Minimum.Budget', 'Maximum.Budget', 'Alpha', 'Beta', 'Count')
  
  # recalibrate alpha
  df_agg[, Alpha := -(Current.Spend / log(1-(Current.Return/Beta)))]
  
  # convert back to dataframe
  df_agg <- data.frame(df_agg)
  
  # create responsive curves
  Curves_agg <- apply(df_agg, 1, function(x) createCurves(x["Minimum.Budget"], alpha=x["Alpha"], beta=x["Beta"], increment, max_curve))
  
  # compile response curves
  cumRet_agg <- data.frame(lapply(1:length(Curves_agg), function(x) do.call("cbind", Curves_agg[[x]]["cumRet"])))
  colnames(cumRet_agg) <- df_agg$Group

  # melt data
  cumRet_melt_agg <- cbind(spend=round(seq(0, max_curve, increment), digits=2), cumRet_agg)
  cumRet_melt_agg <- melt(cumRet_melt_agg, id.vars = "spend")

  # convert group to factor
  cumRet_melt_agg$variable <- factor(cumRet_melt_agg$variable, levels=unlist(unique(df[group])))

  # order by factor (same ordering as original df)
  cumRet_melt_agg <- cumRet_melt_agg[order(cumRet_melt_agg$variable), ]

  return(cumRet_melt_agg)
  
}