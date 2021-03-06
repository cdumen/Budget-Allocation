# custom functions

#------------------------------------------------------------------------------------------
# function to generate:
# 1. cumulative returns
# 2. marginal returns
# for a series of given spend
#------------------------------------------------------------------------------------------
# PACKAGE DEPENDENCIES: none
# FUNCTION DEPENDENCIES: none
# ARGUMENTS:
  # {product_minSpend}: a vector minimum spends for investments
  # {total_budget}: an integer which specifies how much to spend in total
  # {alpha}: a vector of values that determines how quickly diminishing return is reached
  #         (i.e. the 'steepness' of the curve)
  # {beta}: a vector of values to ensure that response values correspond to current returns
  #         (i.e. a scalar that 'stretches' curve)

createCurves <- function(product_minSpend, alpha, beta, max_curve) {
  
  output <- list()
  
  product_minSpend <- as.numeric(product_minSpend)
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)
  
  # Generate a chain of spends going up in "increment"
  # spend <- seq(from = product_minSpend, to = product_minSpend+total_budget, by = increment)
  spend <- seq(from = product_minSpend, max_curve, by = increment)
  
  # Calculate cumulative return for each spend
  output[["cumRet"]] <- beta*(1 - exp(-(spend/alpha)))
  
  # Calculate marginal return for each spend
  output[["marRet"]] <- (output[["cumRet"]] - lag(output[["cumRet"]]) - increment) / increment
  # Remove first value of NA in marRet
  output[["marRet"]] <- output[["marRet"]][-1]
  
  return(output)
}

# -------------------------------------------------------------------------------
# function to allocate budget across multiple investment based on their returns
# method used here is a step by step allocation in increments
# at each step, we check which investment gives the highest marginal return
# and allocate an increment of the spend to it
# process continues until we reach the total_budget
# -------------------------------------------------------------------------------
# PACKAGE DEPENDENCIES: none
# FUNCTION DEPENDENCIES: none
# ARGUMENTS:
  # {df}: a dataframe which must contain: 
          # "Investment" (ID of investments)
          # "Category1" (category of investment)
          # "Category2" (sub-category of investment)
          # "Minimum.Budget"
          # "Maximum.Budget"
  # {marRet}: a dataframe of marginal returns of investments
          # (columns as investment and rows as spend)
  # {total_budget}: an integer which specifies how much to spend in total

budgetAllocation <- function(df, marRet, total_budget) {
  
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
    # store each iteration's marginal return and spend in the relevant lists
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


# -------------------------------------------------------------------------------
genColours <- function(theme, n) {
  theme_colours <-  brewer.pal(8,name=theme)
  n_colours <- colorRampPalette(theme_colours)
  colours <- n_colours(n)
  return(colours)
}

# -------------------------------------------------------------------------------
round_df <- function(df, digits) {
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- round(df[numeric_cols], digits)
  df
}

# -------------------------------------------------------------------------------

format_df <- function(df, accounting, ROI, percent) {
  
  # don't show row names
  rownames(df) <- NULL
  
  # replace all ' ' in column names with '<br>'
  colnames(df)[-length(df)] <- gsub(' ', '<br>', colnames(df)[-length(df)])
  accounting[-length(accounting)] <- gsub(' ', '<br>', accounting[-length(accounting)])
  ROI <- gsub(' ', '<br>', ROI)
  percent <- gsub(' ', '<br>', percent)
  
  # set width of table for equal sized colour bar columns
  fixedWidth <-  100

  # apply the relevant colour and font formatting
  formatted_df <-   formattable(df, align=c('r','r','r','l','r','r','l','r','r','l','r'), list(
    
    # spend / return format
    area(row=1:nrow(df)-1, col=accounting) ~ formatter(
      'span',
      x ~ accounting(x),
      style = x ~ style(display = "inline-block",
                        direction = "rtl", `border-radius` = "4px", `padding-right` = "2px",
                        `background-color` = csscolor("#b7cdeb")
                        , width = paste(fixedWidth*proportion(x),"px",sep=""))
    ),
    
    # ROI format
    area(col = ROI) ~ formatter(
      'span',
      x ~ accounting(x),
      style = x ~ style(display = "block", 
                        padding = "0 4px", `border-radius` = "4px", 
                        `background-color` = csscolor(gradient(as.numeric(x), 'transparent','#ce954b')), width='80px')
    ),
    
    # percentages format
    area(col = percent) ~ formatter(
      'span',
      style = x ~ style(color = ifelse(x < 0 , "#ff9999", "#77dd77")),
      x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), percent(abs(x), digits=1L))
    ),
    
    # total row format: investment
    area(row=nrow(df), col=1) ~ formatter(
      'span',
      style = x ~ style(font.weight='bold')
    ),
    
    # total row format: spend / return / ROI
    area(row=nrow(df), col=c(accounting, ROI)) ~ formatter(
      'span',
      style = x ~ style(font.weight='bold'),
      x ~ accounting(x)
    ),
    
    # total row format: percentages
    area(row=nrow(df), col=percent) ~ formatter(
      'span',
      style = x ~ style(color = ifelse(x < 0 , "#ff8080", "#63d863"), font.weight='bold'),
      x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), percent(abs(x), digits=1L))
    )
    
  ) # list
  ) # formattable
  
  return(formatted_df)
  
}



# -------------------------------------------------------------------------------
agg_rspCurves <- function(df, group, max_curve) {
  
  # make copy of dt and convert to datatable
  dt_agg <- data.table(dt)
  
  # aggregate key metrics
  dt_agg <- dt_agg[, .(sum(Current.Spend), sum(Current.Return), sum(Weeks), sum(Minimum.Budget), sum(Minimum.Budget), sum(Alpha), sum(Beta), .N), by=eval(group)]
  
  # assign names again to columns
  colnames(dt_agg) <- c('Group', 'Current.Spend', 'Current.Return', 'Weeks', 'Minimum.Budget', 'Maximum.Budget', 'Alpha', 'Beta', 'Count')
  
  # recalibrate alpha
  dt_agg[, Alpha := -(Current.Spend / log(1-(Current.Return/Beta)))]
  
  # convert back to dataframe
  dt_agg <- data.frame(dt_agg)
  
  # create responsive curves
  Curves_agg <- apply(dt_agg, 1, function(x) createCurves(x["Minimum.Budget"], alpha=x["Alpha"], beta=x["Beta"], max_curve))
  
  # compile response curves
  cumRet_agg <- data.frame(lapply(1:length(Curves_agg), function(x) do.call("cbind", Curves_agg[[x]]["cumRet"])))
  colnames(cumRet_agg) <- dt_agg$Group
  
  # melt data
  cumRet_melt_agg <- cbind(spend=round(seq(0, max_curve, increment), digits=2), cumRet_agg)
  cumRet_melt_agg <- melt(cumRet_melt_agg, id.vars = "spend")
  
  # convert group to factor
  cumRet_melt_agg$variable <- factor(cumRet_melt_agg$variable, levels=unlist(unique(dt[group])))
  
  # order by factor (same ordering as original df)
  cumRet_melt_agg <- cumRet_melt_agg[order(cumRet_melt_agg$variable), ]
  
  return(cumRet_melt_agg)
  
}

# -------------------------------------------------------------------------------
cur_spend_ret <- function(summary_table_names) {
  
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
  
  return(current)
  
}


