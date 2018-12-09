
createCurves <- function(product_minSpend, total_budget, increment, alpha, beta) {
  
  output <- list()
  
  product_minSpend <- as.numeric(product_minSpend)
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)
  
  # Generate a chain of spends going up in "increment"
  spend <- seq(from = product_minSpend, to = product_minSpend+total_budget, by = increment)
  
  # Calculate cumulative return for each spend
  output[["cumRet"]] <- beta*(1 - exp(-(spend/alpha)))
  # Remove first value of zero spend
  
  # Calculate marginal return for each spend
  output[["marRet"]] <- (output[["cumRet"]] - lag(output[["cumRet"]])) / increment
  # Remove first value of NA in marRet
  output[["marRet"]] <- output[["marRet"]][-1]
  
  return(output)
}