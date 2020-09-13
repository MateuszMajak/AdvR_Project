


var_selection <- function(data, y, x, boxcox = F){
  data_prepared <- data[, c(y, x)]
  data_prepared <- as.data.frame(data_prepared)
  if(boxcox == T){
    if(min(data_prepared[, y] > 0)){
      bc <- boxcox(data_prepared[, y])
      mes <- "Boxplot transformation was executed for the dependent variable."
    }
    else{
      bc <- boxcox(data_prepared[, y] + 1)
      mes <- "The minimum of the dependent variable is equal to 0. All values were changed +1 to execute the Box Cox transformation."
    }
    data_prepared[, y] <- bc$x.t
    
    return(list(data_prepared, mes, bc$lambda))
    
  }
    return(list(data_prepared))
    
}


