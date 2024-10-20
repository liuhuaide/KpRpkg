#' greedy_knapsack_optimized
#' 
#' This function solves the fractional knapsack problem using a greedy approach.
#' This is an optimized greedy algorithm.It improves computational speed, 
#' especially when dealing with a large number of items, 
#' through preallocation and more efficient memory usage.
#' 
#' @param x A data frame containing two columns: 'v' (values) and 'w' (weights) of the items.
#'          Both 'v' and 'w' must be positive numbers.
#' @param W A numeric value representing the maximum allowed weight in the knapsack.
#' @return A list containing:
#'         - `value`: The total value obtained by the selected items.
#'         - `elements`: A vector of indices indicating which items were selected for the knapsack.
#' @export


greedy_knapsack_optimized <- function(x, W) {
  
  if (!is.data.frame(x)) stop("x must be a data frame")
  if (!all(c("v", "w") %in% colnames(x))) stop("The data frame must contain columns 'v' and 'w'")
  if (any(x$v <= 0) || any(x$w <= 0)) stop("All values for 'v' and 'w' must be positive")
  if (W <= 0) stop("The weight limit W must be a positive number")
  
  
  x$ratio <- x$v / x$w
  x <- x[order(-x$ratio), ]  
  
  total_value <- 0
  total_weight <- 0
  
  elements <- integer(nrow(x))  
  count <- 0  
  
  for (i in seq_len(nrow(x))) {
    if (total_weight + x$w[i] <= W) {
      total_weight <- total_weight + x$w[i]
      total_value <- total_value + x$v[i]
      count <- count + 1
      elements[count] <- x$index[i]  
    } else {
      break  
    }
  }
  
  elements <- elements[1:count]
  
  return(list(
    value = total_value,
    elements = elements
  ))
}