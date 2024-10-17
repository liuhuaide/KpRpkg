#' greedy_knapsack
#'
#' This function solves the fractional knapsack problem using a greedy approach.
#' Given a data frame of items, where each item has a value (`v`) and a weight (`w`),
#' it selects items based on the highest value-to-weight ratio until the weight limit `W` is reached.
#' This approach may not always provide the optimal solution for the 0/1 knapsack problem,
#' but it is efficient.
#'
#' @param x A data frame containing two columns: 'v' (values) and 'w' (weights) of the items.
#'          Both 'v' and 'w' must be positive numbers.
#' @param W A numeric value representing the maximum allowed weight in the knapsack.
#' @return A list containing:
#'         - `value`: The total value obtained by the selected items.
#'         - `elements`: A vector of indices indicating which items were selected for the knapsack.
#'
#' @export


greedy_knapsack <- function(x, W) {

  if (!is.data.frame(x)) stop("x must be a data frame")
  if (!all(c("v", "w") %in% colnames(x))) stop("The data frame must contain columns 'v' and 'w'")
  if (any(x$v <= 0) || any(x$w <= 0)) stop("All values for 'v' and 'w' must be positive")
  if (W <= 0) stop("The weight limit W must be a positive number")

  x$ratio <- x$v / x$w
  x$index <- seq_len(nrow(x))
  x <- x[order(-x$ratio), ]

  total_value <- 0
  total_weight <- 0
  elements <- integer(0)

  for (i in 1:nrow(x)) {
    if (total_weight + x$w[i] <= W) {
      total_weight <- total_weight + x$w[i]
      total_value <- total_value + x$v[i]
      elements <- c(elements, x$index[i])
    } else {
      break
    }
  }

  return(list(
    value = total_value,
    elements = elements
  ))
}

