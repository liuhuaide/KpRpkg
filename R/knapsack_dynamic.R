#' knapsack_dynamic
#'
#' This function solves the knapsack problem using dynamic programming.
#' Given a data frame of items, where each item has a value (`v`) and a weight (`w`),
#' it returns the maximum total value that can be obtained without exceeding the
#' specified weight limit `W`. It also identifies which items contribute to this maximum value.
#'
#' @param x A data frame containing two columns: 'v' (values) and 'w' (weights) of the items.
#'          Both 'v' and 'w' must be positive numbers.
#' @param W A numeric value representing the maximum allowed weight in the knapsack.
#' @return A list containing:
#'         - `value`: The maximum total value obtained within the weight limit.
#'         - `elements`: A sorted vector of indices indicating which items are included in the optimal solution.
#'
#' @export


knapsack_dynamic <- function(x, W) {

  if (!is.data.frame(x)) stop("x must be a data frame")
  if (!all(c("v", "w") %in% colnames(x))) stop("The data frame must contain columns 'v' and 'w'")
  if (any(x$v <= 0) || any(x$w <= 0)) stop("All values for 'v' and 'w' must be positive")
  if (W <= 0) stop("The weight limit W must be a positive number")

  n <- nrow(x)
  dp <- matrix(0, nrow = n + 1, ncol = W + 1)

  for (i in 1:n) {
    for (w in 0:W) {
      if (x$w[i] <= w) {
        dp[i + 1, w] <- max(dp[i, w], dp[i, w - x$w[i]] + x$v[i])
      } else {
        dp[i + 1, w] <- dp[i, w]
      }
    }
  }

  max_value <- dp[n + 1, W]

  elements <- c()
  current_weight <- W
  for (i in n:1) {
    if (dp[i + 1, current_weight] != dp[i, current_weight]) {
      elements <- c(elements, i)
      current_weight <- current_weight - x$w[i]
    }
  }

  return(list(
    value = max_value,
    elements = sort(elements)
  ))
}
