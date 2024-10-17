#' brute_force_knapsack
#'
#' This function solves the knapsack problem using a brute-force approach.
#' Given a data frame of items, where each item has a value (`v`) and a weight (`w`),
#' it returns the maximum total value that can be obtained without exceeding the
#' specified weight limit `W`.
#'
#' @param x A data frame containing two columns: 'v' (values) and 'w' (weights) of the items.
#'          Both 'v' and 'w' must be positive numbers.
#' @param W A numeric value representing the maximum allowed weight in the knapsack.
#' @return A list containing:
#'         - `value`: The maximum total value obtained within the weight limit.
#'         - `elements`: A vector of indices indicating which items are included in the optimal solution.
#'
#' @export


brute_force_knapsack <- function(x, W) {

  if (!is.data.frame(x)) stop("x must be a data frame")
  if (!all(c("v", "w") %in% colnames(x))) stop("The data frame must contain columns 'v' and 'w'")
  if (any(x$v <= 0) || any(x$w <= 0)) stop("All values for 'v' and 'w' must be positive")
  if (W <= 0) stop("The weight limit W must be a positive number")

  n <- nrow(x)

  max_value <- 0
  best_combination <- NULL

  for (i in 0:(2^n - 1)) {
    combination <- as.logical(intToBits(i)[1:n])

    total_weight <- sum(x$w[combination])
    total_value <- sum(x$v[combination])

    if (total_weight <= W && total_value > max_value) {
      max_value <- total_value
      best_combination <- combination
    }
  }

  return(list(
    value = max_value,
    elements = which(best_combination)
  ))
}
