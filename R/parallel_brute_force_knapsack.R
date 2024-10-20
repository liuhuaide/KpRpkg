#' parallel_brute_force_knapsack
#' 
#' This function solves the knapsack problem using a brute-force approach.
#' When the data set is large, Parallelization can improve computation speed.
#' 
#' @param x A data frame containing two columns: 'v' (values) and 'w' (weights) of the items.
#'          Both 'v' and 'w' must be positive numbers.
#' @param W A numeric value representing the maximum allowed weight in the knapsack.
#' @param parallel When parallel computing is used, parallel=true
#'
#' @return  A list containing:
#'          - `value`: The maximum total value obtained within the weight limit.
#'          - `elements`: A vector of indices indicating which items are included in the optimal solution.
#' @export


parallel_brute_force_knapsack <- function(x, W, parallel = FALSE) {
  library(parallel)
  # 检查输入
  if (!is.data.frame(x)) stop("x must be a data frame")
  if (!all(c("v", "w") %in% colnames(x))) stop("The data frame must contain columns 'v' and 'w'")
  if (any(x$v <= 0) || any(x$w <= 0)) stop("All values for 'v' and 'w' must be positive")
  
  # 初始化变量
  n <- nrow(x)
  max_value <- 0
  best_combination <- NULL
  
  # 创建组合的索引
  combinations <- 0:(2^n - 1)
  
  # 如果开启并行计算
  if (parallel) {
    # 创建集群
    cl <- makeCluster(detectCores() - 1)  # 留出一个核心
    
    # 使用 parSapply 来并行计算每种组合
    results <- parSapply(cl, combinations, function(i) {
      combination <- as.logical(intToBits(i)[1:n])
      total_weight <- sum(x$w[combination])
      total_value <- sum(x$v[combination])
      # 只返回符合条件的总值和组合
      if (total_weight <= W) {
        return(c(total_value, sum(combination)))  # 返回总价值和符合条件的组合数
      } else {
        return(c(0, 0))
      }
    })
    
    # 关闭集群
    stopCluster(cl) 
    
    # 找到最大价值和最佳组合
    max_index <- which.max(results[1, ])
    max_value <- results[1, max_index]
    best_combination <- as.logical(intToBits(combinations[max_index])[1:n])
    
  } else {
    # 单线程计算
    for (i in combinations) {
      combination <- as.logical(intToBits(i)[1:n])
      total_weight <- sum(x$w[combination])
      total_value <- sum(x$v[combination])
      if (total_weight <= W && total_value > max_value) {
        max_value <- total_value
        best_combination <- combination
      }
    }
  }
  
  return(list(
    value = max_value,
    elements = which(best_combination)
  ))
}