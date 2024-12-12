# install.packages("Rfast")
library(tidyverse)
library(compositions)

var_alr <- function(n, p1, p2, smooth = TRUE) {
  # This function was designed with many iterations
  # of chatgpt, and may still have problems. I was unable
  # to find a direct variance expression for an ALR value,
  # so this is my best approximation for the time being. The intent
  # is to use the inverse of the smoothed output as a regression weight
  # for smoothing ALR age patterns.
  N <- length(n)
  stopifnot(all.equal(N, length(p1),length(p2)))
  # Small offset to avoid numerical issues
  epsilon <- 1e-10
  minimum_variance <- 1e-3  # Floor for variances
  
  cov_matrix <- cov(cbind(log(p1),log(p2)))
  
  # Stabilize probabilities
  p1 <- p1 + epsilon
  p2 <- p2 + epsilon
  
  # Function to compute the binomial PMF using log scale for stability
  binomial_pmf <- function(x, n, p) {
    exp(lchoose(n, x) + x * log(p) + (n - x) * log(1 - p))
  }
  
  # Function to compute E[log(X)] and E[log(X)^2]
  log_moments <- function(n, p) {
    E_log_X <- 0
    E_log_X2 <- 0
    
    for (x in 1:n) {  # Skip x=0 since log(0) is undefined
      prob <- binomial_pmf(x, n, p)
      log_x <- log(x)
      E_log_X <- E_log_X + log_x * prob
      E_log_X2 <- E_log_X2 + (log_x^2) * prob
    }
    
    list(E_log_X = E_log_X, E_log_X2 = E_log_X2)
  }
  
  # Initialize variances vector
  variances <- numeric(N)
  
  # Loop over all inputs (works for both scalar and vector cases)
  for (i in seq_along(n)) {
    # Compute variance for each age or observation
    moments_X1 <- log_moments(n[i], p1[i])
    var_log_X1 <- max(minimum_variance, moments_X1$E_log_X2 - (moments_X1$E_log_X^2))
    
    moments_X2 <- log_moments(n[i], p2[i])
    var_log_X2 <- max(minimum_variance, moments_X2$E_log_X2 - (moments_X2$E_log_X^2))
    
    if (!is.null(cov_matrix)) {
      cov_log_X1_X2 <- cov_matrix[1, 2]  # Extract covariance from provided matrix
      cov_log_X1_X2 <- min(cov_log_X1_X2, sqrt(var_log_X1 * var_log_X2))  # Cap covariance
    } else {
      cov_log_X1_X2 <- 0  # Default fallback if no covariance provided
    }
    
    variances[i] <- max(minimum_variance, var_log_X1 + var_log_X2 - 2 * cov_log_X1_X2)
  }
  
  if (smooth){
    variances <- smooth.spline(variances)$y
  }
  return(variances)
}

fit_alr <- function(long_chunk){
  wide <- long_chunk |> 
    pivot_wider(names_from = from_to, values_from = probability, values_fill = 1e-5) 
  
  x    <- wide$age
  cn <- colnames(wide)
  from_to <- cn[nchar(cn)==2]
  # identify the self-transition to use in denominator
  denom_var <- from_to[substr(from_to,1,1) == substr(from_to,2,2)]
  attr_vars <- from_to[from_to != denom_var]
  
  X <- 
    wide |> 
    select(all_of(from_to)) |> 
    as.matrix()
  A <- 
    X |> 
    alr() |> 
    unclass()
  n <- wide$count_from
  

  V <- A * 0
  for (i in 1:length(attr_vars)){
    V[,i] <- var_alr(n, X[,attr_vars[i]],X[,denom_var],smooth=TRUE)
  }

  Y <- A * 0
  for (i in 1:ncol(A)){
  Y[,i] <- lm(A[,i] ~ splines::ns(x, 2), weights = 1 / V[,i]) |> 
    predict(data.frame(x = x))
  }
  pred <-
    Y |> 
    alrInv() |> 
    unclass() |> 
    as.data.frame() |> 
    mutate(age = x, 
           count_from = n, .before = 1) |> 
    pivot_longer(-(1:2), 
                 names_to = "from_to", values_to = "probability") 
      
  return(pred)
}
#alr_smoothed_probabilities <-
probs<-  read_csv("probs_2.csv", show_col_types = FALSE) |> 
  select(-1)

probs |> 
  mutate(
    count_from_to = if_else(is.na(count_from_to),3,count_from_to),
    probability = count_from_to / count_from) |> 
  select(-1,-from_to,-count_from_to) |> 
  mutate(from_to = paste0(substr(from,1,1) |> toupper(),
                          substr(to,1,1) |> toupper())) |> 
  select(-to) |> 
  group_by(educ, gender, year, from) |> 
  group_modify(~fit_alr(long_chunk = .x))

