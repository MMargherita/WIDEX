# install.packages("Rfast")
library(tidyverse)
library(compositions)

lbinom_var <- function(n, p) {
  x <- 1:n  # Generate the range of possible successes (skip x = 0)
  
  # Compute log-PMF for stability, then exponentiate
  probs <- dbinom(x, size = n, prob = p, log = TRUE) |> exp()
  
  # Compute moments in a vectorized way
  log_x <- log(x)
  E_log_X <- sum(log_x * probs)
  E_log_X2 <- sum((log_x^2) * probs)
  
  # Return variance
  E_log_X2 - E_log_X^2
}

var_alr <- function(
    n, 
    p1, 
    p2, 
    #cov_matrix = NULL, 
    #minimum_variance = 1e-6,
    epsilon = 1e-10, # Small offset to avoid numerical issues
    smooth = TRUE){
 
   N <- length(n)
  stopifnot(all.equal(N, length(p1),length(p2)))

  cov_matrix <- cov(cbind(log(p1),log(p2)))
  #   
  #   # Stabilize probabilities
  p1 <- p1 + epsilon
  p2 <- p2 + epsilon
  
  # Initialize variances vector
  variances <- numeric(length(n))
  
  # Loop over all inputs (works for both scalar and vector cases)
  for (i in seq_along(n)) {
    # Compute variance for each age or observation
    # var_log_X1 <- max(minimum_variance, lbinom_var(n[i], p1[i]))
    # var_log_X2 <- max(minimum_variance, lbinom_var(n[i], p2[i]))
    var_log_X1 <- lbinom_var(n[i], p1[i])
    var_log_X2 <- lbinom_var(n[i], p2[i])
    if (!is.null(cov_matrix)) {
      cov_log_X1_X2 <- cov_matrix[1, 2]  # Extract covariance from provided matrix
      cov_log_X1_X2 <- min(cov_log_X1_X2, sqrt(var_log_X1 * var_log_X2))  # Cap covariance
    } else {
      cov_log_X1_X2 <- 0  # Default fallback if no covariance provided
    }
    
    # variances[i] <- max(minimum_variance, var_log_X1 + var_log_X2 - 2 * cov_log_X1_X2)
    variances[i] <- var_log_X1 + var_log_X2 - 2 * cov_log_X1_X2
    
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
  

  V <- matrix(0,nrow = nrow(A),ncol=ncol(A), dimnames=list(NULL, colnames(A)))
  for (i in 1:length(attr_vars)){
    V[,i] <- var_alr(n, p1 = X[,attr_vars[i]], p2 = X[,denom_var],smooth=TRUE)
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
                 names_to = "from_to", values_to = "probability") |> 
    mutate(type = "alr smoothed")
      
  return(pred)
}
#alr_smoothed_probabilities <-
probs<-  read_csv("probs_2.csv", show_col_types = FALSE) |> 
  select(-1)

alr_pred <-
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


# compare
probs |>  
  mutate(
    count_from_to = if_else(is.na(count_from_to),3,count_from_to),
    probability = count_from_to / count_from, 
    from_to = paste0(substr(from,1,1) |> toupper(),
                          substr(to,1,1) |> toupper())) |> 
  select(-count_from_to) |> 
  bind_rows(alr_pred) |> 
  ggplot(aes(x = age, y = probability, color = from_to, linetype = type)) +
  geom_line() +
  scale_y_log10() +
  theme_minimal()


