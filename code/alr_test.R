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

var_alr <- function(n, p1_name, p2_name, X, smooth = TRUE, epsilon = 1e-6) {
  
  N <- length(n)
  stopifnot(all.equal(N, nrow(X)))
  stopifnot(p1_name %in% colnames(X))
  stopifnot(p2_name %in% colnames(X))
  
  # make sure no 0s
  X <- X + epsilon
  
  vx <- n * 0
  
  for (i in seq_along(n)) {
    var_log_p1 <- lbinom_var(n[i], X[i, p1_name])
    var_log_p2 <- lbinom_var(n[i], X[i, p2_name])
    
    # Compute ALR variance                    
    vx[i] <- var_log_p1 + var_log_p2 + 
      2 / n[i] # this is cov term for multinom
  }
  
  # Smoothing option
  if (smooth) {
    vx <- smooth.spline(vx, w = n)$y
  }
  
  return(vx)
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
    V[,i] <- var_alr(n, 
                     p1_name = attr_vars[i],
                     p2_name = denom_var,
                     X = X,
                     smooth=TRUE)
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






