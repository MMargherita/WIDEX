# install.packages("Rfast")
library(tidyverse)
library(compositions)

lbinom_var <- function(n., p, min_var = 1e-4) {
  n. <- as.integer(n.)
  x <- 1:n.  # Generate the range of possible successes (skip x = 0)
  
  # Compute log-PMF for stability, then exponentiate
  if (zapsmall(p) %in% c(0,1) ) {
    probs <- c(rep(0,n.-1), 1) 
    } else {
      probs <- dbinom(x, size = n., prob = p, log = TRUE) |> exp()
  }

  if (any(is.nan(probs))){
    probs
    # bla
    1+1
    #bla
  }
  
  # Compute moments in a vectorized way
  log_x <- log(x)
  E_log_X <- sum(log_x * probs)
  E_log_X2 <- sum((log_x^2) * probs)
  
  # Return variance
  var_out <- E_log_X2 - E_log_X^2

  var_out
}

# We need an age pattern to the covariance between
# the ALR's numerator transition and denominator transition.
# The cov term reduces to the minimum -1/n for single ages, 
# but this does not convince me. Instead we calculate cov over
# small moving windows of age ranges. But the result is super 
# sensitive to window size. Larger window = larger negative cov,
# it's a sort of conundrum. We make the determination of window
# size a function of sd within a predefined range of possible windows.
# probably overthought, but it works reasonably well it seems.
adaptive_window <- function(i, X, min_window = 3, max_window = 10) {
  # Calculate local variability across rows
  local_sd <- apply(X[max(1, i - max_window):min(nrow(X), i + max_window), ], 2, sd)
  
  # Normalize local_sd to emphasize differences
  normalized_sd <- (local_sd - mean(local_sd)) / sd(local_sd)
  
  # Scale window size
  scaled_window <- max_window - (normalized_sd / max(normalized_sd)) * (max_window - min_window)
  
  # Ensure window size stays within bounds
  return(round(pmax(min_window, pmin(scaled_window, max_window))))
}

var_alr <- function(n, 
                    p1_name, 
                    p2_name, 
                    X, 
                    smooth = TRUE, 
                    epsilon = 1e-10) {
  # Input validation
  N <- length(n)
  stopifnot(N == nrow(X))
  stopifnot(p1_name %in% colnames(X))
  stopifnot(p2_name %in% colnames(X))
  
  # Stabilize probabilities
  X <- X + epsilon
  
  # Compute dynamic covariances
  # dynamic_cov <- dynamic_cov_multinom(p1_name = p1_name, 
  #                                     p2_name = p2_name, 
  #                                     X = X, 
  #                                     window = cov_window, 
  #                                     epsilon = epsilon)
  
  # Initialize variances vector
  variances <- numeric(N)
  
  
  for (i in seq_along(n)) {
    window <- adaptive_window(i, X)
    ind <- max(1, i - window):min(nrow(X), i + window)
    # Compute dynamic covariance
    cov_matrix    <- cov(X[ind, ])
    cov_log_p1_p2 <- cov_matrix[p1_name, p2_name]
    
    # Compute variances for log(p1) and log(p2)
    p1 <- X[i, p1_name]
    p2 <- X[i, p2_name]
    if (zapsmall(p1) %in% c(0,1)){
      var_log_p1 <- 0
    } else {
      var_log_p1 <- lbinom_var(n. = n[i], p = p1)
    }
    if (zapsmall(p2) %in% c(0,1)){
      var_log_p2 <- 0
    } else {
      var_log_p2 <- lbinom_var(n. = n[i], p = p2)
    }

    # Compute ALR variance
    variances[i] <- var_log_p1 + var_log_p2 - 2 * cov_log_p1_p2
  }
  
  # Smoothing option
  if (smooth) {
    variances <- smooth.spline(variances, w = n)$y
  }
  
  return(variances)
}

# the idea is we do a simple smooth of ALR, using 
# splines::ns(age, 2), but we want 1/variance to use as weights,
# ergo the earlier machinery. We probably could get away with using
# n (count_from) as weights, but I think 1/var is better. Alternatively
# still we could use the sum of numerator counts (i.e. both numerators) from
# both fractions used in the ALR, which will be less than n. I tried neither
# of these, because deriving the variance of ALR was too fun. And I prefer
# it anyway. These three options would all make usable regression weights.
fit_alr <- function(long_chunk, 
                    age_fit_min = 65, 
                    age_fit_max = 100,
                    age_out = 65:100){
  # slice(long_chunk,1) |> unlist() |> paste() |> cat("\n")
  wide <- long_chunk |> 
    pivot_wider(names_from = from_to, values_from = probability, values_fill = 1e-3) |> 
    filter(between(age, age_fit_min, age_fit_max))
  

  x <- wide$age
  cn <- colnames(wide)
  from_to <- cn[nchar(cn)==2]
  # identify the self-transition to use in denominator
  denom_var <- from_to[substr(from_to,1,1) == substr(from_to,2,2)]
  attr_vars <- from_to[from_to != denom_var]
  from_to <- c(attr_vars, denom_var)
  X <- 
    wide |> 
    select(all_of(from_to)) |> 
    as.matrix()
  
  A <- 
    X |> 
    alr() |> 
    unclass() 
  n <- wide$count_from
  

   # V <- matrix(0,nrow = nrow(A),ncol=ncol(A), dimnames=list(NULL, colnames(A)))
   # for (i in 1:length(attr_vars)){
   #   V[,i] <- var_alr(n, 
   #                    p1_name = attr_vars[i],
   #                    p2_name = denom_var,
   #                    X = X,
   #                    smooth=TRUE)
   # }
   # V[V < 0] <- 10
   # V[is.na(V)] <- 10

  # Y <- A * 0
  Y <- matrix(0,nrow = length(age_out),ncol=ncol(A), dimnames=list(NULL, attr_vars))
  for (i in 1:ncol(A)){
    ni <- n * rowSums(X[,c(attr_vars[i], denom_var)])
    # ni <- n * X[,attr_vars[i]]
  # Y[,i] <- lm(A[,i] ~ splines::ns(x, 2), weights = 1 / V[,i]) |> 
  #   predict(data.frame(x = x))
  # Y[,i] <- lm(A[,i] ~ splines::ns(x, 1), weights = n) |> 
    Y[,i] <- lm(A[,i] ~ x, weights = ni) |> 
    predict(data.frame(x = age_out))
  }

  pred <-
    Y |> 
    alrInv() |> 
    unclass() |> 
    as.data.frame() |> 
    mutate(age = age_out, 
           .before = 1) |> 
    rename(!!purrr::set_names("V3",denom_var)) |> 
    pivot_longer(-1, 
                 names_to = "from_to", values_to = "probability") |> 
    mutate(type = "alr smoothed")
      
  return(pred)
}
#alr_smoothed_probabilities <-
probs<-  read_csv("output/probs_empirical_v1.csv", show_col_types = FALSE) |> 
  select(-1,-2)

# Noticing two problems
#  probs |> 
#    mutate(test = count_from_to / count_from) |> 
#    select(probability, test) |> 
#    head(100)
#  
#  probs |> 
#    filter(type == "empirical") |> 
#    group_by( year,gender, educ,    age, from) |> 
#    summarize(count_from =count_from[1],
#              count_from_check = sum(count_from_to, na.rm=TRUE))
#  
# # counts <= 5 get NAs
#  
#  probs |>
#    filter(is.na(count_from_to))

# for testing, we can assume probability is valid, but it needs further checking still 

# alr_pred  <-
#   probs |> 
#   mutate(from_to = paste0(substr(from,1,1) |> toupper(),
#                           substr(to,1,1) |> toupper())) |> 
#   select(-count_from_to, -to) |> 
#   filter(type == "empirical") |> 
#   filter(from != "unpartnered") |> 
#   # filter(is.na(count_from)) |> 
#   mutate(probability2 = round(probability,2)) |> 
#   group_by(educ, gender, year, from, age) |> 
#   mutate(n = n(),
#          count_from = case_when(is.na(count_from) & probability == 1 ~ 1,
#                                 is.na(count_from) & n == 2 & probability2 == .5 ~ 2,
#                                 is.na(count_from) & n == 2 & probability2 %in% c(.33,.67) ~ 3,
#                                 is.na(count_from) & n == 2 & probability2 %in% c(.25,.75) ~ 4,
#                                 is.na(count_from) & n == 3 & probability2 == .33 ~ 3,
#                                 is.na(count_from) & n == 3 & probability2 %in% c(.25,.5) ~ 4,
#                                 is.na(count_from) & n == 3 & probability2 %in% c(.2,.4,.6) ~ 5,
#                                 TRUE ~ count_from
#                                 )) |> 
#   ungroup() |> 
#   arrange(educ, gender, year, from, age) |> 
#   select(-probability2, -n) |>
#   group_by(educ, gender, year, from) |>
#   group_modify(~fit_alr(long_chunk = .x, 
#                         age_fit_min = 70,
#                         age_fit_max = 90)) 
# 
# 
# # compare
# probs |>  
#   filter(type == "empirical") |> 
#    mutate(
#      count_from_to = if_else(is.na(count_from_to),3,count_from_to),
#      probability = count_from_to / count_from, 
#      from_to = paste0(substr(from,1,1) |> toupper(),
#                            substr(to,1,1) |> toupper())) |> 
#    select(-count_from_to, -to,-count_from) |> 
#    bind_rows(alr_pred) |> 
#    filter(from_to == "WW") |> 
#    ggplot(aes(x = age, y = probability,  linetype = type, color = as.factor(year), by = interaction(as.factor(year), type))) +
#    geom_line() +
#    scale_y_sqrt() +
#    theme_minimal() +
#   facet_wrap(gender ~ educ)
# 
# 

gam_dat <- 
  probs |> 
  mutate(from_to = paste0(substr(from,1,1) |> toupper(),
                          substr(to,1,1) |> toupper())) |> 
  select(-count_from_to, -to) |> 
  filter(type == "empirical") |> 
  filter(from != "unpartnered") |> 
  # filter(is.na(count_from)) |> 
  mutate(probability2 = round(probability,2)) |> 
  group_by(educ, gender, year, from, age) |> 
  mutate(n = n(),
         count_from = case_when(is.na(count_from) & probability == 1 ~ 1,
                                is.na(count_from) & n == 2 & probability2 == .5 ~ 2,
                                is.na(count_from) & n == 2 & probability2 %in% c(.33,.67) ~ 3,
                                is.na(count_from) & n == 2 & probability2 %in% c(.25,.75) ~ 4,
                                is.na(count_from) & n == 3 & probability2 == .33 ~ 3,
                                is.na(count_from) & n == 3 & probability2 %in% c(.25,.5) ~ 4,
                                is.na(count_from) & n == 3 & probability2 %in% c(.2,.4,.6) ~ 5,
                                TRUE ~ count_from
         )) |> 
  ungroup() |> 
  arrange(educ, gender, year, from, age)


library(mgcv)


fit_alr_gam_educ <- function(long_chunk, 
                    age_fit_min = 65, 
                    age_fit_max = 100,
                    age_out = 65:100){
  # slice(long_chunk,1) |> unlist() |> paste() |> cat("\n")
  wide <- long_chunk |> 
    pivot_wider(names_from = from_to, values_from = probability, values_fill = 1e-3) |> 
    filter(between(age, age_fit_min, age_fit_max))
  
  
  x <- wide$age |> unique() |> sort()
  cn <- colnames(wide)
  from_to <- cn[nchar(cn)==2]
  # identify the self-transition to use in denominator
  denom_var <- from_to[substr(from_to,1,1) == substr(from_to,2,2)]
  attr_vars <- from_to[from_to != denom_var]
  from_to <- c(attr_vars, denom_var)
  to_alr <- paste0("alr_",attr_vars)
  
  fit_this <-
    wide |> 
    mutate(!!to_alr[1] := log(!!sym(attr_vars[1])/!!sym(denom_var)),
           !!to_alr[2] := log(!!sym(attr_vars[2])/!!sym(denom_var))) |> 
    select(-!!from_to) |> 
    pivot_longer(!!to_alr,names_to = "transition", values_to = "alr",names_prefix = "alr_") |> 
    filter(!is.na(count_from)) |> 
    filter(count_from > 10) |> 
    mutate(transition = as.factor(transition))
  
  mod <- gam(alr ~ age * transition * educ, 
            data = fit_this, weights = sqrt(count_from),
            select = TRUE,
            gamma = 1.5,
            method = "REML")

  
  data_out <- expand_grid(age = age_out, 
                          educ = fit_this$educ |> unique(),
                          transition = attr_vars)
  
  mod_pred <-
  tibble(alr = predict(mod, newdata = data_out, type = "response") ) |> 
           bind_cols(data_out) |> 
    pivot_wider(names_from = transition, values_from = alr)#|> 
    # ggplot(aes(x=age,y=alr, color = educ)) +
    # geom_line() + 
    # facet_wrap(transition~educ) + 
    # geom_line(data=fit_this)
  pred_out <-
    mod_pred |> 
    select(-age,-educ) |> 
    as.matrix()|> 
    alrInv()|> 
    unclass() |> 
    as.data.frame() |> 
    bind_cols(mod_pred |> select(age,educ)) |> 
    rename(!!purrr::set_names("V3",denom_var)) |> 
    pivot_longer(-c(age,educ), 
                 names_to = "from_to", 
                 values_to = "probability") |> 
    mutate(type= "gam_alr_linear")

  return(pred_out)
}
#a

gam_alr_linear <- 
  probs |> 
  mutate(from_to = paste0(substr(from,1,1) |> toupper(),
                          substr(to,1,1) |> toupper())) |> 
  select(-count_from_to, -to) |> 
  filter(type == "empirical",
         from != "unpartnered",
         !is.na(count_from)) |> 
  arrange(educ, gender, year, from, age) |> 
  group_by(gender, year, from) |> 
  group_modify(~fit_alr_gam_educ(long_chunk = .x))

joined <- 
  probs |> 
  mutate(from_to = paste0(substr(from,1,1) |> toupper(),
                          substr(to,1,1) |> toupper())) |> 
  select(-count_from_to, -to) |> 
  filter(type == "empirical",
         from != "unpartnered",
         !is.na(count_from)) |> 
  bind_rows(gam_alr_linear) 

# joined |> 
#   ggplot(aes(x = age, y = probability, color = as.factor(year), linetype = type)) +
#   geom_line() +
#   facet_wrap(educ~gender) +
#   scale_y_log10() +
#   scale_linetype_manual(values = c(empirical = 2, gam_alr_linear = 1))


joined |> 
  filter(gender == "men") |> 
  ggplot(aes(x = age, y = probability, color = as.factor(year), linetype = type)) +
  geom_line() +
  facet_grid(vars(from_to),vars(educ), scales = "free_y") +
  scale_y_log10() +
  scale_linetype_manual(values = c(empirical = 2, gam_alr_linear = 1))
