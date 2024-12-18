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
# alr_smoothed_probabilities <-
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













#################################
probs_gen1 <- read_csv("output/output_16_12_2024/export_probs_gen1.csv") %>% 
  mutate(gender="men")
probs_gen2 <- read_csv("output/output_16_12_2024/export_probs_gen2.csv") %>% 
  mutate(gender="women")

probs <- rbind(probs_gen1,probs_gen2) %>% 
  mutate(from_to = case_when(from_to ==  "partnered_dead" ~ "partnered_dead",
                             from_to ==  "partnered_partnered" ~ "partnered_partnered",  
                             from_to ==  "partnered_unpartnerd"~ "partnered_unpartnered",
                             from_to ==  "partnered_widowed"~ "partnered_widowed",    
                             from_to ==  "unpartnerd_dead"~ "unpartnered_dead",
                             from_to ==  "unpartnerd_partnered"~ "unpartnered_partnered", 
                             from_to ==  "unpartnerd_unpartnerd"~ "unpartnered_unpartnered",
                             from_to ==  "unpartnered_dead"~ "unpartnered_dead",     
                             from_to ==  "widowed_dead"~ "widowed_dead", 
                             from_to ==  "widowed_partnered"~ "widowed_partnered",    
                             from_to ==  "widowed_widowed"~ "widowed_widowed",
                             from_to ==  "widowed_unpartnerd"~ "widowed_unpartnered"),
         probability = ifelse(from_to=="widowed_unpartnered",0,probability)) %>%
  separate(from_to, into = c("from", "to"), sep = "_",   
           fill = "right", remove = FALSE) %>% 
  group_by(age, from) %>%
  mutate(probability = probability/sum(probability)) %>%
  ungroup()%>% 
  filter(from_to!="widowed_unpartnered") 

# N.B. change levels of from_to
# unique(probs$from_to)
rm(probs_gen1,probs_gen2)


probs_w_w_l_emp_18 <- probs %>% 
  filter(from=="widowed",
         gender=="women",
         educ=="low",
         type=="empirical",
         year==2018) %>% 
  select(-1)

probs_w_w_l_emp_88 <- probs %>% 
  filter(from=="widowed",
         gender=="women",
         educ=="low",
         type=="empirical",
         year==1988) %>% 
  select(-1)


alr_pred_w_w_l_emp_88 <-
  probs_w_w_l_emp_88 |> 
  # mutate(
  #   count_from_to = if_else(is.na(count_from_to),3,count_from_to),
  #   probability = count_from_to / count_from) |> 
  select(-from_to,-count_from_to) |> 
  mutate(from_to = paste0(substr(from,1,1) |> toupper(),
                          substr(to,1,1) |> toupper())) |> 
  select(-to) |> 
  group_by(educ, gender, year, from) |> 
  group_modify(~fit_alr(long_chunk = .x)) %>% 
  rename(probability_88=probability)  %>%
  ungroup() %>% 
  select(gender, educ, age, from_to,probability_88)


alr_pred_w_w_l_emp_18 <-
  probs_w_w_l_emp_18 |> 
  # mutate(
  #   count_from_to = if_else(is.na(count_from_to),3,count_from_to),
  #   probability = count_from_to / count_from) |> 
  select(-from_to,-count_from_to) |> 
  mutate(from_to = paste0(substr(from,1,1) |> toupper(),
                          substr(to,1,1) |> toupper())) |> 
  select(-to) |> 
  group_by(educ, gender, year, from) |> 
  group_modify(~fit_alr(long_chunk = .x)) %>% 
  rename(probability_18=probability) %>%
  ungroup() %>% 
  select(gender, educ, age, from_to,probability_18)

join_alr_pred_w_w_l_emp_88 <- left_join(alr_pred_w_w_l_emp_88,
                                        alr_pred_w_w_l_emp_18,
                                        by=c("gender", "educ", "age", "from_to"))




join_alr_pred_w_w_l_emp_88 |>  
  filter(from_to=="WD") %>% 
  # mutate(
  #   count_from_to = if_else(is.na(count_from_to),3,count_from_to),
  #   probability = count_from_to / count_from, 
  #   from_to = paste0(substr(from,1,1) |> toupper(),
  #                    substr(to,1,1) |> toupper())) |> 
  # select(-count_from_to) |> 
  # bind_rows(alr_pred_w_w_l_emp_18) |> 
  ggplot(aes(x = age, y = probability_88)) +
  geom_line() +
  geom_line(aes(y=probability_18), color="red")+
  scale_y_log10() +
  theme_minimal()




# compare with (non smoothed) empirical probabilities -----
w_w_l_fit_88 <- probs %>% 
  mutate(from_to = paste0(substr(from,1,1) |> toupper(),
                          substr(to,1,1) |> toupper())) %>% 
  filter(from=="widowed",
         gender=="women",
         educ=="low",
         type=="empirical",
         year==1988) %>% 
  select(-1) %>% 
  rename(probability_88=probability) %>% 
  select(type, gender, educ, age, from_to,probability_88) 

w_w_l_fit_18 <- probs %>% 
  mutate(from_to = paste0(substr(from,1,1) |> toupper(),
                          substr(to,1,1) |> toupper())) %>% 
  filter(from=="widowed",
         gender=="women",
         educ=="low",
         type=="empirical",
         year==2018) %>% 
  select(-1) %>% 
  rename(probability_18=probability) %>% 
  select(type, gender, educ, age, from_to,probability_18)

w_w_l_fit <- left_join(w_w_l_fit_88,w_w_l_fit_18,
                       by=c("type", "gender", "educ", "age", "from_to")) %>% 
  pivot_longer(cols=c("probability_88","probability_18"),
               names_to = "year",
               values_to = "probability")%>% 
  mutate(year=ifelse(year=="probability_18",2018,1988))

# modify the smoother dataset
join_alr_pred_w_w_l_emp_88 <- join_alr_pred_w_w_l_emp_88 %>% 
  mutate(type="smoothed") %>% 
  select(type, gender, educ, age, from_to,probability_88,probability_18) %>% 
  pivot_longer(cols=c("probability_88","probability_18"),
               names_to = "year",
               values_to = "probability") %>% 
  mutate(year=ifelse(year=="probability_18",2018,1988))


joint <- rbind(join_alr_pred_w_w_l_emp_88,w_w_l_fit)

joint %>%
  filter(from_to=="WD") %>% 
  mutate(year=as.factor(year)) %>% 
  ggplot(aes(x = age, y = probability, color=year, linetype = type)) +
  geom_line() +
  scale_y_log10() +
  theme_minimal()
