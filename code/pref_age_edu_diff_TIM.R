library(tidyverse)
library(splines)

N <- 2000

x <- rnorm(N,mean = 2, sd = 5) |> 
  round() |> 
  table()

age_diff <- x |> names() |> as.integer()

data <-
  tibble(age_diff = age_diff, 
         n = as.integer(x)) |> 
  complete(age_diff = -20:20, 
           fill = list(n=0)) |> 
  mutate(prev = n / sum(n),
         prev = prev ^ 2,
         prev = prev / sum(prev),
         n = round(prev * N)) 

# Examine simulated age-diff prevalence:
data |> 
  ggplot(aes(x = age_diff, y = prev)) +
  geom_line() +
  labs(title = "This distribution looks qualitatively\nlike age difference prevalence",
       subtitle = "Note, we also have 0s in tails") +
  theme_minimal()

# Problem: we'd like to share these data, but we can't share
# (1) counts below 5 (unless 0)
# (2) prevalence values that can be used to re-derive exact counts
# For this reason, we want to smooth the prevalence, but in such a way as to
# preserve the sharp peak and tail activity. The following is an ad hoc solution.

knots <- c(-15,-10,-5,seq(-3,5,by=2),10,15)

prev_pred <-
  data %>%
  glm(sqrt(prev) ~ ns(age_diff, 
                      knots = knots), 
      family = binomial(link = "logit"), data = .) |> 
  predict(newdata = data, type=  "response") |> 
  as.data.frame() |> 
  rename(prev_hat = 1) |> 
  mutate(age_diff = -20:20,
         prev_hat = prev_hat ^ 2,
         prev_hat = prev_hat / sum(prev_hat)) 

# Examine fit against simulated age-diff prevalence:
prev_pred |> 
  ggplot(aes(x = age_diff, y = prev_hat)) +
  geom_line() +
  geom_point(data = data, aes(y=prev)) +
  labs(title = "This fit preserves the shape very well",
       subtitle = "importantly, it cannot be used to back out counts") +
  theme_minimal()

# and how to scale this to operate on multiple groups? 
# make a function!

#' @param data tibble with columns `age_diff` and `prev`
#' @param knots a vector of knot locations in `age_diff` units
smooth_prev <- function(data, knots = c(-15,-10,-5,seq(-3,5,by=2),10,15)){
  .age_diffs <- data$age_diff
  data %>%
    glm(sqrt(prev) ~ ns(age_diff, 
                        knots = knots), 
        family = binomial(link = "logit"), data = .) |> 
    predict(newdata = data, type=  "response") |> 
    as.data.frame() |> 
    rename(prev_hat = 1) |> 
    mutate(age_diff = .age_diffs,
           prev_hat = prev_hat ^ 2,
           prev_hat = prev_hat / sum(prev_hat)) |> 
    left_join(data, by = join_by(age_diff))
}

# how to use on all strata
data |> 
  mutate(strata = 1, .before = 1) |> 
  group_modify(~smooth_prev(data = .), .by = strata)

