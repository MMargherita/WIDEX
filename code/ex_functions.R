
# 1: partnered    P
# 2: unpartnered  U
# 3: widowed      W
# 4: dead         D


# Note: UW and WU should be structural 0s
library(readODS)
library(tidyverse)
tmat <- read_ods("output/output_17_11_23/trans_mat/trans_mat_gen1_0204_high.ods")

init <- c(P=.86,U=.1,W = .04)

p_tibble <-
  tmat |> 
  rename(age = rownames) |> 
  filter(age > 64) |> 
  pivot_longer(X_p11:X_p34, names_to = "from_to", values_to = "p") |> 
  mutate(p = ifelse(is.na(p),0,p),
         from_to = gsub("X_p","", from_to),
         from_to = gsub("1","P", from_to),
         from_to = gsub("2","U", from_to),
         from_to = gsub("3","W", from_to),
         from_to = gsub("4","D", from_to)) |> 
  # filter(grepl("D",from_to)) |> 
  # ggplot(aes(x = age, y = p, color = from_to)) +
  # geom_line()
  pivot_wider(names_from = "from_to", values_from = p)

transition_block <- function(p_tibble, transition = "PP"){
  # TR: we can make this more computationally efficient if it becomes necessary
  p_chunk <-
    p_tibble |> 
    select(age_from = age,
           transition = all_of(transition)) |> 
    mutate(age_to = age_from + 1) 
  
  all_age <- c(p_chunk$age_from, p_chunk$age_to) |> unique()
  
  p_chunk |> 
    complete(age_from = all_age,
             age_to = all_age,
             fill = list(transition = 0)) |> 
    pivot_wider(names_from = age_from,
                values_from = transition)  |> 
    column_to_rownames("age_to") |> 
    as.matrix()
}

transient_matrix <- function(p_tibble, states = c("P","U","W")){
  # to be continued
}





