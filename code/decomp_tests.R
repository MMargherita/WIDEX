source("code/ex_functions.R")

# 1: partnered    P
# 2: unpartnered  U
# 3: widowed      W
# 4: dead         D
library(readODS)
tmat1 <- read_ods("output/output_17_11_23/trans_mat/trans_mat_gen1_0204_mid.ods")
tmat2 <- read_ods("output/output_17_11_23/trans_mat/trans_mat_gen2_0204_mid.ods")
init1 <- c(P=.78,U=.18,W = .04)
init2 <- c(P=.61,U=.22,W = .16)
init1 <- init1 / sum(init1)
init2 <- init2 / sum(init2)
attrition_vec1 <-
  tmat1 |> 
  rename(age = rownames) |> 
  # TR: double check this age filter, might need one age higher?
  filter(between(age, 65, 111)) |> 
  pivot_longer(X_p11:X_p34, names_to = "transition", values_to = "p") |> 
  mutate(p = ifelse(is.na(p),0,p),
         transition = gsub("X_p","", transition),
         transition = gsub("1","P", transition),
         transition = gsub("2","U", transition),
         transition = gsub("3","W", transition),
         transition = gsub("4","D", transition),
         from = substr(transition,1,1),
         to = substr(transition,2,2)) |> 
  select(-transition) |> 
  p_tibble_to_attrition_vec()
attrition_vec2 <-
  tmat2 |> 
  rename(age = rownames) |> 
  # TR: double check this age filter, might need one age higher?
  filter(between(age, 65, 111)) |> 
  pivot_longer(X_p11:X_p34, names_to = "transition", values_to = "p") |> 
  mutate(p = ifelse(is.na(p),0,p),
         transition = gsub("X_p","", transition),
         transition = gsub("1","P", transition),
         transition = gsub("2","U", transition),
         transition = gsub("3","W", transition),
         transition = gsub("4","D", transition),
         from = substr(transition,1,1),
         to = substr(transition,2,2)) |> 
  select(-transition) |> 
  p_tibble_to_attrition_vec()

attrition_vec1 <- c(attrition_vec1, init1)
attrition_vec2 <- c(attrition_vec2, init2)

attrition_vec_to_ex(attrition_vec1, state = "W")
attrition_vec_to_ex(attrition_vec2, state = "W")




library(tictoc)
tic()
cc <- horiuchi(attrition_vec_to_ex, attrition_vec1, attrition_vec2, N = 2, state = "W")
toc()
cc[i, j] <- func((x[, j] + DD[, i]), state="W") - 
  func((x[, j] - DD[, i]), state="W")
