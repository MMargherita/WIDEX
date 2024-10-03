# TODO: current the prev estimates are all identical for different education combinations
# we fixed the prev smoothing code to rerun on fiona 
# also in the following, the partner mortality is far too low, so we need to check
# how it was generated. Spot-checking against like-sex own-mortality we see 
# too-large differences

# we are going to use some functions from "ex_functions.R"
source("code/ex_functions.R")

widex_pars_to_p_tibble <- function(PW, PU, PD, WD, age, age_diff=0, age_pw=c(20:110)){
  PW <- tibble(PW,age_pw) %>% 
    mutate(age=age_pw-age_diff) %>%  # "-" because age diff is y_birth_ego-y_birth_pt
    select(-age_pw)
    
  p_tibble <- tibble(age,PU,PD,UP=0,UD=0,UU=1,UW=0,WD,WP=0,WU=0,WW=1-WD) %>% 
    left_join(PW,by=join_by(age)) %>% 
    mutate(PW=(1-PD)*PW,
           PU=(1-PD-PW)*PU,
           PP=1-PD-PW-PU) %>% 
    pivot_longer(-age, names_to = "from_to", values_to = "p") %>% 
    mutate(from = substr(from_to,1,1),
           to = substr(from_to, 2,2)) |> 
    select(-from_to)
  # to be looped over age_diff
  p_tibble
}


widex_pars_to_ex <- function(PW, PU, PD, WD, age, age_ego=65, age_pw=c(20:110),prev){
  
  expect <- tibble(age_pw, prev)%>% 
    mutate(prev=prev/sum(prev),
           ex_W = NA,
           age_diff=age_pw-age_ego)
  
  age_diff <- expect %>% 
    pull(age_diff)
  
  for (i in 1:length(age_pw)) {
   
     ex_W <- widex_pars_to_p_tibble(PW, PU, PD, WD, age = age,age_diff=age_diff[i],
                                    age_pw=age_pw) %>% 
       transient_matrix %>% 
       transient_to_fundamental %>% 
       fundamental_to_ex(x = 65, 
                         init = c(P = 1, U = 0, W = 0),
                         state = "W")
  
     expect[i,"ex_W"] <- ex_W$exs
  }
  
  expect %>% 
    summarise(ex_W=sum(ex_W*prev)) %>% 
    pull(ex_W)
  
}


widex_pars_to_vec <- function(PW, PU, PD, WD, prev, age_pw= c(20:110), age){
  
  names(PW)   <- paste("P","W", age_pw, sep = "_")
  names(PU)   <- paste("P","U", age, sep = "_")
  names(PD)   <- paste("P","D", age, sep = "_")
  names(WD)   <- paste("W","D", age, sep = "_")
  names(prev) <- paste("prev", age_pw, sep = "_")
  
  vec <- c(PW, PU, PD, WD, prev)
  
  return(vec)
  
}


widex_vec_to_ex <- function(widex_vec){
  
  p_ind <- grepl("prev", names(widex_vec))
  prev <- widex_vec[p_ind]
  
  PW <- widex_vec[grepl("P_W", names(widex_vec))]
  PU <- widex_vec[grepl("P_U", names(widex_vec))]
  PD <- widex_vec[grepl("P_D", names(widex_vec))]
  WD <- widex_vec[grepl("W_D", names(widex_vec))]
  
  age_pw <- parse_number(names(prev))
  age <- parse_number(names(PU))
  
  widex_pars_to_ex(PW=PW,
                   PU=PU,
                   PD=PD,
                   WD=WD,
                   age = age,
                   age_ego=65,
                   age_pw=age_pw,
                   prev=prev)
}


p_tibble <- read_csv("output/trans_mat_res/tmat_gen2_0204_low.ods") |> 
  filter(age < 112)

p_partner <- read_csv("output/output_13_12_2023/pr_d_20plus/pr_d_20plus_0204_gen1_low.txt") |> 
  select(age_pw = ika, 
         PW = 3)
# p_partner_check <- read_csv("output/trans_mat_res/tmat_gen1_0204_low.ods") |> 
#   filter(age < 112) |> 
#   select(age_pw=age, PD)
# left_join(p_partner, p_partner_check, by = join_by(age_pw)) |> View()


age_diffs <- -50:50
prev <-
  read_csv("output/output_13_12_2023/prev_age_diff/pred_prev_0204_gen2.txt") |> 
  distinct() |> 
  complete(age_diff = age_diffs, 
           edu_shsp, 
           fill = list(prev_hat = 0)) |> 
  filter(edu_shsp == "low_low") |>
  rename(prev = prev_hat) |> 
  mutate(age_pw = 65 + age_diff)

# widex_pars_to_p_tibble(PW = p_partner$PW,
#                        PU = p_tibble$PU,
#                        PD = p_tibble$PD,
#                        WD = p_tibble$WD,
#                        age = p_tibble$age,
#                        age_diff = 0, 
#                        age_pw = p_partner$age_pw)

test_code <- FALSE

if (test_code){
  p_partner$age_pw
 previ = prev |> 
   filter(between(age_pw,21,102)) |> 
   pull(prev)
 
widex_pars_to_ex(PW = p_partner$PW,
                 PU = p_tibble$PU,
                 PD = p_tibble$PD,
                 WD = p_tibble$WD,
                 age = p_tibble$age,
                 age_ego= 65,
                 age_pw = p_partner$age_pw,
                 prev = previ)

 widex_pars_to_vec(PW = p_partner$PW,
                   PU = p_tibble$PU,
                   PD = p_tibble$PD,
                   WD = p_tibble$WD,
                   age = p_tibble$age,
                   age_pw = p_partner$age_pw,
                   prev = previ) |> widex_vec_to_ex()
}

read_csv("output/output_13_12_2023/prev_age_diff/pred_prev_0204_gen2.txt") |> 
  distinct() |> 
  complete(age_diff = age_diffs, 
           edu_shsp, 
           fill = list(prev_hat = 0)) |> 
  group_by(edu_shsp) |> 
  mutate(prev = prev_hat/sum(prev_hat)) |> 
  ggplot(aes(x = age_diff, y = prev, color = edu_shsp)) +
  geom_line() +
  scale_y_log10()
