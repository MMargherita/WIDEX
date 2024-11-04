widex_pars_to_p_tibble2 <- function(PW, PU, PD, WD, age){
  # everyone starting as partnered or widowed; no unpartnered
  p_tibble <- tibble(age,PU,PD,UP=0,UD=0,UU=1,UW=0,WD,WP=0,WU=0,WW=1-WD) %>% 
    fmutate(PW = (1 - PD) * PW,      # partner dies but not ego
           PU = (1 - PD - PW) * PU,   # mortality takes precedent
           PP = 1 - PD - PW - PU) %>% # straight competing risks
    dt_pivot_longer(-age, names_to = "from_to", values_to = "p") %>% 
    fmutate(from = substr(from_to,1,1),
           to = substr(from_to, 2,2)) |> 
    fselect(-from_to)
  # to be looped over age_diff and edu pairings
  p_tibble
}
source("code/ex_functions.R")
# a simplified version for a single pairing
widex_pars_to_ex2 <- function(PW, PU, PD, WD, age, init_p){
  
  widex_pars_to_p_tibble2(PW, PU, PD, WD, age) %>% 
    transient_matrix %>% 
    transient_to_fundamental %>% 
    fundamental_to_ex(x = 65, 
                      init = c(P = init_p, U = 0, W = 1-init_p),
                      state = "W")
}

widex_pars_to_vec2 <- function(PW, PU, PD, WD, age, init_p){
  
  names(PW)   <- paste("P","W", age, sep = "_")
  names(PU)   <- paste("P","U", age, sep = "_")
  names(PD)   <- paste("P","D", age, sep = "_")
  names(WD)   <- paste("W","D", age, sep = "_")
  names(init_p) <- "init"
  vec <- c(init_p, PW, PU, PD, WD)
  
  return(vec)
}

widex_vec_to_ex2 <- function(widex_vec){
  
  init_p <- widex_vec["init"]
  
  PW <- widex_vec[grepl("P_W", names(widex_vec))]
  PU <- widex_vec[grepl("P_U", names(widex_vec))]
  PD <- widex_vec[grepl("P_D", names(widex_vec))]
  WD <- widex_vec[grepl("W_D", names(widex_vec))]
  
  age <- parse_number(names(PU))
  
  widex_pars_to_ex2(init_p = init_p,
                    PW = PW,
                    PU = PU,
                    PD = PD,
                    WD = WD,
                    age = age) %>%
    pull(exs)
}


# pseudo-code starts here:
# Now we need a loop structure that properly picks out 
# PU, PD, and WD for each ego education
# and which properly picks out the right partner education and age range.
age_diffs <- -50:50


# 8789 ----
prev_edu_8789 <- read_csv("output/output_17_10_2024/prev_age_diff/pred_prev_8789_gen2.txt") |> 
  select(-prev_hat, -age_diff) %>% 
  distinct() %>%
  mutate(prev_edu = n/sum(n)) %>% 
  select(-n)

prev_8789 <- read_csv("output/output_17_10_2024/prev_age_diff/pred_prev_8789_gen2.txt") |> 
  complete(age_diff = age_diffs, 
           edu_shsp, 
           fill = list(prev_hat = 0)) %>% 
  select(-n) %>% 
  left_join(prev_edu_8789, by="edu_shsp") %>% 
  mutate(prev = prev_hat*prev_edu)
# prev_hat is summing over edu composition
# prev is summing over the total row

# prev_8789 %>%
#     ggplot(aes(x = age_diff, y = prev, color = edu_shsp)) +
#     geom_line()

aggr_prev_8789 <- prev_8789 %>% 
  mutate(age_diff_group = case_when(
           between(age_diff,-50,-41) ~ -45, # 10-year
           between(age_diff,-40,-31) ~ -35,
           between(age_diff,41,50) ~ 45,    # 10-year
           between(age_diff,31,40) ~ 35,
           
           between(age_diff,-30,-26) ~ -28, # 5-year
           between(age_diff,-25,-21) ~ -23,
           
           between(age_diff,26,30) ~ 28,    # 5-year
           between(age_diff,21,25) ~ 23,
           between(age_diff, -20,-15) ~ age_diff - age_diff %% 2, # 2-year
           between(age_diff, 15, 20) ~ age_diff - age_diff %% 2,
           
           TRUE ~ age_diff) # single-age diffs
  ) |> 
  group_by(age_diff_group, edu_shsp) |> 
  summarize(prev = sum(prev))
  
# aggr_prev_8789 %>%
#   ggplot(aes(x = age_diff_group, y = prev, color = edu_shsp)) +
#   geom_line() +
#   scale_y_log10()


# we need to add age_sh and age_sp
aggr_prev_8789 <- aggr_prev_8789 %>% 
  mutate(age_pw = 65+age_diff_group) %>% 
  separate_wider_delim(cols = edu_shsp, delim = "_", names = c("edu_ego","edu_pw"))




# 1719 ----
prev_edu_1719 <- read_csv("output/output_17_10_2024/prev_age_diff/pred_prev_1719_gen2.txt") |> 
  select(-prev_hat, -age_diff) %>% 
  distinct() %>%
  mutate(prev_edu = n/sum(n)) %>% 
  select(-n)

prev_1719 <- read_csv("output/output_17_10_2024/prev_age_diff/pred_prev_1719_gen2.txt") |> 
  complete(age_diff = age_diffs, 
           edu_shsp, 
           fill = list(prev_hat = 0)) %>% 
  select(-n) %>% 
  left_join(prev_edu_1719, by="edu_shsp") %>% 
  mutate(prev = prev_hat*prev_edu)
# prev_hat is summing over edu composition
# prev is summing over the total row

# prev_1719 %>%
#     ggplot(aes(x = age_diff, y = prev, color = edu_shsp)) +
#     geom_line()

aggr_prev_1719 <- prev_1719 %>% 
  mutate(age_diff_group = case_when(
    between(age_diff,-50,-41) ~ -45, # 10-year
    between(age_diff,-40,-31) ~ -35,
    between(age_diff,41,50) ~ 45,    # 10-year
    between(age_diff,31,40) ~ 35,
    
    between(age_diff,-30,-26) ~ -28, # 5-year
    between(age_diff,-25,-21) ~ -23,
    
    between(age_diff,26,30) ~ 28,    # 5-year
    between(age_diff,21,25) ~ 23,
    between(age_diff, -20,-15) ~ age_diff - age_diff %% 2, # 2-year
    between(age_diff, 15, 20) ~ age_diff - age_diff %% 2,
    
    TRUE ~ age_diff) # single-age diffs
  ) |> 
  group_by(age_diff_group, edu_shsp) |> 
  summarize(prev = sum(prev))

# aggr_prev_1719 %>%
#   ggplot(aes(x = age_diff_group, y = prev, color = edu_shsp)) +
#   geom_line() +
#   scale_y_log10()


# we need to add age_sh and age_sp
aggr_prev_1719 <- aggr_prev_1719 %>% 
  mutate(age_pw = 65+age_diff_group) %>% 
  separate_wider_delim(cols = edu_shsp, delim = "_", names = c("edu_ego","edu_pw"))




# TMAT ----
# 8789
tmat_8789_low <- read_ods("output/output_17_11_23/trans_mat/trans_mat_gen2_8789_low.ods") %>% 
  rename(age = rownames) %>% 
  filter(between(age, 65, 111)) %>% 
  rename(PU = X_p12,
         PD = X_p14,
         WD = X_p34) %>% 
  mutate(edu_ego = "low") %>% 
  select(edu_ego,age, PU, PD, WD)

tmat_8789_mid <- read_ods("output/output_17_11_23/trans_mat/trans_mat_gen2_8789_mid.ods") %>% 
  rename(age = rownames) %>% 
  filter(between(age, 65, 111)) %>% 
  rename(PU = X_p12,
         PD = X_p14,
         WD = X_p34) %>% 
  mutate(edu_ego = "mid") %>% 
  select(edu_ego,age, PU, PD, WD)

tmat_8789_high <- read_ods("output/output_17_11_23/trans_mat/trans_mat_gen2_8789_high.ods") %>% 
  rename(age = rownames) %>% 
  filter(between(age, 65, 111)) %>% 
  rename(PU = X_p12,
         PD = X_p14,
         WD = X_p34) %>% 
  mutate(edu_ego = "high") %>% 
  select(edu_ego,age, PU, PD, WD)

tmat_8789 <- rbind(tmat_8789_high,tmat_8789_mid,tmat_8789_low)
rm(tmat_8789_high,tmat_8789_mid,tmat_8789_low)




# 1719
tmat_1719_low <- read_ods("output/output_17_11_23/trans_mat/trans_mat_gen2_1719_low.ods") %>% 
  rename(age = rownames) %>% 
  filter(between(age, 65, 111)) %>% 
  rename(PU = X_p12,
         PD = X_p14,
         WD = X_p34) %>% 
  mutate(edu_ego = "low") %>% 
  select(edu_ego,age, PU, PD, WD)

tmat_1719_mid <- read_ods("output/output_17_11_23/trans_mat/trans_mat_gen2_1719_mid.ods") %>% 
  rename(age = rownames) %>% 
  filter(between(age, 65, 111)) %>% 
  rename(PU = X_p12,
         PD = X_p14,
         WD = X_p34) %>% 
  mutate(edu_ego = "mid") %>% 
  select(edu_ego,age, PU, PD, WD)

tmat_1719_high <- read_ods("output/output_17_11_23/trans_mat/trans_mat_gen2_1719_high.ods") %>% 
  rename(age = rownames) %>% 
  filter(between(age, 65, 111)) %>% 
  rename(PU = X_p12,
         PD = X_p14,
         WD = X_p34) %>% 
  mutate(edu_ego = "high") %>% 
  select(edu_ego,age, PU, PD, WD)

tmat_1719 <- rbind(tmat_1719_high,tmat_1719_mid,tmat_1719_low)
rm(tmat_1719_high,tmat_1719_mid,tmat_1719_low)



# pr_d_20 -----
# n.b. partners' mortaloty is computed ovrall, not accounting egos education
# 8789
pr_d_20plus_8789_low <- read_ods("output/output_17_10_2024/pr_d_20plus/pr_d_gen2_8789_low.ods") %>% 
  mutate(edu_partner = "low")

pr_d_20plus_8789_mid <- read_ods("output/output_17_10_2024/pr_d_20plus/pr_d_gen2_8789_mid.ods") %>% 
  mutate(edu_partner = "mid")

pr_d_20plus_8789_high <- read_ods("output/output_17_10_2024/pr_d_20plus/pr_d_gen2_8789_high.ods") %>% 
  mutate(edu_partner = "high")

pr_d_20plus_8789 <- rbind(pr_d_20plus_8789_low,pr_d_20plus_8789_mid,pr_d_20plus_8789_high)%>% 
  rename(age = rownames,
         pr_d =X_p14) %>% 
  filter(between(age, 21, 112))

pr_d_20plus_8789 <-  
  expand_grid(edu_partner = c("low","mid","high"),
              age = 113:160) %>% 
  mutate(pr_d = 1) %>% 
  bind_rows(pr_d_20plus_8789) %>% 
  arrange(edu_partner, age)
  
rm(pr_d_20plus_8789_low,pr_d_20plus_8789_mid,pr_d_20plus_8789_high)

# 1719
pr_d_20plus_1719_low <- read_ods("output/output_17_10_2024/pr_d_20plus/pr_d_gen2_1719_low.ods") %>% 
  mutate(edu_partner = "low")

pr_d_20plus_1719_mid <- read_ods("output/output_17_10_2024/pr_d_20plus/pr_d_gen2_1719_mid.ods") %>% 
  mutate(edu_partner = "mid")

pr_d_20plus_1719_high <- read_ods("output/output_17_10_2024/pr_d_20plus/pr_d_gen2_1719_high.ods") %>% 
  mutate(edu_partner = "high")

pr_d_20plus_1719 <- rbind(pr_d_20plus_1719_low,pr_d_20plus_1719_mid,pr_d_20plus_1719_high) %>% 
  rename(age = rownames,
         pr_d =X_p14) %>% 
  filter(between(age, 21, 112))

pr_d_20plus_1719 <-  
  expand_grid(edu_partner = c("low","mid","high"),
            age = 113:160) %>% 
  mutate(pr_d = 1) %>% 
  bind_rows(pr_d_20plus_1719) %>% 
  arrange(edu_partner, age)

rm(pr_d_20plus_1719_low,pr_d_20plus_1719_mid,pr_d_20plus_1719_high)


# element for the LOOP -----


age <- c(65:111)

library(readODS)
init1_l <- read_ods("output/proportions/prop_gen2_8789_low.ods") %>% 
  select(st_p,prop) %>% 
  mutate(st_p = case_when(st_p == 1 ~ "P",
                          st_p == 2 ~ "U",
                          st_p == 3 ~ "W")) %>% 
  mutate(edu_ego = "low")

init2_l <- read_ods("output/proportions/prop_gen2_1719_low.ods") %>% 
  select(st_p,prop) %>% 
  mutate(st_p = case_when(st_p == 1 ~ "P",
                          st_p == 2 ~ "U",
                          st_p == 3 ~ "W")) %>% 
  mutate(edu_ego = "low")


init1_m <- read_ods("output/proportions/prop_gen2_8789_mid.ods") %>% 
  select(st_p,prop) %>% 
  mutate(st_p = case_when(st_p == 1 ~ "P",
                          st_p == 2 ~ "U",
                          st_p == 3 ~ "W")) %>% 
  mutate(edu_ego = "mid")

init2_m <- read_ods("output/proportions/prop_gen2_1719_mid.ods") %>% 
  select(st_p,prop) %>% 
  mutate(st_p = case_when(st_p == 1 ~ "P",
                          st_p == 2 ~ "U",
                          st_p == 3 ~ "W")) %>% 
  mutate(edu_ego = "mid")


init1_h <- read_ods("output/proportions/prop_gen2_8789_high.ods") %>% 
  select(st_p,prop) %>% 
  mutate(st_p = case_when(st_p == 1 ~ "P",
                          st_p == 2 ~ "U",
                          st_p == 3 ~ "W")) %>% 
  mutate(edu_ego = "high")

init2_h <- read_ods("output/proportions/prop_gen2_1719_high.ods") %>% 
  select(st_p,prop) %>% 
  mutate(st_p = case_when(st_p == 1 ~ "P",
                          st_p == 2 ~ "U",
                          st_p == 3 ~ "W")) %>% 
  mutate(edu_ego = "high")


init2 <- rbind(init2_l, init2_m, init2_h) %>% 
  filter(st_p!="U") %>% 
  group_by(edu_ego) %>% 
  mutate(prop = prop/sum(prop))

init1 <- rbind(init1_l, init1_m, init1_h) %>% 
  filter(st_p!="U") %>% 
  group_by(edu_ego) %>% 
  mutate(prop = prop/sum(prop))


# LOOP ----


do_this <- FALSE
if (do_this){
  
  
  aggr_prev_8789 <- 
  aggr_prev_8789 |> 
    mutate(EW1 = 0,
           year = 1988)
  
  aggr_prev_1719 <- 
    aggr_prev_1719 |> 
    mutate(EW2 = 0,
           year = 2018)
  
  
  
 for (i in nrow(aggr_prev_8789)){

   this_combo <- aggr_prev_8789[i, ]

   
   init1 <- init1 |>
     filter(edu_ego == this_combo[["edu_ego"]] & st_p == "P") %>% 
     pull(prop)
   
   init2 <- init2 |>
     filter(edu_ego == this_combo[["edu_ego"]] & st_p == "P") %>% 
     pull(prop)
   
   
   t1 <- tmat_8789 |> 
     filter(edu_ego == this_combo[["edu_ego"]]) # gives PD, PU, WD
   
   t2 <- tmat_1719 |> 
     filter(edu_ego == this_combo[["edu_ego"]]) # gives PD, PU, WD

   
   PW1 <-  pr_d_20plus_8789 |>
     filter(age %in% this_combo[["age_pw"]]:(this_combo[["age_pw"]]+46),
            edu_partner == this_combo[["edu_pw"]]) %>% 
     pull(pr_d)

   PW2 <-  pr_d_20plus_1719 |>
     filter(age %in% this_combo[["age_pw"]]:(this_combo[["age_pw"]]+46),
            edu_partner == this_combo[["edu_pw"]]) %>% 
     pull(pr_d)
   
   
 # now produce 
 veci1 <- widex_pars_to_vec2(PW1, t1$PU, t1$PD, t1$WD, age, init_p = init1) # ego age vec 65:110
 veci2 <- widex_pars_to_vec2(PW2, t2$PU, t2$PD, t2$WD, age, init_p = init2) # ego age vec 65:110
 
 # repeat for second time point
 aggr_prev_8789[i, "EW1"] <- widex_pars_to_ex2(PW1, t1$PU, t1$PD, t1$WD, age, init1)$exs
 aggr_prev_1719[i, "EW2"] <- widex_pars_to_ex2(PW2, t2$PU, t2$PD, t2$WD, age, init2)$exs
 
 # then decompose widex_vec_to_ex2() using the two vecs.
 library(tictoc)
 tic()
 cc <- DemoDecomp::horiuchi(widex_vec_to_ex2,veci1, veci2, N=2)
 toc()
 # save the output in a tidy object
 # what age and education combo are we looking at
 
 # stack these in the loop, appending this_combo values as columns
 
 
 
 
 if (i %% 10 == 0){
   cat("\n",i,"th iteration")
 }
 } # end loop
  
  # do kitagawa
  kit <- 
    aggr_prev |> 
    mutate(ED_diff = EW2 - EW1,
           prev_diff = prev2 - prev1,
           rate_component = ED_diff * (prev2 + prev1) / 2,
           prev_component = prev_diff * (EW2 + EW1) / 2)
  
  kit |> 
    select(-EW1, -EW2) |> 
    left_join(cc_tidy, by = join_by(age_pw, edu_ego, edu_partner)) |> 
    mutate(cc_total = cc * rate_component)
 
   partner_choice_contribution <-
     kit |> 
     summarize(cc_total = sum(prev_component))
}



