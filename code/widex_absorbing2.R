widex_pars_to_p_tibble2 <- function(PW, PU, PD, WD, age){
  
  p_tibble <- tibble(age,PU,PD,UP=0,UD=0,UU=1,UW=0,WD,WP=0,WU=0,WW=1-WD) %>% 
    mutate(PW = (1 - PD) * PW,      # partner dies but not ego
           PU = (1 - PD - PW) * PU,   # mortality takes precedent
           PP = 1 - PD - PW - PU) %>% # straight competing risks
    pivot_longer(-age, names_to = "from_to", values_to = "p") %>% 
    mutate(from = substr(from_to,1,1),
           to = substr(from_to, 2,2)) |> 
    select(-from_to)
  # to be looped over age_diff and edu pairings
  p_tibble
}
source("code/ex_functions.R")
# a simplified version for a single pairing
widex_pars_to_ex2 <- function(PW, PU, PD, WD, age){
  
  widex_pars_to_p_tibble2(PW, PU, PD, WD, age) %>% 
    transient_matrix %>% 
    transient_to_fundamental %>% 
    fundamental_to_ex(x = 65, 
                      init = c(P = 1, U = 0, W = 0),
                      state = "W")
}
widex_pars_to_vec2 <- function(PW, PU, PD, WD, age){
  names(PW)   <- paste("P","W", age, sep = "_")
  names(PU)   <- paste("P","U", age, sep = "_")
  names(PD)   <- paste("P","D", age, sep = "_")
  names(WD)   <- paste("W","D", age, sep = "_")
  
  vec <- c(PW, PU, PD, WD)
  
  return(vec)
}

widex_vec_to_ex2 <- function(widex_vec){
  
  PW <- widex_vec[grepl("P_W", names(widex_vec))]
  PU <- widex_vec[grepl("P_U", names(widex_vec))]
  PD <- widex_vec[grepl("P_D", names(widex_vec))]
  WD <- widex_vec[grepl("W_D", names(widex_vec))]
  
  age <- parse_number(names(PU))
  
  widex_pars_to_ex2(PW = PW,
                    PU = PU,
                    PD = PD,
                    WD = WD,
                    age = age)
}


# pseudo-code starts here:
# Now we need a loop structure that properly picks out 
# PU, PD, and WD for each ego education
# and which properly picks out the right partner education and age range.

# image you have prev:
# age_pw, edu_ego, edu_partner, prev
do_this <- FALSE
if (do_this){
  prev <- 
  prev |> 
    mutate(EW1 = 0,
           EW2 = 0)
  
  
 for (i in nrow(prev)){
this_combo <- prev[i, ]
trans_mat |> 
  filter(edu == this_combo["edu_ego"]) # gives PD, PU, WD
 PW = PR_D_20 |> filter(age %in% this_combo["age_pw"]:(this_combo["age_pw"]+35),
                        edu == this_combo["edu_partner"])
 # now produce 
 veci1 <- widex_pars_to_vec2(PW, PU, PD, WD, age) # ego age vec 65:110
 
 # repeat for second time point
 prev[i, "EW1"] <- widex_pars_to_ex2(
   # give pars
 )
 prev[i, "EW2"] <- widex_pars_to_ex2(
   # give second pars
 )
 # then decompose widex_vec_to_ex2() using the two vecs.
 cc <- DemoDecomp::horiuchi(widex_vec_to_ex2,veci1, veci2)
 # save the output in a tidy object
 
 # stack these in the loop, appending this_combo values as columns
 
 if (i %% 10 == 0){
   cat("\n",i,"th iteration")
 }
 } # end loop
  
  # do kitagawa
  kit <- 
    prev |> 
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



