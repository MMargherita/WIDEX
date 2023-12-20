setwd("C:/Users/moremarg/PERSONAL FOLDER/POSTDOC/WIDEX")


# we are going to use some functions from "ex_functions.R"
source("code/ex_functions.R")

widex_pars_to_p_tibble <- function(PW, PU, PD, WD, age_diff=0, age_pw=c(20:110)){
  PW <- tibble(PW,age_pw) %>% 
    mutate(age=age_pw-age_diff) %>%  # "-" because age diff is y_birth_ego-y_birth_pt
    select(-age_pw)
    
  p_tibble <- tibble(age,PU,PD,UP=0,UD=0,UU=1,UW=0,WD,WP=0,WU=0,WW=1-WD) %>% 
    left_join(PW,by=join_by(age)) %>% 
    mutate(PW=(1-PD)*PW,
           PU=(1-PD-PW)*PU,
           PP=1-PD-PW-PU) %>% 
    pivot_longer(-age, names_to = "from_to", values_to = "p") %>% 
    separate_wider_delim(from_to, delim="", names_sep="_")
  # to be looped over age_diff
}


widex_pars_to_ex <- function(PW, PU, PD, WD, age_ego=65, age_pw=c(20:110),prev){
  
  expect <- tibble(age_pw,prev)%>% 
    mutate(prev=prev/sum(prev),
           ex_W = NA,
           age_diff=age_pw-age_ego)
  
  age_diff <- expect %>% 
    pull(age_diff)
  
  for (i in 1:length(age_pw)) {
   
     ex_W <- widex_pars_to_p_tibble(PW, PU, PD, WD, age_diff=age_diff[i],
                                    age_pw=age_pw) %>% 
       transient_matrix %>% 
       transient_to_fundamental %>% 
       fundamental_to_ex(x = 65, 
                         init = c(P = 1, U = 0, W = 0),
                         state = "W")
  
     expect[i,"ex_W"] <- ex_W
  }
  
  expect %>% 
    summarise(ex_W=sum(ex_W*prev)) %>% 
    pull(ex_W)
  
}


widex_pars_to_vec <- function(PW, PU, PD, WD, prev, age_pw=c(65:110), age){
  
  names(PW) <- paste("P","W",age_pw,sep="_")
  names(PU) <- paste("P","U",age,sep="_")
  names(PD) <- paste("P","D",age,sep="_")
  names(WD) <- paste("W","D",age,sep="_")
  names(prev) <- paste("prev",age_pw,sep="_")
  
  vec <- c(PW, PU, PD, WD, prev)
  
  return(vec)
  
}


widex_vec_to_ex <- function(widex_vec){
  
  p_ind <- grepl("prev", names(widex_vec))
  prev <- widex_vec[p_ind]
  
  PW <- widex_vec[grepl("PW", names(widex_vec))]
  PU <- widex_vec[grepl("PU", names(widex_vec))]
  PD <- widex_vec[grepl("PD", names(widex_vec))]
  WD <- widex_vec[grepl("WD", names(widex_vec))]
  
  age_pw <- parse_number(names(prev))
  age <- parse_number(names(PU))
  
  widex_pars_to_ex(PW=PW,
                   PU=PU,
                   PD=PD,
                   WD=WD,
                   age_ego=65,
                   age_pw=age_pw,
                   prev=prev)
}


