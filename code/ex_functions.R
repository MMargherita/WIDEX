
# 1: partnered    P
# 2: unpartnered  U
# 3: widowed      W
# 4: dead         D


# Note: UW and WU should be structural 0s
library(readODS)
library(tidyverse)
library(collapse)
library(tidyfast)
tmat <- read_ods("output/output_17_11_23/trans_mat/trans_mat_gen1_0204_high.ods")

init <- c(P=.86,U=.1,W = .04)

p_tibble <-
  tmat |> 
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
  select(-transition) 

# rather flexible, defaults refer to use inside nested data.frame,
# but it can also return a matrix proper if matrix == TRUE
transient_block <- function(p_tibble, from = "P", to = "P", matrix = FALSE){
  # TR, this function is rethought to be subservient to the  
  # transient_matrix function, needs data.frame output, not matrix,
  # don't bother w rownames. 
  # Needed leading 0s for ages for "alphabetization",
  if (matrix){
  from <- from[1]
  to   <- to[1]
  # TR: we can make this more computationally efficient if it becomes necessary
  p_tibble <-
    p_tibble |> 
    # TR: for the record, I hate this sort
    # of construct
    filter(from == (!!from),
           to == (!!to)) 
  }
  all_age <- min(p_tibble$age):(max(p_tibble$age)+1)
  
  if (matrix){
    block <-
      p_tibble |> 
      fselect(age_from = age, 
              p,
              from,
              to) |> 
      fmutate(age_to = age_from + 1) |> 
      complete(age_from = all_age,
               age_to = all_age,
               from, 
               to,
               fill = list(p = 0)) |> 
      fmutate(age_to = sprintf("%03d", age_to),
              age_from = sprintf("%03d", age_from),
              age_to = paste(to, age_to, sep = "::"),
              age_from = paste(from, age_from, sep = "::"))  |> 
      dt_pivot_wider(names_from = age_from,
                     values_from = p) |> 
      fselect(-to,-from) |> 
      column_to_rownames("age_to") |> 
      as.matrix()
  } else {
    block <-
      p_tibble |> 
      fselect(age_from = age, 
              p,
              from,
              to) |> 
      fmutate(age_to = age_from + 1) |> 
      complete(age_from = all_age,
               age_to = all_age,
               from, 
               to,
               fill = list(p = 0)) |> 
      fmutate(age_to = sprintf("%03d",age_to),
              age_from = sprintf("%03d",age_from),
              age_to = paste(to,age_to,sep="::"))  |> 
      dt_pivot_wider(names_from = age_from,
                  values_from = p) |> 
      fselect(-to,-from)
  }
  block
}

transient_matrix <- function(p_tibble, transient_states = c("P","U","W")){
  # to be continued
  
  # from_to_list <- outer(states,
  #                       states,
  #                       paste0) |> 
  #   sort()
  
  # a hack until i have a better solution
  age_name_keep <- paste0(transient_states[[1]],"::age_to")
  
  p_tibble |> 
    filter(to %in% transient_states) |> 
    arrange(from, to, age) |> 
    group_by(from, to) |> 
    # use this because grouping variables kept inside!
    group_nest(keep = TRUE) |> 
    mutate(data = map(data, ~.x |> 
                        transient_block(matrix=FALSE) )) |> 
    # arrange blocks into U configuration
    pivot_wider(names_from = from, values_from = data) |> 
    # stacks blocks in column
    unnest(cols = all_of(states),names_sep="::") |> 
    ungroup() |> 
    # everything else is hackishness to get destination::age in rownames
    select(-to) |> 
    rename(to = (!!age_name_keep)) |> 
    select(-ends_with("age_to")) |> 
    column_to_rownames("to") |> 
    as.matrix()
}

transient_block(p_tibble)
transient_matrix(p_tibble)


