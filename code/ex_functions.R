# Note: UW and WU should be structural 0s

library(tidyverse)
library(collapse)
library(tidyfast)


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
    unnest(cols = all_of(transient_states),names_sep="::") |> 
    ungroup() |> 
    # everything else is hackishness to get destination::age in rownames
    select(-to) |> 
    rename(to = (!!age_name_keep)) |> 
    select(-ends_with("age_to")) |> 
    column_to_rownames("to") |> 
    as.matrix()
}

# transient_block(p_tibble, matrix = TRUE)
# 
# p_tibble |> 
#   filter(from == "P",
#          to == "W") |> 
# transient_block(matrix = FALSE)


p_tibble_to_attrition_vec <- function(p_tibble){
    p_tibble  %>% 
      fsubset(from != to)  %>% 
      fmutate(rowname = paste(from,to,age,sep="_"))  %>% 
      fselect(rowname,p) %>% 
      deframe() 
}

attrition_vec_to_p_tibble <- function(attrition_vec){
  attrition_vec |> 
    enframe(value = "p") |> 
    separate_wider_delim(name, delim = "_", names = c("from","to","age")) |> 
    dt_pivot_wider(names_from = to, values_from = p) |> 
    dt_pivot_longer(D:W, names_to = "to", values_to = "p") |> 
    group_by(from, age) |> 
    fmutate(p = ifelse(is.na(p), 1 - sum(p, na.rm = TRUE), p),
            age = as.integer(age)) |> 
    arrange(age,from)
}

# all.equal(
# transient_matrix(p_tibble),
# 
# p_tibble_to_attrition_vec(p_tibble) |> 
#   attrition_vec_to_p_tibble() |> 
#   transient_matrix(),
# tolerance = 1e-7)

transient_to_fundamental <- function(transient_matrix){
  I <- diag(nrow = nrow(transient_matrix))
  # TR: maybe remove dudel discount and instead 
  # interp over lxs
  N <- solve(I - transient_matrix) - (I / 2)
  N
}


fundamental_to_ex <- function(fundamental_matrix, 
                              x = 65, 
                              init = c(P = .91, U = .08, W = .01),
                              state = "all"){
  init <- enframe(init, name = "from", value = "pi")
  
  exs <- 
    fundamental_matrix |> 
    as.data.frame() |> 
    rownames_to_column("to") |> 
    dt_pivot_longer(-to, names_to = "from", values_to = "lxs") |> 
    separate_wider_delim(to,names = c("to","age_to"),delim = "::") |> 
    separate_wider_delim(from,names = c("from","age"),delim = "::") |> 
    fmutate(age = as.integer(age),
            age_to = as.integer(age_to))  |> 
    fsubset(age_to >= age) |> 
    fsubset(age == x) |> 
    fgroup_by(from, to) |> 
    fsummarise(exs = sum(lxs)) |> 
    left_join(init, by = join_by(from)) |> 
    fgroup_by(to) |> 
    fsummarise(exs = sum(exs * pi))
  
  if (state == "all"){
    return(exs)
  }
  
  exs |> 
  fsubset(to == state)
  
}

attrition_vec_to_ex <- function(attrition_vec, state = "all"){
  init_ind      <- nchar(names(attrition_vec)) == 1
  init          <- attrition_vec[init_ind]
  attrition_vec <- attrition_vec[!init_ind]
  
  attrition_vec |> 
    attrition_vec_to_p_tibble() |> 
    transient_matrix() |> 
    transient_to_fundamental() |> 
    fundamental_to_ex(init = init, state = state) |> 
    deframe()
}

horiuchi <- function (func, pars1, pars2, N, ...) 
{
  d <- pars2 - pars1
  n <- length(pars1)
  delta <- d/N
  grad  <- matrix(rep(0.5:(N - 0.5)/N, n), byrow = TRUE, ncol = N,
                  dimnames = list(names(pars1), 1:N))
  x     <- pars1 + d * grad
  
  cc    <- matrix(0, nrow = n, ncol = N,
                  dimnames = list(names(pars1), 1:N))
  DD <- diag(delta / 2)
  rownames(DD) <- names(pars1)
  for (j in 1:N) {
    
    for (i in 1:n) {
      cc[i, j] <- func((x[, j] + DD[, i]), ...) - 
                  func((x[, j] - DD[, i]), ...)
    }
  }
  out <- rowSums(cc)
  names(out) <- names(pars1)
  out
}
