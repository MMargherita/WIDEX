setwd("C:/Users/moremarg/PERSONAL FOLDER/POSTDOC/WIDEX")

library(tidyverse)
library(readODS)
library(data.table)


res_fun <- function(gen=gen,anno=anno,edu=edu){

  data <- paste(gen,anno,edu, sep="_")
  data <- read_ods(paste(paste("output/output_17_11_23/trans_mat/trans_mat",gen,anno,edu, sep="_"),"ods",sep="."))[-1,]%>%
    rename(age = rownames,
           PP = X_p11,
           PU = X_p12,
           PW = X_p13,
           PD = X_p14,
           UP = X_p21,
           UU = X_p22,
           UW = X_p23,
           UD = X_p24,
           WP = X_p31,
           WU = X_p32,
           WW = X_p33,
           WD = X_p34) %>% 
    mutate(PP = PP,
           PU = PU,
           PW = PW,
           PD = PD,
           UP = 0,
           UU = 1,
           UW = 0,
           UD = 0,
           WP = 0,
           WU = 0,
           WW = 1-WD,
           WD = WD)

  fwrite(data, paste(paste("output/trans_mat_res/tmat",gen,anno,edu, sep="_"),"ods",sep="."),
       row.names=F)
}




res_fun(gen="gen1",anno="8789",edu="low")
res_fun(gen="gen2",anno="8789",edu="low")

res_fun(gen="gen1",anno="8789",edu="mid")
res_fun(gen="gen2",anno="8789",edu="mid")

res_fun(gen="gen1",anno="8789",edu="high")
res_fun(gen="gen2",anno="8789",edu="high")


res_fun(gen="gen1",anno="0204",edu="low")
res_fun(gen="gen2",anno="0204",edu="low")

res_fun(gen="gen1",anno="0204",edu="mid")
res_fun(gen="gen2",anno="0204",edu="mid")

res_fun(gen="gen1",anno="0204",edu="high")
res_fun(gen="gen2",anno="0204",edu="high")


res_fun(gen="gen1",anno="1719",edu="low")
res_fun(gen="gen2",anno="1719",edu="low")

res_fun(gen="gen1",anno="1719",edu="mid")
res_fun(gen="gen2",anno="1719",edu="mid")

res_fun(gen="gen1",anno="1719",edu="high")
res_fun(gen="gen2",anno="1719",edu="high")
