setwd("W:/Margherita/wid_exp/wid_exp")

library(tidyverse)
library(dplyr)
library(data.table)
library(splines)
library(haven)


smooth_prev <- function(data, knots = c(-15,-10,-5,seq(-3,5,by=2),10,15)){
  
  #.age_diffs <- data$age_diff
  .age_diffs <-  tibble(age_diff = -50:50)
  
  
  data %>%
    glm(sqrt(prev) ~ ns(age_diff,
                        knots = knots),
        family = binomial(link = "logit"), data = .) %>% 
   # predict(newdata = tibble(age_diff = .age_diffs),
    predict(newdata = .age_diffs,
            type = "response") %>% 
    as.data.frame() %>% 
    rename(prev_hat = 1) %>% 
    # mutate(age_diff = .age_diffs,
    mutate(age_diff = .age_diffs$age_diff,
           prev_hat = prev_hat^2,
           prev_hat = prev_hat/sum(prev_hat)) %>% 
    left_join(data, by = join_by(age_diff))
}

age_diffs <- c(-50:50)


# GEN_1 ------
load("data/data_8719_gen1_st_by_ed.RData")

data_8719_gen1[, educ := case_when(ututku_aste>=5 & ututku_aste<9 ~ "high",
                                   ututku_aste>=3 & ututku_aste<5 ~ "mid",
                                   # ututku_aste>=9 ~ "low",
                                   ututku_aste=="" ~ "low")]
data_8719_gen1[, educ := factor(educ,
                                levels=c("high","mid","low"))]

data_8719_gen1[, educ_sp := case_when(ututku_aste_sp>=5 & ututku_aste_sp<9 ~ "high",
                                      ututku_aste_sp>=3 & ututku_aste_sp<5 ~ "mid",
                                      # ututku_aste_sp>=9 ~ "low",
                                      ututku_aste_sp=="" ~ "low")]
data_8719_gen1[, educ_sp := factor(educ_sp,
                                   levels=c("high","mid","low"))]

# 1988
prev_8789_gen1 <- data_8719_gen1 %>% 
  filter(vuosi==1988,
         ika%in%c(64:70),
         !is.na(educ),!is.na(educ_sp),
         !is.na(syntyv_sh),!is.na(syntyv_sh)) %>% 
  mutate(edu_shsp = paste(educ,educ_sp,sep="_"),
         age_diff = syntyv_sh-syntyv_sp) %>%
  group_by(age_diff,edu_shsp) %>%
  summarise(n=n(), .groups = "drop") %>% 
  complete(age_diff=age_diffs,edu_shsp,fill=list(n=0)) %>% 
  ungroup() %>%
  group_by(edu_shsp) %>% 
  mutate(prev = n/sum(n)) %>%
  ungroup() %>% 
  mutate(prev = prev^2,
         prev = prev/sum(prev)) %>% 
  arrange(edu_shsp,age_diff)

edu_prop_8789_gen1 <- prev_8789_gen1 %>% 
  summarise(n=sum(n),.by=edu_shsp) %>% 
  mutate(edu_prop = n/sum(n))

pred_prev_8789_gen1 <- prev_8789_gen1 %>%
  group_by(edu_shsp) %>% 
  group_modify(~smooth_prev(data = .)) %>% 
  select(age_diff,edu_shsp,prev_hat) %>% 
  ungroup() 

pred_prev_8789_gen1 %>%
  ggplot(aes(x=age_diff,y=prev_hat))+
  geom_line(linewidth=0.2)+
  geom_line(data=prev_8789_gen1 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
            aes(y=prev),
            linetype="dashed",
            linewidth=0.2)+
  facet_wrap(~edu_shsp)+
  theme_minimal()

pred_prev_8789_gen1 <- pred_prev_8789_gen1 %>% 
  left_join(edu_prop_8789_gen1,by=join_by(edu_shsp)) %>% 
  # # mutate(prev_hat=prev_hat*edu_prop) %>% 
  select(-edu_prop) 
  
# pred_prev_8789_gen1 %>% 
#   group_by(edu_shsp) %>% 
#   summarise(sum=sum(prev_hat))

fwrite(pred_prev_8789_gen1,"tables/prev_age_diff/pred_prev_8789_gen1.txt",
       row.names=F)

# plot_8789_gen1 <- pred_prev_8789_gen1 %>%
#   ggplot(aes(x=age_diff,y=prev_hat))+
#   geom_line(linewidth=0.2)+
#   geom_line(data=prev_8789_gen1 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
#             aes(y=prev),
#             linetype="dashed",
#             linewidth=0.2)+
#   facet_wrap(~edu_shsp)+
#   theme_minimal()

rm(prev_8789_gen1,pred_prev_8789_gen1,edu_prop_8789_gen1)





# 2003
prev_0204_gen1 <- data_8719_gen1 %>% 
  filter(vuosi==2003,
         ika%in%c(64:70),
         !is.na(educ),!is.na(educ_sp),
         !is.na(syntyv_sh),!is.na(syntyv_sh)) %>% 
  mutate(edu_shsp = paste(educ,educ_sp,sep="_"),
         age_diff = syntyv_sh-syntyv_sp) %>%
  group_by(age_diff,edu_shsp) %>%
  summarise(n=n(), .groups = "drop") %>% 
  complete(age_diff=age_diffs,edu_shsp,fill=list(n=0)) %>% 
  ungroup() %>%
  group_by(edu_shsp) %>% 
  mutate(prev = n/sum(n)) %>%
  ungroup() %>% 
  mutate(prev = prev^2,
         prev = prev/sum(prev)) %>% 
  arrange(edu_shsp,age_diff)

edu_prop_0204_gen1 <- prev_0204_gen1 %>% 
  summarise(n=sum(n),.by=edu_shsp) %>% 
  mutate(edu_prop = n/sum(n))

pred_prev_0204_gen1 <- prev_0204_gen1 %>%
  group_by(edu_shsp) %>% 
  group_modify(~smooth_prev(data = .)) %>% 
  select(age_diff,edu_shsp,prev_hat) %>% 
  ungroup() 

pred_prev_0204_gen1 %>%
  ggplot(aes(x=age_diff,y=prev_hat))+
  geom_line(linewidth=0.2)+
  geom_line(data=prev_0204_gen1 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
            aes(y=prev),
            linetype="dashed",
            linewidth=0.2)+
  facet_wrap(~edu_shsp)+
  theme_minimal()

pred_prev_0204_gen1 <- pred_prev_0204_gen1 %>% 
  left_join(edu_prop_0204_gen1,by=join_by(edu_shsp)) %>% 
  # mutate(prev_hat=prev_hat*edu_prop) %>% 
  select(-edu_prop) 

# pred_prev_0204_gen1 %>% 
#   group_by(edu_shsp) %>% 
#   summarise(sum=sum(prev_hat))

fwrite(pred_prev_0204_gen1,"tables/prev_age_diff/pred_prev_0204_gen1.txt",
       row.names=F)

# plot_0204_gen1 <- pred_prev_0204_gen1 %>%
#   ggplot(aes(x=age_diff,y=prev_hat))+
#   geom_line(linewidth=0.2)+
#   geom_line(data=prev_0204_gen1 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
#             aes(y=prev),
#             linetype="dashed",
#             linewidth=0.2)+
#   facet_wrap(~edu_shsp)+
#   theme_minimal()

rm(prev_0204_gen1,pred_prev_0204_gen1,edu_prop_0204_gen1)






# 2018
prev_1719_gen1 <- data_8719_gen1 %>% 
  filter(vuosi==2018,
         ika%in%c(64:70),
         !is.na(educ),!is.na(educ_sp),
         !is.na(syntyv_sh),!is.na(syntyv_sh)) %>% 
  mutate(edu_shsp = paste(educ,educ_sp,sep="_"),
         age_diff = syntyv_sh-syntyv_sp) %>%
  group_by(age_diff,edu_shsp) %>%
  summarise(n=n(), .groups = "drop") %>% 
  complete(age_diff=age_diffs,edu_shsp,fill=list(n=0)) %>% 
  ungroup() %>%
  group_by(edu_shsp) %>% 
  mutate(prev = n/sum(n)) %>%
  ungroup() %>% 
  mutate(prev = prev^2,
         prev = prev/sum(prev)) %>% 
  arrange(edu_shsp,age_diff)

edu_prop_1719_gen1 <- prev_1719_gen1 %>% 
  summarise(n=sum(n),.by=edu_shsp) %>% 
  mutate(edu_prop = n/sum(n))

pred_prev_1719_gen1 <- prev_1719_gen1 %>%
  group_by(edu_shsp) %>% 
  group_modify(~smooth_prev(data = .)) %>% 
  select(age_diff,edu_shsp,prev_hat) %>% 
  ungroup() 

pred_prev_1719_gen1 %>%
  ggplot(aes(x=age_diff,y=prev_hat))+
  geom_line(linewidth=0.2)+
  geom_line(data=prev_1719_gen1 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
            aes(y=prev),
            linetype="dashed",
            linewidth=0.2)+
  facet_wrap(~edu_shsp)+
  theme_minimal()

pred_prev_1719_gen1 <- pred_prev_1719_gen1 %>% 
  left_join(edu_prop_1719_gen1,by=join_by(edu_shsp)) %>% 
  # mutate(prev_hat=prev_hat*edu_prop) %>% 
  select(-edu_prop) 

# pred_prev_1719_gen1 %>% 
#   group_by(edu_shsp) %>% 
#   summarise(sum=sum(prev_hat))

fwrite(pred_prev_1719_gen1,"tables/prev_age_diff/pred_prev_1719_gen1.txt",
       row.names=F)

# plot_1719_gen1 <- pred_prev_1719_gen1 %>%
#   ggplot(aes(x=age_diff,y=prev_hat))+
#   geom_line(linewidth=0.2)+
#   geom_line(data=prev_1719_gen1 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
#             aes(y=prev),
#             linetype="dashed",
#             linewidth=0.2)+
#   facet_wrap(~edu_shsp)+
#   theme_minimal()

rm(prev_1719_gen1,pred_prev_1719_gen1,edu_prop_1719_gen1,data_8719_gen1)










# GEN_2 ------
load("data/data_8719_gen2_st_by_ed.RData")

data_8719_gen2[, educ := case_when(ututku_aste>=5 & ututku_aste<9 ~ "high",
                                   ututku_aste>=3 & ututku_aste<5 ~ "mid",
                                   # ututku_aste>=9 ~ "low",
                                   ututku_aste=="" ~ "low")]
data_8719_gen2[, educ := factor(educ,
                                levels=c("high","mid","low"))]

data_8719_gen2[, educ_sp := case_when(ututku_aste_sp>=5 & ututku_aste_sp<9 ~ "high",
                                      ututku_aste_sp>=3 & ututku_aste_sp<5 ~ "mid",
                                      # ututku_aste_sp>=9 ~ "low",
                                      ututku_aste_sp=="" ~ "low")]
data_8719_gen2[, educ_sp := factor(educ_sp,
                                   levels=c("high","mid","low"))]

# 1988
prev_8789_gen2 <- data_8719_gen2 %>% 
  filter(vuosi==1988,
         ika%in%c(64:70),
         !is.na(educ),!is.na(educ_sp),
         !is.na(syntyv_sh),!is.na(syntyv_sh)) %>% 
  mutate(edu_shsp = paste(educ,educ_sp,sep="_"),
         age_diff = syntyv_sh-syntyv_sp) %>%
  group_by(age_diff,edu_shsp) %>%
  summarise(n=n(), .groups = "drop") %>% 
  complete(age_diff=age_diffs,edu_shsp,fill=list(n=0)) %>% 
  ungroup() %>%
  group_by(edu_shsp) %>% 
  mutate(prev = n/sum(n)) %>%
  ungroup() %>% 
  mutate(prev = prev^2,
         prev = prev/sum(prev)) %>% 
  arrange(edu_shsp,age_diff)

edu_prop_8789_gen2 <- prev_8789_gen2 %>% 
  summarise(n=sum(n),.by=edu_shsp) %>% 
  mutate(edu_prop = n/sum(n))

pred_prev_8789_gen2 <- prev_8789_gen2 %>%
  group_by(edu_shsp) %>% 
  group_modify(~smooth_prev(data = .)) %>% 
  select(age_diff,edu_shsp,prev_hat) %>% 
  ungroup() 

pred_prev_8789_gen2 %>%
  ggplot(aes(x=age_diff,y=prev_hat))+
  geom_line(linewidth=0.2)+
  geom_line(data=prev_8789_gen2 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
            aes(y=prev),
            linetype="dashed",
            linewidth=0.2)+
  facet_wrap(~edu_shsp)+
  theme_minimal()

pred_prev_8789_gen2 <- pred_prev_8789_gen2 %>% 
  left_join(edu_prop_8789_gen2,by=join_by(edu_shsp)) %>% 
  # mutate(prev_hat=prev_hat*edu_prop) %>% 
  select(-edu_prop) 

# pred_prev_8789_gen2 %>% 
#   group_by(edu_shsp) %>% 
#   summarise(sum=sum(prev_hat))

fwrite(pred_prev_8789_gen2,"tables/prev_age_diff/pred_prev_8789_gen2.txt",
       row.names=F)

# plot_8789_gen2 <- pred_prev_8789_gen2 %>%
#   ggplot(aes(x=age_diff,y=prev_hat))+
#   geom_line(linewidth=0.2)+
#   geom_line(data=prev_8789_gen2 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
#             aes(y=prev),
#             linetype="dashed",
#             linewidth=0.2)+
#   facet_wrap(~edu_shsp)+
#   theme_minimal()

rm(prev_8789_gen2,pred_prev_8789_gen2,edu_prop_8789_gen2)





# 2003
prev_0204_gen2 <- data_8719_gen2 %>% 
  filter(vuosi==2003,
         ika%in%c(64:70),
         !is.na(educ),!is.na(educ_sp),
         !is.na(syntyv_sh),!is.na(syntyv_sh)) %>% 
  mutate(edu_shsp = paste(educ,educ_sp,sep="_"),
         age_diff = syntyv_sh-syntyv_sp) %>%
  group_by(age_diff,edu_shsp) %>%
  summarise(n=n(), .groups = "drop") %>% 
  complete(age_diff=age_diffs,edu_shsp,fill=list(n=0)) %>% 
  ungroup() %>%
  group_by(edu_shsp) %>% 
  mutate(prev = n/sum(n)) %>%
  ungroup() %>% 
  mutate(prev = prev^2,
         prev = prev/sum(prev)) %>% 
  arrange(edu_shsp,age_diff)

edu_prop_0204_gen2 <- prev_0204_gen2 %>% 
  summarise(n=sum(n),.by=edu_shsp) %>% 
  mutate(edu_prop = n/sum(n))

pred_prev_0204_gen2 <- prev_0204_gen2 %>%
  group_by(edu_shsp) %>% 
  group_modify(~smooth_prev(data = .)) %>% 
  select(age_diff,edu_shsp,prev_hat) %>% 
  ungroup() 

pred_prev_0204_gen2 %>%
  ggplot(aes(x=age_diff,y=prev_hat))+
  geom_line(linewidth=0.2)+
  geom_line(data=prev_0204_gen2 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
            aes(y=prev),
            linetype="dashed",
            linewidth=0.2)+
  facet_wrap(~edu_shsp)+
  theme_minimal()

pred_prev_0204_gen2 <- pred_prev_0204_gen2 %>% 
  left_join(edu_prop_0204_gen2,by=join_by(edu_shsp)) %>% 
  # mutate(prev_hat=prev_hat*edu_prop) %>% 
  select(-edu_prop) 

# pred_prev_0204_gen2 %>% 
#   group_by(edu_shsp) %>% 
#   summarise(sum=sum(prev_hat))

fwrite(pred_prev_0204_gen2,"tables/prev_age_diff/pred_prev_0204_gen2.txt",
       row.names=F)

# plot_0204_gen2 <- pred_prev_0204_gen2 %>%
#   ggplot(aes(x=age_diff,y=prev_hat))+
#   geom_line(linewidth=0.2)+
#   geom_line(data=prev_0204_gen2 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
#             aes(y=prev),
#             linetype="dashed",
#             linewidth=0.2)+
#   facet_wrap(~edu_shsp)+
#   theme_minimal()

rm(prev_0204_gen2,pred_prev_0204_gen2,edu_prop_0204_gen2)






# 2018
prev_1719_gen2 <- data_8719_gen2 %>% 
  filter(vuosi==2018,
         ika%in%c(64:70),
         !is.na(educ),!is.na(educ_sp),
         !is.na(syntyv_sh),!is.na(syntyv_sh)) %>% 
  mutate(edu_shsp = paste(educ,educ_sp,sep="_"),
         age_diff = syntyv_sh-syntyv_sp) %>%
  group_by(age_diff,edu_shsp) %>%
  summarise(n=n(), .groups = "drop") %>% 
  complete(age_diff=age_diffs,edu_shsp,fill=list(n=0)) %>% 
  ungroup() %>%
  group_by(edu_shsp) %>% 
  mutate(prev = n/sum(n)) %>%
  ungroup() %>% 
  mutate(prev = prev^2,
         prev = prev/sum(prev)) %>% 
  arrange(edu_shsp,age_diff)

edu_prop_1719_gen2 <- prev_1719_gen2 %>% 
  summarise(n=sum(n),.by=edu_shsp) %>% 
  mutate(edu_prop = n/sum(n))

pred_prev_1719_gen2 <- prev_1719_gen2 %>%
  group_by(edu_shsp) %>% 
  group_modify(~smooth_prev(data = .)) %>% 
  select(age_diff,edu_shsp,prev_hat) %>% 
  ungroup() 

pred_prev_1719_gen2 %>%
  ggplot(aes(x=age_diff,y=prev_hat))+
  geom_line(linewidth=0.2)+
  geom_line(data=prev_1719_gen2 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
            aes(y=prev),
            linetype="dashed",
            linewidth=0.2)+
  facet_wrap(~edu_shsp)+
  theme_minimal()

pred_prev_1719_gen2 <- pred_prev_1719_gen2 %>% 
  left_join(edu_prop_1719_gen2,by=join_by(edu_shsp)) %>% 
  # mutate(prev_hat=prev_hat*edu_prop) %>% 
  select(-edu_prop) 

# pred_prev_1719_gen2 %>% 
#   group_by(edu_shsp) %>% 
#   summarise(sum=sum(prev_hat))

fwrite(pred_prev_1719_gen2,"tables/prev_age_diff/pred_prev_1719_gen2.txt",
       row.names=F)

# plot_1719_gen2 <- pred_prev_1719_gen2 %>%
#   ggplot(aes(x=age_diff,y=prev_hat))+
#   geom_line(linewidth=0.2)+
#   geom_line(data=prev_1719_gen2 %>% mutate(prev=prev/sum(prev),.by=edu_shsp),
#             aes(y=prev),
#             linetype="dashed",
#             linewidth=0.2)+
#   facet_wrap(~edu_shsp)+
#   theme_minimal()

rm(prev_1719_gen2,pred_prev_1719_gen2,edu_prop_1719_gen2,data_8719_gen2)

