setwd("C:/Users/moremarg/PERSONAL FOLDER/POSTDOC/WIDEX")

library(tidyverse)
library(data.table)
library(ggplot2)
library(HMDHFDplus)
library(readxl)

# check lifexp at age 65 in finland 
# 88, 01, 18
ex_FI_F <- readHMDweb(CNTR="FIN", item="fltper_1x1",
                    username="margherita.moretti@uniroma1.it",
                    password="Tita1992!") %>%
  dplyr::select(Year,Age,ex) %>% 
  filter(Age == 65 & Year %in% c(1988,2001,2018))
# comparison with msm estimates
# Year Age  ex
# 1988  65 17.57 -- (17.5-19.8)
# 2001  65 19.66 -- (19.9-22.1)
# 2018  65 21.85 -- (21.2-24.1)


ex_FI_M <- readHMDweb(CNTR="FIN", item="mltper_1x1",
                      username="margherita.moretti@uniroma1.it",
                      password="Tita1992!") %>%
  dplyr::select(Year,Age,ex) %>% 
  filter(Age == 65 & Year %in% c(1988,2001,2018))
# comparison with msm estimates
# Year Age  ex
# 1988  65 13.52 -- (13.4-16.1)
# 2001  65 15.72 -- (15.9-18.8)
# 2018  65 18.37 -- (17.3-20.9)




# abs nr. of widow by gender -----
tab_n_abs_age_F <- read_csv("output/output_05_10_2023/tab_n_abs_age_F.csv", 
                            col_types = cols(den = col_skip(),
                                             prev = col_skip())) %>% 
  mutate(sex="F")
tab_n_abs_age_M <- read_csv("output/output_05_10_2023/tab_n_abs_age_M.csv", 
                            col_types = cols(den = col_skip(),
                                             prev = col_skip()))%>% 
  mutate(sex="M")

tab_n_abs_age <- rbind(tab_n_abs_age_F,tab_n_abs_age_M)
rm(tab_n_abs_age_F,tab_n_abs_age_M)


tab_n_abs_age %>% 
  mutate(sex=factor(sex, levels=c("F","M"),
                    labels=c("women","men"))) %>% 
  ggplot(aes(x=vuosi, y=n_abs,fill=ika_3))+
  geom_bar(stat="identity", position = position_stack(reverse = F))+
  facet_grid(rows =vars(sex),scales="free_y")+
  scale_fill_viridis_d()+
  scale_x_continuous(breaks = c(1987,seq(1990,2016,by=5),2019))+
  labs(x="Years",y="abs. number")+
  ggtitle("Trend of widows in Finland 1987-2019")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(size=14))+
  guides(fill=guide_legend(title = "age classes"))

ggsave(file="output/n_abs_gender_age.png",
       width=7, height = 5)


# reverse
tab_n_abs_age %>% 
  mutate(sex=factor(sex, levels=c("F","M"),
                    labels=c("women","men"))) %>% 
  ggplot(aes(x=vuosi, y=n_abs,fill=fct_rev(ika_3)))+
  geom_bar(stat="identity", position = position_stack(reverse = F))+
  facet_grid(rows =vars(sex),scales="free_y")+
  scale_fill_viridis_d(direction = 1)+
  scale_x_continuous(breaks = c(1987,seq(1990,2016,by=5),2019))+
  labs(x="Years",y="abs. number")+
  ggtitle("Trend of widows in Finland 1987-2019")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(size=14))+
  guides(fill=guide_legend(title = "age classes",
                           reverse = T))

ggsave(file="output/n_abs_gender_age_rev_2.png",
       width=7, height = 5)




# prev over time
prev_age_F <- read_csv("output/output_05_10_2023/tab_n_abs_age_F.csv") %>% 
  mutate(sex="F")
prev_age_M <- read_csv("output/output_05_10_2023/tab_n_abs_age_M.csv")%>% 
  mutate(sex="M")

prev_age <- rbind(prev_age_F,prev_age_M)
rm(prev_age_F,prev_age_M)

prev_age %>% 
  mutate(sex=factor(sex, levels=c("F","M"),
                    labels=c("women","men"))) %>% 
  ggplot(aes(x=vuosi, y=prev,color=ika_3))+
  geom_line(size=1)+
  facet_grid(rows =vars(sex))+
  scale_color_viridis_d()+
  scale_x_continuous(breaks = c(1987,seq(1990,2016,by=5),2019))+
  scale_y_continuous(limits=c(0,100),
                     breaks = c(seq(0,100,by=20)))+
  labs(x="Years",y="abs. number")+
  ggtitle("Prevalence of widows in Finland 1987-2019")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(size=14))+
  guides(color=guide_legend(title = "age classes"))

ggsave(file="output/prev_gender_age.png",
       width=7, height = 5)



# widex over time ----
table_widex_time_edu <- read_excel("C:/Users/moremarg/PERSONAL FOLDER/POSTDOC/WIDEX/output/table_widex_time_edu.xlsx", 
                                   col_types = c("text", "numeric", "text", "text"))

table_widex_time_edu %>%
  mutate(edu=factor(edu, levels=c("L","M","H"),
                    labels=c("low","mid","high")),
         sex=factor(sex, levels=c("F","M"),
                    labels=c("women","men"))) %>% 
  ggplot(aes(x=period,y=value,fill=edu)) +
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(cols=vars(sex))+
  scale_y_continuous(limits=c(0,10), breaks = c(seq(0,10,by=2)))+
  scale_fill_viridis_d(option="F",begin=0.2, end=0.8, alpha=0.9)+
  ggtitle("Widowhood expectancy at age 65 in Finland")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(size=14))+
  labs(x="Years",y="nr. of years")+
  guides(fill=guide_legend(title = "Level of\neducation"))

ggsave(file="output/exp_overtime_edu.png",
       width=7, height = 4)



# educational gap
wide_table_widex_time_edu <- table_widex_time_edu %>% 
  pivot_wider(names_from = edu, values_from = value) %>% 
  mutate(gap=H-L)







# AGE AT ONSET
mean_age <- read_excel("output/mean_age.xlsx")

mean_age %>%
  mutate(edu=factor(edu, levels=c("L","M","H"),
                    labels=c("low","mid","high")),
         sex=factor(sex, levels=c("F","M"),
                    labels=c("women","men")),
         year=factor(year, levels=c("_8789","_0204","_1719"),
                     labels=c("1988","2003","2018"))) %>% 
  select(!`lifetime risk`) %>% 
  ggplot(aes(x=year,y=`mean age`,fill=edu)) +
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(cols=vars(sex))+
  scale_y_continuous(limits=c(0,82), breaks = c(seq(0,80,by=20)))+
  scale_fill_viridis_d(option="G",begin=0.1, end=0.8, alpha=0.9)+
  ggtitle("Avg. age at onset of widowhood")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(size=14))+
  labs(x="Years",y="avg. age")+
  guides(fill=guide_legend(title = "Level of\neducation"))

ggsave(file="output/mean_age.png",
       width=7, height = 4)



# lifetime risk
mean_age <- read_excel("output/mean_age.xlsx")

mean_age %>%
  mutate(edu=factor(edu, levels=c("L","M","H"),
                    labels=c("low","mid","high")),
         gender=factor(sex, levels=c("F","M"),
                    labels=c("women","men")),
         year=factor(year,
                     levels=c("_8789","_0204","_1719")),
         year=as.numeric(year),
         year=case_when(year == 1 ~ 1988,
                        year == 2 ~ 2003,
                        year == 3 ~ 2018)) %>% 
  select(!`mean age`) %>% 
  ggplot(aes(x=year,y=`lifetime risk`,linetype=gender,color=edu)) +
  geom_line(size=1)+
  scale_linetype_manual(values=c("twodash","solid"))+
  scale_y_continuous(limits=c(0,0.8), breaks = c(seq(0,0.80,by=0.20)))+
  scale_x_continuous(limits=c(1988,2018), breaks = c(1988,2003,2018))+
  scale_color_viridis_d(option="G",begin=0.1, end=0.8, alpha=0.9)+
  ggtitle("Lifetime risk of widowhood")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(size=14),
        axis.text.x= element_text())+
  labs(x="Years",y="lifetime risk")+
  guides(color=guide_legend(title = "Level of\neducation"))

ggsave(file="output/lifetimerisk.png",
       width=7, height = 3)
