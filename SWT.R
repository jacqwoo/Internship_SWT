rm(list=ls())

# installing packages
install.packages(c("readstata13","reshape2","tidyverse"))

# loading packages

library(tidyverse)
library(readstata13)
library(foreign)
library(reshape2)
library(ggplot2)

if (!require(devtools)){
  install.packages('devtools')
  library(devtools)
}
install_github("larmarange/JLutils")

library(JLutils)

## tidyverse has conflicts with dplyr, so when you want to call filter() and lag(), do stats::filter() or stats::lag()
## for more information on tidyverse and the packages contained within it: https://www.analyticsvidhya.com/blog/2019/05/beginner-guide-tidyverse-most-powerful-collection-r-packages-data-science/

# obtaining node name for your system
user <- Sys.info()["nodename"]

# setting root
if(user == "LAPTOP-11NK5GCF"){
  path_swt <- "C:/Users/admin/Documents/SWT/Graphs/"
}


#file paths
##path_clean_data <- paste0(path_care, '1-input/')
##path_images <- paste0(path_care,'5-graphs/')

# import dataset
df <- read.csv(paste0(path_swt,"Fig 2.csv"))

# laterite ggplot2 theme
## Modifications: changed the base font to sans and removed bolding from axes labels
theme_laterite <- function(){
  theme_bw(base_size = 12, base_family = "sans") %+replace%
    theme(
      #plot.margin = unit(c(20,20,10,10), "pt"),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title = element_text(size=10.25, colour = "black"),
      legend.position="bottom",
      legend.text=element_text(size=8)
      #legend.text.align = 0, 
      #legend.spacing.x = unit(0.15,'cm')
      #plot.title = element_text(hjust = 0.5)
    )
}

#Colour palette
lat_red1 = "#C31420" #"195 20 32" * Laterite Red, Accent 1
lat_red2 = "#EF5E68" #"239 94 104" * Laterite Red, Accent 1, 40%lighter
lat_red3 = "#920f18" #"146 15 24" * Laterite Red, Accent 1, 25%darker
lat_red4 <- "#ff9999" # (255,153,153) *Lightest Red
lat_lime1 = "#aeca2a"  #"174 202 42" * Laterite Lime, Accent 2
lat_lime2 = "#7eba31" #"126 186 49" * Laterite Lime, Accent 6
lat_lime3 = "#d1e47b" #"209 228 123" * Laterite Lime, Accent 2, 40%lighter
lat_lime4 = "#b2dd7c" #"178 221 124" * Laterite Lime, Accent 6, 40%lighter
lat_lime5 = "#82971f" #"130 151 31" * Laterite Lime, Accent 2, 25%darker
lat_lime6 = "#5f8b25" #"95 139 37" * Laterite Lime, Accent 6, 25%darker
lat_green1 = "#4d9034" #"77 144 52" * Laterite Dark Green, Background 2
lat_green2 = "#8dcd75" #"141 205 117" * Laterite Dark Green, 40%lighter
lat_green3 = "#3a6c27" #"58 108 39" * Laterite Dark Green, 25%darker
lat_gold1 = "#f8c43a" #"248 196 58" * Laterite Gold, Accent 5
lat_gold2 = "#fbdc89" #"251 220 137" * Laterite Gold, Accent 5, 40%lighter
lat_gold3 = "#dea308" #"222 163 8" * Laterite Gold, Accent 5, 25%darker

# Fig 2. Highest Grade Attended for Out-of-school Youth Aged 20-24
df %>% 
  ggplot(aes(Country,prop,fill = Education)) + #define a mapping using aes: c3e mapped on x-axis, share mapped on y-axis. using fill=variable means bars will be separately coloured based on that variable
  geom_bar(position = position_fill(reverse=T), stat = "identity")  + #defining graph as a  bar chart. not entirely sure what position and stat do 
  ylab("Share of youth (%)") + xlab("") + labs(fill="") +
  scale_fill_manual(values = c(lat_red1, lat_gold1, lat_gold2, lat_lime1)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.1), breaks = seq(0,1,0.2),labels = scales::percent) + #clips y-axis
  geom_text(aes(label=scales::percent(prop, accuracy=1)), position=position_fill(0.5, reverse=T)) +
  theme_laterite()

rm(df.plot)

# Fig 3. Gap Between Youth Turning 18 each Year and Number of Formal Sector Jobs

df.plot<- df %>% select(student_id,wave,c3h) %>%
  group_by(wave,c3h) %>%
  summarise(total = n()) %>%
  group_by(wave) %>% 
  mutate(wotal = sum(total)) %>%
  mutate(share = total/wotal)

df.plot$c3h <- as.factor(df.plot$c3h)
df.plot %>% 
  filter(!is.na(c3h)) %>%
  ggplot(aes(c3h,share,fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Confidence about helpfulness of Doctors/Nurses") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,.9), breaks = seq(0,.9,0.15),labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 20, hjust=0.95)) +
  scale_x_discrete(labels= c("Disagree Strongly","Disagree somewhat","Agree somewhat","Agree strongly")) 
ggsave(paste0(path_images, 'bee_conf_srh_doctornurse.png'), width = 5, height = 5)

rm(df.plot)

# H7. Do you feel safe at school?

df.plot <- df %>% select(student_id,wave,h7,student_gender) %>%
  group_by(wave,h7) %>%
  summarise(total = n()) %>%
  group_by(wave) %>% 
  mutate(wotal = sum(total)) %>%
  mutate(share = total/wotal)

df.plot$h7 <- as.factor(df.plot$h7)
df.plot %>% 
  filter(!is.na(h7)) %>%
  ggplot(aes(h7,share,fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Do you feel safe at school?") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,.8), breaks = seq(0,.8,0.2),labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 20, hjust=0.95)) +
  scale_x_discrete(labels= c("Disagree Strongly","Disagree somewhat","Agree somewhat","Agree strongly")) 
ggsave(paste0(path_images, 'bee_school_safety_feeling.png'), width = 5, height = 5)

rm(df.plot)

# H7. Do you feel safe at school? (Only Girls)

df.plot <- df %>% select(student_id,wave,h7,student_gender) %>%
  filter(student_gender == 1) %>%
  group_by(wave,h7) %>%
  summarise(total = n()) %>%
  group_by(wave) %>% 
  mutate(wotal = sum(total)) %>%
  mutate(share = total/wotal)

df.plot$h7 <- as.factor(df.plot$h7)
df.plot %>% 
  filter(!is.na(h7)) %>%
  ggplot(aes(h7,share,fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Do you feel safe at school? (Only Girls)") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,.8), breaks = seq(0,.8,0.2),labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 20, hjust=0.95)) +
  scale_x_discrete(labels= c("Disagree Strongly","Disagree somewhat","Agree somewhat","Agree strongly")) 

ggsave(paste0(path_images, 'bee_school_safety_feeling_girls.png'), width = 5, height = 5)

rm(df.plot)

# E4. Where do you get the money to save? (select all that apply)

df.plot <- df %>% select(student_id,wave, e4_1, e4_2, e4_3, e4_4, e4_5, e4_6, e4_7 ) %>%
  #na.omit %>%
  melt(id.vars = c("student_id","wave")) %>%
  group_by(variable,wave) %>%
  summarize(mean = mean(as.numeric(value),na.rm = TRUE)) 

df.plot <- merge(df.plot, saste_nashe, by.x = "variable", by.y = "multi_variable")

df.plot %>%
  ggplot(aes(reorder(labelenglish,-mean), y = mean, fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Source of Savings?") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(limits = c(0,.6), breaks = seq(0,.6,0.1),expand = c(0,0), labels = scales::percent) +
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 30, hjust=0.95))

ggsave(paste0(path_images, 'bee_savings_sources.png'), width = 5, height = 5)

rm(df.plot)

# E11. Types of income generating activities: (check all that apply)

df.plot <- df %>% select(student_id,wave,e11_1:e11_8) %>% 
  na.omit %>%
  melt(id.vars = c("student_id","wave")) %>%
  group_by(variable,wave) %>%
  mutate(mean = mean(value)) %>%
  filter(variable != "e11_98", variable != "e11_6", variable != "e11_8") 

df.plot <- merge(df.plot, saste_nashe, by.x = "variable", by.y = "multi_variable")

df.plot %>% 
  ggplot(aes(reorder(labelenglish,-mean), y = mean, fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Different types of income generating activities?") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(limits = c(0,.5), breaks = seq(0,.5,0.1),expand = c(0,0), labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 30, hjust=0.95))

ggsave(paste0(path_images, 'bee_income_gen_acts.png'), width = 5, height = 5)

rm(df.plot)

# H9. Do you feel safe at home?

df.plot <- df %>% select(student_id,wave,h9,student_gender) %>%
  group_by(wave,h9) %>%
  summarise(total = n()) %>%
  group_by(wave) %>% 
  mutate(wotal = sum(total)) %>%
  mutate(share = total/wotal)

df.plot$h9 <- as.factor(df.plot$h9)
df.plot %>% 
  filter(!is.na(h9)) %>%
  ggplot(aes(h9,share,fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Do you feel safe at home?") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,.9), breaks = seq(0,.9,0.15),labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 20, hjust=0.95)) +
  scale_x_discrete(labels= c("Disagree Strongly","Disagree somewhat","Agree somewhat","Agree strongly")) 

ggsave(paste0(path_images, 'bee_home_safety_feeling.png'), width = 5, height = 5)

rm(df.plot)

# H9. Do you feel safe at home? (Only Girls)

df.plot <- df %>% select(student_id,wave,h9,student_gender) %>%
  filter(student_gender == 1) %>%
  group_by(wave,h9) %>%
  summarise(total = n()) %>%
  group_by(wave) %>% 
  mutate(wotal = sum(total)) %>%
  mutate(share = total/wotal)

df.plot$h9 <- as.factor(df.plot$h9)
df.plot %>% 
  filter(!is.na(h9)) %>%
  ggplot(aes(h9,share,fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Do you feel safe at home? (Only Girls)") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,.9), breaks = seq(0,.9,0.15),labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 20, hjust=0.95)) +
  scale_x_discrete(labels= c("Disagree Strongly","Disagree somewhat","Agree somewhat","Agree strongly")) 

ggsave(paste0(path_images, 'bee_home_safety_feeling_girls.png'), width = 5, height = 5)

rm(df.plot)

#money source

df.plot <- df %>% select(student_id,wave,d2_1,d2_2,d2_3,d2_4,d2_5,d2_6,d2_7,d2_8,d2_9,d2_10) %>% 
  melt(id.vars = c("student_id","wave")) %>%
  group_by(variable,wave) %>%
  summarize(mean = mean(as.numeric(value),na.rm = TRUE))

df.plot <- merge(df.plot, saste_nashe, by.x = "variable", by.y = "multi_variable")

df.plot  %>% 
  ggplot(aes(reorder(labelenglish,-mean), y = mean, fill = wave,group = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Source of Money") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_lime1,lat_green3),labels= c("Baseline","Endline")) +
  scale_y_continuous(limits = c(0,.8), breaks = seq(0,1,0.2),expand = c(0,0), labels = scales::percent) + 
  theme_laterite() + theme(axis.text.x = element_text(angle= 30, hjust=0.95)) 

ggsave(paste0(path_images, 'bee_needs_money.png'), width = 5, height = 5)

#next meal

df.plot<- df %>% select(student_id,wave,d4) %>%
  group_by(wave,d4) %>%
  summarise(total = n()) %>%
  group_by(wave) %>% 
  mutate(wotal = sum(total)) %>%
  mutate(share = total/wotal)

df.plot$d4 <- as.factor(df.plot$d4)

df.plot %>% 
  filter(!is.na(d4)) %>%
  ggplot(aes(d4,share,fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Confidence about getting next meal") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_lime1,lat_green3),labels= c("Baseline","Endline")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,.6), breaks = seq(0,.6,0.1),labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 20, hjust=0.95)) +
  scale_x_discrete(labels= c("Disagree Strongly","Disagree somewhat","Agree somewhat","Agree strongly")) 
ggsave(paste0(path_images, 'bee_needs_nextmeal.png'), width = 5, height = 5)

#body change info
df.plot <- df %>% select(student_id,wave,c1a) %>%
  mutate(c1a= as.factor(recode(as.character(c1a)
                               ,"Yes" = "1", "No" = "0","Refuse to answer" = "NA" ))) %>%
  group_by(wave)%>%
  summarise(sum = sum(as.numeric(as.character(c1a)), na.rm = TRUE), 
            mean = mean(as.numeric(as.character(c1a)), na.rm = TRUE))


df.plot <- df %>% select(student_id,wave,starts_with("c1b_")) %>% 
  select(-c(c1b_,c1b_96,c1b_88)) %>% 
  melt(id.vars = c("student_id","wave")) %>%
  group_by(variable,wave) %>%
  summarize(mean = mean(as.numeric(value),na.rm = TRUE))%>%
  filter(variable != "c1b_9",variable != "c1b_15")

df.plot <- merge(df.plot, saste_nashe, by.x = "variable", by.y = "multi_variable")

df.plot %>% 
  ggplot(aes(reorder(labelenglish,-mean), y = mean, fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Source of Body change information") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1)) +
  scale_y_continuous(limits = c(0,.7), breaks = seq(0,.7,0.1),expand = c(0,0), labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 30, hjust=0.95)) 

#scale_x_discrete(labels = c(paste0(df.labels$choices[df.labels$name == "c1a"])))


# + scale_x_discrete(labels= c("","")) 

ggsave(paste0(path_images, 'bee_bodychangeinfo_money.png'), width = 5, height = 5)

#aids info

df.plot <- df %>% select(student_id,wave,starts_with("c1d_")) %>% 
  select(-c(c1d_,c1d_96,c1d_88)) %>% 
  melt(id.vars = c("student_id","wave")) %>%
  group_by(variable,wave) %>%
  summarize(mean = mean(as.numeric(value),na.rm = TRUE))%>%
  filter(variable != "c1d_15")

df.plot <- merge(df.plot, saste_nashe, by.x = "variable", by.y = "multi_variable")


df.plot %>% 
  ggplot(aes(reorder(labelenglish,-mean), y = mean, fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Source of HIV information") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(limits = c(0,.60), breaks = seq(0,.6,0.1),expand = c(0,0), labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 30, hjust=0.95)) 

# + scale_x_discrete(labels= c("","")) 

ggsave(paste0(path_images, 'bee_srh_aidsinfo.png'), width = 5, height = 5)

# reason for no hospital visit

df.plot <- df %>% select(student_id,wave,starts_with("c1f_")) %>% 
  select(-c(c1f_,c1f_96)) %>% 
  melt(id.vars = c("student_id","wave")) %>%
  group_by(variable,wave) %>%
  summarize(mean = mean(as.numeric(value),na.rm = TRUE)) %>%
  filter(variable != "c1f_9",variable != "c1f_10",variable != "c1f_11",variable != "c1f_12",variable != "c1f_13")

df.plot <- merge(df.plot, saste_nashe, by.x = "variable", by.y = "multi_variable")


df.plot %>% 
  ggplot(aes(reorder(labelenglish,-mean), y = mean, fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Reason for no SRH related hospital visits") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(limits = c(0,.7), breaks = seq(0,.7,0.1),expand = c(0,0), labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 30, hjust=0.95))

ggsave(paste0(path_images, 'bee_srh_hosp.png'), width = 5, height = 5)

# confidence health access - condom

df.conf.condom<- df %>% select(student_id,wave,c3a) %>%
  group_by(wave,c3a) %>%
  summarise(total = n()) %>%
  group_by(wave) %>% 
  mutate(wotal = sum(total)) %>%
  mutate(share = total/wotal)

df.conf.condom$c3a <- as.factor(df.conf.condom$c3a)

df.conf.condom %>% 
  filter(!is.na(c3a)) %>%
  ggplot(aes(c3a,share,fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Confidence about access to condoms") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,.9), breaks = seq(0,.9,0.15),labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 20, hjust=0.95)) +
  scale_x_discrete(labels= c("Disagree Strongly","Disagree somewhat","Agree somewhat","Agree strongly")) 
ggsave(paste0(path_images, 'bee_srh_conf_condom.png'), width = 5, height = 5)

# confidence health access - hivtest

df.conf.hivtest<- df %>% select(student_id,wave,c3b) %>%
  group_by(wave,c3b) %>%
  summarise(total = n()) %>%
  group_by(wave) %>% 
  mutate(wotal = sum(total)) %>%
  mutate(share = total/wotal)

df.conf.hivtest$c3b <- as.factor(df.conf.hivtest$c3b)

df.conf.hivtest %>% 
  filter(!is.na(c3b)) %>%
  ggplot(aes(c3b,share,fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Confidence about access to HIV test") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = seq(0,1,0.2),labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 20, hjust=0.95)) +
  scale_x_discrete(labels= c("Disagree Strongly","Disagree somewhat","Agree somewhat","Agree strongly")) 
ggsave(paste0(path_images, 'bee_srh_conf_hivtest.png'), width = 5, height = 5)

# GBV

df.plot <- df %>% select(student_id,wave,starts_with("c4d2_")) %>% 
  select(-c(c4d2_,c4d2_96)) %>% 
  melt(id.vars = c("student_id","wave")) %>%
  group_by(variable,wave) %>%
  summarize(mean = mean(as.numeric(value),na.rm = TRUE)) %>%
  filter(variable != "c4d2_7",variable != "c4d2_8")

df.plot <- merge(df.plot, saste_nashe, by.x = "variable", by.y = "multi_variable")


df.plot %>% 
  ggplot(aes(reorder(labelenglish,-mean), y = mean, fill = wave)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Source of support in case of GBV") + ylab("Share of students(%)") + labs(fill = "Wave") +
  scale_fill_manual(values = c(lat_red2,lat_red1),labels= c("Baseline","Endline")) +
  scale_y_continuous(limits = c(0,.7), breaks = seq(0,.7,0.1),expand = c(0,0), labels = scales::percent) + 
  theme_laterite() +
  theme(axis.text.x = element_text(angle= 30, hjust=0.95))

ggsave(paste0(path_images, 'bee_gbv.png'), width = 5, height = 5)
