# -----------------------------------------------------------------------------
# Title: Summer 2019 Data Assessment
# Author: Jacqueline Woo, jzw2108@columbia.edu
# Date: 1/23/2019
# R Version: 3.6.3
# RStudio Version: 1.2.5033
# -----------------------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(ggplot2)

#set wd
setwd("C:/Users/jacqw/Documents/Columbia/Internships-Jobs/")
##load("geodistance.RData")

#read data
Rwanda <- read.csv("laterite_education_data.csv",header=T,na.strings=c("","na","NA","N/A"))

#rename variables
colnames(Rwanda) <- c("x","province","district","urban_rural","sampleweight","agg_cons","sex","age","father_alive","mother_alive","health_prob","grade2012","grade2013","sch_type","sch_prob","ed_exp","who_paid","days_missed","reason_noattend","reason_leftsch","read","write","calc","hhfarm")
top <- head(Rwanda,10)

#grade repetition and dropout
###checking that format of grades is the same for both variables
grade2012_unique <- Rwanda %>% select(grade2012) %>% unique(.)
grade2013_unique <- Rwanda %>% select(grade2013) %>% unique(.)
###Investigating NAs 
blank <- Rwanda %>% filter(is.na(grade2012)|is.na(grade2013)) ##drop because we cannot conclude if students were in school and no data was collected, or if they were not in school (10% of obs)
Rwanda_clean <- Rwanda[!is.na(Rwanda$grade2012),] ##can just use grade2012 because when data is unavailable, it's unavailable for both variables of interest
###Assume that post primary is the same as secondary. Convert all post primary to secondary
Rwanda_clean$grade2012 <- gsub("Post primary","Secondary", Rwanda_clean$grade2012)
Rwanda_clean$grade2013 <- gsub("Post primary","Secondary", Rwanda_clean$grade2013)
###create grade repetition variable
Rwanda_clean$repeated <- ifelse(Rwanda_clean$grade2012=="Not in class" & Rwanda_clean$grade2013=="Not in class",NA,
                                ifelse(as.character(Rwanda_clean$grade2012)==as.character(Rwanda_clean$grade2013),1,0))
###create grade dropout variable
Rwanda_clean$sec <- ifelse(substring(as.character(Rwanda_clean$grade2012),1,9)=="Secondary",1,0)
Rwanda_clean$dropout <- ifelse(Rwanda_clean$grade2012=="Not in class" & Rwanda_clean$grade2013=="Not in class",NA,
                                ifelse(Rwanda_clean$sec==1,0,
                                       ifelse(Rwanda_clean$grade2012 != "Not in class" & Rwanda_clean$grade2013=="Not in class",1,0)))
###Find all primary students, then filter by all primary
Rwanda_clean$primary <- ifelse(substring(as.character(Rwanda_clean$grade2012),1,7)=="Primary",1,0)
Rwanda_Primary <- Rwanda_clean %>% filter(primary==1)
#2,260 primary students
###Overall rate of dropout and grade repetition in Primary Education
overall_dropout <- sum(Rwanda_Primary$dropout)
overall_repeated <- sum(Rwanda_Primary$repeated)

#How does grade repetition vary by grade?
grade_rep <- Rwanda_Primary %>% 
  select(grade2012,repeated) %>%
  group_by(grade2012) %>%
  summarise(rep_rate=mean(repeated),pop=n())%>%
  mutate(rep_round=round(rep_rate,digits=3))
p <- ggplot(grade_rep) +
  geom_bar(aes(x=grade2012,y=pop),stat="identity",fill="steelblue")+
  geom_line(aes(x=grade2012,y=rep_round*max(grade_rep$pop),group=1),color="black")+
  geom_text(aes(label=rep_round, x=grade2012, y=rep_round*max(grade_rep$pop)),vjust=-0.3,size=3.5)+
  geom_text(aes(label=pop, x=grade2012, y=pop),vjust=-0.3,size=3.5)+
  scale_y_continuous("School Population",sec.axis = sec_axis(~./max(grade_rep$pop),name="Repetition Rate"))+
  labs(title="School Population and Repetition Rate by Grade",x="Grade in 2012",y="Population")+
  theme(plot.title = element_text(hjust = 0.5))
p

#Overall dropout rates by gender
gender_drop <- Rwanda_Primary %>% 
  select(sex,dropout) %>%
  group_by(sex) %>%
  summarise(dropout=sum(dropout),pop=n())%>%
  mutate(dropout_rate=dropout/pop)
res <- prop.test(x=c(58,59),n=c(1159,1101))
###p-value=0.77; no statistically significant difference in dropout rates by gender

#Variables and Predictors for Grade Repetition
###potential variables using scatterplots:agg_cons, sex, age, health_prob,sch_type,days_missed,read, write, calc,hhold farm
###transform into dummies if needed
Rwanda_Primary$sex_dummy <- ifelse(Rwanda_Primary$sex=="Male",1,0) #Male = 1, Female = 0
health_prob <- unique(Rwanda_Primary$health_prob)
Rwanda_Primary$health_dummy <-ifelse(Rwanda_Primary$health_prob=="Yes",1,0) #Yes = 1, No = 0                                
sch_type <- unique(Rwanda_Primary$sch_type)
unlist(sch_type)
Rwanda_Primary$schtype_dummy <- ifelse(Rwanda_Primary$sch_type=="Free Subsidized",1,0) #free/subsidized=0, public/private=1
Rwanda_Primary$read_dummy <- ifelse(Rwanda_Primary$read=="Yes",1,0) #Read=1, No=0
Rwanda_Primary$write_dummy <- ifelse(Rwanda_Primary$write=="Yes",1,0) #Read=1, No=0
Rwanda_Primary$calc_dummy <- ifelse(Rwanda_Primary$calc=="Yes",1,0) #Write=1, No=0
Rwanda_Primary$farm_dummy <- ifelse(Rwanda_Primary$hhfarm=="Yes",1,0) #Work on a farm=1, No=0
Rwanda_Reg <- Rwanda_Primary %>% select(repeated,agg_cons,sex_dummy,age,health_dummy,schtype_dummy,days_missed,read_dummy,write_dummy,calc_dummy,farm_dummy) ##2260 obs
###Remove all variables with NA
Rwanda_Reg_all <- na.omit(Rwanda_Reg) ###489
###lpm with all variables
repeat_all <- lm(repeated~agg_cons+sex_dummy+age+health_dummy+schtype_dummy+days_missed+read_dummy+write_dummy+calc_dummy+farm_dummy,data=Rwanda_Reg_all)
summary(repeat_all) ###read_dummy multicollinearity? None of the variables besides write_dummy are significant
###lpm excluding read, write, calc to increase n
repeat_excl <- lm(repeated~agg_cons+sex_dummy+age+health_dummy+schtype_dummy+days_missed+farm_dummy,data=Rwanda_Reg)
summary(repeat_excl) ##Age and sex seem to be the most significant causes of repeating grades

#save data
save(list=ls(all = TRUE),file="rwanda.RData")
