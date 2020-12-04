################################
########Research Question 3#####
################################


#loading packages and libraries 
library(tidyverse)
library(psych)
library(lm.beta)
library(dplyr)
install.packages("gridExtra")
library(gridExtra)
install.packages("car")
library(car)
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)
library(boot)
install.packages("lmboot")
library(lmboot) 
install.packages("cAIC4")
library(cAIC4) # for cAIC		
install.packages("r2glmm")
library(r2glmm) # for r2beta		
install.packages("lme4")
library(lme4) # for lmer	
install.packages("lmerTest")
library(lmerTest) # to get singificance test in lmer	
install.packages("MuMIn")
library(MuMIn) # for r.squaredGLMM	
install.packages("optimx")
library(optimx) # for optimx optimizer

#load data
home_assignment3 = read_csv("https://tinyurl.com/ha-dataset3")
summary(home_assignment3)
View(home_assignment3)

#no extremes in sccoring found, checking for coding errors
home_assignment3%>%
  ggplot()+
  aes(x= sex)+
  geom_bar()
#codign error, replace "femlae" with female

home_assignment3 = home_assignment3%>%
  mutate(sex = replace(sex, sex == "femlae", "female"))

home_assignment3%>%
  ggplot()+
  aes(x= sex)+
  geom_bar()

############ PART 1 ######### 
###Create linear mixed model with random intercept - hospital
home_assignment3%>%
  mutate(hospital = factor(hospital))

lm_random_hospital = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data= home_assignment3 , REML = "FALSE")
lm_random_hospital

confint(lm_random_hospital)
summary(lm_random_hospital)

##rerun model 1 on dataset 3 to get coefficents and confidence intervals

pain_mod2_serum = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data= home_assignment3)
pain_mod2_serum

confint(pain_mod2_serum)
summary(pain_mod2_serum)


######## PART 2 ################

#computing variance for fixed effect predictors (dataset3)
r2beta(lm_random_hospital, method = "nsj" , data= home_assignment3) #marginal r^2

r.squaredGLMM(lm_random_hospital) #marginal r^2 is 0.37 , conditional r^2 is 0.49 

cAIC(lm_random_hospital)$caic


####Load datafile 4 for predicting pain
home_assignment4 = read_csv("https://tinyurl.com/ha-dataset4")
summary(home_assignment4)
View(home_assignment4)

#mindfulness value of more than 6 observed, will be deleted. 

home_assignment4.1 = home_assignment4[!home_assignment4$mindfulness >= 6, ]
summary(home_assignment4.1)
View(home_assignment4.1)

#create hospital as factor
home_assignment4.1%>%
  mutate(hospital = factor(hospital))

#predict pain on dataset4 using lm_random_hospital.
random_hospital_predict = predict(lm_random_hospital, home_assignment4.1, allow.new.level = TRUE)
random_hospital_predict

RSS_lm= sum((home_assignment4.1$pain- predict(lm_random_hospital, home_assignment4.1, allow.new.level = TRUE))^2)
RSS_lm

mean_lm_random_hospital= lmer(pain ~ 1 + (1|hospital), data= home_assignment4.1)
mean_lm_random_hospital

TSS_lm= sum((home_assignment4.1$pain - predict(mean_lm_random_hospital, home_assignment4.1, allow.new.level = TRUE))^2)
TSS_lm

r2 = 1- (RSS_lm/TSS_lm)
r2
#r^2 on dataset 4 is 0.049



####### PART 3 ##########
####creating new model with random slope and random intercept
summary(lm_random_hospital)
confint(lm_random_hospital)

##cortisol serum most significant. thus only cortisol serum will be included in the new model. 

mixed_model_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data= home_assignment3)
mixed_model_slope

##creating graph for ech hosp separeately 
home_assignment3 = home_assignment3 %>% 		
  mutate(pred_slope = predict(mixed_model_slope))

home_assignment3%>%
  ggplot()+
  aes(x= cortisol_serum , y= pain, group= hospital)+
  geom_point(aes(color= hospital), size= 4)+
  geom_line(color = "red" , aes(x= cortisol_serum, y= pred_slope))+
  facet_wrap( ~ hospital, ncol = 5 )


############################################################ 
