#####################################
######Research question 1############
#####################################

###Step 1: load and install relevant packages
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

###Step 2: load and view data
home_assignment1 = read_csv("https://tinyurl.com/ha-dataset1")
view(home_assignment1)

###Step 3: check data and make data corrections. Listing the min and max values for all variables to identify any missing values or coding errors. 
summary(home_assignment1)
describe(home_assignment1)
str(home_assignment1)

#certain irrelgularities in the dataset are observed. The max value under col(age) is 444- this does not make sense and hence the participant details will be removed. the min value possible on STAI-T is 20, however, in the summary, the min present value is 3.90- this input will also be delted. The min income can be seen as -3732. This could either be a coding error or indicative of debt. Since there is no clairty there, the input will be retained. 

home_assignment1.2 <- home_assignment1[!home_assignment1$STAI_trait <= 20, ]
home_assignment1.3 <- home_assignment1.2[!home_assignment1.2$age >= 400, ]
summary(home_assignment1.3)
#total of 2 rows were deleted and thus final number of observations are 158. 

###Step 4: building models
#Will first create both models followed by model diagnostics for each in next step

pain_mod1 <- lm( pain ~ age + sex, data = home_assignment1.3)
pain_mod1

pain_mod2 <- lm( pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = home_assignment1.3)
pain_mod2

#predictor visualisations for each model
mod1_plot1 = home_assignment1.3 %>% ggplot() + aes(x = age, y = pain) + geom_point() + geom_smooth(method = "lm", se= FALSE)
mod1_plot1

mod1_plot2 = home_assignment1.3 %>% ggplot() + aes(x = sex, y = pain) + geom_violin()+ geom_jitter()
mod1_plot2

mod1_plot3 = home_assignment1.3%>%
  ggplot()+
  aes(y = pain, 
      x = age,
      colour = sex)+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)
mod1_plot3

#relation between pain and stai-t 
mod2_plot1 = home_assignment1.3 %>% ggplot()+ aes(x = STAI_trait, y= pain)+ geom_point()+ geom_smooth(method = "lm", se = FALSE)
mod2_plot1

#pain and pain_cat
mod2_plot2 = home_assignment1.3 %>% ggplot()+ aes(x = pain_cat, y= pain)+ geom_point()+ geom_smooth(method = "lm", se = FALSE)
mod2_plot2

#pain and mindfulness
mod2_plot3 = home_assignment1.3 %>% ggplot()+ aes(x = mindfulness, y= pain)+ geom_point()+ geom_smooth(method = "lm", se = FALSE)
mod2_plot3

#pain and cortisol serum 
mod2_plot4 = home_assignment1.3 %>% ggplot()+ aes(x = cortisol_serum, y= pain)+ geom_point()+ geom_smooth(method = "lm", se = FALSE)
mod2_plot4

#pain and cortisol saliva
mod2_plot5 = home_assignment1.3 %>% ggplot()+ aes(x = cortisol_saliva, y= pain)+ geom_point()+ geom_smooth(method = "lm", se = FALSE)
mod2_plot5

grid.arrange(mod1_plot1, mod1_plot2, nrow = 1)

mod2_plotcompare = grid.arrange(mod2_plot1,mod2_plot2, mod2_plot3, mod2_plot4, mod2_plot5, nrow = 3)


###Step 5: Model diagnostics
#diagnostics for model 2

#Outlier identification 
#outlier identifucation through cooks distance
Cooksdistance = pain_mod2 %>% plot(which = 4)
levmodel = pain_mod2 %>% plot(which = 5)
#cooks distance criteria: >1, no cases but >4/n (=0.025), major problem cases 68,99,113. 

#1. Normality
#using QQ plot for the model, histogram and skewness and kurtosis
pain_mod2 %>% plot(which=2) #qqplot. 113, 26, 147 seen as deviating from the theoretical diagnol

residuals_pain_mod2 = enframe(residuals(pain_mod2))
residuals_pain_mod2 %>% 
  ggplot()+
  aes(x= value)+
  geom_histogram()     


ggplot(residuals_pain_mod2, aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm)

#histogram seems normally distributed except for a few extreme cases
describe(residuals(pain_mod2))
#both, skewness and kurtosis is within the range of -1 and +1 implying normality. Skew= -0.12, kurtosis= -0.42

#2.linearity
pain_mod2%>%
  residualPlots()
#curves and stats show linearity is maintianed

#3.Homoscedasticity = checking for heteroscedasticity 
#visualisation
pain_mod2%>% plot(which=3)

#NCV test and breush-pagan test 
pain_mod2%>%ncvTest()
pain_mod2%>%bptest()
#results indicate homoscedasticity. NCV p>0.05, bp>0.05

#4.Multicolinealrity 
#using vif test
pain_mod2%>% vif()
#cortisol serum and saliva values are extremely high (>3). COrt_serum = 6.58, cort_sal= 7.47. Theoretically, cortisol serum has higher correlation with stress levels than cortisol saliva, thus a new model without cortisol saliva levels will be created and cortisol serum data will be retained. 
pain_mod2_serum = lm( pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = home_assignment1.3)
pain_mod2_serum

#conducting diagnostics for the new model2 to ensure all assumptions are met
#ouliers
Cooksdistance = pain_mod2_serum %>% plot(which = 4)
levmodel = pain_mod2_serum %>% plot(which = 5)

#1.Normality
#using QQ plot for the model, histogram and skewness and kurtosis
pain_mod2_serum %>% plot(which=2) #qqplot. 113, 26, 147 seen as deviating from the theoretical diagnol

residuals_pain_mod2_serum = enframe(residuals(pain_mod2_serum))
residuals_pain_mod2_serum %>% 
  ggplot()+
  aes(x= value)+
  geom_histogram()     


ggplot(residuals_pain_mod2_serum, aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm)

#histogram seems normally distributed except for a few extreme cases
describe(residuals(pain_mod2_serum))
#both, skewness and kurtosis is within the range of -1 and +1 implying normality. skew = - 0.11, kurt= -0.38

#2.linearity
pain_mod2_serum%>%
  residualPlots()
#curves and stats show linearity is maintianed.                Test stat Pr(>|Test stat|)
#age              -0.0713           0.9432
#STAI_trait       -0.4209           0.6744
#pain_cat          0.5385           0.5911
#cortisol_serum   -0.3676           0.7137
#mindfulness      -0.9205           0.3588
#Tukey test       -0.0325           0.9741

#3.Homoscedasticity = checking for heteroscedasticity 
#visualisation
pain_mod2_serum%>% plot(which=3)

#NCV test and breush-pagan test 
pain_mod2_serum%>%ncvTest() #Chisquare = 2.628848, Df = 1, p = 0.10494
pain_mod2_serum%>%bptest() # BP = 5.3211, df = 6, p-value = 0.5033
#results indicate homoscedasticity


#Diagnostics for model 1
#Outliers
Cooksdistance = pain_mod1 %>% plot(which = 4)
levmodel = pain_mod1%>% plot(which = 5)
#127, 99,140 can have an inlfuence on the model

#1.Normality
#using QQ plot for the model, histogram and skewness and kurtosis
pain_mod1 %>% plot(which=2) #qqplot. 127, 99, 43 are labelled as deviating away from the theoretical diagnal. 
#even though, outlier detection and qq plot have listed two IDs, the decision to maintain them in the dataset is made since they do not seem to have a very high effect as seen from the qq plot.


residuals_pain_mod1= enframe(residuals(pain_mod1))
residuals_pain_mod1 %>% 
  ggplot()+
  aes(x= value)+
  geom_histogram()     


ggplot(residuals_pain_mod1, aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm)

#histogram seems normally distributed except for a few extreme cases
describe(residuals(pain_mod1))
#both, skewness and kurtosis is within the range of -1 and +1 implying normality. skew =.18, kurt=-0.04

#2.linearity
pain_mod1%>%
  residualPlots()
#curves and stats show linearity is maintianed

#3.Homoscedasticity = checking for heteroscedasticity 
#visualisation
pain_mod1%>% plot(which=3)

#NCV test and breush-pagan test 
pain_mod1%>%ncvTest() #Chisquare = 0.9637738, Df = 1, p = 0.32624
pain_mod1%>%bptest() #BP = 1.9748, df = 2, p-value = 0.3725 
#results indicate homoscedasticity

#final models are:
#pain_mod1= lm( pain ~ age + sex, data = home_assignment1.3)
#pain_mod2_serum = lm( pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = home_assignment1.3)


###Step 6: Model Comparison 
pain_mod1
pain_mod2_serum

#regression equation for final model 2 (pain_mod2_serum): 𝑌 = 𝑏0 + 𝑏1 ∗ X1 + 𝑏2 ∗ X2 +...+ bn * Xn,
# y(pain)= 1.8 + (-0.04)*age + (0.27)*sex + (-0.02)*STAI_trait + (0.11)*pain_cat + (0.56)*cortisol_serum + (-0.28)*mindfulness

#adjusted R^2 comparison
summary(pain_mod1)$adj.r.squared #[1] 0.05270514
summary(pain_mod2_serum)$adj.r.squared #[1] 0.4736095

#values indicate that model2 is a better fit for predicting pain

#compare significance of model difference using ANOVA and AIC. 
anova(pain_mod1, pain_mod2_serum) #since model 2 is nested in model 1, can use anova  #model2 anova significance = 151 194.29  4    164.62 31.985 < 2.2e-16 ***

AIC(pain_mod1) #[1] 586.0217
AIC(pain_mod2_serum) #[1] 497.0552

AIC(pain_mod1)-AIC(pain_mod2_serum) #[1] 88.96656 

#difference between model AIC and Anova F value indicates that the models are significantly different from each other with model 2 (pain_mod2_serum)providing significanlty better prediction of pain than model 1 (pain_mod1). 

summary(pain_mod1) #r2= 0.053, f= 5.37, df= 2 and 155 df, p = 0.005
summary(pain_mod2_serum) # Adjusted R-squared:  0.4736 , F-statistic: 24.54 on 6 and 151 DF,  p-value: < 2.2e-16

#confidence intervals for both 
confint(pain_mod1)
lm.beta(pain_mod1)

confint(pain_mod2_serum)
lm.beta(pain_mod2_serum)

#run customcodes for final coefficients table. 
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}

summary_table1 = coef_table(pain_mod1)
summary_table2= coef_table(pain_mod2_serum)

summary_table1
summary_table2

#################END 
