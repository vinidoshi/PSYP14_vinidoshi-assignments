#######################################
#########Research Question 2###########
#######################################

###loading libraries and packages
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


###read data from assignment 1
home_assignment1 = read_csv("https://tinyurl.com/ha-dataset1")
home_assignment1

home_assignment1.2 <- home_assignment1[!home_assignment1$STAI_trait <= 20, ]
home_assignment1.3 <- home_assignment1.2[!home_assignment1.2$age >= 400, ]

###Create initial backward model 
#cortisol_saliva excluded  as explained in assignment brief
regression_bw <- lm( pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum +  mindfulness + weight + IQ + household_income, data = home_assignment1.3)
regression_bw

#Model diagnostics for regression_bw
#outlier identifucation through cooks distance
Cooksdistance = regression_bw %>% plot(which = 4)
levmodel = regression_bw %>% plot(which = 5)
#cooks distance criteria: >1, no cases but >4/n (=0.025), major problem cases 68,99,113. thus, decide to remove these cases from sample


#1. Normality
#using QQ plot for the model, histogram and skewness and kurtosis
regression_bw %>% plot(which=2) 

residuals_regression_bw = enframe(residuals(regression_bw))
residuals_regression_bw %>% 
  ggplot()+
  aes(x= value)+
  geom_histogram()     


ggplot(residuals_regression_bw, aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm)

#histogram seems normally distributed except for a few extreme cases
describe(residuals(regression_bw))
#both, skewness and kurtosis is within the range of -1 and +1 implying normality. 

#2.linearity
regression_bw%>%
  residualPlots()
#curves and stats show linearity is maintianed

#3.Homoscedasticity = checking for heteroscedasticity 
#visualisation
regression_bw%>% plot(which=3)

#NCV test and breush-pagan test 
regression_bw %>%ncvTest()
regression_bw %>%bptest()
#results indicate homoscedasticity

#4.Multicolinealrity 
#using vif test
regression_bw%>% vif()
#no multicolinearality detected. 

###Run backward regression on regression_bw (ie the reseacher's claim)
backwardregression= step(regression_bw, direction= "backward") #step(regression_bw, direction = "backward")
summary(backwardregression)

#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       1.951e+00  1.356e+00   1.439   0.1522    
#age              -4.175e-02  1.851e-02  -2.255   0.0256 *  
#sexmale           2.837e-01  1.868e-01   1.519   0.1309    
#pain_cat          1.070e-01  2.304e-02   4.643 7.40e-06 ***
#cortisol_serum    5.198e-01  1.015e-01   5.120 9.17e-07 ***
#mindfulness      -2.615e-01  1.168e-01  -2.238   0.0267 *  
#household_income -6.511e-06  3.837e-06  -1.697   0.0917 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 1.126 on 151 degrees of freedom
#Multiple R-squared:  0.501,	Adjusted R-squared:  0.4812 
#F-statistic: 25.27 on 6 and 151 DF,  p-value: < 2.2e-16
#based on the results, varibales age, pain, cortisol serum, mindfulness will be retained to form the final backward model

backward_model_new= lm(pain~ age+ pain_cat + cortisol_serum + mindfulness, data= home_assignment1.3)
backward_model_new
#regression equation for final backward_model_new: 𝑌 = 𝑏0 + 𝑏1 ∗ X1 + 𝑏2 ∗ X2 +...+ bn * Xn,
# y(pain)= 1.9 + (-0.04)*age + (0.11)*pain_cat + (0.49)*cortisol_serum + (-0.28)*mindfulness

confint(backward_model_new)
lm.beta(backward_model_new)
#> confint(backward_model_new)
                     #2.5 %       97.5 %
 # (Intercept)    -0.66905928  4.551050279
#age            -0.08180130 -0.008194506
#pain_cat        0.06086902  0.153051861
#cortisol_serum  0.28761337  0.686072206
#mindfulness    -0.50065887 -0.033539666
# lm.beta(backward_model_new)


#Standardized Coefficients::
 # (Intercept)            age       pain_cat cortisol_serum    mindfulness 
#0.0000000     -0.1447820      0.3432676      0.3172642     -0.1561541 

theory_based_model = lm( pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = home_assignment1.3)
theory_based_model

confint(theory_based_model)
lm.beta(theory_based_model)

#comparing both models using AIC 

AIC(backward_model_new) #[1] 496.7585
AIC(theory_based_model) #[1] 497.0552
AIC(backward_model_new)-AIC(theory_based_model) #[1] -0.2966622

anova(backward_model_new, theory_based_model)

####loading new data file (home_assignment_2)
home_assignment2 = read_csv("https://tinyurl.com/ha-dataset2")
View(home_assignment2)
summary(home_assignment2)

#comparing models on predictions
#calculate RSS 
RSS_theorybased = sum((home_assignment2$pain- predict(theory_based_model))^2)
RSS_theorybased #[1] 579.1219

RSS_backwardbased = sum((home_assignment2$pain- predict(backward_model_new))^2)
RSS_backwardbased #[1] 573.6506

summary(theory_based_model)
summary(backward_model_new) #Adjusted R-squared:  0.4682 , F-statistic: 35.55 on 4 and 153 DF,  p-value: < 2.2e-16

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


#summary tables 
summary_table3= coef_table(theory_based_model)
summary_table3

summary_table4= coef_table(backward_model_new)
summary_table4
#                   b 95%CI lb 95%CI ub Std.Beta p-value
#(Intercept)     1.94    -0.67     4.55        0    .144
#age            -0.04    -0.08    -0.01    -0.14    .017
#pain_cat        0.11     0.06     0.15     0.34   <.001
#cortisol_serum  0.49     0.29     0.69     0.32   <.001
#mindfulness    -0.27    -0.50    -0.03    -0.16    .025

#####################