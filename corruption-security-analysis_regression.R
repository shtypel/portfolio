# loading libraries 
library(haven)
library(questionr)
library(sjmisc)
library(sjlabelled)
library(sjPlot)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(PerformanceAnalytics)
library(psych)


# Setting the working directory
setwd("/Users/arishashtypel/Documents/Year 2/PSM2/portfolio")

# getting the data
hongkong <- read_dta("Hong_Kong_WVS.dta")
# getting names of the variables
names(hongkong)
# removing missing values
na.omit(hongkong)
# attaching the dataset to not call it every time
attach(hongkong)

# RESEARCH QUESTION 2 ---------------------------------------------------------
#  Is there an association between the perception of security
# (i.e. did not carry much money due to security concerns)
# and subjective corruption in the country?

# Q139P - did not carry much cash around (yes/no) OUTCOME
# Q112 - perception of corruption (1-10) EXPLANATORY

# creating frequency tables for answers for corruption and security 
frq(Q139P, out="v")
frq(Q112, out="v")

# Fitting binary logistic regression
rq2 <- glm(Q139P ~ Q112, family = "binomial")
summary(rq2)
# extracting the coefficients and CI and
# exponentiating them to get odds ratios and their CI
exp(cbind(coef(rq2), confint(rq2)))

# RESEARCH QUESTION 3 ---------------------------------------------------------
# Is there an association between demographic characteristics (explanatory variables)
# and people’s subjective corruption and perception of security (outcome variables)?

# demographic variables: sex, age, immigrant/not
# Q260 - sex (male/ female)
# Q262 - age (number)
# Q263 - immigrant (yes, no)

# creating frequency tables for answers for demographic variables 
frq(Q260, out="v")
frq(Q262, out="v")
frq(Q263, out="v")

# Fitting multiple linear regression with perception of corruption as outcome var
# and demographic as explanatory
rq3_cor <- lm(Q112~Q260+Q262+Q263)
summary(rq3_cor)
# getting confidence intervals
confint(rq3_cor)

# Fitting a binary logistic regression with perception of security as outcome var
# and demographic as explanatory
rq3_sec <- glm(Q139P~Q260+Q262+Q263, family=binomial)
summary(rq3_sec)
# getting confidence intervals
exp(cbind(coef(rq3_sec), confint(rq3_sec)))

# RESEARCH QUESTION 4 ---------------------------------------------------------
# Using all demographic variables as in (3), is the association between 
# the perceived security and subjective corruption dependent on an individual’s 
# confidence in the police?
# Q69P - individual’s confidence in the police?

# Considering whether there is an interaction between perception of security and 
# confidence in the police using moderation analysis
# Defining an interaction between two variables by using the "*" symbol
rq4 <- lm(Q112~Q139P*Q69P+Q260+Q262+Q263)
summary(rq4)
confint(rq4)

# RESEARCH QUESTION 5 ---------------------------------------------------------

# fitting a simple linear regression
# outcome: Q69P confidence in the police 
# explanatory: Q262 age 
rq5 <- lm(Q69P~Q262)
summary(rq5)
confint(rq5)


# COMPARING MODELS ---------------------------------------------------------
# to juxtapose all models 
library(texreg)
screenreg(list(rq2,rq3_cor,rq3_sec, rq4, rq5),digits=3, ci.force=TRUE)


