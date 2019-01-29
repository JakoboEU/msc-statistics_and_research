# Date: 29/01/2019 
# Name: James Richardson
# Data: Various
# Analysis: Testing for differences Part 2: Comparing data with two categories

#
#
# Two categorical variables
# ----------------------------------------------------
# Differences between groups?
# Chi-square test
#
# Difference between sample and population mean?
# One-sample t-test (data must be normally distributed)
#
#

#########
# Set up
#########

# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/week3")
# list files in working directory
list.files() 

# Load libraries we require
library(tidyverse)
library(ggplot2)
library(MASS)

#####################
# Chi-squared test
#####################
# Chi-squared tests are used to investigate associations between categorical variables

#####################
# Data: Cats
# Analysis: chi-squared test
# Null Hypothesis: there is no significant association between training types and cat 
# defecation rates
#####################
cats<-as.table(rbind (c(28,48) , c(10,114)))

## try adding row names and column names to your table, see dimnames in help file example 
dimnames(cats) <- list(Defecate = c("Yes","No"), Freq = c("Kindness", "Hose"))
                    
#          Freq
# Defecate Kindness Hose
# Yes       28   48
# No        10  114

str(cats)

chi.cat<-chisq.test(cats) 
chi.cat

# 	Pearson's Chi-squared test with Yates' continuity correction

# data:  cats
# X-squared = 23.52, df = 1, p-value = 1.236e-06

# Is there an association between the type of training and defecation rates? How would you write this in a results statement?
# RESULTS STATEMENT: 
# ------------------
# There is a significant association between the training method and the defecation rate.
# (X-squared = 23.52, df = 1, p-value < 0.001).
# Kindness doesn't work

?chi.cat

chi.cat$expected
#           Freq
# Defecate Kindness   Hose
# Yes      14.44      61.56
# No       23.56      100.44

#####################
# Data: Eye Colour
# Analysis: chi-squared test
# Null Hypothesis: There is no significant assocation between eye colour and gender 
#####################
eye_colour <-as.table(rbind (c(89,375,25) , c(77,371,15)))
dimnames(eye_colour) <- list(Gender = c("Male","Female"), EyeColour = c("Blue", "Brown", "Other"))
eye_colour

chi.eye<-chisq.test(eye_colour) 
chi.eye
# Pearson's Chi-squared test
# data:  eye_colour
# X-squared = 2.6808, df = 2, p-value = 0.2617
chi.eye$expected

# RESULTS STATEMENT: 
# ------------------
# There is no significant association between eye colour and gender
# (X-squared = 2.6808, df = 2, p-value = 0.2617).

#####################
# Data: survey dataset
# Analysis: chi-squared test
# Null Hypothesis: There is no significant association between frequency of smoking and frequency of excercise
#####################

summary(survey)
head(survey)

smoking<-table(survey$Smoke,survey$Exer) # table counts each specified category to build a contingency table
smoking
#       Freq None Some
# Heavy    7    1    3
# Never   87   18   84
# Occas   12    3    4
# Regul    9    1    7

chi.smoking <- chisq.test(smoking) 
chi.smoking
# Pearson's Chi-squared test
# 
# data:  smoking
# X-squared = 5.4885, df = 6, p-value = 0.4828
# 
# Warning message:
# In chisq.test(smoking) : Chi-squared approximation may be incorrect

chi.smoking$expected

#         Freq      None      Some
# Heavy  5.360169  1.072034  4.567797
# Never 92.097458 18.419492 78.483051
# Occas  9.258475  1.851695  7.889831
# Regul  8.283898  1.656780  7.059322

# RESULTS STATEMENT: 
# ------------------
# There is no significant association between frequency of smoking and frequency of excercise
# (X-squared = 5.4885, df = 6, p-value = 0.4828)

# The issue is that the chi-square approximation to the distribution of the test statistic relies on the counts being 
# roughly normally distributed. 
# If many of the expected counts are very small, the approximation may be poor.

######################
# Fisher’s exact test
######################
# Fisher’s exact test is used to investigate the associations between two categorical variables, 
# but where expected cell counts are less than 5.

# How to report?
# How to report the results of a Fisher's exact test is pretty much the same as the Chi-square test. 
# Unlike Chi-square test, you don't have any statistics like chi-squared. So, you just need to report the p value. 
# Some people include the odd ratio with the confidence intervals. 

# Is there a difference in the number of convictions between identical and non- identical twins?
#####################
# Data: convictions
# Analysis: fisher's exact test
# Null Hypothesis: There is no signicant association between the number of convictions and the type of twin
#####################

convictions <-as.table(rbind (c(2,15) , c(10,3)))
dimnames(convictions) <- list(Twin = c("Not Identical","Identical"), Conviction = c("Convicted", "Not Convicted"))

# Fisher’s exact test 
fisher.test(convictions)

# Fisher's Exact Test for Count Data
# 
# data:  convictions
# p-value = 0.0005367
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
# 0.003325764 0.363182271
# sample estimates:
# odds ratio 
# 0.04693661 

# RESULTS STATEMENT: 
# ------------------
# There is a signicant association between the number of convictions and the type of twin
# (p < 0.001, 95% confidence interval 0.0033 0.3632, odds ratio 0.0469

##################
# Two-way ANOVA
##################
# Last week we carried out a One-way ANOVA. 
# This is just a small part of the ANOVA family, which are good ways to consider organising experiments and data collection.

# Two-way ANOVAs are used when we have a continuous, response variable measured across levels of two different categorical factors. 
# Factors would classically be manipulated factors consisting of different levels (e.g., different quantities of a drug). 
# The number of subjects measured would be the same for each level of the factor by design.

#  For a 2-way ANOVA, the data need to be arranged into three columns as in this data set. 
# If your categorical data were numbers these would first need to be converted to factors.

moths <- read_csv('moths.csv')
# Parsed with column specification:
#  cols(
#    Location = col_character(),
#    moth_number = col_double()
#    Bait = col_character(),
#  )
summary(moths)
head(moths)

ggplot(moths, aes(Location, moth_number)) + 
  geom_boxplot()  +
  ylab("Moth Couth") 

ggplot(moths, aes(Bait, moth_number)) + 
  geom_boxplot()  +
  ylab("Moth Couth") 

ggplot(moths, aes(Location, moth_number, fill=Bait)) + 
  geom_boxplot()  +
  ylab("Moth Couth") 

# Interaction plot
###
# Now we can make an interaction plot. There may be an interaction if all levels of bait don't respond in the same way across tree locations
interaction.plot(moths$Location, moths$Bait, moths$moth_number)
# – do you think there could be one?

###
# Test the assumptions of a two-way ANOVA
# Like with other ANOVA models, here we must make a few assumptions about our data: 
# * normal distribution, 
# * equal variances, 
# * and the observations are independent of one another.
###

# Test for normality
###
shapiro.test(moths$moth_number)
# Shapiro-Wilk normality test
# 
# data:  moths$moth_number
# W = 0.94533, p-value = 0.009448

# p < 0.001 NOT significantly different from normal

# Test for homogeneity of variances
###
bartlett.test(moths$moth_number, moths$Location, moths$Bait)

# Bartlett test of homogeneity of variances
#
# data:  moths$moth_number and moths$Location
# Bartlett's K-squared = 5.498, df = 3, p-value = 0.1388

# Significant difference in variance

## Model with the main effects of both factors and no interaction 
output1<-aov(moth_number~Location+Bait, data=moths) 
summary(output1)
#                Df Sum Sq Mean Sq F value   Pr(>F)    
# Location       3   1981   660.5  11.327 7.17e-06 ***
#   Bait         2    113    56.5   0.969    0.386    
# Residuals     54   3149    58.3 

# So tree location is significant but bait type is not.
#######

# It could be argued to be reasonable to stop here and not test the interaction effect. 
# Conceptually for there to be a significant interaction effect, 
# both of the main effects involved would need to be significant for the interaction term to be significant. 
# OR you should test for an interaction term if you have an a priori hypothesis specific to the interaction term. 
# But we will test it now anyway.

## Model with the main effects of both factors AND interaction 
output2<-aov(moth_number~Location*Bait, data=moths) 
summary(output2)
#                  Df Sum Sq Mean Sq F value   Pr(>F)    
# Location         3   1981   660.5  10.450 2.09e-05 ***
#   Bait           2    113    56.5   0.894    0.416    
# Location:Bait    6    115    19.2   0.303    0.932    
# Residuals       48   3034    63.2  


## Compare the two models 
anova(output1, output2)
# Analysis of Variance Table
# 
# Model 1: moth_number ~ Location + Bait
# Model 2: moth_number ~ Location * Bait
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     54 3148.6                           
# 2     48 3033.6  6    114.97 0.3032 0.9322

# The models aren't different. By default some people would drop the interaction term and accept the simplest model.
### Because: Pr(>F) = 0.9322


# RESULTS STATEMENT: 
# ------------------
# There is no significant association between the bait used in the trap and the number of moths caught
# p = 0.386, df = 2, mean = 56.5 
# There is a signifcant association between the level of the trap and the number of moths caught
# p < 0.001, df = 3, mean = 660.5
# There is no significant interaction between the bait and the level of the trap
# p = 0.932, df = 6, mean = 19.2

############################################
# Repeated measures ANOVA & interaction plots
############################################
# We often want to investigate if something has changed over time – particularly in Animal Behaviour
# – and we usually do this by comparing observations before and after a treatment. 
# But (as in paired t- tests), we cannot just compare before and after observations as if they were independent, 
# because they aren’t. An individual’s response will depend on their prior experience, genetics, personality etc., 
# and how they respond over time will depend on the same things. So we need to use repeated measures ANOVAs.

exer <- read_csv('exer.csv')
# Parsed with column specification:
#   cols(
#     id = col_double(),
#     diet = col_double(),
#     exertype = col_double(),
#     pulse = col_double(),
#     time = col_double()
#   )
summary(exer)
exer$id <- as.factor(exer$id)
exer$diet <- as.factor(exer$diet)
exer$time <- as.factor(exer$time)
exer$exertype <- as.factor(exer$exertype)
summary(exer)
# Diet 1 = low fat, Diet 2 = Not low fat; 
# Exercise 3 = running, Exercise 2 = walking, Exercise 1 = rest.

interaction.plot(exer$time, exer$diet, exer$pulse)

## Repeated measures anova with subject id as the error i.e. the thing that gets repeatedly 
# measured!
diet.aov <- aov(pulse ~ diet * time + Error(id), data = exer)
summary(diet.aov)

# Error: id
#           Df Sum Sq Mean Sq F value Pr(>F)  
# diet       1   1262    1262   3.147 0.0869 .
# Residuals 28  11227     401                 
# 
# Error: Within
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# time         2   2067  1033.3  11.808 5.26e-05 ***
#   diet:time  2    193    96.4   1.102    0.339    
# Residuals   56   4901    87.5    
  
# From the output, we can see that effect of time is significant but the interaction of time and diet is not significant. 
# The between subject test of the effect of diet (top bit) is also not significant. 
# Does this fit with what you thought from the interaction plot? 
# In the interaction plot we have lines that are not flat, in fact, they are actually increasing over time, 
# which was expected since the effect of time was significant. 
# However, the lines are approximately parallel which was anticipated since the interaction was not significant.

# Now try the same thing with time and exercise. What can you conclude?
interaction.plot(exer$time, exer$exertype, exer$pulse)

exertype.aov <- aov(pulse ~ exertype * time + Error(id), data = exer)
summary(exertype.aov)
# Error: id
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# exertype     2   8326    4163      27 3.62e-07 ***
#   Residuals 27   4163     154                     
# 
# Error: Within
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
# time             2   2067  1033.3   23.54 4.45e-08 ***
#   exertype:time  4   2723   680.8   15.51 1.65e-08 ***
#   Residuals     54   2370    43.9  


# But this doesn’t answer all our questions as may be there is an interaction between exercise and diet. 
# So now build a model with diet, time and exercise, 
# i.e. we would also like to know if the people on the low-fat diet who engage in running have lower pulse rates than the 
# people participating in the not low-fat diet who are not running
# 
# First, split the data by diet (so diet 1 and diet 2), 
# and then plot interaction plots of time with exercise for these separately (there are multiple ways of doing this, 
# below is just one approach), 
# I’ve given you the code below as this is a little tricky.

# exercise, diet and time
attach(exer)
par(mfrow=c(1,2))
interaction.plot(time[diet==1], exertype[diet==1], pulse[diet==1],
                 ylim = c(80, 150), lty = c(1, 12, 8),
                 trace.label = "exertype", ylab = "mean of pulse", xlab = "time") 
title("Diet = 1")
interaction.plot(time[diet==2], exertype[diet==2], pulse[diet==2], ylim = c(80, 150), lty = c(1, 12, 8),
                 trace.label = "exertype", ylab = "mean of pulse", xlab = "time")
title("Diet = 2")

# Now what can you conclude from this?

# You could follow this up with another Repeated Measures Anova that includes a three way interaction, 
# i.e. to test if pulse rate will depend on which diet you follow, the exercise type you engage in 
# and at what time during the exercise that you measure the pulse. 
# This does get pretty complicated to interpret; we will not be testing 3-way Repeated Measures Anova in the quiz.

all.aov <- aov(??????)
summary(all.aov)
