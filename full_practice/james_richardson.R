# Date: 20/02/2019 
# Name: James Richardson
# Student Number: 18057447

# Script Set Up
# -------------
# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/full_practice")
# list files in working directory
list.files() 

# Load required libaries
library(tidyverse)
library(ggplot2)

# In questions where you are asked to analyse data, you must follow the analysis pathways set out in the lectures, e.g.:
#   1. Write clear hypotheses
#   2. Explore descriptive statistics – means, standard deviation, distribution etc
#   3. Create appropriate plots to visualise/explore data
#   4. Decide on test(s) to be done (see flow chart)
#   5. Examine assumptions of tests: normality, variances, independence
#   6. Carry out tests
#   7. Report your results: results statements / plots

#################
# Section 1
#################
# The dataset InsectSprays contains data on insect density in areas treated with different kinds of pesticide spray. 
# Load the data into R. Investigate if the different kinds of spray are effective in reducing insect density
# – i.e. is there a difference in insect density between sprays.
#
# Marking guidance
# * 1 mark for the hypothesis; 
# * 2 marks for preliminary data exploration and visualisation, 
# * 3 marks for testing the assumptions and reporting, 
# * 2 marks for selection and carrying out correct test, 
# * 2 marks reporting your results and presenting the data.

insectSprays <- read_csv("InsectSprays.csv")

# Parsed with column specification:
#  cols(
#    count = col_double(),
#    spray = col_character()
#  )

head(insectSprays)

# Null Hypothesis:
# ----------------
# None of the different sprays have a significantly different number of insects

str(insectSprays)

v

# Means
tapply(insectSprays$count, insectSprays$spray, FUN=mean)
#     A         B         C         D         E         F 
# 14.500000 15.333333  2.083333  4.916667  3.500000 16.666667 

# SDs
tapply(insectSprays$count, insectSprays$spray, FUN=sd)
#     A        B        C        D        E        F 
# 4.719399 4.271115 1.975225 2.503028 1.732051 6.213378 

# Choose Tests:
# -------------
# Comparing one categorical with one continuous variable
# 3+ sprays
# If normal use ANOVA otherwise use Kruskal-Wallis

# Test for Normality
shapiro.test(insectSprays$count) 
# W = 0.9216, p-value = 0.0002525
# Significantly different to normal

# Test for homogeneity of variances
bartlett.test(insectSprays$count, insectSprays$spray)
# Bartlett's K-squared = 25.96, df = 5, p-value = 9.085e-05
# <- Significant difference in variance

# Use Kruskal-Wallis
kruskal.test(spray~count, data = insectSprays) 
# Kruskal-Wallis chi-squared = 43.357, df = 23, p-value = 0.006281

# So different sprays do have a significantly different number of insects,
# But which ones?
pairwise.wilcox.test(insectSprays$count,insectSprays$spray)
#       A       B       C       D       E      
#  B 1.00000 -       -       -       -      
#  C 0.00051 0.00051 -       -       -      
#  D 0.00062 0.00062 0.01591 -       -      
#  E 0.00051 0.00051 0.26287 0.69778 -      
#  F 1.00000 1.00000 0.00051 0.00062 0.00051
#
#  P value adjustment method: holm 

# So it looks like A, B and F are significantly differnet to C, D, E


# Results Statement:
# ------------------
# The number of insects was significantly different between sprays.
# A, B and F were all similar and significantly differnet to C, D and E.
# (Chi=43.36; df= 23; p = 0.006).

ggplot(insectSprays, aes(x=spray, y=count)) + 
  geom_boxplot() +
  xlab("Spray") + ylab("Number of Insects")

#################
# Section 2
#################
# The dataset Raccoon has three variables: weight (lb), sex (male and female) 
# and population location (N, S, E, W) for a number of animals. 
# Load the dataset into R and investigate how weight differs with sex and/or population location.
# Marking guidance
# * 1 mark for the hypothesis; 
# * 1 marks for preliminary data exploration and visualisation, 
# * 3 marks for testing the assumptions and reporting, 
# * 3 marks for selection and carrying out correct test, 
# * 2 marks reporting your results and presenting the data.

raccoon <- read_csv("Raccoon.csv")
# Parsed with column specification:
# cols(
#   weight = col_double(),
#   sex = col_character(),
#   pop = col_character()
# )

str(raccoon)

# Null Hypothesis:
# ----------------
# There is no significant difference in weight between sex or population location

ggplot(raccoon, aes(x=sex, y=weight)) + 
  geom_boxplot() +
  xlab("Sex") + ylab("Weight (lbs)")

ggplot(raccoon, aes(x=pop, y=weight)) + 
  geom_boxplot() +
  xlab("Location") + ylab("Weight (lbs)")

# Mean by sex
tapply(raccoon$weight, raccoon$sex, FUN=mean)
# female     male 
# 41.19808 39.56875 

# SD by sex
tapply(raccoon$weight, raccoon$sex, FUN=sd)
#    female      male 
#   0.6178681 0.4816445 

# Mean by location
tapply(raccoon$weight, raccoon$pop, FUN=mean)
#        e        n        s        w 
#   40.46667 40.40435 40.48667 40.30385 

# SD by location
tapply(raccoon$weight, raccoon$pop, FUN=sd)
# e         n         s         w 
# 1.0165300 1.1034979 0.9164148 0.9856899 

# Test for normality of weight
shapiro.test(raccoon$weight) # W = 0.98463, p-value = 0.2983
# NOT significantly different to normal

# Test for homogeneity of variances for sex
bartlett.test(raccoon$weight, raccoon$sex)
# Bartlett's K-squared = 2.9537, df = 1, p-value = 0.08568
# So variances not significantly different

# Test for homogeneity of variances for location
bartlett.test(raccoon$weight, raccoon$pop)
# Bartlett's K-squared = 0.87902, df = 3, p-value = 0.8305
# So variances not significantly different

# Choose Tests:
# -------------
# Have one categorical and one continuous varialbe for each test
# Weight is normal and no significant difference variance
# Sex has 2 groups from different subjects so use independent t-test
# Location has 3+ groups so use ANOVA

# Test sex
####
t.test(raccoon$weight~raccoon$sex)
# 	Welch Two Sample t-test
# 	
# 	data:  raccoon$weight by raccoon$sex
# 	t = 14.767, df = 95.391, p-value < 2.2e-16
# 	alternative hypothesis: true difference in means is not equal to 0
# 	95 percent confidence interval:
# 	  1.410290 1.848364
# 	sample estimates:
# 	  mean in group female   mean in group male 
# 	              41.19808             39.56875 

# So significant different in weight between sexes

# Test location
####
annova.raccoon.pop <- aov(weight~pop, data=raccoon)
summary(annova.raccoon.pop)
#             Df Sum Sq Mean Sq F value Pr(>F)
# pop          3   0.53   0.178   0.178  0.911
# Residuals   96  96.10   1.001   

# So no significant difference in weights between populations

# Results Statement:
# ------------------
# There is no significant different in racoon weight between sites (F=0.19; df= 3; p = 0.91).
# There is a significant difference in racoon weight between the sexes;
# males (mean±sd, 39.57±0.48) and females (mean±sd, 41.2±0.62)  (t=14.77, df=95.39, p < 0.001).
ggplot(raccoon, aes(x=sex, y=weight)) + 
  geom_boxplot() +
  xlab("Sex") + ylab("Weight (lbs)")

ggplot(raccoon, aes(x=pop, y=weight)) + 
  geom_boxplot() +
  xlab("Location") + ylab("Weight (lbs)")

#################
# Section 3
#################

# Part 1:
# -------
# The figures below are taken from Garbutt & Wolters (2008) 
# “The natural regeneration of salt marsh on formerly reclaimed land” published in Applied Vegetation Science.
# Note: de-embankment means the point at which regeneration began. Reference marshes are natural marshes.

# a) For each figure write a sentence, suitable for a results section, that describes the contents of the figure. [2 marks]
####

# Figure 2:
# The number of years since de-embankment is a significant positive predictor of the number of species (R2 = 21.9, p = 0.05)

# Figure 3:
# The percentage cover of Spartina anglica is a significant negative predictor of the number of species (R2 = 30.3, p = 0.018)

# b) Using Fig 2 (left-hand plot), calculate the difference in species richness 
# if years since de- embankment is 136 (give your answer as an integer). [1 mark]
####
# ds = -5.01  + 0.037a where a is years since de-embankment
-5.01 + (0.037 * 136) # [1] 0.022
# 0

# c) Briefly state the confidence you would place in the relationships in the figures below 
# and why you think that this is the case. [2 marks]
####
# I wouldn't put a lot of confidence in either relationship, in each case the R2 is below 50% meaning that data points 
# are far away from the model.  The p value in each case is also only just inside what we would accept
# as a significant predictor.

# Part 2:
# -------
# a) Write a sentence suitable for a results section that describes the findings of this model. [3 marks]
####
# Results of a multiple linear model
# Overall, mean annual temperature, mean annual percipitation, draught stress
# and temperature of coldest month explained 20% of the variation in net primary productivity (adj R2 = 0.17, 
# F = 6.8, df = 8,217, p < 0.001).
# There is a significant negative relationship between mean annual temperature and net primary productivity (t = -3.22, p = 0.001),
# and a significant negative relationship between draught stress and net primary productivity (t = -3.82, p < 0.001),
# and a significant negative relationship between mean annual percipitation and net primary productivity (t = -2.0, 
# p = 0.05).  Temperature of coldest month was not a significant predictor of net primary productivity.

# b) What would you do next if you got this output? Describe the procedures you would carry out next [2 marks]
####
# I would remove the non-significant predictor (temperature of coldest month) and re-run the model. 
# If all other variables remained significant I would look at the model validation plots. 
# I would be looking for the residuals in the q-q plot to fit the line, check for patterns in the
# residual vs fitted plots and I would check that there were no outliers having undue influence on the model (cooks distance).

#################
# Section 4
#################
# The dataset bodyfat contains data on the percentage body fat from adult human with a range of other body 
# measurements and age. Load the data into R. Investigate which of the variables below are good predictors
# (explanatory variables) of percentage body fat.
# Explanatory variables: age, weight, height, chest, hip, ankle
#
# Marking guidance
# * 1 mark for the hypothesis; 
# * 1 mark for preliminary data exploration and visualisation; 
# * 2 marks for testing and reporting collinearity, 
# * 4 marks for model building and validation,
# * 2 marks reporting your results.

bodyfat <- read_csv("bodyfat.csv")
# Parsed with column specification:
#   cols(
#     ID = col_double(),
#     Perc_bodyfat = col_double(),
#     density = col_double(),
#     age = col_double(),
#     weight = col_double(),
#     height = col_double(),
#     chest = col_double(),
#     hip = col_double(),
#     ankle = col_double(),
#     adiposity = col_double(),
#     `fat free weight` = col_double(),
#     neck = col_double(),
#     abdomen = col_double(),
#     thigh = col_double(),
#     knee = col_double(),
#     bicep = col_double(),
#     forarm = col_double(),
#     wrist = col_double()
#   )

# Null Hypothesis:
# ----------------
# None of the body measurements are significant predictors of body fat.

str(bodyfat)
bodyfat_ds <- bodyfat[c(2,4,5,6,7,8,9)]

pairs(bodyfat_ds[2:7])
cor(bodyfat_ds[2:7])

#                age      weight     height     chest        hip      ankle
# age     1.00000000 -0.01605487 -0.2458865 0.1818148 -0.0581336 -0.1096162
# weight -0.01605487  1.00000000  0.5129130 0.8912862  0.9326905  0.5809059
# height -0.24588654  0.51291305  1.0000000 0.2235895  0.3967239  0.3945472
# chest   0.18181484  0.89128618  0.2235895 1.0000000  0.8249076  0.4470861
# hip    -0.05813360  0.93269051  0.3967239 0.8249076  1.0000000  0.5211850
# ankle  -0.10961619  0.58090590  0.3945472 0.4470861  0.5211850  1.0000000

cor(bodyfat_ds[2:7], method="spearman")
#               age      weight     height     chest         hip      ankle
# age     1.00000000 -0.01432326 -0.2324440 0.1678720 -0.07443697 -0.1329007
# weight -0.01432326  1.00000000  0.5255648 0.8957640  0.92848863  0.6916092
# height -0.23244400  0.52556484  1.0000000 0.2620227  0.43524041  0.4636551
# chest   0.16787197  0.89576397  0.2620227 1.0000000  0.80941792  0.5531059
# hip    -0.07443697  0.92848863  0.4352404 0.8094179  1.00000000  0.6205013
# ankle  -0.13290074  0.69160915  0.4636551 0.5531059  0.62050129  1.0000000

# It looks like weight, chest and hip are significantly correlated

# Check for normality of variables
shapiro.test <- sapply(bodyfat_ds[2:7], FUN=shapiro.test)
shapiro.test[2,]
# $age
# [1] 0.001033575
# $weight
# [1] 0.02741065
# $height
# [1] 0.2308541
# $chest
# [1] 0.00268305
# $hip
# [1] 0.005371076
# $ankle
# [1] 1.321744e-14

# Height is significantly different to normal
# Age, weight, chest, hip and ankle are not significant different to normal

# Let's check their correlation using pearson (as variables are normal)
cor.test(bodyfat$weight, bodyfat$chest) # t = 30.954, df = 248, p-value < 2.2e-16
cor.test(bodyfat$weight, bodyfat$hip) # t = 40.723, df = 248, p-value < 2.2e-16
cor.test(bodyfat$chest, bodyfat$hip) # t = 22.981, df = 248, p-value < 2.2e-16

# So weight, hip and chest are significantly correlated (p < 0.001, df = 248)
# We can combine these into a single variable ????

Mult.lm1 <- lm(Perc_bodyfat~age+weight+height+chest+hip+ankle, data=bodyfat) 
summary(Mult.lm1)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 11.11580   21.31377   0.522   0.6025    
# age          0.12273    0.02760   4.447 1.33e-05 ***
# weight       0.10515    0.05811   1.810   0.0716 .  
# height      -0.89369    0.18621  -4.799 2.78e-06 ***
# chest        0.16702    0.11554   1.446   0.1496    
# hip          0.36928    0.14546   2.539   0.0118 *  
# ankle       -0.31660    0.23735  -1.334   0.1835  

# So weight, chest and ankle are not signficant in model <- also all three are strongly correlated
# Let's drop these and try again
Mult.lm2 <- lm(Perc_bodyfat~age+height+hip, data=bodyfat) 
summary(Mult.lm2)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -24.29443    9.46301  -2.567   0.0108 *  
#   age           0.16546    0.02641   6.266 1.65e-09 ***
#   height       -0.76334    0.13887  -5.497 9.67e-08 ***
#   hip           0.89674    0.05475  16.377  < 2e-16 ***
hist(Mult.lm2$residuals)
shapiro.test(Mult.lm2$residuals) # W = 0.99274, p-value = 0.2614
# Residuals are significantly different to normal
par(mfrow=c(2,2)) 
plot(Mult.lm2)

# Let's try a GLM
Mult.glm <- glm(Perc_bodyfat~age+height+hip, data=bodyfat, family = poisson) 
summary(Mult.glm)
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.432905   0.431816   1.003    0.316    
# age          0.008989   0.001171   7.675 1.65e-14 ***
# height      -0.030766   0.005973  -5.151 2.59e-07 ***
# hip          0.042358   0.002186  19.373  < 2e-16 ***
  
hist(Mult.glm$residuals)
shapiro.test(Mult.glm$residuals)
par(mfrow=c(2,2)) 
plot(Mult.glm)
  
# Choosing Model
# -------------
# In the GLM the residuals go towards the cook's distance.  So will choose the LM.

# Results Statement
# ------------------
# Age, height and hip measurements were significant predictors of percentage of body fat
# predictors of the total amount of time spent asleep (multiple linear regression: adj R2 = 0.56, df = 246, p < 0.001).
# There was a significant positive relationship between age and percentage of body fat (t = 6.37, p < 0.001), a signficant
# negative relationship between height and percentage body fat (t = -5.50, p < 0.01) and a significant positive relationship
# between hip measurement and percentage body fat (t = 16.38, p < 0.001).

#################
# Section 5
#################
# The dataset Titanic contains data on the frequency of different passenger categories that either survived 
# or perished during the Titanic passenger liner tragedy. This includes information on passenger gender, age and class.
# Conduct a test to see if there is an association between class and survival for male adult passengers.
# Marking guidance
# * 1 mark for the hypothesis; 
# * 1 marks for preliminary data exploration and visualisation, 
# * 4 marks selecting the correct data in right format, 
# * 2 marks for selection and carrying out correct test, 
# * 2 marks reporting your results.

titanic <- read_csv("Titanic.csv")
# cols(
#   X1 = col_double(),
#   Class = col_character(),
#   Sex = col_character(),
#   Age = col_character(),
#   Survived = col_character(),
#   Freq = col_double()
# )
head(titanic)
str(titanic)
titanic

# Null Hypothesis:
# ----------------
# There is no sigificant association between class and survival for male adult passengers on titatic.

titantic_ds <- titanic[which(titanic$Sex=="Male" & titanic$Age=="Adult"),c(2,5,6)]
titantic_ds

titantic_tab <- with(titantic_ds, tapply(Freq, list(Survived, Class), FUN = identity))

# 2 categorical variables (class and survived) so use chi-squared test
chi.titanic<-chisq.test(titantic_tab) 
chi.titanic
# X-squared = 37.988, df = 3, p-value = 2.843e-08

# RESULTS STATEMENT: 
# ------------------
# There is a significant association between the passenger class and whether the person survived for male adults onboard Titanic.
# (X-squared = 37.99, df = 3, p-value < 0.001).
