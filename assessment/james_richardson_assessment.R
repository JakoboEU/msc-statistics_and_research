# Date: 05/03/2019 
# Name: James Richardson
# Student Number: 18057447

# Script Set Up
# -------------
# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/assessment")
# list files in working directory
list.files() 

# Load required libaries
library(tidyverse)
library(ggplot2)

#################
# Section 1
#################
root_biomass <- read_csv("root_biomass.csv")
# Parsed with column specification:
#   cols(
#     root_sample_ID = col_double(),
#     biomass = col_double(),   // root biomass of mangrove sample (mg)
#     species = col_character() // species name Bruguiera_spp + Rhizophora_spp
#   )

# Increased rootmass can provide more resilient protection as plants with increased roto systems can withstand longer wave energy
# before being ripped out.

# Investiage data to examine differences in root biomass between two plant species

str(root_biomass)

tapply(root_biomass$biomass, root_biomass$species, FUN=mean)
# Bruguiera_spp Rhizophora_spp 
# 19.63250       16.82471 
tapply(root_biomass$biomass, root_biomass$species, FUN=sd)
# Bruguiera_spp Rhizophora_spp 
# 1.2102203      0.9550321 

ggplot(root_biomass, aes(x=species, y=biomass)) + 
  geom_boxplot()

# Null Hypothesis:
# ----------------
# There is no significant difference in the root biomass between the two species.

# Test for normality
shapiro.test(root_biomass$biomass) # W = 0.97896, p-value = 0.2241
# NOT significantly different from normal

bartlett.test(root_biomass$biomass, root_biomass$species) # Bartlett's K-squared = 2.0063, df = 1, p-value = 0.1567
#  Variances of two groups are NOT significantly different

# Test for differences
# One categorical (species), one continuous (biomass)
# Biomass is normal and no significant difference in variance
# Use two-sample independent t-test
t.test(root_biomass$biomass~root_biomass$species)

# data:  root_biomass$biomass by root_biomass$species
# t = 11.452, df = 75.955, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2.319471 3.296117
# sample estimates:
#   mean in group Bruguiera_spp mean in group Rhizophora_spp 
# 19.63250                     16.82471 

# Results Statement:
# ------------------
# The biomass of Bruguiera (mean±sd, 19.63±1.21) vs Rhizophora (mean±sd, 16.82±0.96) was significantly different 
#  (t=11.452, df=75.955, p < 0.001).

ggplot(root_biomass, aes(x=species, y=biomass, group=species)) + 
  geom_boxplot() +
  ylab("Biomass (mg)") + xlab("Species") +
  scale_x_discrete(labels=c("Bruguiera", "Rhizophora"))

#################
# Section 2
#################
weta_count <- read_csv("weta_count.csv")
# cols(
#   transect_number = col_double(),   // id number of transect
#   weta_count = col_double(),        // counts of weta on each transect
#   beetle_abund = col_double(),      // density of ship rats measured in baited traps on each transect
#   rat_abund = col_double()          // density of ground beetles caught in pitfall traps on each transect
# )

# Is there a relationship between the weta counts and the two explanatory variables?

summary(weta_count)

plot(weta_count~beetle_abund, data=weta_count)
plot(weta_count~rat_abund, data=weta_count) 

# Null Hypothesis:
# ----------------
# There is no significant relationship between the number of weta and the number of beetles or ship rats

shapiro.test(weta_count$weta_count) # W = 0.99205, p-value = 0.3979
shapiro.test(weta_count$beetle_abund) # W = 0.93031, p-value = 8.238e-08
shapiro.test(weta_count$rat_abund) # W = 0.94624, p-value = 1.74e-06

# Abundances significantly different to normal
# Try some transformations
shapiro.test(sqrt(weta_count$beetle_abund)) # W = 0.92798, p-value = 5.466e-08
shapiro.test(log(weta_count$beetle_abund)) # W = 0.9025, p-value = 9.553e-10

shapiro.test(sqrt(weta_count$rat_abund)) # W = 0.98381, p-value = 0.0295

# So looks like beetle abundance can't be normal. So use spearman test
# The sqrt of rat abundance is however normal. So can use pearson test
# 

cor.test(weta_count$weta_count, sqrt(weta_count$rat_abund))
# t = -13.934, df = 185, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.7790952 -0.6375786
# sample estimates:
#   cor 
# -0.7156026 

cor.test(weta_count$weta_count, weta_count$beetle_abund, method="spearman")
# S = 1123200, p-value = 0.6777
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# -0.03059285 

nrow(weta_count)

# Correlation Results Statement (1):
# ----------------------------------
# There was a significant negative correlation between the number of weta
# and square root of the abundance of rat (Pearsons; n=187, p < 0.001, rho=-0.716)
# There was no signficiant correlation between the number of weta
# and the abundance of beetles (Spearmans; n=187, p = 0.68, rho = -0.03)

ggplot(weta_count, aes(x=rat_abund, y=weta_count)) + 
  geom_point() +
  ylab("Number of Weta") +
  xlab("Abundance of Rats") 

# Do either of the explanatory variables explain the weta count?
# --------------------------------------------------------------
# Check to see that beetle and rat abundance are not correlated.  Use spearman as neither normal
cor.test(weta_count$beetle_abund, weta_count$rat_abund, method="spearman") # p-value = 0.6945
# Beetle abundance and rat abundance are not correlated - so use both in model

model1<-lm(weta_count~beetle_abund+rat_abund, data=weta_count)
summary(model1)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     25.733247   1.022111  25.177   <2e-16 ***
#   beetle_abund  -0.008995   0.014377  -0.626    0.532    
# rat_abund       -0.484691   0.039791 -12.181   <2e-16 ***
  
# beetle abundance is not significant so remove from model

model2<-lm(weta_count~rat_abund, data=weta_count)
summary(model2)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     25.26988    0.70325   35.93   <2e-16 ***
#   rat_abund     -0.48459    0.03972  -12.20   <2e-16 ***

par(mfrow=c(2,2))
plot(model2)
# Q-Q Looks good; however bit of a tendancy towards cook's distance.
# We have count data so worth trying GLM with poisson distribution

model3<-glm(weta_count~rat_abund, data=weta_count, family="poisson")
summary(model3)
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     3.306816    0.029925  110.50   <2e-16 ***
#   rat_abund     -0.030536   0.002008  -15.21   <2e-16 ***
# Null deviance: 558.78  on 186  degrees of freedom
# Residual deviance: 306.09  on 185  degrees of freedom

par(mfrow=c(2,2))
plot(model3)
# Q-Q Plot is worse, leaving line for both low and high quantiles
# Better Residuals vs Leverage as not approaching cook's distance.
# Other two lines are flatter, as we have count data will accept the GLM model with poisson distribution

# Pseudo-R2 = (Null deviance-Residual deviance)/Null deviance
(558.78 - 306.09) / 558.78 # [1] 0.4522173

# Predictor Results Statement (2):
# --------------------------------
# Abundance of rats was found to be a significant negative predictor of the number of weta
# (Pseudo-R2 = 0.45, P < 0.001)

ggplot(weta_count, aes(x=rat_abund, y=weta_count)) + 
  geom_point() +
  geom_smooth(method=glm, method.args = list(family = "poisson")) + 
  ylab("Number of Weta") +
  xlab("Abundance of Rats") 


#################
# Section 3
#################
par(mfrow=c(1,1))
rats <- read_csv("rat.csv")
# cols(
#   population = col_character(),  // display = zoo population on public display, breeding = not on public display
#   copulation = col_character() // zero, one, two_plus
# )

# Investigate if there is an assocation between the number of copulation attempts and the jumping rats captivity status

summary(rats) 
head(rats)

# Have two categorical variables: use chi-squared.  First need to transform data
rats_table <- table(rats)
rats_table

#               copulation
# population  one   two_plus  zero
# breeding      8       31       9
# display       7        6      24

ggplot(rats, aes(copulation, ..count..)) + 
  geom_bar(aes(fill = population), position = "dodge") +
  scale_x_discrete(limits = c("zero", "one", "two_plus"), labels=c("Zero", "One", "2+")) +
  xlab("Number of attempted copulations")


# Null Hypothesis:
# ----------------
#  There is no signficant difference between the captivity status of a jumping rat and how many copulations it attempts

chisq.test(rats_table) # X-squared = 22.734, df = 2, p-value = 1.157e-05

# Results Statement:
# ------------------
# There was a significant association between the captivity status of a jumping rat and how many copulations it attempts (chi squared
# 22.734, df = 2, p < 0.001), with the breeding population attemping more copulations than the display population.

ggplot(rats, aes(copulation, ..count..)) + 
  geom_bar(aes(fill = population), position = "dodge") +
  scale_x_discrete(limits = c("zero", "one", "two_plus"), labels=c("Zero", "One", "2+")) +
  xlab("Number of attempted copulations")


#################
# Section 4
#################
rhinohorn <- read_csv("rhinohorn.csv")
# cols(
#   width = col_double(),         // rhinoceros beetle horn width in mm
#   location = col_character()    // three sampling locations; Alpine, Forest and Lowland
# )

# Investigate the differences in beetle horn width and the three landscape types

summary(rhinohorn)
hist(rhinohorn$width)
boxplot(rhinohorn$width~rhinohorn$location)

tapply(rhinohorn$width, rhinohorn$location, FUN=mean)
#  Alpine  Forest Lowland 
#  2.026   1.326   0.384 
tapply(rhinohorn$width, rhinohorn$location, FUN=sd)
#    Alpine    Forest   Lowland 
#  0.2746501 0.1977527 0.2333081 

# Null Hypothesis:
# ----------------
#  There is no significant difference between the width of rhinoceros beetle's horn at the different locations

shapiro.test(rhinohorn$width) # W = 0.93771, p-value = 3.556e-06
# Significantly different from normal

# Try some transforms
shapiro.test(sqrt(rhinohorn$width)) # W = 0.91356, p-value = 8.327e-08
shapiro.test(log(rhinohorn$width)) # W = 0.85373, p-value = 6.568e-11
# So width cannot be made normal with a transformation

# Comparing one continuous variable with one categorical variable with three groups.
# Have non-normal data
# So will use Kruskal-Wallis test
kruskal.test(width~as.factor(location), data = rhinohorn) # Kruskal-Wallis chi-squared = 129.93, df = 2, p-value < 2.2e-16
# So looks like groups significantly different.
# Let's check which groups are different

pairwise.wilcox.test(rhinohorn$width, as.factor(rhinohorn$location))
#         Alpine  Forest
#  Forest  <2e-16 -     
#  Lowland <2e-16 <2e-16
# So all locations are signicantly different

# Results Statement:
# ------------------
# Rhinoceros beetle horn width was significantly different at each of the locations, Alpine (mean±sd, 2.03±0.27),
# Forest (mean±sd, 1.33±0.20) and Lowland (mean±sd, 0.38±0.23) (Chi = 129.93, df = 2, p < 0.001).
#
ggplot(rhinohorn, aes(x=location, y=width)) + 
  geom_boxplot() +
  xlab("Location of Rhinoceros Beetle") + ylab("Horn Width (mm)") 

#################
# Section 5
#################

# Part a)
## i)
# The NMDS of gut microbe communities in primates fit the data well with low stress (0.12).  
# Wild douc monkeys and wild howler monkeys had significantly different gut microbe communities to all other primates. 
# There was some overlap between gut microbe communities in humans living in the USA and non-western countries.
# There was some overlap between gut microbe communities in humans living in non-western countries and other captive monkeys
# Humans were significantly separated along the NMDS x-axis from all wild monkeys.

## ii)
# anosim – Analysis of similarity
sim.type<-anosim(primates,  primates$type, permutations = 999, distance = "bray")
summary(sim.type)

# Part b)
## i)
# Diet diversity (family index) explains 58% of the gut microbial richness of primates (Rsq=0.58, 
# F=152.8, df= 1,107, p < 0.001).  

## ii)
# R2 > 50% and the p-value is very low so I would have a reasonable amount of confidence in this relationship.

## iii)
# 189.38x - 1158.54
(189.38 * 18) - 1158.54
# [1] 2250.3 OTU
