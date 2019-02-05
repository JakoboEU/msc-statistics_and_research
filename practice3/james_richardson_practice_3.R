# Date: 06/02/2019 
# Name: James Richardson
# Student Number: 18057447
# Data:  lichen.csv

# Script Set Up
# -------------
# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/practice3")
# list files in working directory
list.files() 

# Load required libaries
library(tidyverse)
library(ggplot2)
library(multcomp)

# Analysis Pathway
# ------------------
# 1. Write clear hypotheses
# 2. Explore descriptive statistics – means, standard deviation, distribution etc
# 3. Create appropriate plots to visualise/explore data
# 4. Decide on test(s) to be done (see flow chart)
# 5. Examine assumptions of tests: normality, variances, independence
# 6. Carry out tests
# 7. Report your results: results statements / plots

# Marking guidance
# -----------------
# 2 marks for testing correlations; 
# 3 marks for correctly reporting the correlations; 
# 2 marks for testing the relationship between growth rate and pollution index; 
# 2 marks for reporting the results statement; 
# 1 mark for plotting the relationship, including a fitted/modeled line.

# Different species of lichen are often identified as tolerant or intolerant to atmospheric pollution, 
# and hence the occurrence of different lichen species has been used as a guide to pollution levels. 
# This dataset contains the results of a study looking at the lichen growth rate of a single species 
# at various locations across the UK. At each location several measures of pollution were recorded 
# as well as the number of bacteria on each sampled lichen. 
# The researchers are interested to know if the growth rate of an individual species can be related 
# to the amount of pollution and if the growth rate is related to the bacterial community inhabiting the lichen. 
# The variables in the dataset are;
# * plot – individual number of each sampled location.
# * growth_rate – lichen growth rate (cm2/yr).
# * pollution_index – composite measure of pollution (no units), higher values indicate greater level of pollution.
# * NOx – pollution measurement, oxides of nitrogen (g/ha/yr).
# * bacteria – number of bacteria (measured in colony forming units) from sampled lichen branch (cfu/g).
# * NH3 – pollution measurement, atmospheric ammonia (ppb).

lichen <- read_csv("lichen.csv")
# Parsed with column specification:
#   cols(
#     Plot = col_character(),
#     growth.rate = col_double(),
#     polution.index = col_double(),
#     NH3 = col_double(),
#     bacteria = col_double(),
#     NOx = col_double()
#   )

# Examine the dataset to answer the following research questions;
# 1) Is there correlation between any of the measured variables? 
#     If so, statistically report the three strongest correlations.
# 2) Is there a relationship between lichen growth rate and the pollution index? 
#     [hint: think carefully about this relationship, does one variable directly impact the other?]
# P.S. there will be no hints in the unit assessment.

summary(lichen)
lichen_continuous<-na.omit(lichen[,2:6])
pairs(lichen_continuous)
cor(lichen_continuous)

