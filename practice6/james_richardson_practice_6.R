# Date: 20/02/2019 
# Name: James Richardson
# Student Number: 18057447
# Data:  canopy.inverts.csv

# Script Set Up
# -------------
# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/practice6")
# list files in working directory
list.files() 

# Load required libaries
library(tidyverse)
library(ggplot2)
library(vegan)

# The multivariate dataset ‘canopy.inverts.csv’ can be downloaded from moodle. 
# This dataset contains the community composition of Diperta and Hemiptera from different types of forest in Ireland. 
# Samples were obtained by canopy fogging so represent the canopy invertebrates of these forests. 
# The forest types included in the study are Affor and Refor (afforested and reforested conifer plantations) 
# Ash and Oak (natural forest) and NS (Norway Spruce conifer plantations).
#
# Several environmental variables were also recorded;
# * canopy.cover = canopy cover measured by spherical densitometer 
# * soil.pH = average soil pH taken in each forest patch
# * ground.cover = average vegetation cover at ground level
# * under.light = understory vegetation height (m)
# * under.divers = understory plant species diversity (Shannon diversity)
#
# Using the vegan package for community analysis;
#   a)  Produce a NMDS ordination plot showing the different sites, using forest ‘type’ as
#       the site. Write a results sentence describing the plot [3 marks]
#   b)  Carry out a statistical test to investigate if there is a difference in the invertebrate communities between 
#       the forest types. Write a results sentence describing the results [2 marks]
#   c)  Carry out an analysis to assess if the environmental variables are significantly influencing the invertebrate community. 
#       Interpret the results. [3 marks]
#   d)  Finally, produce an ordination plot of the species data showing any significant environmental variables as 
#       vectors on the plot. Write a sentence describing the plot [2 marks]

canopy.inverts <- read_csv("canopy.inverts.csv")

# Parsed with column specification:
# cols(
#   .default = col_double(),
#   X1 = col_character(),
#   type = col_character()
# )
# See spec(...) for full column specifications.
# Warning messages:
#   1: Missing column names filled in: 'X1' [1] 
#   2: Duplicated column names deduplicated: 'Micr' => 'Micr_1' [32], 'Lonc' => 'Lonc_1' [38], 'Anth' => 'Anth_1' [43], 'Scat' => 'Scat_1' [52]
names(canopy.inverts)[names(canopy.inverts) == 'X1'] <- 'site'

names(canopy.inverts)
summary(canopy.inverts[2:72])

###################################################################################################
# Produce a NMDS ordination plot showing the different sites, using forest ‘type’ as the site. 
# Write a results sentence describing the plot [3 marks]
###################################################################################################
canopy.inverts.nmds <- metaMDS(canopy.inverts[2:72], k=2, trymax = 30) 

plot(canopy.inverts.nmds, display = "sites")
with(canopy.inverts, ordiellipse(canopy.inverts.nmds, type, col=4, lwd=2, draw = "polygon", kind = c("sd"))) 
with(canopy.inverts, ordispider(canopy.inverts.nmds, type, label=TRUE))

scores(canopy.inverts.nmds)
canopy.inverts.nmds 
# Call:
# metaMDS(comm = canopy.inverts[2:72], k = 2, trymax = 30) 
# 
# global Multidimensional Scaling using monoMDS
# 
# Data:     wisconsin(sqrt(canopy.inverts[2:72])) 
# Distance: bray 
# 
# Dimensions: 2 
# Stress:     0.1391154 
# Stress type 1, weak ties
# Two convergent solutions found after 20 tries
# Scaling: centring, PC rotation, halfchange scaling 
# Species: expanded scores based on ‘wisconsin(sqrt(canopy.inverts[2:72]))’ 

#
# There are three distinct communities of canopy invertebrates that inhabit conifer plantations, norway spruce
# and natural forests.   The community make up is very similar in conifer plantations no matter
# whether they're afforested or reforested.  The community make up is also very similar in natural forests no
# matter whether they're Oak or Ash.
# The variation in community is mainly along the X-Axis.
# The stress of the ordination is 0.14 which suggests that the ordination can be used.
#


###################################################################################################
# Carry out a statistical test to investigate if there is a difference in the invertebrate communities 
# between the forest types. Write a results sentence describing the results [2 marks]
###################################################################################################

sim.type<-anosim(canopy.inverts[2:72], canopy.inverts$type, permutations = 999, distance = "bray") 
summary(sim.type)

# Call:
# anosim(x = canopy.inverts[2:72], grouping = canopy.inverts$type,      permutations = 999, distance = "bray") 
# Dissimilarity: bray 
# 
# ANOSIM statistic R: 0.4807 
# Significance: 0.001 
# 
# Permutation: free
# Number of permutations: 999
# 
# Upper quantiles of permutations (null model):
#   90%    95%  97.5%    99% 
#   0.0808 0.1129 0.1457 0.1831 
# 
# Dissimilarity ranks between and within classes:
#         0%    25%   50%    75% 100%   N
# Between  1 131.75 242.5 341.25  435 360
# Affor    4  25.50 178.0 332.00  406  15
# Ash     20  69.50  98.0 184.00  275  15
# NS      35 138.50 171.0 221.50  334  15
# Oak      7  35.50  66.0 114.00  216  15
# Refor    2  11.00  33.0 156.50  323  15

# Results Statement
# -----------------
# There is a significant difference between invertibrate communities between forest types (ANOSIM statistic R = 0.4807, p = 0.001)

###################################################################################################
# Carry out an analysis to assess if the environmental variables are significantly influencing 
# the invertebrate community.  Interpret the results. [3 marks]
###################################################################################################
summary(canopy.inverts[73:78])

pairs(canopy.inverts[74:78])
cor(canopy.inverts[74:78])

# envfit: Environmental variables fitted to an ordination
canopy.inverts.ef <- envfit(canopy.inverts.nmds, canopy.inverts[74:78], na.rm = TRUE, permu = 999) 
canopy.inverts.ef
# ***VECTORS
# 
#                 NMDS1    NMDS2     r2   Pr(>r)    
# canopy.cover  -0.93679  0.34988 0.4332  0.001 ***
# soil.pH       -0.97906 -0.20359 0.0524  0.500    
# ground.cover   0.66145  0.74999 0.5133  0.001 ***
# under.hght     0.59747  0.80189 0.5453  0.001 ***
# under.divers   0.85410  0.52010 0.3886  0.002 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Permutation: free
# Number of permutations: 999


#
# canopy.cover is significantly acting on the X-axis.
# ground.cover is significantly acting on the X-axis and Y-axis
# under.hght is significantly acting on the Y-axis
# under.divers is acting on the X-axis

###################################################################################################
# Finally, produce an ordination plot of the species data showing any significant environmental 
# variables as vectors on the plot. Write a sentence describing the plot [2 marks]
###################################################################################################
plot(canopy.inverts.nmds, display = "sites")
with(canopy.inverts, ordiellipse(canopy.inverts.nmds, type, col=4, lwd=2, draw = "polygon", kind = c("sd"), label=TRUE)) 
plot(canopy.inverts.ef, p.max = 0.05)

#
# An increase in canopy cover affects the community make up in the conifer forests whereas an increase in 
# under canopy light, ground cover and diversity affects the community make up in the natural forests.
#

