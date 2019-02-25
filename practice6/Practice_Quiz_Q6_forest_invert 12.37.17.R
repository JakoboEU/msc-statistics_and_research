# DD/MM/YY Your Name
# Practice Quiz Question 6
# Community ananlysis - ordination


# clears r console
rm(list=ls())
# check working directory
getwd()
# set working directory
setwd("") 
# show list of files in working directory
list.files()


################################################################################
## NMDS of forest canopy hemiptera and diptera

# load vegan package 
library(vegan) 

inverts<-read.csv("canopy.inverts.csv", header = TRUE) # import data 

# check data
names(inverts)
str(inverts) 

# nmds analysis 
invert.nmds <- metaMDS(inverts[2:72], k=2, trymax = 30) # stress = 0.1391
invert.nmds

# plot ordination 
plot(invert.nmds, display = "sites")
with(inverts, ordiellipse(invert.nmds, type, col=4,lwd=2, draw = "polygon", kind = c("sd"))) # add forest type ellipse
with(inverts, ordispider(invert.nmds, type, col="black", label=TRUE)) ## add forest type centroid lines

# Results sentence: # The NMDS of canopy invertebrate communities fit the data well with low stress (0.1391) using a Bray Curtis dissimilarity measure.  
# There was significant overlap in communities between afforested and reforested conifer plantations and also significant overlap between the two natural forest types
# (Oak and Ash). Norway Spruce plantations had very different canopy communties to all other forest types. Natural forest were seperated from all conifer plantations along 
# axis one of the NMDS. 

################################################################################

# anosim - provides a way to test statistically whether there is a significant difference between two or more groups of sampling units.
sim.type<-anosim(inverts[2:72],  inverts$type, permutations = 999, distance = "bray")
summary(sim.type)
# Results sentence: There was a signifcicant difference between canopy invertebrate communities sampled from the different forest types in Ireland (R=0.481, P-value<0.001)

################################################################################

# envfit: Environmental variables fitted to ordination
ef <- envfit(invert.nmds, inverts[74:78], na.rm = TRUE, permu = 999)
ef
plot(invert.nmds, display = "sites")
plot(ef, p.max = 0.05)
# Results: Understory vegetation height had the strongest influence on canopy invertebrate composition (R2=0.55, p-value<0.001) followed by ground vegetation 
# cover (R2=0.51, p-value<0.001), canopy cover (R2=0.43, p-value<0.001) and understory plant diversity (R2=0.39, p-value<0.001). Soil pH did not have a signifcant 
# influence on canopy invertebrate composition (P>0.05).

################################################################################

# fit surface to ordination 
plot(invert.nmds, display = "sites", type="n")
with(inverts, ordispider(invert.nmds, type, label=TRUE))
with(inverts, ordisurf(invert.nmds, under.hght, add = TRUE, col = "cornflowerblue"))
plot(ef, p.max = 0.05)

################################################################################

