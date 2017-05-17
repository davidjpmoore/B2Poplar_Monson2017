# # Main version
# install.packages("lme4")
# install.packages("arm")
# install.packages("lsmeans")
# install.packages("nlme")

#########################################################
#Author: Dave Moore
#Date: 05/16/2017
#Purpose: create ls means for Monson et al Manuscript on Poplar biomass and isoprene
#########################################################
library(lme4)  # load library
library(arm)  # convenience functions for regression in R
library(lsmeans) #get LSmeans from models
library(nlme) #mixed effects model
library(dplyr) #grammar for data
library(ggplot2) #plotting function

#
AGBio= read.csv("./data/AGbiomassPopB205162017.csv",header = T)
AGBio_F = AGBio %>%
  mutate(Time = as.factor(YEAR))

av = aov(AgBiomass_kg ~ Block + Time*Type, data=AGBio_F)
anova(av)
AGbiomas.rg0 <- ref.grid(av)
lsMeansfxd = lsmeans(av, ~ Time*Type)
lsmeans(av, ~ Type)
#lsmeans stored in csv file

#load lsmeans
AGBLSMEANS= read.csv("./data/AGlsmeans.csv",header = T)
#plot lsmeans

pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(AGBLSMEANS, aes(x=Time, y=AGB_lsmean, colour=Type)) + 
  geom_errorbar(aes(ymin=ABGlowerCL, ymax=ABGupperCL), width=.1) +
  geom_line() +
  geom_point( size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Above Ground Biomass") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0.7))               # Position legend in top right

#### below ground

BGBio= read.csv("./data/RootbiomassPopB205162017.csv",header = T)

bav = aov(RootWt_kg ~ Type, data=BGBio)
anova(bav)

BGbiomas.rg0 <- ref.grid(bav)
lsmeans(bav, ~ Type)

#load lsmeans
BGBLSMEANS= read.csv("./data/BGlsmeans.csv",header = T)
#plot lsmeans

ggplot(BGBLSMEANS, aes(x=Type, y=lsmean)) + 
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1) +
  geom_line() +
  geom_point( size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Genotype") +
  ylab("Below Ground Biomass") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0.7))               # Position legend in top right

