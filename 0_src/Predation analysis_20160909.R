
setwd("C:\\Users\\nicol\\Dropbox\\Data entry\\Paper_3_mortality")

#DATA
predation<-read.csv("Predation_experiment_20160909.csv")
mammal<-read.csv("Predation_experiment_mammals_20160910.csv")
bird<-read.csv("Predation_experiment_birds_20160910.csv")

#LIBRARIES
library(lme4)
library(car)
library(effects)
library(aods3)
library(AICcmodavg)
library(multcompView)
library(lsmeans)
library(vegan)
library(MuMIn)
library(blme)

#trap.no_factor
str(predation_no_model)
predation_no_model$trap.no<-as.factor(predation_no_model$trap.no)
str(predation_no_model)

#SCENTED MODEL ANALYSIS - MODELS ONLY
#revised analysis
anova(update(mammal.model,.~.-(model.type:treatment:habitat:harvest+treatment:habitat:harvest)),test="Chisq")
#allpredators
m3<-glmer(predation_2 ~ model.type +(1|site/unique_trap_no), family=binomial, data=predation)
#mammals
m2<-glmer(predation_2 ~ model.type +(1|site/unique_trap_no), family=binomial, data=mammal)
lsm = lsmeans(m2, pairwise ~ model.type,adjust="tukey")
letterz<-cld(lsm, alpha=.2,  Letters=letters)
posthoc<-letterz$.group
letterz
#birds
m4<-glmer(response ~ model.type +(1|site/unique_trap_no), family=binomial, data=bird)

#OVERALL ANALYSIS
#PREDATION ALL
m1<-glmer(predation_2 ~ treatment*habitat*harvest + model.type + (1|site/unique_trap_no), family=binomial, data=predation)

#posthoc
#habitat  
lsm = lsmeans(m1, pairwise ~ habitat,adjust="tukey")
letterz<-cld(lsm, alpha=0.05, Letters=letters)
posthoc<-letterz$.group
letterz
#treatment:harvest  
lsm = lsmeans(m1, pairwise ~ treatment:harvest,adjust="tukey")
letterz<-cld(lsm, alpha=0.09, Letters=letters)
posthoc<-letterz$.group
letterz

#MAMMALS
m2<-glmer(predation_2 ~ treatment + habitat + harvest +
            treatment:harvest + harvest:habitat + treatment:habitat+
          + (1|site/unique_trap_no), family=binomial, data=mammal)
#post hoc
#treatment:habitat  
lsm = lsmeans(m3, pairwise ~ treatment:habitat,adjust="tukey")
letterz<-cld(lsm, alpha=0.1, Letters=letters)
posthoc<-letterz$.group
letterz
#habitat  
lsm = lsmeans(m3, pairwise ~ habitat,adjust="tukey")
letterz<-cld(lsm, alpha=0.05, Letters=letters)
posthoc<-letterz$.group
letterz
#habitat:harvest  
lsm = lsmeans(m2, pairwise ~ habitat:harvest,adjust="tukey")
letterz<-cld(lsm, alpha=0.11, Letters=letters)
posthoc<-letterz$.group
letterz
#treatment:habitat:harvest  
lsm = lsmeans(m3, pairwise ~ treatment:habitat:harvest,adjust="tukey")
letterz<-cld(lsm, alpha=0.1, Letters=letters)
posthoc<-letterz$.group
letterz

#BIRD
m3<-glmer(response~treatment+habitat+harvest +
            treatment:habitat+ treatment:harvest+harvest:habitat+(1|site/unique_trap_no), family=binomial, data=bird)
#posthoc
#habitat:harvest
lsm = lsmeans(m3, pairwise ~ habitat:harvest,adjust="tukey")
letterz<-cld(lsm, alpha=0.05, Letters=letters)
posthoc<-letterz$.group
letterz
#habitat  
lsm = lsmeans(m3, pairwise ~ habitat,adjust="tukey")
letterz<-cld(lsm, alpha=0.05, Letters=letters)
posthoc<-letterz$.group
letterz
#harvest
lsm = lsmeans(m3, pairwise ~ harvest,adjust="tukey")
letterz<-cld(lsm, alpha=0.05, Letters=letters)
posthoc<-letterz$.group
letterz

#mammal
m6<-glmer(predation_2 ~ treatment + habitat + harvest + treatment:harvest + harvest:habitat + treatment:habitat + (1|site/unique_trap_no), family=binomial, data=mammal)

#posthoc
#habitat:harvest
lsm = lsmeans(m6, pairwise ~ habitat:harvest,adjust="tukey")
letterz<-cld(lsm, alpha=0.06, Letters=letters)
posthoc<-letterz$.group
letterz
#habitat:harvest  
lsm = lsmeans(m6, pairwise ~ habitat:harvest,adjust="tukey")
letterz<-cld(lsm, alpha=0.2, Letters=letters)
posthoc<-letterz$.group
letterz
#model.type 
lsm = lsmeans(m6, pairwise ~ model.type,adjust="tukey")
letterz<-cld(lsm, alpha=0.2, Letters=letters)
posthoc<-letterz$.group
letterz