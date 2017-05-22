#Linear Models Assigment:
#Make a linear model for one or more of your hypotheses. Do some regression plots, and discuss a possible transformation.

#Transformation == not necessary

#If working directory is assigments folder (should just move all to one directory), need to make wd back one...
#setwd("../")

#Libraries:
library(lme4)
library(car)
#install.packages("effects")
library(effects)

#Perception
#source("Knoops_perception_Set_Data.R")
#Copied below!

############
library(ggplot2)
library(dplyr)
#library(lme4)
#library(car)
#The data
perception <- read.csv('Perception_all_Data.csv', h=TRUE)
##Notes from Dushoff:
# Don't use shortcut for TRUE. It's bad practice for both clarity and robustness
# It's better for the course if you can set your working directory to your source directory using R
#perception <- read.csv('Perception_all_Data.csv', h=T)

## Data manipulation:

# DGRP as factor
perception$DGRP <- factor(perception$DGRP)

#Average temp and humidity over 3 days and scaled:
perception$Avg_temp <- ((perception$Temp_24 + perception$Temp_48 + perception$Temp_start)/3)
perception$Avg_hum <- ((perception$Humidity_24 + perception$Humidity_48 + perception$Humidity_start)/3)
perception$Avg_BP <- ((perception$BP_Start + perception$BP_24 + perception$BP_48)/3)

## BMB: with() would save you some typing above, but saves you nothing
##  below ... in general, with() saves you whenever you have to type
##   mydataframename$  **more than once** in a command.

perception$TempScaled <- with(perception, scale(Avg_temp))
perception$HumidityScaled <- with(perception, scale(Avg_hum))
perception$BPScaled <- with(perception, scale(Avg_BP))

# Total those assorted into vials
perception$spiANDno_48 <- perception$Spider_48 + perception$Not_Spider_48

#Subsample of those with more than six total in vials
perception <- subset(perception, spiANDno_48 >= 6)

#remove unnecessary rows and rename variables (24 hour counts, etc.):
DGRP_by_counts_1 <- data.frame(perception$DGRP, perception$Sex, perception$Spider_48, perception$Not_Spider_48, perception$Outside_48, perception$Start_date, perception$TempScaled, perception$HumidityScaled, perception$BPScaled, perception$spiANDno_48)                                                              
colnames(DGRP_by_counts_1) <- c("DGRP", "Sex", "Spider", "Not_spider", "Outside", "Date", "Temp_Scaled", "Humidity_Scaled", "BP_Scaled" ,"Vial_total")

## BMB:
DGRP_by_counts <- with(perception,
                       data.frame(DGRP, Sex, Spider_48,
                                  Not_spider=Not_Spider_48, Outside_48,
                                  Date=Start_date, TempScaled, HumidityScaled,
                                  BPScaled, Vial_total= spiANDno_48))
names(DGRP_by_counts) <- gsub("Scaled","_Scaled",names(DGRP_by_counts))
names(DGRP_by_counts) <- gsub("_48$","",names(DGRP_by_counts))

stopifnot(all.equal(DGRP_by_counts_1,DGRP_by_counts))
#Proportion of flies with spiders for each observation:
DGRP_by_counts$proportion_spider <- with(DGRP_by_counts, Spider/Vial_total)

#Data set with all DGRP lines together into one group (by sex also): inclueds mean temperature scales (and humidity) and the mean proportion with spiders
DGRP_sub <-DGRP_by_counts %>%
  group_by(DGRP, Sex) %>%
  summarise(prop_spider=mean(proportion_spider),
            Mean_temp_scaled=mean(Temp_Scaled),
            Mean_hum_scaled=mean(Humidity_Scaled),
            Mean_BP_scaled=mean(BP_Scaled),
            se =((sd(proportion_spider))/sqrt(n())))


### Have two data sets to work with:
#DGRP_by_counts == each observation seperate
#DGRP_sub == DGRP lines combined into one line proportion

#print(summary(DGRP_by_counts))
#print(summary(DGRP_sub))


#Variables that may be used
## BMB: this breaks for me before I add unique()

DGRP_sub$DGRP <- factor(DGRP_sub$DGRP, levels = unique(DGRP_sub$DGRP[order(DGRP_sub$prop_spider)]))


DGRP_sub_Male <- subset(DGRP_sub, Sex == "Male")
DGRP_sub_Male$DGRP <- factor(DGRP_sub_Male$DGRP, levels = DGRP_sub_Male$DGRP[order(DGRP_sub_Male$prop_spider)])
DGRP_by_countsMALE <- subset(DGRP_by_counts, Sex == "Male")
DGRP_sub_Female <- subset(DGRP_sub, Sex == "Female")
DGRP_sub_Female$DGRP <- factor(DGRP_sub_Female$DGRP, levels = DGRP_sub_Female$DGRP[order(DGRP_sub_Female$prop_spider)])
DGRP_by_countsFEMALE <- subset(DGRP_by_counts, Sex == "Female")
####################


### Have two data sets to work with:
#DGRP_by_counts == each observation seperate
#DGRP_sub == DGRP lines combined into one line proportion
#Each is also split into only males and only females
print(summary(DGRP_by_counts))
print(summary(DGRP_sub))




#Perception Linear Models


# Generalized linear model of proportion with spider for DGRP sex differences
# Random effect == Humidity, Barometric Pressure and Temperature

#Fix binomial: use Spider and not spider (success and failure) 
#Need glmer (warning)
mod1 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date) + (0 + Sex|DGRP), data = DGRP_by_counts, family= "binomial")
print(summary(mod1))
Anova(mod1)
plot(allEffects(mod1))
plot(effect("Sex", mod1), ylab = "Proportion with Spider")

# This plot shows the population mean difference in proportion with the spider. With each DGRP line being identical, we can imagine each line replicate is a replicate of the same individual, and we measured the proportion of time with the spider for 59 "individuals" (lines). The population mean therefore can be an interpretation of a fly population proportion as a whole. The model (with random effects of day, temperature, humidity and barometric pressure) shows no sex difference with both ~ 50% of the time with spiders for the whole population. However, due to the negative correlation of within line variation, we can determine that the population propensity to go with the spider is random. 



#Line differences

#slow!
mod2 <- glmer(cbind(Spider, Not_spider) ~ 0+DGRP + Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date), data = DGRP_by_counts, family = "binomial"
)
print(summary(mod2), correlation = TRUE)
Anova(mod2)
plot(allEffects(mod2))
plot(effect("DGRP", mod2), ylab = "Proportion with spider", rotx = 90)

#Within lines, the proportion with spider is shown to be highly variable due to effects of the experimental days, with high overlap between lines, indicating that a mean proportion with the spider is ~50% for all lines. 


#Include Sex effects:
#R crashes with glmer / Binomial -- something wrong in the code? The lmer works.


#ALl effects combined:

mod3 <- lmer(proportion_spider ~ DGRP:Sex + Sex + DGRP + Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date), data = DGRP_by_counts)
print(summary(mod3), correlation = TRUE)
Anova(mod3)
plot(allEffects(mod3))
plot(effect("DGRP:Sex", mod3), multiline = TRUE, rotx = 90)


# R crashes with glmer / Binomial -- very slow so not needed to run

#Hashing out to not bother others computers if Source file

#mod4 <- glmer(cbind(Spider, Not_spider) ~ DGRP:Sex + Sex + DGRP + Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date), data = DGRP_by_counts, family = "binomial")

#print(summary(mod4), correlation = TRUE)
#Anova(mod4)
#plot(allEffects(mod4))
#plot(effect("DGRP:Sex", mod4), multiline = TRUE, rotx = 90)

#Plots for residals? Not sure and very confusing: Hashing out
require(graphics)
plot(mod3)
plot(mod1)
#plot(mod4)

#Very simple Linear model to show try diagnostic plots
mod5 <- lm(proportion_spider ~ Sex*DGRP, data = DGRP_by_counts)
plot(allEffects(mod5))
plot(mod5)
print(summary(mod5))