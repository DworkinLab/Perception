#Official R-code for the perception Data:

#Packages:
library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(effects)
require(lattice)

#install.packages("pbkrtest")
library("pbkrtest")

#Possibly not needed
library(Hmisc)


perception <- read.csv('Perception_all_Data.csv', h=TRUE)

#Remove all temp, bp and humidity (not using in analysis) and 24 hour counts
#Note: Humidity showed to have some effect but beacues this is only three readings over 48 hours, day effects should incorperate changing humiduty throughout the day

perception <- subset(perception, select = -c(Spider_24, Not_Spider_24, Outside_24, Humidity_start, Humidity_24, Humidity_48, Temp_start, Temp_24, Temp_48, BP_Start, BP_24, BP_48) )

#DGRP as factor

perception$DGRP <- factor(perception$DGRP)

#Total those assorted into vials
perception$spiANDno_48 <- with(perception, Spider_48 + Not_Spider_48)

#Subsample of those with more than six total in vials
perception <- subset(perception, spiANDno_48 >= 6)

#New data frame with needed variables (and change some names):
DGRP_by_counts <- with(perception,
                       data.frame(DGRP, Sex, Spider=Spider_48,
                                  Not_spider=Not_Spider_48, Outside=Outside_48,
                                  Date=Start_date, Vial_total= spiANDno_48, Number=Number))

DGRP_by_counts$proportion_spider <- with(DGRP_by_counts, Spider/Vial_total)

#Data set with all DGRP lines together into one group (by sex also): inclueds mean temperature scales (and humidity) and the mean proportion with spiders and SE

DGRP_sub <-DGRP_by_counts %>%
  group_by(DGRP, Sex) %>%
  summarise(prop_spider=mean(proportion_spider),
            se =((sd(proportion_spider))/sqrt(n())))


###Sex Correlation:
with(DGRP_sub, cor(prop_spider[Sex == "Female"],prop_spider[Sex == "Male"] ))

MFcor <- with(DGRP_sub, cor(prop_spider[Sex == "Female"],prop_spider[Sex == "Male"] ))

##rcorr:
#??Hmisc
with(DGRP_sub, rcorr(x = prop_spider[Sex == "Female"],y = prop_spider[Sex == "Male"]))

line2 <- lm(DGRP_sub$prop_spider[DGRP_sub$Sex == "Female"] ~ DGRP_sub$prop_spider[DGRP_sub$Sex == "Male"])

with(DGRP_sub, plot(x = prop_spider[Sex == "Female"],y = prop_spider[Sex == "Male"], xlab = "Females", ylab = "Males", abline(line2), main = "Male Female correlation: proportion with spider"))

legend("bottomleft", bty="y", 
       legend=paste("R = -0.28" , "p-value = 0.0321" ))


#Model:
## DGRP is a random effect

#Remove date or temp,hum,bp:  Redundant to have both?

mod1 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex  + (1|Date) 
              + (0 + Sex|DGRP), data = DGRP_by_counts, family = "binomial")
summary(mod1)
rr1 <- ranef(mod1, condVar = TRUE)
dotplot(rr1)
pv <- attr(rr1$DGRP, "postVar")
se_fem <- pv[1, 1, ]
se_ma <- pv[2,2, ]
#Data frame of intercepts
rand.interc<-rr1$DGRP
df<-data.frame(Intercepts=rr1$DGRP[,1:2], DGRP=rownames(rand.interc), se_female=se_fem, se_male=se_ma)
colnames(df) <- c("Female_Int", "Male_Int", "DGRP", "Female_se", "Male_se")
df$m.low <- with(df, Male_Int - 2 * Male_se)
df$m.high <- with(df, Male_Int + 2 * Male_se)
df$f.low <- with(df, Female_Int - 2 * Female_se)
df$f.high <- with(df, Female_Int + 2 * Female_se)

ggplot(df, aes(y=Female_Int, x=reorder(DGRP, Female_Int))) + 
  geom_linerange(aes(ymin=f.low, ymax=f.high), colour="black") + 
  geom_point(colour="red") + 
  coord_flip() + 
  labs(y="Intercept", x="DGRP Females")

ggplot(df, aes(y=Male_Int, x=reorder(DGRP, Male_Int))) + 
  geom_linerange(aes(ymin=m.low, ymax=m.high), colour="black") + 
  geom_point(colour="blue") + 
  coord_flip() + 
  labs(y="Intercept", x="DGRP Males")

#Model comparison: comparing with random DGRP effects and without:

#Large model: mod1

mod1 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + 
                (1|Date) + (0 + Sex|DGRP),
              data = DGRP_by_counts, family = "binomial")
summary(mod1)
Anova(mod1)

mod2 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + (1|Date) + (1|DGRP),
              data = DGRP_by_counts, family = "binomial")
summary(mod2)
Anova(mod2)

#Small Model:
mod3 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + (1|Date), 
              data = DGRP_by_counts, family = "binomial")
summary(mod3)
Anova(mod3)

PB_Mod_1 <- PBmodcomp(mod2, mod3, nsim = 10, ref = NULL, seed = NULL,
                    cl = NULL, details = 0)
summary(PB_Mod_1)

PB_Mod_2 <- PBmodcomp(mod1, mod2, nsim = 10, ref = NULL, seed = NULL,
                    cl = NULL, details = 0)
summary(PB_Mod_2)
