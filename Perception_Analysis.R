#Official R-code for the perception Data:

#Packages:
library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(effects)
require(lattice)

perception <- read.csv('Perception_all_Data.csv', h=TRUE)

#DGRP as factor
perception$DGRP <- factor(perception$DGRP)

#Average temp and humidity over three days and scaled:
perception$Avg_temp <- with(perception, (Temp_24 + Temp_48 + Temp_start)/3)

perception$Avg_hum <- with(perception, (Humidity_24 + Humidity_48 + Humidity_start)/3)

perception$Avg_BP <- with(perception, (BP_Start + BP_24 + BP_48)/3)

perception$TempScaled <- with(perception, scale(Avg_temp))

perception$HumidityScaled <- with(perception, scale(Avg_hum))

perception$BPScaled <- with(perception, scale(Avg_BP))

#Total those assorted into vials
perception$spiANDno_48 <- with(perception, Spider_48 + Not_Spider_48)

#Subsample of those with more than six total in vials
perception <- subset(perception, spiANDno_48 >= 6)

#New data frame with needed variables (and change some names):
DGRP_by_counts <- with(perception,
                       data.frame(DGRP, Sex, Spider=Spider_48,
                                  Not_spider=Not_Spider_48, Outside=Outside_48,
                                  Date=Start_date, Temp_Scaled=TempScaled, Humidity_Scaled=HumidityScaled,
                                  BP_Scaled=BPScaled, Vial_total= spiANDno_48, Number=Number))

DGRP_by_counts$proportion_spider <- with(DGRP_by_counts, Spider/Vial_total)

#Data set with all DGRP lines together into one group (by sex also): inclueds mean temperature scales (and humidity) and the mean proportion with spiders and SE

DGRP_sub <-DGRP_by_counts %>%
  group_by(DGRP, Sex) %>%
  summarise(prop_spider=mean(proportion_spider),
            Mean_temp_scaled=mean(Temp_Scaled),
            Mean_hum_scaled=mean(Humidity_Scaled),
            Mean_BP_scaled=mean(BP_Scaled),
            se =((sd(proportion_spider))/sqrt(n())))


###Sex Correlation:
with(DGRP_sub, cor(prop_spider[Sex == "Female"],prop_spider[Sex == "Male"] ))

MFcor <- with(DGRP_sub, cor(prop_spider[Sex == "Female"],prop_spider[Sex == "Male"] ))

##rcorr:
#install.packages("Hmisc")
library(Hmisc)
#??Hmisc
with(DGRP_sub, rcorr(x = prop_spider[Sex == "Female"],y = prop_spider[Sex == "Male"]))

line2 <- lm(DGRP_sub$prop_spider[DGRP_sub$Sex == "Female"] ~ DGRP_sub$prop_spider[DGRP_sub$Sex == "Male"])

with(DGRP_sub, plot(x = prop_spider[Sex == "Female"],y = prop_spider[Sex == "Male"], xlab = "Females", ylab = "Males", abline(line2), main = "Male Female correlation: proportion with spider"))

legend("bottomleft", bty="y", 
       legend=paste("R = -0.28" , "p-value = 0.0321" ))




#Model:
## DGRP is a random effect

#Remove date or temp,hum,bp:  Redundant to have both?

mod1 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + Temp_Scaled + Humidity_Scaled + BP_Scaled #+ (1|Date) 
              + (0 + Sex|DGRP), data = DGRP_by_counts, family = "binomial")
summary(mod1)
plot(allEffects(mod1))
ranef(mod1)
ranef(mod1, condVar = TRUE)
rr1 <- ranef(mod1, condVar = TRUE)
str(rr1)
fixef(mod1)
dotplot(rr1)
qqmath(rr1)


rr1 <- ranef(mod1, condVar = TRUE)
pv <- attr(rr1$DGRP, "postVar")
pv
se_fem <- pv[1, 1, ]
se_ma <- pv[2,2, ]
#Data frame of intercepts
rand.interc<-rr1$DGRP
df<-data.frame(Intercepts=rr1$DGRP[,1:2], DGRP=rownames(rand.interc), se_female=se_fem, se_male=se_ma)
head(df)
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

#install.packages("pbkrtest")
library("pbkrtest")

#Removing temp, humidity and BP from analysis: Humidity showed to have an effect but this is incorperated into to date effect we keep!
# With temp, humidity and BP removed.
#Large model: mod1
mod1 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + (1|Date) + (0 + Sex|DGRP),
              data = DGRP_by_counts, family = "binomial")
summary(mod1)

mod2 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + (1|Date) + (1|DGRP),
              data = DGRP_by_counts, family = "binomial")
summary(mod2)

#Small Model:
mod3 <- glmer(cbind(Spider, Not_spider) ~ 1 + Sex + (1|Date), 
              data = DGRP_by_counts, family = "binomial")


PB_Mod <- PBmodcomp(mod2, mod3, nsim = 100, ref = NULL, seed = NULL,
          cl = NULL, details = 0)
summary(PB_Mod)

