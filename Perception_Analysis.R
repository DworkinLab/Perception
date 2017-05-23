#Official R-code for the perception Data:

#Packages:
library(ggplot2)
 library(dplyr)
library(lme4)
library(car)
library(effects)

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

line2 <- lm(DGRP_sub$prop_spider[DGRP_sub$Sex == "Female"] ~ DGRP_sub$prop_spider[DGRP_sub$Sex == "Male"])

with(DGRP_sub, plot(x = prop_spider[Sex == "Female"],y = prop_spider[Sex == "Male"], xlab = "Females", ylab = "Males", abline(line2), main = "Male Female correlation: proportion with spider"))


#Models: 
mod2 <- glmer(cbind(Spider, Not_spider) ~ 0+DGRP + Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date), data = DGRP_by_counts, family = "binomial"
)
print(summary(mod2), correlation = TRUE)
Anova(mod2)
plot(allEffects(mod2))
plot(effect("DGRP", mod2), ylab = "Proportion with spider", rotx = 90)

DGRP_by_counts$proportion_spider
mod3 <- lmer(proportion_spider ~ DGRP:Sex + Sex + DGRP + Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date), data = DGRP_by_counts)
print(summary(mod3), correlation = TRUE)
Anova(mod3)
plot(allEffects(mod3))
plot(effect("DGRP:Sex", mod3), multiline = TRUE, rotx = 90)
xx <- plot(effect("DGRP*Sex", mod3), multiline = FALSE, rotx = 90)
xx

DGRP_by_countsMALE <- subset(DGRP_by_counts, Sex == "Male")
DGRP_by_countsFEMALE <- subset(DGRP_by_counts, Sex == "Female")

mod_male <- lmer(proportion_spider ~ DGRP+ Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date), data = DGRP_by_countsMALE)
print(summary(mod_male), correlation = TRUE)
Anova(mod_male)
#plot(allEffects(mod_male))
plot(effect("DGRP", mod_male), rotx = 90, main="Male")

mod_female <- lmer(proportion_spider ~ DGRP+ Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date), data = DGRP_by_countsFEMALE)
print(summary(mod_female), correlation = TRUE)
Anova(mod_female)
#plot(allEffects(mod_female))
plot(effect("DGRP", mod_female), rotx = 90, main="Female")
mod_female

#summary(mod_female)$coefficients
eff1 <- effect("DGRP", mod_female)
eff1 <- as.data.frame(eff1)
eff1
p2 <- ggplot(eff1, aes(y = fit, x = reorder(DGRP,fit))) + 
  geom_point(size = 4) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.2, width = 0.2) +
  labs(y = "Fit", x = "DGRP Line")
p2
