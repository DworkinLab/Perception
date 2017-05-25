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

eff_plot <- effect("DGRP", mod2)
eff_plot <- as.data.frame(eff_plot)
#eff_plot
p_eff <- ggplot(eff_plot, aes(y = fit, x = reorder(DGRP,fit))) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.2, width = 0.2) +
  labs(y = "Fit", x = "DGRP Line") + 
  ggtitle("DGRP effect plot") +
  geom_hline(yintercept = 0.5, size=0.25) +
  theme(plot.title = element_text(size=22)) +
  theme(axis.text.x = element_text(angle=90, hjust = 1))
p_eff


#Model for sex effects

DGRP_by_counts$proportion_spider
mod3 <- lmer(proportion_spider ~ DGRP:Sex + Sex + DGRP + Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date), data = DGRP_by_counts)
print(summary(mod3), correlation = TRUE)
Anova(mod3)
plot(allEffects(mod3))
plot(effect("DGRP:Sex", mod3), multiline = TRUE, rotx = 90)
xx <- plot(effect("DGRP*Sex", mod3), multiline = FALSE, rotx = 90)
xx

dgrp_sex <- effect("DGRP*Sex", mod3)
dgrp_sex <- as.data.frame(dgrp_sex)

p_eff <- ggplot(data=dgrp_sex, aes(x=DGRP, y=fit, colour=Sex))+ 
  geom_point(size = 3, alpha=0.5) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.2, width = 0.2) +
  labs(y = "Fit", x = "DGRP Line") + 
  ggtitle("DGRP proportion with spider effects plot") +
  geom_hline(yintercept = 0.5, size=0.35, alpha=0.5) +
  theme(plot.title = element_text(size=22)) +
  theme(axis.text.x = element_text(angle=90, hjust = 1))
p_eff

DGRP_by_countsMALE <- subset(DGRP_by_counts, Sex == "Male")
DGRP_by_countsFEMALE <- subset(DGRP_by_counts, Sex == "Female")

#MALES
mod_male <- lmer(proportion_spider ~ DGRP+ Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date), data = DGRP_by_countsMALE)
print(summary(mod_male), correlation = TRUE)
Anova(mod_male)
#plot(allEffects(mod_male))
plot(effect("DGRP", mod_male), rotx = 90, main="Male")

eff_male <- effect("DGRP", mod_male)
eff_male <- as.data.frame(eff_male)
#eff_male
p_male <- ggplot(eff_male, aes(y = fit, x = reorder(DGRP,fit))) + 
  geom_point(size = 3, colour="blue") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.2, width = 0.2, colour="blue") +
  labs(y = "Fit", x = "DGRP Line") + 
  ggtitle("Male DGRP Lines") +
  geom_hline(yintercept = 0.5, size=0.5) +
  theme(plot.title = element_text(size=22)) +
  theme(axis.text.x = element_text(angle=90, hjust = 1))
p_male

#FEMALES
mod_female <- lmer(proportion_spider ~ DGRP+ Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date), data = DGRP_by_countsFEMALE)
print(summary(mod_female), correlation = TRUE)
Anova(mod_female)
#plot(allEffects(mod_female))
plot(effect("DGRP", mod_female), rotx = 90, main="Female")

#CHANGE COLOUR!
eff_female <- effect("DGRP", mod_female)
eff_female <- as.data.frame(eff_female)
#eff_female
p_female <- ggplot(eff_female, aes(y = fit, x = reorder(DGRP,fit))) + 
  geom_point(size = 3, colour="purple") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.2, width = 0.2, colour="purple") +
  labs(y = "Fit", x = "DGRP Line") + 
  ggtitle("Female DGRP Lines") +
  geom_hline(yintercept = 0.5, size=0.5) +
  theme(plot.title = element_text(size=22)) +
  theme(axis.text.x = element_text(angle=90, hjust = 1))
p_female
