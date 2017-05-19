

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("lme4")
#install.packages("car")
library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(effects)

#The Data
perception <- read.csv('Perception_all_Data.csv', h=T)

#DGRP as factor
perception$DGRP <- factor(perception$DGRP)

#Average temp and humidity over three days and scaled:
perception$Avg_temp <- ((perception$Temp_24 + perception$Temp_48 + perception$Temp_start)/3)

perception$Avg_hum <- ((perception$Humidity_24 + perception$Humidity_48 + perception$Humidity_start)/3)

perception$Avg_BP <- ((perception$BP_Start + perception$BP_24 + perception$BP_48)/3)

perception$TempScaled <- with(perception, scale(Avg_temp))

perception$HumidityScaled <- with(perception, scale(Avg_hum))

perception$BPScaled <- with(perception, scale(Avg_BP))

#Total those assorted into vials
perception$spiANDno_48 <- perception$Spider_48 + perception$Not_Spider_48

#Subsample of those with more than six total in vials
perception <- subset(perception, spiANDno_48 >= 6)

#Remove unnecessary rows and rename variables:
DGRP_by_counts <- data.frame(perception$DGRP, perception$Sex, perception$Spider_48, perception$Not_Spider_48, perception$Outside_48, perception$Start_date, perception$Avg_hum, perception$Avg_temp, perception$Avg_BP, perception$TempScaled, perception$HumidityScaled, perception$BPScaled, perception$spiANDno_48, perception$Number)                                                              
colnames(DGRP_by_counts) <- c("DGRP", "Sex", "Spider", "Not_spider", "Outside", "Date", "Average_Hum", "Average_Temp",  "Average_BP","Temp_Scaled", "Humidity_Scaled", "BP_Scaled" ,"Vial_total", "Number")


#Proportion of flies with spiders for each observation:
DGRP_by_counts$proportion_spider <- with(DGRP_by_counts, Spider/Vial_total)

#Data set with all DGRP lines together into one group (by sex also): inclueds mean temperature scales (and humidity) and the mean proportion with spiders

DGRP_sub <-DGRP_by_counts %>%
  group_by(DGRP, Sex) %>%
  summarise(Spider=sum(Spider), Not_Spider=sum(Not_spider), Outside=sum(Outside), Vial_total=sum(Vial_total), Mean_prop_spi=mean(proportion_spider),Mean_temp_scaled=mean(Temp_Scaled), Mean_hum_scaled=mean(Humidity_Scaled), Mean_BP_scaled=mean(BP_Scaled))

#Similar to average proportion already included:
DGRP_sub$prop_spider <- DGRP_sub$Spider/DGRP_sub$Vial_total

#Have two data sets to work with:
#DGRP_by_counts == each observation seperat
summary(DGRP_by_counts)

#DGRP_sub == DGRP lines combined into one line proportion
summary(DGRP_sub)

###Sex Correlation:

with(DGRP_sub, cor(prop_spider[Sex == "Female"],prop_spider[Sex == "Male"] ))

line2 <- lm(DGRP_sub$prop_spider[DGRP_sub$Sex == "Female"] ~ DGRP_sub$prop_spider[DGRP_sub$Sex == "Male"])

with(DGRP_sub, plot(x = prop_spider[Sex == "Female"],y = prop_spider[Sex == "Male"], xlab = "Females", ylab = "Males", abline(line2), main = "Male Female correlation: proportion with spider"))

#Variables for plots:
DGRP_dev <-DGRP_by_counts %>%
  group_by(DGRP, Sex) %>%
  summarise(Mean_prop_spi=mean(proportion_spider),se =((sd(proportion_spider))/sqrt(n())))

DGRP_dev$DGRP <- factor(DGRP_dev$DGRP, levels = DGRP_dev$DGRP[order(DGRP_dev$Mean_prop_spi)])

DGRP_dev_Male <- subset(DGRP_dev, Sex == "Male")
DGRP_dev_Male$DGRP <- factor(DGRP_dev_Male$DGRP, levels = DGRP_dev_Male$DGRP[order(DGRP_dev_Male$Mean_prop_spi)])
DGRP_by_countsMALE <- subset(DGRP_by_counts, Sex == "Male")


DGRP_dev_Female <- subset(DGRP_dev, Sex == "Female")
DGRP_dev_Female$DGRP <- factor(DGRP_dev_Female$DGRP, levels = DGRP_dev_Female$DGRP[order(DGRP_dev_Female$Mean_prop_spi)])
DGRP_by_countsFEMALE <- subset(DGRP_by_counts, Sex == "Female")




#Males
summary(DGRP_by_countsMALE$DGRP)

#Females
summary(DGRP_by_countsFEMALE$DGRP)



###Plots
ggplot(data=DGRP_dev, aes(x=DGRP, y=Mean_prop_spi, colour=Sex))+ 
  geom_point(size = 4) + geom_errorbar(aes(ymax = Mean_prop_spi + se, ymin = Mean_prop_spi - se, width=0.5)) +
  guides(colour=FALSE) +
  ggtitle("Proportion of flies with spider (with std Error)") +
  xlab("DGRP") + ylab("Proportion with spider") +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15)) +
  geom_hline(yintercept = 0.5, size=0.25) +
  theme(plot.title = element_text(size=22)) +
  theme(axis.text.x = element_text(angle=90, hjust = 1))


ggplot(data=DGRP_dev_Male, aes(x=DGRP, y=Mean_prop_spi, fill=Sex)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymax = Mean_prop_spi + se, ymin = Mean_prop_spi - se, width=0.5)) +
  ggtitle("Males") +
  xlab("DGRP") + ylab("Proportion with Spider") +
  scale_fill_manual(values=c("#00BFC4")) +
  guides(fill=FALSE, size=FALSE) +
  geom_hline(yintercept = 0.5, size=0.25) +
  theme(plot.title = element_text(size=22)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15)) +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

ggplot(data=DGRP_dev_Female, aes(x=DGRP, y=Mean_prop_spi, fill=Sex)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymax = Mean_prop_spi + se, ymin = Mean_prop_spi - se, width=0.5)) +
  ggtitle("Females") +
  xlab("DGRP") + ylab("Proportion with Spider") +
  guides(fill=FALSE, size=FALSE) +
  theme(plot.title = element_text(size=22)) +
  geom_hline(yintercept = 0.5, size=0.25) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15)) +
  theme(axis.text.x = element_text(angle=90, hjust = 1))  

#Create sum of all
DGRP_sum <- DGRP_by_counts %>%
  group_by(Sex) %>%
  summarise(Mean_prop_spi=mean(proportion_spider))

ggplot(data=DGRP_sum, aes(x=Sex, y=Mean_prop_spi)) + 
  geom_bar(stat="identity", position=position_dodge(), fill=c("#F8766D", "#00BFC4")) +
  ggtitle("Total population means") +
  ylim(0,1) +
  geom_hline(yintercept = 0.5) +
  xlab("Sex") + ylab("Proportion with Spider") +
  scale_fill_manual(values=c("#00BFC4")) +
  guides(fill=FALSE, size=FALSE, alpha=FALSE, colour=FALSE)


###Models:

mod1 <- glmer(cbind(Not_spider, Spider) ~ 1 + Sex + Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date) 
              + (0 + Sex|DGRP), data = DGRP_by_counts, family = "binomial")
summary(mod1)




mod2 <- lmer(proportion_spider ~ 1 + Sex + Temp_Scaled + Humidity_Scaled + BP_Scaled + (1|Date) 
             + (0 + Sex|DGRP), data = DGRP_by_counts)

summary(mod2)




mod3 <- lmer(proportion_spider ~ 1 + (1|Date) 
             + (1|DGRP), data = DGRP_by_counts)

summary(mod3)

mod4 <- lmer(Mean_prop_spi ~ 1 + Sex + Mean_temp_scaled + Mean_hum_scaled + Mean_BP_scaled + 
               (1|DGRP), data = DGRP_sub)
summary(mod4)


