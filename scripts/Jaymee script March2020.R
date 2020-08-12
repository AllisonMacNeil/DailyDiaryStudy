## JAYMEE CODING ##
## MARCH 10th 2020 ##
## Allison MacNeil ##

#Load PAckages
library(psych)
library(lavaan)
library(haven)
library(dplyr)
library(tidyr)
library(lme4)
library(readr)
library(mice)
library(apaTables)
library(coefplot)
library(sjPlot)
library(nlme)
library(stargazer)
library(survey) #for stratification
library(reghelper) #for simp
library(naniar) #for 996
library(plyr) #renaming column

# Load Data
Checklist_Data_summed_variables_selfreport_Allison_s_MacBook_Air <- read_sav("OneDrive - McGill University/Research/Daily Diary/Data/Checklist Data_summed variables_selfreport-Allison’s MacBook Air.sav")

##########   INITIAL   #######################
#Day 1 subset of data
Day1 <- subset(Checklist_Data_summed_variables_selfreport_Allison_s_MacBook_Air, Day == 1)
View(Day1)

# 999 & 996 to NA
#fix 996
Day1 <- Day1 %>% replace_with_na_all(condition = ~.x %in% c(999, 996))
View(Day1)

#Mean of negative peer experiences, new column
Day1$PeerNeg <- rowMeans(Day1[,c("PeerExp_LeftOutAct", "PeerExp_Chased", "PeerExp_Avoided", "PeerExp_Rumours", "PeerExp_NotInvited", "PeerExp_LeftOut", "PeerExp_NotMyFriend", "PeerExp_Gossiped", "PeerExp_Threatened", "PeerExp_SilentTreatmnt", "PeerExp_SaidMeanThngs", "PeerExp_HitKickPush", "PeerExp_Teased")], na.rm=TRUE)
describe(Day1$PeerNeg)


#Descriptive Statistics
describe(Day1$Age)
describe(Day1$Dummy_Gender)
describe(Day1$SES_sum)
describe(Day1$Food_GoHungry)
describe (Day1$Food_MontlyHunger)
describe(Day1$PeerNeg)

# Regression #
Model1 <- lm(PeerNeg ~ Food_GoHungry + Food_MontlyHunger + SES_sum + Dummy_Gender, data = Day1)
summary(Model1)
tab_model(Model1)

# Test interaction #
Model1.1 <- lm(PeerNeg ~ Food_GoHungry + Food_GoHungry*Dummy_Gender + Food_MontlyHunger + SES_sum + Dummy_Gender, data = Day1)
summary(Model1.1)
tab_model(Model1.1)

Model1.2 <- lm(PeerNeg ~ Food_GoHungry + Food_MontlyHunger*Dummy_Gender + Food_MontlyHunger + SES_sum + Dummy_Gender, data = Day1)
summary(Model1.2)
tab_model(Model1.2)

Model1.3 <- lm(PeerNeg ~ Food_GoHungry + SES_sum*Dummy_Gender + Food_MontlyHunger + SES_sum + Dummy_Gender, data = Day1)
summary(Model1.3)
tab_model(Model1.3)

# Divide by gender #

Day1.Girl <- subset(Day1, Dummy_Gender == 1)
describe(Day1.Girl$Dummy_Gender)

Day1.Boy<- subset(Day1, Dummy_Gender == 0)
describe(Day1.Boy$Dummy_Gender)

# Regression Girls #
Model1.Girls <- lm(PeerNeg ~ Food_GoHungry + Food_MontlyHunger + SES_sum, data = Day1.Girl)
summary(Model1.Girls)
tab_model(Model1.Girls)

# Regression Boys #
Model1.Boy <- lm(PeerNeg ~ Food_GoHungry + Food_MontlyHunger + SES_sum, data = Day1.Boy)
summary(Model1.Boy)
tab_model(Model1.Boy)

######################## Daily #####################

Daily <- Checklist_Data_summed_variables_selfreport_Allison_s_MacBook_Air

### Descriptives 
describe(Daily$Food_Sec_DailyBreakfast)
describe(Daily$Food_Sec_DailyLunch)
describe(Daily$Food_Sec_DailyHunger)
describe(Daily$mean_sum)

# MLM ###
Model2<-lmer(mean_sum ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + SES_sum + Dummy_Gender + (1|ID), data=Daily)
tab_model(Model2)

#NOTE: Nothing significant.


###EXTRA####
Model2.1<-lmer(mean_sum ~ 1 + Food_Sec_DailyBreakfast*Dummy_Gender +Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + SES_sum + Dummy_Gender + (1|ID), data=Daily)
tab_model(Model2.1)

Model2.2<-lmer(mean_sumA ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + SES_sum + Dummy_Gender + (1|ID), data=Daily)
tab_model(Model2.2)

##EXTRA###
write_sav(Day1, "OneDrive - McGill University/Research/Daily Diary/Data/Daily_Diary_Day1.sav", compress=FALSE)


write_sav(data, path, compress = FALSE)

