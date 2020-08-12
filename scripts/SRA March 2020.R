## SRA CODING ##
## MARCH 4th 2020 ##
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

#Suppress scientific notation
options(scipen = 999)

# Load Data
Checklist_Data_summed_variables_selfreport <- read_sav("OneDrive - McGill University/Research/Daily Diary/Data/Checklist Data_summed variables_selfreport.sav")
View(Checklist_Data_summed_variables_selfreport)

DD.Data<-Checklist_Data_summed_variables_selfreport

colnames(DD.Data)

DD.Data<-DD.Data %>% replace_with_na_all(condition = ~.x %in% c(999, 996))

#Make new columns: mean positive, mean negative, total daily fi, dummy school,
DD.Data$posmean <- rowMeans(DD.Data[,c("PANAS_Joyful", "PANAS_Lively", "PANAS_Cheerful", "PANAS_Proud", "PANAS_Happy")], na.rm=TRUE)

DD.Data$negmean <- rowMeans(DD.Data[,c("PANAS_Afraid", "PANAS_Embarrassed", "PANAS_Miserable", "PANAS_Scared", "PANAS_Mad", "PANAS_Sad", "PANAS_Ashamed" )], na.rm=TRUE)

DD.Data$dailyFI <- rowSums(DD.Data[,c("Food_Sec_DailyBreakfast", "Food_Sec_DailyLunch", "Food_Sec_DailyHunger")])

DD.Data$school<- as.integer(grepl(pattern = "^1", x = df$ID))


describe(DD.Data$negmean)
describe (DD.Data$posmean)
describe(DD.Data$dailyFI)
describe(DD.Data$school)

# Subset to main variables
df<- DD.Data[,c("ID", "Day", "Gender", "Age", "negmean", "posmean", "PANAS_Miserable", "PANAS_Mad", "PANAS_Afraid",  "PANAS_Scared", "PANAS_Sad", "PANAS_Joyful", "PANAS_Cheerful", "PANAS_Happy", "PANAS_Lively", "PANAS_Proud", "PANAS_Ashamed", "PANAS_Embarrassed", 
                "Food_Sec_DailyBreakfast", "Food_Sec_DailyLunch", "Food_Sec_DailyHunger", "dailyFI","SES_Vehicle", 
                "SES_OwnBedrm", "SES_Comp", "SES_Bathrms", "SES_Dishwasher", "SES_Travel",  "Food_GoHungry", "Food_MontlyHunger", "Dummy_Gender" , "SES_sum", "school")]

View(df)
DD<-DD.Data[,c("ID", "Day",  "Age", "negmean", "posmean",  
               "Food_Sec_DailyBreakfast", "Food_Sec_DailyLunch", "Food_Sec_DailyHunger", "dailyFI", "Food_GoHungry", "Food_MontlyHunger", "Dummy_Gender" , "SES_sum", "school")]

# View missing data
md.pattern(DD)
# Delete completely missing days
DD.1 <- DD[!with(DD,is.na(negmean) & is.na(posmean) & is.na(Food_Sec_DailyBreakfast) & is.na(Food_Sec_DailyLunch) & is.na(Food_Sec_DailyHunger)),]
md.pattern(DD.1)
# Delete completely missing initial questionnaires
DD.2 <- DD.1[!with(DD.1, is.na(Age) & is.na(Dummy_Gender) & is.na(SES_sum) & is.na(Food_GoHungry) & is.na(Food_MontlyHunger)),]
md.pattern(DD.2)

#Listwise deletion for NA on categorical variables
DD.3 <- DD.2 %>% drop_na(Food_Sec_DailyBreakfast, Food_Sec_DailyLunch, Food_Sec_DailyHunger)
md.pattern(DD.3)

View(DD.3)

#Imputation
DD.imp<- mice(DD.3, m=5, maxit=50, meth='pmm', seed=500)
plot(DD.imp, c("negmean", "posmean", "Age"))
densityplot(DD.imp)


imp.check<-mice.mids(DD.imp, maxit = 20, print = FALSE)
plot(imp.check, c("negmean", "posmean", "Age"))
densityplot(imp)

#Descriptives
summary(complete (DD.imp))

describe(DD.3)
table(DD.3$Dummy_Gender)
table(DD.3$Age)
table(DD.3$SES_sum)
table(DD.3$Food_Sec_DailyBreakfast)
table(DD.3$Food_Sec_DailyLunch)
table(DD.3$Food_Sec_DailyHunger)

DD.3[DD.3$Food_Sec_DailyBreakfast == "0", c("ID")]
filter(DD.3, Food_Sec_DailyBreakfast == "0")

test.bf<-subset(DD.3, Food_Sec_DailyBreakfast == 0)
View(test.bf)
table(test.bf$ID)

test.lu<-subset(DD.3, Food_Sec_DailyLunch == 0)
table(test.lu$ID)

test.hu<-subset(DD.3, Food_Sec_DailyHunger == 1)
table(test.hu$ID)

test.g <- subset(DD.3, Dummy_Gender == 1)
table(test.g$ID)

table(DD.3$ID)

apa.cor.table(DD.3, filename = "DDMay10", table.number=1)

#Subset on gender
DD.g <- subset(DD.3, Dummy_Gender == 1)
DD.b <- subset(DD.3, Dummy_Gender == 0)

describe(DD.g)
describe(DD.b)

####Models#######

##Negative Mood##
NegMood <-lmer(negmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + school + SES_sum + (1|ID), data=DD.imp)
summary(NegMood)
tab_model(NegMood)


NegMood.imp <-with(DD.imp, lmer(negmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + school + SES_sum + (1|ID)))
summary(est <- pool(NegMood.imp))
tab_model(NegMood)

fm1 <- with(DD.imp, lmer(log(wage) ~ gender + age + age_sqr + occupation + degree + private_sector + overtime + (1+gender|faculty))) #my multilevel model
summary(est <- pool(fm1)) #pool my results

###Examples####
#Negative mood#
NegMood <-lmer(negmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + school + SES_sum + (1|ID), data=DD.3)
summary(NegMood)
tab_model(NegMood)

#Positive mood
PosMood<-lmer(posmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + school + SES_sum + (1|ID), data=DD.3)
summary(PosMood)
tab_model(PosMood)

#table of neg and pos
tab_model(NegMood, PosMood)

#Neg with gender interaction
NegMood.int<-lmer(negmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + Food_Sec_DailyBreakfast*Dummy_Gender + Food_Sec_DailyLunch*Dummy_Gender + Food_Sec_DailyHunger*Dummy_Gender + school + SES_sum + (1|ID), data=DD.3)
tab_model(NegMood.int)

#Neg Boys
NegBoys <-lmer(negmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + school + SES_sum + (1|ID), data=DD.b)
tab_model(NegBoys)

#Neg Girls
NegGirls <- lmer(negmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + school + SES_sum + (1|ID), data=DD.g)
tab_model(NegGirls)

#Table together
tab_model(NegBoys, NegGirls)


#Positive with interactions
PosMood.int<-lmer(posmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + Food_Sec_DailyBreakfast*Dummy_Gender + Food_Sec_DailyLunch*Dummy_Gender + Food_Sec_DailyHunger*Dummy_Gender + school + SES_sum + (1|ID), data=DD.3)
tab_model(PosMood.int)

PosBoys <-lmer(posmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + school + SES_sum + (1|ID), data=DD.b)
tab_model(PosBoys)

PosGirls <- lmer(posmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + school + SES_sum + (1|ID), data=DD.g)
tab_model(PosGirls)

tab_model(PosBoys, PosGirls)


#### Simple ####

NegMood.test <-lmer(negmean ~ 1 + Food_Sec_DailyHunger + (1|ID), data=DD.Temp)
summary(NegMood.test)
tab_model(NegMood.test)

ash.mod <- lmer(PANAS_Ashamed ~ 1 + Food_Sec_DailyHunger + (1|ID), data=DD.Temp)
tab_model(ash.mod)

sad.mod <-lmer(PANAS_Sad ~ 1 + Food_Sec_DailyHunger + (1|ID), data=DD.Temp)
tab_model(sad.mod)

### MODEL 1: Negative Mood ~ Hunger, Breakfast, Lunch, Gender, SES, Age, School ###

NegMood <-lmer(negmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + school + (1|ID), data=DD.Temp)
summary(NegMood)
tab_model(NegMood)

NegMood.1 <-lmer(negmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Dummy_Gender + SES_sum + school + (1|ID), data=DD.Temp)
summary(NegMood.1)
tab_model(NegMood.1)

NegMood.2<-lmer(negmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Food_Sec_DailyHunger*Food_Sec_DailyBreakfast + (1|ID), data=DD.Temp)
summary(NegMood.2)
tab_model(NegMood.2)


### MODEL 2: Positive Mood ~ Hunger, Breakfast, Lunch, Gender, SES, Age, School ###
PosMood <-lmer(posmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + SES_sum + Age + school + (1|ID), data=DD.Temp)
summary(PosMood)
tab_model(PosMood)

PosMood<-lmer(posmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + school + SES_sum + (1|ID), data=DD.Temp)
summary(PosMood)
tab_model(PosMood)


#Extra
#Dummy school
# put as a number ( search for (all those = starts by 1, from data = dataset$column))
df$school <- as.integer(grepl(pattern = "^1", x = df$ID))
describe(df$school)

# Quick fix for now: listwise deletion
DD.Temp <- na.omit(DD.Final)
View(DD.Temp)
describe(DD.Temp)

#mice stuff

imp.check<-mice.mids(imp, maxit = 20, print = FALSE)
plot(imp.check, c("negmean", "posmean", "Age"))
densityplot(imp)

impl <- mice(DD.3, meth=meth, pred=pred, print = FALSE)

#compare 
summary(complete (impl))
summary(DD.3)


predM = init$predictorMatrix
predM[c("ID")]=0

#specify method
ini <- mice(DD.3, maxit=0)
meth <- ini$meth
meth 

#use "norm" rather than pmm for the method
meth[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)] <- "norm"
meth

# exclude ID and day as predictors
pred <- ini$pred
pred

pred[, "ID"] <- 0
pred[, "Day"] <- 0
pred

#interaction ses test
NegMood.ses<-lmer(negmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + Food_Sec_DailyBreakfast*Dummy_Gender + Food_Sec_DailyLunch*Dummy_Gender + Food_Sec_DailyHunger*Dummy_Gender + SES_sum*Dummy_Gender + school + SES_sum + (1|ID), data=DD.3)
tab_model(NegMood.ses)

PosMood.ses<-lmer(posmean ~ 1 + Food_Sec_DailyBreakfast + Food_Sec_DailyLunch + Food_Sec_DailyHunger + Dummy_Gender + Food_Sec_DailyBreakfast*Dummy_Gender + Food_Sec_DailyLunch*Dummy_Gender + Food_Sec_DailyHunger*Dummy_Gender + SES_sum*Dummy_Gender + school + SES_sum + (1|ID), data=DD.3)
tab_model(PosMood.ses)


##testing##
describe(DD.Temp$Food_Sec_DailyHunger)
hunger.table <- table(DD.Temp$Food_Sec_DailyHunger)
hunger.table

breakfast.table <- table(DD.Temp$Food_Sec_DailyBreakfast)
breakfast.table

lunch.table <- table(DD.Temp$Food_Sec_DailyLunch)
lunch.table