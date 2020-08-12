##SRA CODING##
##September 6th 2019
##Allison MacNeil

# Load Packages
library(psych)
library(haven)
library(dplyr)
library(tidyr)
library(lme4)
library(readr)
library(mice)
library(coefplot)
library(sjPlot)
library(nlme)
library(stargazer)
library(fastDummies)
library(data.table)
library(naniar)

# Import Data
Checklist_Data_summed_variables_selfreport <- read_sav("OneDrive - McGill University/Research/Daily Diary/Data/Checklist Data_summed variables_selfreport.sav")
View(Checklist_Data_summed_variables_selfreport)

#Dataframe
df <- Checklist_Data_summed_variables_selfreport

#Check column names
colnames(df)

#fix 996
df <- df %>% replace_with_na_all(condition = ~.x == 996)
View(df)

#Make mean of positive & negativePANAS
df$posmean <- rowMeans(df[,c("PANAS_Joyful", "PANAS_Lively", "PANAS_Cheerful", "PANAS_Proud", "PANAS_Happy")], na.rm=TRUE)
View(df)

df$negmean <- rowMeans(df[,c("PANAS_Afraid", "PANAS_Embarrassed", "PANAS_Miserable", "PANAS_Scared", "PANAS_Mad", "PANAS_Sad", "PANAS_Ashamed" )], na.rm=TRUE)
colnames(df)

df$FI <- rowSums(df[,c("Food_Security_1", "Food_Security_2")], na.rm=TRUE)

describe(df$posmean)
describe(df$negmean)
describe(df$FI)


#Dummy gender
df <- fastDummies::dummy_cols(df, select_columns = "Gender")
describe(df$Gender_Girl)

#Dummy school
# put as a number ( search for (all those = starts by 1, from data = dataset$column))
df$school <- as.integer(grepl(pattern = "^1", x = df$ID))
describe(df$school)

describe(df$Food_Security_1)

#Suppress scientific notation
options(scipen = 999)

#Create subset
DD <- df[,c("ID","Gender_Girl", "school", "Food_Security_1", "Food_Security_2", "Food_Security_3", "negmean", "posmean", "PANAS_Ashamed", "PANAS_Embarrassed", "FI", "Age")]
View(DD)


#Remove NA
DD.final <- na.omit(DD)
View(DD.final)

describe(DD.final)


#FINAL
MODEL.A <-lmer(negmean ~ 1 + Food_Security_1 + Food_Security_2 + Gender_Girl + school +(1|ID), data=DD.final)
summary(MODEL.A)
tab_model(MODEL.A)

MODEL.B <- lmer(posmean ~ 1 + Food_Security_1 + Food_Security_2 + Gender_Girl + school +(1|ID), data=DD.final)
tab_model(MODEL.B)

tab_model(MODEL.A, MODEL.B)

MODEL.AI <-lmer(negmean ~ 1 + Food_Security_1 + Food_Security_2 + Gender_Girl + school + Food_Security_1*Gender_Girl + Food_Security_2*Gender_Girl +(1|ID), data=DD.final)
summary(MODEL.AI)
tab_model(MODEL.AI)

MODEL.BI <-lmer(posmean ~ 1 + Food_Security_1 + Food_Security_2 + Gender_Girl + school + Food_Security_1*Gender_Girl + Food_Security_2*Gender_Girl +(1|
                                                                                                                                                       ), data=DD.final)
summary(MODEL.BI)
tab_model(MODEL.BI)

tab_model(MODEL.AI, MODEL.BI)

##Attempt at 2 outcomes
length(unique(unlist(DD.final[c("ID")])))

DV <- cbind(DD.final$posmean, DD.final$negmean)
print(DV)

library(MCMCglmm)


model <- MCMCglmm(cbind(posmean, negmean)~Food_Security_1 + Food_Security_2 + Gender_Girl + school,  data=DD.final)

model <- MCMCglmm(cbind(posmean, negmean)~trait:Food_Security_1 + trait:Food_Security_2 + trait:Gender_Girl + trait:school -1, random=~us(trait):ID,  rcov = ~ us(trait):units, rep("gaussian", 2) ,data=DD.final)

summary(model)
tab_model(model)

#TRIALS - DO NOT RUN

length(unique(unlist(DD.final[c("ID")])))

DV <- cbind(DD.final$posmean, DD.final$negmean)
print(DV)

M.test<-lmer(Food_Security_2 ~ 1 + Gender_Girl + (1|ID), data=DD.final)
tab_model(M.test)

#Model 1: negmean
MODEL.1 <-lmer(negmean ~ 1 + Food_Security_1 + Food_Security_2 + (1|ID), data=DD.final)
summary(MODEL.1)
tab_model(MODEL.1)

MODEL.1.1 <-lmer(negmean ~ 1 + FI + (1|ID), data=DD.final)
summary(MODEL.1.1)
tab_model(MODEL.1.1)

MODEL.1.2 <-lmer(negmean ~ 1 + FI + Gender_Girl + Gender_Girl*FI + (1 |ID), data=DD.final)
summary(MODEL.1.2)
tab_model(MODEL.1.2)

MODEL.1.3 <-lmer(negmean ~ 1 + FI + school + Gender_Girl + Gender_Girl*FI + (1 |ID), data=DD.final)
summary(MODEL.1.3)
tab_model(MODEL.1.3)

MODEL.1.4 <-lmer(negmean ~ 1 + Food_Security_1 + Gender_Girl + Gender_Girl*Food_Security_1 + (1 |ID), data=DD.final)
summary(MODEL.1.4)
tab_model(MODEL.1.4)

MODEL.1.5 <-lmer(negmean ~ 1 + Food_Security_2 + Gender_Girl + Gender_Girl*Food_Security_2 + (1 |ID), data=DD.final)
summary(MODEL.1.5)
tab_model(MODEL.1.5)

MODEL.1.6 <-lmer(negmean ~ 1 + Food_Security_2 + Food_Security_1 + Gender_Girl + Gender_Girl*Food_Security_2 + Gender_Girl*Food_Security_1 + (1 |ID), data=DD.final)
summary(MODEL.1.6)
tab_model(MODEL.1.6)

#Model 2: posmean
MODEL.2<-lmer(posmean ~ 1 + Food_Security_1 + Food_Security_2 + (1|ID), data=DD.final)
summary(MODEL.2)
tab_model(MODEL.2)

MODEL.2.1<-lmer(posmean ~ 1 + Food_Security_1 + Food_Security_2 + Gender_Girl + Gender_Girl*Food_Security_2 + Gender_Girl*Food_Security_1 + school + (1 |ID), data=DD.final)
summary(MODEL.2.1)
tab_model(MODEL.2.1)


#Model 3: embarrassed
MODEL.3<-lmer(PANAS_Embarrassed ~ 1 + Food_Security_1 + Food_Security_2 + (1|ID), data=DD.final)
summary(MODEL.3)
tab_model(MODEL.3)

MODEL.3.1<-lmer(PANAS_Embarrassed ~ 1 + FI + (1|ID), data=DD.final)
summary(MODEL.3.1)
tab_model(MODEL.3.1)

MODEL.3.2<-lmer(PANAS_Embarrassed ~ 1 + FI + school + (1|ID), data=DD.final)
summary(MODEL.3.2)
tab_model(MODEL.3.2)
#school not significant

MODEL.3.3<-lmer(PANAS_Embarrassed ~ 1 + FI + Gender_Girl + Gender_Girl*FI + (1|ID), data=DD.final)
summary(MODEL.3.3)
tab_model(MODEL.3.3)

MODEL.3.4<-lmer(PANAS_Embarrassed ~ 1 + Food_Security_1 + Gender_Girl + Gender_Girl*Food_Security_1 + (1|ID), data=DD.final)
summary(MODEL.3.4)
tab_model(MODEL.3.4)




