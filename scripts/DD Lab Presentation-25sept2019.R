## Daily Diary Lab Presentation
## September 25th 2019
## Allison MacNeil


# Load Packages
library(psych)
library(dplyr)
library(tidyr)
library(lme4)
library(readr)
library(mice) #missing data
library(sjPlot) #generate tables
library(nlme)
library(stargazer)
library(fastDummies) #dummy variables
library(data.table) #generate tables
library(naniar)

#Suppress scientific notation
options(scipen = 999)

# Import Data
Checklist_Data_summed_variables_selfreport <- read_sav("OneDrive - McGill University/Research/Daily Diary/Checklist Data_summed variables_selfreport.sav")
View(Checklist_Data_summed_variables_selfreport)

#Dataframe
df <- Checklist_Data_summed_variables_selfreport

#fix 996
df <- df %>% replace_with_na_all(condition = ~.x == 996)

#Make mean of positive & negativePANAS
df$posmean <- rowMeans(df[,c("PANAS_Joyful", "PANAS_Lively", "PANAS_Cheerful", "PANAS_Proud", "PANAS_Happy")], na.rm=TRUE)
df$negmean <- rowMeans(df[,c("PANAS_Afraid", "PANAS_Embarrassed", "PANAS_Miserable", "PANAS_Scared", "PANAS_Mad", "PANAS_Sad", "PANAS_Ashamed" )], na.rm=TRUE)

#Dummy gender
df <- fastDummies::dummy_cols(df, select_columns = "Gender")

#Dummy school
df$school <- as.integer(grepl(pattern = "^1", x = df$ID))

#Impute missing data

#1 look at missing pattern
md.pattern(df)
#2 Impute
imputed_df <- mice(df, m=5, maxit=50, method="pmm", seed=500)
summary(imputed_df)


#Cut down dataset
#THIS FILE HAS BEEN CHANGED

