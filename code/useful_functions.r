######################
## Useful Functions ##
######################

library(ggplot2)
library(plyr)
library(xtable)
library(maptools)
library(reshape2)



# Unifying character strings

UnifyNames <- function(x){
  x <- tolower(x)
  x <- gsub(' ' , '_' , x )
  x <- gsub('-' , '_' , x)
  x
}

# Formatting times properly

GetTime <- function(x){
  x <- as.character(x)
  x <- tolower(x)
  as.POSIXct(x , format = "%b %d, %Y %I:%M:%OS %p")
}


# Formatting dates properly

GetDate <- function(x){
  as.Date(as.character(x) , format = "%Y-%m-%d")
}


# Standardizing money values between FC and USD

CurrencyStandardization <- function(Value , Currency){
  ValueDollar <- Value
  ValueFC <- Value
  ValueDollar[Currency == 'fc'] <- Value[Currency == 'fc'] / 923
  ValueFC[Currency == 'usd'] <- Value[Currency == 'usd'] * 923
  StandardizedValue <- data.frame(ValueFC , ValueDollar)
  StandardizedValue
}

StandardizeOver <- function(data , Value , Currency , Category){
  standard <- CurrencyStandardization(data[,Value] , data[,Currency])
  varNameDollar <- paste(Category , 'Dollar' , sep = '')
  varNameFC <- paste(Category , 'FC', sep = '')
  data[,varNameDollar] <- standard$ValueDollar
  data[,varNameFC] <- standard$ValueFC
  data
}


##Output Functions

output.table <- function(tab , name){
  adress <- paste('output/tables/' , name , '.csv' ,  sep = '')
  write.csv(tab , adress)
}




##Data Environments loads



if(stage == 'select'){
  indiv <- read.csv('data/questionnaires_init/questionnaire_individuel_final.csv'  , as.is = TRUE)
  facilities <- read.csv('data/questionnaires_init/questionnaire_cs.csv' , as.is = TRUE)
  loop_appui_zs <- read.csv('data/questionnaires_init/questionnaire_cs_LoopZSAppui.csv' , as.is = TRUE)
  loop_appui_fac <- read.csv('data/questionnaires_init/questionnaire_cs_FacSupportGroup.csv' , as.is = TRUE)
  loop_activ_non_sante <-read.csv('data/questionnaires_init/questionnaire_individuel_final_ActNonSanteGroup.csv' , as.is = TRUE)
  loop_activ_privee <-read.csv('data/questionnaires_init/questionnaire_individuel_final_ActPriveeGroup.csv' , as.is = TRUE)
  loop_autre_revenu <-read.csv('data/questionnaires_init/questionnaire_individuel_final_AutreRevenuGroup.csv' , as.is = TRUE)
  loop_perdiem <-read.csv('data/questionnaires_init/questionnaire_individuel_final_GroupPerDiem.csv' , as.is = TRUE)
  loop_prime_partenaire <-read.csv('data/questionnaires_init/questionnaire_individuel_final_PrimesPartenairesGroup.csv', as.is = TRUE)
  
}

if(stage == 'cleaning'){ 
  indiv <- read.csv('data/questionnaires_temp/individual_data_select.csv'  , as.is = TRUE)
  facilities <- read.csv('data/questionnaires_temp/facility_data_select.csv' , as.is = TRUE)
  loop_appui_zs <- read.csv('data/questionnaires_temp/loop_appui_zs_select.csv' , as.is = TRUE)
  loop_appui_fac <- read.csv('data/questionnaires_temp/loop_appui_fac_select.csv' , as.is = TRUE)
  loop_activ_non_sante <-read.csv('data/questionnaires_temp/loop_activ_non_sante_select.csv' , as.is = TRUE)
  loop_activ_privee <-read.csv('data/questionnaires_temp/loop_activ_privee_select.csv' , as.is = TRUE)
  loop_autre_revenu <-read.csv('data/questionnaires_temp/loop_autre_revenu_select.csv' , as.is = TRUE)
  loop_perdiem <-read.csv('data/questionnaires_temp/loop_perdiem_select.csv' , as.is = TRUE)
  loop_prime_partenaire <-read.csv('data/questionnaires_temp/loop_prime_partenaire_select.csv', as.is = TRUE)  
}


if(stage == 'analysis'){
  indiv <- read.csv('data/questionnaires_analysis/individual_data_select.csv'  , as.is = TRUE)
  facilities <- read.csv('data/questionnaires_analysis/facility_data_select.csv' , as.is = TRUE)
  loop_appui_zs <- read.csv('data/questionnaires_analysis/loop_appui_zs_select.csv' , as.is = TRUE)
  loop_appui_fac <- read.csv('data/questionnaires_analysis/loop_appui_fac_select.csv' , as.is = TRUE)
  loop_activ_non_sante <-read.csv('data/questionnaires_analysis/loop_activ_non_sante_select.csv' , as.is = TRUE)
  loop_activ_privee <-read.csv('data/questionnaires_analysis/loop_activ_privee_select.csv' , as.is = TRUE)
  loop_autre_revenu <-read.csv('data/questionnaires_analysis/loop_autre_revenu_select.csv' , as.is = TRUE)
  loop_perdiem <-read.csv('data/questionnaires_analysis/loop_perdiem_select.csv' , as.is = TRUE)
  loop_prime_partenaire <-read.csv('data/questionnaires_analysis/loop_prime_partenaire_select.csv', as.is = TRUE)  
}