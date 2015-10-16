setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'modelisation'

source('code/useful_functions.r')



####### Recoding Education #######
total_revenu$LastEducation <- total_revenu$LastEduc
total_revenu$LastEducation [total_revenu$LastEducation  %in%
                              c('medecin_generaliste' , 'medecin_specialiste' , 
                                'medecin_generaliste medecin_specialiste' ,
                                'pharmacien' , 'diplome_etudes_superieures')] <- 'medecin-pharma-etudesup'

total_revenu$LastEducation[total_revenu$LastEducation %in% 
                             c('gestion_administration' , 'gestion_administration autre'  , 
                               'autre')] <- 'autre'

total_revenu$LastEducation[total_revenu$LastEducation %in% 
                             c('infirmiere_ao' , 'infirmiere_a1' ,
                               'technicien_labo' , 'infirmiere_a2' )] <- 'a0-a1-a2'

total_revenu$LastEducation[total_revenu$LastEducation %in% 
                             c('infirmiere_a3')] <- 'a3'

total_revenu$LastEducation[total_revenu$LastEducation %in%c('')] <- NA


##### Denoting power positions####
total_revenu$Power[total_revenu$RoleInit %in% c('directeur_nursing' , 
                                                'medecin_chef_staff' ,
                                                'medecin_directeur')] <- 1
total_revenu$Power[total_revenu$RoleInit %in% c('medecin','infirmier_titulaire' ,
                                                'medecin_directeur')] <- 1

total_revenu$Power[is.na(total_revenu$Power)] <- 0
total_revenu$Role[total_revenu$Role == ''] <- NA



#### Denoting Motivation schemes ####
total_revenu$FacMotivation <- (total_revenu$NAppuiMotivZs > 0 | total_revenu$NAppuiMotivFac > 1 )
total_revenu$FacMotivation[is.na(total_revenu$FacMotivation)] <- FALSE

orderingIncome <- c('Salaire' , 'Prime de Risque' , 'Prime de Partenaire' , 
                    'Per Diem' , 'Prime Locale' , 'Heures supplémentaires' ,
                    'Activité  Privée' , 'Activité non santé' , 'Informel')

# Keep only relevant data
total_revenu <- subset(total_revenu , Role != '' & variable != "Autres revenus")

######## Some data management that has never been put before... #####
total_revenu$FacLevel[total_revenu$Role %in% c('administrateur_gestionnaire' ,
                                               'infirmier_superviseur' , 
                                               'medecin_chef_zone')] <- 'ecz'
total_revenu$Role[total_revenu$Role  == 'infirmier' &
                    total_revenu$FacLevel == 'ecz'] <- 'infirmier_superviseur' 
total_revenu$Role[total_revenu$Role == 'medecin' &
                    total_revenu$FacLevel == 'ecz'] <- 'medecin_chef_zone' 

#total_revenu$FacLevel[total_revenu$Role %in% c('medecin' , 'infirmier') & 
#                        total_revenu$FacLevel == 'ecz'] <- NA

total_revenu <- subset(total_revenu , !is.na(FacLevel))

# Recode the type of structure to conduct to separate analysis
total_revenu$FacSimple <- 'facility'
total_revenu$FacSimple[total_revenu$FacLevel == 'ecz'] <- 'ecz'

# Group informel revenues
total_revenu$variable[total_revenu$variable %in% 
                        c("Vente de Medicament" , "Cadeau")] <- 'Informel'

total_revenu$RevenueEntry <- total_revenu$variable

## Taking out people who only get income from informal sources
percent_revenu <- function(data , total){
  sum(data$value) / total
}

distrib_data <- ddply(total_revenu , .(Role, instanceID) , 
                      function(data){
                        total <- sum(data$value)
                        out <- ddply(data , .(RevenueEntry) ,
                                     function(x) percent_revenu(x , total))
                        out
                      })

exclude <- distrib_data$instanceID[distrib_data$RevenueEntry == 'Informel' & 
                                     distrib_data$V1 >= 1]

total_revenu <- subset(total_revenu , !(instanceID %in% exclude))

distrib_data <- ddply(total_revenu , .(Role, instanceID) , 
                      function(data){
                        total <- sum(data$value)
                        out <- ddply(data , .(RevenueEntry) ,
                                     function(x) percent_revenu(x , total))
                        out
                      })


######## Collapsing revenues by individual #############

# Computing total incomes by income type and total
RevSum <- ddply(total_revenu , .(instanceID , variable , Role , FacilityType, FacSimple) ,  
                function(x) sum(x$value))
RevTot <- ddply(total_revenu , .(instanceID , FacSimple , Role) ,  
                function(x) sum(x$value, na.rm = TRUE))

ddply(RevTot , .(Role) , function(x) median(x$V1))
ddply(RevTot , .(Role) , function(x) max(x$V1))

  ######## Setting orderings for plotting categories ####

# For plotting purposes, Ordering Individuals by total income
ordRev <- RevTot$instanceID[order(RevTot$V1)]
RevSum$instanceID <- factor(RevSum$instanceID , levels =  ordRev , ordered = TRUE)

# Declaring the ordering of revenues in the graphs
orderingIncome <- c('Salaire' , 'Prime de Risque' , 'Prime de Partenaire' , 'Per Diem' , 
                    'Prime Locale' , 'Heures supplémentaires' , 'Activité  Privée' , 
                    'Activité non santé' , "Informel")

# Ordering role and revenue type for plotting purpose
RevSum$Role <- factor(as.character(RevSum$Role) , levels = rev(ordering_staff) , ordered = TRUE)

RevSum$variable <- as.character(RevSum$variable)
RevSum$variable <- factor(RevSum$variable , levels = rev(orderingIncome) , ordered = TRUE)

######## PLOT1 : General plotting of amounts earned ####

pdf('article/distrib_revenus.pdf' , width = 14)
qplot(data = RevSum[RevSum$FacSimple == 'facility' ,] , 
      x = instanceID , y = V1 , geom = 'bar' , 
      stat = 'identity' , width = 1 , fill = variable)+
  theme(axis.text.x = element_blank()) + 
  facet_wrap(~Role , scales = 'free_x') + 
  ylab('Income') + xlab('') + 
  scale_fill_brewer(palette="Set1") 

qplot(data = RevSum[RevSum$FacSimple == 'facility' ,],
      x = instanceID , y = V1 , geom = 'bar' , 
      stat = 'identity' , width = 1 , fill = variable)+
  theme(axis.text.x = element_blank()) + 
  facet_wrap(~Role , scales = 'free') + 
  ylab('Income') + xlab('') + 
  scale_fill_brewer(palette="Set1") 
dev.off()

######## Table1 : % of individuals receiving income by role structure and income type ####
N_rev <- ddply(total_revenu , .(Role , FacSimple , RevenueEntry) ,
               function(x) length(unique(x$instanceID)))
N_Indiv <- ddply(total_revenu , .(Role , FacSimple) ,
               function(x) length(unique(x$instanceID)))

N_received <- merge(N_rev , N_Indiv , by = c('Role' , 'FacSimple') , 
                    suffixes = c('get' , 'total') , all = T)

N_received$percentage <- N_received$V1get / N_received$V1total

######## Table2 : % of individuals receiving income by type of income ####
N_rev <- ddply(total_revenu , .(RevenueEntry) ,
               function(x) length(unique(x$instanceID)))
N_Indiv <- length(unique(total_revenu$instanceID))

N_rev$percentage <- N_rev$V1 / N_Indiv

######## Table3 : % of income from each type of income by role and structure ####
N_rev <- ddply(total_revenu , .(Role , FacSimple , RevenueEntry) ,
               function(x) sum(x$value))
N_Indiv <- ddply(total_revenu , .(Role , FacSimple) ,
                 function(x) sum(x$value))

N_received_3 <- merge(N_rev , N_Indiv , by = c('FacSimple' , 'Role') , 
                    suffixes = c('get' , 'total') , all = T)

N_received_3$percentage <- N_received_3$V1get / N_received_3$V1total

sal_pdr <- ddply(N_received_3 , .(FacSimple , Role) ,
             function(x) sum(x$percentage[x$RevenueEntry %in% c('Salaire' , 
                                                                'Prime de Risque')]) )

N_received_3[N_received_3$RevenueEntry == 'Prime Locale' ,]
top_perdiem <- ddply(N_received_3 , .(FacSimple , Role) ,
                 function(x) sum(x$percentage[x$RevenueEntry %in% c('Prime de Partenaire', 
                                                                    'Per Diem')]) )

######## Table4 : Idem on distribution of median ####
#distrib_data$RevenueEntry <- factor(distrib_data$RevenueEntry ,
#                                    levels = orderingIncome , 
#                                    ordered = TRUE )


## Function to get the median of income for each Role
revenue_median <- ddply(total_revenu , .(Role) , 
                        function(x){
                          total <- ddply(x , .(instanceID),
                                         function(x){
                                           sum(x$value)
                                         }
                          )
                          median(total$V1)
                        }
                        )
colnames(revenue_median) <- c('Role' , 'median')




distrib_data_grid <- data.frame(RevenueEntry = orderingIncome , dumm = 'dummy')

distrib_data_explode <- ddply(distrib_data , .(instanceID) ,
                              function(x){
                                out <- merge(x , distrib_data_grid , 
                                             by = 'RevenueEntry' , all.y = TRUE)
                                out$Role <- unique(x$Role)
                                out
                              }
                              )

distrib_data_explode$V1[is.na(distrib_data_explode$V1)] <- 0

distrib_data_explode <- merge(distrib_data_explode , revenue_median ,
                              by = 'Role' , all.x = TRUE)

distrib_data_explode$amount <- distrib_data_explode$V1 * distrib_data_explode$median

dist_role <- ddply(distrib_data_explode , .(Role , RevenueEntry) , 
                   function(x) mean(x$amount))


ecz_roles <- c('medecin_chef_zone' , 'infirmier_superviseur' , 
               'administrateur_gestionnaire')
dist_role$structure <- 'Facilities'
dist_role$structure[dist_role$Role %in% ecz_roles] <- 'Health Zone Medical Team'



translate <- function(data){
  data$Role_en <- 'Doctor'
  data$Role_en[data$Role == 'administrateur'] <- 'Administrator'
  data$Role_en[data$Role == 'administrateur_gestionnaire'] <- 'Zone Administrator'
  data$Role_en[data$Role == 'autre'] <- 'Other'
  data$Role_en[data$Role == 'infirmier'] <- 'Nurse'
  data$Role_en[data$Role == 'infirmier_superviseur'] <- 'Supervising Nurse'
  data$Role_en[data$Role == 'labo'] <- 'Lab Technician'
  data$Role_en[data$Role == 'medecin_chef_zone'] <- 'Zone Medical Officer'
  data$Role_en[data$Role == 'pharmacien'] <- 'Pharmacist'
  
  data$Income_en <- 'Salary'
  data$Income_en[data$RevenueEntry == 'Prime de Risque'] <- 'Risk Allowance'
  data$Income_en[data$RevenueEntry == 'Prime de Partenaire'] <- 'Top-up'
  data$Income_en[data$RevenueEntry == 'Per Diem'] <- 'Per Diem'
  data$Income_en[data$RevenueEntry == 'Prime Locale'] <- 'User Fees'
  data$Income_en[data$RevenueEntry == 'Heures supplémentaires'] <- 'Paid Overtime'
  data$Income_en[data$RevenueEntry == 'Activité  Privée'] <- 'Private Practice'
  data$Income_en[data$RevenueEntry == 'Activité non santé'] <- 'Non Health Activity'
  data$Income_en[data$RevenueEntry == 'Informel'] <- 'Informal income'
  data
}

orderingIncome <- c('Salary' , 'Risk Allowance' , 'Top-up' , 'Per Diem' , 
                    'User Fees' , 'Paid Overtime' , 'Private Practice' , 
                    'Non Health Activity' , "Informal income")

revenue_median <- translate(revenue_median)
dist_role <- translate(dist_role)

ord_st <- revenue_median$Role_en[order(revenue_median$median)]
dist_role$Role_en <- factor(dist_role$Role_en ,
                                      levels = ord_st ,
                                      ordered = TRUE)

dist_role$Income_en <- as.character(dist_role$Income_en)
dist_role$Income_en <- factor(dist_role$Income_en , 
                          levels =orderingIncome , ordered = TRUE)

dist_role <- dist_role[order(dist_role$Income_en) ,]

pdf('article/median_income_distribution.pdf' , width = 14)
dataPlot <- subset(dist_role , structure == 'Health Zone Medical Team')
qplot(data = dataPlot , y = V1 , x = Role_en , fill = Income_en , 
      geom = 'bar' , position = 'stack' ,
      stat = 'identity') +
  theme_bw() + scale_fill_brewer(palette="Set1", name = 'Type of Income') + 
  coord_flip() + 
  xlab('') + ylab('Median Income for facility based HWs')

dataPlot <- subset(dist_role , structure != 'Health Zone Medical Team')
qplot(data = dataPlot , y = V1 , x = Role_en , fill = Income_en , 
      geom = 'bar' , position = 'stack' ,
      stat = 'identity') +
  theme_bw() + scale_fill_brewer(palette="Set1", name = 'Type of Income') + 
  coord_flip() + 
  xlab('') + ylab('Median Income for facility based HWs')

dev.off()

dist_role_table <- dcast(dist_role , formula = Role ~ RevenueEntry , value.var = V1)
table_out <- ddply(distrib_data_explode , .(Role , RevenueEntry) ,
                   function(x) mean(x$V1)*100)
oo <- dcast(table_out , formula = Role ~ RevenueEntry , value.var = 'V1')
tab_out <- data.frame(cadre = oo$Role ,
                      Salary_Risk = oo$`Prime de Risque` + oo$Salaire , 
                      top_up_perdiem_heursup = oo$`Prime de Partenaire` + oo$`Per Diem` +
                        oo$`Heures supplémentaires` ,
                      user_fees = oo$`Prime Locale` , 
                      private_pract = oo$`Activité  Privée` ,
                      non_health = oo$`Activité non santé` ,
                      informal = oo$Informel)

## How many topups

count_perdiem <- function(data){
  nrow(subset(data , variable == 'Per Diem'))
}

n_perdiem <- ddply(total_revenu , .(instanceID , FacSimple) , count_perdiem)

max(n_perdiem$V1[n_perdiem$FacSimple == 'facility'])
max(n_perdiem$V1[n_perdiem$FacSimple == 'ecz'])


count_topups <- function(data){
  nrow(subset(data , variable == 'Prime de Partenaire'))
}

n_topup <- ddply(total_revenu , .(instanceID , FacSimple) , count_topups)

max(n_topup$V1[n_topup$FacSimple == 'facility'])
max(n_topup$V1[n_topup$FacSimple == 'ecz'])

##% fac worker who received perdiem
length(unique(total_revenu$instanceID[total_revenu$variable == 'Per Diem' & 
                                        total_revenu$FacSimple == 'facility'])) / 
  length(unique(total_revenu$instanceID[total_revenu$FacSimple == 'facility'])) 

##% fac worker who received topup
length(unique(total_revenu$instanceID[total_revenu$variable == 'Prime de Partenaire' & 
                                        total_revenu$FacSimple == 'facility'])) / 
  length(unique(total_revenu$instanceID[total_revenu$FacSimple == 'facility'])) 

##% ecz worker who received perdiem
length(unique(total_revenu$instanceID[total_revenu$variable == 'Per Diem' & 
                                        total_revenu$FacSimple == 'ecz'])) / 
  length(unique(total_revenu$instanceID[total_revenu$FacSimple == 'ecz'])) 

##% ecz worker who received topup
length(unique(total_revenu$instanceID[total_revenu$variable == 'Prime de Partenaire' & 
                                        total_revenu$FacSimple == 'ecz'])) / 
  length(unique(total_revenu$instanceID[total_revenu$FacSimple == 'ecz'])) 




## Dist Income Most
get_total <- function(data){
  sum(data$value)
}

get_dist <- function(data){
  ddply(data , .(variable) , get_total)
}

get_dist_perc <- function(data){
  d <- get_dist(data)
  t <- get_total(data)
  prop <- d$V1 / t
  data.frame(rev = d$variable , value = prop)
}

dist_rev <- ddply(total_revenu[total_revenu$FacSimple == 'facility' , ] , .(instanceID) , get_dist_perc)
first_source <- function(data){
  m <- max(data$value)
  r <- data$rev[data$value == m]
  r <- data.frame(rev = r )
  r
}

first_rev <- ddply(dist_rev , .(instanceID) , first_source)


length(unique(first_rev$instanceID[first_rev$r == 'Prime Locale'])) / length(unique(first_rev$instanceID))

length(unique(first_rev$instanceID[first_rev$r == 'Activité non santé'])) / length(unique(first_rev$instanceID))












##Modelisation

library(lme4)
library(reshape2)

modelData <- dcast(total_revenu , formula = instanceID + Structure + FacLevel + FacOwnership + 
                     FacAppui + FacRurban + EczAppui + Province + Sex + Age + Matrimonial + RoleInit + 
                     NumberFinancialDependants + LastEducation + FacilityType + Role + Power +
                     FacMotivation ~ RevenueEntry ,
                   function(x) length(x) > 0
                   )


revs <- c("ActPrivee" , "ActNonSante" , "HeureSup" , "Informel" , "PerDiem"  , "PrimesPartenaires" ,  
          "PrimeRisque"  , "PrimeLocale" , "Salaire"  )

colnames(modelData) <- c("instanceID" , "Structure" , "FacLevel" , "FacOwnership" ,
                         "FacAppui" , "FacRurban" , "EczAppui" , "Province" , 
                         "Sex" , "Age" , "Matrimonial" , "RoleInit" , 
                         "NumberFinancialDependants" , "LastEducation" , "FacilityType" , "Role" , 
                         "Power" , "FacMotivation" , revs)


##Create Models

make_formula <- function(y , covariates){
  formula <- paste(y,  covariates, sep = ' ~ ')
  as.formula(formula)
}

covs_indiv <- "Sex + Age + Role  + LastEducation"
covs_hgrcs <-  " FacOwnership + FacRurban + FacMotivation + Power + Province + FacLevel"
covs_ecz <- "FacRurban + FacMotivation + Province"

covs_hgrcs <- paste(covs_indiv , covs_hgrcs , sep = "+")
covs_ecz <- paste(covs_indiv , covs_ecz ,  sep = "+")

models_ecz <- list()
models_fac <- list()

sink('article/models_logistics.txt')
for(i in 1:length(revs)){
  rev <- revs[i]
  print(paste('Running model for ' , rev , 'in facilities'))
  model_fit_fac <- glm(make_formula(rev , covs_hgrcs) ,
                         family = binomial(link = 'logit') , 
                         data = modelData[modelData$FacLevel != 'ecz' , ])
  print(summary(model_fit_fac))
  
  print(paste('Running model for ' , rev , 'in ecz'))
  model_fit_ecz <- glm(make_formula(rev , covs_ecz) ,
                       family = binomial(link = 'logit') , 
                       data = modelData[modelData$FacLevel == 'ecz' , ])
  print(summary(model_fit_ecz))
  
  models_ecz[[rev]] <- model_fit_ecz
  models_fac[[rev]] <- model_fit_fac
}
sink()

### Models on amount

data_amount <- dcast(total_revenu , formula = instanceID + Structure + FacLevel + FacOwnership + 
                       FacAppui + FacRurban + EczAppui + Province + Sex + Age + Matrimonial + RoleInit + 
                       NumberFinancialDependants + LastEducation + FacilityType + Role + Power +
                       FacMotivation ~ RevenueEntry , value.var = 'value' , 
                     function(x){
                       a <- log(sum(x , na.rm = TRUE) + 1)
                       a[a == 0] <- NA
                       a
                     }
                     )

data_amount$Role <- factor(data_amount$Role)
data_amount <- within(data_amount, Role <- relevel(Role, ref = 'medecin'))

revs <- c("ActPrivee" , "ActNonSante" , "HeureSup" , "Informel" , "PerDiem"  , "PrimesPartenaires" ,
          "PrimeRisque"  , "PrimeLocale" , "Salaire")

colnames(data_amount) <- c("instanceID" , "Structure" , "FacLevel" , "FacOwnership" ,
                           "FacAppui" , "FacRurban" , "EczAppui" , "Province" , 
                           "Sex" , "Age" , "Matrimonial" , "RoleInit" , 
                           "NumberFinancialDependants" , "LastEducation" , "FacilityType" , "Role" , 
                           "Power" , "FacMotivation" , revs)

models_ecz2 <- list()
models_fac2 <- list()

sink('article/models_amount.txt')
for(i in 1:length(revs)){
  print(paste('Running model for ' , rev , 'in facilities'))
  model_fit_fac <- lm(make_formula(logging , covs_hgrcs) ,
                        data = data_amount[data_amount$FacLevel != 'ecz' , ])
  print(summary(model_fit_fac))
  models_fac2[[rev]] <- model_fit_fac
  
  
  print(paste('Running model for ' , rev , 'in ecz'))
  if (sum(!is.na(data_amount[data_amount$FacLevel == 'ecz' , rev])) > 10){
    model_fit_ecz <- lm(make_formula(logging , covs_ecz) , 
                        data = data_amount[data_amount$FacLevel == 'ecz' , ])
    print(summary(model_fit_ecz))
    models_ecz2[[rev]] <- model_fit_ecz
  }  
}
sink()

#### Model complete revenue

data_total_revenu <- dcast(total_revenu , formula = instanceID + Structure + FacLevel + FacOwnership + 
                             FacAppui + FacRurban + EczAppui + Province + Sex + Age + Matrimonial + 
                             RoleInit + 
                             NumberFinancialDependants + LastEducation + FacilityType + Role + Power +
                             FacMotivation + MonthlyDollar~ . , value.var = 'value' , 
                           function(x) sum(x , na.rm = TRUE) #log(sum(x , na.rm = TRUE)
)



covs_indiv <- "Sex + Age"
covs_hgrcs <-  "FacOwnership + FacRurban + FacMotivation + Power + Province + FacLevel"
covs_ecz <- "FacRurban + FacMotivation + Province"

#data_total_revenu$Role <- factor(data_total_revenu$Role)
#data_total_revenu <- within(data_total_revenu, Role <- relevel(Role, ref = 'medecin'))


covs_hgrcs <- paste(covs_indiv , covs_hgrcs , sep = '+')

colnames(data_total_revenu)[ncol(data_total_revenu)] <- 'revenue'

data_total_revenu$revenue <- data_total_revenu$revenue



sink('article/models_total_rev1.txt')
print('Running model for facilities')
for (role in unique(data_total_revenu$Role[data_total_revenu$FacLevel != 'ecz'])){
    print(role)
    data_mod <- subset(data_total_revenu , Role == role)
    model_fit_fac <- lm(make_formula('revenue' , covs_hgrcs) ,
                      data = data_mod[data_mod$FacLevel != 'ecz' , ])
print(summary(model_fit_fac))
}
print('Running model for ecz')
for (role in unique(data_total_revenu$Role[data_total_revenu$FacLevel == 'ecz'])){
  print(role)
  data_mod <- subset(data_total_revenu , Role == role)
  model_fit_ecz <- lm(make_formula('revenue' , covs_ecz) , 
                    data = data_mod[data_mod$FacLevel == 'ecz' , ])
  print(summary(model_fit_ecz))
}
sink()
