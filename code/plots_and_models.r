setwd('DRC_HRH/')

stage <- 'modelisation'

source('code/useful_functions.r')


total_revenu <- subset(total_revenu , Role != '' & variable != "Autres revenus")
total_revenu$variable[total_revenu$variable == "Activit\xe9 non sant\xe9"] = "Activité non santé"
total_revenu$variable[total_revenu$variable == "Activit\xe9  Priv\xe9e"] = "Activité  Privée"
total_revenu$variable[total_revenu$variable == "Heures suppl\xe9mentaires"] = "Heures supplémentaires"

total_revenu$FacSimple <- 'facility'
total_revenu$FacSimple[total_revenu$FacilityType == 'ecz'] <- 'ecz'


RevSum <- ddply(total_revenu , .(instanceID , variable , Role , FacilityType) ,  
                function(x) sum(x$value))
RevTot <- ddply(total_revenu , .(instanceID) ,  function(x) sum(x$value))

##Ordering Individuals
ordRev <- RevTot$instanceID[order(RevTot$V1)]
RevSum$instanceID <- factor(RevSum$instanceID , levels =  ordRev , ordered = TRUE)

##Ordering Revenues
orderedIncome <- c('Salaire' , 'Prime de Risque' , 'Prime de Partenaire' , 'Per Diem' , 
                   'Prime Locale' , 'Heures supplémentaires' , 'Activité  Privée' , 
                   'Activité non santé' , "Informel")

RevSum$variable <- as.character(RevSum$variable)

RevSum$Role <- factor(as.character(RevSum$Role) , levels = rev(ordering_staff) , ordered = TRUE)
RevSum$variable[RevSum$variable %in% c("Vente de Medicament" , "Cadeau")] <- 'Informel'

RevSum$variable <- factor(RevSum$variable , levels = rev(orderedIncome) , ordered = TRUE)

pdf('output/graphs/distrib_revenus.pdf' , width = 14)
ggplot(data = RevSum[RevSum$FacilityType %in% c('cs' , 'csr' , 'hgr') ,] , aes(x = instanceID , y = V1, fill=variable) ) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_blank()) + 
  facet_wrap(~Role , scales = 'free_x') + ylab('Income') + xlab('') + scale_fill_brewer(palette="Set1") 

ggplot(data = RevSum[RevSum$FacilityType %in% c('cs' , 'csr' , 'hgr') ,] , aes(x = instanceID , y = V1, fill=variable) ) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_blank()) + 
  facet_wrap(~Role , scales = 'free') + ylab('Income') + xlab('') + scale_fill_brewer(palette="Set1") 
dev.off()

### Some data management

total_revenu$FacRurban[total_revenu$FacRurban == ''] <- 'NA'

total_revenu$LastEducation <- total_revenu$LastEduc
total_revenu$LastEducation [total_revenu$LastEducation  %in%
                              c('medecin_generaliste' , 'medecin_specialiste' , 
                                'medecin_generaliste medecin_specialiste' ,
                                'pharmacien' , 'diplome_etudes_superieures')] <- 'medecin-pharma-etudesup'

total_revenu$LastEducation[total_revenu$LastEducation %in% 
                             c('gestion_administration' , 'gestion_administration autre'  , 'autre')] <-
  'autre'

total_revenu$LastEducation[total_revenu$LastEducation %in% 
                             c('infirmiere_ao' , 'infirmiere_a1' ,
                               'technicien_labo' , 'infirmiere_a2' )] <- 'a0-a1-a2'

total_revenu$LastEducation[total_revenu$LastEducation %in% 
                             c('infirmiere_a3')] <- 'a3'

total_revenu$LastEducation[total_revenu$LastEducation %in%c('')] <- NA


total_revenu$Power[total_revenu$RoleInit %in% c('directeur_nursing' , 
                                                'medecin_chef_staff' ,
                                                'medecin_directeur')] <- 1
total_revenu$Power[total_revenu$RoleInit %in% c('medecin','infirmier_titulaire' ,
                                                'medecin_directeur')] <- 1

total_revenu$Power[is.na(total_revenu$Power)] <- 0
total_revenu$Role[total_revenu$Role == ''] <- NA

total_revenu$FacMotivation <- (total_revenu$NAppuiMotivZs > 0 | total_revenu$NAppuiMotivFac > 1 )
total_revenu$FacMotivation[is.na(total_revenu$FacMotivation)] <- FALSE


table(total_revenu$Role , total_revenu$FacLevel)

##This should be handled before => trace and check

total_revenu$FacLevel[total_revenu$Role %in% c('administrateur_gestionnaire' ,
                                               'infirmier_superviseur' , 
                                               'medecin_chef_zone')] <- 'ecz'

total_revenu$FacLevel[total_revenu$Role %in% c('medecin' , 'infirmier') & 
                        total_revenu$FacLevel == 'ecz'] <- NA



##Pour les modèles simples logistiques, dcast pour avoir juste les données

rev_type <- read.csv('data/revenues_entry.csv')
rev_type <- subset(rev_type , select = c(RevenueLabel , RevenueEntry))
rev_type$RevenueLabel <- as.character(rev_type$RevenueLabel)
rev_type$RevenueEntry <- as.character(rev_type$RevenueEntry)

rev_type$RevenueEntry <- substr(rev_type$RevenueEntry , 1 , nchar(rev_type$RevenueEntry) - 2)


total_revenu <- merge(total_revenu , rev_type , 
                      by.x = 'variable' , by.y = 'RevenueLabel' , all.x = TRUE)


total_revenu$RevenueEntry <- total_revenu$variable
total_revenu$RevenueEntry[total_revenu$variable %in% c('Cadeau' , 'Vente de Medicament')] <- 'Informel' 
total_revenu$RevenueEntry[total_revenu$variable %in% c("Autres revenus")] <- 'Activité non santé' 



orderedIncome <- c('Salaire' , 'Prime de Risque' , 'Prime de Partenaire' , 'Per Diem' , 
                   'Prime Locale' , 'Heures supplémentaires' , 'Activité  Privée' , 
                   'Activité non santé' , "Informel")



##Representation du revenu des MCZ

revenu_mcz <- subset(total_revenu , Role == "medecin_chef_zone")

revenu_mcz$RevenueEntry <- factor(revenu_mcz$RevenueEntry , levels =  orderedIncome)

rev_comp_mcz <- ddply(revenu_mcz , .(Province) , 
                      function(x){
                        nombre <- length(unique(x$instanceID))
                        ddply(x , .(RevenueEntry) , 
                              function(x){
                                sum(x$value) / nombre
                              })
                      } 
                        )


pdf('output/graphs/MCZ_income_dist.pdf', width = 10)
qplot(data = rev_comp_mcz , y = V1 , x = 1 , fill = RevenueEntry , geom = 'bar' , position = 'stack' ,
      stat = 'identity') +
  facet_grid(Province~.) + theme_bw() + scale_fill_brewer(palette="Set1") + 
  theme(axis.text.y = element_blank()) +
  xlab('') + ylab('Average Income') + coord_flip() + labs(title = "Distribution of MCZ Income")
dev.off()


output.table(rev_comp_mcz , 'mcz_revenue')

## Dist of total income

data_total_income <- ddply(total_revenu , .(instanceID , Role , Province , FacLevel)  , 
                           function(x) sum(x$value , na.rm = TRUE))


data_total_income <- subset(data_total_income , !is.na(Role) & !is.na(FacLevel))

make_dist <- function(x){
  median <- median(x$V1)
  max <- max(x$V1)
  min <- min(x$V1)
  data.frame(median , max , min)
}

data_total_income_plot <- ddply(data_total_income , .(Province , Role , FacLevel) , 
                                make_dist)

data_total_income_plot$Role <- factor(data_total_income_plot$Role , 
                                      levels = ordering_staff , 
                                      ordered = TRUE)
data_total_income_plot$FacLevel <- factor(data_total_income_plot$FacLevel , 
                                      levels = ordering_facilities , 
                                      ordered = TRUE)

pdf('output/graphs/income_median_distribution.pdf' , width = 14)
ggplot(data = data_total_income_plot , aes(x = median , y = Role , col = 1 )) +
  geom_point(size = 2.5) + 
  facet_grid(FacLevel~ Province, scales = 'free_y') +
  geom_errorbarh(aes(xmax = max, xmin = min , height = .5 ), col = 2)+
  theme_bw() + xlab('Total Income') +
  guides(col = FALSE)
dev.off()

table_rev_role_only <- ddply(data_total_income , .(Role) , 
                                make_dist)
output.table(table_rev_role_only , 'revenu_distributions_role')

table_rev_province_role <- ddply(data_total_income , .(Province , Role) , 
                                make_dist)
output.table(table_rev_province_role , 'revenu_distributions_province_role')

### Table 1

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
                                             distrib_data$V1 == 1]

total_revenu <- subset(total_revenu , !(instanceID %in% exclude))


distrib_data$RevenueEntry <- factor(distrib_data$RevenueEntry ,
                                levels = orderedIncome , 
                                ordered = TRUE )

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
                        

distrib_data <- subset(distrib_data , !is.na(Role))
revenue_median <- subset(revenue_median , !is.na(Role))

colnames(revenue_median) <- c('Role' , 'median')

distrib_data_grid <- data.frame(RevenueEntry = orderedIncome , dumm = 'dummy')

distrib_data_explode <- ddply(distrib_data , .(instanceID) ,
                              function(x){
                                out <- merge(x , distrib_data_grid , by = 'RevenueEntry' , all.y = TRUE)
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


trans_frenc <- function(data){
  data$Role_french_graph[data$Role == 'medecin_chef_zone'] <- 'Médecin (ECZ)'
  data$Role_french_graph[data$Role == 'medecin'] <- 'Médecin (FoSa)'
  data$Role_french_graph[data$Role == 'infirmier_superviseur'] <- 'Infirmier (ECZ)'
  data$Role_french_graph[data$Role == 'administrateur_gestionnaire'] <- 'Admin (ECZ)'
  data$Role_french_graph[data$Role == 'administrateur'] <- 'Admin (FoSa)'
  data$Role_french_graph[data$Role == 'labo'] <- 'Tech Labo'
  data$Role_french_graph[data$Role == 'pharmacien'] <- 'Pharm. / prep Pharm.'
  data$Role_french_graph[data$Role == 'infirmier'] <- 'Infirmier (Fosa)'
  data$Role_french_graph[data$Role == 'autre'] <- 'Autre'
  data
  }

revenue_median <- trans_frenc(revenue_median)
dist_role <- trans_frenc(dist_role)

ord_st <- revenue_median$Role_french_graph[order(revenue_median$median)]
dist_role$Role_french_graph <- factor(dist_role$Role_french_graph ,
                                      levels = ord_st ,
                                      ordered = TRUE)

pdf('output/graphs/median_income_distribution.pdf' , width = 14)
qplot(data = dist_role , y = V1 , x = Role_french_graph , fill = RevenueEntry , 
      geom = 'bar' , position = 'stack' ,
      stat = 'identity') +
  theme_bw() + scale_fill_brewer(palette="Set1", name = 'Type de revenu') + 
  coord_flip() + 
  xlab('') + ylab('Revenu median')

qplot(data = dist_role , y = V1 , x = Role , fill = RevenueEntry , geom = 'bar' , position = 'stack' ,
      stat = 'identity') +
  theme_bw() + scale_fill_brewer(palette="Set1", name = 'Type de revenu') + 
  xlab('') + ylab('Median Income') + 
  labs(title = "Median income and average distribution") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

dist_role_table <- dcast(dist_role , formula = Role ~ RevenueEntry)
output.table(dist_role_table , 'median_revenue_composition')




#####



### Salaires

percentage_revenu <- function(data , N){
  perc <- length(unique(data$instanceID)) / N
  perc
}

CreateHeatMapData <- function(data , revenue_name){
  N <- length(unique(data$instanceID))
  data <- subset(data , RevenueEntry == revenue_name)
  print(N)
  heat_data <- percentage_revenu(data , N)
  heat_data
}

make_heat_revenue <- function(data , revenu_name){
  heat_data <- ddply(total_revenu , .(Province , FacilityType , Role) , 
                     function(data) CreateHeatMapData(data , revenu_name ))
  heat_data <- subset(heat_data ,  Role != '')
  heat_data$FacilityType <- factor(heat_data$FacilityType , levels = ordering_facilities , 
                                   ordered = TRUE)
  heat_data$Role <- factor(heat_data$Role , levels = ordering_staff , ordered = TRUE)
  
  title <- paste('Heatmap for' , revenu_name , sep = ' ')
  plot <- qplot(y = Role , data = heat_data, fill = V1, x = rep("" , nrow(heat_data)) ,
                geom = "raster" , label = round(V1 , 2) , main = title)+
    scale_fill_gradient(limits=c(0,1) , low="red" , high = "green" , name = 'Legend') +
    facet_grid(FacilityType~Province , scales = 'free_y') +
    theme_bw()+
    geom_text() + xlab('') + ylab('')
  heat_data$Source <- revenu_name
  list(heat_data , plot)
}

heat_complete <- data.frame(Province = character() , FacilityType = character() , Role = character() , 
                            V1 = numeric() , Source = character())
pdf('output/graphs/revenue_heatmap.pdf', width = 14)
for(i in 1:length(orderedIncome)){
  heat <- make_heat_revenue(total_revenu , orderedIncome[i])
  heat_complete <- rbind(heat_complete , heat[[1]])
  provs <- unique(heat_complete$Province)
}

revenu <- orderedIncome
heat_complete$Source <- factor(heat_complete$Source , 
                               levels = revenu , 
                               ordered = TRUE)


p <- qplot(x =Province ,y = Role , data = heat_complete, 
           fill = V1, geom = "raster"  , main = 'Prevalence of each source of revenue')+
  scale_fill_gradient(limits=c(0,1) , low="red" , high = "green" , name = 'Legend') +
  facet_grid(FacilityType~Source , scales = 'free_y' ) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('') + ylab('')
print(p)
dev.off()

output.table(heat_complete , 'revenue_type_prevalence')



















### Modelling #####

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


##Modelisation

library(lme4)


##Create Models

make_formula <- function(y , covariates){
  formula <- paste(y,  covariates, sep = ' ~ ')
  as.formula(formula)
}

covs_indiv <- "Sex + Age + LastEducation + Role" 
covs_hgrcs <-  "FacLevel + FacOwnership + FacRurban + FacMotivation + Power + (1|Province)"
covs_ecz <- "FacRurban + FacMotivation + Province"

covs_hgrcs <- paste(covs_indiv , covs_hgrcs , sep = "+")
covs_ecz <- paste(covs_indiv , covs_ecz ,  sep = "+")



models_ecz <- list()
models_fac <- list()

sink('output/models_logistics.txt')
for(i in 1:length(revs)){
  rev <- revs[i]
  print(paste('Running model for ' , rev , 'in facilities'))
  model_fit_fac <- glmer(make_formula(rev , covs_hgrcs) ,
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
                        function(x) sum(x , na.rm = TRUE)
                     )


revs <- c("ActPrivee" , "ActNonSante" , "HeureSup" , "Informel" , "PerDiem"  , "PrimesPartenaires" ,
          "PrimeRisque"  , "PrimeLocale" , "Salaire")

colnames(data_amount) <- c("instanceID" , "Structure" , "FacLevel" , "FacOwnership" ,
                         "FacAppui" , "FacRurban" , "EczAppui" , "Province" , 
                         "Sex" , "Age" , "Matrimonial" , "RoleInit" , 
                         "NumberFinancialDependants" , "LastEducation" , "FacilityType" , "Role" , 
                         "Power" , "FacMotivation" , revs)

revs <- c("ActPrivee" , "ActNonSante" , "HeureSup" , "Informel" , "PerDiem"  , "PrimesPartenaires" ,
          "PrimeLocale")

models_ecz2 <- list()
models_fac2 <- list()

sink('output/models_amount.txt')
for(i in 1:length(revs)){
  rev <- revs[i]
  zeros <- data_amount[,rev] == 0
  if(sum(zeros) >= 1){
    data_amount[zeros,rev] <- NA
  }
  logging <- paste('log(' , rev , ')' , sep = '')
  print(paste('Running model for ' , rev , 'in facilities'))
  model_fit_fac <- lmer(make_formula(logging , covs_hgrcs) ,
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
                             FacAppui + FacRurban + EczAppui + Province + Sex + Age + Matrimonial + RoleInit + 
                             NumberFinancialDependants + LastEducation + FacilityType + Role + Power +
                             FacMotivation ~ . , value.var = 'value' , 
                           function(x) sum(x , na.rm = TRUE)
                           )
colnames(data_total_revenu)[ncol(data_total_revenu)] <- 'revenue'


sink('output/models_total.txt')
print('Running model for facilities')
model_fit_fac <- lmer(make_formula('revenue' , covs_hgrcs) ,
                    data = data_total_revenu[data_total_revenu$FacLevel != 'ecz' , ])
print(summary(model_fit_fac))
  
print('Running model for ecz')
model_fit_ecz <- lm(make_formula('revenue' , covs_ecz) , 
                     data = data_total_revenu[data_total_revenu$FacLevel == 'ecz' , ])
print(summary(model_fit_ecz))
sink()


## Count Models

count_rev <- ddply(total_revenu , .(RevenueEntry , instanceID , Structure , FacLevel , FacOwnership , 
                                      FacAppui , FacRurban , EczAppui , Province , Sex , Age , Matrimonial , 
                                      RoleInit , NumberFinancialDependants , LastEducation ,
                                      FacilityType , Role , Power , FacMotivation) , 
                   nrow)
colnames(count_rev)[ncol(count_rev)] <- 'Number'

dumm <- data.frame(instanceID = unique(total_revenu$instanceID) , dum = 'dummy')

count_perdiem <- subset(count_rev , RevenueEntry == 'Per Diem')
count_perdiem <- merge(count_perdiem  ,dumm , all.y = TRUE)
count_perdiem$Number[is.na(count_perdiem$Number)] <- 0


sink('output/models_count_perdiem.txt')
print('Running PerDiem model for facilities')
model_fit_fac <- glmer(make_formula('Number' , covs_hgrcs ) , family = poisson ,
                    data = count_perdiem[count_perdiem$FacLevel != 'ecz' , ])
print(summary(model_fit_fac))

print('Running PerDiem model for ecz')
model_fit_ecz <- glm(make_formula('Number' , covs_ecz) , family = poisson , 
                     data = count_perdiem[count_perdiem$FacLevel == 'ecz' , ])
print(summary(model_fit_ecz))
sink()



count_prim_part <- subset(count_rev , RevenueEntry == 'Prime de Partenaire')
count_prim_part <- merge(count_prim_part  , dumm , all.y = TRUE)
count_prim_part$Number[is.na(count_prim_part$Number)] <- 0


  
sink('output/models_count_prime_partenaire.txt')
print('Running Prime Partenaire model for facilities')
model_fit_fac <- glmer(make_formula('Number' , covs_hgrcs ) , family = poisson ,
                     data = count_prim_part[count_prim_part$FacLevel != 'ecz' , ])
print(summary(model_fit_fac))

print('Running Prime Partenaire for ecz')
model_fit_ecz <- glm(make_formula('Number' , covs_ecz) , family = poisson , 
                     data = count_prim_part[count_prim_part$FacLevel == 'ecz' , ])
print(summary(model_fit_ecz))
sink()

tab_perdiem <- table(count_perdiem$Number)
tab_prim_part <- table(count_prim_part$Number)

output.table(tab_perdiem , 'distribution_perdiem')
output.table(tab_prim_part , 'distribution_prime_partenaire')








#### Counterfactual Analysis #####

library(arm)



compute_link <- function(data , betas, hierarchic){
  fix <- betas[[1]]
  rand <- betas[[2]]
  randeffect <- 0
  fixeffect <-  fix[,1] + 
    fix[,2]*(data$Sex == 'Homme') + fix[,3]*data$Age + 
    fix[,4]*(data$LastEducation == 'a3') + fix[,5]*(data$LastEducation == 'autre') + 
    fix[,6]*(data$LastEducation == 'medecin-pharma-etudesup') +
    fix[,7]*(data$Role == 'autre') + fix[,8]*(data$Role == 'infirmier') +
    fix[,9]*(data$Role == 'labo') + fix[,10]*(data$Role == 'medecin') +
    fix[,11]*(data$Role == 'pharmacien') + 
    fix[,12]*(data$FacLevel == 'csr') + fix[,13]*(data$FacLevel == 'hgr') +
    fix[,14]*(data$FacOwnership == 'privee') + fix[,15]*(data$FacOwnership == 'publique') +
    fix[,15]*(data$FacRurban == 'urbain') + 
    fix[,16]*data$FacMotivation +
    fix[,17]*data$Power
  
  if(hierarchic == TRUE){
    randeffect <- rand[,2]*(data$Province == 'bandundu') +
      rand[,3]*(data$Province == 'equateur') +
      rand[,4]*(data$Province == 'katanga') +
      rand[,5]*(data$Province == 'sud_kivu')
  }
  out <- fixeffect + randeffect
  out
}

generate_betas <- function(result_model , n){
  fix <- data.frame(fixef(result_model))
  rand <- data.frame(unlist(ranef(result_model)))
  vcovfix <- vcov(result_model)
  vcovrand <- attr(ranef(result_model , condVar = TRUE)[[1]] , 'postVar')[1,,]
  betas_fix <- mvrnorm(n , fix[,1] , as.matrix(vcovfix))
  betas_rand <- as.data.frame(rnorm(4*n , rand[,1] , vcovrand))
  betas_rand$prov <- rep(c('bandundu' , 'equateur' , 'katanga' , 'sud_kivu') , n)
  colnames(betas_rand) <- c('betas' , 'prov')
  betas_rand$indiv <- sort(rep(seq(1:n) , 4))
  betas_rand <- dcast(betas_rand , indiv ~ prov , value.var = 'betas')
  list(betas_fix , betas_rand)
}

compute_generated_links <- function(result_model , n , data , hierarc){
  aa <- generate_betas(result_model , n)
  compute_link(data , aa , hierarc)  
}

simulateLogit <- function(result_model , n , data , hierarc){
  invlogit(compute_generated_links(result_model , n , data , hierarc)  )
}

amount_predict <- function(result_model , n , data , hierarc){
  exp(compute_generated_links(result_model , n , data , hierarc))
}

quantile_to_plot <- function(outputs){
  quantile(outputs , probs = c(0.025 , 0.5 , 0.975))  
}



######### Make Counterfactuals ###########

counterfac_by_province <- function(counterfac){
  list(bandundu = data.frame(counterfac , Province = 'bandundu') ,
       equateur = data.frame(counterfac , Province = 'equateur') ,
       katanga = data.frame(counterfac , Province = 'katanga') ,
       sud_kivu = data.frame(counterfac , Province = 'sud_kivu')
  )
}

###Create base scenario

scenario_0_cs <-data.frame(Role = 'administrateur' ,
                           FacRurban = 'rural' ,
                           Power = 0,
                           Sex = 'Homme' ,
                           Age = 40 , 
                           LastEducation = 'autre' , 
                           FacLevel = 'cs' ,
                           FacOwnership = 'publique' ,
                           FacMotivation = 0)


simulateLogit(model_fit_fac , 1000 , scenario_0_cs , TRUE)


scenario_0_cs <-data.frame(Role = 'administrateur' ,
                           FacRurban = 'rural' ,
                           Power = 0,
                           Sex = '' ,
                           Age = 40 , 
                           LastEducation = 'autre' , 
                           FacLevel = 'cs' ,
                           FacOwnership = 'publique' ,
                           FacMotivation = 0 , 
                           Province = 'bandundu')


qplot(simulateLogit(model_fit_fac , 1000 , scenario_0_cs , TRUE))







scenario_0_hgr <- scenario_0_cs
scenario_0_hgr$FacLevel <- 'hgr'

scenario_0_ecz <- scenario_0_cs
scenario_0_ecz$FacLevel <- 'ecz'
scenario_0_ecz$ecz <- 1
scenario_0_ecz$hgrcs <- 0





###

activatePost <- function(data , post , educ){
  postsList <- c('administrateur_gestionnaire' ,
                 'autre' , 'infirmier' , 'infirmier_superviseur' , 'labo' , 'medecin' , 
                 'mcz' , 'pharmacien')
  data[,post] <- 1
  postNon <- postsList[postsList != post]
  data[,postNon] <- 0
  data$post <- post
  data$LastEducation <- educ
  data
}

scenar_admin_cs <- scenario_0_cs
scenar_autre_cs <- activatePost(scenario_0_cs , 'autre' , 'autre')
scenar_infirmier_cs <- activatePost(scenario_0_cs , 'infirmier' , 'a0-a1-a2')
scenar_labo_cs <- activatePost(scenario_0_cs , 'labo' , 'a0-a1-a2 ')
scenar_medecin_cs <- activatePost(scenario_0_cs , 'medecin' , 'medecin-pharma-etudesup')
scenar_pharmacien_cs <- activatePost(scenario_0_cs , 'pharmacien' , 'a0-a1-a2')

scenar_admin_hgr <- scenario_0_hgr
scenar_autre_hgr <- activatePost(scenario_0_hgr , 'autre', 'autre')
scenar_infirmier_hgr <- activatePost(scenario_0_hgr , 'infirmier', 'a0-a1-a2')
scenar_labo_hgr <- activatePost(scenario_0_hgr , 'labo', 'a0-a1-a2 ')
scenar_medecin_hgr <- activatePost(scenario_0_hgr , 'medecin', 'medecin-pharma-etudesup')
scenar_pharmacien_hgr <- activatePost(scenario_0_hgr , 'pharmacien', 'a0-a1-a2')

scenar_admin_g_ecz <- activatePost(scenario_0_ecz , 'administrateur_gestionnaire' , 'autre')
scenar_autre_ecz <- activatePost(scenario_0_ecz , 'autre' , 'autre')
scenar_is_ecz <- activatePost(scenario_0_ecz , 'infirmier_superviseur' , 'a0-a1-a2')
scenar_mcz_ecz <- activatePost(scenario_0_ecz , 'mcz' , 'medecin-pharma-etudesup')

## Autorite scenar

add_autorite <- function(data){
  data$Power <- 1
  data
}

scenar_infirmier_cs_a <- add_autorite(scenar_infirmier_cs)
scenar_medecin_cs_a <- add_autorite(scenar_medecin_cs)

scenar_infirmier_hgr_a <- add_autorite(scenar_infirmier_hgr)
scenar_medecin_hgr_a <- add_autorite(scenar_medecin_hgr)

## Appui partenaire

add_partenaire <- function(data){
  data$FacAppui <- 'oui'
  data
}

scenar_admin_cs_appui <- add_partenaire(scenario_0_cs)
scenar_autre_cs_appui <- add_partenaire(scenar_autre_cs)
scenar_infirmier_cs_appui <- add_partenaire(scenar_infirmier_cs)
scenar_labo_cs_appui <- add_partenaire(scenar_labo_cs)
scenar_medecin_cs_appui <- add_partenaire(scenar_medecin_cs)
scenar_pharmacien_cs_appui <- add_partenaire(scenar_pharmacien_cs)

scenar_admin_hgr_appui <- add_partenaire(scenario_0_hgr)
scenar_autre_hgr_appui <- add_partenaire(scenar_autre_hgr)
scenar_infirmier_hgr_appui <- add_partenaire(scenar_infirmier_hgr)
scenar_labo_hgr_appui <- add_partenaire(scenar_labo_hgr)
scenar_medecin_hgr_appui <- add_partenaire(scenar_medecin_hgr)
scenar_pharmacien_hgr_appui <- add_partenaire(scenar_pharmacien_hgr)

scenar_admin_g_ecz_appui <- add_partenaire(scenar_admin_g_ecz)
scenar_autre_ecz_appui <- add_partenaire(scenar_autre_ecz)
scenar_is_ecz_appui <- add_partenaire(scenar_is_ecz)
scenar_mcz_ecz_appui <- add_partenaire(scenar_mcz_ecz)




## Provinces

counterfacts1 <- list(scenar_admin_cs , scenar_autre_cs , scenar_infirmier_cs , scenar_labo_cs ,
                      scenar_medecin_cs , scenar_pharmacien_cs , scenar_admin_hgr , scenar_autre_hgr ,
                      scenar_infirmier_hgr , scenar_labo_hgr , scenar_medecin_hgr , scenar_pharmacien_hgr ,
                      scenar_admin_g_ecz , scenar_autre_ecz , scenar_is_ecz , scenar_mcz_ecz , scenar_infirmier_cs_a , 
                      scenar_medecin_cs_a ,scenar_infirmier_hgr_a ,scenar_medecin_hgr_a , scenar_admin_cs_appui ,
                      scenar_autre_cs_appui , scenar_infirmier_cs_appui , scenar_labo_cs_appui , 
                      scenar_medecin_cs_appui , scenar_pharmacien_cs_appui , scenar_admin_hgr_appui ,
                      scenar_autre_hgr_appui , scenar_infirmier_hgr_appui , scenar_labo_hgr_appui , 
                      scenar_medecin_hgr_appui , scenar_pharmacien_hgr_appui , scenar_admin_g_ecz_appui ,
                      scenar_autre_ecz_appui , scenar_is_ecz_appui , scenar_mcz_ecz_appui 
)

counterfacts_province <-list()
for(i in 1:length(counterfacts1)){
  counterfacts_province[[i]] <- counterfac_by_province(counterfacts1[[i]])
}


compute_counterfac <- function(counterfacs_prov , func , model , n){
  out <- quantile(func(model , n , counterfacs_prov[[1]] ) ,
                  probs = c(0.025 , 0.5 , 0.975))
  for(i in 2:4){
    ex <- quantile(func(model , n , counterfacs_prov[[i]] ) ,
                   probs = c(0.025 , 0.5 , 0.975))
    out <- rbind(out , ex , deparse.level = 0)
  }
  out <- as.data.frame(out)
  colnames(out) <- c('q1' , 'mean' , 'q9')
  out$prov <- c('bandundu' , 'equateur' , 'katanga' , 'sud_kivu')
  out
}

make_plot_df <- function(counterfacs_prov , func , model , n , type){
  xout <- list()
  for(i in 1:length(counterfacs_prov)){
    out <- compute_counterfac(counterfacs_prov[[i]] , func ,
                              model , n)
    out$income <- type
    out$autorite <- counterfacs_prov[[i]][[1]]$Power
    out$facility <- counterfacs_prov[[i]][[1]]$FacLevel
    out$post <- counterfacs_prov[[i]][[1]]$post
    out$FacAppui <- counterfacs_prov[[i]][[1]]$FacAppui
    xout[[i]] <- out
  }
  toPlot <- data.frame(q1 =numeric() , 
                       mean  =numeric() ,
                       q9  =numeric() ,
                       prov  =character() ,
                       income  =character() , 
                       autorite  =numeric() ,
                       facility   =character() ,
                       post =character() ,
                       appui = character())
  
  for(i in 1:length(xout)){
    toPlot <- rbind(toPlot , xout[[i]])
  }
  toPlot
}

plot_df_Salaire_prob <- make_plot_df(counterfacts_province , simulateLogit , prob_salaire , 10000 , 'salaire')
plot_df_pdr_prob <- make_plot_df(counterfacts_province , simulateLogit , prob_pdr , 10000 , 'prime de risque')
plot_df_hon_amount <- make_plot_df(counterfacts_province , amount_predict , amount_honoraire , 10000 , 'honoraire')
plot_df_primePart_amount <- make_plot_df(counterfacts_province , amount_predict , amount_prime , 10000 , 'top up')
