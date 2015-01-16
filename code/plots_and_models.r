setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'modelisation'

source('code/useful_functions.r')

RevSum <- ddply(total_revenu , .(instanceID , variable , Role) ,  function(x) sum(x$value))
RevTot <- ddply(total_revenu , .(instanceID) ,  function(x) sum(x$value))

##Ordering Individuals
ordRev <- RevTot$instanceID[order(RevTot$V1)]
RevSum$instanceID <- factor(RevSum$instanceID , levels =  ordRev , ordered = TRUE)

##Ordering Revenues
orderedIncome <- c('Salaire' , 'Prime de Risque' , 'Prime de Partenaire' , 'Per Diem' , 
                   'Prime Locale' , 'Heures supplémentaires' , 'Activité  Privée' , 
                   'Activité non santé' , "Autres revenus" , "Vente de Medicament" , "Cadeau")

RevSum$variable <- factor(RevSum$variable , levels =  orderedIncome)

RevSum <- subset(RevSum , Role != '')

RevSum$Role <- factor(as.character(RevSum$Role) , levels = rev(ordering_staff) , ordered = TRUE)

pdf('output/graphs/distrib_revenus.pdf' , width = 14)
qplot(data = RevSum , x = instanceID , y = V1 , geom = 'bar' , stat = 'identity' , width = 1 , fill = variable)+
  theme(axis.text.x = element_blank()) + facet_wrap(~Role , scales = 'free') + ylab('Income') + xlab('')
dev.off()

table(RevSum$variable)

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

ord_st <- revenue_median$Role[order(revenue_median$median)]


dist_role$Role <- factor(dist_role$Role ,
                         levels = ord_st , 
                         ordered = TRUE)

pdf('output/graphs/median_income_distribution.pdf' , width = 14)
qplot(data = dist_role , y = V1 , x = Role , fill = RevenueEntry , geom = 'bar' , position = 'stack' ,
      stat = 'identity') +
  theme_bw() + scale_fill_brewer(palette="Set1", name = 'Type of Income') + 
  coord_flip() + 
  xlab('') + ylab('Median Income') + 
  labs(title = "Median income and average distribution")

dist_role$Role <- factor(dist_role$Role ,
                         levels = rev(ordering_staff) , 
                         ordered = TRUE)

qplot(data = dist_role , y = V1 , x = Role , fill = RevenueEntry , geom = 'bar' , position = 'stack' ,
      stat = 'identity') +
  theme_bw() + scale_fill_brewer(palette="Set1", name = 'Type of Income') + 
  xlab('') + ylab('Median Income') + 
  labs(title = "Median income and average distribution") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

dist_role_table <- dcast(dist_role , formula = Role ~ RevenueEntry)
output.table(dist_role_table , 'median_revenue_composition')






### Modelling

modelData <- dcast(total_revenu , formula = instanceID + Structure + FacLevel + FacOwnership + 
             FacAppui + FacRurban + EczAppui + Province + Sex + Age + Matrimonial + RoleInit + 
             NumberFinancialDependants + LastEducation + FacilityType + Role + Power +
               FacMotivation ~ RevenueEntry ,
             function(x) length(x) > 0
             )
  

##Modelisation

library(lme4)


##Create Models

make_formula <- function(y , covariates){
  formula <- paste(y,  covariates, sep = ' ~ ')
  as.formula(formula)
}

covs_indiv <- "Sex + Age + LastEducation + Role" 
covs_hgrcs <-  "FacLevel + FacOwnership + FacRurban + FacMotivation + Power + Province"
covs_ecz <- "FacRurban + FacMotivation + Province"

covs_hgrcs <- paste(covs_indiv , covs_hgrcs , sep = "+")
covs_ecz <- paste(covs_indiv , covs_ecz ,  sep = "+")

revs <- c("ActNonSante" , "ActPrivee" , "AutreRevenu" , "Honoraire" , "Informel" , "PerDiem"  ,  
          "PrimeRisque" , "PrimesPartenaires" , "Salaire")

models_ecz <- list()
models_fac <- list()


sink('output/models_logistics.txt')
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
                        function(x) sum(x , na.rm = TRUE)
                     )




models_ecz2 <- list()
models_fac2 <- list()

revs <- c("ActNonSante" , "ActPrivee" , "AutreRevenu" , "Honoraire" , "Informel" , "PerDiem"  ,  
          "PrimesPartenaires")


sink('output/models_amount.txt')
for(i in 1:length(revs)){
  rev <- revs[i]
  zeros <- data_amount[,rev] == 0
  
  data_amount[zeros,rev] <- NA
  logging <- paste('log(' , rev , ')' , sep = '')
  print(paste('Running model for ' , rev , 'in facilities'))
  model_fit_fac <- lm(make_formula(logging , covs_hgrcs) ,
                       data = data_amount[data_amount$FacLevel != 'ecz' , ])
  print(summary(model_fit_fac))
  
  print(paste('Running model for ' , rev , 'in ecz'))
  if (sum(!is.na(data_amount[data_amount$FacLevel == 'ecz' , rev])) > 10){
    model_fit_ecz <- lm(make_formula(logging , covs_ecz) , 
                       data = data_amount[data_amount$FacLevel == 'ecz' , ])
    print(summary(model_fit_ecz))
    models_ecz2[[rev]] <- model_fit_ecz
  }
  
  models_fac2[[rev]] <- model_fit_fac
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
model_fit_fac <- lm(make_formula('revenue' , covs_hgrcs) ,
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
model_fit_fac <- glm(make_formula('Number' , covs_hgrcs ) , family = poisson ,
                    data = count_perdiem[count_perdiem$FacLevel != 'ecz' , ])
print(summary(model_fit_fac))

print('Running PerDiem model for ecz')
model_fit_ecz <- glm(make_formula('Number' , covs_ecz) , family = poisson , 
                     data = count_perdiem[count_perdiem$FacLevel == 'ecz' , ])
print(summary(model_fit_ecz))
sink()



count_prim_part <- subset(count_rev , RevenueEntry == 'Prime de Partenaire')
count_prim_part <- merge(count_prim_part  ,dumm , all.y = TRUE)
count_prim_part$Number[is.na(count_prim_part$Number)] <- 0

sink('output/models_count_prime_partenaire.txt')
print('Running Prime Partenaire model for facilities')
model_fit_fac <- glm(make_formula('Number' , covs_hgrcs ) , family = poisson ,
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













library(arm)

compute_link <- function(data , betas){
  adm.ecz <- data$administrateur_gestionnaire*data$ecz
  ecz.autre <- data$autre*data$ecz
  infirmier.hgrcs <- data$infirmier*data$hgrcs
  infirmier_sup.ecz <- data$ecz*data$infirmier_superviseur
  hgrcs.labo <- data$hgrcs*data$labo
  hgrcs.medecin <- data$hgrcs*data$medecin
  ecz.mcz <- data$ecz*data$mcz
  hgrcs.pharmacien <- data$hgrcs*data$pharmacien
  hgrcs.administrateur <- data$hgrcs*data$administrateur
  hgrcs.autre <- data$hgrcs*data$autre
  fix <- betas[[1]]
  rand <- betas[[2]]
  out <-  fix[,1] + 
    fix[,2]*(data$GroupDemographics.IndivSex == 'Homme') + fix[,3]*data$GroupDemographics.IndivAge +
    fix[,4]*(data$FacRurban == 'urbain') + fix[,5]*(data$FacAppui == 'oui') +   
    fix[,6]*(data$LastEducation == 'a3') + fix[,7]*(data$LastEducation == 'autre') +
    fix[,8]*(data$LastEducation == 'medecin-pharma-etudesup') + 
    fix[,9]*(data$FacLevel == 'ecz') + fix[,10]*(data$FacLevel == 'hgr') + 
    fix[,11]*data$Power +
    fix[,12]*hgrcs.administrateur + fix[,13]*hgrcs.autre + fix[,14]*infirmier.hgrcs +
    fix[,15]*hgrcs.labo + fix[,16]*hgrcs.medecin + fix[,17]*hgrcs.pharmacien
  fix[,18]*adm.ecz + fix[,19]*ecz.autre + 
    fix[,20]*infirmier_sup.ecz + fix[,21]*ecz.mcz + 
    rand[,2]*(data$Province == 'bandundu') +
    rand[,3]*(data$Province == 'equateur') +
    rand[,4]*(data$Province == 'katanga') +
    rand[,5]*(data$Province == 'sud_kivu')
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


compute_generated_links <- function(result_model , n , data){
  aa <- generate_betas(result_model , n)
  compute_link(data , aa)  
}


simulateLogit <- function(result_model , n , data){
  invlogit(compute_generated_links(result_model , n , data)  )
}

amount_predict <- function(result_model , n , data){
  exp(compute_generated_links(result_model , n , data))
}

```


```{r}
quantile_to_plot <- function(outputs){
  quantile(outputs , probs = c(0.025 , 0.5 , 0.975))  
}

counterfac_by_province <- function(counterfac){
  list(bandundu = data.frame(counterfac , Province = 'bandundu') ,
       equateur = data.frame(counterfac , Province = 'equateur') ,
       katanga = data.frame(counterfac , Province = 'katanga') ,
       sud_kivu = data.frame(counterfac , Province = 'sud_kivu')
  )
}


```



```{r ,warning=FALSE}

###Create scenarios

scenario_0_cs <-data.frame(administrateur_gestionnaire = 0 ,
                           autre = 0 , 
                           infirmier = 0 ,
                           labo = 0 ,
                           medecin = 0 ,
                           mcz = 0 ,
                           pharmacien = 0 ,
                           FacRurban = 'rural' , 
                           FacAppui = 'non' ,
                           infirmier_superviseur = 0 ,
                           Power = 0,
                           GroupDemographics.IndivSex = 'Homme' ,
                           GroupDemographics.IndivAge = 40 , 
                           LastEducation = 'autre' , 
                           FacLevel = 'cs'  , 
                           ecz = 0 ,
                           hgrcs = 1 ,
                           post = 'administrateur')

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
```


```{r, fig.width=10}  

plot1 <- subset(plot_df_Salaire_prob , FacAppui == 'non')
ggplot(plot1, aes(x=mean, y =  post , col = factor(autorite))) +
  geom_point() + geom_errorbarh(aes(xmax = q1, xmin = q9 , height = .2)) +
  xlim(c(0,1)) + theme_bw() +
  xlab("Probabilité qu'un agent recoive un salaire") +
  ylab("") +
  facet_grid(prov ~ facility , scales = 'free_y'  )

plot2 <- subset(plot_df_Salaire_prob , autorite == 0)
ggplot(plot2, aes(x=mean, y =  post , col = factor(FacAppui))) +
  geom_point() + geom_errorbarh(aes(xmax = q1, xmin = q9 , height = .2)) +
  xlim(c(0,1)) + theme_bw() +
  xlab("Probabilité qu'un agent recoive un salaire") +
  ylab("") +
  facet_grid(prov ~ facility , scales = 'free_y'  )

plot3 <- subset(plot_df_pdr_prob , FacAppui == 'non')
ggplot(plot3, aes(x=mean, y =  post , col = factor(autorite)))+
  geom_point() +
  geom_errorbarh(aes(xmax = q1, xmin = q9 , height = .1)) +
  xlim(c(0,1)) + theme_bw() +
  xlab("Probabilité qu'un agent recoive une prime de risque") +
  ylab("") +
  facet_grid(prov ~ facility , scales = 'free_y'  )

plot4 <- subset(plot_df_pdr_prob , autorite == 0)
ggplot(plot4, aes(x=mean, y =  post , col = factor(FacAppui))) +
  geom_point() + geom_errorbarh(aes(xmax = q1, xmin = q9 , height = .2)) +
  xlim(c(0,1)) + theme_bw() +
  xlab("Probabilité qu'un agent recoive un salaire") +
  ylab("") +
  facet_grid(prov ~ facility , scales = 'free_y'  )


plot5 <- subset(plot_df_hon_amount , FacAppui == 'non' & autorite == 0)
ggplot(plot5, aes(x=mean, y =  post)) +
  geom_point() +
  geom_errorbarh(aes(xmax = q1, xmin = q9 , height = .1)) +
  theme_bw() +
  xlab("Montant d'honoraire recu") +
  ylab("") +
  facet_grid(prov ~ facility , scales = 'free'  )

plot6 <- subset(plot_df_hon_amount , autorite == 0)
ggplot(plot6, aes(x=mean, y =  post , col = factor(FacAppui))) +
  geom_point() + geom_errorbarh(aes(xmax = q1, xmin = q9 , height = .2)) +
  theme_bw() +
  xlab("Montant d'honoraire recu") +
  ylab("") +
  facet_grid(prov ~ facility , scales = 'free'  )


plot7 <- subset(plot_df_primePart_amount , FacAppui == 'non' & autorite == 0)
ggplot(plot5, aes(x=mean, y =  post)) +
  geom_point() +
  geom_errorbarh(aes(xmax = q1, xmin = q9 , height = .1)) +
  theme_bw() +
  xlab("Montant de prime partenaires recus") +
  ylab("") +
  facet_grid(prov ~ facility , scales = 'free'  )

plot8 <- subset(plot_df_primePart_amount , autorite == 0)
ggplot(plot6, aes(x=mean, y =  post , col = factor(FacAppui))) +
  geom_point() + geom_errorbarh(aes(xmax = q1, xmin = q9 , height = .2)) +
  theme_bw() +
  xlab("Montant de prime partenaires recus") +
  ylab("") +
  facet_grid(prov ~ facility , scales = 'free'  )