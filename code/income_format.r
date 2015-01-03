setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'analysis'

source('code/useful_functions.r')

###Reconstituer tous les revenus








## Extract revenues from loops

melted_indiv <- melt(indiv , id = c("instanceID" , "Province" , "Sex" , "Age" , "Matrimonial" , 
                                  "NumberFinancialDependants" , "LastEduc" , 
                                  "FacilityType" , "Structure" , "Role"))

indiv_list <- subset(melted_indiv , select = -c(value , variable))

indiv_list <- indiv_list[!duplicated(indiv_list) , ]

extract_from_loop <- function(data , variable , value){
    out <- data.frame(PARENT_KEY =  data$PARENT_KEY , value = data[,value] , variable = variable)
    merge(indiv_list , out , by.y = 'PARENT_KEY' , by.x = 'instanceID' , all.y = TRUE)
}

revenue_entry <- read.csv('data/revenues_entry.csv' , as.is = TRUE)
nonIndivRev <- revenue_entry[revenue_entry$RevenueTable != 'indiv' ,]

tot_rev <- melted_indiv[melted_indiv$variable %in% revenue_entry$RevenueAmount ,]
tot_rev$variable <- as.character(tot_rev$variable)

for(i in 1:nrow(nonIndivRev)){
  tab <- eval(parse(text=nonIndivRev$RevenueTable[i]))
  var <- nonIndivRev$RevenueAmount[i]
  lab <- nonIndivRev$RevenueLabel[i]
  print(lab)
  out <- extract_from_loop(tab , lab , var)
  tot_rev <- rbind(tot_rev , out)
}
  
tot_rev <- subset(tot_rev , !is.na(value))


FacRelevant <- subset(facilities ,  select = c("Structure"  , "FacLevel" , "FacOwnership" , 
                                               "FacAppui" , "FacRurban" , "EczAppui"))
tot_rev_full <- merge(FacRelevant , tot_rev , by = 'Structure' , all.x = FALSE)







####Dropper valeurs aberrantes
melt_wage_facil <- subset(melt_indiv , !( (variable == 'WageDollar' & value > 4000) | 
                                            variable == 'PrimeDollar' & value > 1500))

loop_prime_partenaire <- subset(loop_prime_partenaire , 
                                CompSalaireDollar < 100000)


####Plot and Model
qplot(data = melt_wage_facil[melt_wage_facil$variable == 'WageDollar' ,] , 
      x = value , geom = 'jitter' , col = FacOwnership) + 
  facet_grid(Province ~ FacilityType) +
  theme_bw()

### Prime de Risque

qplot(data = melt_wage_facil , y = value , x = PrimeDollar , geom = 'jitter', col = FacOwnership) + 
  facet_grid(Province ~ FacilTypeEntry) +
  theme_bw()

### Prime Partenaire


qplot(data = loop_prime_partenaire , x = CompSalaireDollar ,
      binwidth = 5 , fill = PrimePartenaireFrequence) +
  facet_grid(PrimePartenaireVar~CompSalairePeriod , scales = 'free')


flat_prim <- ddply(loop_prime_partenaire , 
                   .(PARENT_KEY , PrimePartenaireVar , CompSalairePeriod) ,
                   function(x){
                     n <- nrow(x)
                     d <- sum(x$CompSalaireDollar)
                     data.frame(NInc = n ,
                                Amount = d)
                   })

flat_prim <- subset(flat_prim , PrimePartenaireVar != '')

qplot(data = flat_prim , x = Amount) +
  facet_grid(CompSalairePeriod~PrimePartenaireVar , scales = 'free')

flat_prim <- subset(flat_prim , Amount < 500 & 
                      !(CompSalairePeriod %in% c('autre' , '') ) )

NormalizeIncome <- function(Amount , Period){
  AmountNorm <- Amount
  AmountNorm[Period == 'bimestre_precedent'] <- Amount[Period == 'bimestre_precedent']/2
  AmountNorm[Period == 'trimestre_precedent'] <- Amount[Period == 'trimestre_precedent']/3
  AmountNorm
}

flat_prim$NormalDollar <- NormalizeIncome(flat_prim$Amount , flat_prim$CompSalairePeriod)

qplot(flat_prim$NormalDollar)

primIndiv <- ddply(flat_prim , .(PARENT_KEY)  ,
                   function(x) sum(x$NormalDollar))

qplot(primIndiv$V1)


### Partage de recettes
ii <- subset(indiv , HonoraireDollar < 750)

qplot(ii$HonoraireDollar)

indiv$HonoraireNorm <- NormalizeIncome(indiv$HonoraireDollar , indiv$GroupHonoraire.HonorairePeriod)

### Heures supplémentaires

qplot(data = indiv , x =HeureSupDollar) + 
  facet_wrap(~Province)

### Per Diems

NPerDiemed <- length(unique(loop_perdiem$PARENT_KEY))

table(loop_perdiem$PerDiemRaison)

count_perdiem <- as.data.frame(table(loop_perdiem$PARENT_KEY))
table(count_perdiem$Freq)

loop_perdiem <- subset(loop_perdiem , PerDiemDollar < 500)

qplot(data = loop_perdiem , x = PerDiemDollar) +
  facet_wrap(~PerDiemRaison , scales = 'free')

Flatten PerDiems

loop_perdiem <- subset(loop_perdiem , 
                       !(PerDiemDollar > 100 & PerDiemRaison == 'jnv') &
                         !(PerDiemDollar > 400 & PerDiemRaison == 'atelier'))

flat_perdiem <- ddply(loop_perdiem , .(PARENT_KEY) , 
                      function(x) sum(x$PerDiemDollar) / 3)

qplot(flat_perdiem$V1)


### Vente Médicament

table(indiv$IndivVendeMedicUB1)
table(indiv$IndivVendeMedicMontantExact == 0)

### Cadeaux

table(indiv$IndivCadeauUB1)
table(indiv$IndivCadeauMontantExact == 0)


### Activité privé

loop_activ_privee <- subset(loop_activ_privee , 
                            ActivPriveeDollar < 10000)

qplot(data = loop_activ_privee , x = ActivPriveeDollar) +
  facet_wrap(~ActPriveeLieu , scales = 'free')

table(loop_activ_privee$ActPriveeLieu)


loop_activ_privee <- subset(loop_activ_privee , 
                            ActivPriveeDollar < 800)

flat_act_privee <- ddply(loop_activ_privee , .(PARENT_KEY) , 
                         function(x) sum(x$ActivPriveeDollar))

qplot(flat_act_privee$V1)

### Activité non santé

Quelles sont ces activites ?

loop_activ_non_sante <- subset(loop_activ_non_sante , ActivNonSanteDollar < 5000)

qplot(data = loop_activ_non_sante , x = ActivNonSanteDollar) +
  facet_wrap(~ActNonSanteType , scales = 'free')


flat_ans <- ddply(loop_activ_non_sante , .(PARENT_KEY) ,
                  function(x) sum(x$ActivNonSanteDollar))

qplot(flat_ans$V1)


### Autre revenus

Quels sont ces revenus ?

qplot(loop_autre_revenu$AutreRevenuDollar)

__Recoder et reaffecter dans revenus prives plus haut si besoin__

### Reconstruction Revenu Mensuel

```{r}
out_income <- function(Id , Value , Income){
  data.frame(indiv = Id , value = Value , income = Income)
}

wage <- out_income(indiv$meta.instanceID , indiv$WageDollar , 'Salaire')
prime_risque <- out_income(indiv$meta.instanceID , indiv$PrimeDollar , 'Prime de Risque')
prime_partenaire <- out_income(primIndiv$PARENT_KEY , primIndiv$V1 , 
                               'Prime Partenaire')
per_diem <- out_income(flat_perdiem$PARENT_KEY , flat_perdiem$V1 , 'Per Diems')
honoraire <- out_income(indiv$meta.instanceID , indiv$HonoraireNorm ,
                        'Partage de Recettes')
heures_sup <- out_income(indiv$meta.instanceID , indiv$HeureSupDollar , 
                         'Heures Supplementaires')
act_privee <- out_income(flat_act_privee$PARENT_KEY , flat_act_privee$V1 , 
                         'Activite Privee')
act_non_sante <- out_income(flat_ans$PARENT_KEY , flat_ans$V1 , 
                            'Activite Non Sante')

data_flat <- rbind(wage , prime_risque , prime_partenaire , 
                   per_diem , honoraire , heures_sup , act_privee , act_non_sante)

data_flat <- subset(data_flat , !is.na(value))
data_flat$value <- as.numeric(as.character(data_flat$value))

total_income <- ddply(data_flat , .(indiv) , 
                      function(x) sum(x$value))


total_income <- subset(total_income  , V1 < 10000)

qplot(total_income$V1 , binwidth = 50)

total_income <- merge(total_income , indiv , by.x='indiv' , by.y='meta.instanceID')

ddply(total_income , .(ResumePost) , function(x)  median(x$V1))


```

```{r}
ForAnalysisIncome <- subset(data_flat , indiv %in% total_income$indiv)

bysource <- ddply(ForAnalysisIncome, .(income) , 
                  function(x) sum(x$value))

qplot(data = bysource , x = income , y = V1 , geom = 'bar' , stat = "identity")

FirstSourceOfIncome <- ddply(data_flat , .(indiv) , 
                             function(x){
                               mainIncome <- max(x$value , na.rm = TRUE)
                               val <- as.character(x$income[x$value == mainIncome])
                               if(length(val) > 1) val <- val[1]
                               val
                             }
)
N <- nrow(FirstSourceOfIncome)

ggplot(data  = FirstSourceOfIncome , aes(x= V1)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  ylab('% pour qui cela constitue le premier revenu') +
  xlab('Type de Revenu') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))




data_flat <- subset(data_flat , value < 5000)

matRev <- dcast(data_flat , 
                indiv ~ income , value.var = 'value' , 
                fun.aggregate = function(x) sum(x , na.rm = FALSE))


aa <- princomp(as.matrix(matRev[,-c(1)]) , cor = TRUE )

biplot(aa)

data_flat2 <- merge(data_flat , indiv_full , by.x = 'indiv' , by.y = 'meta.instanceID' ,
                    all = FALSE)



RevSum <- ddply(data_flat2 , .(indiv) ,  function(x) sum(x$value))

ordRev <- RevSum$indiv[order(RevSum$V1)]


data_flat2$indiv <- factor(data_flat2$indiv , levels =  ordRev , ordered = TRUE)

orderedIncome <- c('Salaire' , 'Prime de Risque' , 'Prime Partenaire' , 'Per Diems' , 
                   'Partage de Recettes' , 'Heures Supplementaires' , 'Activite Privee' , 
                   'Activite Non Sante')

data_flat2$income <- factor(data_flat2$income , levels =  orderedIncome , ordered = TRUE)

qplot(x = indiv, y = value , data=data_flat2, geom="bar", fill=factor(income) ,
      stat = 'identity') + 
  facet_wrap(~ResumePost , scales = 'free') + 
  theme(axis.text.x = element_blank())



classify_profile <- function(data){
  tot <- sum(data$value)
  hono <- sum(data$value[data$income %in% c('Partage de Recettes')])
  gouv <- sum(data$value[data$income %in% c('Salaire' , 'Prime de Risque')])
  part <- sum(data$value[data$income %in% c('Prime Partenaire' , 'Per Diems' , 
                                            'Heures Supplementaires')])
  outsider <- sum(data$value[data$income %in% c('Activite Privee' , 'Activite Non Sante')])
  data.frame(honoraire = hono , gouvernemental = gouv , partenaire = part , outside = outsider , total = tot)
}

profiles <- ddply(data_flat2 , .(indiv) , function(x) classify_profile(x))

profiles_melted <- melt(data = profiles , 
                        id = c("indiv" ) , 
                        value = c("honoraire" , "gouvernemental" , "partenaire"  , 
                                  "outside" , "total")
)


profiles_melted2 <- merge(profiles_melted , indiv_full , 
                          by.x = 'indiv' , by.y = 'meta.instanceID' ,
                          all.y = FALSE)

id <- ddply(profiles_melted2 , .(indiv) , function(x) nrow(x[x$variable == 'total' , ]))

id <- subset(id , V1 == 1)


toPlot <- subset(profiles_melted2 ,  ResumePost != '' & indiv %in% id$indiv)
tot <- toPlot[as.character(toPlot$variable) == 'total' , ]
ordRev <- tot$indiv[order(tot$value)]
orderedIncome <- c('honoraire' , 'gouvernemental' , 'partenaire' , 'outside' , 'total')

toPlot$indiv <- factor(toPlot$indiv , levels =  ordRev , ordered = TRUE)

toPlot$variable <- factor(toPlot$variable , levels =  orderedIncome , ordered = TRUE)
toPlot <- toPlot[order(toPlot$variable) , ]

toPlot2 <- subset(toPlot , variable != 'total')

qplot(y = value, x = indiv , data=toPlot2 , geom="bar", fill=variable ,
      stat = 'identity') +
  facet_wrap(~ResumePost , scales = 'free') +
  theme_bw()+ 
  theme(axis.text.x = element_blank())

data_infirmier <- subset(toPlot2 , ResumePost == 'infirmier' & value < 1500)

qplot(y = value, x = indiv , data=data_infirmier , geom="bar", fill=variable ,
      stat = 'identity') +
  theme_bw()+ 
  theme(axis.text.x = element_blank()) + ylim(c(0 , 1500))

data_medecin <- subset(toPlot2 , ResumePost == 'medecin')

qplot(x = indiv, y = value , data=data_medecin, geom="bar", fill=factor(variable) ,
      stat = 'identity') + 
  facet_wrap(~ResumePost , scales = 'free') + 
  theme(axis.text.x = element_blank())

data_ecz <- subset(toPlot2 , ResumePost %in% c('administrateur_gestionnaire' ,
                                               'infirmier_superviseur' , 'medecin_chef_zone') )
qplot(x = indiv, y = value , data=data_ecz, geom="bar", fill=factor(variable) ,
      stat = 'identity') + 
  facet_wrap(~ResumePost , scales = 'free') + 
  theme(axis.text.x = element_blank())




GetType <- function(x , threshold){
  Type <- 'mixte'
  if(x$value[x$variable == 'honoraire'] /  x$value[x$variable == 'total'] > threshold){
    Type <- 'honoraire'
  }
  if(x$value[x$variable == 'gouvernemental'] /  x$value[x$variable == 'total'] > threshold){
    Type <- 'gouvernemental'
  }
  if(x$value[x$variable == 'partenaire'] /  x$value[x$variable == 'total'] > threshold){
    Type <- 'partenaire'
  }
  if(x$value[x$variable == 'outside'] /  x$value[x$variable == 'total'] > threshold){
    Type <- 'outside'
  }
  Type
}

typed <- ddply(toPlot , .(indiv) , 
               function(x) GetType(x , 0.6)
)

table(typed$V1)

table(typed$V1) / nrow(typed)


colnames(typed)[2] <- 'Profile'

totalIncome <- subset(toPlot , variable == 'total')

totalIncome <- merge(totalIncome , typed)

ddply(totalIncome , .(Profile) , 
      function(x) median(x$value))
```


### Determinants

```{r}
colnames(matRev) <- c("indiv" , "Salaire"  , "Prime_de_Risque","Prime_Partenaire","Per_Diems" , "Partage_de_Recettes","Heures_Supplementaires","Activite_Privee","Activite_Non Sante" )
indout <- ddply(indiv_full , .(meta.instanceID) , nrow)
indout <- subset(indout , V1 >1)
indiv_model <- subset(indiv_full , !(meta.instanceID %in% indout$meta.instanceID))

modelData <- merge(matRev , indiv_model , all.x = FALSE , all.y = FALSE , by.x = 'indiv' , by.y = 'meta.instanceID')

####Les logistiques pour le plaisir


library(lme4)
##Probabilite d'avoir un salaire

modelData$ecz <- as.numeric(modelData$FacLevel %in% c('ecz'))
modelData$hgrcs <- as.numeric(modelData$FacLevel %in% c('hgr' , 'cs'))
modelData$wage <- modelData$WageYN == 'oui'

##

modelData$FacRurban[modelData$FacRurban == ''] <- 'NA'

modelData$LastEducation <- modelData$GroupDemographics.IndivLastEduc
modelData$LastEducation [modelData$LastEducation  %in%
                           c('medecin_generaliste' , 'medecin_specialiste' ,
                             'pharmacien' , 'diplome_etudes_superieures')] <- 'medecin-pharma-etudesup'

modelData$LastEducation[modelData$LastEducation %in% 
                          c('gestion_administration' , 'gestion_administration autre'  , 'autre')] <-
  'autre'

modelData$LastEducation[modelData$LastEducation %in% 
                          c('infirmiere_ao' , 'infirmiere_a1' ,
                            'technicien_labo' , 'infirmiere_a2' )] <- 'a0-a1-a2'

modelData$LastEducation[modelData$LastEducation %in% 
                          c('infirmiere_a3')] <- 'a3'

modelData$LastEducation[modelData$LastEducation %in%c('')] <- NA


modelData$PostECZ <- modelData$ResumePost
modelData$PostECZ[modelData$FacLevel == 'ecz'] <- NA

modelData$PostCS <- modelData$ResumePost
modelData$PostCS[modelData$FacLevel != 'ecz'] <- NA

disjunctif <- model.matrix( ~ ResumePost , data = modelData)
colnames(disjunctif) <- c('x' , 'administrateur' , 'administrateur_gestionnaire' , 'autre' , 'infirmier' , 
                          'infirmier_superviseur' , 'labo' , 'medecin' , 'mcz' , 'pharmacien')
modelData <- cbind(modelData , disjunctif)


modelData$Power[modelData$DataPostInitHGR %in% c('directeur_nursing' , 
                                                 'medecin_chef_staff' ,
                                                 'medecin_directeur')] <- 1
modelData$Power[modelData$DataPostInitCS %in% c('medecin','infirmier_titulaire' ,
                                                'medecin_directeur')] <- 1

modelData$Power[is.na(modelData$Power)] <- 0

```

On modélise 

Etapes :
  
  1. declarer les covariables du modele
2. creer le modele en ajoutant le y
3. extraire les donneees avec `simcf`
4. faire tourner (tous les modeles)
5. constituer les cf
6. calculer expected values from cf
7. compile cfs dans plotting frames
8. plot


2 -> 4 peuvent se faire dans une boucle

```{r}

make_formula <- function(y , covariates){
  formula <- paste(var,  covariates, sep = ' ~ ')
  as.formula(formula)
}

covs_indiv <- "GroupDemographics.IndivSex + GroupDemographics.IndivAge + LastEducation" 
covs_hgrcs <-  "administrateur + autre + infirmier +  labo + medecin + pharmacien+ FacAppui"
covs_ecz <- "administrateur_gestionnaire  + autre  + infirmier_superviseur + mcz + EczAppui"
covs_hfsgen <- "FacRurban + FacLevel + Power  + (1|Province)"

covs_hgrcs <- paste(covs_indiv , covs_hgrcs , covs_hfsgen, sept = "+")
covs_ecz <- paste(covs_indiv , covs_ecz , covs_hfsgen, sept = "+")



prob_salaire <- glmer(make_formula('wage') ,
                      family = binomial(link = 'logit') , 
                      data = modelData)
summary(prob_salaire)

modelData$PrimeRisque <- modelData$PrimeRisqueYN == 'oui'
prob_pdr <- glmer(make_formula('PrimeRisque') ,
                  family = binomial(link = 'logit') , 
                  data = modelData)
summary(prob_pdr)

modelData$Partage_de_Recettes[is.na(modelData$Partage_de_Recettes) |
                                modelData$Partage_de_Recettes == 0] <- 1

amount_honoraire <- lmer(make_formula('log(Partage_de_Recettes)'), 
                         data = modelData)
summary(amount_honoraire)

modelData$Prime_Partenaire[is.na(modelData$Prime_Partenaire) |
                             modelData$Prime_Partenaire == 0] <- 1
modelData$Prime_Partenaire[modelData$Prime_Partenaire < 0 |
                             modelData$Prime_Partenaire > 10000] <- NA

amount_prime <- lmer(make_formula('log(Prime_Partenaire)') ,
                     data = modelData)
summary(amount_prime)

```



```{r}
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

plot_df_wage_prob <- make_plot_df(counterfacts_province , simulateLogit , prob_salaire , 10000 , 'salaire')
plot_df_pdr_prob <- make_plot_df(counterfacts_province , simulateLogit , prob_pdr , 10000 , 'prime de risque')
plot_df_hon_amount <- make_plot_df(counterfacts_province , amount_predict , amount_honoraire , 10000 , 'honoraire')
plot_df_primePart_amount <- make_plot_df(counterfacts_province , amount_predict , amount_prime , 10000 , 'top up')
```


```{r, fig.width=10}  

plot1 <- subset(plot_df_wage_prob , FacAppui == 'non')
ggplot(plot1, aes(x=mean, y =  post , col = factor(autorite))) +
  geom_point() + geom_errorbarh(aes(xmax = q1, xmin = q9 , height = .2)) +
  xlim(c(0,1)) + theme_bw() +
  xlab("Probabilité qu'un agent recoive un salaire") +
  ylab("") +
  facet_grid(prov ~ facility , scales = 'free_y'  )

plot2 <- subset(plot_df_wage_prob , autorite == 0)
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