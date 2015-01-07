## Data Cleaning

setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'cleaning'

source('code/useful_functions.r')


## Loading Data from ODK Briefcase

### Standardize Currency

indiv <- StandardizeOver(indiv , 'GroupWage.WageLastAmount' , 'GroupWage.WageLastUnit' ,
                         'Wage')

indiv <- StandardizeOver(indiv , 'GroupPrimeRisque.PrimeRisqueLastAmount' , 'GroupPrimeRisque.PrimeRisqueLastUnit' , 
                         'Prime')

loop_prime_partenaire <- StandardizeOver(loop_prime_partenaire ,
                                         'PrimePartenairesAmount' ,
                                         'CompSalaireUnit' ,
                                         'CompSalaire')

indiv <- StandardizeOver(indiv , 'HeureSupGroup.HeureSupAmount' , 'HeureSupGroup.HeureSupUnit' ,
                         'HeureSup')

indiv <- StandardizeOver(indiv , 'GroupHonoraire.HonoraireValue' , 'GroupHonoraire.HonoraireUnit' ,
                         'Honoraire')

loop_perdiem <- StandardizeOver(loop_perdiem , 'PerDiemValue' , 'PerDiemUnit' ,
                                'PerDiem')

loop_activ_privee <- StandardizeOver(loop_activ_privee ,'ActPriveeAmount' , 'ActPriveeUnit' ,
                                     'ActivPrivee')

loop_activ_non_sante <- StandardizeOver(loop_activ_non_sante , 'ActNonSanteAmount' , 'ActNonSanteUnit' ,
                                        'ActivNonSante')

loop_autre_revenu <- StandardizeOver(loop_autre_revenu , 'AutreRevenuAmount' , 'AutreRevenuUnit' ,
                                     'AutreRevenu')

## Reclassification of interviews in multiple Units

indiv$FacilTypeEntry[indiv$FacilTypeEntry %in% c('ecz cs' , 'ecz hgr' , 'hgr cs') & 
                       indiv$GroupECZQuests.IndivECZRole == 'medecin_chef_zone'] <- 'ecz'

indiv$FacilTypeEntry[indiv$FacilTypeEntry %in% c('ecz cs' , 'ecz hgr' , 'hgr cs') & 
                       indiv$GroupECZQuests.IndivECZRole == 'medecin_directeur_hopital'] <- 'hgr'

indiv$FacilTypeEntry[indiv$FacilTypeEntry %in% c('ecz cs' , 'ecz hgr' , 'hgr cs') & 
                       indiv$GroupECZQuests.IndivECZRole == 'infirmier_superviseur'] <- 'ecz'

indiv$FacilTypeEntry[indiv$FacilTypeEntry %in% c('ecz cs' , 'ecz hgr' , 'hgr cs') & 
                       indiv$GroupECZQuests.IndivECZRole == 'directeur_nursing'] <- 'hgr'

indiv$FacilTypeEntry[indiv$FacilTypeEntry %in% c('ecz hgr') & 
                       indiv$GroupECZQuests.IndivECZRole == 'autre'] <- 'hgr'

indiv$FacilTypeEntry[indiv$FacilTypeEntry %in% c('hgr cs') & 
                       indiv$GroupECZQuests.IndivECZRole == ''] <- 'hgr'


## Recode ecz medecin directeur and directeur nursing to hgr 

table(indiv$GroupECZQuests.IndivECZRole[indiv$FacilTypeEntry == 'ecz'] , 
      indiv$GroupHGR.IndivHGRPost[indiv$FacilTypeEntry == 'ecz'] )

zonesSwitch <- indiv$structuremystructure_zone[indiv$FacilTypeEntry == 'ecz' &
                                                 indiv$GroupECZQuests.IndivECZRole %in% c('directeur_nursing' ,
                                                                                          'medecin_directeur_hopital')]

indiv$structuremystructure[indiv$FacilTypeEntry == 'ecz' &
                             indiv$GroupECZQuests.IndivECZRole %in% c('directeur_nursing' ,
                                                                      'medecin_directeur_hopital')] <-
  paste('hgr_',zonesSwitch , sep='')


indiv$GroupHGR.IndivHGRPost[indiv$FacilTypeEntry == 'ecz' &
                              indiv$GroupECZQuests.IndivECZRole %in% c('directeur_nursing' ,
                                                                       'medecin_directeur_hopital')] <-
  indiv$GroupECZQuests.IndivECZRole[indiv$FacilTypeEntry == 'ecz' &
                                      indiv$GroupECZQuests.IndivECZRole %in% c('directeur_nursing' ,
                                                                               'medecin_directeur_hopital')]
indiv$GroupECZQuests.IndivECZRole[indiv$FacilTypeEntry == 'ecz' &
                                    indiv$GroupECZQuests.IndivECZRole %in% c('directeur_nursing' ,
                                                                             'medecin_directeur_hopital')] <-
  ''

indiv$FacilTypeEntry[indiv$FacilTypeEntry == 'ecz' &
                       indiv$GroupECZQuests.IndivECZRole %in% c('directeur_nursing' ,
                                                                'medecin_directeur_hopital')] <-
  'hgr'


### Recode Posts in facilities

### Recode health resource categories => HAVE THAT IN EXCEL INPUT 

RecodePost <- function(PostInit){
  PostInit[PostInit %in% c('administrateur' , 'administrateur_gestionnaire')] <- 'administrateur'
  
  PostInit[PostInit %in% c('autre' , 'Autre')] <- 'autre'
  
  PostInit[PostInit %in% c("membre_du_personnel" , "membre_personnel")] <- 'autre'
  
  PostInit[PostInit %in% c("technicien_labo" , "technicien_laboratoire")] <- 'labo'
  
  PostInit[PostInit %in%  c('infirmier' , 'infirmier_superviseur' , 
                            'infirmier_titulaire' , 'directeur_nursing')] <- 'infirmier'
  
  PostInit[PostInit %in% c("medecin" , "medecin_chef_staff" , "medecin_directeur" ,
                           "medecin_generaliste_specialiste")] <- 'medecin'
  PostInit
}

indiv$DataPostInitCS <- indiv$GroupCS.IndivCSPost
indiv$DataPostInitHGR <- indiv$GroupHGR.IndivHGRPost
indiv$DataPostInitECZ <- indiv$GroupECZQuests.IndivECZRole


indiv$GroupCS.IndivCSPost <- RecodePost(indiv$GroupCS.IndivCSPost)
indiv$GroupHGR.IndivHGRPost <- RecodePost(indiv$GroupHGR.IndivHGRPost)



### Recode some facilities characteristics

facilities$FacRurban[facilities$FacRurban == 'semiurbain'] <- 'urbain'
facilities$FacLevel[facilities$FacLevel %in% c('autre')] <- 'cs'


ZoneMilieu <- function(data){
  if(sum(data$FacLevel == 'hgr') >= 1){
    out <- data$FacRurban[data$FacLevel == 'hgr'][1]
    print(out)
  }
  if(sum(data$FacLevel == 'hgr') == 0) out <- NA
  out
}

zonezs <- ddply(facilities , .(structuremystructure_zone) , 
                ZoneMilieu)

for(zone in facilities$structuremystructure_zone){
  facilities$FacRurban[facilities$structuremystructure_zone == zone &
                         facilities$FacLevel == 'ecz'] <- 
    zonezs$V1[zonezs$structuremystructure_zone == zone]  
}


## Rename  

varnames <- read.csv('data/variable_names.csv' , as.is = TRUE)

simplifNames <- function(data , varnames , set){
  for(i in 1:ncol(data)){
    varnames <- subset(varnames , df == set)
    nameInit <- colnames(data)[i]
    if(nameInit %in% varnames$colnam){
      nameNew <- varnames[varnames$colnam == nameInit , 'newName']
      colnames(data)[i] <- nameNew
    }
  }
#  data <- subset(data , select = varnames$newName)
  data
}


### Autre

recode_autre <- read.csv('data/recode_source_autre_revenu.csv' , as.is = TRUE)
loop_autre_revenu <- merge(loop_autre_revenu , recode_autre , 
                           by.x = 'AutreRevenuSource' , by.y = 'Autre')


autre_for_prive <- subset(loop_autre_revenu , Recode == 'prestation des soins de santé')
autre_for_non_sante <- subset(loop_autre_revenu , 
                              !(Recode %in% c('prestation des soins de santé' , 'famille') ) 
                              )
                                          
autre_for_prive <- data.frame(X = autre_for_prive$X ,
                              ActPriveeLieu = autre_for_prive$Recode ,
                              ActPriveeAmount = autre_for_prive$AutreRevenuAmount ,
                              ActPriveeUnit = autre_for_prive$AutreRevenuUnit ,
                              PARENT_KEY = autre_for_prive$PARENT_KEY ,
                              KEY = autre_for_prive$KEY , 
                              SET.OF.ActPriveeGroup = '' ,
                              ActivPriveeDollar = autre_for_prive$AutreRevenuDollar ,
                              ActivPriveeFC = autre_for_prive$AutreRevenuFC
                              )

autre_for_non_sante <- data.frame(X = autre_for_non_sante$X ,
                              ActNonSanteType = autre_for_non_sante$Recode ,
                              ActNonSanteProprio = '' ,
                              ActNonSanteAmount = autre_for_non_sante$AutreRevenuAmount ,
                              ActNonSanteUnit = autre_for_non_sante$AutreRevenuUnit ,
                              PARENT_KEY = autre_for_non_sante$PARENT_KEY ,
                              KEY = autre_for_non_sante$KEY , 
                              SET.OF.ActNonSanteGroup = '' ,
                              ActivNonSanteDollar = autre_for_non_sante$AutreRevenuDollar ,
                              ActivNonSanteFC = autre_for_non_sante$AutreRevenuFC
                              )

loop_activ_privee <- rbind(loop_activ_privee , autre_for_prive)
loop_activ_non_sante <- rbind(loop_activ_non_sante , autre_for_non_sante)

## Simplify

loop_activ_non_sante <- simplifNames(loop_activ_non_sante , varnames , 'loop_activ_non_sante')
loop_activ_privee <- simplifNames(loop_activ_privee , varnames , 'loop_activ_prive')
loop_appui_fac <- simplifNames(loop_appui_fac , varnames , 'loop_appui_fac')
loop_appui_zs <- simplifNames(loop_appui_zs , varnames , 'loop_appui_zs')
loop_autre_revenu <- simplifNames(loop_autre_revenu , varnames , 'loop_autre_rev')
loop_perdiem <- simplifNames(loop_perdiem , varnames , 'loop_perdiem')
loop_prime_partenaire <- simplifNames(loop_prime_partenaire , varnames , 'loop_prim_part')
indiv <- simplifNames(indiv , varnames , 'individuals')
facilities <- simplifNames(facilities , varnames , 'facilities')

## Creating Unique Role Variable

indiv$Role <- indiv$CSRole
indiv$Role[indiv$FacilityType == 'hgr'] <-indiv$HGRRole[indiv$FacilityType == 'hgr']
indiv$Role[indiv$FacilityType == 'ecz'] <- indiv$DataPostInitECZ[indiv$FacilityType == 'ecz']



## Export selected data

write.csv(loop_appui_zs , 'data/questionnaires_analysis/loop_appui_zs_select.csv')
write.csv(loop_appui_fac , 'data/questionnaires_analysis/loop_appui_fac_select.csv')
write.csv(loop_activ_non_sante , 'data/questionnaires_analysis/loop_activ_non_sante_select.csv')
write.csv(loop_activ_privee , 'data/questionnaires_analysis/loop_activ_privee_select.csv')
write.csv(loop_autre_revenu , 'data/questionnaires_analysis/loop_autre_revenu_select.csv')
write.csv(loop_perdiem , 'data/questionnaires_analysis/loop_perdiem_select.csv')
write.csv(loop_prime_partenaire , 'data/questionnaires_analysis/loop_prime_partenaire_select.csv')
write.csv(indiv , 'data/questionnaires_analysis/individual_data_select.csv')
write.csv(facilities , 'data/questionnaires_analysis/facility_data_select.csv')

rm(list = ls())