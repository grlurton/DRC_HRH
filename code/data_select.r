setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'select'

source('code/useful_functions.r')



#######################
## Data Preparation ###
#######################

### Taking data training data out

# Load sampling data

sample_final <- read.csv('method/sampling/samplefinal.csv' , strip.white=TRUE)

# Standardize the names

sample_final$Province <- UnifyNames(sample_final$Province)
sample_final$Zone <- UnifyNames(sample_final$Zone)

# Set data collection dates

start_dates <- data.frame(province = c( 'bandundu' , 'sud_kivu' , 'equateur' , 'katanga') ,
                          date_deb = c(as.Date('2014/10/20') , as.Date('2014/10/22') , 
                                       as.Date('2014/10/21') , as.Date('2014/10/23')))

# Only keep data collected after data collection start

SubsetCollection <- function(data_frame , starting_dates){
  data_frame$StartTime <- GetTime(data_frame$StartTime)
  data_frame$StartDay <- GetDate(data_frame$StartTime )
  
  data_frame <- merge(data_frame , starting_dates , 
                      by.x = 'structuremystructure_province' , by.y = 'province' )
  
  data_frame <- subset(data_frame , StartDay >= date_deb)
  
  data_frame
}

indiv <- SubsetCollection(indiv , start_dates)
facilities <- SubsetCollection(facilities , start_dates)

### Taking out doublons facilities

NQuestFac <- ddply(facilities , .(structuremystructure) , nrow)

doublons_names <- subset(NQuestFac , V1 > 1)

doublons_forms <- subset(facilities , structuremystructure %in% doublons_names$structuremystructure)
doublons_forms <- doublons_forms[order(doublons_forms$structuremystructure),]

#View(doublons_forms)

NonDoublon <- c('uuid:c0e3e131-ba3d-43f6-80e5-727496d07202' , 'uuid:c34d008c-a93a-4d24-afff-94388bfeb7da' ,
                'uuid:ab4cf74d-b5f3-4689-a0f0-614aa392e63b' , 'uuid:452a4c2f-c8f6-4332-9815-b43de15d02ee' ,
                'uuid:c073658b-b8a4-42b5-adfc-a6c82cd1345b' , 'uuid:d77b6385-f23c-4104-ac8e-896b5c26217f' ,
                'uuid:8d2f1c07-c36c-49f2-9402-1aa53d6e6e73' , 'uuid:60f7e570-5759-46de-954d-2bf1844b24af' ,
                'uuid:bf7e5583-a6b3-41a3-bf41-523d48350150' , 'uuid:3c02b95f-f8be-4976-bcdc-39b024c73bcb' ,
                'uuid:73b29e45-fe87-497a-96ac-c52e5c82b92a' , 'uuid:2068316f-6cde-48c1-aafd-a3175fa90531' ,
                'uuid:bd394797-000a-454b-a05a-bc95c4b20e2e' , 'uuid:dc7c232b-2741-47d2-b6e6-4a7ada617f9d' ,
                'uuid:987be58a-0d7a-40d6-ad1c-31d48393fe20' , 'uuid:b8dff064-5729-4a40-9366-231f2d23d7b0' ,
                'uuid:60c34187-2d6d-47b3-b4d3-1498408af0c0' , 'uuid:fc4b9d2a-11da-4d72-92f4-4f7e67c7a795' ,
                'uuid:fd50da55-e3b4-408d-a0df-e27c953a5d6c' , 'uuid:7e790b6c-9359-4ae2-8519-4daedb1099be' )

NonDoublon2 <- facilities$meta.instanceID[!(facilities$meta.instanceID %in% doublons_forms$meta.instanceID)]


facilities <- subset(facilities , meta.instanceID %in% c(NonDoublon , NonDoublon2))

## Subsetting to only facilities for which we have matching patients et vice versa

facilities$NomUnique <- paste(facilities$structuremystructure_zone , facilities$structuremystructure , sep = ' - ')

sub_fac <- data.frame(Nom = facilities$NomUnique, 
                      Province = facilities$structuremystructure_province ,
                      Level = facilities$FacLevel)

indiv$NomUnique <- paste(indiv$structuremystructure_zone , indiv$structuremystructure , sep = ' - ')
sub_indiv <- data.frame(Nom = indiv$NomUnique , ID = indiv$meta.instanceID)

tabIndiv <- merge(sub_indiv , sub_fac , by = 'Nom')

indiv <- subset(indiv , NomUnique %in% tabIndiv$Nom)
facilities <- subset(facilities , NomUnique %in% tabIndiv$Nom)

# Finally subsetting also the loops data to keep only the data that has parents in facility or individual 
# questionnaire

loop_appui_zs <- subset(loop_appui_zs , PARENT_KEY %in% facilities$meta.instanceID)
loop_appui_fac <- subset(loop_appui_fac , PARENT_KEY %in% facilities$meta.instanceID)
loop_activ_non_sante <- subset(loop_activ_non_sante , PARENT_KEY %in% indiv$meta.instanceID)
loop_activ_privee <- subset(loop_activ_privee , PARENT_KEY %in% indiv$meta.instanceID)
loop_autre_revenu <- subset(loop_autre_revenu , PARENT_KEY %in% indiv$meta.instanceID)
loop_perdiem <- subset(loop_perdiem , PARENT_KEY %in% indiv$meta.instanceID)
loop_prime_partenaire <- subset(loop_prime_partenaire , PARENT_KEY %in% indiv$meta.instanceID)

## Export selected data

write.csv(loop_appui_zs , 'data/questionnaires_temp/loop_appui_zs_select.csv')
write.csv(loop_appui_fac , 'data/questionnaires_temp/loop_appui_fac_select.csv')
write.csv(loop_activ_non_sante , 'data/questionnaires_temp/loop_activ_non_sante_select.csv')
write.csv(loop_activ_privee , 'data/questionnaires_temp/loop_activ_privee_select.csv')
write.csv(loop_autre_revenu , 'data/questionnaires_temp/loop_autre_revenu_select.csv')
write.csv(loop_perdiem , 'data/questionnaires_temp/loop_perdiem_select.csv')
write.csv(loop_prime_partenaire , 'data/questionnaires_temp/loop_prime_partenaire_select.csv')
write.csv(indiv , 'data/questionnaires_temp/individual_data_select.csv')
write.csv(facilities , 'data/questionnaires_temp/facility_data_select.csv')

rm(list = ls())
