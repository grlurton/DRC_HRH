setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'modelisation'

source('code/useful_functions.r')


data <- subset(total_revenu , FacilityType %in% c('cs' , 'csr') & !(FacOwnership == 'privee'))

data$variable[data$variable %in% c("Activité non santé" , "Autres revenus")] <- "Autres revenus"
data$variable[data$variable %in% c('Cadeau'  , 'Vente de Medicament')] <- 'Informel'

##### Table 1

revenus <- c("Salaire" , "Prime de Risque" , "Prime Locale" , "Heures supplémentaires" , "Prime de Partenaire" ,
             "Per Diem" , "Activité  Privée" , "Autres revenus" , 'Informel')

data <- subset(data , Role != '')

indiv <- merge(indiv , facilities , by.x = c('Province' , 'Zone' , 'Structure') ,
               by.y = c('Province' , 'Zone' , 'Structure')  , all.x = TRUE)

table_sample <- ddply(indiv[indiv$FacilityType %in% c('cs' , 'csr') &
                              !(indiv$FacOwnership == 'privee') & indiv$Role != '' , ] ,
                      .(Role , Province , Sex) ,
                      function(x){
                          length(unique(x$instanceID.x))
                      })

table_sample <- dcast(table_sample , Role ~ Province + Sex , fill = 0)
output.table(table_sample , 'joint_report_table1')


##### Table 2


data_out <- data.frame(Role = unique(data$Role) ,
                       bandundu = numeric(length(unique(data$Role))) ,
                       equateur = numeric(length(unique(data$Role))) ,
                       katanga = numeric(length(unique(data$Role))) ,
                       sud_kivu = numeric(length(unique(data$Role))) )

variables <- c()

for(i in 1:length(revenus)){
  dd <- subset(data , variable == revenus[i])
  print(revenus[i])
  table <- ddply(dd , .(Role , Province) ,
                 function(data){
                   data <- subset(data , value > 0)
                   length(unique(data$instanceID))
                 })
  table_reshaped <- dcast(table , Role ~ Province , fill = 0)
  var <- c(paste(c('bandundu' , 'equateur' , 'katanga' , 'sud_kivu'),
                          revenus[i] , sep = '_'))
  colnames(table_reshaped) <-   c('Role' , var)
  data_out <- cbind(data_out , table_reshaped)
  variables <- c(variables , var)
}
data_out <- subset(data_out , select = variables)

data_out$Cadre <- sort(unique(data$Role))
output.table(data_out , 'joint_report_table2')

#### Table 2

data_out <- data.frame(Role = unique(data$Role) ,
                       bandundu = numeric(length(unique(data$Role))) ,
                       equateur = numeric(length(unique(data$Role))) ,
                       katanga = numeric(length(unique(data$Role))) ,
                       sud_kivu = numeric(length(unique(data$Role))) )

variables <- c()

data <- subset(data , !(variable == 'Prime de Risque' & value > 200))

for(i in 1:length(revenus)){
  dd <- subset(data , variable == revenus[i])
  print(revenus[i])
  table <- ddply(dd , .(Role , Province) ,
                 function(data){
                   data <- subset(data , value > 0)
                   mean(data$value , na.rm = TRUE)
                 })
  table_reshaped <- dcast(table , Role ~ Province , fill = NA)
  var <- c(paste(c('bandundu' , 'equateur' , 'katanga' , 'sud_kivu'),
                          revenus[i] , sep = '_'))
  colnames(table_reshaped) <-   c('Role' , var)
  data_out <- cbind(data_out , table_reshaped)
  variables <- c(variables , var)
}
data_out <- subset(data_out , select = variables)

data_out$Cadre <- sort(unique(data$Role))
output.table(data_out , 'joint_report_table3')

### Table 4


tab4 <- ddply(data  , .(instanceID , Role , Province) ,
              function(data){
                nrow(data[data$variable == 'Per Diem' , ])
              })

tab4 <- dcast(tab4 , Role ~ Province + V1)
output.table(tab4 , 'joint_report_table4')

### Table 5

tab5 <- ddply(data  , .(instanceID , Province , Role) ,
              function(data){
                nrow(data[data$variable == 'Prime de Partenaire' , ])
              })
tab5 <- dcast(tab5 , Role ~ Province + V1)
output.table(tab5 , 'joint_report_table5')


#Table 6 => ajouter aucune activite

iga <- read.csv('data/questionnaires_analysis/loop_activ_non_sante_select.csv')

iga <- merge(indiv , iga , by.x = 'instanceID.x' , by.y = 'PARENT_KEY' , all = TRUE)

iga <- subset(iga , instanceID.x %in% data$instanceID)

iga$ActNonSanteType <- as.character(iga$ActNonSanteType)

iga$ActNonSanteType[iga$ActNonSanteType %in% c('Agriculture-elevage-peche-chasse' ,
                                               'agriculture_elevage')] <- 'Agriculture, élevage, chasse'
iga$ActNonSanteType[iga$ActNonSanteType %in% c('autre', 'Autres')] <- 'Autre'
iga$ActNonSanteType[iga$ActNonSanteType %in% c('location maison', 'location_maison')] <-
  'location maison'
iga$ActNonSanteType[iga$ActNonSanteType %in% c('negoce-commerce', 'negoce_commerce')] <-
  'Négoce, Commerce'
iga$ActNonSanteType[iga$ActNonSanteType %in% c('location moyens de transport', 'taxi')] <-
  'Taxi'



tab6 <- as.data.frame(table(iga$ActNonSanteType))
output.table(tab6 , 'joint_report_table6')
