setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'modelisation'

source('code/useful_functions.r')


data <- subset(total_revenu , FacilityType %in% c('cs' , 'csr'))

data$variable[data$variable %in% c("Activité non santé" , "Autres revenus")] <- "Autres revenus"

revenus <- c("Salaire" , "Prime de Risque" , "Prime Locale" , "Heures supplémentaires" , "Prime de Partenaire" ,
             "Per Diem" , "Activité  Privée" , "Autres revenus")

data <- subset(data , Role != '')

table_sample <- dcast(table_sample , Role ~ Province + Sex)
output.table(table_sample , 'joint_report_table1')


##### Table 1


data_out <- data.frame(Role = unique(data$Role) , 
                       bandundu = numeric(length(unique(data$Role))) , 
                       equateur = numeric(length(unique(data$Role))) , 
                       katanga = numeric(length(unique(data$Role))) , 
                       sud_kivu = numeric(length(unique(data$Role))) )

variables <- c()


for(i in 1:length(revenus)){
  dd <- subset(data , variable == revenus[i])
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

data_out$Cadre <- unique(data$Role)
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

data_out$Cadre <- unique(data$Role)
output.table(data_out , 'joint_report_table3')

### Table 4






