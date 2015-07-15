setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'modelisation'

source('code/useful_functions.r')


data <- subset(total_revenu , FacilityType %in% c('cs' , 'csr')
               & !(FacOwnership == 'privee' & variable != "Heures supplémentaires")
               )

data$variable[data$variable %in% c("Activité non santé" , "Autres revenus")] <- "Autres revenus"
data$variable[data$variable %in% c('Cadeau'  , 'Vente de Medicament')] <- 'Informel'


facil_data <- subset(facilities , Structure %in% data$Structure & FacLevel != 'hgr')

facsDesc <- ddply(facil_data , .(Province , FacLevel) ,
                  function(x) table(x$FacRurban))

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



### Additional Graphs #####

##Ordering Revenues
rev_type <- read.csv('data/revenues_entry.csv')
rev_type <- subset(rev_type , select = c(RevenueLabel , RevenueEntry))
rev_type$RevenueLabel <- as.character(rev_type$RevenueLabel)
rev_type$RevenueEntry <- as.character(rev_type$RevenueEntry)

rev_type$RevenueEntry <- substr(rev_type$RevenueEntry , 1 , nchar(rev_type$RevenueEntry) - 2)


total_revenu <- merge(data , rev_type , 
                      by.x = 'variable' , by.y = 'RevenueLabel' , all.x = TRUE)


total_revenu$RevenueEntry <- total_revenu$variable
total_revenu$RevenueEntry[total_revenu$variable %in% c('Cadeau' , 'Vente de Medicament')] <- 'Informel' 
total_revenu$RevenueEntry[total_revenu$variable %in% c("Autres revenus")] <- 'Activité non santé' 



orderedIncome <- c('Salaire' , 'Prime de Risque' , 'Prime de Partenaire' , 'Per Diem' , 
                   'Prime Locale' , 'Activité  Privée' , 
                   'Activité non santé' , "Informel")


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

dist_role <- subset(dist_role , !is.na(dist_role$Role_french_graph))

pdf('output/graphs/median_income_distribution_JR.pdf' , width = 14)
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

dist_role_table <- dcast(dist_role , formula = Role_french_graph ~ RevenueEntry , value.var = 'V1')


output.table(dist_role_table , 'median_revenue_composition_JR')




#####
