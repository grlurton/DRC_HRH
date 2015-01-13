## Basic Descriptives

setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'analysis'

source('code/useful_functions.r')


NStructures <- nrow(facilities)
NInterviews <- length(unique(indiv$instanceID))

######Description de l'echantillon######


## Interviews for facilities

DistFacilities <- table(facilities$FacLevel)
output.table(DistFacilities , 'facilities_by_FacilType')

fac_by_prov <- table(facilities$Province , facilities$FacLevel)
output.table(fac_by_prov , 'facilities_by_Province_FacilType')

fac_comp_explose <- ddply(facilities , .(Province , FacLevel , FacRurban , FacOwnership ) , nrow)
output.table(fac_comp_explose , 'facilities_complete_table')

## Entretiens Individuels

DistInterViews <- ddply(indiv , .(FacilityType) , function(x) nrow(x))
output.table(DistInterViews , 'interviews_by_FacilType')

indiv_by_prov <- table(indiv$Province , indiv$FacilityType)
output.table(indiv_by_prov , 'interviews_by_Province_FacilType')

TabInterviews <- function(Role , Type , data ){
  out <- as.data.frame(table(data[data$FacilityType == Type , Role],
                             data$Sex[data$FacilityType == Type])  )
  print(head(out))
  colnames(out) <- c('Role' , 'Sexe' , 'Nombre')
  out
}

TabInterviews_forStruct <- function(Role, Type, data){
  ddply(data , .(Province) , 
        function(x){
          TabInterviews(Role, Type , x)
        } )
}


cs_role_by_province <- TabInterviews_forStruct('CSRole' , 'cs' , indiv)
output.table(cs_role_by_province , 'cs_role_by_province')

ecz_role_by_province <- TabInterviews_forStruct('ECZRole' , 'ecz' , indiv)
output.table(ecz_role_by_province , 'ecz_role_by_province')

hgr_role_by_province <- TabInterviews_forStruct('HGRRole' , 'hgr' , indiv)
output.table(hgr_role_by_province , 'hgr_role_by_province')


### Support reçu par les strctures de santé

tabAppui <- ddply(loop_appui_fac , .(PARENT_KEY) , function(x) data.frame(NAppui = nrow(x)))

tabAppuiFull <- MergeLoop(facilities , tabAppui)

tabAppuiFull$NAppui[is.na(tabAppuiFull$NAppui)] <- 0

tab_appui_full <- ddply(tabAppuiFull , .(Province , FacLevel , FacRurban , FacOwnership ) , nrow)
output.table(tab_appui_full , 'fac_number_appui')

### Staffing 

#### ECZ

staff_ecz <- function(facilities){
  ecz <- subset(facilities , FacLevel == 'ecz')
  N <- nrow(ecz)
  MCZ <- c(sum(ecz$ECZMCZNbre == 'oui') ,
           sum(ecz$ECZMCZImmatricule == 'oui') ,
           sum(ecz$ECZMCZMecanise == 'oui') ,
           sum(ecz$ECZMCZPresent == 'oui') )
  MDH <- c(sum(ecz$ECZMDHNbre == 'oui') ,
           sum(ecz$ECZMDHImmatricule == 'oui') ,
           sum(ecz$ECZMDHMecanise == 'oui') ,
           sum(ecz$ECZMDHPresent == 'oui') )
  DN <- c(sum(ecz$ECZDNNbre == 'oui') ,
          sum(ecz$ECZDNImmatricule == 'oui') ,
          sum(ecz$ECZDNMecanise == 'oui') ,
          sum(ecz$ECZDNPresent == 'oui') )
  AG <- c(sum(ecz$ECZAGNbre == 'oui') ,
          sum(ecz$ECZAGImmatricule == 'oui') ,
          sum(ecz$ECZAGMecanise == 'oui') ,
          sum(ecz$ECZAGPresent == 'oui') )
  IS <- c(mean(ecz$ECZISNbre , na.rm = TRUE) ,
          mean(ecz$ECZISImmatricule , na.rm = TRUE) ,
          mean(ecz$ECZISMecanise , na.rm = TRUE) ,
          mean(ecz$ECZISPresent , na.rm = TRUE))
  Noms <- c('MCZ' , 'MDH' , 'DN' , 'AG' , 'IS (Nombre moyen)')
  out <- matrix(c(MCZ , MDH , DN , AG , IS) , byrow = TRUE , nrow = 5 ,
                dimnames = list(Noms ,
                                c('Nombre' , 'Immatriculés' , 'Mecanise' , 'Presents')))
  out <- as.data.frame(out)
  out$Staff <- Noms
  out
}

ecz_staffing <- ddply(facilities , .(Province) ,
                      staff_ecz)
output.table(ecz_staffing , 'ecz_staffing')

#### Centres de santé

melted_fac <- melt(data = facilities , id = c("instanceID", "Province" , "FacLevel" , "FacRurban") )

effectif_variables <- read.csv('data/variables_effectif.csv' , as.is = TRUE)

effectif_data <- subset(melted_fac , variable %in% effectif_variables$VarName)
effectif_data <- merge(effectif_data , effectif_variables , by.x ='variable' , by.y = 'VarName')
effectif_data$value[effectif_data$value == 'oui'] <- 1
effectif_data$value[effectif_data$value == 'non'] <- 0
effectif_data$value <- as.numeric(effectif_data$value)

meanNumHW <- ddply(effectif_data , .(Province , Statut , FacLevel , Role) , 
                   function(x)  mean(x$value , na.rm = TRUE))

meanNumHW <- subset(meanNumHW , !is.na(V1))

#Comparing to norm

##Loading the norm is

norms <- read.csv('data/input_norms.csv' , as.is = TRUE)

##Recoding categories in melted facilities data
recode_staff_input <-  read.csv('data/input_recode_staff.csv' , as.is = TRUE)

staff_recode <- function(melted_data , recode_input){
  out <- merge(melted_data  , recode_input , by.x = 'Role' , by.y = 'staff_init' , all.x = TRUE)
  out <- subset(out , !is.na(value))
  out
}

effectif_data <- staff_recode(effectif_data , recode_staff_input)

output.table(meanNumHW , 'hw_by_faclevel_role_prov')

data_plot <- merge(effectif_data , norms , 
                   by.x = c('FacLevel' , 'staff_recode') ,
                   by.y = c('facility_type' , 'categorie'))

plot_norm <- function(data , FacLevel , maxValue , xlab){
  qplot(data = data_plot[data_plot$Statut == "Total"  & 
                           data_plot$FacLevel %in% FacLevel &
                           data_plot$value <= maxValue, ] ,
        x = value , y = FacLevel , col = FacRurban , shape = FacLevel , geom = 'jitter' ) +
    facet_grid(staff_recode ~ Province ) + theme_bw()+
    geom_vline(data = data_plot[data_plot$Statut == "Total" & 
                                  data_plot$FacLevel %in% FacLevel,], 
               mapping = aes(xintercept = nombre) , color = 'green' , size = 1.2) +
    scale_x_continuous(breaks=seq(0,maxValue , 5)) + scale_colour_brewer(palette="Set1") +
    xlab(xlab) + ylab('')
}

pdf(file = "output/graphs/staffing_comparison_to_norm.pdf" , width = 14)
plot_norm(data_plot , c('cs' , 'csr') , 20 , 'Distribution du nombre de travailleurs dans les centres de santé')
plot_norm(data_plot , 'hgr' , 10 , 'Distribution du nombre de travailleurs dans les HGR')
dev.off()


## HeatMaps code

percentage_norm <- function(melted_facilities){
  perc <- mean(melted_facilities$value >= melted_facilities$nombre )
  if(max(melted_facilities$nombre) == 0){
    perc <- NA 
  }
  perc
}

create_split <- expand.grid(unique(data_plot$Province) ,
                            ordering_staff , unique(data_plot$FacLevel) , 
                            unique(data_plot$FacRurban) )
colnames(create_split) <- c("province" , "staff" , "level" , "rurbain")


data_heat_map <- ddply(data_plot , 
                       .(staff_recode , Province , FacLevel , FacRurban) ,
                       percentage_norm)


data_heat_map$staff <- factor(x = data_heat_map$staff , 
                              levels =  ordering_staff ,
                              ordered = TRUE)

pdf('output/graphs/staffing_heatmap.pdf' , width = 14)
qplot(x =FacRurban ,y = staff , data = data_heat_map, fill = V1, 
      geom = "raster" , label = round(V1 , 2) , main = '% of facilities with appropriate level of staffing')+
  scale_fill_gradient(limits=c(0,1) , low="red" , high = "green" ,
                      name = 'Legend') +
  facet_grid(FacLevel~Province) + theme_bw() + 
  geom_text() +
  xlab('') + ylab('Staffing category')
dev.off()



### Taux de mecanisation

taux_status <- function(staff_data){
  data_use <- dcast(staff_data , instanceID ~ Statut , value.var = 'value' , fun.aggregate = sum)
  data_use <- subset(data_use , data_use$Total >= data_use$Mecanise) #adhoc fixing here...
  if (nrow(data_use) > 0){
    Mecanise_prop <-  sum(data_use$Mecanise) / sum(data_use$Total)
    Mecanise_N <- sum(data_use$Mecanise)
    Present_prop <-  sum(data_use$Present) / sum(data_use$Total)
    Present_N <- sum(data_use$Present)
    Immatricule_prop <-  sum(data_use$Immatricule) / sum(data_use$Total)
    Immatricule_N <- sum(data_use$Immatricule)
    Total <- sum(data_use$Total)
  }
  if (nrow(data_use) == 0){
    Total <- Immatricule_prop <- Immatricule_N <- Mecanise_prop <- 
      Mecanise_N <- Present_prop <- Present_N<- NA
  }
  data.frame(Total , Immatricule_prop , Immatricule_N , Mecanise_prop , Mecanise_N , 
             Present_prop , Present_N)
}

status_dist <- ddply(effectif_data , .(staff_recode , FacLevel , Province , FacRurban) , taux_status)
output.table(status_dist , 'status_distribution')

status_dist <- subset(status_dist , !is.na(FacRurban))
status_dist$staff_recode <- factor(x = status_dist$staff_recode , 
                              levels =  ordering_staff ,
                              ordered = TRUE)

status_dist$FacLevel <- factor(x = status_dist$FacLevel , 
                                   levels =  ordering_facilities ,
                                   ordered = TRUE)

heat_map_status <- function(data , var , titre){
  data$varToPlot <- data[,var]
  qplot(x =FacRurban ,y = staff_recode , data = data, fill = varToPlot ,
        geom = "raster" , label = round(varToPlot , 2) , main = titre)+
    scale_fill_gradient(limits=c(0,1) , low="red" , high = "green" , name = 'Legend') +
    facet_grid(FacLevel~Province , scales = 'free_y') +
    theme_bw()+
    geom_text() + xlab('') + ylab('')
}

pdf('output/graphs/heat_maps_status.pdf' , width = 14)
heat_map_status(status_dist , 'Mecanise_prop' , 'Proportion de personnel mécanisé')
heat_map_status(status_dist , 'Immatricule_prop' , 'Proportion de personnel immatriculé')
heat_map_status(status_dist , 'Present_prop' , 'Proportion de personnel présent')
dev.off()


sum(status_dist$Immatricule_N) / sum(status_dist$Total)
sum(status_dist$Mecanise_N) / sum(status_dist$Total)

mean(indiv$HWImmatriculation == 'oui')
mean(indiv$HWMecanise == 'oui')


####Donnees inidividuelles

### Echantillon

table(indiv$HGRRole ,  indiv$Province)
table(indiv$CSRole ,  indiv$Province)
table(indiv$ECZRole ,  indiv$Province)

## Information démographique

# Distribution par genre

table(indiv$Sex)

# Genre et niveau d'éducation

table(indiv$LastEduc , indiv$Sex)

indiv$Age[indiv$Age > 100 | indiv$Age < 18] <- NA

retraite <- function(data){
  data <- subset(data , !is.na(Age) )
  non_med <- data.frame(Cadre = 'Non medecin' , 
                     Total = nrow(data[data$Role != 'medecin' , ]) , 
                     Retraite = nrow(data[data$Role != 'medecin' & data$Age > 62, ])
                     )
  med <- data.frame(Cadre = 'Medecin' , 
                    Total = nrow(data[data$Role == 'medecin' , ]) , 
                    Retraite = nrow(data[data$Role == 'medecin' & data$Age > 65, ])
  )
  rbind(non_med , med)
}

df_retraite <- ddply(indiv , .(FacilityType) , retraite)
output.table(tab = df_retraite , name =  'table_retraite')


## Description de la distribution des revenus

melted_indiv <- melt(indiv , id = c("Province" , "Sex" , "Age" , "Matrimonial" , "NumberFinancialDependants" , 
                                    "LastEduc" , "FacilityType" , "Structure" , "Role"))

### Salaires

FacRelevant <- subset(facilities ,  select = c("Structure"  , "FacLevel" , "FacOwnership" , 
                                               "FacAppui" , "FacRurban" , "EczAppui"))

melted_indiv_full <- merge(FacRelevant , melted_indiv , by = 'Structure')

percentage_revenu <- function(data , var){
  perc <- mean(data$value[data$variable == var] == 'oui')
  perc
}

CreateHeatMapData <- function(data , dimensions , revenu_type){
  heat_data <- ddply(data , as.quoted(dimensions) ,
                     function(x) percentage_revenu(x , revenu_type))
  heat_data$FacilityType <- factor(heat_data$FacilityType , levels = c('cs' , 'hgr' , 'ecz') , 
                                     ordered = TRUE)

  heat_data$Role <- factor(heat_data$Role , levels = ordering_staff , ordered = TRUE)
  heat_data
}


make_heat_revenue <- function(data , revenu_data , revenu_name){
  heat_data <- CreateHeatMapData(data , c('Province','FacilityType','Role') , revenu_data)
  heat_data <- subset(heat_data ,  Role != '')
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

revenue_entry <- read.csv('data/revenues_entry.csv' , as.is = TRUE)



heat_complete <- data.frame(Province = character() , FacilityType = character() , Role = character() , 
                            V1 = numeric() , Source = character())
pdf('output/graphs/revenue_heatmap.pdf', width = 14)
for(i in 1:nrow(revenue_entry)){
  heat <- make_heat_revenue(melted_indiv_full , revenue_entry[i,1], revenue_entry[i,2])
  print(heat[[2]])
  heat_complete <- rbind(heat_complete , heat[[1]])
  provs <- unique(heat_complete$Province)
}

revenu <- revenue_entry$RevenueLabel
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

##Bancarisation

bancar_data <- subset(melted_indiv_full , variable == 'WageMode' & value != '' & Role != '')

bancarisation_heatmap <- ddply(bancar_data , .(Role , Province , FacilityType , FacRurban) , 
                               function(x) mean(x$value == 'banque'))
output.table(tab = bancarisation_heatmap , name = 'bancarisation_data')


pdf('output/graphs/bancarisation_heatmap.pdf', width = 14)
p <- qplot(x =FacRurban ,y = Role , data = bancarisation_heatmap, 
           fill = V1, geom = "raster"  , main = 'Percentage of bancarized people')+
  scale_fill_gradient(limits=c(0,1) , low="red" , high = "green" , name = 'Legend') +
  facet_grid(FacilityType~Province , scales = 'free_y' ) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('') + ylab('')
print(p)
dev.off()