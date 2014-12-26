## Basic Descriptives

setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'analysis'

source('code/useful_functions.r')


NStructures <- nrow(facilities)

NInterviews <- length(unique(indiv$meta.instanceID))


DistFacilities <- table(facilities$FacLevel)
output.table(DistFacilities , 'facilities_by_FacilType')

DistInterViews <- ddply(indiv , .(FacilTypeEntry) , function(x) nrow(x))
output.table(DistInterViews , 'interviews_by_FacilType')


fac_by_prov <- table(facilities$structuremystructure_province , facilities$FacLevel)
output.table(fac_by_prov , 'facilities_by_Province_FacilType')

indiv_by_prov <- table(indiv$structuremystructure_province , indiv$FacilTypeEntry)
output.table(indiv_by_prov , 'interviews_by_Province_FacilType')




## Entretiens Individuels

TabInterviews <- function(Role , Type , data ){
  out <- as.data.frame(table(data[data$FacilTypeEntry == Type , Role],
                             data$GroupDemographics.IndivSex[data$FacilTypeEntry == Type])  )
  print(head(out))
  colnames(out) <- c('Role' , 'Sexe' , 'Nombre')
  out
}

TabInterviews_forStruct <- function(Role, Type, data){
  ddply(data , .(structuremystructure_province) , 
        function(x){
          TabInterviews(Role, Type , x)
        } )
}

TabInterviews_forStruct('GroupCS.IndivCSPost' , 'cs' , indiv)
TabInterviews_forStruct('GroupCS.IndivCSPost' , 'ecz' , indiv)
TabInterviews_forStruct('GroupCS.IndivCSPost' , 'hgr' , indiv)


## Description des centres de santé

Quelques cartes

__Les données SIS ont été transmises par le SNIS. Citer / remercier__

```{r}
zones_shapefile <- readShapePoly('../data/Shapefiles/Zones/znrdcII.shp')

zones_shapefile$NOM_ZS <- UnifyNames(as.character(zones_shapefile$NOM_ZS))
zones_shapefile$PROVINCE <- UnifyNames(as.character(zones_shapefile$PROVINCE ))

zones_shapefile$NOM_ZS[zones_shapefile$NOM_ZS == 'kisanji'] <- 'kisandji'


sampled_zones <- zones_shapefile$NOM_ZS %in%  sample_final$Zone

plot(zones_shapefile )
plot(zones_shapefile[sampled_zones , ] , col = 'darkred'  , add = TRUE)
title("Zones échantillonées pour l'enquête")
```


La projection de la carte fournie par le SNIS est bizarre, mais je trouve pas commment convertir les coordonnées dans un shapefile. Solution quick and dirty : convertir les coordonnées des sites plutôt...

__ATTENTION : devra être réglé si analyses spatiales, sinon on va avoir des distances bizarres__


```{r}
facilities$FacLocalisation.FacLocation.Latitude2 <- facilities$FacLocalisation.FacLocation.Latitude * 100000
facilities$FacLocalisation.FacLocation.Longitude2 <- facilities$FacLocalisation.FacLocation.Longitude * 100000

for (i in 1:length(unique(sample_final$Province))){ 
Province <- unique(sample_final$Province)[i]
sub_fac <- subset(facilities , 
structuremystructure_province == Province)  

shapefile <- zones_shapefile[zones_shapefile$PROVINCE  == Province, ]

sampled_zones <- shapefile$NOM_ZS %in%  as.character(sample_final$Zone)



sub_fac <- subset(sub_fac , !is.na(FacLocalisation.FacLocation.Latitude))
coordinates(sub_fac) <- ~FacLocalisation.FacLocation.Longitude2+FacLocalisation.FacLocation.Latitude2


plot(shapefile )
plot(shapefile[sampled_zones , ] , col = 'darkred'  , add = TRUE)
plot(sub_fac , add = TRUE , col = as.factor(sub_fac$FacLevel))
legend("right" , col = c(1:5) , legend =  sort(unique(sub_fac$FacLevel)) , pch = 3)
}

```

Hmm... les données géograpiques du SNIS sont vraiment un peu funky...

Milieu des centres de santé

```{r}
table(facilities$FacLevel , facilities$FacRurban ,
facilities$structuremystructure_province)
```

Quel type de structures ?

```{r}
table(facilities$FacLevel , facilities$FacOwnership ,
facilities$structuremystructure_province)
```

### Support reçu par les strctures de santé

```{r}
centre_sante <- subset(facilities , FacLevel %in% c('cs' , 'csr' , 'hgr'))
ecz <- subset(facilities , FacLevel %in% c('ecz'))
```

Combien d'équipes cadre de district ont reçu un appui ?

```{r}
table(ecz$EczAppui , ecz$structuremystructure_province)
```

Combien de centres de sante / hgr ont reçu un appui ?

```{r}
table(centre_sante$FacAppui, centre_sante$structuremystructure_province)
```

Qui recoit un appui ?

__faire plutot du Poisson__

```{r}
FacAppuiYN <- centre_sante$FacAppui == 'oui'

summary(glm(FacAppuiYN ~ structuremystructure_province + FacLevel + FacOwnership +
FacRurban , data = centre_sante , family = binomial))
```

Nombres d'appuis différents reçus dans les sites qui recoivent cet appui

__rajouter la catégorie 0 pour plus de clarté__

```{r}
facComp <- merge(centre_sante , loop_appui_fac , 
by.x = 'meta.instanceID' , by.y = 'PARENT_KEY' , 
all.x = TRUE)

facAppui <- subset(facComp , FacAppui == 'oui')

##Fatigue, passage en loop => revoir ca en ddply ?

NAppui <- data.frame(Province = character() , Nappui = numeric() , Nsite = numeric())
for (i in 1:length(unique(facAppui$structuremystructure_province))){
prov <- unique(facAppui$structuremystructure_province)[i]
x <- subset(facAppui , structuremystructure_province == prov)
N <- as.data.frame(table(table(x$meta.instanceID)))
out <- data.frame(Province = prov , Nappui = N[,1] , Nsite = N[,2])
NAppui <- rbind(NAppui , out)
}
```

Description des types d'appui

Duree 
```{r}
table(facAppui$FacSupportDuree , facAppui$structuremystructure_province)
```

Motivation financiere

```{r}
table(facAppui$FacSupportMotivation , facAppui$structuremystructure_province)
```

### Staffing 

#### ECZ

```{r}
staff_ecz <- function(facilities){
ecz <- subset(facilities , FacLevel == 'ecz')
N <- nrow(ecz)
MCZ <- c(sum(ecz$ECZMCZ.MCZNbre == 'oui') ,
sum(ecz$ECZMCZ.MCZImmatricule == 'oui') ,
sum(ecz$ECZMCZ.MCZMecanise == 'oui') ,
sum(ecz$ECZMCZ.MCZPresent == 'oui') )
MDH <- c(sum(ecz$ECZMDH.ECZMDHNbre == 'oui') ,
sum(ecz$ECZMDH.ECZMDHImmatricule == 'oui') ,
sum(ecz$ECZMDH.ECZMDHMecanise == 'oui') ,
sum(ecz$ECZMDH.ECZMDHPresent == 'oui') )
DN <- c(sum(ecz$ECZDN.ECZDNNbre == 'oui') ,
sum(ecz$ECZDN.ECZDNImmatricule == 'oui') ,
sum(ecz$ECZDN.ECZDNMecanise == 'oui') ,
sum(ecz$ECZDN.ECZDNPresent == 'oui') )
AG <- c(sum(ecz$ECZAG.ECZAGNbre == 'oui') ,
sum(ecz$ECZAG.ECZAGImmatricule == 'oui') ,
sum(ecz$ECZAG.ECZAGMecanise == 'oui') ,
sum(ecz$ECZAG.ECZAGPresent == 'oui') )
IS <- c(mean(ecz$ECZIS.ECZISNbre , na.rm = TRUE) , 
mean(ecz$ECZIS.ECZISImmatricule , na.rm = TRUE) , 
mean(ecz$ECZIS.ECZISMecanise , na.rm = TRUE) , 
mean(ecz$ECZIS.ECZISPresent , na.rm = TRUE))
Noms <- c('MCZ' , 'MDH' , 'DN' , 'AG' , 'IS (Nombre moyen)') 
out <- matrix(c(MCZ , MDH , DN , AG , IS) , byrow = TRUE , nrow = 5 ,
dimnames = list(Noms ,
c('Nombre' , 'Immatriculés' , 'Mecanise' , 'Presents')))
out <- as.data.frame(out)
out$Staff <- Noms
out
}

ecz_staffing <- ddply(facilities , .(structuremystructure_province) ,
staff_ecz)
print(ecz_staffing)

write.csv(ecz_staffing , '../output//tables/ecz_staffing.csv')
```

#### Centres de santé

Vérifier les données 

```{r}
check_staff <- function(facilities , type){
test <- matrix(ncol = 14*4 , nrow = nrow(facilities))
for(i in 1:14){
num <- 47 + i
tab <- facilities[, c(num ,(62 + (num - 48)*4):(62 +(num - 48)*4+3)) ]
if(sum(tab[,1] == 0 | is.na(tab[,1])) > 0){
tab[tab[,1] == 0 | is.na(tab[,1]) , c(2:4)] <- tab[tab[,1] == 0 | is.na(tab[,1]) , 1]
}
for(j in 2:5){
test[ , (i-1)*4 + j -1] <- tab[,1] < tab[,j]
}
}
test
}

aa <- check_staff(facilities , 'hgr')

facilities2 <- apply(aa , 1 , 
function(x){
sum(x , na.rm = TRUE)
}
)

fffac <- facilities[facilities2 > 0,]
```

```{r}
staff_fac <- function(facilities , type){
fac <- subset(facilities , FacLevel == type)
Noms <- c('Medecins Specialistes' , 'Medecins Generalistes' ,
'Infirmiere A0' , 'Infirmiere A1', 'Infirmiere A2' , 'Infirmiere A3' ,
'Pharmaciens' , 'Preparateurs Pharmacie' , 'Techniciens Labo' ,
'Preposes Labos' , 'AG' , 'Huissiers' , 'Autres Techniciens'  ,
'Autres')
Effectifs <- c()
for(i in 1:14){
num <- 47 + i
tab <- fac[, c(num ,(62 + (num - 48)*4):(62 +(num - 48)*4+3)) ]
if(sum(tab[,1] == 0 | is.na(tab[,1])) > 0){
tab[tab[,1] == 0 | is.na(tab[,1]) , c(2:4)] <- tab[tab[,1] == 0 | is.na(tab[,1]) , 1]
}
for(j in 1:5){
tab[is.na(tab[,j]) , j] <- 0
var <- c(mean(tab[,j]))
Effectifs <- c(Effectifs , var)
}
}
out <- matrix(Effectifs , byrow = TRUE , ncol = 5,
dimnames = list(Noms ,
c('Nombre' , 'Immatriculés' , 
'Mecanise' , 'Reguliers' , 'Presents')))
out <- as.data.frame(out)
out$Staff <- Noms
out  
}

hgr_staffing <- ddply(facilities , .(structuremystructure_province) ,
function(x) staff_fac(x , 'hgr'))
write.csv(hgr_staffing , '../output//tables/hgr_staffing.csv')

cs_staffing <- ddply(facilities , .(structuremystructure_province) ,
function(x) staff_fac(x , 'cs'))
write.csv(cs_staffing , '../output//tables/cs_staffing.csv')

#csr_staffing <- ddply(facilities , .(structuremystructure_province) ,
#                      function(x) staff_fac(x , 'csr'))
#write.csv(csr_staffing , '../output//tables/csr_staffing.csv')
```


Comparing to norm

```{r}
##Declaring what the norm is

norms <- read.csv('../data/input_norms.csv' , as.is = TRUE)

facilities$FacLevel_group <- facilities$FacLevel
facilities$FacLevel_group[facilities$FacLevel_group == 'csr'] <- 'cs'

##Recoding categories in facilities data

staff_collapse <- function(facilities){
fac <- subset(facilities)
Noms <- c('Medecins Specialistes' , 'Medecins Generalistes' ,
'Infirmiere A0' , 'Infirmiere A1', 'Infirmiere A2' , 'Infirmiere A3' ,
'Pharmaciens' , 'Preparateurs Pharmacie' , 'Techniciens Labo' ,
'Preposes Labos' , 'AG' , 'Huissiers' , 'Autres Techniciens'  ,
'Autres')
Effectifs <- c()
for(i in 1:14){
num <- 47 + i
tab <- fac[, c(num ,(62 + (num - 48)*4):(62 +(num - 48)*4+3)) ]
if(sum(tab[,1] == 0 | is.na(tab[,1])) > 0){
tab[tab[,1] == 0 | is.na(tab[,1]) , c(2:4)] <- tab[tab[,1] == 0 | is.na(tab[,1]) , 1]
}
for(j in 1:5){
tab[is.na(tab[,j]) , j] <- 0
var <- c(mean(tab[,j]))
Effectifs <- c(Effectifs , var)
}
}
out <- matrix(Effectifs , byrow = TRUE , ncol = 5,
dimnames = list(Noms ,
c('Nombre' , 'Immatriculés' , 
'Mecanise' , 'Reguliers' , 'Presents')))
out <- as.data.frame(out)
out$Staff <- Noms
out  
}

recode_staff_input <-  read.csv('../data/input_recode_staff.csv' , as.is = TRUE)

staff_recode <- function(collapsed_data , recode_input){
merge(collapsed_data  , recode_input , by.x = 'Staff' , by.y = 'staff_init' , all.x = TRUE)
}


staff_collapse_recode <- function(data , recode){
collapsed <- ddply(data , .(structuremystructure_province , structuremystructure , FacLevel , FacRurban) ,
function(x) staff_collapse(x ))
out <- ddply(collapsed , .(structuremystructure) , 
function(x) staff_recode(x , recode))
out <- subset(out , select = c("structuremystructure_province" ,
"structuremystructure" , "FacLevel", "Nombre" ,
"Immatriculés" , "Mecanise" ,"Reguliers" , 
"Presents" , "staff_recode" , "FacRurban"))
out
}


flat_staffing <- ddply(facilities , 
.(structuremystructure_province , structuremystructure , FacLevel , FacRurban) ,
function(x) staff_collapse_recode(x  ,  recode_staff_input))


melted_staffing <- melt(data = flat_staffing , 
id = c("structuremystructure_province" ,
"structuremystructure" , "FacLevel",
"staff_recode" , "FacRurban") , 
value = c("Nombre" , "Immatriculés"  
, "Mecanise" ,"Reguliers" , "Presents")
)
```

Now Looking at some plots

```{r}
## Taking out two outliers

## Semiurbain == Urbain



data_plot <- merge(melted_staffing , norms , 
by.x = c('FacLevel' , 'staff_recode') ,
by.y = c('facility_type' , 'categorie'))


qplot(data = data_plot[data_plot$variable == "Nombre"  & 
data_plot$FacLevel == "cs"&
data_plot$value <= 20, ] , 
x = value , y = FacLevel , 
col = FacRurban , shape = FacLevel  ,
geom = 'jitter' ) +
facet_grid(staff_recode ~ structuremystructure_province ) +
theme_bw()+
geom_vline(data = data_plot[data_plot$variable == "Nombre" & 
data_plot$FacLevel == "cs",], 
mapping = aes(xintercept = nombre) , color = 'green' , size = 1.2)+ 
scale_x_continuous(breaks=seq(0,20 , 5)) + scale_colour_brewer(palette="Set1") +
xlab('Number of health workers') + ylab('')


qplot(data = data_plot[data_plot$variable == "Nombre" & 
data_plot$FacLevel == "hgr" &
data_plot$value <= 60, ] , 
x = value , y = FacLevel , 
col = FacRurban ,
geom = 'jitter' ) +
facet_grid(staff_recode ~ structuremystructure_province , scales = "free") +
theme_bw()+
geom_vline(data = data_plot[data_plot$variable == "Nombre" & 
data_plot$FacLevel == "hgr",], 
mapping = aes(xintercept = nombre) , color = 'green' , size = 1.2)+ 
scale_x_continuous(breaks=seq(0,60 , 10)) + scale_colour_brewer(palette="Set1")+
xlab('Number of health workers') + ylab('')


```

Heat Map

```{r}
percentage_norm <- function(melted_facilities){
perc <- mean(melted_facilities$value >= melted_facilities$nombre )
if(max(melted_facilities$nombre) == 0){
perc <- NA 
}
perc
}

ordered_staff <- c("autre" ,
"administrateur_gestionnaire", "infirmier_superviseur" ,  "medecin_chef_zone" ,
"administrateur" , "labo" , "pharmacien" , "infirmier" , "medecin")

create_split <- expand.grid(unique(data_plot$structuremystructure_province) , 
ordered_staff , unique(data_plot$FacLevel) , 
unique(data_plot$FacRurban) )
colnames(create_split) <- c("province" , "staff" , "level" , "rurbain")


data_heat_map <- ddply(data_plot , 
.(staff_recode , structuremystructure_province ,
FacLevel , FacRurban) ,
percentage_norm)

data_heat_map <- subset(data_heat_map , !is.na(V1))

data_heat_map$staff <- factor(x = data_heat_map$staff , 
levels =  ordered_staff ,
ordered = TRUE)

qplot(x =FacRurban ,y = staff_recode , data = data_heat_map, fill = V1, 
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacLevel~structuremystructure_province) +
theme_bw()+ 
geom_text()

```


### Taux de mecanisation

```{r}
taux_meca <- function(data_melt){
data_melt <- subset(data_melt , Nombre >= Mecanise)
if (nrow(data_melt) > 0){
a<-  sum(data_melt$Mecanise) / sum(data_melt$Nombre)
b <- sum(data_melt$Mecanise)
c <- sum(data_melt$Nombre)
}
if (nrow(data_melt) == 0){
a<- NA
}  
data.frame(taux  = a , Meca = b , total = c)
}

taux_immatricule <- function(data_melt){
data_melt <- subset(data_melt , Nombre >= Immatriculés)
if (nrow(data_melt) > 0){
a<-  sum(data_melt$Immatriculés) / sum(data_melt$Nombre)
b <- sum(data_melt$Immatriculés)
c <- sum(data_melt$Nombre)
}
if (nrow(data_melt) == 0){
a<- NA
}  
data.frame(taux  = a , Immat = b , total = c)
}

taux_immatricule(flat_staffing)


flat_staffing <- subset(flat_staffing , FacRurban != '')

data_heat_map <- ddply(flat_staffing , 
.(staff_recode , structuremystructure_province ,
FacLevel , FacRurban) ,
taux_meca)

taux_meca(flat_staffing)

ddply(flat_staffing , .(structuremystructure_province) ,
taux_meca)

data_heat_map <- subset(data_heat_map , !is.na(V1))

data_heat_map$staff <- factor(x = data_heat_map$staff , 
levels =  ordered_staff ,
ordered = TRUE)

qplot(x =FacRurban ,y = staff_recode , data = data_heat_map, fill = V1, 
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacLevel~structuremystructure_province , scales = 'free_y') +
theme_bw()+ 
geom_text()


mean(indiv$GroupStatut.IndivImmatricule == 'oui')
mean(indiv$GroupStatut.IndivMecanise == 'oui')
```

### Echantillon

Dans les HGR

```{r}
hgr <- subset(indiv , FacilTypeEntry == 'hgr')
table(hgr$GroupHGR.IndivHGRPost ,  hgr$structuremystructure_province)

```

Dans les CS

```{r}
cs <- subset(indiv , FacilTypeEntry == 'cs' )
table(cs$GroupCS.IndivCSPost  ,  cs$structuremystructure_province)
```



## Information démographique

Distribution par genre

```{r}
table(indiv$GroupDemographics.IndivSex)
```

Genre et niveau d'éducation

```{r}
table(indiv$GroupDemographics.IndivLastEduc ,
indiv$GroupDemographics.IndivSex)
```

Genre et rôle dans les centres de santé

```{r}
table(cs$GroupCS.IndivCSPost ,
cs$GroupDemographics.IndivSex)
```

```{r}
indiv$GroupDemographics.IndivAge[indiv$GroupDemographics.IndivAge > 100 |
indiv$GroupDemographics.IndivAge < 18] <- NA


qplot(data = indiv , x = GroupDemographics.IndivAge , binwidth = 1) +
facet_grid(~structuremystructure_province)

over65 <- function(dd){
table(dd$GroupDemographics.IndivAge > 65)
}

```



## Description de la distribution des revenus

### Salaires

```{r}
indiv$ResumePost <- indiv$GroupCS.IndivCSPost
indiv$ResumePost[indiv$FacilTypeEntry == 'hgr'] <- 
indiv$GroupHGR.IndivHGRPost[indiv$FacilTypeEntry == 'hgr']
indiv$ResumePost[indiv$FacilTypeEntry == 'ecz'] <- 
indiv$GroupECZQuests.IndivECZRole[indiv$FacilTypeEntry == 'ecz']

Table_Revenu <- function(indiv , var){
out <- data.frame(NSalaire = sum(indiv[ , var] == 'oui') ,
NIndiv = nrow(indiv))
out$Prop  <- out$NSalaire / out$NIndiv
out
}

MultipTables <- function(indiv , var){
revenu_table <- ddply(indiv , 
.(structuremystructure_province , FacilTypeEntry , ResumePost) , 
function(x ) Table_Revenu( x , var))
factors_lev <- unique(indiv$FacilTypeEntry)
out_tab <- list()
for(j in 1:length(factors_lev)){
level <- factors_lev[j]
dd <- revenu_table[revenu_table[,'FacilTypeEntry'] == level , ]
casted <- recast(dd , ResumePost ~ structuremystructure_province , 
fun = function(x) mean(x , na.rm = TRUE) ,
measure.var = 4)
suf <- c('NIndiv' , 'Prop')
for(i in 5:6){
xx <- recast(dd , ResumePost ~ structuremystructure_province , mean ,
measure.var = i)
casted <- merge(casted , xx , by = 'ResumePost' ,suffixes = c( '' , suf[i-4])) 
}
out_tab[[level]] <- casted
}
out_tab
}

#MultipTables(indiv , "WageYN") Remplace par heatmap
```

```{r}
FacRelevant <- subset(facilities , 
select = c("structuremystructure"  , "FacLevel" ,
"FacOwnership" , "FacAppui" , "HGRVolume.HGRNbreLits" ,
"FacRurban" , "EczAppui"))

indiv_full <- merge(FacRelevant , indiv , by = 'structuremystructure')
```

```{r SalaireHeatMap}
perc_revenu <- function(data , var){
perc <- mean(data[ , var] == 'oui')
perc
}

CreateHeatMapData <- function(data , dimensions , revenu_type){
heat_data <- ddply(data , as.quoted(dimensions) ,
function(x) perc_revenu(x , revenu_type))
heat_data$FacilTypeEntry <- factor(heat_data$FacilTypeEntry , levels = c('cs' , 'hgr' , 'ecz') , 
ordered = TRUE)
order_cadre <- c("autre" ,
"administrateur_gestionnaire", "infirmier_superviseur" ,  "medecin_chef_zone" ,
"administrateur" , "labo" , "pharmacien" , "infirmier" , "medecin")
heat_data$ResumePost <- factor(heat_data$ResumePost , levels = order_cadre , 
ordered = TRUE)

heat_data
}

heat_wage <- CreateHeatMapData(indiv_full , c('structuremystructure_province'  , 
'FacilTypeEntry' , 'ResumePost') , 'WageYN')

heat_wage <- subset(heat_wage ,  ResumePost != '')

qplot(y = ResumePost , data = heat_wage, fill = V1, x = rep("" , nrow(heat_wage)) ,
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacilTypeEntry~structuremystructure_province , scales = 'free_y') +
theme_bw()+ 
geom_text()

heat_wage$Source <- 'Salaire'

```



```{r}
indiv$ID <- seq(1 , nrow(indiv))
data_wage <- c("WageYN" , "GroupWage.WageLastDate" , "GroupWage.WageLastAmount" ,
"GroupWage.WageLastUnit" , "GroupWage.WageLast6Month" , 
"GroupWage.WageMode" , "GroupWage.WagePayer"  ,
"WageDollar" , "WageFC")

data_structure <- c("structuremystructure_province" , "structuremystructure" , 
"FacilTypeEntry")

data_indiv <- c("ID" , "GroupDemographics.IndivSex" , "GroupDemographics.IndivAge" ,
"GroupDemographics.IndivMatrim" , "GroupDemographics.IndivNFinancDep"  ,
"GroupDemographics.IndivOrigProvince" , "GroupDemographics.IndivLastEduc")

data_prime <- c("PrimeRisqueYN" , "GroupPrimeRisque.PrimeRisqueLastDate" ,
"GroupPrimeRisque.PrimeRisqueLastAmount" , "GroupPrimeRisque.PrimeRisqueLastUnit" ,
"GroupPrimeRisque.PrimeRisqueLast6Month" , 
"PrimeDollar" ,  "PrimeFC" )


melt_wage <- melt(data = indiv ,
id.vars = c( data_indiv ,  data_wage , data_structure , data_prime)  ,
measure.var = c("GroupCS.IndivCSPost" , "GroupHGR.IndivHGRPost" ,
"GroupECZQuests.IndivECZRole") , 
na.rm = TRUE
)

melt_keep <- subset(melt_wage , (variable == "GroupCS.IndivCSPost" & FacilTypeEntry == 'cs') |
(variable == "GroupHGR.IndivHGRPost" & FacilTypeEntry == 'hgr') |
(variable == "GroupECZQuests.IndivECZRole" & FacilTypeEntry == 'ecz')
)


```

```{r}

melt_keep <- merge(melt_keep , FacRelevant ,
by = 'structuremystructure')

````

```{r}
melt_wage_facil <- subset(melt_keep , (PrimeDollar < 4000 | is.na(PrimeDollar) & WageDollar < 1500)
)

qplot(data = melt_wage_facil , y = value , x = WageDollar , geom = 'jitter' , col = FacOwnership) + 
facet_grid(structuremystructure_province ~ FacilTypeEntry) +
theme_bw()

```

__Gosh data for Katanga is bad__

### Prime de Risque

```{r PrimeDeRisqueHeatMap}
#MultipTables(indiv , 'PrimeRisqueYN')

heat_prime <- CreateHeatMapData(indiv_full , c('structuremystructure_province'  , 
'FacilTypeEntry' ,'ResumePost') , 'PrimeRisqueYN')

heat_prime <- subset(heat_prime , ResumePost != '' )

qplot(x = rep("" , nrow(heat_prime)) ,y = ResumePost , data = heat_prime, fill = V1, 
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacilTypeEntry~structuremystructure_province , scales = 'free_y') +
theme_bw()+ 
geom_text()

heat_prime$Source <- 'Prime de Risque'
```

```{r}
qplot(data = melt_wage_facil , y = value , x = PrimeDollar , geom = 'jitter', col = FacOwnership) + 
facet_grid(structuremystructure_province ~ FacilTypeEntry) +
theme_bw()


```

### Prime Partenaire

```{r PrimePartenaireHeatMap}
#MultipTables(indiv , 'PrimesPartenairesYN')

heat_prime_part <- CreateHeatMapData(indiv_full , c('structuremystructure_province'  , 
'FacilTypeEntry' , 'ResumePost') , 'PrimesPartenairesYN')

heat_prime_part <- subset(heat_prime_part , ResumePost != '')

qplot(x = rep("" , nrow(heat_prime_part)) ,y = ResumePost , 
data = heat_prime_part, fill = V1, 
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacilTypeEntry~structuremystructure_province , scales = 'free_y') +
theme_bw()+ 
geom_text()

heat_prime_part$Source <- 'Prime Partenaire'
```

Qui paye les primes de partenaire ?
```{r SourcePrimes}
#table(loop_prime_partenaire$PrimesPartenairesSource)

#out <- unique(loop_prime_partenaire$PrimesPartenairesSource)

#write.csv(out, '../output//tables/partenaires_sources.csv')

```

```{r FlattenLoop}
loop_prime_partenaire <- subset(loop_prime_partenaire , 
CompSalaireDollar < 100000)

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

```

```{r}
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
```



### Partage de recettes

```{r PartageRecetteHeatMap}
heat_honoraire <- CreateHeatMapData(indiv_full , c('structuremystructure_province'  , 
'FacilTypeEntry' , 'ResumePost') , 'HonoraireYN')

heat_honoraire <- subset(heat_honoraire , ResumePost != '')

qplot(x = rep("" , nrow(heat_honoraire)) ,y = ResumePost , data = heat_honoraire, fill = V1, 
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacilTypeEntry~structuremystructure_province , scales = 'free_y') +
theme_bw()+ 
geom_text()

#MultipTables(indiv ,  'HonoraireYN')

heat_honoraire$Source <- 'Partage des Recettes'
```



```{r HonoraireDistrib}
ii <- subset(indiv , HonoraireDollar < 750)

qplot(ii$HonoraireDollar)
```

```{r HonoraireNormalize}
indiv$HonoraireNorm <- NormalizeIncome(indiv$HonoraireDollar , indiv$GroupHonoraire.HonorairePeriod)
```




### Heures supplémentaires

```{r HeurSupHeatMap}
heat_heuresup <- CreateHeatMapData(indiv_full , c('structuremystructure_province'  , 
'FacilTypeEntry' , 'ResumePost') , 'HeureSupYN')

heat_heuresup <- subset(heat_heuresup , ResumePost != '')

qplot(x = rep("" , nrow(heat_heuresup)) ,y = ResumePost , data = heat_heuresup, fill = V1, 
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacilTypeEntry~structuremystructure_province , scales = 'free_y') +
theme_bw()+ 
geom_text()
#MultipTables(indiv , 'HeureSupYN')
heat_heuresup$Source <- 'Heures Supplementaires'
```

```{r HeurSupDistrib}
qplot(data = indiv , x =HeureSupDollar) + 
facet_wrap(~structuremystructure_province)

```


### Per Diems

```{r PerDiemHeatMap}
heat_PerDiems <- CreateHeatMapData(indiv_full , c('structuremystructure_province'  , 
'FacilTypeEntry' , 'ResumePost') , 'PerDiemYN')

heat_PerDiems <- subset(heat_PerDiems , ResumePost != '')

qplot(x = rep("" , nrow(heat_PerDiems)) ,y = ResumePost , data = heat_PerDiems, fill = V1, 
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacilTypeEntry~structuremystructure_province , scales = 'free_y') +
theme_bw()+ 
geom_text()

heat_PerDiems$Source <- 'Per Diem'
#MultipTables(indiv ,  'PerDiemYN')
```

Combien de personnes ont eu des *per diems* ?

```{r}
NPerDiemed <- length(unique(loop_perdiem$PARENT_KEY))
```

`r NPerDiemed` personnes (`r round(NPerDiemed / NInterviews , 3) * 100` %) ont reçu un per diem

Visiblement, les per diems sont surtout obtenus pendant les campagnes vacciales (JNV)... Sera-ce un problème ?

```{r}
table(loop_perdiem$PerDiemRaison)
```

Combien de per diems différents ont-ils reçu ?

```{r}
count_perdiem <- as.data.frame(table(loop_perdiem$PARENT_KEY))
table(count_perdiem$Freq)
```

Distribution of Perdiems

```{r PerDiemDistrib}
loop_perdiem <- subset(loop_perdiem , PerDiemDollar < 500)

qplot(data = loop_perdiem , x = PerDiemDollar) +
facet_wrap(~PerDiemRaison , scales = 'free')

```

Flatten PerDiems

```{r}
loop_perdiem <- subset(loop_perdiem , 
!(PerDiemDollar > 100 & PerDiemRaison == 'jnv') &
!(PerDiemDollar > 400 & PerDiemRaison == 'atelier'))

flat_perdiem <- ddply(loop_perdiem , .(PARENT_KEY) , 
function(x) sum(x$PerDiemDollar) / 3)

qplot(flat_perdiem$V1)

```



### Vente Médicament

```{r}
table(indiv$IndivVendeMedicUB1)

table(indiv$IndivVendeMedicMontantExact == 0)
```

### Cadeaux

```{r}
table(indiv$IndivCadeauUB1)

table(indiv$IndivCadeauMontantExact == 0)
```




### Activité privé

```{r ActPriveeHeatMap}
heat_Prive <- CreateHeatMapData(indiv_full , c('structuremystructure_province'  , 
'FacilTypeEntry' , 'ResumePost') , 'ActPriveeYN')

heat_Prive <- subset(heat_Prive , ResumePost != '')

qplot(x = rep("" , nrow(heat_Prive)) ,y = ResumePost , data = heat_Prive, fill = V1, 
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacilTypeEntry~structuremystructure_province , scales = 'free_y') +
theme_bw()+ 
geom_text()

#MultipTables(indiv ,  'ActPriveeYN')

heat_Prive$Source <- 'Activite Privee'
```



```{r ActivPriveeDistrib}
loop_activ_privee <- subset(loop_activ_privee , 
ActivPriveeDollar < 10000)

qplot(data = loop_activ_privee , x = ActivPriveeDollar) +
facet_wrap(~ActPriveeLieu , scales = 'free')

table(loop_activ_privee$ActPriveeLieu)

```


```{r FlatActivPrivee}
loop_activ_privee <- subset(loop_activ_privee , 
ActivPriveeDollar < 800)

flat_act_privee <- ddply(loop_activ_privee , .(PARENT_KEY) , 
function(x) sum(x$ActivPriveeDollar))

qplot(flat_act_privee$V1)
```


### Activité non santé

```{r ActNonSanteHeatMap}
mean(indiv_full$ActNonSanteYN == 'oui')


heat_NonSante <- CreateHeatMapData(indiv_full , c('structuremystructure_province'  , 
'FacilTypeEntry' , 'ResumePost') , 'ActNonSanteYN')

heat_NonSante <- subset(heat_NonSante , ResumePost != '')

qplot(x =rep("" , nrow(heat_NonSante)) ,y = ResumePost , data = heat_NonSante, fill = V1, 
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacilTypeEntry~structuremystructure_province  , scales = 'free_y') +
theme_bw()+ 
geom_text()

#MultipTables(indiv ,  'ActNonSanteYN')
heat_NonSante$Source <- 'Activite Non Sante'
```

Quelles sont ces activites ?

```{r ActNonSanteDistrib}
loop_activ_non_sante <- subset(loop_activ_non_sante , ActivNonSanteDollar < 5000)

qplot(data = loop_activ_non_sante , x = ActivNonSanteDollar) +
facet_wrap(~ActNonSanteType , scales = 'free')

#table(loop_activ_non_sante$ActNonSanteType)

````

```{r flatActivNonSante}
flat_ans <- ddply(loop_activ_non_sante , .(PARENT_KEY) ,
function(x) sum(x$ActivNonSanteDollar))

qplot(flat_ans$V1)

```


### Autre revenus

```{r AutreRevenusHeatMap}
heat_AutreRevenu <- CreateHeatMapData(indiv_full , c('structuremystructure_province'  , 
'FacilTypeEntry' , 'ResumePost') , 'AutreRevenuYN')

heat_AutreRevenu <- subset(heat_AutreRevenu , ResumePost != '')

qplot(x = rep("" , nrow(heat_AutreRevenu)), y = ResumePost , 
data = heat_AutreRevenu, fill = V1, 
geom = "raster" , label = round(V1 , 2))+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacilTypeEntry~structuremystructure_province , scales = 'free_y') +
theme_bw()+  
geom_text() 

#MultipTables(indiv ,  'AutreRevenuYN')

heat_AutreRevenu$Source <- 'Autre Revenus'
```

Quels sont ces revenus ?

```{r AutreDistrib}

qplot(loop_autre_revenu$AutreRevenuDollar)

#table(loop_autre_revenu$AutreRevenuSource)

#out <- unique(loop_autre_revenu$AutreRevenuSource)
#write.csv(out , '../output/tables/source_autre_revenu.csv')

```

__Recoder et reaffecter dans revenus prives plus haut si besoin__

### Résumés

```{r HeatByProvince , fig.width=12}

heat_by_province <- rbind(heat_AutreRevenu , heat_NonSante , heat_PerDiems ,
heat_Prive , heat_heuresup , heat_honoraire , 
heat_prime , heat_prime_part , heat_wage )

provs <- unique(heat_by_province$structuremystructure_province)

revenu <- c("Salaire"  , "Prime de Risque" , "Partage des Recettes" ,
"Heures Supplementaires"  , "Prime Partenaire" , "Per Diem" , 
"Activite Privee" , "Activite Non Sante" , "Autre Revenus")

heat_by_province$Source <- factor(heat_by_province$Source , 
levels = revenu , 
ordered = TRUE
)


qplot(x =structuremystructure_province ,y = ResumePost , data = heat_by_province, 
fill = V1, geom = "raster")+
scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
facet_grid(FacilTypeEntry~Source , scales = 'free_y' ) +
theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
xlab('') + ylab('')


#for(i in 1:length(provs)){
#  prov <- provs[i]
#  dat <- subset(heat_by_province , 
#                structuremystructure_province == prov )
#  p <- qplot(x =rep("" , nrow(dat)) ,y = ResumePost , data = dat, fill = V1, 
#        geom = "raster" , label = round(V1 , 2))+
#    scale_fill_gradient(limits=c(0,1) , low="red" , high = "green") +
#    facet_grid(FacilTypeEntry~Source) +
#    theme_bw() + labs(title = prov) +
#    xlab('') + ylab('')
#  print(p)
#  }

```



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
covs_hfsgen <- "FacRurban + FacLevel + Power  + (1|structuremystructure_province)"

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
rand[,2]*(data$structuremystructure_province == 'bandundu') +
rand[,3]*(data$structuremystructure_province == 'equateur') +
rand[,4]*(data$structuremystructure_province == 'katanga') +
rand[,5]*(data$structuremystructure_province == 'sud_kivu')
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
list(bandundu = data.frame(counterfac , structuremystructure_province = 'bandundu') ,
equateur = data.frame(counterfac , structuremystructure_province = 'equateur') ,
katanga = data.frame(counterfac , structuremystructure_province = 'katanga') ,
sud_kivu = data.frame(counterfac , structuremystructure_province = 'sud_kivu')
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

```