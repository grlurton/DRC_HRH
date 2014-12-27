## Cartography

setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'analysis'

source('code/useful_functions.r')

zones_shapefile <- readShapePoly('data/shapefiles/Zones/znrdcII.shp')

zones_shapefile$NOM_ZS <- UnifyNames(as.character(zones_shapefile$NOM_ZS))
zones_shapefile$PROVINCE <- UnifyNames(as.character(zones_shapefile$PROVINCE ))
zones_shapefile$NOM_ZS[zones_shapefile$NOM_ZS == 'kisanji'] <- 'kisandji'


sampled_zones <- zones_shapefile$NOM_ZS %in%  sample_final$Zone

pdf(file = "output/maps/sampling_map.pdf")
plot(zones_shapefile )
plot(zones_shapefile[sampled_zones , ] , col = 'darkred'  , add = TRUE)
title("Zones échantillonées pour l'enquête")
dev.off()


#La projection de la carte fournie par le SNIS est bizarre, 
#mais je trouve pas commment convertir les coordonnées dans un shapefile. Solution quick and dirty : convertir les coordonnées des sites plutôt...

## devra être réglé si analyses spatiales, sinon on va avoir des distances bizarres

facilities$Latitude2 <- facilities$Latitude * 100000
facilities$Longitude2 <- facilities$Longitude * 100000

for (i in 1:length(unique(sample_final$Province))){ 
  Province <- unique(sample_final$Province)[i]
  sub_fac <- subset(facilities , Province == Province)  
  
  shapefile <- zones_shapefile[zones_shapefile$PROVINCE  == Province, ]
  
  sampled_zones <- shapefile$NOM_ZS %in%  as.character(sample_final$Zone)
  
  sub_fac <- subset(sub_fac , !is.na(Latitude))
  coordinates(sub_fac) <- ~Longitude2+Latitude2
  
  
  plot(shapefile )  
  plot(shapefile[sampled_zones , ] , col = 'darkred'  , add = TRUE)
  plot(sub_fac , add = TRUE , col = as.factor(sub_fac$FacLevel))
  legend("right" , col = c(1:5) , legend =  sort(unique(sub_fac$FacLevel)) , pch = 3)
}