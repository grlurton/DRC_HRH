setwd('C:/Users/grlurton/Documents/DRCHRH')

stage <- 'analysis'

source('code/useful_functions.r')

## Extract revenues from loops

melted_indiv <- melt(indiv , id = c("instanceID" , "Province" , "Sex" , "Age" , "Matrimonial" , 
                                  "NumberFinancialDependants" , "LastEduc" , "RoleInit" ,
                                  "FacilityType" , "Structure" , "Role" , "HonorairePeriod" , "HeureSupPeriod"))

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

tot_rev$period <- ''
tot_rev$period[tot_rev$variable == 'HeureSupDollar'] <- tot_rev$HeureSupPeriod[tot_rev$variable == 'HeureSupDollar']
tot_rev$period[tot_rev$variable == 'HonoraireDollar'] <- tot_rev$HonorairePeriod[tot_rev$variable == 'HonoraireDollar']



for(i in 1:nrow(nonIndivRev)){
  tab <- eval(parse(text=nonIndivRev$RevenueTable[i]))
  var <- nonIndivRev$RevenueAmount[i]
  lab <- nonIndivRev$RevenueLabel[i]
  period <- nonIndivRev$Periodicity[i]
  print(lab)
  out <- extract_from_loop(tab , lab , var)
  if (period %in% c('mois_precedent' , 'bimestre_precedent' , 'trimestre_precedent')){
    out$period <- period
  }
  if (!(period %in% c('mois_precedent' , 'bimestre_precedent' , 'trimestre_precedent'))){
    out$period <- tab[,period]
  }
  tot_rev <- rbind(tot_rev , out)
}


IndivRev <- revenue_entry[revenue_entry$RevenueTable == 'indiv' ,]

for(i in 1:nrow(IndivRev)){
  rev <- IndivRev$RevenueAmount[i]
  revlab <- IndivRev$RevenueLabel[i]
  tot_rev$variable[tot_rev$variable == rev] <- revlab
  period <- IndivRev$Periodicity[i]
  if (period %in% c('mois_precedent' , 'bimestre_precedent' , 'trimestre_precedent')){
    tot_rev$period[tot_rev$variable == revlab] <- period
  }
}

tot_rev <- subset(tot_rev , !is.na(value) & !(period %in% c('','autre')))
  


### Getting Monthly values

NormalizeIncome <- function(Amount , Period){
  AmountNorm <- Amount
  AmountNorm[Period == 'bimestre_precedent'] <- Amount[Period == 'bimestre_precedent']/2
  AmountNorm[Period == 'trimestre_precedent'] <- Amount[Period == 'trimestre_precedent']/3
  AmountNorm
}

tot_rev$value <- as.numeric(tot_rev$value)

tot_rev$MonthlyDollar <- NormalizeIncome(tot_rev$value , tot_rev$period)


### Drop Unlikely values

qplot(data = tot_rev , x = value) +
  facet_wrap(~variable , scales = 'free')

outliers <- unique(tot_rev$instanceID[
  (tot_rev$variable == 'Activité  Privée' & tot_rev$value > 7000) |
  (tot_rev$variable == 'Activité non santé' & tot_rev$value > 2000) |
  (tot_rev$variable == 'Prime Locale' & tot_rev$value > 1000) |
  (tot_rev$variable == 'Per Diem' & tot_rev$value > 500) |
  (tot_rev$variable == 'Prime de Partenaire' & tot_rev$value > 750) |
  (tot_rev$variable == 'Prime de Risque' & tot_rev$value > 750) |  
  (tot_rev$variable == 'Wage' & tot_rev$value > 1000) ]
  )

tot_rev <- subset(tot_rev , !(instanceID %in% outliers))

qplot(data = tot_rev , x = value) +
  facet_wrap(~variable , scales = 'free')

## Unfolding Brackets

ub_abaque <- read.csv('data/unfolding_brackets.csv' , as.is = TRUE)

ub_data <- indiv[ ,  c(ub_abaque$Label , 'instanceID')]

ub_flat <- melt(ub_data , id = 'instanceID')
ub_flat <- subset(ub_flat , !is.na(value) & value != '')

ub_flat <- merge(ub_flat , ub_abaque , by.x = 'variable' , by.y = 'Label')

ub_last <- ddply(ub_flat , .(instanceID) , 
                 function(data){
                   mm <- max(data$rank)
                   subset(data , rank == mm)
                 })

ub_unit <- subset(melt(indiv , id = 'instanceID') ,
                  variable %in% c('IndivVendeMedicMontantUnit' , 'IndivCadeauMontantUnit') & value != '')

colnames(ub_unit) <- c("instanceID" , "variable" , "unit")

ub_last$variable <- as.character(ub_last$variable)
ub_unit$variable <- as.character(ub_unit$variable)
ub_unit$variable[ub_unit$variable == 'IndivCadeauMontantUnit'] <- 'IndivCadeauMontantExact'
ub_unit$variable[ub_unit$variable == 'IndivVendeMedicMontantUnit'] <- 'IndivVendeMedicMontantExact'


ub_last <- merge(ub_last , ub_unit , by.x = c('instanceID' , 'variable') , by.y =  c('instanceID' , 'variable') ,
                 all = TRUE)

table(ub_last$variable[ub_last$value == 'no_response'])
ub_last <- subset(ub_last , value != 'no_response')

ub_last_num <- subset(ub_last , !is.na(unit))
ub_last_num$value <- as.numeric(ub_last_num$value)
ub_last_num <- StandardizeOver(data = ub_last_num , Value = 'value' , Currency = 'unit' , 'unfoldingBracket')

ub_bracketing <- read.csv('data/ub_bracketing.csv')
ub_last_bracket <- subset(ub_last , is.na(unit))
ub_last_bracket <- merge(ub_last_bracket , ub_bracketing , by.x = 'value' , by.y = 'value')
ub_last_bracket$amount <- runif(n = nrow(ub_last_bracket) , min = ub_last_bracket$min , max = ub_last_bracket$max)



mean(runif(n = nrow(ub_last_bracket) , min = ub_last_bracket$min , max = ub_last_bracket$max))
mean(ub_last_num$unfoldingBracketDollar[ub_last_num$unfoldingBracketDollar != 0 & 
                                          ub_last_num$unfoldingBracketDollar < 1000])

ext_ub_brack <- subset(ub_last_bracket , select = c(variable, amount  , instanceID))
ext_ub_num <- subset(ub_last_num , select = c(variable , unfoldingBracketDollar , instanceID))

colnames(ext_ub_brack) <- colnames(ext_ub_num) <- c('variable' , 'value' , 'instanceID')

ub_final <- rbind(ext_ub_brack , ext_ub_num)
ub_final <- subset(ub_final , value < 1000)


ub_final$variable[substr(ub_final$variable , 1 , 6) == 'IndivC'] <- 'Cadeau'
ub_final$variable[substr(ub_final$variable , 1 , 6) == 'IndivV'] <- 'Vente de Medicament'

ddply(ub_final , .(variable) , function(x) mean(x$value))

qplot(data = ub_final[ub_final$value < 100 ,]  , x = value) +
  facet_wrap(~ variable)

total_ub <- merge(ub_final , indiv_list , by = 'instanceID')
total_ub$period <- 'mois_precedent'
total_ub$MonthlyDollar <- total_ub$value

total_ub <- subset(total_ub , MonthlyDollar != 0)
 
ddply(total_ub , .(Province , variable) , function(x) mean(x$value) )

tot_rev <- rbind(tot_rev , total_ub)

### Resume Income

rev_resumed <- ddply(tot_rev , .(instanceID , variable) , function(x) sum(x$MonthlyDollar))
qplot(data = rev_resumed , x = V1) + facet_wrap(~variable , scales = 'free')


## Add facilities variables

FacRelevant <- subset(facilities ,  select = c("Structure"  , "FacLevel" , "FacOwnership" , 
                                               "FacAppui" , "FacRurban" , "EczAppui"))
tot_rev_full <- merge(FacRelevant , tot_rev , by = 'Structure' , all.x = FALSE)


write.csv(tot_rev_full , 'data/questionnaires_analysis/total_revenue_table.csv')
