library(readxl)
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)

importation <- read_csv("importation.csv",
                        col_types = cols(`DATED,D` = col_date(format = "%m/%d/%Y"),
                                         `NUMD,C,15` = col_character()))
#source('C:/Users/mohamed/Desktop/buji/nouv.R')
local_prive_med <- importation %>%  dplyr::select(`DATED,D`,`DESTYPE,C,20`,`QUANTITE,N,9,0`,`PRIX,N,11,3`,`TAUX,N,20,9`)
names(local_prive_med)<- c('Date','Type','Quantite','PRIX','TAUX')
local_prive_med %<>% filter(Type == "LOCAL PRIVE")
local_hospitalie_med <- importation %>%  dplyr::select(`DATED,D`,`DESTYPE,C,20`,`QUANTITE,N,9,0`,`PRIX,N,11,3`,`TAUX,N,20,9`)
names(local_hospitalie_med)<- c('Date','Type','Quantite','PRIX','TAUX')
local_hospitalie_med %<>% filter(Type == "LOCAL HOSPITALIER")
local_med<-rbind(local_hospitalie_med,local_prive_med)

importe_prive_med <- importation %>%  dplyr::select(`DATED,D`,`DESTYPE,C,20`,`QUANTITE,N,9,0`,`PRIX,N,11,3`,`TAUX,N,20,9`)
names(importe_prive_med)<- c('Date','Type','Quantite','PRIX','TAUX')
importe_prive_med %<>% filter(Type == "IMPORTE PRIVE")
importe_hospitalie_med <- importation %>%  dplyr::select(`DATED,D`,`DESTYPE,C,20`,`QUANTITE,N,9,0`,`PRIX,N,11,3`,`TAUX,N,20,9`)
names(importe_hospitalie_med)<- c('Date','Type','Quantite','PRIX','TAUX')
importe_hospitalie_med %<>% filter(Type == "IMPORTE HOSPITALIER")
importe_med<-rbind(importe_hospitalie_med,importe_prive_med)

Quantite_local<-aggregate(local_med$Quantite,
                    by = list(typ2 = year(local_med$Date)), sum)
names(Quantite_local)<-c('year','quant')
Quantite_local %<>% filter(year<2019)

prix_Quantite_local<-aggregate(local_med$Quantite*local_med$PRIX*local_med$TAUX,
                          by = list(typ2 = year(local_med$Date)), sum)
names(prix_Quantite_local)<-c('year','prix')
prix_Quantite_local %<>% filter(year<2019)
############
Quantite_importe<-aggregate(importe_med$Quantite,
                          by = list(typ2 = year(importe_med$Date)), sum)
names(Quantite_importe)<-c('year','quant')
Quantite_importe %<>% filter(year<2019)

prix_Quantite_importe<-aggregate(importe_med$Quantite*importe_med$PRIX*importe_med$TAUX,
                               by = list(typ2 = year(importe_med$Date)), sum)
names(prix_Quantite_importe)<-c('year','prix')
prix_Quantite_importe %<>% filter(year<2019)
########## hist_Quant ##########
Base_final1 <- cbind(Quantite_local,Quantite_importe$quant)
names(Base_final1)<-c('Annee','Quantite_locale','Quantite_importee')

######### hist_Prix#########
Base_final2 <- cbind(prix_Quantite_local,prix_Quantite_importe$prix)
names(Base_final2) <- c('Annee','Prix_Quantite_locale','Prix_Quantite_importee')






