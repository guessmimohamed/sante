library(readxl)
library(ggplot2)
library(plotly)
require(dplyr)
library(lubridate)
library(viridisLite)
library(viridis)
listatt <-  readxl::read_excel("listatt.xls",
                      col_types = c("text", "text", "date",
                                    "numeric", "text", "text", "text",
                                    "text", "date", "text", "text", "text",
                                    "text", "text"))
##################################################################################
V <- mutate(listatt,v=time_length(interval (start=listatt$date_insc,end=Sys.Date() ),
                                         unit = "day"))

C<-aggregate(V$v,by = list(typ2 =V$lib_g), mean)
names(C)=c("region","jour")
C$jour<-round(C$jour,0)


C <- arrange(C, desc(region))
p9<-ggplot2::ggplot(C, aes(x = reorder(region, jour), y = jour)) +
  geom_bar(stat = "identity",aes(fill = jour))  + ###
  geom_text(aes(label = jour), hjust =0.5)+coord_flip() +
  ggtitle("Temps moyen d'attente par gouvernorat") +
  labs(x = "", y = "temps")+scale_color_viridis( option = "D")+
  scale_fill_viridis(option="D") +

  theme(panel.background = element_blank()) ###
p9
###############################################
A <- mutate(listatt,a=time_length(interval (start=listatt$date_dip,end=listatt$date_insc ),
                                  unit = "day"))

B<-aggregate(A$a,by = list(typ2 =A$lib_g), mean,na.rm=T)
D<-aggregate(V$v,by = list(typ2 =V$categorie), mean,na.rm=T)
names(D)=c("categorie","jour")
D$jour<-round(D$jour,0)
p10<-ggplot2::ggplot(D, aes(x = categorie, y = jour)) +
  geom_bar(stat = "identity",aes(fill = jour))  + ###
  geom_text(aes(label = jour), vjust =-1) +
  ggtitle("Temps moyen d'attente par categorie") +
  labs(x = "", y = "temps")+scale_color_viridis( option = "plasma")+
  scale_fill_viridis(option="plasma") +
  theme(panel.background = element_blank())
p10

