library(tidyverse)
library(sf)
library(readxl)
library(data.table)
library(ggspatial)
library(magrittr)
library(fuzzyjoin)
library(tmap)
library(RColorBrewer)
library(svglite)
# setwd("PATHHERE")


popdata <- fread("98-401-X2021005_English_CSV_data.csv",encoding = "Latin-1")

popdata <- popdata[,c(1:5,8:12,18)]
popdata %<>% filter(GEO_LEVEL=="Dissemination area") 

popdata %<>% filter(CHARACTERISTIC_ID%in%c(725:1045))#725 for french
popdatanie <- filter(popdata, grepl("n\\.",CHARACTERISTIC_NAME))
popdata <- filter(popdata, !grepl("languages",CHARACTERISTIC_NAME))
popdata <- filter(popdata, !grepl("n\\.",CHARACTERISTIC_NAME))
popdata <- rbind(popdata, popdatanie)
popdata$CHARACTERISTIC_NAME <- trimws(popdata$CHARACTERISTIC_NAME,"both")

popdata <- popdata %>% group_by(ALT_GEO_CODE) %>% filter(sum(C1_COUNT_TOTAL)!=0) %>% filter(as.integer(ordered(-C1_COUNT_TOTAL))==1)
#popdata <- popdata %>% group_by(ALT_GEO_CODE) %>% filter(CHARACTERISTIC_NAME=="Mandarin")
#this is wrong
popdata2 <- popdata %>% group_by(ALT_GEO_CODE) %>% filter(length(ALT_GEO_CODE)==1)
popdata234234234 <- popdata%>%group_by(ALT_GEO_CODE) %>% filter(length(ALT_GEO_CODE)>1) %>%distinct(.,ALT_GEO_CODE,.keep_all = T)



data_qual <- popdata[,c(3,6)] %>% group_by(ALT_GEO_CODE) %>% summarise(Data_qual = toString(DATA_QUALITY_FLAG))

popdata <- popdata[,-c(6,7,9,11)] %>% group_by(ALT_GEO_CODE) %>% pivot_wider(names_from=CHARACTERISTIC_NAME,values_from=C1_COUNT_TOTAL)

popdata <- left_join(popdata,data_qual,by=c("ALT_GEO_CODE"="ALT_GEO_CODE"))
popdata %<>% mutate(ALT_GEO_CODE=factor(ALT_GEO_CODE))

###### MAP DATA
canada <- read_sf(dsn = "./ShapeFiles/DA Shapefiles/DA Map.shp", 
                  stringsAsFactors = T)

# canada <- read_sf(dsn = "./ShapeFiles/CensusDivisionSimplified.shp", 
#                    stringsAsFactors = T)

#Join map data to case data for easy ggplotting
canada <- left_join(canada,popdata2, by=c("DGUID"="DGUID")) 


# dates <- colnames(ontario@data)[10:(length(colnames(ontario@data))-1)]
#places <- c(10:13,24,35,46:48,59:62)

# c("Newfoundland and Labrador\n(Terre-Neuve-et-Labrador)",
#   "Prince Edward Island\n(Île-du-Prince-Édouard)",
#   "Nova Scotia\n(Nouvelle-Écosse)",
#   "New Brunswick\n(Nouveau-Brunswick)",
#   "Quebec\n(Québec",
#   "Ontario",
#   "Manitoba",
#   "Saskatchewan",
#   "Alberta",
#   "British Columbia\n(Colombie-Britannique)",
#   "Yukon",
#   "Northwest Territories\n(Territoires du Nord-Ouest)",
#   "Nunavut")
places <- c(35)
pnames <- c("Ontario")
temp <- canada[canada$DGUID%in%popdata$DGUID,]
# temp <- canada[gsub("(^\\d{2}).*", "\\1", as.integer(as.character(canada$PRUID)))==places[i],]
ggplot()+
  annotation_spatial(temp,lwd=0.05)+
  layer_spatial(temp,aes(fill=CHARACTERISTIC_NAME),color="black",lwd=0.03)+
   scale_fill_manual(values=rainbow(51))+
   labs(fill="% Speaking Language at Home")+
  theme(legend.title.align=0.5,
        axis.line=element_blank(),  #bunch of options to remove "graph" visuals
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.title = element_text(hjust = 0.5,size=15),
        legend.text=element_text(size=10),  #legend text sizes
        legend.title=element_text(size=13))+ 
  ggtitle(paste0("Most Popular Non-English Language\n Spoken at Home in ",pnames[1],"\n2021"))+
  guides(colour=guide_legend("", override.aes=list(colour="black")))

  

languages <- c(`Arabic`="lightcoral",
               `Punjabi (Panjabi)`="red",
               `Urdu`="firebrick",
               `Gujarati`="tomato",
               `Tamil`="darkred",
               
               `Innu (Montagnais)`="darkgreen",
               `Inuktitut`="green",
               `Mi'kmaq`="darkolivegreen1",
               `Oji-Cree`="darkseagreen3",
               `Atikamekw`="chartreuse",
               `Dakelh (Carrier)`="darkolivegreen",
               `Inuinnaqtun (Inuvialuktun)`="chartreuse",
               `Tlicho (Dogrib)`="green",
               `Cree, n.o.s.`="darkgreen",
               `Ojibway, n.o.s.`="chartreuse",
               `Dene, n.o.s.`="darkseagreen",
               `Slavey, n.o.s.`="darkgreen",
               `Anicinabemowin (Algonquin)`="chartreuse4",
               
               `Tagalog (Pilipino, Filipino)`="coral",
               `Mandarin`="pink",
               `Korean`="deeppink",
               `Yue (Cantonese)`="deeppink3",
               `Vietnamese`="darksalmon",
               
               `French`="deepskyblue",
               `American Sign Language`="aliceblue",
               `German`="blue",
               `Spanish`="darkslateblue",
               `Russian`="blue4",
               `Pennsylvania German`="darkcyan",
               `Portuguese`="deepskyblue4",
               `Polish`="cornflowerblue",
               `Italian`="cyan",
               `Germanic languages, n.i.e.`="cyan3",
               
               `Yiddish`="red")

# `Arabic`="darkslateblue",
# `Punjabi (Panjabi)`="darkmagenta",
# `Urdu`="darkorchid4",
# `Gujarati`="darkorchid",
# `Tamil`="darkorchid2",


# ggsave(paste0(sub("\\n.*","",pnames[i]),".pdf"),path="./Maps/")

# 10 	Newfoundland and Labrador/Terre-Neuve-et-Labrador
# 11 	Prince Edward Island/Île-du-Prince-Édouard
# 12 	Nova Scotia/Nouvelle-Écosse
# 13 	New Brunswick/Nouveau-Brunswick
# 24 	Quebec/Québec
# 35 	Ontario
# 46 	Manitoba
# 47 	Saskatchewan
# 48 	Alberta
# 59 	British Columbia/Colombie-Britannique
# 60 	Yukon
# 61 	Northwest Territories/Territoires du Nord-Ouest
# 62 	Nunavut

#Add interaction?
#canada@data$CSDUID

#tm_shape(temp)+tm_fill(col="Population percentage change, 2016 to 2021")+tm_borders()

