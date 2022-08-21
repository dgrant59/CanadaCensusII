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
library(ggnewscale)
# setwd("PATHHERE")


colnames_popdata <-  colnames(fread("98-401-X2021006_English_CSV_data_Ontario.csv",encoding = "Latin-1",skip=0,nrows=1))
popdata <- fread("98-401-X2021006_English_CSV_data_Ontario.csv",encoding = "Latin-1",skip=7593826,nrows=5192928)
colnames(popdata) <- colnames_popdata

popdata <- popdata[,c(1:5,8:12,18)]
popdata %<>% filter(GEO_LEVEL=="Dissemination area") 

popdata %<>% filter(CHARACTERISTIC_ID%in%c(725:1045))#725 for french
popdatanie <- filter(popdata, grepl("n\\.",CHARACTERISTIC_NAME))
popdata <- filter(popdata, !grepl("languages",CHARACTERISTIC_NAME))
popdata <- filter(popdata, !grepl("n\\.",CHARACTERISTIC_NAME))
popdata <- rbind(popdata, popdatanie)
popdata$CHARACTERISTIC_NAME <- trimws(popdata$CHARACTERISTIC_NAME,"both")

# data_qual <- popdata[,c(3,6)] %>% group_by(ALT_GEO_CODE) %>% summarise(Data_qual = toString(DATA_QUALITY_FLAG))
# 
# popdata <- popdata[,-c(6,7,9,11)] %>% group_by(ALT_GEO_CODE) %>% pivot_wider(names_from=CHARACTERISTIC_NAME,values_from=C1_COUNT_TOTAL)
# popdata <- left_join(popdata,data_qual,by=c("ALT_GEO_CODE"="ALT_GEO_CODE"))
# popdata %<>% mutate(ALT_GEO_CODE=factor(ALT_GEO_CODE))

languages <- unique(popdata$CHARACTERISTIC_NAME)

for(i in 1:1){
  print(languages[i])
  ###### MAP DATA
  canada <- read_sf(dsn = "./ShapeFiles/DA Shapefiles/DA Map.shp", 
                    stringsAsFactors = T)
  
  #Join map data to case data for easy ggplotting
  canada <- left_join(canada,popdata %>% group_by(ALT_GEO_CODE) %>% filter(CHARACTERISTIC_NAME==languages[i]), by=c("DGUID"="DGUID")) 
  temp <- canada[canada$DGUID%in%popdata$DGUID,]
  if(sum(temp[!is.na(temp$C10_RATE_TOTAL),]$C10_RATE_TOTAL!=0)){
    ggplot()+
      annotation_spatial(temp,lwd=0.05)+
      layer_spatial(temp,aes(fill=C10_RATE_TOTAL),color="grey91",lwd=0.03)+
      scale_fill_gradient(low = "white", high = "darkgreen",na.value = "red",
                          breaks=c(0,max(temp[!is.na(temp$C10_RATE_TOTAL),]$C10_RATE_TOTAL)),
                          guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"))+
      theme(legend.title.align=0,
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
            plot.title = element_text(hjust = 0.5,vjust=-10,size=18,face="bold"),
            legend.text=element_text(size=10),  #legend text sizes
            legend.title=element_text(size=13),
            plot.margin = unit(c(-1, -1, -0.9, -1), "cm"),
            legend.box.margin=margin(0,0,0,-80))+ 
     guides(fill= guide_colourbar(barwidth=1.5,barheight=6,title.position="top",title = "% Speaking \nLanguage"))+
      ggtitle(paste0("% Speaking\n",languages[i]," \n Most Often at Home \n (2021)"))
    ggsave(paste0("Language",i,".svg"),path="./TorontoMaps/")
  }
  
  
}

