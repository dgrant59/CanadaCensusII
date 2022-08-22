library(tidyverse)
library(sf)
library(data.table)
library(ggspatial)
library(magrittr)
library(svglite)

#Popdata is statcan Census data at the dissemination area (DA) level. Found here
#https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E

#Canada's shapemap files are available here
#https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
#where you can choose DA, census division, or census subdivision levels to correspond
#to the census data you want.

#Read in column names, add them to data taken after skipping 7593826 lines 
#(this is where Toronto starts in the Canada-wide census data)
#refer to StatCan's supplemental file that indicates where locations are within 
#the large main file

colnames_popdata <-  colnames(fread("98-401-X2021006_English_CSV_data_Ontario.csv",
                                    encoding = "Latin-1",
                                    skip=0,
                                    nrows=1))
popdata <- fread("98-401-X2021006_English_CSV_data_Ontario.csv",
                 encoding = "Latin-1",
                 skip=7593826,
                 nrows=5192928)
colnames(popdata) <- colnames_popdata

#Take only a subset of columns, year, location data, counts, %s
head(popdata)
popdata %<>% popdata[,c(1:5,8:12,18)]

#Since data is hierarchical, only take DA-level data
#(Can switch this for "Census division" or "Census subdivision" if you want
#to plot less granularly)
popdata %<>% filter(GEO_LEVEL=="Dissemination area") 

# Each DA has >1000 attributes recorded. We only want attributes associated
# with "Most common language spoken at home", i.e. CHARACTERISTIC_ID 725-1045
# to include English, change to 724, to exclude both Eng and French, change to 726

popdata %<>% filter(CHARACTERISTIC_ID%in%c(725:1045))

#Language data here is also hierarchical, i.e. 
# Non-Indigenous languages
#   Afro-Asiatic languages
#     Semitic languages
#       Arabic
# and statistics are summarized at each level

# I am only interested in the individual languages.
# I discovered that individual languages never contain the word "languages"
# BUT I was still interested in categories of the form "German languages, n.X.X"
# filter all "not otherwise indicated" languages out, remove all 
# characteristics with the word "languages", remove all other "n.X.X" languages
# then re add the "not otherwise indicated" languages.

popdatanie <- filter(popdata, grepl("n\\.",CHARACTERISTIC_NAME))
popdata <- filter(popdata, !grepl("languages",CHARACTERISTIC_NAME))
popdata <- filter(popdata, !grepl("n\\.",CHARACTERISTIC_NAME))
popdata <- rbind(popdata, popdatanie)
popdata$CHARACTERISTIC_NAME <- trimws(popdata$CHARACTERISTIC_NAME,"both")


#List of unique languages that we have data on
languages <- unique(popdata$CHARACTERISTIC_NAME)

#Looping through all languages to create .svg files for each

for(i in 1:length(languages)){
  print(languages[i])
  ###### MAP DATA
  canada <- read_sf(dsn = "./ShapeFiles/DA Shapefiles/DA Map.shp", 
                    stringsAsFactors = T)
  
  #Join map data to case data for easy ggplotting
  #Each DA (or CD/CS) has a specific ID called DGUID, match this to the ones found
  #in the .shp file
  canada <- left_join(canada,popdata %>% 
                        group_by(ALT_GEO_CODE) %>% 
                          filter(CHARACTERISTIC_NAME==languages[i]), 
                                 by=c("DGUID"="DGUID")) 
  
  #"canada" is all of canada, we only want Toronto so only use DGUID from popdata
  temp <- canada[canada$DGUID%in%popdata$DGUID,]
  
  #if nobody in toronto speaks that language (or too few to be shown at 1 decimal
  #place), skip mapping it
  if(sum(temp[!is.na(temp$C10_RATE_TOTAL),]$C10_RATE_TOTAL!=0)){
    ggplot()+
      layer_spatial(temp,aes(fill=C10_RATE_TOTAL,color=""),lwd=0.05)+
      scale_fill_gradient(low = "white", high = "blue",na.value = "grey20",
                          breaks=c(0,max(temp[!is.na(temp$C10_RATE_TOTAL),]$C10_RATE_TOTAL)),
                          guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"))+
      scale_colour_manual(values="black") +              
      guides(colour=guide_legend("No data", override.aes=list(fill="grey20")))+
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
            plot.title = element_text(hjust = 0.5,
                                      vjust=-15,
                                      size=22,
                                      face="bold",
                                      margin = margin(-20,0,-20,0)),
            legend.text=element_text(size=10),  #legend text sizes
            legend.title=element_text(size=13),
            plot.margin = unit(c(-1, -1, -0.9, -1), "cm"),
            legend.box.margin=margin(0,0,0,-80))+ 
      guides(fill=guide_colourbar(barwidth=1.5,
                                  barheight=6,
                                  title.position="top",
                                  title = "% Speaking \nLanguage"))+
      ggtitle(paste0("% Speaking\n",languages[i]," \n Most Often at Home \n (2021)"))
    
    #Save to .svg
    # ggsave(paste0(sub("\\,.*", "", languages[i]),".svg"),path="./TorontoMaps/",
    #        width=3500,
    #        height=3000,
    #        dpi=300,
    #        units="px")
  }
}

