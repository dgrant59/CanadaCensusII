library(tidyverse)
library(readxl)
library(magrittr)
library(sf)
library(shiny)
library(shinyjs)
library(bslib)
library(data.table)
library(ggspatial)
library(magrittr)
library(plotly)
library(crosstalk)
library(shinythemes)
library(shinycssloaders)
# library(listviewer)
# library(jsonlite)
library(viridis)
library(colorspace)


popdata <- fread("98-401-X2021005_English_CSV_data.csv",encoding = "Latin-1")

popdata %<>% select(DGUID,ALT_GEO_CODE,GEO_LEVEL,DATA_QUALITY_FLAG,
                    CHARACTERISTIC_ID,CHARACTERISTIC_NAME,C1_COUNT_TOTAL,
                    C10_RATE_TOTAL)
colnames(popdata)[6] <- "Language"



####---------PROVINCIAL-----------------------
popdata_PROV <- popdata %>% filter(GEO_LEVEL=="Province"|GEO_LEVEL=="Territory") 

#725 for french, 724 for english
popdata_PROV %<>% filter(CHARACTERISTIC_ID%in%c(726:1045))

# I am only interested in the individual languages.
# I discovered that individual languages never contain the word "languages"
# BUT I was still interested in categories of the form "German languages, n.X.X".
# 1) filter all "not otherwise indicated" language data to a new tibble
# 2) remove all characteristics with the word "languages", 
# 3) remove all other "n.X.X" languages
# 4) re add the "not otherwise indicated" languages from 1)

popdata_PROV_nie <- filter(popdata_PROV, grepl("n\\.",Language))
popdata_PROV <- filter(popdata_PROV, !grepl("languages",Language))
popdata_PROV <- filter(popdata_PROV, !grepl("n\\.",Language))
popdata_PROV <- rbind(popdata_PROV, popdata_PROV_nie)
popdata_PROV$Language <- trimws(popdata_PROV$Language,"both")

#filter these n-way ties out, THEN go on to select the top language from each CD
#(this will include some ties, but not the large n-way ties from above so they can
#have their own special labels)
popdata_PROV <- popdata_PROV %>% 
  group_by(ALT_GEO_CODE) %>% 
  filter(as.integer(ordered(-C1_COUNT_TOTAL))==1)

write.csv(popdata_PROV,file="popdata_PROV.csv")

####---------CANADA (CD)----------------------
popdata_CD <- popdata %>% filter(GEO_LEVEL=="Census division") 

#725 for french, 724 for english
popdata_CD %<>% filter(CHARACTERISTIC_ID%in%c(726:1045))

# I am only interested in the individual languages.
# I discovered that individual languages never contain the word "languages"
# BUT I was still interested in categories of the form "German languages, n.X.X".
# 1) filter all "not otherwise indicated" language data to a new tibble
# 2) remove all characteristics with the word "languages", 
# 3) remove all other "n.X.X" languages
# 4) re add the "not otherwise indicated" languages from 1)

popdata_CD_nie <- filter(popdata_CD, grepl("n\\.",Language))
popdata_CD <- filter(popdata_CD, !grepl("languages",Language))
popdata_CD <- filter(popdata_CD, !grepl("n\\.",Language))
popdata_CD <- rbind(popdata_CD, popdata_CD_nie)
popdata_CD$Language <- trimws(popdata_CD$Language,"both")

#Since some Census divisions have no non-english/french speakers, this will result
#in an n-way tie of 0 between all languages. Just take first row's language data
#and replace language with "No non-English/french speakers"

popdata_CD_none <- popdata_CD %>% 
  group_by(ALT_GEO_CODE) %>% 
  filter(sum(C1_COUNT_TOTAL)==0) %>% 
  slice(1) %>% 
  mutate(Language = "No non-English/French Speakers")

#filter these n-way ties out, THEN go on to select the top language from each CD
#(this will include some ties, but not the large n-way ties from above so they can
#have their own special labels)
popdata_CD <- popdata_CD %>% 
  group_by(ALT_GEO_CODE) %>% 
  filter(sum(C1_COUNT_TOTAL)!=0) %>% 
  filter(as.integer(ordered(-C1_COUNT_TOTAL))==1)

popdata_CD <- rbind(popdata_CD,popdata_CD_none)


#Give these tie areas their own "Languages" column, which will aggregate all tie 
#languages with a // between them
popdata_CD_ties <- popdata_CD %>% 
  group_by(ALT_GEO_CODE) %>% 
  filter(length(ALT_GEO_CODE)>1) %>% 
  mutate(Languages = paste(Language,collapse="//")) %>% 
  slice(1)
#remove these ties from the rest of the data temporarily
popdata_CD <- popdata_CD %>% 
  group_by(ALT_GEO_CODE) %>% 
  filter(length(ALT_GEO_CODE)==1)

#Use the "Language" column for map colours, and the "Languages" column for
#hover information (so that each tie does not get its own colour on the map,
#but will get its own label when you hover over the CD)
popdata_CD_ties$Language <- "Tie (Hover to See Languages)"
popdata_CD %<>% mutate(Languages=Language)
popdata_CD <- rbind(popdata_CD, popdata_CD_ties)


write.csv(popdata_CD,file="popdata_CD.csv")

####---------CITIES----------------------


colnames_popdata_CITY <-  colnames(fread("98-401-X2021006_English_CSV_data_Ontario.csv",
                                    encoding = "Latin-1",
                                    skip=0,
                                    nrows=1))
#Toronto
# city <- "Toronto"
# city_start <- 7593827
# city_end <- 12788141
# city <- "KWRegion"
# city_start <- 19970028   #"2021S051235300749"
# city_end <- 21043565
# # 
# city <- "London"
# city_start <- 23544327
# city_end <- 24336303
cities <- data.frame(city = c("Toronto","KWRegion","London"),
                     city_start = c(7593827,19970028,23544327),
                     city_end = c(12788141,21043565,24336303))

for(i in 1:nrow(cities)){
popdata_CITY <- fread("98-401-X2021006_English_CSV_data_Ontario.csv",
                 encoding = "Latin-1",
                 skip=cities$city_start[i]-1,
                 nrows=cities$city_end[i]-cities$city_start[i]+1)
colnames(popdata_CITY) <- colnames_popdata_CITY

#Take only a subset of columns, year, location data, counts, %s
popdata_CITY %<>% select(DGUID,ALT_GEO_CODE,GEO_LEVEL,DATA_QUALITY_FLAG,
                    CHARACTERISTIC_ID,CHARACTERISTIC_NAME,C1_COUNT_TOTAL,
                    C10_RATE_TOTAL)
colnames(popdata_CITY)[6] <- "Language"

#Since data is hierarchical, only take DA-level data
#(Can switch this for "Census division" or "Census subdivision" if you want
#to plot less granularly)
popdata_CITY %<>% filter(GEO_LEVEL=="Dissemination area") 

# Each DA has >1000 attributes recorded. We only want attributes associated
# with "Most common language spoken at home", i.e. CHARACTERISTIC_ID 725-1045
# to include English, change to 724, to exclude both Eng and French, change to 726

popdata_CITY %<>% filter(CHARACTERISTIC_ID%in%c(726:1045))

popdata_CITY_nie <- filter(popdata_CITY, grepl("n\\.",Language))
popdata_CITY <- filter(popdata_CITY, !grepl("languages",Language))
popdata_CITY <- filter(popdata_CITY, !grepl("n\\.",Language))
popdata_CITY <- rbind(popdata_CITY, popdata_CITY_nie)
popdata_CITY$Language <- trimws(popdata_CITY$Language,"both")

#Since some Census divisions have no non-english/french speakers, this will result
#in an n-way tie of 0 between all languages. Just take first row's language data
#and replace language with "No non-English/french speakers"

popdata_CITY_none <- popdata_CITY %>% 
  group_by(ALT_GEO_CODE) %>% 
  filter(sum(C1_COUNT_TOTAL)==0) %>% 
  slice(1) %>% 
  mutate(Language = "No non-English/French Speakers")

popdata_CITY_missing <- popdata_CITY %>% 
  group_by(ALT_GEO_CODE) %>% 
  filter(all(is.na(C1_COUNT_TOTAL))) %>% 
  slice(1) %>% 
  mutate(Language = "NO DATA")

#filter these n-way ties out, THEN go on to select the top language from each DA in 
#Toronto (this will include some ties, but not the large n-way ties from above so they can
#have their own special labels)
popdata_CITY <- popdata_CITY %>% 
  group_by(ALT_GEO_CODE) %>% 
  filter(sum(C1_COUNT_TOTAL)!=0) %>% 
  filter(as.integer(ordered(-C1_COUNT_TOTAL))==1)

popdata_CITY <- rbind(popdata_CITY,popdata_CITY_none,popdata_CITY_missing)


#Give these tie areas their own "Languages" column, which will aggregate all tie 
#languages with a // between them
popdata_CITY_ties <- popdata_CITY %>% 
  group_by(ALT_GEO_CODE) %>% 
  filter(length(ALT_GEO_CODE)>1) %>% 
  mutate(Languages = paste(Language,collapse="//")) %>% 
  slice(1)
#remove these ties from the rest of the data temporarily
popdata_CITY <- popdata_CITY %>% 
  group_by(ALT_GEO_CODE) %>% 
  filter(length(ALT_GEO_CODE)==1)

#Use the "Language" column for map colours, and the "Languages" column for
#hover information (so that each tie does not get its own colour on the map,
#but will get its own label when you hover over the DA)
popdata_CITY_ties$Language <- "Tie (Hover to See Languages)"
popdata_CITY %<>% mutate(Languages=Language)
popdata_CITY <- rbind(popdata_CITY, popdata_CITY_ties)

write.csv(popdata_CITY,file=paste0("popdata_",cities$city[i],".csv"))



canada <- read_sf(dsn = "./ShapeFiles/DA Shapefiles/FullDA.shp",
                  stringsAsFactors = T)

st_write(subset(canada, DGUID%in%unique(popdata_CITY$DGUID)),paste0("./ShapeFiles/DA Shapefiles/",cities$city[i],".shp"))
}
#https://mapshaper.org/ simplify map here, otherwise read in the above shp file





