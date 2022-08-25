library(tidyverse)
library(readxl)
library(magrittr)
library(RColorBrewer)
library(sf)
library(shiny)
library(shinyjs)
library(leaflet)
library(leafpop)
library(bslib)
library(scatterD3)
library(data.table)
library(ggspatial)
library(magrittr)
library(tmap)
library(svglite)
library(plotly)
library(crosstalk)
popdata <- fread("98-401-X2021005_English_CSV_data.csv",encoding = "Latin-1")




popdata <- popdata[,c(1:5,8:12,18)]
popdata %<>% filter(GEO_LEVEL=="Census division") 

popdata %<>% filter(CHARACTERISTIC_ID%in%c(725:1045))#725 for french
popdatanie <- filter(popdata, grepl("n\\.",CHARACTERISTIC_NAME))
popdata <- filter(popdata, !grepl("languages",CHARACTERISTIC_NAME))
popdata <- filter(popdata, !grepl("n\\.",CHARACTERISTIC_NAME))
popdata <- rbind(popdata, popdatanie)
popdata$CHARACTERISTIC_NAME <- trimws(popdata$CHARACTERISTIC_NAME,"both")

popdata <- popdata %>% group_by(ALT_GEO_CODE) %>% filter(sum(C1_COUNT_TOTAL)!=0) %>% filter(as.integer(ordered(-C1_COUNT_TOTAL))==1)
#2021A00031008 2021A00031311 2021A00035957 are ties

#popdata <- popdata %>% group_by(ALT_GEO_CODE) %>% filter(CHARACTERISTIC_NAME=="Mandarin")
#this is wrong
popdata2 <- popdata %>% group_by(ALT_GEO_CODE) %>% filter(length(ALT_GEO_CODE)==1)




popdata2$randx <- runif(nrow(popdata2))
popdata2$randy <- runif(nrow(popdata2))

canada <- read_sf(dsn = "./ShapeFiles/CensusDivisionSimplified.shp", 
                  stringsAsFactors = T)



###### MAP DATA
# canada <- read_sf(dsn = "TestReduced.shp", 
#                   stringsAsFactors = T)

#convert projection to standardized projection, WSG84
canada <- st_transform(canada, 4326)

#Join map data to case data for easy plotting
canada <- right_join(canada,popdata2[popdata2$ALT_GEO_CODE<1100,], by=c("DGUID"="DGUID"),keep=F) 

#ALT_GEO_CODE contains the province/territory for each subdivision as its first 2 digits, take these
#2 digits and make a new column
# PRUID <- factor(gsub("(^\\d{2}).*", "\\1", as.integer(as.character(popdata$ALT_GEO_CODE)),useBytes = T))
# popdata$PRUID <- PRUID
# 
# #Remove decimals from population counts
# popdata %<>% mutate(`Population, 2016`= as.integer(`Population, 2016`))
# popdata %<>% mutate(`Population, 2021`= as.integer(`Population, 2021`))

#These are the StatCan's province/territory IDs, will use these names/IDs as a key
#to match PRUID in popdata
#places <- c(10:13,24,35,46:48,59:62)
# pnames <- c("Newfoundland and Labrador\n(Terre-Neuve-et-Labrador)",
#             "Prince Edward Island\n(Île-du-Prince-Édouard)",
#             "Nova Scotia\n(Nouvelle-Écosse)",
#             "New Brunswick\n(Nouveau-Brunswick)",
#             "Quebec\n(Québec",
#             "Ontario",
#             "Manitoba",
#             "Saskatchewan",
#             "Alberta",
#             "British Columbia\n(Colombie-Britannique)",
#             "Yukon",
#             "Northwest Territories\n(Territoires du Nord-Ouest)",
#             "Nunavut")
# #Can quickly change between EN and FR by changing the definition in sub() below
# pnames_simp <- data.frame(place = paste0(sub("\\n.*","",pnames)),
#                           ID = as.numeric(levels(popdata$PRUID)))
# #Associate PRUID with named province/territory
# popdata %<>% mutate(PRName = factor(PRUID, labels = pnames_simp[,1]))
# pnames_simp <- pnames_simp[order(paste0(sub("\\n.*","",pnames))),]
# 
# #Change census subdivision column name for nice output in leaflet popup
# names(canada)[11] <- "Name"
# names(popdata)[5] <- "Name"

##### LEAFLET MAP/SHINY APP PREREQS
canadaworld <- as(canada,"Spatial")
## THE SHINY APP #####
pal <- colorFactor(
  palette = rainbow(length(unique(canada$CHARACTERISTIC_NAME))),
  domain = canada$CHARACTERISTIC_NAME
)

ui <- fluidPage(
  plotlyOutput(outputId = "scatter"),
  plotlyOutput(outputId = "scatter2"),
  plotlyOutput(outputId = "map")
)
server <- function(input,output,session){
  shared_lang <- SharedData$new(canada)
  shared_lang_df <- SharedData$new(canada,group=shared_lang$groupName())
# output$map <- renderLeaflet({
#   leaflet(shared_lang) %>%
#     addProviderTiles("CartoDB.Positron") %>%
#     addPolygons(data=canada, color = "#969696", weight = 1, fillColor = "#808080") %>%
#     addPolygons(color = 'white',
#       weight = 2,
#       fillOpacity = .5,
#       fillColor = ~pal(CHARACTERISTIC_NAME)
#     )%>%highlight("plotly_selected", dynamic = TRUE)
# })

output$map <- renderPlotly({
  ggplotly(ggplot(shared_lang) +
             geom_sf(aes(fill=CHARACTERISTIC_NAME))+
              theme_minimal())
})
output$scatter <- renderPlotly({
  ggplotly(
    ggplot(shared_lang_df,aes(x=LANDAREA,y=LANDAREA))+
      geom_point()+
        theme_minimal())
  })
output$scatter2 <- renderPlotly({
  ggplotly(
    ggplot(shared_lang_df,aes(x=LANDAREA,y=LANDAREA))+
      geom_point()+
      theme_minimal())
})
}

shinyApp(ui = ui, server = server)




