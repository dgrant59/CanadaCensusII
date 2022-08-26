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

popdata %<>% filter(CHARACTERISTIC_ID%in%c(726:1045))#725 for french
popdatanie <- filter(popdata, grepl("n\\.",CHARACTERISTIC_NAME))
popdata <- filter(popdata, !grepl("languages",CHARACTERISTIC_NAME))
popdata <- filter(popdata, !grepl("n\\.",CHARACTERISTIC_NAME))
popdata <- rbind(popdata, popdatanie)
popdata$CHARACTERISTIC_NAME <- trimws(popdata$CHARACTERISTIC_NAME,"both")

colnames(popdata)[8] <- "Language"
popdataNONE <- popdata %>% group_by(ALT_GEO_CODE) %>% filter(sum(C1_COUNT_TOTAL)==0) %>% slice(1) %>% mutate(Language = "No non-English Speakers")

popdata <- popdata %>% group_by(ALT_GEO_CODE) %>% filter(sum(C1_COUNT_TOTAL)!=0) %>% filter(as.integer(ordered(-C1_COUNT_TOTAL))==1)
popdata <- rbind(popdata,popdataNONE)
#2021A00031008 2021A00031311 2021A00035957 are ties
popdataTIES <- popdata %>% group_by(ALT_GEO_CODE) %>% filter(length(ALT_GEO_CODE)>1) %>% mutate(Languages = paste(Language,collapse="//")) %>% slice(1)
popdataTIES$Language <- "Tie (Hover to See Languages)"
popdata %<>% mutate(Languages=Language)
popdata <- popdata %>% filter()
popdata <- popdata %>% group_by(ALT_GEO_CODE) %>% filter(length(ALT_GEO_CODE)==1)
popdata <- rbind(popdata, popdataTIES)



lang_order <- popdata %>% group_by(Language) %>% summarise(n=n()) %>% arrange(desc(n)) %>% select(Language)



###### MAP DATA
canada <- read_sf(dsn = "./ShapeFiles/CensusDivisionSimplified2.shp", 
                  stringsAsFactors = T)              

#convert projection to standardized projection, WSG84
canada <- st_transform(canada, 4326)


#Join map data to case data for easy plotting
canada <- right_join(canada,popdata, by=c("DGUID"="DGUID"),keep=F) 

##### LEAFLET MAP/SHINY APP PREREQS
## THE SHINY APP #####

ui <- fluidPage(
  fillPage(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    column(4,
           plotlyOutput(outputId = "scatter2", width = "100%",height = "100%")),
    column(8,
           plotlyOutput(outputId = "map", width = "100%",height = "100%"))
  )

)
server <- function(input,output,session){
shared_lang <- highlight_key(canada)

# output$map <- renderLeaflet({
#   leaflet(shared_lang) %>%
#     addProviderTiles("CartoDB.Positron") %>%
#     addPolygons(data=canada, color = "#969696", weight = 1, fillColor = "#808080") %>%
#     addPolygons(color = 'white',
#       weight = 2,
#       fillOpacity = .5,
#       fillColor = ~pal(Language)
#     )%>%highlight("plotly_selected", dynamic = TRUE)
# })

output$map <- renderPlotly({
  ggplotly(ggplot(shared_lang) + #fill=CHARACTERISTIC_ID,
             geom_sf(data=canada, fill="white",color="black",lwd=0.03,aes())+
             geom_sf(aes(fill=Language,text=paste("Census Division (CD):",CDNAME,"\nLanguage(s):",Languages, "\n% of CD Population:",C10_RATE_TOTAL)),lwd=0.03)+ #note note combined for tie
             scale_fill_manual(values=c(rainbow(33)[1:17],"grey",rainbow(33)[18:28],"black",rainbow(33)[29:33]))+
             coord_fixed(2)+
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
                    plot.background=element_blank()),tooltip = c("text"))
})

output$scatter2 <- renderPlotly({
  ggplotly(
    ggplot(canada,aes(x=Language,fill=Language))+
      scale_fill_manual(values=rainbow(length(unique(canada$Language))))+
      geom_bar()+
      scale_x_discrete(limits = lang_order$Language)+
      ggtitle("Regions where Language is Most Spoken Non-English Language")+
      theme(legend.title.align=0.5,
            axis.line=element_blank(),  #bunch of options to remove "graph" visuals
            axis.text.x = element_text(angle = -45, hjust=-1,size=11),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            plot.title = element_text(hjust=0.5,size=12),
            legend.position='none',
            legend.title=element_blank()),tooltip = c("x","y"))
})
}

shinyApp(ui = ui, server = server)


#

