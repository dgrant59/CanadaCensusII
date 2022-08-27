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
library(shinythemes)
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



lang_order <- popdata %>% group_by(Language) %>% summarise(n=n()) %>% arrange(n) %>% select(Language)



###### MAP DATA
canada <- read_sf(dsn = "./ShapeFiles/CensusDivisionSimplified2.shp", 
                  stringsAsFactors = T)              

#convert projection to standardized projection, WSG84
canada <- st_transform(canada, 4326)


#Join map data to case data for easy plotting
canada <- right_join(canada,popdata, by=c("DGUID"="DGUID"),keep=F) 

##### LEAFLET MAP/SHINY APP PREREQS
## THE SHINY APP #####

##3e3f3a
ui <- navbarPage(
  "2021 Census Language Data",
  theme = shinytheme("sandstone"),
  
  tabPanel(
    "Canada (Provincial)",
     
  ),
  tabPanel(
    "Canada (Census Divisions)",
    tags$style(type='text/css', ".tab-content, plot-container plotly {background-color: #f8f5f0;}"),
    tags$style(type='text/css', ".col-sm-8 {padding-top: 50px;
  padding-bottom: 50px;padding-right:30px;}"),
    sidebarLayout(
      sidebarPanel(
        width=4,
        fluidRow(
          p("Here is the blah blah", a(href = 'https://stackoverflow.com/', 'favorite link ever'),"a new paragraph. 
            more text Supply more text Supplymore text Supplymore text Supplymore text Supplymore text Supply"),
          h3("Top Unofficial Language Spoken at Home",style="text-align: center"),
          tags$style(type = "text/css", "#langbar {height: calc(80vh - 120px) !important;}"),
          plotlyOutput(outputId = "langbar", width = "100%",height = "100%")
        )
      ),
      mainPanel(
        width = 8,
        align = "center",
        tags$style(type = "text/css", "#map {height: calc(100vh - 220px) !important;}"),
        plotlyOutput(outputId = "map", width = "100%",height = "100%")
      )
      
    )        
  ),
  navbarMenu(
    "Cities",
    tabPanel(
      "Toronto (Dissemination Areas)",
             
    ),
    tabPanel(
      "Waterloo (Dissemination Areas)",
             
    ),
    tabPanel(
      "London (Dissemination Areas)",
             
    )
  )
)

#get ggplot fill page correctly
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

output$langbar <- renderPlotly({
  ggplotly(
    ggplot(canada,aes(y=Language,fill=Language))+
      scale_fill_manual(values=c(rainbow(33)[1:17],"grey",rainbow(33)[18:28],"black",rainbow(33)[29:33]))+
      geom_bar(aes(text=Language))+
      geom_text(aes(label = Language), stat = "count", hjust="center", nudge_x =strwidth(sort(unique(canada$Language)),font=3,units="in")/2*12, colour = "black",size=3)+
      scale_y_discrete(limits = lang_order$Language)+
      labs(x = NULL, y = NULL)+
      scale_x_continuous(limits=c(0,max(table(canada$Language))+11),expand = c(0,0)) +
      theme(legend.title.align=0.5,
            axis.line=element_blank(),  #bunch of options to remove "graph" visuals
            axis.text.x= element_blank(),
            axis.text.y= element_blank(),
            axis.ticks=element_blank(),
            axis.ticks.length = unit(0, "pt"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            plot.title = element_text(size=12),
            legend.position='none',
            plot.margin=unit(c(0,0,0,0), "mm"),
            legend.title=element_blank()),tooltip = c("text","x")) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30))
})
}

shinyApp(ui = ui, server = server)


