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
library(shinycssloaders)
library(listviewer)
library(jsonlite)
library(viridis)
popdata_PROV <- read.csv("popdata_PROV.csv")

lang_order_PROV <- popdata_PROV %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)


popdata_CD <- read.csv("popdata_CD.csv")

lang_order_CD <- popdata_CD %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)

popdata_Toronto <- read.csv("popdata_Toronto.csv")

lang_order_Toronto <- popdata_Toronto %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)

popdata_KWRegion <- read.csv("popdata_KWRegion.csv")

lang_order_KWRegion <- popdata_KWRegion %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)

popdata_London <- read.csv("popdata_London.csv")

lang_order_London <- popdata_London %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)

###### MAP DATA
#--------------------------------------------------------------------------
canada_PROV <- read_sf(dsn = "./ShapeFiles/PROV Shapefiles/PROVSimplified.shp", 
                     stringsAsFactors = T)              

#convert projection to standardized projection, WSG84
canada_PROV <- st_transform(canada_PROV, 4326)


#Join map data to case data for easy plotting
canada_PROV <- right_join(canada_PROV,popdata_PROV, by=c("DGUID"="DGUID"),keep=F) 

#--------------------------------------------------------------------------
canada_CD <- read_sf(dsn = "./ShapeFiles/CensusDivisionSimplified2.shp", 
                  stringsAsFactors = T)              

#convert projection to standardized projection, WSG84
canada_CD <- st_transform(canada_CD, 4326)


#Join map data to case data for easy plotting
canada_CD <- right_join(canada_CD,popdata_CD, by=c("DGUID"="DGUID"),keep=F) 
#--------------------------------------------------------------------------

canada_Toronto <- read_sf(dsn = "./ShapeFiles/DA Shapefiles/TorontoSimplified.shp", 
                     stringsAsFactors = T)              

#convert projection to standardized projection, WSG84
canada_Toronto <- st_transform(canada_Toronto, 4326)


#Join map data to case data for easy plotting
canada_Toronto <- right_join(canada_Toronto,popdata_Toronto, by=c("DGUID"="DGUID"),keep=F) 

canada_KWRegion <- read_sf(dsn = "./ShapeFiles/DA Shapefiles/KWRegionSimplified.shp", 
                          stringsAsFactors = T)              

#convert projection to standardized projection, WSG84
canada_KWRegion <- st_transform(canada_KWRegion, 4326)


#Join map data to case data for easy plotting
canada_KWRegion <- right_join(canada_KWRegion,popdata_KWRegion, by=c("DGUID"="DGUID"),keep=F) 

canada_London <- read_sf(dsn = "./ShapeFiles/DA Shapefiles/LondonSimplified.shp", 
                          stringsAsFactors = T)              

#convert projection to standardized projection, WSG84
canada_London <- st_transform(canada_London, 4326)


#Join map data to case data for easy plotting
canada_London <- right_join(canada_London,popdata_London, by=c("DGUID"="DGUID"),keep=F) 

#-------------------------------------------------------------------------------



##### LEAFLET MAP/SHINY APP PREREQS
## THE SHINY APP #####

map_theme <- theme(legend.title.align=0.5,
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
                   plot.background=element_blank())

bar_theme <- theme(legend.title.align=0.5,
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
                   legend.title=element_blank())


ui <- navbarPage(
  "2021 Census Language Data",
  theme = shinytheme("sandstone"),
  tabPanel("Main page",
    h2("Note: pages of more detailed maps (in particular Toronto's dissemination area map) may take a few minutes to load."),
    h2("Graphics on the following pages are best viewed when made using a wide range of colours. 
       View graphics in full rainbow colour or colourblind accessible palette Viridis? Warning: changing palettes immediately unloads all graphs."),
    radioButtons("colourradio",label="Colour",choices = list("Rainbow" = 1, "Viridis" = 2),selected = 1)
  ),
  tabPanel(
    "Canada (Provincial)",
    tags$style(type='text/css', ".tab-content, plot-container plotly {background-color: #f8f5f0;}"),
    tags$style(type='text/css', ".col-sm-8 {padding-top: 50px;
  padding-bottom: 50px;padding-right:30px;}"),
    sidebarLayout(
      sidebarPanel(
        width=4,
        fluidRow(
          p("Double click on map legend to isolate particular language. Shift click on a language to (de)select it. Double click on white space to return map to its regular state.", a(href = 'https://stackoverflow.com/', 'favorite link ever'),"a new paragraph. 
            more text Supply more text Supplymore text Supplymore text Supplymore text Supplymore text Supply"),
          h3("Top Unofficial Language Spoken at Home",style="text-align: center"),
          tags$style(type = "text/css", "#langbar_PROV {height: calc(80vh - 120px) !important;}"),
          plotlyOutput(outputId = "langbar_PROV", width = "100%",height = "100%")%>%withSpinner(color="#3e3f3a")
        )
      ),
      mainPanel(
        width = 8,
        align = "center",
        tags$style(type = "text/css", "#map_PROV {height: calc(100vh - 220px) !important;}"),
        plotlyOutput(outputId = "map_PROV", width = "100%",height = "100%")%>%withSpinner(color="#3e3f3a")
      )
      
    )
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
          tags$style(type = "text/css", "#langbar_CD {height: calc(80vh - 120px) !important;}"),
          plotlyOutput(outputId = "langbar_CD", width = "100%",height = "100%") %>%withSpinner(color="#3e3f3a")
        )
      ),
      mainPanel(
        width = 8,
        align = "center",
        tags$style(type = "text/css", "#map_CD {height: calc(100vh - 220px) !important;}"),
        plotlyOutput(outputId = "map_CD", width = "100%",height = "100%") %>% withSpinner(color="#3e3f3a")
      )
      
    )        
  ),
  navbarMenu(
    "Cities",
    tabPanel(
      "Toronto (Dissemination Areas)",
      tags$style(type='text/css', ".tab-content, plot-container plotly {background-color: #f8f5f0;}"),
      tags$style(type='text/css', ".col-sm-8 {padding-top: 50px;padding-bottom: 50px;padding-right:30px;}"),
      sidebarLayout(
        sidebarPanel(
          width=4,
          fluidRow(
            p("Here is the blah blah", a(href = 'https://stackoverflow.com/', 'favorite link ever'),"a new paragraph. 
            more text Supply more text Supplymore text Supplymore text Supplymore text Supplymore text Supply"),
            h3("Top Unofficial Language Spoken at Home",style="text-align: center"),
            tags$style(type = "text/css", "#langbar_Toronto {height: calc(80vh - 120px) !important;}"),
            plotlyOutput(outputId = "langbar_Toronto", width = "100%",height = "100%") %>%withSpinner(color="#3e3f3a")
          )
        ),
        mainPanel(
          width = 8,
          align = "center",
          tags$style(type = "text/css", "#map_Toronto {height: calc(100vh - 220px) !important;}"),
          plotlyOutput(outputId = "map_Toronto", width = "100%",height = "100%") %>% withSpinner(color="#3e3f3a")
        )
        
      )      
    ),
    tabPanel(
      "Waterloo (Dissemination Areas)",
      tags$style(type='text/css', ".tab-content, plot-container plotly {background-color: #f8f5f0;}"),
      tags$style(type='text/css', ".col-sm-8 {padding-top: 50px;padding-bottom: 50px;padding-right:30px;}"),
      sidebarLayout(
        sidebarPanel(
          width=4,
          fluidRow(
            p("Here is the blah blah", a(href = 'https://stackoverflow.com/', 'favorite link ever'),"a new paragraph. 
            more text Supply more text Supplymore text Supplymore text Supplymore text Supplymore text Supply"),
            h3("Top Unofficial Language Spoken at Home",style="text-align: center"),
            tags$style(type = "text/css", "#langbar_KWRegion {height: calc(80vh - 120px) !important;}"),
            plotlyOutput(outputId = "langbar_KWRegion", width = "100%",height = "100%") %>%withSpinner(color="#3e3f3a")
          )
        ),
        mainPanel(
          width = 8,
          align = "center",
          tags$style(type = "text/css", "#map_KWRegion {height: calc(100vh - 220px) !important;}"),
          plotlyOutput(outputId = "map_KWRegion", width = "100%",height = "100%") %>% withSpinner(color="#3e3f3a")
        )
        
      )       
    ),
    tabPanel(
      "London (Dissemination Areas)",
      tags$style(type='text/css', ".tab-content, plot-container plotly {background-color: #f8f5f0;}"),
      tags$style(type='text/css', ".col-sm-8 {padding-top: 50px;padding-bottom: 50px;padding-right:30px;}"),
      sidebarLayout(
        sidebarPanel(
          width=4,
          fluidRow(
            p("Here is the blah blah", a(href = 'https://stackoverflow.com/', 'favorite link ever'),"a new paragraph. 
            more text Supply more text Supplymore text Supplymore text Supplymore text Supplymore text Supply"),
            h3("Top Unofficial Language Spoken at Home",style="text-align: center"),
            tags$style(type = "text/css", "#langbar_London {height: calc(80vh - 120px) !important;}"),
            plotlyOutput(outputId = "langbar_London", width = "100%",height = "100%") %>%withSpinner(color="#3e3f3a")
          )
        ),
        mainPanel(
          width = 8,
          align = "center",
          tags$style(type = "text/css", "#map_London {height: calc(100vh - 220px) !important;}"),
          plotlyOutput(outputId = "map_London", width = "100%",height = "100%") %>% withSpinner(color="#3e3f3a")
        )
        
      )       
    )
  )
)

#get ggplot fill page correctly
server <- function(input,output,session){

  shared_lang_CD <- highlight_key(canada_CD)
  shared_lang_PROV <- highlight_key(canada_PROV)
  shared_lang_Toronto <- highlight_key(canada_Toronto)
  shared_lang_KWRegion <- highlight_key(canada_KWRegion)
  shared_lang_London <- highlight_key(canada_London)
  
  rainbowviridis <- function(n){
    if(input$colourradio==2){viridis(n)}
    else{rainbow(n)}
  } 
  
  output$map_PROV <- renderPlotly({
    ggplotly(ggplot(shared_lang_PROV) + #fill=CHARACTERISTIC_ID,
               geom_sf(data=canada_PROV, fill="white",color="black",lwd=0.03,aes())+
               geom_sf(aes(fill=Language,text=paste("Province/Territory:",PRNAME,"\nLanguage(s):",Language, "\n% of Prov/Terr Population:",C10_RATE_TOTAL)),lwd=0.03)+ #note note combined for tie
               scale_fill_manual(values=rainbowviridis(8))+
               coord_fixed(2)+
               map_theme,tooltip = c("text"))
  })
  
  output$langbar_PROV <- renderPlotly({
    ggplotly(
      ggplot(canada_PROV,aes(y=Language,fill=Language))+
        scale_fill_manual(values=rainbowviridis(8))+
        geom_bar(aes(text=Language))+
        geom_text(aes(label = Language), stat = "count", hjust="center", nudge_x =strwidth(sort(unique(canada_PROV$Language)),font=5,units="in")/1.5, colour = "black",size=4)+
        scale_y_discrete(limits = lang_order_PROV$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_PROV$Language))+5),expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)
    ) %>%
      style(hoverinfo = "none", traces = c(9:16))
  })
  
  
  output$map_CD <- renderPlotly({
    ggplotly(
      ggplot(shared_lang_CD) + #fill=CHARACTERISTIC_ID,
      geom_sf(data=canada_CD, fill="white",color="black",lwd=0.03,aes())+
      geom_sf(aes(fill=Language,
                  text=paste("Census Division (CD):",CDNAME,
                             "\nLanguage(s):",Languages, 
                             "\n% of CD Population:",C10_RATE_TOTAL)),
              lwd=0.03)+ #note note combined for tie
      scale_fill_manual(values=c(rainbowviridis(33)[1:17],"grey",rainbowviridis(33)[18:28],"black",rainbowviridis(33)[29:33]))+
      coord_fixed(2)+
      map_theme,tooltip = c("text"))
  })
  
  output$langbar_CD <- renderPlotly({
    ggplotly(
      ggplot(canada_CD,aes(y=Language,fill=Language))+
        scale_fill_manual(values=c(rainbowviridis(33)[1:17],"grey",rainbowviridis(33)[18:28],"black",rainbowviridis(33)[29:33]))+
        geom_bar(aes(text=Language))+
        geom_text(aes(label = Language), stat = "count", hjust="center", nudge_x =strwidth(sort(unique(canada_CD$Language)),font=3,units="in")/2*12, colour = "black",size=3)+
        scale_y_discrete(limits = lang_order_CD$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_CD$Language))+11),expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(36:70))
  })
  
  output$map_Toronto <- renderPlotly({
    ggplotly(
      ggplot(shared_lang_Toronto) + #fill=CHARACTERISTIC_ID,
        geom_sf(data=canada_Toronto, fill="white",color="black",lwd=0.03,aes())+
        geom_sf(aes(fill=Language,
                    text=paste("Dissemination Area (DA):",DAUID,
                               "\nLanguage(s):",Languages, 
                               "\n% of CD Population:",C10_RATE_TOTAL)),
                lwd=0.03)+ #note note combined for tie
        scale_fill_manual(values=c(rainbowviridis(54)[1:28],"white","grey",rainbowviridis(54)[31:45],"black",rainbowviridis(54)[47:54]))+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))
  })
  
  output$langbar_Toronto <- renderPlotly({
    ggplotly(
      ggplot(canada_Toronto,aes(y=Language,fill=Language))+
        scale_fill_manual(values=c(rainbowviridis(54)[1:28],"white","grey",rainbowviridis(54)[31:45],"black",rainbowviridis(54)[47:54]))+
        geom_bar(aes(text=Language))+
        geom_text(aes(label = Language), stat = "count", hjust="center", nudge_x =strwidth(sort(unique(canada_Toronto$Language)),font=3,units="in")*25, colour = "black",size=3)+
        scale_y_discrete(limits = lang_order_Toronto$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_Toronto$Language))+400),expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(55:107))
  })

  output$map_KWRegion <- renderPlotly({
    ggplotly(
      ggplot(shared_lang_KWRegion) + #fill=CHARACTERISTIC_ID,
        geom_sf(data=canada_KWRegion, fill="white",color="black",lwd=0.03,aes())+
        geom_sf(aes(fill=Language,
                    text=paste("Dissemination Area (DA):",DAUID,
                               "\nLanguage(s):",Languages,
                               "\n% of CD Population:",C10_RATE_TOTAL)),
                lwd=0.03)+ #note note combined for tie
        scale_fill_manual(values=c(rainbowviridis(39)[1:19],"white","grey",rainbowviridis(39)[22:33],"black",rainbowviridis(39)[35:39]))+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))
  })
  
  output$langbar_KWRegion <- renderPlotly({
    ggplotly(
      ggplot(canada_KWRegion,aes(y=Language,fill=Language))+
        scale_fill_manual(values=c(rainbowviridis(39)[1:19],"white","grey",rainbowviridis(39)[22:33],"black",rainbowviridis(39)[35:39]))+
        geom_bar(aes(text=Language))+
        geom_text(aes(label = Language), stat = "count", hjust="center", nudge_x =strwidth(sort(unique(canada_KWRegion$Language)),font=3,units="in")*30, colour = "black",size=3)+
        scale_y_discrete(limits = lang_order_KWRegion$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_KWRegion$Language))+180),expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(40:77))
  })
  output$map_London <- renderPlotly({
    ggplotly(
      ggplot(shared_lang_London) + #fill=CHARACTERISTIC_ID,
        geom_sf(data=canada_London, fill="white",color="black",lwd=0.03,aes())+
        geom_sf(aes(fill=Language,
                    text=paste("Dissemination Area (DA):",DAUID,
                               "\nLanguage(s):",Languages, 
                               "\n% of CD Population:",C10_RATE_TOTAL)),
                lwd=0.03)+ #note note combined for tie
        scale_fill_manual(values=c(rainbowviridis(30)[1:17],"grey",rainbowviridis(30)[19:25],"black",rainbowviridis(30)[27:30]))+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))
  })
  
  output$langbar_London <- renderPlotly({
    ggplotly(
      ggplot(canada_London,aes(y=Language,fill=Language))+
        scale_fill_manual(values=c(rainbowviridis(30)[1:17],"grey",rainbowviridis(30)[19:25],"black",rainbowviridis(30)[27:30]))+
        geom_bar(aes(text=Language))+
        geom_text(aes(label = Language), stat = "count", hjust="center", nudge_x =strwidth(sort(unique(canada_London$Language)),font=3,units="in")*20, colour = "black",size=3)+
        scale_y_discrete(limits = lang_order_London$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_London$Language))+150),expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(31:60))
  })
# outputOptions(output, "map_PROV", suspendWhenHidden = FALSE)
# outputOptions(output, "langbar_PROV", suspendWhenHidden = FALSE)
# outputOptions(output, "map_CD", suspendWhenHidden = FALSE)
# outputOptions(output, "langbar_CD", suspendWhenHidden = FALSE)
# outputOptions(output, "map_Toronto", suspendWhenHidden = FALSE)
# outputOptions(output, "langbar_Toronto", suspendWhenHidden = FALSE)
outputOptions(output, "map_KWRegion", suspendWhenHidden = FALSE)
outputOptions(output, "langbar_KWRegion", suspendWhenHidden = FALSE)
# outputOptions(output, "map_London", suspendWhenHidden = FALSE)
# outputOptions(output, "langbar_London", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)

#https://coolbutuseless.github.io/package/ggpattern/index.html
# p_json <- plotly_json(frend)
# 
# print(paste0(fromJSON(p_json$x$data)$data$type, ": ", 
#              fromJSON(p_json$x$data)$data$name))