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

popdata_PROV <- read.csv("popdata_PROV.csv",encoding='UTF-8')

lang_order_PROV <- popdata_PROV %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)


popdata_CD <- read.csv("popdata_CD.csv",encoding='UTF-8')

lang_order_CD <- popdata_CD %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)

popdata_Toronto <- read.csv("popdata_Toronto.csv",encoding='UTF-8')

lang_order_Toronto <- popdata_Toronto %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)

popdata_KWRegion <- read.csv("popdata_KWRegion.csv",encoding='UTF-8')

lang_order_KWRegion <- popdata_KWRegion %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  select(Language)

popdata_London <- read.csv("popdata_London.csv",encoding='UTF-8')

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
canada_CD <- read_sf(dsn = "./ShapeFiles/CD Shapefiles/CensusDivisionSimplified2.shp", 
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
  tags$style(HTML(".radio {margin-bottom: 42px;}")),
  tabPanel("Main page",
           h2("Welcome!"),
           HTML("<h5><a href='https://www.github.com/dgrant59' target='_blank'>My Github</a></h5>"),
           HTML("<h4>It is well-known that modern Canada is a mosaic of cultures and traditions.
         One clear indication of this is the variety of languages you may hear 
         being spoken as you travel throughout the country.
         My goal with this app is to attempt to simply convey this diversity by 
         answering the question:</h4><h3 style='text-align:center'>
         Ignoring English and French, which language is most popular in a given 
         area in Canada?</h3>"),
           
           HTML("<br><h4>As such, the following maps pertain to the 'Language spoken most often at 
         home for the total population excluding institutional residents' 
         <a href='https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=pop186'> 
         defined here</a> from the 
         <a href='https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E'>
         2021 Census Profile</a> which I believe most closely answers this question. 
         If you prefer, you could replace 'most spoken at home' with the 'mother tongue'
         language section of the census profile on languagues. This data selection is done in
         <code>PrepareCensusData.R</code> found on my github.<br> 
         Shapefiles for these maps can be found
         <a href='https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21'>
         here</a>."),
           
           
           h4("Note: pages of more detailed maps (in particular Toronto's dissemination area map) 
    may take a few minutes to load. Because some maps are
       so granular, data may not be complete. Adding DATA_QUALITY_FLAG to the 
       hover text will let you see this for each area. Flags are defined in the 
       census profile metadata file,",br(),br(),br()),
           h4("Graphics on the following pages are best viewed when made using a wide range of colours. 
       View graphics in full rainbow colour or colourblind accessible palette Viridis?",br()),
           h5(HTML("<b>Warning</b>"),": As you read this, some graphs on subsequent pages are being preloaded. 
       Clicking these radio buttons will immediately start this process over with the chosen palette."),
           fluidRow(column(width=3,radioButtons("colourradio",label="Colour",choices = list("Rainbow" = 1, "Viridis" = 2),selected = 1)),
                    column(width=3,img(src='rainbowviridis.png', align = "left",width="600px")))
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
          HTML("<b>Double click</b> on a legend language to (de)isolate it from the rest. 
          <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
          on white space to return map to its regular zoom state. "),
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
          HTML("<b>Double click</b> on a legend language to (de)isolate it from the rest. 
          <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
          on white space to return map to its regular zoom state. "),
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
            HTML("<b>Double click</b> on a legend language to (de)isolate it from the rest. 
          <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
          on white space to return map to its regular zoom state. "),
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
            HTML("<b>Double click</b> on a legend language to (de)isolate it from the rest. 
          <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
          on white space to return map to its regular zoom state. "),
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
            HTML("<b>Double click</b> on a legend language to (de)isolate it from the rest. 
          <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
          on white space to return map to its regular zoom state. "),
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

server <- function(input,output,session){
  
  rainbowviridis <- function(n){
    if(input$colourradio==2){viridis(n)}
    else{desaturate(rainbow(n),0.2)}
  } 
  
  output$map_PROV <- renderPlotly({
    ggplotly(ggplot(canada_PROV) + #fill=CHARACTERISTIC_ID,
               geom_sf(data=canada_PROV, fill="white",color="black",lwd=0.03)+
               geom_sf(aes(fill=Language,text=paste("Province/Territory:",PRNAME,"\nLanguage(s):",Language, "\n% of Prov/Terr Population:",C10_RATE_TOTAL)),lwd=0.03)+ #note note combined for tie
               scale_fill_manual(values=rainbowviridis(8))+
               coord_fixed(2)+
               map_theme,tooltip = c("text")) %>% layout(clickmode="none")
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
        bar_theme,tooltip = c("text","x")) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>%
      style(hoverinfo = "none", traces = c(9:16))
  })
  
  
  output$map_CD <- renderPlotly({
    ggplotly(
      ggplot(canada_CD) + #fill=CHARACTERISTIC_ID,
        geom_sf(data=canada_CD, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language,
                    text=paste("Census Division (CD):",CDNAME,
                               "\nLanguage(s):",Languages, 
                               "\n% of CD Population:",C10_RATE_TOTAL)),
                lwd=0.03)+ #note note combined for tie
        scale_fill_manual(values=c(rainbowviridis(33)[1:17],desaturate("grey",0.2),rainbowviridis(33)[18:28],desaturate("black",0.2),rainbowviridis(33)[29:33]))+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")
  })
  
  output$langbar_CD <- renderPlotly({
    ggplotly(
      ggplot(canada_CD,aes(y=Language,fill=Language))+
        scale_fill_manual(values=c(rainbowviridis(33)[1:17],desaturate("grey",0.2),rainbowviridis(33)[18:28],desaturate("black",0.2),rainbowviridis(33)[29:33]))+
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
      ggplot(canada_Toronto) + #fill=CHARACTERISTIC_ID,
        geom_sf(data=canada_Toronto, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language,
                    text=paste("Dissemination Area (DA):",DAUID,
                               "\nLanguage(s):",Languages, 
                               "\n% of CD Population:",C10_RATE_TOTAL)),
                lwd=0.03)+ #note note combined for tie
        scale_fill_manual(values=c(rainbowviridis(54)[1:28],desaturate("grey20",0.2),desaturate("grey",0.2),rainbowviridis(54)[31:45],desaturate("black",0.2),rainbowviridis(54)[47:54]))+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")
  })
  
  output$langbar_Toronto <- renderPlotly({
    ggplotly(
      ggplot(canada_Toronto,aes(y=Language,fill=Language))+
        scale_fill_manual(values=c(rainbowviridis(54)[1:28],desaturate("grey20",0.2),desaturate("grey",0.2),rainbowviridis(54)[31:45],desaturate("black",0.2),rainbowviridis(54)[47:54]))+
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
      ggplot(canada_KWRegion) + #fill=CHARACTERISTIC_ID,
        geom_sf(data=canada_KWRegion, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language,
                    text=paste("Dissemination Area (DA):",DAUID,
                               "\nLanguage(s):",Languages,
                               "\n% of CD Population:",C10_RATE_TOTAL)),
                lwd=0.03)+ #note note combined for tie
        scale_fill_manual(values=c(rainbowviridis(39)[1:19],desaturate("grey20",0.2),desaturate("grey",0.2),rainbowviridis(39)[22:33],desaturate("black",0.2),rainbowviridis(39)[35:39]))+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")
  })
  
  output$langbar_KWRegion <- renderPlotly({
    ggplotly(
      ggplot(canada_KWRegion,aes(y=Language,fill=Language))+
        scale_fill_manual(values=c(rainbowviridis(39)[1:19],desaturate("grey20",0.2),desaturate("grey",0.2),rainbowviridis(39)[22:33],desaturate("black",0.2),rainbowviridis(39)[35:39]))+
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
      ggplot(canada_London) + #fill=CHARACTERISTIC_ID,
        geom_sf(data=canada_London, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language,
                    text=paste("Dissemination Area (DA):",DAUID,
                               "\nLanguage(s):",Languages, 
                               "\n% of CD Population:",C10_RATE_TOTAL)),
                lwd=0.03)+ #note note combined for tie
        scale_fill_manual(values=c(rainbowviridis(30)[1:17],desaturate("grey",0.2),rainbowviridis(30)[19:25],desaturate("black",0.2),rainbowviridis(30)[27:30]))+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")
  })
  
  output$langbar_London <- renderPlotly({
    ggplotly(
      ggplot(canada_London,aes(y=Language,fill=Language))+
        scale_fill_manual(values=c(rainbowviridis(30)[1:17],desaturate("grey",0.2),rainbowviridis(30)[19:25],desaturate("black",0.2),rainbowviridis(30)[27:30]))+
        geom_bar(aes(text=Language))+
        geom_text(aes(label = Language), stat = "count", hjust="center", nudge_x =strwidth(sort(unique(canada_London$Language)),font=3,units="in")*20, colour = "black",size=3)+
        scale_y_discrete(limits = lang_order_London$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_London$Language))+150),expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(31:60))
  })
  outputOptions(output, "map_PROV", suspendWhenHidden = FALSE)
  outputOptions(output, "langbar_PROV", suspendWhenHidden = FALSE)
  outputOptions(output, "map_CD", suspendWhenHidden = FALSE)
  outputOptions(output, "langbar_CD", suspendWhenHidden = FALSE)
  # outputOptions(output, "map_Toronto", suspendWhenHidden = FALSE)
  # outputOptions(output, "langbar_Toronto", suspendWhenHidden = FALSE)
  # outputOptions(output, "map_KWRegion", suspendWhenHidden = FALSE)
  # outputOptions(output, "langbar_KWRegion", suspendWhenHidden = FALSE)
  # outputOptions(output, "map_London", suspendWhenHidden = FALSE)
  # outputOptions(output, "langbar_London", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)

#In order to see the "traces" or layers on a plotly graph, you need to save the map as an object 
# and run the following code. This is not really necessary, but made it so that
# there was no hover text on the bar plot language labels
#library(listviewer)
#library(jsonlite)
# p_json <- plotly_json(YOUR_MAP)
# 
# print(paste0(fromJSON(p_json$x$data)$data$type, ": ", 
#              fromJSON(p_json$x$data)$data$name))