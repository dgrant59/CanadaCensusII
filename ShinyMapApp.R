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
set.seed(20220905)
languages_used <- distinct(rbind(lang_order_PROV,
                                 lang_order_CD,
                                 lang_order_Toronto,
                                 lang_order_London,
                                 lang_order_KWRegion)
                           ,Language)
languages_used %<>% arrange(Language)

### Have to swap around some colours because they don't work well being so similar
#on maps
map_colours_rainbow<- desaturate(rainbow(nrow(languages_used)),0.2)
names(map_colours_rainbow) <- languages_used$Language
map_colours_rainbow[names(map_colours_rainbow)=="Tie (Hover to See Languages)"] <- desaturate("grey30",0.2)
map_colours_rainbow[names(map_colours_rainbow)=="NO DATA"] <- "white"
map_colours_rainbow[names(map_colours_rainbow)=="No non-English/French Speakers"] <- desaturate("grey80",0.2)

map_colours_rainbow[names(map_colours_rainbow)=="Tagalog (Pilipino, Filipino)"] <- "#CAB2D6"
map_colours_rainbow[names(map_colours_rainbow)=="Spanish"] <- desaturate("khaki2",0.2)
map_colours_rainbow[names(map_colours_rainbow)=="Polish"] <- "dodgerblue2"
map_colours_rainbow[names(map_colours_rainbow)=="Portuguese"] <- desaturate("khaki1",0.2)

map_colours_viridis <- viridis(nrow(languages_used))
names(map_colours_viridis) <- languages_used$Language
map_colours_viridis[names(map_colours_viridis)=="Tie (Hover to See Languages)"] <- desaturate("grey30",0.2)
map_colours_viridis[names(map_colours_viridis)=="NO DATA"] <- "white"
map_colours_viridis[names(map_colours_viridis)=="No non-English/French Speakers"] <- desaturate("grey80",0.2)

colourswap1 <- map_colours_viridis[names(map_colours_viridis)=="American Sign Language"]
colourswap2 <- map_colours_viridis[names(map_colours_viridis)=="Cree, n.o.s."]
colourswap3 <- map_colours_viridis[names(map_colours_viridis)=="Slavey, n.o.s."]


map_colours_viridis[names(map_colours_viridis)=="American Sign Language"] <- map_colours_viridis[names(map_colours_viridis)=="Tagalog (Pilipino, Filipino)"]
map_colours_viridis[names(map_colours_viridis)=="Cree, n.o.s."] <- map_colours_viridis[names(map_colours_viridis)=="Spanish"]
map_colours_viridis[names(map_colours_viridis)=="Slavey, n.o.s."] <- map_colours_viridis[names(map_colours_viridis)=="Polish"]

map_colours_viridis[names(map_colours_viridis)=="Tagalog (Pilipino, Filipino)"] <- colourswap1
map_colours_viridis[names(map_colours_viridis)=="Spanish"] <- colourswap2
map_colours_viridis[names(map_colours_viridis)=="Polish"] <- colourswap3

#-------------------------------------------------------------------------------
map_theme <- theme(legend.title.align = 0.5,
                   axis.line = element_blank(),  #bunch of options to remove "graph" visuals
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.background = element_blank())

bar_theme <- theme(legend.title.align = 0.5,
                   axis.line = element_blank(),  #bunch of options to remove "graph" visuals
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank(),
                   axis.ticks.length = unit(0, "pt"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.background = element_blank(),
                   plot.title = element_text(size=12),
                   legend.position = 'none',
                   plot.margin = unit(c(0,0,0,0), "mm"),
                   legend.title = element_blank())
ui <- navbarPage(
  "2021 Census Language Data",
  theme = shinytheme("sandstone"),
  tags$style(
    HTML(
      ".radio {margin-bottom: 42px;}"
      )
  ),
  tabPanel("Main page",
    h2("Welcome!"),
    HTML(
      "<h5><a href='https://www.github.com/dgrant59' target='_blank'>My Github</a></h5>"
    ),
    HTML(
      "<h4>It is well-known that modern Canada is a mosaic of cultures and traditions.
      One clear indication of this is the variety of languages you may hear 
      being spoken as you travel throughout the country.
      My goal with this app is to attempt to simply convey this diversity by 
      answering the question:</h4><h3 style='text-align:center'>
      Ignoring English and French, which language is most popular in a given 
      area in Canada?</h3>"
    ),
    HTML(
      "<br><h4>In an attempt to answer this, the following maps pertain to the 'Language spoken most often at 
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
      here</a>."
    ),
    h4("Because some maps are
      so granular, data may not be complete. Data quality flags shown on hover are defined in the 
      census profile metadata file (0 == everything is more or less 'fine')."
    ),
    h5(
      "Note: Pages of more detailed maps (in particular Toronto's dissemination area map) 
      may take a few minutes to load. These maps may also be somewhat laggy depending on the amount of 
      points drawn. Be aware point sizes are a fixed % of the viewing window size,
      i.e. zooming in will make it easier to see the regions that a point covers because the 
      region grows but the point does not.",br(),br(),br(),br()),
   
    h4("\"Why are colours in the legend sometimes 'out of order'?\""),
    h5("I tried my best to ensure that common languages were given their own unique colour so that it would be 
       easy to differentiate them without needing to select the language in the legend.",br(),br()),
    h4(
      "Graphics on the following pages are best viewed when made using a wide range of colours. 
      View graphics in full rainbow colour or colourblind-accessible palette Viridis?",br()
    ),
    h5(
      HTML("<b>Warning</b>"),": As you read this, some graphs on subsequent pages are being preloaded. 
      Clicking these radio buttons will immediately start this process over with the chosen palette."
    ),
    fluidRow(
      column(
        width=3,
        radioButtons(
          "colourradio",
          label="Colour",
          choices = list("Rainbow" = 1, "Viridis" = 2),
          selected = 1
        )
      ),
      column(
        width=3,
        img(
          src='rainbowviridis.png', 
          align = "left",
          width="600px"
        )
      )
    )
  ),
  tabPanel(
    "Canada (Provincial)",
    tags$style(
      type='text/css', 
      ".tab-content, plot-container plotly {background-color: #f8f5f0;}
       .col-sm-8 {padding-top: 50px;padding-bottom:50px;padding-right:30px;}"
    ),
    sidebarLayout(
      sidebarPanel(
        width=4,
        fluidRow(
          HTML(
            "<b>Double click</b> on a legend language to (de)isolate it from the rest. 
            <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
            on white space to return map to its regular zoom state. "
          ),
          h3(
            "Top Unofficial Language Spoken at Home",
            style="text-align: center"
          ),
          tags$style(
            type = "text/css", 
            "#langbar_PROV {height: calc(80vh - 120px) !important;}"
          ),
          plotlyOutput(
            outputId = "langbar_PROV", 
            width = "100%",
            height = "100%"
          ) %>% withSpinner(color="#3e3f3a")
        )
      ),
      mainPanel(
        width = 8,
        align = "center",
        tags$style(
          type = "text/css", 
          "#map_PROV {height: calc(100vh - 220px) !important;}"
        ),
        plotlyOutput(
          outputId = "map_PROV", 
          width = "100%",
          height = "100%"
        ) %>% withSpinner(color="#3e3f3a")
      )
    )
  ),
  tabPanel(
    "Canada (Census Divisions)",
    tags$style(
      type='text/css', 
      ".tab-content, plot-container plotly {background-color: #f8f5f0;}
       .col-sm-8 {padding-top:50px;padding-bottom:50px;padding-right:30px;}"
    ),
    sidebarLayout(
      sidebarPanel(
        width=4,
        fluidRow(
          HTML(
            "<b>Double click</b> on a legend language to (de)isolate it from the rest. 
            <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
            on white space to return map to its regular zoom state. "
          ),
          h3(
            "Top Unofficial Language Spoken at Home",
            style="text-align: center"
          ),
          tags$style(
            type = "text/css", 
            "#langbar_CD {height: calc(80vh - 120px) !important;}"
          ),
          plotlyOutput(
            outputId = "langbar_CD", 
            width = "100%",
            height = "100%"
          ) %>% withSpinner(color="#3e3f3a")
        )
      ),
      mainPanel(
        width = 8,
        align = "center",
        tags$style(
          type = "text/css", 
          "#map_CD {height: calc(100vh - 220px) !important;}"
        ),
        plotlyOutput(
          outputId = "map_CD", 
          width = "100%",
          height = "100%"
        ) %>% withSpinner(color="#3e3f3a")
      )
    )        
  ),
  navbarMenu(
    "Cities",
    tabPanel(
      "Toronto (Dissemination Areas)",
      tags$style(
        type='text/css', 
        ".tab-content, plot-container plotly {background-color: #f8f5f0;}
         .col-sm-8 {padding-top: 50px;padding-bottom: 50px;padding-right:30px;}"
      ),
      sidebarLayout(
        sidebarPanel(
          width=4,
          fluidRow(
            HTML(
              "<b>Double click</b> on a legend language to (de)isolate it from the rest. 
              <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
              on white space to return map to its regular zoom state. "
            ),
            h3(
              "Top Unofficial Language Spoken at Home",
              style="text-align: center"
            ),
            tags$style(
              type = "text/css", 
              "#langbar_Toronto {height: calc(80vh - 120px) !important;}"
            ),
            plotlyOutput(
              outputId = "langbar_Toronto", 
              width = "100%",
              height = "100%"
            ) %>% withSpinner(color="#3e3f3a")
          )
        ),
        mainPanel(
          width = 8,
          align = "center",
          tags$style(
            type = "text/css", 
            "#map_Toronto {height: calc(100vh - 220px) !important;}"
          ),
          plotlyOutput(
            outputId = "map_Toronto", 
            width = "100%",
            height = "100%"
          ) %>% withSpinner(color="#3e3f3a")
        )
      )      
    ),
    tabPanel(
      "Waterloo (Dissemination Areas)",
      tags$style(
        type='text/css', 
        ".tab-content, plot-container plotly {background-color: #f8f5f0;}
         .col-sm-8 {padding-top: 50px;padding-bottom: 50px;padding-right:30px;}"
      ),
      sidebarLayout(
        sidebarPanel(
          width=4,
          fluidRow(
            HTML(
              "<b>Double click</b> on a legend language to (de)isolate it from the rest. 
              <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
              on white space to return map to its regular zoom state. "
            ),
            h3(
              "Top Unofficial Language Spoken at Home",
              style="text-align: center"
            ),
            tags$style(
              type = "text/css", 
              "#langbar_KWRegion {height: calc(80vh - 120px) !important;}"
            ),
            plotlyOutput(
              outputId = "langbar_KWRegion", 
              width = "100%",
              height = "100%"
            ) %>% withSpinner(color="#3e3f3a")
          )
        ),
        mainPanel(
          width = 8,
          align = "center",
          tags$style(
            type = "text/css", 
            "#map_KWRegion {height: calc(100vh - 220px) !important;}"
          ),
          plotlyOutput(
            outputId = "map_KWRegion", 
            width = "100%",
            height = "100%"
          ) %>% withSpinner(color="#3e3f3a")
        )
      )       
    ),
    tabPanel(
      "London (Dissemination Areas)",
      tags$style(
        type='text/css', 
        ".tab-content, plot-container plotly {background-color: #f8f5f0;}
         .col-sm-8 {padding-top: 50px;padding-bottom: 50px;padding-right:30px;}"
      ),
      sidebarLayout(
        sidebarPanel(
          width=4,
          fluidRow(
            HTML(
              "<b>Double click</b> on a legend language to (de)isolate it from the rest. 
              <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
              on white space to return map to its regular zoom state. "),
            h3(
              "Top Unofficial Language Spoken at Home",
              style="text-align: center"
            ),
            tags$style(
              type = "text/css", 
              "#langbar_London {height: calc(80vh - 120px) !important;}"
            ),
            plotlyOutput(
              outputId = "langbar_London", 
              width = "100%",
              height = "100%"
            ) %>% withSpinner(color="#3e3f3a")
          )
        ),
        mainPanel(
          width = 8,
          align = "center",
          tags$style(
            type = "text/css", 
            "#map_London {height: calc(100vh - 220px) !important;}"
          ),
          plotlyOutput(
            outputId = "map_London", 
            width = "100%",
            height = "100%"
          ) %>% withSpinner(color="#3e3f3a")
        )
      )       
    )
  )
)

server <- function(input,output,session){
  
  #Tells graphs what palette to use based on radio button selection
  rainbowviridis <- function(){
    if(input$colourradio==2){
      map_colours_viridis
    }
    else{
      map_colours_rainbow
    }
  } 
  
  # output$map_PLACE <- renderPlotly({
  #   ggplotly(
  #     ggplot(canada_PLACE) + 
  #
  #This is the backgroun map that appears behind a region when you deselect it
  #White fill, black border, borderlines 0.03
  #       geom_sf(data=canada_PLACE, fill="white",color="black",lwd=0.03)+
  #
  #This is the actual map with information on it. Fill is the discrete language
  #variable, and borderlines are 0.1 
  #
  #       geom_sf(aes(fill=Language),
  #               lwd=0.1)+ 
  #
  #Customizing the fill of the above map layer, we use the rainbowviridis() function
  #which will output the correct palette based on the radio buttons. We select only
  #unique(canada_PLACE$Language) because map_colours_X is a dictionary that contains
  #colours for every region that is mapped across all 5 maps. If we do not filter to
  #unique(canada_PLACE$Language), the legend would contain a bunch of unused (on this map)
  #languages
  #
  #       scale_fill_manual(values=rainbowviridis()[unique(canada_PLACE$Language)])+
  #
  #This adds a geom point somewhere guaranteed to be within each polygon. It too
  #will be filled with the colour assigned to Language, and it will also be
  #scaled based on the % of the region that language's speakers make up.
  #The "text" attribute defines what appears when you hover over each point in the
  #info box (\n makes a new line). Stroke defines the border around each point
  #do not show legend because this would add onto the existing legend and give each
  #language a line and a point entry in the legend.
  #
  #       stat_sf_coordinates(
  #         aes(fill=Language,
  #             size=C10_RATE_TOTAL,
  #             text=paste("Dissemination Area (DA):",DAUID,
  #                        "\nLanguage(s):",Languages,
  #                        "\n% of CD Population:",C10_RATE_TOTAL,
  #                        "\nData Quality Flag:", DATA_QUALITY_FLAG)),
  #         stroke=0.2,
  #         show.legend = F)+
  #
  #
  #deals with the aspect ratio of the plot, twice as tall as wide
  #       coord_fixed(2)+
  #
  #
  #add map theme as defined above, make sure the "text" attribute shows up on hover
  #and do not allow clicking on the map to do anything.
  
  #       map_theme,tooltip = c("text"))%>% layout(clickmode="none")
  # })
  # 
  #This is for the language bar
  #
  # output$langbar_Toronto <- renderPlotly({
  #   ggplotly(
  #
  #Since this plot is rotated, y will be the languages and x will be the count of 
  #each language
  #
  #     ggplot(canada_PLACE,aes(y=Language,fill=Language))+
  #       scale_fill_manual(values=rainbowviridis()[unique(canada_PLACE$Language)])+
  #       geom_bar(aes(text=Language))+
  #       geom_text(
  #         aes(label = Language), 
  #         stat = "count", 
  #         hjust="center", 
  #
  #nudge_x is a way to move the labels to the right of the bar so they do not intersect it
  #and look ugly. Since each language has a different number of characters, each
  #text label will need to move a different amount. These numbers are all basically 
  #trial and error.
  #
  #         nudge_x =strwidth(sort(unique(canada_PLACE$Language)),font=3,units="in")*85, 
  #         colour = "black",
  #         size=3)+
  #       scale_y_discrete(limits = lang_order_Toronto$Language)+
  #       labs(x = NULL, y = NULL)+
  #
  #add extra x axis room for label to appear beyond bar
  #
  #       scale_x_continuous(limits=c(0,max(table(canada_PLACE$Language))+500),
  #                          expand = c(0,0)) +
  #
  #show count and language on hover
  #
  #       bar_theme,tooltip = c("text","x")) %>% 
  #
  #
  #
  #     layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
  #
  # Text labels are also classified as a "trace" and would have an on hover effect
  #this is obviously unwanted, so I have disabled the hover effects above each
  #language label
  #
  #     style(hoverinfo = "none", traces = c(55:107))
  # })
  
  output$map_PROV <- renderPlotly({
    ggplotly(
      ggplot(canada_PROV) +
        geom_sf(data=canada_PROV, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language),
                lwd=0.1
        ) + 
        scale_fill_manual(values=rainbowviridis()[unique(canada_PROV$Language)])+
        stat_sf_coordinates(aes(
          fill=Language,
          size=C10_RATE_TOTAL,
          text=paste("Province/Territory:",PRNAME,
                     "\nLanguage(s):",Language, 
                     "\n% of Prov/Terr Population:",C10_RATE_TOTAL,
                     "\nData Quality Flag:", DATA_QUALITY_FLAG)),
          stroke=0.2,
          show.legend = F)+
        coord_fixed(2)+
        map_theme,tooltip = c("text")
    ) %>% layout(clickmode="none")
  })
  
  output$langbar_PROV <- renderPlotly({
    ggplotly(
      ggplot(canada_PROV,aes(y=Language, fill=Language))+
        scale_fill_manual(values=rainbowviridis()[unique(canada_PROV$Language)])+
        geom_bar(aes(text=Language))+
        geom_text(
          aes(label = Language), 
          stat = "count", 
          hjust="center", 
          nudge_x =strwidth(sort(unique(canada_PROV$Language)),font=5,units="in")/1.5, 
          colour = "black",
          size=4)+
        scale_y_discrete(limits = lang_order_PROV$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_PROV$Language))+5),expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")
    ) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(9:16))
  })
  
  
  output$map_CD <- renderPlotly({
    ggplotly(
      ggplot(canada_CD) +
        geom_sf(data=canada_CD, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language),
                lwd=0.1
        )+ 
        scale_fill_manual(values=rainbowviridis()[unique(canada_CD$Language)])+
        stat_sf_coordinates(
          aes(fill=Language,
              size=C10_RATE_TOTAL,
              text=paste("Census Division (CD):",CDNAME,
                         "\nLanguage(s):",Languages, 
                         "\n% of CD Population:",C10_RATE_TOTAL,
                         "\nData Quality Flag:", DATA_QUALITY_FLAG)),
          stroke=0.2,
          show.legend = F)+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")
  })
  
  output$langbar_CD <- renderPlotly({
    ggplotly(
      ggplot(canada_CD,aes(y=Language,fill=Language))+
        scale_fill_manual(values=rainbowviridis()[unique(canada_CD$Language)])+
        geom_bar(aes(text=Language))+
        geom_text(aes(label = Language), 
                  stat = "count",
                  hjust="center", 
                  nudge_x =strwidth(sort(unique(canada_CD$Language)),font=3,units="in")*12, 
                  colour = "black",
                  size=3)+
        scale_y_discrete(limits = lang_order_CD$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_CD$Language))+20),
                           expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")
      ) %>% layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(36:70))
  })
  
  output$map_Toronto <- renderPlotly({
    ggplotly(
      ggplot(canada_Toronto) +
        geom_sf(data=canada_Toronto, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language),
                lwd=0.1)+ 
        scale_fill_manual(values=rainbowviridis()[unique(canada_Toronto$Language)])+
        stat_sf_coordinates(
          aes(fill=Language,
              size=C10_RATE_TOTAL,
              text=paste("Dissemination Area (DA):",DAUID,
                         "\nLanguage(s):",Languages,
                         "\n% of CD Population:",C10_RATE_TOTAL,
                         "\nData Quality Flag:", DATA_QUALITY_FLAG)),
          stroke=0.2,
          show.legend = F)+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")
  })
  
  output$langbar_Toronto <- renderPlotly({
    ggplotly(
      ggplot(canada_Toronto,aes(y=Language,fill=Language))+
        scale_fill_manual(values=rainbowviridis()[unique(canada_Toronto$Language)])+
        geom_bar(aes(text=Language))+
        geom_text(
          aes(label = Language), 
          stat = "count", 
          hjust="center", 
          nudge_x =strwidth(sort(unique(canada_Toronto$Language)),font=3,units="in")*85, 
          colour = "black",
          size=3)+
        scale_y_discrete(limits = lang_order_Toronto$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_Toronto$Language))+500),
                           expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% 
      layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(55:107))
  })
  
  output$map_KWRegion <- renderPlotly({
    ggplotly(
      ggplot(canada_KWRegion)+
        geom_sf(data=canada_KWRegion, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language),
                lwd=0.1)+ 
        scale_fill_manual(values=rainbowviridis()[unique(canada_KWRegion$Language)])+
        stat_sf_coordinates(
          aes(fill=Language,
              size=C10_RATE_TOTAL,
              text=paste("Dissemination Area (DA):",DAUID,
                         "\nLanguage(s):",Languages,
                         "\n% of CD Population:",C10_RATE_TOTAL,
                         "\nData Quality Flag:", DATA_QUALITY_FLAG)),
          stroke=0.2,
          show.legend = F)+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")
  })
  
  output$langbar_KWRegion <- renderPlotly({
    ggplotly(
      ggplot(canada_KWRegion,aes(y=Language,fill=Language))+
        scale_fill_manual(values=rainbowviridis()[unique(canada_KWRegion$Language)])+
        geom_bar(aes(text=Language))+
        geom_text(
          aes(label = Language), 
          stat = "count", 
          hjust="center", 
          nudge_x =strwidth(sort(unique(canada_KWRegion$Language)),font=3,units="in")*30, 
          colour = "black",
          size=3)+
        scale_y_discrete(limits = lang_order_KWRegion$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_KWRegion$Language))+180),
                           expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% 
      layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>%
      style(hoverinfo = "none", traces = c(40:77))
  })
  output$map_London <- renderPlotly({
    ggplotly(
      ggplot(canada_London) +
        geom_sf(data=canada_London, fill="white",color="black",lwd=0.03)+
        geom_sf(aes(fill=Language),
                lwd=0.1)+ 
        scale_fill_manual(values=rainbowviridis()[unique(canada_London$Language)])+
        stat_sf_coordinates(
          aes(fill=Language,
              size=C10_RATE_TOTAL,
              text=paste("Dissemination Area (DA):",DAUID,
                         "\nLanguage(s):",Languages,
                         "\n% of CD Population:",C10_RATE_TOTAL,
                         "\nData Quality Flag:", DATA_QUALITY_FLAG)),
          stroke=0.2,
          show.legend = F)+
        coord_fixed(2)+
        map_theme,tooltip = c("text"))%>% layout(clickmode="none")
  })
  
  output$langbar_London <- renderPlotly({
    ggplotly(
      ggplot(canada_London,aes(y=Language,fill=Language))+
        scale_fill_manual(values=rainbowviridis()[unique(canada_London$Language)])+
        geom_bar(aes(text=Language))+
        geom_text(
          aes(label = Language), 
          stat = "count", 
          hjust="center", 
          nudge_x = strwidth(sort(unique(canada_London$Language)),font=3,units="in")*20, 
          colour = "black",
          size=3)+
        scale_y_discrete(limits = lang_order_London$Language)+
        labs(x = NULL, y = NULL)+
        scale_x_continuous(limits=c(0,max(table(canada_London$Language))+150),
                           expand = c(0,0)) +
        bar_theme,tooltip = c("text","x")) %>% 
      layout(margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
      style(hoverinfo = "none", traces = c(31:60))
  })
  #preload whatever you want
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