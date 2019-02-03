library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)


shinyUI(  
  fluidPage(id = "main", theme="styles.css", 
    fluidRow(
      column(width = 12, leafletOutput("map"),
        absolutePanel(id = "abpanel",
          sliderInput('yearslide','Please select the year to visualise the colors on the map', width = '80%', min = 1960, max = 2015, value = 2015)
        )
      )
    ),
  wellPanel(id = "control", useShinyjs(),
    fluidRow(
      column(width = 12,
      column(width = 3, 
             radioButtons("radio", label = h3("Used variable"),
                                     choices = list("Show crude births" = 1, "Show crude deaths" = 2, "Show crude growth" = 3), 
                                     selected = 1),
      
             radioButtons("plt", label = h3("Plot display options"),
                                     choices = list("Display line per country over years" = 1, "Display boxplot per country" = 2), 
                                     selected = 1)),
      tags$div(width = 8, offset = 1, "Click on a country above to show a graph", align="center", id = 'placeholder'),
      column(id="pltt", width = 8, offset = 1, plotlyOutput("plot", height="400px"))
    )
  )
)
))