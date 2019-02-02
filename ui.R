library(shiny)
library(leaflet)
library(plotly)

shinyUI(
  fluidPage(id = "main", theme="styles.css", 
    fluidRow(
      column(width = 12, leafletOutput("map"),
        absolutePanel(id = "abpanel",
          sliderInput('yearslide','Please select the year', width = '80%', min = 1960, max = 2015, value = 2015)
        )
      )
    ),
  wellPanel(id = "control",
    fluidRow(
      column(width = 12,
      align = "center",
      column(width = 3, 
             radioButtons("radio", label = h3("Map display options"),
                                     choices = list("Show crude births" = 1, "Show crude deaths" = 2, "Show crude growth" = 3), 
                                     selected = 1)),
      column(width = 8, offset = 1, plotlyOutput("plot", height="400px"))
    )
  )
)
))