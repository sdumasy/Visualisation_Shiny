library(shiny)
library(rgdal)
library(leaflet)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(rlang)
library(shinyjs)


# Makes a dataframe containing all the shapes of the world (France and Norway are missing from the countries shapes file)
create_shape_world <- function() {
    world <- readOGR(dsn = "Data", layer = "ne_50m_admin_0_countries", encoding = "UTF-8")
    franor <- readOGR(dsn = "Data", layer = "ne_10m_admin_0_map_units", encoding = "UTF-8")
    fr <- subset(franor, ISO_A3 == "FRA" | ISO_A3 == "NOR")
    rbind(world, fr)
}
# Create the leaflet with zoom options
generate_leaflet <- function(data) {
    leaflet(data = data, options = leafletOptions(zoomControl = TRUE, minZoom = 3, maxZoom = 5))
}

# Draw the polygons and legend on the map
draw_world <- function(leaflet, dataset, year, pal, title, draw_legend = TRUE, transparent = TRUE) {
        layerIds <- as.vector(dataset$ISO_A3)
        
        addPolygons(leaflet, 
                    data = dataset,
                    fillColor = ~pal(dataset[[toString(year)]]),
                    highlightOptions = highlightOptions(color = "yellow", weight = 3, bringToFront = TRUE),
                    layerId = dataset$ISO_A3,
                    fillOpacity = if(transparent) 0.8 else 1,
                    color = "black",
                    weight = if (transparent) 1 else 2) 
        if(draw_legend) {
          addLegend(leaflet, "topright", pal = pal, values = ~dataset[[toString(year)]],
                    title = paste(paste(title, year), "<br>(per 1000 people)"),
                    opacity = 1)
        }
}

# Render the leaflet
create_world <- function(leaflet, dataset, year, pal, title) {
    renderLeaflet({
       leaflet %>% draw_world(dataset, year, pal, title)
    })
}

# Create a color palette
create_color_pallette <- function(data, pall) {
    pal <- colorNumeric(
      palette = pall,
      domain = data[,c(5:ncol(data))]
    )
}

# change display mode -> draw world -> clear selected characters
change_mode_redraw_world <- function(map, output, mode, data, raw_data, colors, titles) {
    display_mode <<- mode
    
    clearControls(map)
    draw_world(map, data, current_year, colors, titles)
    draw_plots(data, raw_data, output)
}

# change year -> draw world
change_year_redraw_world <- function(map, year, data, colors, gray_colors, titles) {
    current_year <<- year
    clearControls(map)
    draw_world(map, data, year, colors, titles)
    
    clicked_polys <- subset(data, ISO_A3 %in% selected_countries)
    
    draw_world(map ,clicked_polys, year, gray_colors, titles, draw_legend = FALSE)
}

# Creates and preprocesses the data
create_data <- function() {
    birth_data <- read.csv(skip = 3, "Data/birth.csv")
    death_data <- read.csv(skip = 3, "Data/death.csv")
    birth_data <- subset(birth_data, select = -c(X, X2017, X2016))
    death_data <- subset(death_data, select = -c(X, X2017, X2016))
    
    names(birth_data) <- gsub(x = names(birth_data), pattern = "X", replacement = "")  
    names(death_data) <- gsub(x = names(death_data), pattern = "X", replacement = "")  
    
    growth_data = cbind(birth_data[,c(0:2)], birth_data[,c(5:ncol(birth_data))] - death_data[,c(5:ncol(death_data))])
    return(list("birth"= birth_data, "death"=death_data, "growth"=growth_data))
}

# Merges the data with the shape file of the world
merge_data <- function(raw_data) {
    
    world_shapes <- create_shape_world()
    
    full_world_birth <- merge(world_shapes, raw_data$birth, by.x = "ISO_A3", by.y = "Country.Code", sort = FALSE, all.x = TRUE)
    full_world_death <- merge(world_shapes, raw_data$death, by.x = "ISO_A3", by.y = "Country.Code", sort = FALSE, all.x = TRUE)
    full_world_growth <- merge(world_shapes, raw_data$growth, by.x = "ISO_A3", by.y = "Country.Code", sort = FALSE, all.x = TRUE)
    return(list(full_world_birth, full_world_death, full_world_growth))
}

change_plot <- function(plt_mode, data, raw_data, output) {
  plot_mode <<- plt_mode
  draw_plots(data, raw_data, output)
}


draw_plots <- function(data, raw_data, output) {
  if(length(selected_countries) > 0) {
    shinyjs::hide(id = "placeholder")
    shinyjs::show(id = "pltt")
  clicked_polys <- subset(data, ISO_A3 %in% selected_countries)

        leafletProxy("map", data = data) %>% draw_world(clicked_polys, current_year, create_color_pallette(raw_data, "Greys"), titles[[display_mode]], draw_legend = FALSE)
        
        output$plot = renderPlotly({

          filter_countries <- raw_data %>% filter(Country.Code %in% selected_countries) %>% select(num_range("", 1960:2015))
          
          filter_countries_long <- data.frame()
            
            for(i in 1:length(selected_countries)) {
            print(length(selected_countries))

            name <- subset(raw_data, Country.Code == selected_countries[i], select= Country.Name)
            filter_countries_long2 <- gather(filter_countries[i,], year, var, '1960':'2015')
            names(filter_countries_long2)[names(filter_countries_long2) == "var"] <- as.character(name[1,1])
            print(filter_countries_long2)
            
            if(is.data.frame(filter_countries_long) && nrow(filter_countries_long)==0) {
                filter_countries_long <- filter_countries_long2 
            } else {
                filter_countries_long2 <- select(filter_countries_long2, -year)
                filter_countries_long <- cbind(filter_countries_long, filter_countries_long2)
            }
            
            }
            
          

         year <- rep(filter_countries_long$year, length(selected_countries))
         values <- c()
         colour <- c()
         for(i in 2:ncol(filter_countries_long)) {
           values <- c(values, unlist(filter_countries_long[i]))
         }
         for(i in 1:length(selected_countries)) {
           colour = c(colour, as.character(rep(clicked_polys$FORMAL_EN[[i]], each=nrow(filter_countries_long))))
         }
         if(plot_mode == 1) {
           df <- data.frame(x = year, y = values)
           ggplot(df, aes(x,y)) +
           geom_line(aes(colour = colour), group = 1) +
           scale_x_discrete(breaks = seq(1950, 2015, by = 5)) +
             labs (x = "Year", y = plot_y_titles[[display_mode]], title = plot_titles[[display_mode]]) +
                theme(
                  legend.title = element_blank(),
                  panel.background = element_rect(fill = "#f5f5f5") # bg of the panel
                  ,plot.background = element_rect(fill = "#f5f5f5") # bg of the plot
                  ,panel.grid.major = element_blank() # get rid of major grid
                  ,panel.grid.minor = element_blank() # get rid of minor grid
                  ,legend.background = element_rect(fill = "#f5f5f5") # get rid of legend bg
                  ,legend.box.background = element_rect(fill = "#f5f5f5") # get rid of legend panel bg
                )
           
         } else {
           data_long <- raw_data %>% filter(Country.Code %in% selected_countries) 
           data_long <- gather(data_long, year, value, "1960":"2015")
           df2 <- data.frame(x=data_long$Country.Name, y = data_long$value)
           
         ggplot(df2, aes(x,y)) +
           geom_boxplot(outlier.colour="black", outlier.shape=16,
                        outlier.size=2, notch=FALSE)+
                labs (x = "Country", y = plot_y_titles[[display_mode]], title = box_titles[[display_mode]]) +
                theme(
                  legend.title = element_blank(),
                  panel.background = element_rect(fill = "#f5f5f5") # bg of the panel
                  ,plot.background = element_rect(fill = "#f5f5f5") # bg of the plot
                  ,panel.grid.major = element_blank() # get rid of major grid
                  ,panel.grid.minor = element_blank() # get rid of minor grid
                  ,legend.background = element_rect(fill = "#f5f5f5") # get rid of legend bg
                  ,legend.box.background = element_rect(fill = "#f5f5f5") # get rid of legend panel bg
                )
           
         }
          })
  } else {
    shinyjs::hide(id = "pltt")
    shinyjs::show(id = "placeholder")
  }
}
shinyServer(
  function(input, output) {
    display_mode <<- 1
    plot_mode <<- 1
    current_year <<- 2015
    
    selected_countries <<- character()
  
    raw_data <- create_data()
    data <- merge_data(raw_data)
    colors <- list(create_color_pallette(raw_data[[1]], "OrRd"), create_color_pallette(raw_data[[2]], "YlOrRd"), create_color_pallette(raw_data[[3]], "GnBu"))
    gray_colors <- list(create_color_pallette(raw_data[[1]], "Greys"), create_color_pallette(raw_data[[2]], "Greys"), create_color_pallette(raw_data[[3]], "Greys"))
    
    titles <<- list("Birth in", "Death in", "Growth in")
    plot_titles <<- list("Amount of births over time per 1000 people", "Amount of deaths over time per 1000 people", "Amount of growth over time per 1000 people")
    box_titles <<- list("Amount of births over time per 1000 people between 1960-2015", "Amount of deaths over time per 1000 people between 1960-2015", "Amount of growth over time per 1000 people between 1950-2015")
    plot_y_titles <<- list("Births per 1000 people", "Deaths per 1000 people", "Growth per 1000 people")
    
    
    leaflet <- generate_leaflet(data[[display_mode]]) #create leaflet
    output$map <- create_world(leaflet, data[[display_mode]], current_year, colors[[display_mode]], titles[[display_mode]])
    # #react on yearslider
    observeEvent(input$yearslide,
        leafletProxy("map", data = data[[display_mode]]) %>% change_year_redraw_world(input$yearslide, data[[display_mode]], colors[[display_mode]], gray_colors[[display_mode]], titles[[display_mode]])
    )

    # react on radio
    observeEvent(input$radio,
        leafletProxy("map", data = data[[as.numeric(input$radio)]]) %>% change_mode_redraw_world(output, as.numeric(input$radio), data[[as.numeric(input$radio)]], raw_data[[as.numeric(input$radio)]], colors[[as.numeric(input$radio)]], titles[[as.numeric(input$radio)]])
    )
    
    observeEvent(input$plt,
         change_plot(input$plt, data[[display_mode]], raw_data[[display_mode]], output)
    )
    
    
    # react when click on a shape
    observeEvent(input$map_shape_click, {
        p <- input$map_shape_click
        if (is.null(p))
          return()
        
        if(p$id %in% selected_countries) {
          selected_countries <<- selected_countries[selected_countries != p$id]
          clicked_count <- subset(data[[display_mode]], ISO_A3 == p$id)
          leafletProxy("map", data = data[[display_mode]]) %>% draw_world(clicked_count, current_year, colors[[display_mode]], titles[[display_mode]], draw_legend = FALSE)
          
        } else {
          selected_countries <<- c(selected_countries, p$id)
        }
        
        draw_plots(data[[display_mode]], raw_data[[display_mode]], output)

        
    })
   

  })
