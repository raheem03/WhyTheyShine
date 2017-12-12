library(shiny)
library(DT)
library(devtools)
library(tidyverse)
library(geojsonio)
library(leaflet)

shinyServer(function(input, output) {
  df <- read.csv("Disenfranchised-Final-Data-For-Mapping.csv")
  df$Former.Slave.State <- as.factor(df$Former.Slave.State)  
  states <- geojson_read("USStates.json", what = "sp")
  joined <- sp::merge(states, df, by.x = "NAME", by.y = "State")
  
  dataInput <- reactive ({
    if(input$filter == TRUE) {
      df.filtered <- df[c("State", input$firstmap, input$secondmap)]
    } else {
        df.filtered <- df
      }
    })
 
  mappingVar <- reactive ({
    col <- joined[[input$firstmap]]

  })
  
  mappingVar2 <- reactive ({
    col <- joined[[input$secondmap]]
    
  })

  output$Table <- renderDataTable ({
    df <- dataInput()
    table <- datatable(df)
    numericCols <- df[names(df[sapply(df, is.numeric)])] # problem seem to be that when we mix numeric and factor, this line turns out wrong --> gets df, but not col names
    print(length(numericCols))
    if(length(numericCols) != 0) {
      percentCols <- names(numericCols[sapply(numericCols, function(x) max(x[which(!is.na(x))]) <= 1)])
      numberCols <- names(numericCols[sapply(numericCols, function(x) max(x[which(!is.na(x))]) > 1)])
      if(length(percentCols) != 0) {
        table <- table %>% formatPercentage(percentCols, 1)
      }
      if(length(numberCols) != 0) {
        table <- table %>% formatCurrency(numberCols, "", digits = 0)
      }
    }
    print(table)
    },
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 25, autoWidth = TRUE, columnDefs = list(list(width = '250px', targets = "_all"))
      ))
  
  output$MapOne <- renderLeaflet({
    mappingVar <- mappingVar()
    if (is.factor(mappingVar)) {
      pal <- colorFactor("YlOrRd", domain = mappingVar)
    } else {
        pal <- colorQuantile("YlOrRd", domain = mappingVar, n = 7)
      }
    if (is.numeric(mappingVar)) {
      if (max(mappingVar[which(!is.na(mappingVar))]) <= 1) {
        adjustment <- 100
        labels <- sprintf("%s: %s", joined$NAME, sprintf("%1.1f%%", adjustment*mappingVar)) %>% lapply(htmltools::HTML)
      } else {
        adjustment <- 1
        labels <- sprintf("%s: %s", joined$NAME, format(adjustment*mappingVar, scientific = FALSE, big.mark = ",")) %>% lapply(htmltools::HTML)
      }
    } else {
      adjustment <- 1
      labels <- sprintf("%s: %s", joined$NAME, format(adjustment*as.integer(mappingVar), scientific = FALSE, big.mark = ",")) %>% lapply(htmltools::HTML)
    }
    if(class(file) == "try-error") {
      adjustment <- 1
      labels <- sprintf("%s: %s", joined$NAME, format(adjustment*mappingVar, scientific = FALSE, big.mark = ",")) %>% lapply(htmltools::HTML)
    }
    
    leaflet(joined) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOXKEY'))) %>% 
    addPolygons(
      fillColor = ~pal(mappingVar),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 3,
        color = "#F63A25",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))  %>% 
      addLegend(
        pal = pal, 
        values = as.numeric(mappingVar), 
        opacity = 0.7, 
        title = input$firstmap,
        position = "bottomright",
        labFormat = labelFormat(transform = function(x) as.integer(adjustment*x)))
  })
  
  output$MapTwo <- renderLeaflet({
    mappingVar2 <- mappingVar2()
    if (is.factor(mappingVar2)) {
      pal <- colorFactor("Blues", domain = mappingVar2)
    } else {
      pal <- colorQuantile("Blues", domain = mappingVar2, n = 7)
    }
    if (is.numeric(mappingVar2)) {
      if (max(mappingVar2[which(!is.na(mappingVar2))]) <= 1) {
        adjustment <- 100
        labels <- sprintf("%s: %s", joined$NAME, sprintf("%1.1f%%", adjustment*mappingVar2)) %>% lapply(htmltools::HTML)
      } else {
        adjustment <- 1
        labels <- sprintf("%s: %s", joined$NAME, format(adjustment*as.integer(mappingVar2), scientific = FALSE, big.mark = ",")) %>% lapply(htmltools::HTML)
      }
    } else {
      adjustment <- 1
      labels <- sprintf("%s: %s", joined$NAME, format(adjustment*mappingVar2, scientific = FALSE, big.mark = ",")) %>% lapply(htmltools::HTML)
    }
    if(class(file) == "try-error") {
      adjustment <- 1
      labels <- sprintf("%s: %s", joined$NAME, format(adjustment*mappingVar2, scientific = FALSE, big.mark = ",")) %>% lapply(htmltools::HTML)
    }
    
    leaflet(joined) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOXKEY'))) %>% 
      addPolygons(
        fillColor = ~pal(mappingVar2),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#F63A25",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))  %>% 
      addLegend(
        pal = pal, 
        values = as.numeric(mappingVar2), 
        opacity = 0.7, 
        title = input$secondmap,
        position = "bottomright",
        labFormat = labelFormat(transform = function(x) as.integer(adjustment*x)))
  })

  })