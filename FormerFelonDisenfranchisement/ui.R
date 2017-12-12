library(shiny)
library(leaflet)
library(DT)
df <- read.csv("Disenfranchised-Final-Data-For-Mapping.csv")
columns <- colnames(df[,3:ncol(df)])

#stats.2016 <- read.csv("C:/Users/rchaudhry/OneDrive - Center on Budget and Policy Priorities/Projects/GitHub/Basketball/Apps/PlayerStats/CompleteSeasonStats_2016.csv")
#basic.2016 <- read.csv("C:/Users/rchaudhry/OneDrive - Center on Budget and Policy Priorities/Projects/GitHub/Basketball/Apps/PlayerStats/SeasonStatsBasic_2016.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Mapping the Disenfranchised"),
  mainPanel(
    tabsetPanel(id = "tabs",
      tabPanel("Select Values to Map",
        mainPanel(
          selectInput("firstmap", "First map:", columns),
          selectInput("secondmap", "Second map:", columns),
          checkboxInput("filter", "Show only above columns in table?")
        )
      ),
      tabPanel("Maps", 
          fluidRow(
            leafletOutput("MapOne"),
            leafletOutput("MapTwo")
          )
      ),
      tabPanel("Table", dataTableOutput("Table"))
    )
  )
))