library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(googlesheets4)
library(googledrive)
gs4_auth()

# https://docs.google.com/spreadsheets/d/1e9963LdfMkas-KQHrQNLIvkyWHFOFYxJstpMjiRHfcA/edit#gid=0&fvid=375149192
# id is after d
spreadsheet_id <- "1e9963LdfMkas-KQHrQNLIvkyWHFOFYxJstpMjiRHfcA"
sheet_name <- "Sheet1"

ui <- dashboardPage(
  dashboardHeader(title = "CLY23-D4 FieldBook Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Filter", tabName = "filter", icon = icon("filter")),
      menuItem("Histograms", tabName = "histograms", icon = icon("bar-chart"))
      # you can add more items here
    )
  ),
  dashboardBody(
    tabItems(
      # Display data table
      tabItem(tabName = "table",
              DT::dataTableOutput("dataTable")
      ),
      # Filter data
      tabItem(tabName = "filter",
              selectInput("genotype", "Choose a Female Genotype", choices = NULL),
              selectInput("species", "Choose a Species", choices = NULL),
              DT::dataTableOutput("filteredTable")
      ),
      # Histogram
      tabItem(tabName = "histograms",
              selectInput("hist_var", "Choose a Variable", choices = NULL),
              plotOutput("histPlot")
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    # Read data
    df <- read_sheet(spreadsheet_id,sheet=sheet_name)
    updateSelectInput(session, "genotype", choices = unique(df$`Female genotype`))
    updateSelectInput(session, "species", choices = unique(df$Species))
    updateSelectInput(session, "hist_var", choices = names(df))
    
  })
  
  output$dataTable <- DT::renderDataTable({
    DT::datatable(data())
  })
  
  output$filteredTable <- DT::renderDataTable({
    req(input$genotype, input$species)
    DT::datatable(data() %>% filter(`Female genotype` == input$genotype, Species == input$species))
  })
  
  output$histPlot <- renderPlot({
    req(input$hist_var)
    hist(data()[[input$hist_var]], main = paste("Histogram of", input$hist_var), xlab = input$hist_var)
  })
  
}

shinyApp(ui = ui, server = server)

colnames(df)
str(df)
