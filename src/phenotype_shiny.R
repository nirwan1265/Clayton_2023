library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)

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
    df <- read.csv("/Users/nirwan/Library/Mobile Documents/com~apple~CloudDocs/Github/Clayton_2023/data/CLY23-D4-FieldBook.csv", stringsAsFactors = FALSE, check.names = F)
    updateSelectInput(session, "genotype", choices = unique(df$`Female genotype`))
    updateSelectInput(session, "species", choices = unique(df$Species))
    updateSelectInput(session, "hist_var", choices = names(df))
    df
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
    hist(df[[input$hist_var]], main = paste("Histogram of", input$hist_var), xlab = input$hist_var)
  })
}

shinyApp(ui = ui, server = server)
unique(df$Female.genotype)
colnames(df)
typeof(df$`PH (cm)`)
hist(df$`PH (cm)`)

str(df)
