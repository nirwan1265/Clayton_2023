ui <- dashboardPage(
  dashboardHeader(title = "CLY23-D4 FieldBook Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Filter", tabName = "filter", icon = icon("filter")),
      menuItem("Histograms", tabName = "histograms", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Other tabs remain the same
      # Filter data
      tabItem(tabName = "filter",
              uiOutput("dynamic_filter"),
              DT::dataTableOutput("filteredTable")
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    # Read data
    #df <- read_sheet(spreadsheet_id,sheet=sheet_name)
    df <- read.csv("data/CLY23-D4-FieldBook.csv", stringsAsFactors = F)
    updateSelectInput(session, "hist_var", choices = names(df))
    df
  })
  
  # Dynamic filter UI
  output$dynamic_filter <- renderUI({
    df <- data()
    
    tagList(lapply(names(df), function(column) {
      # If column is of type character, render selectInput
      if (is.character(df[[column]])) {
        selectInput(inputId = column, label = paste("Choose a", column), choices = unique(df[[column]]), selected = NULL)
      } 
      # If column is of type numeric, render sliderInput
      else if (is.numeric(df[[column]])) {
        sliderInput(inputId = column, label = paste("Select a range for", column), min = min(df[[column]], na.rm = TRUE), max = max(df[[column]], na.rm = TRUE), value = c(min(df[[column]], na.rm = TRUE), max(df[[column]], na.rm = TRUE)))
      } 
      # If column is of type date, render dateRangeInput
      else if (inherits(df[[column]], "Date")) {
        dateRangeInput(inputId = column, label = paste("Select a date range for", column), start = min(df[[column]], na.rm = TRUE), end = max(df[[column]], na.rm = TRUE))
      }
    }))
  })
  
  # Filtered data based on user input
  filteredData <- reactive({
    df <- data()
    
    for (column in names(df)) {
      if (!is.null(input[[column]])) {
        if (is.character(df[[column]])) {
          df <- df %>% filter(df[[column]] == input[[column]])
        } else if (is.numeric(df[[column]])) {
          df <- df %>% filter(df[[column]] >= input[[column]][1] & df[[column]] <= input[[column]][2])
        } else if (inherits(df[[column]], "Date")) {
          df <- df %>% filter(df[[column]] >= input[[column]][1] & df[[column]] <= input[[column]][2])
        }
      }
    }
    df
  })
  
  # Other outputs remain the same
  
  output$filteredTable <- DT::renderDataTable({
    DT::datatable(filteredData())
  })
}

shinyApp(ui = ui, server = server)
colnames(df)

####################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "CLY23-D4 FieldBook Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Filter", tabName = "filter", icon = icon("filter")),
      menuItem("Histograms", tabName = "histograms", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Other tabs remain the same
      # Filter data
      tabItem(tabName = "filter",
              uiOutput("dynamic_filter"),
              DT::dataTableOutput("filteredTable")
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    # Read data
    #df <- read_sheet(spreadsheet_id,sheet=sheet_name)
    df <- read.csv("data/CLY23-D4-FieldBook.csv", stringsAsFactors = F)
    updateSelectInput(session, "hist_var", choices = names(df))
    
  })
  
  # Define the columns for which you want to display filters
  filter_columns <- c("Species", "DTS", "DTA", "PH", "EH")
  
  # Dynamic filter UI
  output$dynamic_filter <- renderUI({
    df <- data()
    
    tagList(lapply(filter_columns, function(column) {
      # Only display filter for the specified columns
      if (column %in% names(df)) {
        # If column is of type character, render selectInput
        if (is.character(df[[column]])) {
          selectInput(inputId = column, label = paste("Choose a", column), choices = unique(df[[column]]), selected = NULL)
        } 
        # If column is of type numeric, render sliderInput
        else if (is.numeric(df[[column]])) {
          sliderInput(inputId = column, label = paste("Select a range for", column), min = min(df[[column]], na.rm = TRUE), max = max(df[[column]], na.rm = TRUE), value = c(min(df[[column]], na.rm = TRUE), max(df[[column]], na.rm = TRUE)))
        } 
        # If column is of type date, render dateRangeInput
        else if (inherits(df[[column]], "Date")) {
          dateRangeInput(inputId = column, label = paste("Select a date range for", column), start = min(df[[column]], na.rm = TRUE), end = max(df[[column]], na.rm = TRUE))
        }
      }
    }))
  })
  
  # Filtered data based on user input
  filteredData <- reactive({
    df <- data()
    
    for (column in filter_columns) {
      if (column %in% names(df) && !is.null(input[[column]])) {
        if (is.character(df[[column]])) {
          df <- df %>% filter(df[[column]] == input[[column]])
        } else if (is.numeric(df[[column]])) {
          df <- df %>% filter(df[[column]] >= input[[column]][1] & df[[column]] <= input[[column]][2])
        } else if (inherits(df[[column]], "Date")) {
          df <- df %>% filter(df[[column]] >= input[[column]][1] & df[[column]] <= input[[column]][2])
        }
      }
    }
    df
  })
  
  # Other outputs remain the same
  
  output$filteredTable <- DT::renderDataTable({
    DT::datatable(filteredData())
  })
}

shinyApp(ui = ui, server = server)


##########


ui <- dashboardPage(
  dashboardHeader(title = "CLY23-D4 FieldBook Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Filter", tabName = "filter", icon = icon("filter")),
      menuItem("Histograms", tabName = "histograms", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Other tabs remain the same
      # Filter data
      tabItem(tabName = "filter",
              uiOutput("dynamic_filter"),
              actionButton("reset_filters", "Reset Filters"),
              DT::dataTableOutput("filteredTable")
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    # Read data
    #df <- read_sheet(spreadsheet_id,sheet=sheet_name)
    df <- read.csv("data/CLY23-D4-FieldBook.csv", stringsAsFactors = F)
    updateSelectInput(session, "hist_var", choices = names(df))
    df
  })
  
  # Define the columns for which you want to display filters
  filter_columns <- c("Species", "DTS", "DTA", "PH", "EH")
  
  # Dynamic filter UI
  output$dynamic_filter <- renderUI({
    df <- data()
    
    tagList(lapply(filter_columns, function(column) {
      # Only display filter for the specified columns
      if (column %in% names(df)) {
        # If column is of type character, render selectInput with "Any" as the default
        if (is.character(df[[column]])) {
          selectInput(inputId = column, label = paste("Choose a", column), choices = c("Any" = "", unique(df[[column]])), selected = "")
        } 
        # If column is of type numeric, render sliderInput with min and max values as the default
        else if (is.numeric(df[[column]])) {
          sliderInput(inputId = column, label = paste("Select a range for", column), min = min(df[[column]], na.rm = TRUE), max = max(df[[column]], na.rm = TRUE), value = c(min(df[[column]], na.rm = TRUE), max(df[[column]], na.rm = TRUE)))
        } 
        # If column is of type date, render dateRangeInput with the earliest and latest dates as the default
        else if (inherits(df[[column]], "Date")) {
          dateRangeInput(inputId = column, label = paste("Select a date range for", column), start = min(df[[column]], na.rm = TRUE), end = max(df[[column]], na.rm = TRUE))
        }
      }
    }))
  })
  
  # Filtered data based on user input
  filteredData <- reactive({
    df <- data()
    
    for (column in filter_columns) {
      if (column %in% names(df) && !is.null(input[[column]])) {
        if (is.character(df[[column]])) {
          if (input[[column]] != "") df <- df %>% filter(df[[column]] == input[[column]])
        } else if (is.numeric(df[[column]])) {
          df <- df %>% filter(df[[column]] >= input[[column]][1] & df[[column]] <= input[[column]][2])
        } else if (inherits(df[[column]], "Date")) {
          df <- df %>% filter(df[[column]] >= input[[column]][1] & df[[column]] <= input[[column]][2])
        }
      }
    }
    df
  })
  
  # Other outputs remain the same
  
  output$filteredTable <- DT::renderDataTable({
    DT::datatable(filteredData())
  })
  
  # Reset all filters when the "Reset Filters" button is clicked
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "Species", selected = "")
    updateDateRangeInput(session, "DTS", start = min(data()$DTS, na.rm = TRUE), end = max(data()$DTS, na.rm = TRUE))
    updateDateRangeInput(session, "DTA", start = min(data()$DTA, na.rm = TRUE), end = max(data()$DTA, na.rm = TRUE))
    updateSliderInput(session, "PH", value = c(min(data()$PH, na.rm = TRUE), max(data()$PH, na.rm = TRUE)))
    updateSliderInput(session, "EH", value = c(min(data()$EH, na.rm = TRUE), max(data()$EH, na.rm = TRUE)))
  })
}

shinyApp(ui = ui, server = server)
colnames(df)




###############

ui <- dashboardPage(
  dashboardHeader(title = "CLY23_D4 FieldBook Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Filter", tabName = "filter", icon = icon("filter")),
      menuItem("Histograms", tabName = "histograms", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Other tabs remain the same
      # Filter data
      tabItem(tabName = "filter",
              uiOutput("dynamic_filter"),
              actionButton("reset_filters", "Reset Filters"),
              DT::dataTableOutput("filteredTable")
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    # Read data
    #df <- read_sheet(spreadsheet_id,sheet=sheet_name)
    df <- read.csv("data/CLY23-D4-FieldBook.csv", stringsAsFactors = F)
    updateSelectInput(session, "hist_var", choices = names(df))
    df
  })
  
  # Define the columns for which you want to display filters
  filter_columns <- c("Species", "DTS", "DTA", "PH..cm.", "EH..cm.")
  
  # Dynamic filter UI
  output$dynamic_filter <- renderUI({
    df <- data()
    
    tagList(lapply(filter_columns, function(column) {
      # Only display filter for the specified columns
      if (column %in% names(df)) {
        # If column is of type character, render selectInput with "Any" as the default
        if (is.character(df[[column]])) {
          selectInput(inputId = column, label = paste("Choose a", column), choices = c("Any" = "", unique(df[[column]])), selected = "")
        } 
        # If column is of type numeric, render sliderInput with min and max values as the default
        else if (is.numeric(df[[column]])) {
          sliderInput(inputId = column, label = paste("Select a range for", column), min = min(df[[column]], na.rm = TRUE), max = max(df[[column]], na.rm = TRUE), value = c(min(df[[column]], na.rm = TRUE), max(df[[column]], na.rm = TRUE)))
        } 
        # If column is of type date, render dateRangeInput with the earliest and latest dates as the default
        else if (inherits(df[[column]], "Date") || inherits(df[[column]], "POSIXct")) {
          dateRangeInput(inputId = column, label = paste("Select a date range for", column), start = min(df[[column]], na.rm = TRUE), end = max(df[[column]], na.rm = TRUE))
        }
      }
    }))
  })
  
  # Filtered data based on user input
  filteredData <- reactive({
    df <- data()
    
    for (column in filter_columns) {
      if (column %in% names(df) && !is.null(input[[column]])) {
        if (is.character(df[[column]])) {
          if (input[[column]] != "") df <- df %>% filter(df[[column]] == input[[column]])
        } else if (is.numeric(df[[column]])) {
          df <- df %>% filter(df[[column]] >= input[[column]][1] & df[[column]] <= input[[column]][2])
        } else if (inherits(df[[column]], "Date") || inherits(df[[column]], "POSIXct")) {
          df <- df %>% filter(df[[column]] >= input[[column]][1] & df[[column]] <= input[[column]][2])
        }
      }
    }
    df
  })
  
  # Other outputs remain the same
  
  output$filteredTable <- DT::renderDataTable({
    DT::datatable(filteredData())
  })
  
  # Reset all filters when the "Reset Filters" button is clicked
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "Species", selected = "")
    updateDateRangeInput(session, "DTS", start = min(data()$DTS, na.rm = TRUE), end = max(data()$DTS, na.rm = TRUE))
    updateDateRangeInput(session, "DTA", start = min(data()$DTA, na.rm = TRUE), end = max(data()$DTA, na.rm = TRUE))
    updateSliderInput(session, "PH..cm.", value = c(min(data()$"PH..cm.", na.rm = TRUE), max(data()$"PH..cm.", na.rm = TRUE)))
    updateSliderInput(session, "EH..cm.", value = c(min(data()$"EH..cm.", na.rm = TRUE), max(data()$"EH..cm.", na.rm = TRUE)))
  })
}

shinyApp(ui, server)


################



ui <- dashboardPage(
  dashboardHeader(title = "CLY23_D4 FieldBook Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Filter", tabName = "filter", icon = icon("filter")),
      menuItem("Histograms", tabName = "histograms", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "filter",
              uiOutput("dynamic_filter"),
              actionButton("reset_filters", "Reset Filters"),
              DT::dataTableOutput("filteredTable")
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    #df <- read_sheet(spreadsheet_id,sheet=sheet_name)
    df <- read.csv("data/CLY23-D4-FieldBook.csv", stringsAsFactors = F)
    df$DTS <- as.Date(df$DTS)
    df$DTA <- as.Date(df$DTA)
    updateSelectInput(session, "hist_var", choices = names(df))
    df
  })
  
  filter_columns <- c("Species", "DTS", "DTA", "PH..cm.", "EH..cm.")
  
  output$dynamic_filter <- renderUI({
    df <- data()
    
    tagList(lapply(filter_columns, function(column) {
      if (column %in% names(df)) {
        if (is.character(df[[column]])) {
          selectInput(inputId = column, label = paste("Choose a", column), choices = c("Any" = "", unique(df[[column]])), selected = "")
        } 
        else if (is.numeric(df[[column]])) {
          sliderInput(inputId = column, label = paste("Select a range for", column), min = min(df[[column]], na.rm = TRUE), max = max(df[[column]], na.rm = TRUE), value = c(min(df[[column]], na.rm = TRUE), max(df[[column]], na.rm = TRUE)))
        } 
        else if (inherits(df[[column]], "Date") || inherits(df[[column]], "POSIXct")) {
          dateRangeInput(inputId = column, label = paste("Select a date range for", column), start = min(df[[column]], na.rm = TRUE), end = max(df[[column]], na.rm = TRUE))
        }
      }
    }))
  })
  
  filteredData <- reactive({
    df <- data()
    
    for (column in filter_columns) {
      if (column %in% names(df) && !is.null(input[[column]])) {
        if (is.character(df[[column]])) {
          if (input[[column]] != "") df <- df %>% filter(df[[column]] == input[[column]])
        } 
        else if (is.numeric(df[[column]])) {
          df <- df %>% filter(df[[column]] >= input[[column]][1] & df[[column]] <= input[[column]][2])
        } 
        else if (inherits(df[[column]], "Date") || inherits(df[[column]], "POSIXct")) {
          df <- df %>% filter(df[[column]] >= input[[column]][1] & df[[column]] <= input[[column]][2])
        }
      }
    }
    df
  })
  
  output$filteredTable <- DT::renderDataTable({
    DT::datatable(filteredData())
  })
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "Species", selected = "")
    updateDateRangeInput(session, "DTS", start = min(data()$DTS, na.rm = TRUE), end = max(data()$DTS, na.rm = TRUE))
    updateDateRangeInput(session, "DTA", start = min(data()$DTA, na.rm = TRUE), end = max(data()$DTA, na.rm = TRUE))
    updateSliderInput(session, "PH..cm.", value = c(min(data()$"PH..cm.", na.rm = TRUE), max(data()$"PH..cm.", na.rm = TRUE)))
    updateSliderInput(session, "EH..cm.", value = c(min(data()$"EH..cm.", na.rm = TRUE), max(data()$"EH..cm.", na.rm = TRUE)))
  })
}

shinyApp(ui, server)


