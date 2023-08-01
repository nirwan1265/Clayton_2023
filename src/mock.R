ui <- dashboardPage(
  dashboardHeader(title = "CLY23_D4 FieldBook Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Statistics", tabName = "stats", icon = icon("line-chart")),
      menuItem("Figures", tabName = "figures", icon = icon("bar-chart"),
               menuSubItem("Histogram", tabName = "histogram"),
               menuSubItem("Violin Plot", tabName = "violin"),
               menuSubItem("Boxplot", tabName = "boxplot"),
               menuSubItem("Density Plot", tabName = "density"),
               menuSubItem("Scatter Plot", tabName = "scatter"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table",
              fluidRow(
                column(6, uiOutput("dynamic_filter1")),
                column(6, uiOutput("dynamic_filter2")),
                column(12, actionButton("reset_filters", "Reset Filters"), downloadButton('downloadData', 'Download CSV'))
              ),
              fluidRow(
                column(12, DT::dataTableOutput("filteredTable"))
              )
      ),
      tabItem(tabName = "stats",
              selectInput("stat_var", "Select a variable", choices = NULL),
              verbatimTextOutput("stats")
      ),
      tabItem(tabName = "histogram",
              selectInput("fig_var", "Select a variable", choices = NULL),
              plotOutput("hist")
      ),
      tabItem(tabName = "violin",
              selectInput("fig_var", "Select a variable", choices = NULL),
              plotOutput("violin")
      ),
      tabItem(tabName = "boxplot",
              selectInput("fig_var", "Select a variable", choices = NULL),
              plotOutput("boxplot")
      ),
      tabItem(tabName = "density",
              selectInput("fig_var", "Select a variable", choices = NULL),
              plotOutput("density")
      ),
      tabItem(tabName = "scatter",
              selectInput("scatter_var1", "Select variable for x-axis", choices = NULL),
              selectInput("scatter_var2", "Select variable for y-axis", choices = NULL),
              plotOutput("scatter")
      )
    )
  )
)


server <- function(input, output, session) {
  data <- reactive({
    df <- read.csv("data/CLY23-D4-FieldBook.csv", stringsAsFactors = F)
    df$DTS <- as.Date(df$DTS, format = "%d/%m/%Y")
    df$DTA <- as.Date(df$DTA, format = "%d/%m/%Y")
    updateSelectInput(session, "fig_var", choices = names(df[sapply(df, is.numeric)]))
    updateSelectInput(session, "scatter_var1", choices = names(df[sapply(df, is.numeric)]))
    updateSelectInput(session, "scatter_var2", choices = names(df[sapply(df, is.numeric)]))
    updateSelectInput(session, "stat_var", choices = names(df[sapply(df, is.numeric)]))  # Update the "stat_var"
    df
  })
  
  filter_columns <- c("Species", "DTS", "DTA", "PH..cm.", "EH..cm.")
  
  output$dynamic_filter1 <- renderUI({
    df <- data()
    tagList(lapply(filter_columns[1:3], function(column) {
      renderFilter(df, column)
    }))
  })
  
  output$dynamic_filter2 <- renderUI({
    df <- data()
    tagList(lapply(filter_columns[4:5], function(column) {
      renderFilter(df, column)
    }))
  })
  
  renderFilter <- function(df, column) {
    if (is.character(df[[column]])) {
      selectInput(inputId = column, label = paste("Choose a", column), choices = c("Any" = "", unique(df[[column]])), selected = "")
    } else if (is.numeric(df[[column]])) {
      sliderInput(inputId = column, label = paste("Select a range for", column), min = min(df[[column]], na.rm = TRUE), max = max(df[[column]], na.rm = TRUE), value = c(min(df[[column]], na.rm = TRUE), max(df[[column]], na.rm = TRUE)))
    } else if (inherits(df[[column]], "Date") || inherits(df[[column]], "POSIXct")) {
      dateRangeInput(inputId = column, label = paste("Select a date range for", column), start = min(df[[column]], na.rm = TRUE), end = max(df[[column]], na.rm = TRUE))
    }
  }
  
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
  
  # output$filteredTable <- DT::renderDataTable({
  #   datatable <- DT::datatable(filteredData(), options = list(pageLength = 25))
  #   datatable = datatable %>% formatDate(c('DTS', 'DTA'), method = function(x) format(x, "%d/%m/%Y"))
  #   return(datatable)
  # })
  # 
  output$filteredTable <- DT::renderDataTable({
    DT::datatable(filteredData())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "Species", selected = "")
    updateDateRangeInput(session, "DTS", start = min(data()$DTS, na.rm = TRUE), end = max(data()$DTS, na.rm = TRUE))
    updateDateRangeInput(session, "DTA", start = min(data()$DTA, na.rm = TRUE), end = max(data()$DTA, na.rm = TRUE))
    updateSliderInput(session, "PH..cm.", value = c(min(data()$"PH..cm.", na.rm = TRUE), max(data()$"PH..cm.", na.rm = TRUE)))
    updateSliderInput(session, "EH..cm.", value = c(min(data()$"EH..cm.", na.rm = TRUE), max(data()$"EH..cm.", na.rm = TRUE)))
  })
  
  output$stats <- renderPrint({
    df <- data()
    if (!is.null(input$stat_var) & input$stat_var %in% names(df)) {
      variable <- df[[input$stat_var]]
      na_count <- sum(is.na(variable))
      list(
        Mean = mean(variable, na.rm = TRUE),
        Median = median(variable, na.rm = TRUE),
        SD = sd(variable, na.rm = TRUE),
        Min = min(variable, na.rm = TRUE),
        Max = max(variable, na.rm = TRUE),
        "Count of NA" = na_count,
        "Percentage of NA" = round(na_count / nrow(df) * 100, 2)
      )
    }
  })
  
  output$hist <- renderPlot({
    df <- data()
    if (!is.null(input$fig_var) & input$fig_var %in% names(df)) {
      hist(df[[input$fig_var]], main = paste(input$fig_var, "Histogram"), xlab = NULL, col = "skyblue")
    }
  })
  
  output$violin <- renderPlot({
    df <- data()
    if (!is.null(input$fig_var) & input$fig_var %in% names(df)) {
      ggplot(df, aes(x = "", y = .data[[input$fig_var]])) +
        geom_violin(fill = "skyblue") +
        labs(x = NULL, y = NULL, title = paste(input$fig_var, "Violin Plot")) +
        theme_minimal()
    }
  })
  
  output$boxplot <- renderPlot({
    df <- data()
    if (!is.null(input$fig_var) & input$fig_var %in% names(df)) {
      boxplot(df[[input$fig_var]], main = paste(input$fig_var, "Boxplot"), xlab = NULL, col = "skyblue")
    }
  })
  
  output$density <- renderPlot({
    df <- data()
    if (!is.null(input$fig_var) & input$fig_var %in% names(df)) {
      plot(density(df[[input$fig_var]], na.rm = TRUE), main = paste(input$fig_var, "Density Plot"))
    }
  })
  
  output$scatter <- renderPlotly({
    df <- data()
    if (!is.null(input$scatter_var1) & !is.null(input$scatter_var2) & input$scatter_var1 %in% names(df) & input$scatter_var2 %in% names(df)) {
      # Filter out rows with NA in the variables to be plotted
      df <- df[!is.na(df[[input$scatter_var1]]) & !is.na(df[[input$scatter_var2]]), ]
      plot_ly(df, x = ~.data[[input$scatter_var1]], y = ~.data[[input$scatter_var2]], type = 'scatter', mode = 'markers', text = ~paste('CLY23.D4:', .data$CLY23.D4, 'Rep:', .data$Rep), hoverinfo = 'text') %>%
        layout(title = "Scatter Plot", xaxis = list(title = input$scatter_var1), yaxis = list(title = input$scatter_var2))
    }
  })
  
}

shinyApp(ui, server)


colnames(df)
