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
              selectInput("hist_var", "Select a variable", choices = NULL),
              plotOutput("hist")
      ),
      tabItem(tabName = "violin",
              selectInput("violin_var", "Select a variable", choices = NULL),
              plotOutput("violin")
      ),
      tabItem(tabName = "boxplot",
              selectInput("box_var", "Select a variable", choices = NULL),
              plotOutput("boxplot")
      ),
      tabItem(tabName = "density",
              selectInput("density_var", "Select a variable", choices = NULL),
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
