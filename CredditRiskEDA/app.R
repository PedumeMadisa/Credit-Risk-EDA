
# Credit Risk EDA Dashboard
# app.R


# Load Global Data
source("Global.R")

# Load Modules
source("R/home.R")
source("R/quality.R")
source("R/univariate.R")
source("R/bivariate.R")
source("R/insights.R")


# User Interface


ui <- dashboardPage(
  
  dashboardHeader(
    title = "Credit Risk EDA"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
      
      menuItem(
        "Data Quality",
        tabName = "quality",
        icon = icon("database")
      ),
      
      menuItem(
        "Univariate",
        tabName = "univariate",
        icon = icon("chart-bar")
      ),
      
      menuItem(
        "Bivariate",
        tabName = "bivariate",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Insights",
        tabName = "insights",
        icon = icon("lightbulb")
      )
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      home_ui,
      quality_ui,
      univariate_ui,
      bivariate_ui,
      insights_ui
      
    )
    
  )
  
)


# Server


server <- function(input, output){
  
  home_server(input, output)
  
  quality_server(input, output)
  
  univariate_server(input, output)
  
  bivariate_server(input, output)
  
  insights_server(input, output)
  
}

# Run Application
shinyApp(ui, server)