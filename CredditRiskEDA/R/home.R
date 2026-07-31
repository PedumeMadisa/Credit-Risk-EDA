
# Credit Risk EDA Dashboard
# Home Module



# Home UI


home_ui <- tabItem(
  
  tabName = "home",
  

  # Value Boxes

  
  fluidRow(
    
    valueBox(
      value = number_observations,
      subtitle = "Loan Applications",
      icon = icon("users"),
      color = "blue"
    ),
    
    valueBox(
      value = number_variables,
      subtitle = "Variables",
      icon = icon("table"),
      color = "green"
    ),
    
    valueBox(
      value = paste0(default_rate, "%"),
      subtitle = "Default Rate",
      icon = icon("exclamation-triangle"),
      color = "red"
    ),
    
    valueBox(
      value = paste(train_size, "/", test_size),
      subtitle = "Train / Test",
      icon = icon("database"),
      color = "purple"
    )
    
  ),
  

  # Project Description

  
  fluidRow(
    
    box(
      title = "Project Overview",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      p(
        "This dashboard provides an exploratory data analysis (EDA) of the loan dataset.",
        "The analysis focuses on understanding the data before building a credit risk model."
      )
      
    )
    
  ),
  

  # Dataset Summary

  
  fluidRow(
    
    box(
      title = "Dataset Summary",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      
      tableOutput("dataset_summary")
      
    )
    
  )
  
)


# Home Server


home_server <- function(input, output){
  
  output$dataset_summary <- renderTable({
    
    data.frame(
      
      Metric = c(
        "Observations",
        "Variables",
        "Numeric Variables",
        "Categorical Variables",
        "Missing Values",
        "Duplicate Records"
      ),
      
      Value = c(
        number_observations,
        number_variables,
        number_numeric,
        number_categorical,
        total_missing,
        duplicated_records
      )
      
    )
    
  })
  
}