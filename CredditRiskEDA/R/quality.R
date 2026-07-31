
# Credit Risk EDA Dashboard
# Data Quality Module



# Data Quality UI


quality_ui <- tabItem(
  
  tabName = "quality",
  

  # Value Boxes

  
  fluidRow(
    
    valueBox(
      value = total_missing,
      subtitle = "Missing Values",
      icon = icon("exclamation-circle"),
      color = "orange"
    ),
    
    valueBox(
      value = duplicated_records,
      subtitle = "Duplicate Records",
      icon = icon("copy"),
      color = "red"
    ),
    
    valueBox(
      value = number_numeric,
      subtitle = "Numeric Variables",
      icon = icon("calculator"),
      color = "blue"
    ),
    
    valueBox(
      value = number_categorical,
      subtitle = "Categorical Variables",
      icon = icon("list"),
      color = "green"
    )
    
  ),
  

  # Missing Values Table

  
  fluidRow(
    
    box(
      
      title = "Missing Values by Variable",
      
      width = 12,
      
      status = "primary",
      
      solidHeader = TRUE,
      
      DTOutput("missing_table")
      
    )
    
  ),
  

  # Missing Values Plot

  
  fluidRow(
    
    box(
      
      title = "Missing Values Plot",
      
      width = 12,
      
      status = "info",
      
      solidHeader = TRUE,
      
      plotOutput("missing_plot", height = "400px")
      
    )
    
  ),
  

  # Variable Types

  
  
  fluidRow(
    
    box(
      
      title = "Variable Data Types",
      
      width = 12,
      
      status = "warning",
      
      solidHeader = TRUE,
      
      DTOutput("datatype_table")
      
    )
    
  )
  
)


# Data Quality Server


quality_server <- function(input, output){
  

  # Missing Values Table

  
  output$missing_table <- renderDT({
    
    data.frame(
      
      Variable = names(loan_data),
      
      Missing_Values = colSums(is.na(loan_data)),
      
      Missing_Percentage =
        round(colSums(is.na(loan_data)) /
                nrow(loan_data) * 100, 2)
      
    )
    
  },
  options = list(pageLength = 10))
  

  # Missing Values Plot

  
  output$missing_plot <- renderPlot({
    
    missing_df <- data.frame(
      
      Variable = names(loan_data),
      
      Missing = colSums(is.na(loan_data))
      
    )
    
    ggplot(missing_df,
           aes(x = reorder(Variable, Missing),
               y = Missing)) +
      
      geom_col(fill = "steelblue") +
      
      coord_flip() +
      
      labs(
        x = "Variable",
        y = "Missing Values"
      ) +
      
      theme_minimal()
    
  })
  

  # Variable Types Table

  
  output$datatype_table <- renderDT({
    
    data.frame(
      
      Variable = names(loan_data),
      
      Data_Type = sapply(loan_data, class)
      
    )
    
  },
  options = list(pageLength = 10))
  
}