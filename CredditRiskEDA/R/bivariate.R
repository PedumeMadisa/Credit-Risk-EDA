
# Credit Risk EDA Dashboard
# Bivariate Analysis Module





# UI



bivariate_ui <- tabItem(
  
  tabName = "bivariate",
  
  

  # Variable Selection

  
  fluidRow(
    
    box(
      
      title = "Select Predictor Variable",
      
      width = 12,
      
      status = "primary",
      
      solidHeader = TRUE,
      
      selectInput(
        
        "bivariate_variable",
        
        "Variable",
        
        choices = setdiff(names(loan_data),
                          "default_flag")
        
      )
      
    )
    
  ),
  
  
  

  # Relationship Plot

  
  
  fluidRow(
    
    box(
      
      title = "Relationship with Default Status",
      
      width = 8,
      
      status = "info",
      
      solidHeader = TRUE,
      
      plotlyOutput("bivariate_plot")
      
    ),
    
    
    box(
      
      title = "Default Rate Summary",
      
      width = 4,
      
      status = "warning",
      
      solidHeader = TRUE,
      
      DTOutput("default_rate_table")
      
    )
    
  ),
  
  
  

  # Correlation Section

  
  
  fluidRow(
    
    box(
      
      title = "Numeric Correlation with Default",
      
      width = 12,
      
      status = "success",
      
      solidHeader = TRUE,
      
      DTOutput("correlation_table")
      
    )
    
  )
  
  
)






# SERVER




bivariate_server <- function(input, output){
  
  
  
  # Selected variable
  
  selected_variable <- reactive({
    
    req(input$bivariate_variable)
    
    loan_data[[input$bivariate_variable]]
    
  })
  
  
  
  

  # Relationship Plot

  
  
  output$bivariate_plot <- renderPlotly({
    
    
    variable <- selected_variable()
    
    
    df <- data.frame(
      
      variable = variable,
      
      default_flag =
        factor(loan_data$default_flag)
      
    )
    
    
    if(is.numeric(variable)){
      
      
      p <- ggplot(
        
        df,
        
        aes(
          x=default_flag,
          y=variable
        )
        
      ) +
        
        geom_boxplot(
          
          fill="orange"
          
        ) +
        
        labs(
          
          x="Default Flag",
          
          y=input$bivariate_variable
          
        ) +
        
        theme_minimal()
      
      
      
    } else {
      
      
      p <- ggplot(
        
        df,
        
        aes(
          x=variable,
          
          fill=default_flag
          
        )
        
      ) +
        
        geom_bar(
          
          position="fill"
          
        ) +
        
        labs(
          
          x=input$bivariate_variable,
          
          y="Proportion",
          
          fill="Default Flag"
          
        ) +
        
        theme_minimal()
      
      
    }
    
    
    ggplotly(p)
    
    
  })
  
  
  
  

  # Default Rate Table

  
  
  output$default_rate_table <- renderDT({
    
    
    variable <- selected_variable()
    
    
    df <- data.frame(
      
      variable = variable,
      
      default_flag =
        loan_data$default_flag
      
    )
    
    
    
    summary <- df %>%
      
      group_by(variable) %>%
      
      summarise(
        
        Applications = n(),
        
        Defaults = sum(default_flag),
        
        Default_Rate =
          round(
            mean(default_flag)*100,
            2
          )
        
      )
    
    
    
    datatable(
      
      summary,
      
      options=list(
        
        pageLength=10
        
      ),
      
      rownames=FALSE
      
    )
    
    
  })
  
  
  
  

  # Correlation Table

  
  
  output$correlation_table <- renderDT({
    
    
    numeric_data <- loan_data %>%
      
      select(where(is.numeric))
    
    
    
    correlations <- data.frame(
      
      Variable =
        names(numeric_data),
      
      Correlation =
        sapply(
          numeric_data,
          function(x)
            cor(
              x,
              loan_data$default_flag,
              use="complete.obs"
            )
        )
      
    )
    
    
    correlations <- correlations %>%
      
      arrange(
        desc(abs(Correlation))
      )
    
    
    
    datatable(
      
      correlations,
      
      options=list(
        
        pageLength=10
        
      ),
      
      rownames=FALSE
      
    )
    
    
  })
  
  
}