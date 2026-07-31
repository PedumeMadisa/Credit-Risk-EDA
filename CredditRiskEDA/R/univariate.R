
# Credit Risk EDA Dashboard
# Univariate Analysis Module





# UI



univariate_ui <- tabItem(
  
  tabName = "univariate",
  
  
  #-----------------------------------
  # Variable Selection
  #-----------------------------------
  
  fluidRow(
    
    box(
      
      title = "Select Variable",
      
      width = 12,
      
      status = "primary",
      
      solidHeader = TRUE,
      
      selectInput(
        
        "selected_variable",
        
        "Variable",
        
        choices = setdiff(names(loan_data), "default_flag")
        
      )
      
    )
    
  ),
  
  
  #-----------------------------------
  # Summary Statistics
  #-----------------------------------
  
  fluidRow(
    
    box(
      
      title = "Summary Statistics",
      
      width = 4,
      
      status = "info",
      
      solidHeader = TRUE,
      
      DTOutput("summary_stats")
      
    ),
    
    
    box(
      
      title = "Distribution",
      
      width = 8,
      
      status = "info",
      
      solidHeader = TRUE,
      
      plotlyOutput("distribution_plot")
      
    )
    
  ),
  
  
  #-----------------------------------
  # Distribution by Target
  #-----------------------------------
  
  fluidRow(
    
    box(
      
      title = "Distribution by Default Status",
      
      width = 12,
      
      status = "warning",
      
      solidHeader = TRUE,
      
      plotlyOutput("target_plot")
      
    )
    
  ),
  
  
  #-----------------------------------
  # WoE and IV Section
  #-----------------------------------
  
  fluidRow(
    
    box(
      
      title = "Weight of Evidence (WoE) & Information Value (IV)",
      
      width = 12,
      
      status = "success",
      
      solidHeader = TRUE,
      
      DTOutput("woe_iv_table")
      
    )
    
  )
  
)



# ==============================
# SERVER
# ==============================


univariate_server <- function(input, output){
  
  
  # Selected variable
  
  selected_data <- reactive({
    
    req(input$selected_variable)
    
    loan_data[[input$selected_variable]]
    
  })
  
  
  
  #----------------------------------
  # Summary Statistics
  #----------------------------------
  
  
  output$summary_stats <- renderDT({
    
    
    variable <- selected_data()
    
    
    if(is.numeric(variable)){
      
      
      summary_df <- data.frame(
        
        Statistic = c(
          
          "Minimum",
          "1st Quartile",
          "Median",
          "Mean",
          "3rd Quartile",
          "Maximum",
          "Standard Deviation",
          "Missing Values"
          
        ),
        
        
        Value = c(
          
          min(variable, na.rm = TRUE),
          
          quantile(variable,
                   0.25,
                   na.rm = TRUE),
          
          median(variable,
                 na.rm = TRUE),
          
          mean(variable,
               na.rm = TRUE),
          
          quantile(variable,
                   0.75,
                   na.rm = TRUE),
          
          max(variable,
              na.rm = TRUE),
          
          sd(variable,
             na.rm = TRUE),
          
          sum(is.na(variable))
          
        )
        
      )
      
      
    } else {
      
      
      frequency <- table(variable)
      
      
      summary_df <- data.frame(
        
        Category = names(frequency),
        
        Frequency = as.numeric(frequency)
        
      )
      
      
    }
    
    
    
    datatable(
      
      summary_df,
      
      options = list(
        
        dom = "t",
        
        paging = FALSE
        
      ),
      
      rownames = FALSE
      
    )
    
    
  })
  
  
  
  
  #----------------------------------
  # Distribution Plot
  #----------------------------------
  
  
  output$distribution_plot <- renderPlotly({
    
    
    variable <- selected_data()
    
    
    df <- data.frame(
      
      value = variable
      
    )
    
    
    if(is.numeric(variable)){
      
      
      p <- ggplot(
        
        df,
        
        aes(x=value)
        
      ) +
        
        geom_histogram(
          
          bins = 30,
          
          fill = "steelblue",
          
          color = "white"
          
        ) +
        
        labs(
          
          x=input$selected_variable,
          
          y="Count"
          
        ) +
        
        theme_minimal()
      
      
    }else{
      
      
      p <- ggplot(
        
        df,
        
        aes(x=as.factor(value))
        
      ) +
        
        geom_bar(
          
          fill="steelblue"
          
        ) +
        
        labs(
          
          x=input$selected_variable,
          
          y="Count"
          
        ) +
        
        theme_minimal()
      
      
    }
    
    
    ggplotly(p)
    
    
  })
  
  
  
  
  #----------------------------------
  # Distribution by Default Flag
  #----------------------------------
  
  
  output$target_plot <- renderPlotly({
    
    
    variable <- selected_data()
    
    
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
          
          y=input$selected_variable
          
        ) +
        
        theme_minimal()
      
      
    }else{
      
      
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
          
          x=input$selected_variable,
          
          y="Proportion",
          
          fill="Default Flag"
          
        ) +
        
        theme_minimal()
      
      
    }
    
    
    
    ggplotly(p)
    
    
  })
  
  
  
  
  #----------------------------------
  # WoE and IV Table
  #----------------------------------
  
  
  output$woe_iv_table <- renderDT({
    
    
    data.frame(
      
      Message =
        "WoE and IV calculation will be added using the scorecard package."
      
    ) %>%
      
      datatable(
        
        options=list(
          
          dom="t"
          
        )
        
      )
    
    
  })
  
  
}