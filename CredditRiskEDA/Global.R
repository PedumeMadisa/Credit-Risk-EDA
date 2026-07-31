

# Global.R



# Load Required Packages


library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(DataExplorer)
library(corrplot)


# Read Dataset


loan_data <- read.csv(
  "loan_book.csv",
  stringsAsFactors = FALSE
)


# Dataset Information


number_observations <- nrow(loan_data)

number_variables <- ncol(loan_data)

train_size <- sum(loan_data$set == "train")

test_size <- sum(loan_data$set == "test")

default_rate <- round(
  mean(loan_data$default_flag == 1) * 100,
  2
)

number_numeric <- sum(
  sapply(loan_data, is.numeric)
)

number_categorical <- sum(
  sapply(
    loan_data,
    function(x)
      is.character(x) || is.factor(x)
  )
)

missing_values <- colSums(
  is.na(loan_data)
)

total_missing <- sum(
  missing_values
)

duplicated_records <- sum(
  duplicated(loan_data)
)


# Variable Lists


numeric_variables <- names(
  loan_data[, sapply(loan_data, is.numeric)]
)

categorical_variables <- names(
  loan_data[, sapply(
    loan_data,
    function(x)
      is.character(x) || is.factor(x)
  )]
)