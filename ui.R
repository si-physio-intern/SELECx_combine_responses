### Combine response from SELECx

library(shiny)
library(shinyFeedback)
library(tidyverse)
library(vroom)
library(tools)
library(glue)
library(readxl)
library(openxlsx)


# UI ----------------------------------------------------------------------



ui <- fluidPage(
    
    shinyFeedback::useShinyFeedback(),
    
    titlePanel("SELECx - Combine Responses"),
    hr(),
    
    fluidRow(
        column(8,
               
               helpText("1",tags$sup("st"),": Download .csv or .xlsx from SELECx, Rename that files to English (Short names)"),
               helpText("2",tags$sup("nd"),": Prepare student's ID file that has student's id, student's name
           as column names:\'ID',\'Name' respectively"),
           
           helpText("3",tags$sup("rd"),": Upload Quiz from SELECx that has column \'Response' (usually Essay quiz)"),
           
           fileInput("file", NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload files",
                     placeholder = "choose file .csv or .xlsx",multiple = TRUE)
           
        ),
        
        column(4,
               downloadButton("download", "Download Data .xlsx"),
               downloadButton("download_2", "Download Count .xlsx")
        )
        
    ),
    
    fluidRow(
        column(6,
               helpText("4",tags$sup("th"),": Upload student's ID file and choose more column if you want"),
               fileInput("file_id", NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload ID",
                         placeholder = "choose file .csv or .xlsx"),
               
        ),
        
        column(4,offset = 2,
               checkboxInput("add_cols","Add more column from ID file ?",value = FALSE),
               uiOutput("select")
               
               
        )
        
    ),
    hr(),
    
    h3("Data"),
    br(),
    dataTableOutput("table"),
    
    hr(),
    
    h3("Responses Count"),
    helpText("Algorithm counts number of answered \'Response' of each student. 
           Dash response (-) will be 0. No record from SELECx will be blank."),
    helpText("If multiple attempt is detected, algorithm counts the maximum responses of each students."),
    br(),
    dataTableOutput("table_2"),
    
    hr(),
    h3("Missing name or No response"),
    dataTableOutput("missing")
    # verbatimTextOutput("raw"),
    # verbatimTextOutput("raw_2"),
)

