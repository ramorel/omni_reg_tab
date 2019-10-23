#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Upload data file",
                      multiple = TRUE,
                      accept = c(".RData",
                                 ".rda",
                                 ".rds",
                                 ".dta")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            # Horizontal line ----
            tags$hr(),
            
            uiOutput("iv"),
            
            # Horizontal line ----
            tags$hr(),
            
            uiOutput("dvs"),
            
            
            # Horizontal line ----
            tags$hr(),
            selectInput("model", "Select your model(s)",
                        choices = 
                            c("Choose model",
                              "OLS",
                              "GLM - binomial",
                              "GLM - poisson",
                              "Fixed effects",
                              "Random effects")),
            
            # Horizontal line ----
            tags$hr(),
            
            conditionalPanel(
                condition = "input.model == 'Fixed effects'",
                uiOutput("entity_effects"),
                uiOutput("time_effects"))
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tableOutput("contents"),
            conditionalPanel(
                condition = "input.model != 'Choose model'",
                textOutput("formula")
                )
            
        )
        
    )
)


# Define server logic to read selected file ----
server <- function(input, output) {
    
    df <- reactive({
        req(input$file1)
        
        if (str_detect(input$file1$datapath, ".dta")) {
            
            df <- haven::read_dta(input$file1$datapath)
            
        } else {
            
            df <- readRDS(input$file1$datapath)
            
        }
    })
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        if(input$disp == "head") {
            return(head(df()))
        }
        else {
            return(df())
        }
        
    })
    
    output$dvs <- renderUI({
        selectInput("dvs","Select your dependent variables",
                    multiple = TRUE,
                    choices = names(df()))
    })
    
    output$iv <- renderUI({
        selectInput("iv","Select your independent variable",
                    choices = c("Independent variable", names(df())))
    })
    
    output$entity_effects <- renderUI({
        conditionalPanel(
            condition = "input.model %in% c('Fixed effects', 'Random effects')",
            selectInput("entity_effects", "Select entity fixed effects",
                        choices = c("None", names(df()))))
    }) 
    
    output$time_effects <- renderUI({
            selectInput("time_effects", "Select time fixed effects",
                        choices = c("None", names(df())))
    })
    
    output$formula <- renderText({
        
        if (input$model == "Fixed Effects") {
            
            if (input$entity_effects != "None" & input$entity_effects != "None") {
            
                fes <- paste(input$entity_effects, " + ", input$time_effects) 
            
            }
            
            else if (input$entity_effects != "None" & input$entity_effects == "None") {
                
                fes <- input$entity_effects
                
            }
            
            else {
                
                fes <- input$time_effects
            }
            
            paste("Formula: ", input$iv, "~", paste(input$dvs, collapse = " + "), " | ", fes)
            
        }
        
        else {
         
            paste("Formula: ", input$iv, "~", paste(input$dvs, collapse = " + "))
            
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
