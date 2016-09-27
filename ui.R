#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

focusOnUserInput <- "shinyjs.focusOnUserInput = function(){document.getElementById('userInput').focus();}"
#focusOnSelectInput <- "shinyjs.focusOnSelectInput = function(){document.getElementById('selectInput').onmouseover().focus();}"


# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        theme = "bootstrap.cerulean.css",
        includeCSS("www/bootstrap.cerulean.min.css"),
        useShinyjs(),
        extendShinyjs(text = focusOnUserInput),
        
        # Application title
        tabsetPanel(
            tabPanel("Welcome!",
                     titlePanel("You're welcome in the world of Natural Language Processing!")
            ),
            tabPanel("Let's start playing!",
                     titlePanel("Let's start playing!"),
                     hr(),
                     fluidRow(
                         column(8,
                                h3("Text input"),
                                wellPanel(
                                    textInput(inputId = "userInput",  label = "", placeholder = "Type Your text here")
                                ),
                                hr(),
                                h3("Did You mean something of these? - Then select!"),
                                wellPanel(
                                    selectInput(inputId = 'inSelect', label='', choices = c(), 
                                                size = 15, selectize=F, selected = 10000000L, width = "50%")
                                ),
                                hr(),
                                h3("How did we come to these choices?"),
                                wellPanel(
                                    textOutput(outputId = "log")
                                )
                                
                         ),
                         
                         column(4,
                                h3("Configuration on the fly"),
                                wellPanel(
                                        fluidRow(
                                            column(6,
                                                   radioButtons(inputId = 'db_mono', 'List of used words',
                                                                choices = c("small" = 2,"big" = 1),
                                                                selected = 2
                                                   ),
                                                   radioButtons(inputId = 'db_bi', 'DB one-word predictors',
                                                                choices = c("small" = 3,"middle" = 2,"big" = 1),
                                                                selected = 2
                                                   ),
                                                   radioButtons(inputId = 'db_tri', 'DB two-words predictors',
                                                                choices = c("small" = 3,"middle" = 2,"big" = 1),
                                                                selected = 2
                                                   )
                                            ),
                                            column(6,
                                                   radioButtons(inputId = 'db_four', 'DB three-words predictors',
                                                                choices = c("small" = 3,"middle" = 2,"big" = 1),
                                                                selected = 2
                                                   ),
                                                   radioButtons(inputId = 'db_penta', 'DB four-words predictors',
                                                                choices = c("small" = 3,"middle" = 2,"big" = 1),
                                                                selected = 2
                                                   ),
                                                   h6("-----------------"),
                                                   actionButton("reloadDB", "Reload"),
                                                   h6("-----------------")
                                                   
                                            )
                                        )
                                    ),
                                    
                                    hr(),
                                    h3("Intelligence has its price"),
                                    wellPanel(
                                        h4("Execution time of the last prediction, sec."),
                                        h4("Memory actually used, MByte"),
                                        htmlOutput(outputId = "memSize"),
                                        dataTableOutput(outputId = 'statusDB')
                                        
                                    )
                                )
                         )
                     ),
                     tabPanel("System information"
                              
                     ),
                     tabPanel("Log"
                              
                     ),
                     tabPanel("Background concepts"
                              
                     )
                     
            )
        )
    )



