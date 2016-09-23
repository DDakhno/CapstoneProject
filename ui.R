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
                                hr(),
                                
                                wellPanel(
                                    selectInput(inputId = 'inSelect', label='', choices = c(), 
                                                size = 15, selectize=F, selected = 10000000L)
                                ),
                                hr(),
                                h3("How did we come to these choices?"),
                                wellPanel(
                                    textOutput(outputId = "log")
                                )
                                
                         ),
                         
                         column(4,
                                h2("Configuration on the fly"),
                                wellPanel(
                                    selectInput('x', 'Database of four-words-predictors', c("small","middle","big")),
                                    selectInput('x', 'Database of three-words-predictors', c("small","middle","big")),
                                    selectInput('x', 'Database of two-words-predictors', c("small","middle","big")),
                                    selectInput('x', 'Database of one-word-predictors', c("small","middle","big")),
                                    checkboxInput('jitter', 'Something')
                                ),
                                hr(),
                                h3("Intelligence has its price"),
                                wellPanel(
                                    h4("Execution time of the last prediction, sec."),
                                    h4("Memory actually used, MByte")
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


