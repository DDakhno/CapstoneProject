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
library(bootstrap)
library(V8)

useShinyjs()
focusOnUserInput <- "shinyjs.focusOnUserInput = function(){document.getElementById('userInput').focus();}"
extendShinyjs(text = focusOnUserInput)

fluidPage(
    
    theme = "bootstrap.cerulean.css",
    includeCSS("www/bootstrap.cerulean.min.css"),
    
    
    
    # Application title
    tabsetPanel(
        tabPanel("Welcome!",
                 titlePanel("You're welcome in the world of Natural Language Processing!"),
                 wellPanel(
                     h2("Imagine...")
                 ),
                 wellPanel(
                     h4("- We are a young software company, excited developing Data Science products."),
                     h4("- Our recent client is an emerging dating portal. The management of the site will have their own implementation of word prediction engine, that 
                        would be used on-line by their clients."),
                     h4("- Sadly, the  customers can't tell much more than \"We want it different and better!\". We could not get a detailed system concept, elaborated requirements 
                        list or use case catalog. By now, we don't know exactly , whether there'll be desktop computers or gadgets using the prediction engine. \"May be, both...\""),
                     h4( "- We were not really surprised. The situation is somehow trivial..."),
                     h4("- Fortunately, we practice agile software development and not the waterfall model. It means, we can start with a bare idea, achieving 
                        perfection in repeating cycles of  planning, analysis, design, coding, unit and acceptance testing."),
                     h4("- So, we rolled up sleeves and started with the project. We had to use the toolbox of Natural Language Processing."),
                     h4("- We decided on analyzing the n-gram models of the vivid language as seen in Internet. We have taken the text corpus called HC Corpora, representing the real world English from blogs, news and Twitter messages. The language is nothing but regular, accurate English. So, we've invested a lot of time
                        and efforts in exploration and preparing tidy data bases."), 
                     h4("- Time and again we had to make assumptions and take decisions that are yet to be validated externally. Just as an example, we decided to respect only regular words (i.e. consisting only of letters) and hashtags."),
                     h4("- After four weeks of hard work it is time now to pick up our customers. We expect them to test our prediction engine with respect to language and technical aspects."),
                     h4("- So, we place now our TestBoard on-line at the disposal of our customers. It is not an ultimate product, but the tool for testing and finding the path to perfection.")
                     
                 ),
                 wellPanel(
                     h2("We are proud about the work done and invite You our playground, the TestBoard. Give it a try just now!")
                     
                 )
        ),
        tabPanel("TestBoard",
                 titlePanel("Let's start playing!"),
                 hr(),
                 fluidRow(
                     column(8,
                            h3(
                                "Text input"),
                            wellPanel(
                                textInput(inputId = "userInput",  label = "", placeholder = "Type Your text here")
                            ),
                            
                            hr(),
                            h3("Did You mean something of these? - Then select!"),
                            wellPanel(
                                selectInput(inputId = 'inSelect', label='', choices = c(), 
                                            
                                            size = 15, selectize=F,  width = "50%")
                            ),
                            hr(),
                            h3("How did we come to these choices?"),
                            
                            wellPanel(
                                actionButton('saveLog', 'Save the log'),
                                selectInput(inputId = 'LOG', label='', choices = c(), 
                                            size = 15, selectize=F,  multiple = T)
                            )
                            
                     ),
                     
                     column(4,
                            h3("Configuration on-the-fly"),
                            
                            wellPanel(
                                fluidRow(
                                    column(6,
                                           radioButtons(inputId = 'db_mono', 'DB used words',
                                                        
                                                        choices = c("small" = 3,"middle" = 2,"big" = 1),
                                                        selected = 3
                                           ),
                                           
                                           radioButtons(inputId = 'db_bi', 'DB one-word predictors',
                                                        choices = c("small" = 3,"middle" = 2,"big" = 1),
                                                        selected = 3
                                                        
                                           ),
                                           radioButtons(inputId = 'db_tri', 'DB two-words predictors',
                                                        choices = c("small" = 3,"middle" = 2,"big" = 1),
                                                        selected = 3
                                           )
                                    ),
                                    column(6,
                                           
                                           radioButtons(inputId = 'db_four', 'DB three-words predictors',
                                                        choices = c("small" = 3,"middle" = 2,"big" = 1),
                                                        
                                                        selected = 3
                                           ),
                                           radioButtons(inputId = 'db_penta', 'DB four-words predictors',
                                                        
                                                        choices = c("small" = 3,"middle" = 2,"big" = 1),
                                                        selected = 3
                                           ),
                                           h6("-----------------\
                                              "),
                                           actionButton("reloadDB", "Reload"),
                                           h6("-----------------")
                                           
                                           
                                    )
                                )
                            ),
                            
                            hr(),
                            h3("Language intelligence has its price"),
                            
                            wellPanel(
                                h4("Execution time on server, last prediction, msec."),
                                textOutput( outputId = 'execTime'),
                                h4("Network round trip time, msec."),
                                fluidRow(
                                    column(7,
                                           textOutput(outputId='roundTrip')
                                    ),
                                    
                                    column(5,
                                           actionButton(inputId='pingShiny', label = 'Refresh')
                                    )
                                ),
                                h4
                                ("Memory actually used, MByte"),
                                htmlOutput(outputId = "memSize"),
                                dataTableOutput(outputId = 'statusDB')
                                
                                
                            )
                     )
                 )
        ),
        tabPanel("Short user manual",
                 wellPanel(
                     h3("General information"),
                     h5("You are dealing wit
                        h the site aimed at the purposes of remote testing. This is on-line front-end for the working prediction engine running at an 
                        Internet server. It means, Your input will be transferred over multiple hops in network to the prediction engine. The packages with suggestions follow the path backwards to You. It might be some longer way, so we would ask You to be indulgent. 
                        You'll see the actual execution times on the server."),
                     h5("The TestBoard is a prototype, so it is a really simple front-end, without glamour and a lot of gimmicks. We have tested some ideas like capitalization at the beginning of sentences, but it doesn't really work by now. One more time, be indulgent.")
                 ),
                 wellPanel(
                     h3("Making input"),
                     h5("1. Go to the tab \"TestBoard\""),
                     
                     h5("2. Start typing under \"Text input\""),
                     h5("3. Each change in the text input (even adding or removing of the blank) initiates the request toward prediction engine."),
                     h5("4. Within a split second the suggestions appear in the selection area beneath Your input."),
                     h5("5. You may click on the matching suggestion to add it to the line being typed.")
                 ),
                 
                 wellPanel(
                     h3("Reading technical information"),
                     h5("1. At the TestBoard tab in the right lower corner You can see the execution time of the last request."),
                     h5("2. At the same place You can see the size in memory for the loaded prediction data banks."),
                     h5("3. At the tab \"System information\" You can get the general system information and actual resource consumption from the viewpoint 
                        of operational system. The resource consumption information will be updated after each request."),
                     h5("4. You may see the network round-trip times in the technical corner. With the but
                        ton [Refresh] You can recheck them as needed.")
                 ),
                 wellPanel(
                     h3("Reconfiguring on-the-fly"),
                     h5("Tester has the option to test prediction engine with n-g
                        ram data banks of different size."),
                     h5("1. Middle size will be set at start as default for all data banks."),
                     h5("2. Click on the proper radio button to load data bank of another size."),
                     
                     h5("3. For the first time after start click on the button [Reload] and wait till the memory size beneath is updated."),
                     h5("4. All the following reloads will be triggered through activation of radio button itself.")
                 ),
                 wellPanel(
                     h3("Logging"),
                     h5("1. After each request the sequence of the steps needed for prediction will be shown in the text area in the lower
                        left corner."),
                     h5("2. Optionally, the log may be saved into a file by pressing the button [Save log]")
                     
                 )
        ),
        tabPanel("System information",
                 
                 wellPanel(
                     h3("General system information"),
                     htmlOutput(outputId='generalInfo')
                 ),
                 wellPanel(
                     h3("Actual resource 
consumption"),
                     htmlOutput(outputId='consumption')
                 )
        ),
        tabPanel("Background concepts",
                 
                 wellPanel(
                     h3("Starting with questions"),
                     htmlOutput(outputId="questions")
                 ),
                 wellPanel(
                     h3("Initial expectations and assumptions"),
                     htmlOutput(outputId="assumptions")
                 ),
                 
                 wellPanel(
                     h3("Preliminary text corpus analysis and preprocessing the data"),
                     htmlOutput(outputId="preprocessing")
                 ),
                 
                 wellPanel(
                     h3("Exploration data analysis"),
                     htmlOutput(outputId="eda")
                 ),
                 
                 
                 wellPanel(
                     h3("Study design and concepts for prediction engine"),
                     htmlOutput(outputId="concepts")
                 )
                 
        ) 
    )
)