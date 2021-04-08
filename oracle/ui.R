#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
shinyUI(navbarPage("Data Science Capstone: Course Project",
                   tabPanel("Oracle Word Predictor",
                            HTML("<strong><H1>Author: Werner Eduardo Garcia</H1></strong>"),
                            br(),
                            HTML("<strong><H2>Date: April 8, 2021 </H2></strong>"),
                            br(),
                            # Sidebar
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Enter a partially complete sentence to make oracle star working"),
                                textInput("inputString", "Enter a partial sentence here",value = ""),
                                br(),
                                br(),
                                br(),
                                br()
                              ),
                              mainPanel(
                                h2("Predicted Word"),
                                verbatimTextOutput("prediction"),
                                strong("Sentence Input by user:"),
                                tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
                                textOutput('text1'),
                                br(),
                                strong("Important Note:"),
                                tags$style(type='text/css', '#text2 {background-color: rgba(255,255,0,0.40); color: black;}'),
                                textOutput('text2')
                              )
                            )
                            
                   ),
                   tabPanel("About",
                            mainPanel(
                              
                              includeMarkdown("acerca.Rmd")
                            )
                   )
)
)