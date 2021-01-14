#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
shinyUI(navbarPage("Capstone: Course Project",
        tabPanel("Predict the Next Word",
            # Sidebar
            sidebarLayout(
                sidebarPanel(
                    helpText("Enter a partially complete sentence to begin the next word prediction"),
                    textInput("inputString", "Enter a partial sentence here",value = ""),
                    br(),
                    br(),
                    br(),
                    br()
                ),
                mainPanel(
                    h2("Predicted Next Word"),
                    verbatimTextOutput("prediction"),
                    strong("Sentence Input:"),
                    tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
                    textOutput('text1'),
                    br(),
                    strong("Note:"),
                    tags$style(type='text/css', '#text2 {background-color: rgba(255,255,0,0.40); color: black;}'),
                    textOutput('text2')
                )
            )       
        )
    )
)