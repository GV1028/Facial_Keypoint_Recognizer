library(shiny)
shinyUI(fluidPage(fluidPage(theme="boot.css",
                  headerPanel("Facial Keypoint Detection"),
                  sidebarPanel(
                               fileInput('inimage',"Upload the Image"),
                               wellPanel(h5("Please Wait, this might take a few minutes."))),
                          
                  mainPanel(
                   plotOutput("image")
                  ))
))