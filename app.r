library(dplyr)
library(tidyverse)
library(ggplot2)

#make a variable for the Interactive Page 1

#make a variable for the Interactive Page 2

#make a variable for the Interactive Page 3 

#define the UI
ui <- navbarPage("SAN Final Project",
                 tabPanel(
                   "Intro",
                   h1("SEER Breast Cancer Dataset" ),
                   p("This dataset is about....."),
                   p("put more stuff here....")
                 ),
                 tabPanel("Interactive Page 1"), 
                 tabPanel("Interactive Page 2"),
                 tabPanel("Interactive Page 3"),
                 plotOutput("plot"),
                 tabPanel("Summary")
)

sever <- function(input, output){

}

shinyApp(ui = ui, server = sever)

