library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinythemes)

#make a variable for the Interactive Page 1

#make a variable for the Interactive Page 2

#make a variable for the Interactive Page 3 

#define the UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage("SAN Final Project",
                 tabPanel(
                   "Introduction",
                   tags$img(src = "IMG_0550.png", height=360, width=600),
                   h1("SEER Breast Cancer Dataset" ),
                   p("The purpose of this project is to explore and find disparities within Breast Cancer patients that disproportionately affect women of color. In the health care field, women of color especially black women are disproportionately mistreated, misdiagnosed, and dying because of inherent bias. It is important to look at the differences between women and color and white women to see if there is any disparities between them. By pointing out these disparities we can acknowledge the inherent bias and racisim within our healthcare system."),
                   h3("Questions We Seek to Answer"),
                   p("Is there a correlation between certain factors such as race, marriage or class status? What are the disparities between certain racial groups? Is one group of people disproportionately affected by breast cancer? Why is this certain group disproportionately affected by cancer?"),
                   h3("Our Data Source"),
                   p("The source of data we are focusing on for this project comes from the SEER Breast Cancer dataset. The SEER Breast Cancer Dataset shows cancer patient's age, race, martial status, stages, grade, tumor size, estrogen/progesterone status, regional node, survival months, and life status (dead or alive). The data is collected by SEER which provides information on population-based cancer statistics. "),
                 ),
                 tabPanel("Interactive Page 1"), 
                 tabPanel("Interactive Page 2"),
                 tabPanel("Interactive Page 3"),
                 plotOutput("plot"),
                 tabPanel(
                   "Summary",
                          h1("Specific Takeaway 1"),
                          p("Insert Specific Takeaway 1"),
                          h2("Notable Data-Insight/Pattern 1"),
                          p("Insert Notable Data-Insight/Pattern 1"),
                          p("Insert Broader Implication(s) 1"),
                          h1("Specific Takeaway 2"),
                          p("Insert Specific Takeaway 2"),
                          h2("Notable Data-Insight/Pattern 2"),
                          p("Insert Notable Data-Insight/Pattern 2"),
                          p("Insert Broader Implication(s) 2"),
                          h1("Specific Takeaway 3"),
                          p("Insert Specific Takeaway 3"),
                          h2("Notable Data-Insight/Pattern 3"), 
                          p('Insert Notable Data-Insight/Pattern 3'),
                          p("Insert Broader Implication(s) 3"),
                          )
))

sever <- function(input, output){

}

shinyApp(ui = ui, server = sever)

