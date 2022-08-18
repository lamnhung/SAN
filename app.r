library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)

#make a variable for the Interactive Page 1

#make a variable for the Interactive Page 2

#make a variable for the Interactive Page 3 
page3 <- fluidPage(
  titlePanel("Tumor Disparities"), 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "Race",
        label = "Select a Race",
        choices = c("Black", "White", "Other (American Indian/AK Native, Asian/Pacific Islander)"),
      ),
    ),
    mainPanel(
      plotOutput("scatterplot"),
      )
    )
  )


#define the UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage("SAN Final Project",
                 tabPanel(
                   "Introduction",
                   tags$img(src = "IMG_0550.png", height=410, width=650),
                   h1("SEER Breast Cancer Dataset" ),
                   p("The purpose of this project is to explore and find disparities within Breast Cancer patients that disproportionately affect women of color. In the health care field, women of color especially black women are disproportionately mistreated, misdiagnosed, and dying because of inherent bias. It is important to look at the differences between women and color and white women to see if there is any disparities between them. By pointing out these disparities we can acknowledge the inherent bias and racisim within our healthcare system."),
                   h3("Questions We Seek to Answer"),
                   p("Is there a correlation between certain factors such as race, marriage or class status? What are the disparities between certain racial groups? Is one group of people disproportionately affected by breast cancer? Why is this certain group disproportionately affected by cancer?"),
                   h3("Our Data Source"),
                   p("The source of data we are focusing on for this project comes from the SEER Breast Cancer dataset. The SEER Breast Cancer Dataset shows cancer patient's age, race, martial status, stages, grade, tumor size, estrogen/progesterone status, regional node, survival months, and life status (dead or alive). The data is collected by SEER which provides information on population-based cancer statistics. "),
                 ),
                 tabPanel("Interactive Page 1"), 
                 tabPanel("Interactive Page 2"),
                 tabPanel("Interactive Page 3", page3,
                          h2("Average Survival Months in Each Race Group"),
                          plotOutput("bar", click = "plot_click"),
                          verbatimTextOutput("info"),
                          p("The above chart attempts to show the disparity of average survival months in Women of Color compared to White women. We can see that that Black women have the lowest average survival months with breast cancer while White women. Due to the skewness of our data set, it is shown that the other racial group has a higher survival rate but due to the low number of data points on the other racial groups, it is not a significant comparison to make unlike Black women to White Women. Although the dataset has much more data on White women, we can still see the disparity of the black women's survival months being the lowest compared to White women."),
                          h2("Difference in Tumor Stage for Deceased Individuals"),
                          tags$img(src = "Rplot.png", height=410, width=650),
                          p("This chart attempts to show the disparities in tumor stages between deceased Women of color and White women. This chart prevents the deviation that looks at the differences in tumor stages for white women and women of color with respect to if their status shows that they’ve passed  from cancer. A bar that is above 0.00, a positive trend represents that more white women have died compared to women of color for that specified tumor stage. When the bar is below 0.0, a negative trend, it shows that more women of color have died compared to white women for that tumor stage. The chart shows the first three tumor stages more women of color had died compared to white women. The fourth stage shows that more white women have died compared to women of color but that would because most of the cases counted for this dataset died in the early stages causing white women in T4 to be compared to no other variable.
")),
                 tabPanel("Summary",
                          h1("Specific Takeaways"),
                          h2("Disproportionate Data: The Need for Better Data Collection"),
                          p("The SEER's Breast Cancer dataset is skewed, meaning it had a disproportionate data points on Women of color and White women. Although the data was skewed, we still we're able to gain valuble insight of the disparities Women of color who suffer from breast cancer. This pie chart visualization does a better job of showing the skewness of the dataset with most of the observations being predominantly White. This inadequate balance of data collections shows that we still aren't collecting data from marginalized communities adequately. This lack in data representations not only disproprtionately affects them but fails to recognize the help these marginalized communities need. "),
                          tags$img(src = "RplotPie.png", height=300, width=350),
                          h2("Disparities are STILL Present in Disproportionate Data"),
                          p("Although the data is skewed, through our visualizations and charts we can see that even though there were much less data on Women of Color compared to White women affected by breast cancer, there is a large disproportionality seen between the adverse effects of breast cancer in Women of color."),
                          p("For example, in the third interactive page, we can see in the proportions bar chart that Women of color are disproportionately dying from breast cancer from stages T1-T3 compared to white women. Even with such a small data set and skewed data, we can still see how the disparities between Women of color and White women are apprent."),
                          h2("Medical/Healthcare Racism Biases is Real and Present"),
                          p("Through this project one of our main takeaways is that there is still medical/healthcare racism biases still present in the field. The lack of data collection, the misrepresentation and lack of representation downplays marginalized groups such as Women of color who suffer from diseases such as breast cancer. In a small dataset, we can still see that there are persisting racial disparities in our healthcare system today. For example, Women of color are more likely than White women to lack health insurance and have adequate access to healthcare. This lack in universal healthcare not only contributes to the misrepresentation of data but also less diagnosis of Women of Color and hence, higher fatality rates for Women of Color. It is important to address this issue of medical racism so these communities can prevail and provide adequate resources for all. There are many other factors that this dataset doesn't show such as the many different factors such as their environment that gives Women of color a predisposition to developing cancer and therefore dying earlier."),
                          )
))

server <- function(input, output){
  seerbreast <- read_csv("SEER _Breast_Cancer_Dataset.csv")
  survival <- select(seerbreast, Race, "Survival Months", Status)
  combine_survival <- aggregate(survival$"Survival Months", list(survival$Race), FUN=mean)
  survival <- select(seerbreast, Race, "Survival Months", Status)
  combine_survival <- aggregate(survival$"Survival Months", list(survival$Race), FUN=mean)
  race_rename <- rename(combine_survival, Race = Group.1)
  Xrename <- rename(race_rename, "Average Survival Months" = x)
  output$bar <- renderPlot({
  barplot(height=Xrename$"Average Survival Months", names.arg=c("Black", "Other", "White"))
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
  
  output$scatterplot <- renderPlot({
    age_df <- select(seerbreast, Age, Race, "Tumor Size")
    data = switch(
      input$Race, 
      "Black" = subset(age_df, Race == "Black"),
      "White" = subset(age_df), Race == 'White')
    "Other (American Indian/AK Native, Asian/Pacific Islander)" = subset(age_df, Race == "Other (American Indian/AK Native, Asian/Pacific Islander)")
    
  plot(x = age_df$Age, y=age_df$"Tumor Size")
  })
  
}


shinyApp(ui = ui, server = server)

