library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)

seerbreast <- read_csv("SEER _Breast_Cancer_Dataset.csv")

deathrates <- read.csv("deathrates.csv")
deathrates <- subset(deathrates, select=-c(X, X.1, X.2))

mutated_seer_df <- seerbreast %>% 
  mutate(White_NonWhite = 
           case_when(`Race` %in% c("Other (American Indian/AK Native, Asian/Pacific Islander)",
                                   "Black") ~ "WOC",
                     `Race` %in% c("White") ~ "White"
                     
           )
  )

mutated_seer_df <- mutated_seer_df %>% 
  mutate(Age_Groups = 
           case_when(`Age` %in% c("30", "31", "32", "33", "34", "35", "36", 
                                  "37", "38", "39") ~ "Thirties",
                     `Age` %in% c("40", "41", "42", "43", "44", "45", "46",
                                  "47", "48", "49") ~ "Fourties",
                     `Age` %in% c("50", "51", "52", "53", "54", "55", "56",
                                  "57", "58", "59") ~ "Fifties", 
                     `Age` %in% c("60", "61", "62", "63", "64", "65", "66",
                                  "67", "68", "69") ~ "Sixties"
           )
  )



mutated_seer_df %>% with(table(White_NonWhite, `T Stage`)) 

#,mutated_seer_df %>% with(table(White_NonWhite, Status)) %>% prop.table(margin=1)

mutated_seer_df %>% with(table(White_NonWhite, Age_Groups)) %>% prop.table(margin=1)

age_rates <- mutated_seer_df %>% 
  with(table(White_NonWhite, `Age_Groups`)) %>% prop.table(margin=1)
ggplot(as.data.frame(age_rates), aes(x=Age_Groups, y=Freq, fill=White_NonWhite)) +
  geom_bar(stat = "identity", position = "dodge") + ggtitle("Age of Diagnosis for White Women and Women of Color") + xlab("Age Groups") + ylab("Total Frequency")


#make a variable for the Interactive Page 1

#make a variable for the Interactive Page 2

#make a variable for the Interactive Page 3 
page3 <- fluidPage(
  titlePanel("Breast Cancer Death Rates By State"), 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "state",
        label = "Select a State:",
        c(deathrates$State),
      ),
    ),
    mainPanel(plotOutput("barchar")),
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
                   p("The main source of data we are focusing on for this project comes from the SEER Breast Cancer dataset that is found on Kaggle (https://www.kaggle.com/datasets/asdsadasdsadsa/seer-breast-cancer-dataset). The SEER Breast Cancer Dataset shows cancer patient's age, race, martial status, stages, grade, tumor size, estrogen/progesterone status, regional node, survival months, and life status (dead or alive). The data is collected by SEER which provides information on population-based cancer statistics. A secondary data source we will be using is the SEER Cancer Statistics Review (CSR) 1975-2018 (https://seer.cancer.gov/archive/csr/1975_2018/browse_csr.php?sectionSEL=1&pageSEL=sect_01_table.20) which shows many different cancer type statistics but we will only be focusing on the breast cancer statistics. This specific dataset displays the death rate per 100,000 per race group."),
                 ),
                 tabPanel("Interactive Page 1"), 
                 tabPanel("Interactive Page 2"), 
                 tabPanel("Interactive Page 3", page3,
                          p("The above interactive chart displays the Age vs Tumor Sizes for each racial group."),
                          h2("Average Breast Cancer Death Rate per 100,000 in Each Race Group"),
                          plotOutput("bar", click = "plot_click"),
                          verbatimTextOutput("info"),
                          p("The above chart attempts to show the disparity of average death rates of breast cancer in WOC, especially black women. We can see the huge discrepency between the death rate of white and black women for breast cancer. This is especially important to notice that although it seems that the other WOC groups seem lower than White women, we need to remember the density of each population. White women are far more abundanct compared to the other racial groups yet, black women die of breast cancer at much higher rates. This should be concerning becuase this suggest that they are being disproportionately affected. Point and click on the bar chart to reveal more detailed values of the death rate per 100,000. The detailed values will appear at the y = the death rate."),
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
  
  cancer <- read.csv("canceractual.csv")
  cancer <- subset(cancer, select=-c(X, X.1, X.2, X.3, X.4))
  output$bar <- renderPlot({
    barplot(height=cancer$"Death.Rate", names.arg=c("Total", "White", "Black", "AI/AK", "API", "Hispanic", "W-Hispanic"))
  })
  data <- reactive({
    req(input$state)
    group_by(Race)
  })
  output$barchar <- renderPlot({
    g <- ggplot(data(), aes(y=Death.Rate, X = Race))
  })
}

shinyApp(ui = ui, server = server)

