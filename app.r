library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinythemes)
library(usmap)


seerbreast <- read_csv("seerbreast.csv")

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
                                  "37", "38", "39") ~ "30-39",
                     `Age` %in% c("40", "41", "42", "43", "44", "45", "46",
                                  "47", "48", "49") ~ "40-49",
                     `Age` %in% c("50", "51", "52", "53", "54", "55", "56",
                                  "57", "58", "59") ~ "50-59", 
                     `Age` %in% c("60", "61", "62", "63", "64", "65", "66",
                                  "67", "68", "69") ~ "60-69"
           )
  )

tstage_rate <- mutated_seer_df %>% 
  with(table(White_NonWhite, `T Stage`)) %>% prop.table(margin=1)
ggplot(as.data.frame(tstage_rate), aes(x= T.Stage, y=Freq, fill=White_NonWhite)) +
  geom_bar(stat = "identity", position = "dodge") + ggtitle("Tumor Stages for White Women and Women of Color") + xlab("Tumor Stage") + ylab("Total Frequency")

mutated_seer_df %>% 
  group_by(White_NonWhite, `T Stage`) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(White_NonWhite) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(p = n/total) %>% 
  select(-c(n, total)) %>% 
  pivot_wider(names_from = White_NonWhite, values_from = p) %>% 
  mutate(difference = White - WOC) %>% 
  ggplot(aes(x = `T Stage`, y = difference)) +
  geom_col() + ggtitle("Difference in Tumor Stage for Women of Color and White Women") + xlab("Tumor stage") + ylab("Difference")

age_rates <- mutated_seer_df %>% 
  with(table(White_NonWhite, `Age_Groups`)) %>% prop.table(margin=1)
ggplot(as.data.frame(age_rates), aes(x=Age_Groups, y=Freq, fill=White_NonWhite)) +
  geom_bar(stat = "identity", position = "dodge") + ggtitle("Age of Diagnosis for White Women and Women of Color") + xlab("Age Groups") + ylab("Total Frequency")

deathrates_df <- read.csv("cancer.csv")
death_df <- select(deathrates_df, State, Types.Breast.Total, Types.Breast.Race.White, 
                   Types.Breast.Race.White.non.Hispanic., Types.Breast.Race.Black, 
                   Types.Breast.Race.Black.non.Hispanic, Types.Breast.Race.Asian, 
                   Types.Breast.Race.Indigenous, Types.Breast.Race.Hispanic)
cancer <- death_df %>% 
  pivot_longer(2:9, names_to = "Race", values_to = "Rate") %>% 
  mutate(Race = str_sub(Race, start = 14, end  = -1),
         Race = ifelse(Race != "Total",
                       str_sub(Race, 6, -1),
                       Race),
         Race = str_replace_all(Race, "\\.", " "),
         Rate = ifelse(Rate == 0, 
                       NA_real_,
                       Rate)) %>% 
  rename(state = State )

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage("SAN Final Project",
             tabPanel(
               "Introduction",
               tags$img(src = "IMG_0550.png", height=350, width=650),
               h1("SEER and CORGIS Breast Cancer Dataset" ),
               p("The purpose of this project is to explore and find disparities within Breast Cancer patients that disproportionately affect women of color. In the health care field, women of color especially black women are disproportionately mistreated, misdiagnosed, and dying because of inherent racists ideals/biases. It is important to look at the differences between women and color and white women to see if there is any disparities between them. By pointing out these disparities we can acknowledge the inherent bias and racisim within our healthcare system. By addressing these disparities, this can lead to better healthcare access, policy, treatment, etc for marginalized groups."),
               h3("Questions We Seek to Answer:"),
               h4("Our Main Question: What are the disparities Women of Color face within and becuase of the healthcare field?"),
               p("Is there a correlation between certain factors such as race and cancer status? What are the disparities between Women of color and White women? Is one group of people disproportionately affected by breast cancer? Why is this certain group disproportionately affected by cancer?"),
               h3("Our Data Source"),
               p("The source of data we are focusing on for this project comes from the SEER Breast Cancer dataset (https://www.kaggle.com/datasets/asdsadasdsadsa/seer-breast-cancer-dataset). The SEER Breast Cancer Dataset shows cancer patient's age, race, martial status, stages, grade, tumor size, estrogen/progesterone status, regional node, survival months, and life status (dead or alive). The data is collected by SEER which provides information on population-based cancer statistics. Another dataset we looedk at was CORGIS (https://corgis-edu.github.io/corgis/csv/cancer/) datasets which displays mutlpe different types of cancer and it's death rate. In our project, we filtered down the dataset just to present data on breast cancer."),
             ),
             tabPanel("Tumor Stage Disparities",
                      h3("Deceased WOC vs White Women by Tumor Stages"),
                      tags$img(src = "Rplot.png", height=410, width=650), 
                      p("The above chart is a deviation plot that looks at the difference in tumor stages for white women and women of color with respect to if their status is deceased from breast cancer. To interpret this chart, when the bar is positive (above 0), it means white women have died of breast cancer more than compared to women of color for that certain tumor stage.
When the bar is negative (below 0) it shows that more women of color have died compared to white women for that tumor stage. This chart attempts to show that for the 
first three tumor stages more women of color had died compared to white women. The fourth stage shows that more white women have died compared to women of color
but that would most likely be because most of the cases counted for this dataset died in the early stages causing white women in T4 to be compared to no other variable. We can see that WOC tended to die much earlier/survived less that White women. "),
                      
                      p("The interactive chart below depicts a grouped bar chart that shows the frequency of women of color and White women for each tumor stage. When you select a radio button, more information about the certain tumor stage appears. This is an important chart that shows how deadly cancer can be which can be seen in the chart below. Stage T4 is the highest stage that means the tumor has developed into an inflammatory carcinoma. We can see the frequncy decreases over the stages as either people are cured or people unfortunately die from breast cancer. Select each radio label to read more information about the bar plot. "),
                      radioButtons(inputId = "tumorstage",
                                   label = "Select a Tumor Stage for  More Information",
                                   choices = list("T1", "T2", "T3", "T4"),
                                   selected = "T1"),
                      mainPanel(
                        plotlyOutput(outputId = "barchart"),
                        textOutput(outputId = "tumortalk")
                      )
             ),
             tabPanel("Age of Diagnosis",
                      h3("Disparities in Age Diagnosis"),
                      p("This chart displays the the average of diagnosis vs WOC/White Women. This chart attempts to show the disparity that there tends to be more misdiagnosis/mistreatment of WOC. We can see this slighty as the diagnosis of White women increases with age but it actually decreases with age with WOC. This disparity is common in healthcare that WOC are overexaggerating, yet White Women's pain/illness/diseases are more easily believed."),
                      sidebarLayout(position = "left",
                                    sidebarPanel(style = "background:black",
                                                 wellPanel(style = "background: White",
                                                           selectInput(inputId = "Race",
                                                                       label = "Select Racial category", 
                                                                       list("Both","White", "WOC")),
                                                           helpText("The chart on the right depicts the average age of diagnosis of breast cancer between WOC and White Women. We can see the average age of diagnosis increases as age went for White women up while the average diagnosis decreases age for WOC")
                                                 ),
                                                 
                                    ),
                                    mainPanel(
                                      #textOutput(outputId = "userNum"),
                                      #tableOutput(outputId = "test"),
                                      plotlyOutput(outputId = "barplot")
                                    )
                      )
             ),
             tabPanel("Death Rates for Breast Cancer",
                      h3("Average Breast Cancer Death Rate per 100,000 in Each Racial Group"),
                      tags$img(src = "avgBC.png", height=410, width=750),
                      p("The above chart displays the Average breast cancer death rate in different racial groups. This data is from the US and SEER data that shows the cancer death rate per 100,000. We can see that Black women have the highest deeath rate compared to any other group. Below this chart is our interactive chart which displays the breast cancer death rates per 100,000 in each state categorized by the racial groups: White, Black, Black Non-Hispanic, , Asian, Indigenous, and Hispanic. The purpose of this interactive chart is to show the states where certain racial groups are dying of breast cancer disproportionately" ),
                      p("The below interactive map/chart attempts to display the disproportionately affected marginalized groups in sepcific states known for a lack of healthcare or tend to be larger in one population of the other. For example states with larger Indigenous populations such as South Dakota see much higher death rates than any other group. One data trend that stands out the most is that the Black racial group's death rate is much higher than most other racial groups. Although the White population is much bigger, this trend in data suggest that WOC, especially Black women are being disproportionately affected by cancer."),
                      h3("Interactive Map"),
                      sidebarLayout(position = "left",
                                    sidebarPanel(style = "background:black",
                                                 wellPanel(style = "background: White",
                                                           selectInput(inputId = "racegroup",
                                                                       label = "Select Racial category", 
                                                                       list("Total","White", 
                                                                            "Black", "Black non Hispanic",
                                                                            "Asian", "Indigenous", "Hispanic")),
                                                           p("By selecting a certain racial group, the interactive map will display the corresponding data and presents a US map of the breast cancer death rates per 100,000 for the selected race for each state."),
                                                 )
                                    ),
                                    mainPanel(
                                      plotlyOutput(outputId = "usmap")
                                    )
                      )
             ),
             tabPanel("Summary",
                      h1("Specific Takeaways"),
                      h2("Disproportionate Data: The Need for Better Data Collection"),
                      p("The SEER's Breast Cancer dataset is skewed, meaning it had a disproportionate data points on Women of color and White women. Although the data was skewed, we still we're able to gain valuble insight of the disparities Women of color who suffer from breast cancer. The pie chart visualization below does a better job of showing the skewness of the dataset with most of the observations being predominantly White. This inadequate balance of data collections shows that we still aren't collecting data from marginalized communities adequately. This lack in data representations not only disproprtionately affects them but fails to recognize the help these marginalized communities need. Although the CORGIS dataset was much larger than the SEER's dataset, when we look at certain racial groups, some states are greyed out which means that there was no cancer death rate data available. Missing data points like this don't properly represent some of these racial groups. By missing so much data, we can misinterpret/misrepresent these marginalized communities that are being disproportionately affected by not only by breast cancer but many other diseases. We can see on the interactive map that many areas are greyed out for Indigenous populations. Almost half the US map is greyed out because of the lack of data. This lack of data can be very harmful for the indigenous populations as they'll be underrepresented in need to take action for better healthcare/treatment "),
                      tags$img(src = "RplotPie.png", height=300, width=350),
                      h2("Disparities are STILL Present in Disproportionate Data"),
                      p("Although the data is skewed, through our visualizations and charts we can see that even though there were much less data on Women of Color compared to White women affected by breast cancer, there is a large disproportionality seen between the adverse effects of breast cancer in Women of color. Even in the map example, the greyed out areas are missing very important data but when we look at states with high populations of the particualr marginalized group, they tend to be the one that's disproportionately affected. Another chart that displays this is the disproportionate diagnosis of WOC and White Women. The data/trend suggest that the common healthcare bias is that WOC exaggerate their pain and in result don't receive as accurate diagnosis as White Women"),
                      p("For example, in the first interactive page, we can see in the proportions bar chart that Women of color are disproportionately dying from breast cancer from stages T1-T3 compared to white women. Even with such a small data set and skewed data, we can still see how the disparities between Women of color and White women are apprent."),
                      h2("Medical/Healthcare Racism Biases is Real and Present"),
                      p("Through this project one of our main takeaways is that there is still medical/healthcare racism biases still present in the field. The lack of data collection, the misrepresentation and lack of representation downplays marginalized groups such as Women of color who suffer from diseases such as breast cancer. In a small dataset, we can still see that there are persisting racial disparities in our healthcare system today. For example, Women of color are more likely than White women to lack health insurance and have adequate access to healthcare. This lack in universal healthcare not only contributes to the misrepresentation of data but also less diagnosis of Women of Color and hence, higher fatality rates for Women of Color. It is important to address this issue of medical racism so these communities can prevail and provide adequate resources for all. There are many other factors that this dataset doesn't show such as the many different factors such as their environment that gives Women of color a predisposition to developing cancer and therefore dying earlier. Using our map example once again, we can see that ccancer death rates are higher in states that have low accessibility to adequate health care such as many states in the south. We can see the higher trend of cancer death rates per 100,000 accumulate more darkly in the southern states. Places with stringent laws against adequate health care not only negatively affects marginalized groups but also pushes the idea that they don't need it, when they are the one's who need it the most. This relates to the interactive chart 2 where we looked at the diagnosis of White women vs WOC, which we saw an increase for White women but decrease for WOC as age increased. This is just one example of how our healthcare systems still hold racist beliefs/values"),
             )
  ))

server <- function(input, output){
  output$barplot <- renderPlotly({
    if(input$Race == "Both"){
      ggplot(as.data.frame(age_rates), aes(x=Age_Groups, y=Freq, fill=White_NonWhite)) +
        geom_bar(stat = "identity", position = "dodge") + 
        labs(x="Tumor Stage", y="Total Frequency",
             title = "Age of Diagnosis for White women and Women of Color",
             fill = "Race")
    } else if (input$Race == "White"){
      filter_df <- filter(as.data.frame(age_rates), White_NonWhite == "White")
      ggplot(data = filter_df, aes(x=Age_Groups, y=Freq)) + 
        geom_bar(stat = "identity", position = "dodge", fill = "coral1") + 
        ggtitle("Age of Diagnosis for White Women and Women of Color") + 
        xlab("Age Groups") + ylab("Total Frequency")
    }
    else{
      filter_df <- filter(as.data.frame(age_rates), White_NonWhite == "WOC")
      ggplot(data = filter_df, aes(x=Age_Groups, y=Freq)) + 
        geom_bar(stat = "identity", position = "dodge", fill = "cadetblue2") + 
        ggtitle("Age of Diagnosis for White Women and Women of Color") + 
        xlab("Age Groups") + ylab("Total Frequency")
    }
  }) 
  
  output$barchart <- renderPlotly({
    ggplot(as.data.frame(tstage_rate), aes(x= T.Stage, y=Freq, fill=White_NonWhite)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Tumor Stage", y = "Total Frequency",
           title = "Tumor Stages for White Women and Women of Color",
           fill = "Race")
  })
  
  output$tumortalk <- renderText(
    if (input$tumorstage == "T1"){
      return("Tumor stage one (T1) describes invasive breast cancer; the cancer cells are breaking through to or invading normal surrounding breast tissues. Stage I is divided into two subcategories: IA and IB. 

Breast cancer in the first tumor stage (T1) is invasive; the cancer cells have damaged or invaded the normal surrounding breast tissues. IA and IB are the two subgroups of Stage 1. 

Stage IA often refers to invasive breast cancer with a tumor up to 2 cm in size, no lymph nodes affected, and no external spread of cancer outside the breast. Invasive breast cancer is also referred to as stage IB when there is no tumor in the breast but instead, tiny clusters of cancer cells measuring 0.2-2 millimeters are discovered in the lymph nodes. Additionally, if there are little clusters of cancer cells in the lymph nodes that are 0.2-2 millimeters in size, there may be a tumor in the breast that is less than 2 cm in size. This stage is more common which makes sense in the chart because the frequency difference between WOC and White women is not that large.  ")
    } else if (input$tumorstage == "T2"){
      return("Tumor stage two (T2 ) describes invasive breast cancer; the cancer cells are breaking through to or invading normal surrounding breast tissues. Stage 2 is divided into two subcategories: IIA and IIB. 

Invasive breast cancer is classified as stage IIA if there is no detectable tumor in the breast but cancer larger than 2 millimeters is discovered in one to three axillary lymph nodes (under the arm) or in the lymph nodes close to the breast bone during a sentinel node biopsy. The axillary lymph nodes can be affected by the tumor, which has a diameter of less than 2 cm. The tumor may also range in size from 2 to 5 cm and have not spread to the axillary lymph nodes. Additionally, Invasive breast cancer is classified as stage IIB if the tumor is between 2 and 5 centimeters in size or between 0.22 and 2.25 millimeters in the lymph nodes, between 2 and 5 centimeters in size and the cancer has spread to 1-3 axillary lymph nodes or to lymph nodes close to the breastbone discovered during a sentinel node biopsy, or if the tumor is greater than 5 centimeters in size but has not yet reached the axillary lymph nodes. ")
    } else if (input$tumorstage == "T3"){
      return("Tumor stage three (T3 ) describes invasive breast cancer; the cancer cells are breaking through to or invading normal surrounding breast tissues. Stage 3 is divided into three subcategories: IIIA IIIB, and IIIC.

Stage IIIA breast cancer is characterized by the presence of 4-9 axillary lymph nodes or lymph nodes close to the breastbone, or the absence of a tumor in the breast or the presence of a tumor of any size (found during imaging tests or a physical exam) or the tumor is > 5 cm; tiny clusters of breast cancer cells, measuring between 0.2-2 millimeters, are discovered in the lymph nodes; or the tumor is > than 5 cm; the cancer has progressed to 1-3 axillary lymph nodes; or to the lymph nodes close to the breastbone (found during a sentinel lymph node biopsy)

Stage IIIB often refers to invasive breast cancer that has advanced to the chest wall, breast skin, and/or produced edema or an ulcer. It may also have spread up to nine axillary lymph nodes or to lymph nodes close to the breastbone. Breast cancer that is inflammatory is regarded as being at least stage IIIB. The breast feels warm and may be swollen; cancer cells have spread to the lymph nodes; and they may be discovered in the skin. These are typical symptoms of inflammatory breast cancer.

Stage IIIC generally refers to invasive breast cancer in which there may be no symptoms of the disease or, if there is a tumor (it may be of any size) have spread to the chest wall and/or the breast skin, and have affected 10+ axillary lymph nodes, lymph nodes above or below the collarbone, axillary lymph nodes, or lymph nodes close to the breastbone.
")
    }
    else{
      return("Stage four (T4) describes invasive breast cancer that has reached other body organs, such as the lungs, distant lymph nodes, skin, bones, liver, or brain, in addition to the breast and surrounding lymph nodes. T4 stage has four classes: T4a, T4b,T4c, and T4c. T4a means the tumor has spread into the chest walls. T4b means the tumor has spread into the skin. T4c menas the tumor has spread to both the skin and chest wall. T4c means the tumor is now a inflammatrory carcinoma. T4 is pretty serious and likely results in a death sentence/a much lower survival rate. The frequency isn't as high but we can see that WOC tend to be lower in most of the stages.This suggests that WOC are dying much earlier hence the difference between White and WOC.")
    }
  )
  
  output$usmap <- renderPlotly({
    plot_usmap(data = cancer %>% filter(Race == input$racegroup),
               values = "Rate",
               regions = "states",
               # color of the state borders
               color = "grey70") +
      scale_fill_continuous(low = "white", high = "blue",
                            name = "Breast cancer death rate (%)",
                            label = scales::comma) +
      labs(title = "Breast Cancer Death Rates per 100,000 by State and Racial group",
           subtitle = "") +
      theme(legend.position = "right")
  })
}

shinyApp(ui = ui, server = server)
