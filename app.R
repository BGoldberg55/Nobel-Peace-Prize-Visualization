#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
nobel_data <- read.csv("data/nobel_prize_by_winner.csv")
dummy <- "all"
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Final Project"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          radioButtons("filter_gender", "Filter by Gender:",
                       c("No Filter" = "all",
                         "Female" = "female",
                         "Male" = "male")),
          radioButtons("filter_field", "Filter by Field:",
                       c("No Filter" = "all",
                         "Economics" = "economics",
                         "Chemistry" = "chemistry",
                         "Medicine" = "medicine",
                         "Physics" = "physics",
                         "Literature" = "literature",
                         "Peace" = "peace"
                         )),
          radioButtons("filter_length", "How Many Countries Should be Shown?",
                       c("3" = 3,
                         "5" = 5,
                         "10" = 10)),
          
        )
        ,

        # Show a plot of the generated distribution
        mainPanel( width = 500,
          tabsetPanel( type = "tabs",
           tabPanel("Graph",plotOutput("distPlot")),
           tabPanel("Table", tableOutput("distTable"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      nobel_prize_by_winner %>% filter(gender == input$filter_gender | dummy == input$filter_gender) %>%
        filter(category == input$filter_field | dummy == input$filter_field) %>%
        group_by(bornCountry) %>%
        filter(!is.na(bornCountry)) %>% summarize(total_people = n()) %>% arrange(desc(total_people))%>%
        slice(1:input$filter_length) %>% ggplot() + 
        geom_bar(aes(x = fct_reorder(bornCountry, total_people , .desc = TRUE), y = total_people, fill = bornCountry), stat = "identity") +
        labs(title = "Nobel Prize Country Anlaysis", x = "Country", y = "Number of people", fill = "Country") +
        scale_x_discrete(guide = guide_axis(n.dodge=2)) +
        theme(legend.key.size = unit(1.2, 'cm'), legend.title = element_text(size=24),
              legend.text = element_text(size=10),
              plot.title = element_text(size = 40, hjust = 0.5),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.text.x= element_text(size = 12),
              axis.text.y= element_text(size = 12)
              )
    })
    output$distTable <- renderTable ({
      nobel_prize_by_winner %>% filter(gender == input$filter_gender | dummy == input$filter_gender) %>%
        filter(category == input$filter_field | dummy == input$filter_field) %>% group_by(bornCountry) %>%
        filter(!is.na(bornCountry)) %>% summarize(total_people = n()) %>% arrange(desc(total_people)) %>%
        slice(1:input$filter_length) %>% rename(Country = bornCountry, Total_People = total_people)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
