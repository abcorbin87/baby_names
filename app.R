# andrew.corbin.ngs@gmail.com 

library(shiny)
library(tidyverse)
library(babynames)
library(dplyr)
library(plotly)
library(shinythemes)
library(data.table)
library(DT)

# Make needed input data for app
# for random name generator - take names with at least one year with n occurrences
M_names <- babynames %>%
  filter(sex == "M") %>%
  filter(n > 900)

M_names <- as.data.frame(unique(M_names$name))

F_names <- babynames %>%
  filter(sex == "F") %>%
  filter(n > 900)

F_names <- as.data.frame(unique(F_names$name))

# filter data for data table summary
female_sum <- babynames %>%
  filter(sex == "F") %>%
  group_by(name) %>%
  summarize(total_occurrences = sum(n))

male_sum <- babynames %>%
  filter(sex == "M") %>%
  group_by(name) %>%
  summarize(total_occurrences = sum(n))

#test to see if only one row per name
#length(male_sum$name[male_sum$name == "Andrew"])

# Design user interface
ui <- fluidPage(theme = shinytheme("flatly"), 
                titlePanel("Baby Name Popularity Throughout Time"),
                sidebarLayout(
                  sidebarPanel(
                    textInput(inputId = "name",
                              label = "Name (Case sensitive):",
                              value = "Andrew"),
                    actionButton("Fbutton", "Generate Random Girl Name", class = "btn-info", style="color: #fff; background-color: #e44ef5; border-color: #e44ef5"),
                    actionButton("Mbutton", "Generate Random Boy Name", class = "btn-info", style="color: #fff; background-color: #337ab7; border-color: #337ab7"),
                    verbatimTextOutput("text"),
                    selectInput(inputId = "sex",
                                label = "Gender:",
                                choices = list(Male = "M",
                                               Female = "F"),
                                selected = "M"),
                    sliderInput(inputId = "year",
                                label =  "Year Range Slider:",
                                min = min(babynames$year),
                                max = max(babynames$year),
                                value = c(min(babynames$year),
                                          max(babynames$year)),
                                sep = ""),
                    br(),
                    p("Summarize names by year"),
                    textInput(inputId = "year_txt",
                              label = "Year to Summarize:",
                              value = 1987),
                    p("Aggregate female name popularity accross time"),
                    dataTableOutput(outputId = "f_nametable"),
                    br(),
                    br(),
                    p("Aggregate male name popularity accross time"),
                    dataTableOutput(outputId = "m_nametable"),
                  ),
                  mainPanel(
                    plotlyOutput(outputId = "nameplot"),
                    plotlyOutput(outputId = "yearplot"),
                  )
                )
)

server <- function(input, output) {
  input_yeari <- reactive(as.integer(input$year_txt))
  input_year <- reactive(input$year_txt)
  output$nameplot <- renderPlotly({
    babynames %>%
      filter(sex == input$sex,
             name == input$name) %>%
      ggplot(aes(x = year,
                 y = n)) +
      labs(y = "Number of People", 
           x = "Year") +
      geom_line() +
      scale_x_continuous(limits = input$year) +
      theme_minimal()  +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
  })
  output$yearplot <- renderPlotly({
    top_names <- babynames %>%
      filter(year == input_yeari()) %>%
      group_by(name) %>%
      summarize(total_occurrences = sum(n)) %>%
      arrange(desc(total_occurrences)) %>%
      head(10)
    
    # Plotting using ggplot2 with scaled color
    ggplot(top_names, aes(x = total_occurrences, y = reorder(name, total_occurrences), fill = total_occurrences)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top 10 Names in", input_year()),
           x = "Number of Occurrences",
           y = "Name") +
      theme_minimal() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14, face="bold")) +
      scale_fill_gradient(low = "lightblue", high = "darkblue")
  })
  output$f_nametable <- renderDataTable(female_sum)
  output$m_nametable <- renderDataTable(male_sum)
  v <- reactiveValues(data = NULL)
  observeEvent(input$Fbutton, {
    output$text <- renderText(sample(F_names$`unique(F_names$name)`, 1))
  })
  observeEvent(input$Mbutton, {
    output$text <- renderText(sample(M_names$`unique(M_names$name)`, 1))
  })
}

shinyApp(ui = ui, server = server)
