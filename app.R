# andrew.corbin.ngs@gmail.com 

library(shiny)
library(tidyverse)
library(babynames)
library(dplyr)
library(plotly)

M_names <- babynames %>%
  filter(sex == "M") %>%
  filter(n > 1000)

M_names <- as.data.frame(unique(M_names$name))

F_names <- babynames %>%
  filter(sex == "F") %>%
  filter(n > 1000)

F_names <- as.data.frame(unique(F_names$name))

ui <- fluidPage(titlePanel("Baby Name Popularity Throughout Time"),
                textInput(inputId = "name",
                          label = "Name (Case sensitive):",
                          value = "Kolt"),
                actionButton("Fbutton", "Generate Random Girl Name", class = "btn-info", style="color: #fff; background-color: #e44ef5; border-color: #2e6da4"),
                actionButton("Mbutton", "Generate Random Boy Name", class = "btn-info", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
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
                plotlyOutput(outputId = "nameplot")
                )

server <- function(input, output) {
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
  v <- reactiveValues(data = NULL)
  observeEvent(input$Fbutton, {
    output$text <- renderText(sample(F_names$`unique(F_names$name)`, 1))
  })
  observeEvent(input$Mbutton, {
    output$text <- renderText(sample(M_names$`unique(M_names$name)`, 1))
  })
}
shinyApp(ui = ui, server = server)

