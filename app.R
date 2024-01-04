# baby names app to generate random boy or girl name for family planning purposes. Data from babynames R application
# andrew.corbin.ngs@gmail.com

library(shiny)
library(babynames)
library(dplyr)


# 
# M_names <- filter(babynames, sex == "M")
# M_names <- as.data.frame(unique(M_names$name))
# F_names <- filter(babynames, sex == "F")
# F_names <- as.data.frame(unique(F_names$name))
# 
# 
# F_names <- filter(babynames, sex == "F")
# F_names <- filter(F_names, n > 1000)
# F_names <- as.data.frame(unique(F_names$name))
# 
# M_names <- filter(babynames, sex == "M")
# M_names <- filter(M_names, n > 1000)
# M_names <- as.data.frame(unique(M_names$name))

M_names <- babynames %>%
  filter(sex == "M") %>%
  filter(n > 1000)

M_names <- as.data.frame(unique(M_names$name))

F_names <- babynames %>%
  filter(sex == "F") %>%
  filter(n > 1000)

F_names <- as.data.frame(unique(F_names$name))

ui <- fluidPage(titlePanel("Baby Name Generator"),
                actionButton("Fbutton", "Girl Name", class = "btn-info", style="color: #fff; background-color: #e44ef5; border-color: #2e6da4"),
                actionButton("Mbutton", "Boy Name", class = "btn-info", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                verbatimTextOutput("text")
)
server <- function(input, output){
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$Fbutton, {
    output$text <- renderText(sample(F_names$unique, 1))
  })
  observeEvent(input$Mbutton, {
    output$text <- renderText(sample(M_names$unique, 1))
  })
}

shinyApp(ui=ui, server=server)

########
# pull male and female names
# 
# sex <- "M"
# ocr <- 1000
# M_names <- babynames %>%
#   filter(sex == "M") %>%
#   filter(n > ocr)
# 
# M_names <- as.data.frame(unique(M_names$name))
# 
# F_names <- babynames %>%
#   filter(sex == "F")
# 
# 
# 
# F_names <- filter(babynames, sex == "F")
# F_names <- filter(F_names, n > 1000)
# F_names <- as.data.frame(unique(F_names$name))
# 
# M_names <- filter(babynames, sex == "M")
# M_names <- filter(M_names, n > 1000)
# M_names <- as.data.frame(unique(M_names$name))
# 
# head(M_names)
# head(F_names)
