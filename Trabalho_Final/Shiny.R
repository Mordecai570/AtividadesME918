library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)


ui <-  navbarPage(selected = "precos", #theme = shinytheme("cerulean"),
                  "Projeto",
                  tabPanel("Primeira aba"),
                  tabPanel("Modelo", numericInput("x","caloria", value = 0 ), numericInput("y","gordura", value = 0),numericInput("z","proteina", value = 0 )),
                  tabPanel("Pesquisa", selectInput("Cereal", "Cereal favorito?",state.name)),
                  tabPanel("Cadastro"),
                  tabPanel("EAD" )
                           
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
