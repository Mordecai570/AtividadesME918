library(ggplot2)
library(plotly)
library(RCurl)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

df <- getURL("https://raw.githubusercontent.com/Mordecai570/AtividadesME918/main/Trabalho_Final/cereal.csv")

df <- read.csv(text = df)

ui <-  navbarPage(selected = "df", theme = shinytheme("cerulean"),
                  "Projeto",
                  tabPanel("Primeira aba"),
                  tabPanel("Modelo", numericInput("x","caloria", value = 0 ), numericInput("y","gordura", value = 0),numericInput("z","proteina", value = 0 )),
                  tabPanel("Pesquisa", icon = icon("magnifying-glass"), selectInput(inputId = "Cereal", label = "Cereal favorito?",choices = df$name, selected = "All",multiple = FALSE), downloadButton(
                             outputId = "downloadData",
                             label = "Download banco de dados"
                           )),
                  tabPanel("Cadastro"),
                  tabPanel("EAD" )
                           
  
)


server <- function(input, output, session) {
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("df_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
