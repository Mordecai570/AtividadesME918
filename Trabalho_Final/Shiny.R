library(ggplot2)
library(plotly)
library(RCurl)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

df <- getURL("https://raw.githubusercontent.com/Mordecai570/AtividadesME918/main/Trabalho_Final/cereal.csv")

df <- read.csv(text = df)

library(shiny)
library(shinythemes)

# Assuming df is already defined

# Define UI
ui <- navbarPage(
  selected = "df", theme = shinytheme("cerulean"),
  "Projeto",
  tabPanel("Primeira aba"),
  tabPanel("Modelo", 
           numericInput("x", "caloria", value = 0), 
           numericInput("y", "gordura", value = 0),
           numericInput("z", "proteina", value = 0)
  ),
  tabPanel("Pesquisa", 
           icon = icon("magnifying-glass"), 
           selectInput(inputId = "Cereal", label = "Cereal favorito?", choices = df$name, selected = "All", multiple = FALSE),
           downloadButton(
             outputId = "downloadData",
             label = "Download banco de dados"
           )
  ),
  tabPanel("Cadastro",
           fluidPage(
             titlePanel("Inserir novas Obs"),
             sidebarLayout(
               sidebarPanel(
                 textInput("name", "Name"),
                 textInput("mfr", "Manufacturer"),
                 textInput("type", "Type"),
                 numericInput("calories", "Calories", value = 0),
                 numericInput("protein", "Protein", value = 0),
                 numericInput("fat", "Fat", value = 0),
                 numericInput("sodium", "Sodium", value = 0),
                 numericInput("fiber", "Fiber", value = 0),
                 numericInput("carbo", "Carbohydrates", value = 0),
                 numericInput("sugars", "Sugars", value = 0),
                 numericInput("potass", "Potassium", value = 0),
                 numericInput("vitamins", "Vitamins", value = 0),
                 numericInput("shelf", "Shelf", value = 0),
                 numericInput("weight", "Weight", value = 0),
                 numericInput("cups", "Cups", value = 0),
                 numericInput("rating", "Rating", value = 0),
                 actionButton("submit", "Submit")
               ),
               mainPanel(
                 tableOutput("dataframeTable")
               )
             )
           )
  ),
  tabPanel("EAD")  
)

# Define Server
server <- function(input, output, session) {
  # Reactive values for the dataframe
  df_reactive <- reactiveVal(df)
  
  # Função para inserir valores no df
  observeEvent(input$submit, {
    new_row <- data.frame(
      name = input$name,
      mfr = input$mfr,
      type = input$type,
      calories = input$calories,
      protein = input$protein,
      fat = input$fat,
      sodium = input$sodium,
      fiber = input$fiber,
      carbo = input$carbo,
      sugars = input$sugars,
      potass = input$potass,
      vitamins = input$vitamins,
      shelf = input$shelf,
      weight = input$weight,
      cups = input$cups,
      rating = input$rating
    )
    
    if (nrow(df_reactive()) == 0) {
      df_reactive(data.frame(new_row, stringsAsFactors = FALSE))
    } else {
      df_reactive(rbind(df_reactive(), new_row))
    }
    
    # Salva a nova adição no novas_add.csv
    write.csv(df_reactive(), "../Data/novas_add.csv", row.names = FALSE)
  })
  
  # Mostra o dataframe atualizado
  output$dataframeTable <- renderTable({
    df_reactive()
  })
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
shinyApp(ui, server)
