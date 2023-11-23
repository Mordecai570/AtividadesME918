library(ggplot2)
library(plotly)
library(RCurl)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

#source("scripts.R")

df <- getURL("https://raw.githubusercontent.com/Mordecai570/AtividadesME918/main/Trabalho_Final/cereal.csv")

df <- read.csv(text = df)

df <- subset(df, select = -c(mfr,type,shelf, cups))

# Assuming df is already defined

# Define UI
ui <- navbarPage(
  selected = "df", theme = shinytheme("cerulean"),
  "Projeto",
  tabPanel("Sobre"),
  tabPanel("Cadastro",
           fluidPage(
             titlePanel("Inserir novas Obs"),
             sidebarLayout(
               sidebarPanel(
                 textInput("name", "Name"),
                 numericInput("calories", "Calories", value = 0),
                 numericInput("protein", "Protein", value = 0),
                 numericInput("fat", "Fat", value = 0),
                 numericInput("sodium", "Sodium", value = 0),
                 numericInput("fiber", "Fiber", value = 0),
                 numericInput("carbo", "Carbohydrates", value = 0),
                 numericInput("sugars", "Sugars", value = 0),
                 numericInput("potass", "Potassium", value = 0),
                 numericInput("vitamins", "Vitamins", value = 0),
                 numericInput("weight", "Weight", value = 0),
                 actionButton("submit", "Submit")
               ),
               mainPanel(
                 tableOutput("dataframeTable"),
                 downloadButton(outputId = "downloadData",label = "Download banco de dados")
               )
             )
           )
  ),
  tabPanel("Estatísticas",
           fluidPage(
             titlePanel("Statistics"),
             # Add your statistics visualization code here
             # Example: 
             plotOutput("caloriesPlot"),
             plotOutput("carboPlot"),
             plotOutput("proteinPlot")
           )
  ),
  tabPanel("Comparações",
           fluidPage(
             titlePanel("Pesquise e Compare!"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "searchName", label = "Cereal favorito?", choices = df$name, selected = "All", multiple = FALSE),
                 actionButton("searchButton", "Procurar")
               ),
               mainPanel(
                 textOutput("searchResult"),
                 uiOutput("caloriesComparison"),
                 uiOutput("sugarsComparison"),
                 uiOutput("proteinComparison"),
                 uiOutput("fatComparison"),
                 
               )
             )
           )
  )
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
      weight = input$weight,
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
  # Placeholder for statistics visualization (replace with your actual code)
  output$caloriesPlot <- renderPlot({
    hist(df_reactive()$calories, main = "Calories Distribution")
  })
  
  output$carboPlot <- renderPlot({
    hist(df_reactive()$carbo, main = "Carbohydrates Distribution")
  })
  
  output$proteinPlot <- renderPlot({
    hist(df_reactive()$protein, main = "Protein Distribution")
  })
  
  # Placeholder for prediction logic (replace with your actual code)
  observeEvent(input$predictButton, {
    # Add your prediction logic here
    # For demonstration purposes, just echoing the input
    result <- paste("Predicted score for", input$newProductName, ":", input$newProductCalories + input$newProductCarbo + input$newProductProtein)
    output$predictionResult <- renderText({
      result
    })
  })
  
  # Search and Comparisons
  output$searchResult <- renderText({
    if (input$searchName %in% df$name) {
      search_name <- input$searchName
      result <- ""
      selected_row <- df_reactive()[df_reactive()$name %in% search_name, ]
      result
    }
  })
  
  output$caloriesComparison <- renderUI({
    createStatBox("Calories", "calories", df_reactive())
  })
  
  output$proteinComparison <- renderUI({
    createStatBox("Protein", "protein", df_reactive())
  })
  
  output$fatComparison <- renderUI({
    createStatBox("Fat", "fat", df_reactive())
  })
  
  output$sugarsComparison <- renderUI({
    createStatBox("Sugars", "sugars", df_reactive())
  })
  
  createStatBox <- function(label, column, data) {
    if (!is.null(input$searchButton) && input$searchButton > 0) {
      search_name <- input$searchName
      selected_row <- data[data$name == search_name, ]
      
      if (nrow(selected_row) > 0) {
        # Perform statistic comparison
        comparison_data <- data[data$name != search_name, ]
        
        # Calculate the percentage difference from the mean for the selected statistic
        mean_stat <- mean(comparison_data[[column]])
        stat_difference <- ((selected_row[[column]] - mean_stat) / mean_stat) * 100
        
        # Determine letter and color based on the stat_difference
        if (stat_difference < 0) {
          letter <- 'A'
          color <- '#87CEEB'  # Light Sky Blue
        } else {
          letter <- 'F'
          color <- '#FFA07A'  # Light Salmon
        }
        
        HTML(
          paste0(
            '<div style = "background-color: ', color,'; border: 2px solid #808080; border-radius: 10px; padding: 10px;">',
            '<center>',
            '<font style = "color: black; font-size: 24px;">', round(stat_difference, 2), '%</font>',
            '<br>',
            '<b>', label, '</b>',
            '</center>',
            '</div>'
          )
        )
      } else {
        HTML('<div>No data found for the specified name.</div>')
      }
    }
  }
  
}
# Run the application
shinyApp(ui, server)

