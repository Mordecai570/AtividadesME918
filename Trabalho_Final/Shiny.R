library(ggplot2)
library(plotly)
library(RCurl)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

source("script.R")

#df <- getURL("https://raw.githubusercontent.com/Mordecai570/AtividadesME918/main/Trabalho_Final/cereal_ajustado.csv")
#df <- read.csv(text = df)

df <- read.csv("../Data/cereal_ajustado.csv")

# Define UI
ui <- navbarPage(
  selected = "df", theme = shinytheme("paper"),
  title = div("", img(src = "menu.png", id = "simulation", height = "60px",width = "1000px",style = "position: relative; margin:-15px 0px; display:right-align;")),
  tabPanel("Sobre",
           fluidRow(
             column(width = 10 , offset = 1, align = "center", 
                    div(
                      img(src = "sobre.png", width = "1000px", height = "3000px")
                    ),
                    tags$a(href = "https://github.com/Mordecai570/AtividadesME918/tree/main/Trabalho_Final", target = "_blank",
                           tags$i(class = "fab fa-github fa-3x"),  # Font Awesome GitHub icon class
                           " GitHub"
                    ),
                    style = "background-color: #cef3c4; padding: 40px; border-radius: 5px; box-shadow: 0px 0px 5px 0px #ccc;"
             ), 
           )),
  tabPanel("Cadastro",
           fluidPage(
             titlePanel(""),
             sidebarLayout(
               sidebarPanel(
                 div(
                   img(src = "cadastro.png", width = "400px", height = "300px")
                 ),
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
                 actionButton("submit", "Registrar"),
                 div(
                   img(src = "imgtabela.png", width = "400px", height = "1800px")
                 ),
                 style = "background-color: #cef3c4; padding: 20px; border-radius: 5px; box-shadow: 0px 0px 5px 0px #ccc;"
               ),
               mainPanel(
                 tableOutput("dataframeTable"),
                 downloadButton(outputId = "downloadData", label = "Download banco de dados"),
                 # Adding CSS styles
                 style = "background-color: #cef3c4; padding: 20px; border-radius: 5px; box-shadow: 0px 0px 5px 0px #ccc;"
               )
             )
           )
  ),
  tabPanel("Estatísticas",
           fluidRow(
             column(6,plotOutput("caloriesPlot")),
             column(6,plotOutput("proteinPlot")),
             column(6,plotOutput("fatPlot")),
             column(6, plotOutput("sodiumPlot")),
             column(6, plotOutput("fiberPlot")),
             column(6, plotOutput("carboPlot")),
             column(6,plotOutput("sugarsPlot")),
             column(6,plotOutput("potassPlot")),
             column(6,plotOutput("vitaminsPlot")),
           )
  ),
  tabPanel("Comparações",
           fluidPage(
             titlePanel(""),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "searchName", label = "Escolha um cereal e compare ele com todos os outros!", choices = df$name, selected = "All", multiple = FALSE),
                 actionButton("searchButton", "Procurar"),
                 div(
                   img(src = "compara.png", width = "400px", height = "300px")
                 ),
                 # Adding CSS styles
                 style = "padding: 20px; background-color: #cef3c4; border-radius: 5px; box-shadow: 0px 0px 5px 0px #ccc;"
               ),
               mainPanel(
                 textOutput("searchResult"),
                 uiOutput("caloriesComparison"),
                 uiOutput("sugarsComparison"),
                 uiOutput("proteinComparison"),
                 uiOutput("fatComparison"),
                 # Adding CSS styles
                 style = "padding: 20px; background-color: #cef3c4; border-radius: 5px; box-shadow: 0px 0px 5px 0px #ccc;"
               )
             )
           )
  ),
  tags$style(HTML(".navbar-default { background-color: #e5c1c1; }")),
  
  # tags$head for additional CSS/JavaScript
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$style(HTML("
      /* Additional CSS styles */
    ")),
    
    tags$script('
      $(document).on("shiny:inputchanged", function(event) {
        if (event.name.endsWith("_color")) {
          var columnName = event.name.replace("_color", "");
          $("#" + columnName + "-stat-box").css("background-color", event.value);
        }
      });
    ')
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive values for the dataframe
  df_reactive <- reactiveVal(df)
  
  # Função para inserir valores no df
  observeEvent(input$submit, {
    # Get the current data frame
    current_df <- df_reactive()
    
    # Create a new row without the "rating" column
    new_row <- data.frame(
      name = input$name,
      calories = input$calories,
      protein = input$protein,
      fat = input$fat,
      sodium = input$sodium,
      fiber = input$fiber,
      carbo = input$carbo,
      sugars = input$sugars,
      potass = input$potass,
      vitamins = input$vitamins,
      rating = NA,
      Nationality = "BR"
    )
    
    # Call the train_and_predict and predict_rating functions
    
    train_and_predict(current_df)
    new_row$rating = predict_rating(new_row)
    
    # Check if the data frame is empty
    df_reactive(rbind(current_df, new_row))
    
    # Salva a nova adição no novas_add.csv
    #write.csv(df_reactive(), "../Data/cereal.csv", row.names = FALSE)
  })
  
  # Mostra o dataframe atualizado
  output$dataframeTable <- renderTable({
    df_reactive()
  })
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("df_", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(df, file)
  #   }
  # )
 # GRÁFICO:

  output$caloriesPlot <- renderPlot({
    hist(df_reactive()$calories, main = "Distriubuição das calorias",
         col = "#BDECB6",
         border = "white", 
         alpha = 0.7,
         xlab = "Calorias",
         ylab = "Frequência")
  })
  
  
  output$proteinPlot <- renderPlot({
    hist(df_reactive()$protein, main = "Distriubuição da proteína",
         col = "#BDECB6",
         border = "white", 
         alpha = 0.7,
         xlab = "Proteína",
         ylab = "Frequência")
  })
  
   
  output$fatPlot <- renderPlot({
    hist(df_reactive()$fat, main = "Distriubuição das gorduras",
         col = "#BDECB6",
         border = "white", 
         alpha = 0.7,
         xlab = "Gorduras",
         ylab = "Frequência")
  })
  
  
 output$sodiumPlot <- renderPlot({
    hist(df_reactive()$sodium, main = "Distriubuição do sódio",
         col = "#BDECB6",
         border = "white", 
         alpha = 0.7,
         xlab = "Sódio",
         ylab = "Frequência")
 })

 output$fiberPlot <- renderPlot({
   hist(df_reactive()$fiber, main = "Distriubuição das fibras",
        col = "#BDECB6",
        border = "white", 
        alpha = 0.7,
        xlab = "Fibras",
        ylab = "Frequência")
 })
 
 output$carboPlot <- renderPlot({
   hist(df_reactive()$carbo, main = "Distriubuição da carboidrato",
        col = "#BDECB6",
        border = "white", 
        alpha = 0.7,
        xlab = "Carboidrato",
        ylab = "Frequência")
 })
 
 output$sugarsPlot <- renderPlot({
   hist(df_reactive()$sugars, main = "Distriubuição do açúcares",
        col = "#BDECB6",
        border = "white", 
        alpha = 0.7,
        xlab = "Açúcares",
        ylab = "Frequência")
 })
 
 output$potassPlot <- renderPlot({
   hist(df_reactive()$potass, main = "Distriubuição do potássio",
        col = "#BDECB6",
        border = "white", 
        alpha = 0.7,
        xlab = "Potássio",
        ylab = "Frequência")
 })
 
 output$vitaminsPlot <- renderPlot({
   hist(df_reactive()$vitamins, main = "Distriubuição das vitaminas",
        col = "#BDECB6",
        border = "white", 
        alpha = 0.7,
        xlab = "Vitaminas",
        ylab = "Frequência")
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
    createStatBox2("Calories", "calories", df_reactive())
  })
  
  output$proteinComparison <- renderUI({
    createStatBox("Protein", "protein", df_reactive())
  })
  
  output$fatComparison <- renderUI({
    createStatBox2("Fat", "fat", df_reactive())
  })
  
  output$sugarsComparison <- renderUI({
    createStatBox2("Sugars", "sugars", df_reactive())
  })
  
  #funcao para as proteinas porque elas fazem bem 
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
        
        
        # Determine color based on the stat_difference
        if (stat_difference < 0) {
          intensity <- 190*abs(stat_difference)
          color <-  paste0("rgb(", intensity*0.5, ",50 ,97)") # ruim 
        } else {
          intensity <- 210*abs(stat_difference)
          color <- paste0("rgb(167,171 ,", intensity*1, ")") # bom 
        }
        
        HTML(
          paste0(
            '<div id="', column, '-stat-box" style = "background-color: ', color,'; border: 2px solid #808080; border-radius: 10px; padding: 10px;">',
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
  #funcao para o resto porque faz mal
  createStatBox2 <- function(label, column, data) {
    if (!is.null(input$searchButton) && input$searchButton > 0) {
      search_name <- input$searchName
      selected_row <- data[data$name == search_name, ]
      
      if (nrow(selected_row) > 0) {
        # Perform statistic comparison
        comparison_data <- data[data$name != search_name, ]
        
        # Calculate the percentage difference from the mean for the selected statistic
        mean_stat <- mean(comparison_data[[column]])
        stat_difference <- ((selected_row[[column]] - mean_stat) / mean_stat) * 100
        
        # Determine color based on the stat_difference
        if (stat_difference < 0) {
          intensity <- 210*abs(stat_difference)
          color <- paste0("rgb(167,171 ,", intensity*0.5, ")") # bom 
        } else {
          intensity <- 190*abs(stat_difference)
          color <-  paste0("rgb(", intensity*1, ",50 ,97)") # ruim 
        }
        
        HTML(
          paste0(
            '<div id="', column, '-stat-box" style = "background-color: ', color,'; border: 2px solid #808080; border-radius: 10px; padding: 10px;">',
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

