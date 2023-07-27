# Librerías necesarias ----

library(shiny)
library(shinyWidgets)
library(magrittr)
library(ggplot2)
library(shinyhelper)

# Importar Funciones para gráficos ----

source("Funciones/FunPerfilVerosimilitud.R")

# ------------------------------------------------------------------------------

# Interfaz de usuario ----
ui <- fixedPage(
  
  # Configuraciones de visualización ----
  tags$style("body {
              -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
              zoom: 0.8; /* Other non-webkit browsers */
              zoom: 80%; /* Webkit browsers */
              }"),
  
  fluidRow(
    
    # Título Principal ----
    titlePanel(h1("Perfiles de Verosimilitud", align = "center",
                  style = "font-family: Arial Black;
                color: #15297c;
                border: 2px solid LightGray;
                border-radius: 5px;
                padding: 20px")), 
    
    # Panel lateral ----
    column(4,
           
      # Imágenes panel lateral
      tags$figure(
        tags$img(src = "Logo.png", height = 130), align = "center"),
      
      # Título
      h4("Modifica tus Parámetros", align = "center", 
         style = "font-family: Arial Black; color: #15297c; padding: 5px") %>%
        helper(buttonLabel = "Volver",
               colour = "#2898EE",
               type = "markdown",
               content = "Perfiles/AyudaPerfiles"),
      
      # Panel ----
      wellPanel(
        style = "background-color : White; border: 2px solid LightGray",
        
        # Parámetros modificables ----
        
        # Selección tamaño de muestra
        numericInput("n", label = "Tamaño de muestra", min = 2, max = 100, value = 50),
        
        # Barra selección de distribución
        selectInput("distribucion", label = "Distribución", 
                    choices = c("Normal" = 1, "Log-normal" = 2, "Exponencial" = 3)),
        
        # Aparecen parámetros según la distribución seleccionada
        uiOutput("parametros"),
      )
    ),
    
    # Panel principal ----
    column(8,
           
      # Menú ----
      tabsetPanel(
        type = "pills",
        
        # Secciones del menú ----
        
        tabPanel(
          title = "Nuestro Equipo",
          icon = icon("people-group", lib = "font-awesome"),
          includeMarkdown("www/Complementos/NuestroEquipo.md")
        ),
        
        tabPanel(
          title = "Resultados", 
          icon = icon("square-poll-vertical", lib = "font-awesome"),
          style = "border: 2px solid LightGray; 
                  border-radius: 5px;
                  padding: 20px",
          plotOutput("plotPerfilV", width = "100%", height = "550px")
        ),
        
        tabPanel(
          title="Teoría Aplicada", 
          icon=icon("circle-info", lib = "font-awesome"),
          
          tabsetPanel(
            type = "tabs",
            
            tabPanel(
              title = "Preguntas Frecuentes",
              tags$figure(
                tags$img(src="Teoria/Perfiles/Preguntas.png", width = "110%"), align = "center")),
            
            tabPanel(
              title = "Normal",
              tags$figure(
                tags$img(src="Teoria/Perfiles/DistNormal.png", width = "110%"), align = "center")),
            
            tabPanel(
              title = "Log-Normal",
              tags$figure(
                tags$img(src="Teoria/Perfiles/DistLogNormal.png", width = "110%"), align = "center")),
            
            tabPanel(
              title = "Exponencial",
              tags$figure(
                tags$img(src="Teoria/Perfiles/DistExp.png", width = "110%"), align = "center"))
          )
        ),
        
        tabPanel(
          title = "Código Fuente",
          icon = icon("github", lib = "font-awesome"),
          
          tabsetPanel(
            type = "tabs",
            
            tabPanel(
              title = "Repositorio",
              align="center",
              includeMarkdown("www/Complementos/GitHub.md")),
            
            tabPanel(
              title = "server",
              verbatimTextOutput("serverOutput")),
            
            tabPanel(
              title = "ui",
              verbatimTextOutput("uiOutput"))
          )
        )
      )
    )
  )
)

# Lógica del servidor ----
server <- function(input, output) {
  
  # Sección de ayuda ----
  observe_helpers()
  
  # Leer el contenido del archivo .txt ----
  codigoserver <- reactive({
    codigo_texto <- readLines("www/Codigo/Perfiles/server.txt", warn=FALSE)
    return(paste(codigo_texto, collapse = "\n"))
  })
  
  codigoui <- reactive({
    codigo_texto <- readLines("www/Codigo/Perfiles/ui.txt", warn=FALSE)
    return(paste(codigo_texto, collapse = "\n"))
  })
  
  # Mostrar el código en el cuadro de texto ----
  output$serverOutput <- renderPrint({
    cat(codigoserver())
  })
  
  output$uiOutput <- renderPrint({
    cat(codigoui())
  })
  
  # Panel parámetros dinámicos ----
  output$parametros <- renderUI({
    
    if (input$distribucion == 1) { # Para distribución Normal
      tagList (
        withMathJax(),
        selectInput("perfilVN", label = "Perfil de verosimilitud", choices =  c("Media" = 1, "Varianza" = 2)),
        withMathJax(),
        numericInput("muN", label = "\\(\\mu\\)", value = 0),
        numericInput("sigmaN", label = "\\(\\sigma^2\\)", value = 1, min = 1)
      )
    } 
    else if (input$distribucion == 2) { # Para distribución Log-Normal
      tagList (
        withMathJax(),
        selectInput("perfilVLN", label = "Perfil de verosimilitud", choices = c("μ" = 1,  "σ" = 2)), 
        withMathJax(),
        numericInput("muL", label = "\\(\\mu\\)", value = 1, min = 1),
        numericInput("sigmaL", label = "\\(\\sigma\\)", value = 2, min = 2)
      )
    }
    else { # Para distribución Exponencial
      tagList(
        selectInput("perfilVE", label = "Perfil de verosimilitud", choices = c("Media" = 1)), 
        withMathJax(),
        numericInput("beta", label = "\\(\\beta\\)", value = 1, min = 1)
      )
    }
  })
  
  # Gráficos perfiles de verosimilitud ----
  output$plotPerfilV <- renderPlot ({
    if (is.null(input$distribucion) || is.null(input$n)) {
      return(NULL)
    }
    
    if (input$distribucion == 1) {
      if (is.null(input$muN) || is.null(input$sigmaN)) {
        return(NULL)
      }
      
      muestra <- rnorm(n = input$n, mean = input$muN, sd = input$sigmaN)
      if (input$perfilVN == 1) {mvMediaN(muestra)}
      else if (input$perfilVN == 2) {mvVarianzaN(muestra)}
    }
    
    else if (input$distribucion == 2) {
      if (is.null(input$muL) || is.null(input$sigmaL)) {
        return(NULL)
      }
      
      muestra <- rlnorm(n = input$n, mean = input$muL, sd = input$sigmaL)
      if (input$perfilVLN == 1) {mvMediaLN(muestra)}
      else if (input$perfilVLN == 2) {mvVarianzaLN(muestra)}
    }
    
    else if (input$distribucion == 3) {
      if (is.null(input$beta)) {
        return(NULL)
      }
      
      muestra <- rexp(n = input$n, rate = 1/input$beta)
      mvMediaE(muestra)
    }
  })
}

# Ejecutar aplicación ----
shinyApp(ui = ui, server = server)
