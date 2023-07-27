# Librerías necesarias ----

library(shiny)
library(MASS)
library(magrittr)
library(shinyhelper)
library(ggplot2)

# Importar Funciones para gráficos ----

source("Funciones/FunInterVerosimilitud.R")
#-------------------------------------------------------------------------------

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
    titlePanel(h1("Intervalos de Verosimilitud", align = "center",
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
      h4("Modifica tus parámetros", align = "center", 
         style = "font-family: Arial Black; color: #15297c; padding: 5px"),
      
      # Panel ----
      wellPanel(
        style = "background-color : White; border: 2px solid LightGray",
        
        # Parámetros modificables ----
        
        # Cargar base de datos
        fileInput("file", "Carga la Base de Datos", 
                  accept = ".csv",
                  buttonLabel = "Buscar...",
                  placeholder = "Ningún archivo seleccionado") %>%
          helper(buttonLabel = "Volver",
                 colour = "#2898EE",
                 type = "markdown",
                 content = "Intervalos/AyudaPaso1")),
      
      # Panel selección de variable
      uiOutput("panelVar"),
      
      # Panel selección prueba de bondad de ajuste 
      uiOutput("panelPrueba"),
      
      # Selección parámetros gráfico 
      uiOutput("panelParam"),
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
          verbatimTextOutput("prueba"),
          tags$head(tags$style("#prueba{color: black;
                                 font-size: 14px;
                                 font-family: Helvetica;
                                 font-weight: bold;
                                 background-color : White; 
                                 border: 2px solid LightGray
                                 }")),
          plotOutput("plotPerfilV", width = "100%", height = "600px")),
        
        tabPanel(
          title="Teoría Aplicada", 
          icon=icon("circle-info", lib = "font-awesome"),
          
          tabsetPanel(
            type = "tabs",
            
            tabPanel(
              title = "Preguntas Frecuentes",
              tags$figure(
                tags$img(src="Teoria/Intervalos/Preguntas.png", width = "110%"), align = "center")),
            
            tabPanel(
              title = "Normal",
              tags$figure(
                tags$img(src="Teoria/Intervalos/DistNormal.png", width = "110%"), align = "center")),
            
            tabPanel(
              title = "Log-Normal",
              tags$figure(
                tags$img(src="Teoria/Intervalos/DistLogNormal.png", width = "110%"), align = "center")),
            
            tabPanel(
              title = "Exponencial",
              tags$figure(
                tags$img(src="Teoria/Intervalos/DistExp.png", width = "110%"), align = "center"))
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
    codigo_texto <- readLines("www/Codigo/Intervalos/server.txt", warn=FALSE)
    return(paste(codigo_texto, collapse = "\n"))
  })
  
  codigoui <- reactive({
    codigo_texto <- readLines("www/Codigo/Intervalos/ui.txt", warn=FALSE)
    return(paste(codigo_texto, collapse = "\n"))
  })
  
  # Mostrar el código en el cuadro de texto ----
  output$serverOutput <- renderPrint({
    cat(codigoserver())
  })
  
  output$uiOutput <- renderPrint({
    cat(codigoui())
  })

  # Verifica si ya se cargó una base de datos ----
  filedata <- reactive ({
    infile <- input$file
    if (is.null(infile)) {
      return(NULL)      
    }
    else{read.csv(infile$datapath)}
  })
  
  # Panel selección de variable
  output$panelVar <- renderUI ({
    if (is.null(filedata())) {
      return(NULL)
    } 
    else {
      wellPanel(
        style = "background-color : White; border: 2px solid LightGray",
        selectInput("variable", label = "Selecciona la Variable", choices =  c("Ninguna", names(filedata()))) %>%
          helper(buttonLabel = "Volver",
                 colour = "#2898EE",
                 type = "markdown",
                 content = "Intervalos/AyudaPaso2")
      )}})
  
  # Panel selección prueba de bondad de ajuste
  output$panelPrueba <- renderUI ({
    if (input$variable == "Ninguna" || is.null(input$variable) || is.null(filedata())) {
      return(NULL)
    } else {
      wellPanel(
        style = "background-color : White; border: 2px solid LightGray",
        selectInput("distribucion", label = "Prueba de Bondad de Ajuste" , 
                    choices = c("Ninguna" = 0, "Normal" = "pnorm", "Log-normal" = "plnorm", "Exponencial" = "pexp")) %>%
          helper(buttonLabel = "Volver",
                 colour = "#2898EE",
                 type = "markdown",
                 content = "Intervalos/AyudaPaso3")
      )}})
  
  # Prueba bondad de ajuste
  test <- reactive({
    if (input$distribucion == 0 || is.null(input$distribucion) ||
        input$variable == "Ninguna" || is.null(input$variable) || is.null(filedata())) {
      return(NULL)
    }
    datos <- filedata()[input$variable]
    if (input$distribucion == "pnorm") {
      ajuste <- fitdistr(unlist(datos), "normal")
      ks.test(datos, input$distribucion, mean = ajuste$estimate[1], sd = ajuste$estimate[2])
    } else if (input$distribucion == "plnorm") {
      ajuste <- fitdistr(unlist(datos), "lognormal")
      ks.test(datos, input$distribucion, meanlog = ajuste$estimate[1], sdlog = ajuste$estimate[2])
    } else if (input$distribucion == "pexp") {
      ajuste <- fitdistr(unlist(datos), "exponential")
      ks.test(datos, input$distribucion, rate = ajuste$estimate[1])
    }
  })
  
  # Resultados prueba bondad de ajuste
  output$prueba <- renderText ({
    if (input$distribucion == 0 || is.null(input$distribucion) ||
        input$variable == "Ninguna" || is.null(input$variable) || is.null(filedata())) {
      return(NULL)
    } else {
      
      if (input$distribucion == "pnorm") {aux1 <- "Normalidad"}
      else if (input$distribucion == "plnorm") {aux1 <- "Log-Normalidad"}
      else if (input$distribucion == "pexp") {aux1 <- "Exponencialidad"}
      
      if (test()$p.value > 0.05) {aux2 <- paste("\nSus datos poseen una distribución de", aux1)}
      else {aux2 <- paste("\nSus datos NO poseen una distribución de", aux1,
                   "\nPruebe con otra variable o distribución para la prueba de bondad de ajuste")}

      paste("Prueba de bondad de ajuste:", aux1, 
            "\n Método: Kolmogorov-Smirnov Test",
            "\n Estadístico W = ", round(test()$statistic, 5),
            "\n Valor P = ", round(test()$p.value, 5),
            "\n", aux2)
    }
  })
  
  # Selección parámetros gráfico
  output$panelParam <- renderUI ({
    if (input$distribucion == 0 || is.null(input$distribucion) || test()$p.value < 0.05 ||
        input$variable == "Ninguna" || is.null(input$variable) || is.null(filedata())) {
      return(NULL)
    } else {
      if (input$distribucion == "pnorm") {perfilV <- c("Media", "Varianza")}
      else if (input$distribucion == "plnorm") {perfilV <- c("μ", "σ")}
      else if (input$distribucion == "pexp") {perfilV <- c("Media")}
       
      wellPanel(
        style = "background-color : White; border: 2px solid LightGray",
        selectInput("perfil", label = "Perfil de Verosimilitud", 
                    choices = c("Ninguno" = 0, perfilV)) %>%
          helper(buttonLabel = "Volver",
                 colour = "#2898EE",
                 type = "markdown",
                 content = "Intervalos/AyudaPaso4"),
        
        numericInput("alpha", label = "Nivel de Significancia", value = 0.05,
                     min = 0.01, max = 1, step = 0.01),
      )}})
  
  # Gráficos intervalos de verosimilitud ----
  output$plotPerfilV <- renderPlot ({
    if (is.null(filedata()) || input$variable == "Ninguna" || input$distribucion == 0 || input$perfil == 0 ||
        is.null(input$variable) || is.null(input$distribucion) || is.null(input$perfil) || test()$p.value < 0.05) {
      return(NULL)
    }
    
    muestra <- filedata()[input$variable]
    if (input$distribucion == "pnorm") {
      if (input$perfil == "Media") {icMediaN(unlist(muestra), alpha = input$alpha)}
      else if (input$perfil == "Varianza") {icVarianzaN(unlist(muestra), alpha = input$alpha)}
    } 
    else if (input$distribucion == "plnorm") {
      if (input$perfil == "μ") {icMediaLN(unlist(muestra), alpha = input$alpha)}
      else if (input$perfil == "σ") {icVarianzaLN(unlist(muestra), alpha = input$alpha)}
    }
    else if (input$distribucion == "pexp") {
      icMediaE(unlist(muestra), alpha = input$alpha)
    }
  })
}

# Ejecutar aplicación ----
shinyApp(ui = ui, server = server)
