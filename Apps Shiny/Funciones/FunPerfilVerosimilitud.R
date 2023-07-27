# Funciones para para perfiles de verosimilitud --------------------------------

# Gráfico ----

grafico <- function(estimador, eje_x, eje_y, titulo, xlim, titulo_x) {
  options(warn=-1)
  ggplot() +
    geom_area(aes(x = eje_x, y = eje_y), 
              fill = "#0cbccc", alpha = 0.5, color = "#2898ee", lwd = 1.5) +
    xlim(xlim[1], xlim[2]) +
    labs(title = titulo, y = "Probabilidad", x = titulo_x) +
    geom_vline(xintercept = estimador, color = "#107acc", lwd = 1, alpha = 0.8) +
    geom_text(aes(estimador, 0, label = round(estimador, 3)), color = "#142157", size = 5) +
    theme_bw() +
    theme(plot.title = element_text(color="#15297c", size=17, face="bold.italic", hjust=0.5),
          axis.title.x = element_text(color="#15297c", size=15, face="bold"),
          axis.title.y = element_text(color="#15297c", size=15, face="bold"))
}

# Verosimilitud Media Normal ----
mvMediaN <- function(x) {
  
  x_barra <- mean(x)
  desv <- sd(x)
  n <- length(x)
  var_estimada <- (n-1)/n * desv^2
  
  R_mu <- function(mu) {
    return(exp(n/2 * log(var_estimada/(var_estimada + (x_barra - mu)^2))))
  }
  
  # Valores mínimos y máximos para el gráfico de la normal
  x_min <- qnorm(0.0001, mean = x_barra, sd = desv)
  x_max <- qnorm(0.9999, mean = x_barra, sd = desv)
  
  # Se simula el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max - x_min)/1000)
  eje_y <- R_mu(eje_x)
  
  # Gráfico
  grafico(x_barra, eje_x, eje_y, "Perfil de Verosimilitud: Media de una Normal", 
          c(x_barra - desv, x_barra + desv), "μ")
}

# Verosimilitud Varianza Normal ----
mvVarianzaN <- function(x) {
  
  # Función de verosimilitud relativa
  n<-length(x)
  x_barra <- mean(x)
  s2_x_barra <- (n-1)/n * var(x)
  
  R_varianza <- function(varianza){
    return(exp(n/2 * (log(s2_x_barra/varianza) + 1-s2_x_barra/varianza))) 
  }
  
  # Valores mínimos y máximos para el gráfico
  x_min <- 0
  x_max <- 2*s2_x_barra
  
  # Se obtiene el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_varianza(eje_x)
  
  # Gráfico
  grafico(s2_x_barra, eje_x, eje_y, "Perfil de Verosimilitud: Varianza de una Normal", 
          c(0, x_max), "σ²")
}

# Verosimilitud Media Log-Normal ----
mvMediaLN <- function(x){
  
  x_barra <- mean(log(x))
  n <- length(x)
  desv <- sd(log(x))
  var_estimada <- (n-1)/n * var(log(x))
  
  R_mu <- function(mu){
    return(exp(n/2 * log(var_estimada/(var_estimada + (x_barra - mu)^2))))
  }
  
  # Valores mínimos y máximos para el gráfico de la normal
  x_min <- 0
  x_max <- 2*x_barra
  
  # Se simula el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_mu(eje_x)
  
  # Gráfico
  grafico(x_barra, eje_x, eje_y, "Perfil de Verosimilitud para el párametro μ", 
          c(x_barra - desv, x_barra + desv), "μ")
}

# Verosimilitud Varianza Log-Normal ----
mvVarianzaLN <- function(x) {
  
  # Función de verosimilitud relativa
  n <- length(x)
  x_barra <- mean(log(x))
  s2_x_barra <- sqrt((n-1)/n * var(log(x)))
  
  R_varianza <- function(varianza) {
    return(exp(n/2 * (log(s2_x_barra/varianza) + 1-s2_x_barra/varianza))) 
  }
  
  # Valores mínimos y máximos para el gráfico
  x_min <- 0
  x_max <- 2*s2_x_barra
  
  # Se obtiene el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_varianza(eje_x)
  
  # Gráfico
  grafico(s2_x_barra, eje_x, eje_y, "Perfil de Verosimilitud para el parámetro σ", 
          c(0, x_max), "σ")
}

# Verosimilitud Media Exponencial ----
mvMediaE <- function(x) {
  
  # Función de verosimilitud relativa
  n <- length(x)
  x_barra <- mean(x)
  
  R_media <- function(media){  
    return(exp(n * (1-x_barra/media))*(x_barra/media)^n) 
  }
  
  # Valores mínimos y máximos para el gráfico
  x_min <- 0
  x_max <- 2*x_barra
  
  # Se obtiene el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_media(eje_x)
  
  # Gráfico
  grafico(x_barra, eje_x, eje_y, "Perfil de Verosimilitud: Media de una Exponencial", 
          c(0, x_max), "β")
}