# Funciones para para perfiles de verosimilitud --------------------------------

# Gráfico ----
grafico <- function(estimador, eje_x, eje_y, titulo, xlim, li, ls, x_ic, y_ic, p, alpha, titulo_x) {
  options(warn=-1)
  ggplot() +
    geom_area(aes(x = eje_x, y = eje_y), 
              fill = "#0cbccc", alpha = 0.5, color = "#2898ee", lwd = 1.5) +
    geom_area(aes(x = x_ic, y = y_ic),
              fill = "#3f1fc0", alpha = 0.8, color = "#142157", lwd = 1.5) +
    xlim(xlim[1], xlim[2]) +
    labs(title = titulo, y = "Probabilidad", x = titulo_x) +
    geom_hline(yintercept = p, color = "#2898ee", lwd = 1.5, linetype = "dashed") +
    geom_vline(xintercept = estimador, color = "#107acc", lwd = 1.5, alpha = 1) +
    geom_text(aes(li - (ls-li)/6, p+0.02, label = paste("Confianza", 1-alpha, "%"), fontface = "bold"), color = "#142157", size = 4) +
    geom_text(aes(estimador, -0.015, label = round(estimador, 3), fontface = "bold"), color = "#142157", size = 5) +
    geom_text(aes(li, -0.015, label = round(li, 3), fontface = "bold"), color = "#142157", size = 4) +
    geom_text(aes(ls, -0.015, label = round(ls, 3), fontface = "bold"), color = "#142157", size = 4) +
    theme_bw() +
    theme(plot.title = element_text(color="#15297c", size=17, face="bold.italic", hjust=0.5),
          axis.title.x = element_text(color="#15297c", size=15, face="bold"),
          axis.title.y = element_text(color="#15297c", size=15, face="bold"))
}

# Verosimilitud Media Normal ----
mvMediaN <- function(x, li, ls, alpha) {
  
  n <- length(x)
  x_barra <- mean(x)
  desv <- sd(x)
  var_estimada <- (n-1)/n * desv^2
  
  R_mu <- function(mu) {
    return(exp(n/2 * log(var_estimada/(var_estimada + (x_barra - mu)^2))))
  }
  
  # Valores mínimos y máximos para el gráfico de la normal
  x_min <- qnorm(0.0001, mean = x_barra, sd = desv)
  x_max <- qnorm(0.9999, mean = x_barra, sd = desv)
  
  # Se simula el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_mu(eje_x)
  
  x_ic <- c(li, seq(li, ls, by=(ls-li)/1000), ls)
  y_ic <- c(0, R_mu(seq(li, ls, by=(ls-li)/1000)), 0) 
  
  p <- exp(-1 * qchisq( 1 - alpha, df = 1)/2)
  titulo <- "Intervalo de confianza máximo verosimil para μ"
  xlim <- c(li - (ls-li)/4, ls + (ls-li)/4)
  
  grafico(x_barra, eje_x, eje_y, titulo, xlim, li, ls, x_ic, y_ic, p, alpha, "μ")
}

icMediaN <- function(x, alpha) {
  
  # Calculo del intervalo ---------------------------------------------------
  n <- length(x)
  x_barra <- mean(x)
  desv <- sd(x)
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  var_estimada <- (n-1)/n * desv^2
  
  rmax_mu <- function(mu) {
    n/2 * log(var_estimada/(var_estimada + (x_barra - mu)^2)) - log(p)
  }
  
  mv_li <- uniroot(rmax_mu, c(-999999, x_barra), tol = 0.000001)$root
  mv_ls <- uniroot(rmax_mu, c(x_barra, 999999), tol = 0.000001)$root
  
  # Obtener gráfica ---------------------------------------------------------
  mvMediaN(x, mv_li, mv_ls, alpha)
}

# Verosimilitud Varianza Normal ----
mvVarianzaN <- function(x, li, ls, alpha) {
  
  n <- length(x)
  x_barra <- mean(x)
  desv <- sd(x)
  s2_x_barra <- (n-1)/n * var(x)
  
  R_varianza <- function(varianza) {
    return(exp(n/2 * ( log(s2_x_barra/varianza) + 1-s2_x_barra/varianza))) 
  }
  
  # valores mínimos y máximos para el gráfico
  x_min <- 0
  x_max <- ls + (ls-li)/4
  
  # Se obtiene el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_varianza(eje_x)
  
  # Se crean los puntos para pintar el área del intervalo
  x_ic <- c(li, seq(li, ls, by=(ls-li)/1000), ls)
  y_ic <- c(0, R_varianza(seq(li, ls, by=(ls-li)/1000)), 0)
  
  p <- exp(-1 * qchisq( 1 - alpha, df = 1)/2)
  titulo <- "Intervalo de confianza máximo verosimil para σ²"
  xlim <- c(li - (ls-li)/4, ls + (ls-li)/4)
  
  grafico(s2_x_barra, eje_x, eje_y, titulo, xlim, li, ls, x_ic, y_ic, p, alpha, "σ²")
}

icVarianzaN <- function(x, alpha) {
  
  # Calculo del intervalo ---------------------------------------------------
  n <- length(x)
  x_barra <- mean(x)
  desv <- sd(x)
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  s2_x_barra <- (n-1)/n * var(x)
  
  R_varianza <- function(varianza) {
    return(n/2 * ( log(s2_x_barra/varianza) + 1-s2_x_barra/varianza)-log(p)) 
  }
  
  mv_li <- uniroot( R_varianza, c(0.00001, s2_x_barra), tol = 0.000001)$root
  mv_ls <- uniroot( R_varianza, c(s2_x_barra, 999999), tol = 0.000001)$root
  
  # Obtener gráfica ---------------------------------------------------------
  mvVarianzaN(x, mv_li, mv_ls, alpha)
}

# Verosimilitud Media Log-Normal ----
mvMediaLN <- function(x, li, ls, alpha) {
  
  n <- length(x)
  x_barra <- mean(log(x))
  desv <- sd(log(x))
  var_estimada <- (n-1)/n * desv^2
  
  R_mu <- function(mu) {
    return(exp(n/2 * log(var_estimada/(var_estimada + (x_barra - mu)^2))))
  }
  
  # valores minimos y máximos para el gráfico de la normal
  x_min <- qnorm(0.0001, mean = x_barra, sd = desv)
  x_max <- qnorm(0.9999, mean = x_barra, sd = desv)
  
  # Se simula el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_mu(eje_x)
  
  x_ic <- c(li, seq(li, ls, by=(ls-li)/1000), ls)
  y_ic <- c(0, R_mu(seq(li, ls, by=(ls-li)/1000)), 0) 
  
  p <- exp(-1 * qchisq( 1 - alpha, df = 1)/2)
  titulo <- "Intervalo de confianza máximo verosimil para μ"
  xlim <- c(li - (ls-li)/4, ls + (ls-li)/4)
  
  grafico(x_barra, eje_x, eje_y, titulo, xlim, li, ls, x_ic, y_ic, p, alpha, "μ")
}

icMediaLN <- function(x, alpha) {
  
  # Calculo del intervalo ---------------------------------------------------
  n <- length(x)
  x_barra <- mean(log(x))
  desv <- sd(log(x))
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  var_estimada <- (n-1)/n * desv^2
  
  rmax_mu <- function(mu) {
    n/2 * log(var_estimada/(var_estimada + (x_barra - mu)^2)) - log(p)
  }
  
  mv_li <- uniroot(rmax_mu, c(-999999, x_barra), tol = 0.000001)$root
  mv_ls <- uniroot(rmax_mu, c(x_barra, 999999), tol = 0.000001)$root
  
  # Obtener gráfica ---------------------------------------------------------
  mvMediaLN(x, mv_li, mv_ls, alpha)
}

# Verosimilitud Varianza Log-Normal ----
mvVarianzaLN <- function(x, li, ls, alpha) {
  
  n <- length(x)
  x_barra <- mean(log(x))
  desv <- sd(log(x))
  s2_x_barra <- (n-1)/n * var(log(x))
  
  R_varianza <- function(sigma) {
    return(exp(n/2 * ( log(s2_x_barra/sigma^2) + 1-s2_x_barra/sigma^2))) 
  }
  
  # valores minimos y máximos para el gráfico
  x_min <- li - (ls-li)/4
  x_max <- ls + (ls-li)/4
  
  # Se obtiene el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_varianza(eje_x)
  
  # Se crean los puntos para pintar el área del intervalo
  x_ic <- c(li, seq(li, ls, by=(ls-li)/1000), ls)
  y_ic <- c(0, R_varianza(seq(li, ls, by=(ls-li)/1000)), 0)
  
  p <- exp(-1 * qchisq( 1 - alpha, df = 1)/2)
  titulo <- "Intervalo de confianza máximo verosimil para σ"
  xlim <- c(li - (ls-li)/4, ls + (ls-li)/4)
  
  grafico(sqrt(s2_x_barra), eje_x, eje_y, titulo, xlim, li, ls, x_ic, y_ic, p, alpha, "σ")
}

icVarianzaLN <- function(x, alpha) {
  
  # Calculo del intervalo ---------------------------------------------------
  n <- length(x)
  x_barra <- mean(log(x))
  desv <- sd(log(x))
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  s2_x_barra <- (n-1)/n * var(log(x))
  
  R_varianza <- function(sigma) {
    return(n/2 * ( log(s2_x_barra/sigma^2) + 1-s2_x_barra/sigma^2)-log(p)) 
  }
  
  mv_li <- uniroot( R_varianza, c(0.00001, sqrt(s2_x_barra)), tol = 0.000001)$root
  mv_ls <- uniroot( R_varianza, c(sqrt(s2_x_barra), 999999), tol = 0.000001)$root
  
  # Obtener gráfica ---------------------------------------------------------
  mvVarianzaLN(x, mv_li, mv_ls, alpha)
}

# Verosimilitud Media Exponencial ----
mvMediaE <- function(x, li, ls, alpha) {
  
  n <- length(x)
  x_barra <- mean(x)
  desv <- sd(x)
  var_estimada <- (n-1)/n * desv^2
  
  R_mu <- function(media) {
    return(exp(n * ( 1-x_barra/media))*(x_barra/media)^n) 
  }
  
  # valores minimos y máximos para el gráfico de la normal
  x_min <- 0
  x_max <- 2*x_barra
  
  # Se obtiene el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_mu(eje_x)
  
  x_ic <- c(li, seq(li, ls, by=(ls-li)/1000), ls)
  y_ic <- c(0, R_mu(seq(li, ls, by=(ls-li)/1000)), 0) 
  
  p <- exp(-1 * qchisq( 1 - alpha, df = 1)/2)
  titulo <- "Intervalo de confianza máximo verosimil para β"
  xlim <- c(li - (ls-li)/4, ls + (ls-li)/4)
  
  grafico(x_barra, eje_x, eje_y, titulo, xlim, li, ls, x_ic, y_ic, p, alpha, "β")
}

icMediaE <- function(x, alpha) {
  
  # Calculo del intervalo ---------------------------------------------------
  n <- length(x)
  x_barra<-mean(x)
  desv <- sd(x)
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  var_estimada <- (n-1)/n * desv^2
  
  rmax_mu <- function(mu) {
    n * ( 1-x_barra/mu)+n*log(x_barra/mu) -log(p)
  }
  
  mv_li <- uniroot(rmax_mu, c(0.001, x_barra), tol = 0.000001)$root
  mv_ls <- uniroot(rmax_mu, c(x_barra, 999999), tol = 0.000001)$root
  
  # Obtener gráfica ---------------------------------------------------------
  mvMediaE(x, mv_li, mv_ls, alpha)
}