mean<-100
y <- rexp(n=50, rate=1/mean)

require(MASS) # El paquete ya está instalado, solo se debe cargar
res <- fitdistr(y, densfun='exponential')
res



graf_mv_media <- function(x){
  
  # Función de verosimilitud relativa
  
  n<-length(x)
  
  x_barra <- mean(x)
  
  R_media <- function(media){
    
    return(exp(n * ( 1-x_barra/media))*(x_barra/media)^n) 
    
  }
  
  # valores minimos y máximos para el gráfico
  x_min <- 0
  x_max <- 2*x_barra
  
  # Se obtiene el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_media(eje_x)
 
  
  # Gráfico
    
    plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n",  xaxt = "n", xlim = c(0, x_max))
    axis(1, at = round(c(x_barra), 3), las = 2)
    title(main = "Perfil de Verosimilitud para media de una exponencial")
    grid()
    abline(v = x_barra, col ="red", lty=3, lwd = 3)
    abline(h = 0)

}


graf_mv_media (y)

