mu<-20
sigma<-5
y <- rlnorm(n=50, mean=mu, sd=sigma)

require(MASS) # El paquete ya está instalado, solo se debe cargar
res <- fitdistr(y, densfun='lognormal')
res



graf_mv_varianza <- function(x){
  
  # Función de verosimilitud relativa
  
  n<-length(x)
  
  x_barra <- mean(log(x))
  
  s2_x_barra <- (n-1)/n * var(log(x))
  
 
  R_varianza <- function(sigma){
    
    return(exp(n/2 * ( log(s2_x_barra/sigma^2) + 1-s2_x_barra/sigma^2))) 
    
  }
  
  # valores minimos y máximos para el gráfico
  x_min <- 0
  x_max <- 2*sqrt(s2_x_barra)
  
  # Se obtiene el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_varianza(eje_x)
  
  
  # Gráfico
    
    plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n",  xaxt = "n", xlim = c(0, x_max))
    axis(1, at = round(c(sqrt(s2_x_barra)), 3), las = 2)
    title(main = "Perfil de Verosimilitud para sigma de una Lognormal")
    grid()
    abline(v = sqrt(s2_x_barra), col ="red", lty=3, lwd = 3)
    abline(h = 0)

  

}


graf_mv_varianza (y)
