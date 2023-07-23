mu<-100
sigma<-2
n<-50
y <- rlnorm(n, mean=mu, sd=sigma)


graf_mv_varianza <- function(x, li, ls, alpha){
  
  x_barra<-mean(log(x))
  
  desv<-sd(log(x))
  
  s2_x_barra <- (n-1)/n * var(log(x))
  
  R_varianza <- function(sigma){
    
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
  
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n",  xaxt = "n", xlim = c(li - (ls-li)/4, ls + (ls-li)/4))
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  text(li - (ls-li)/6,p+0.05, labels = paste("Confianza ",(1-alpha)*100, "%", sep=""))
  axis(1, at = round(c(li, sqrt(s2_x_barra), ls), 3), las = 2)
  title(main = "Intervalo de confianza máximo verosimil para sigma")
  grid()
  abline(v=sqrt(s2_x_barra), lty=3)
  abline(h = p, lty=3,col="blue",lwd=3)
  abline(h = 0)
  
  
  # grafico <- grDevices::recordPlot()
  
}


ic_mv_Var <- function(x, alpha){
  
  # Calculo del intervalo ---------------------------------------------------
  
  x_barra<-mean(log(x))
  
  desv<-sd(log(x))
  
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  
  
  s2_x_barra <- (n-1)/n * var(log(x))
  
 
  R_varianza <- function(sigma){
    
    return(n/2 * ( log(s2_x_barra/sigma^2) + 1-s2_x_barra/sigma^2)-log(p)) 
    
  }
  
  mv_li <- uniroot( R_varianza, c(0.00001, sqrt(s2_x_barra)), tol = 0.000001)$root
  mv_ls <- uniroot( R_varianza, c(sqrt(s2_x_barra), 999999), tol = 0.000001)$root
  
  
 
  # Obtener gráfica ---------------------------------------------------------
  
  graf_mv_varianza(y,mv_li,mv_ls,alpha)
  
  }


ic_mv_Var(y, alpha=0.05)


