mu<-100
sigma<-100
n<-50
y <- rnorm(n, mean=mu, sd=sigma)


graf_mv_media <- function(x, li, ls, alpha){
  
  x_barra<-mean(x)
  
  desv<-sd(x)
  
  var_estimada <- (n-1)/n * desv^2
  
  R_mu <- function(mu){
    
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
  
  
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n",  xaxt = "n", xlim= c(li - (ls-li)/4, ls + (ls-li)/4))
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(h = p, lty=3,col="blue",lwd=3)
  text(li - (ls-li)/6,p+0.05, labels = paste("Confianza", (1-alpha)*100,"%", sep=" "))
  axis(1, at = round(c(li, x_barra, ls), 3), las = 2)
  title(main = "Intervalo de confianza máximo verosimil para mu")
  grid()
  abline(v=x_barra, lty=3)
  abline(h = 0)
  
  
  
  # grafico <- grDevices::recordPlot()
  
}



ic_mv_media <- function(x, alpha){
  
  # Calculo del intervalo ---------------------------------------------------
  
  x_barra<-mean(x)
  
  desv<-sd(x)
  
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  
  
  var_estimada <- (n-1)/n * desv^2
  
  rmax_mu <- function(mu){
    
    n/2 * log(var_estimada/(var_estimada + (x_barra - mu)^2)) - log(p)
    
  }
  
    mv_li <- uniroot(rmax_mu, c(-999999, x_barra), tol = 0.000001)$root
    mv_ls <- uniroot(rmax_mu, c(x_barra, 999999), tol = 0.000001)$root
    

  # Obtener gráfica ---------------------------------------------------------
  
  graf_mv_media(y,mv_li,mv_ls,alpha)
  
  }


ic_mv_media(y, alpha=0.05)


