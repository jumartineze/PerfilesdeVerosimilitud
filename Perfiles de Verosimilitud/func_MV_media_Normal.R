mu<-20
sigma<-10
y <- rnorm(n=50, mean=mu, sd=sigma)

 require(MASS) # El paquete ya está instalado, solo se debe cargar
 res <- fitdistr(y, densfun='normal')
 res
 

 graf_mv_media <- function(x){
   
   x_barra<-mean(x)
   
   desv<-sd(x)
   
   n<-length(x)
   
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
   

   plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n",  xaxt = "n", xlim= c(x_barra-desv, x_barra+desv))
   axis(1, at = round(c( x_barra), 3), las = 2)
   title(main = "Perfil de Verosimilitud para la media de una Normal")
   grid()
   abline(v = x_barra, col ="red", lty=3, lwd = 3)
   abline(h = 0)
   
   
   
   # grafico <- grDevices::recordPlot()
   
 }
 
 
 
 
 graf_mv_media(y)
   