# -------------------------------------------------------------------------------------------------#
# Skript zur Darstellung des Gradientenverfahrens  ------------------------------------------------#  
# Am Beispiel des Verfahrens des steilsten Abstiegs -----------------------------------------------#
# Author: Michael Lebacher (michael.lebacher@gmx.de) ----------------------------------------------#
# -------------------------------------------------------------------------------------------------#


# Globale Parameter  ------------------------------------------------------------------------------# 

# Grenzen der Funktion für die wir uns interessieren
x1lim = c(-pi,0) # Entlang der X-Achse
x2lim = c(-0.5*pi,0.5*pi) # Entlang der Y-Achse

# Starwerte
x0 = c(-0.2,1)

# Schrittweite
gamma = 1

# Maximale Anzahl Schritte
max_steps = 30

# Definition von Funktionen  -----------------------------------------------------------------------#  

# Defx0ion der Funktion die minimiert werden soll
fun = function(x1,x2){
  #' Definition der zu minimierenden Funktion
  #' Input: x1, x2 beide floats als Argumente der Funktion
  #' Output: 1d float der den Funktionswert angibt
  
  return(sin(x1)*cos(x2))
}

# Defx0ion des Gradienten der Funktion
gradient = function(x1,x2){
  #' Gibt die partiellen Ableitungen der Funktion an 
  #' Input: x1, x2 beide floats als Argumente der Funktion
  #' Output: Vektor der partiellen Ableitungen

  dell_f_dell_x1=cos(x1)*cos(x2) # partielle Ableitung nach x1
  dell_f_dell_x2=-sin(x1)*sin(x2) # partielle Ableitung nach x2
  
  return(c(dell_f_dell_x1,dell_f_dell_x2))
}


gradient_descent=function(x0, # Startwert
                          gradient, # Funktion die den Gradienten definiert
                          fun, # Zu minimierende Funktion
                          x1lim, # Bereich entlang der x1-Achse der uns interesisert
                          x2lim, # Bereich entlang der x2-Achse der uns interesisert
                          gamma, # Schrittweite/Lernrate
                          max_steps=20, # Maximale Anzahl Schritte
                          iter_global=50, # Maximale Anzahl Schritte die auf den Plots gezeigt werden
                          plot_vec=FALSE, # Soll der nächste Schrittvektor angezeit werden? 
                          individual_vec=FALSE, # Sollen die Schritte Richtung x1 und x2 gezegt werden? 
                          opt_step=FALSE, # Soll die Schrittweite optimiert werden 
                          crit=0 # Konvergenzkriterium. 
                          ){
  
  # Speichert die Iteratinsschritte
  memory=c() 
  # Speichert den Funktionswert über die Iterationsschritte
  outcome=c()
  
  # Schleife über max_steps
  for (i in 1:max_steps){
    # Speichere die aktuelel Iteration
    memory = cbind(memory, x0)
    # Optimierung der Schrittweite
    if (opt_step==TRUE){
      gamma_=0.01
      mark=TRUE
      history = c()
      history_g = c()
      while (mark==TRUE){
        c = x0-gamma_*gradient(x0[1],x0[2])
        mark = fun(x0[1],x0[2]) - fun(c[1],c[2])>0
        history = c(history, fun(x0[1],x0[2]) - fun(c[1],c[2]))
        history_g = c(history_g,gamma_)
        gamma_ = gamma_*1.0001
      }
      gamma = history_g[history==max(history)]
    }
    
    # Anwendung der Iterationsvorschrift
    x0 = x0 - gamma*gradient(x0[1],x0[2])
    
    # Speichen des aktuellen Funktionswerts
    outcome = c(outcome,fun(x0[1],(x0[2])))
    
    # Falls der kritische Wert unterschritten ist, erfolgt Konvergenz
    if (crit>0){
      if (dim(memory)[2]>1){
      if(
        c(memory[,dim(memory)[2]]-memory[,dim(memory)[2]-1])%*%
        c(memory[,dim(memory)[2]]-memory[,dim(memory)[2]-1])<crit
      ){
        break
      }
    }
    }
  }
  
  # Definition eines Gitters für die Visualisierung
  x <- seq(from=x1lim[1],to=x1lim[2],by=0.01) # entlang der x1-Achse
  y <- seq(from=x2lim[1],to=x2lim[2],by=0.01) # entlang der x2-Achse
  z <- outer(x,y,fun) # entlang der Funktionswerte
   
  layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE))
  
  # Visualisierung der Konturplots
  plot(memory[1,],
       memory[2,],
       type='b',
       ylim=x2lim,
       xlim=x1lim,
       xlab=expression(x[1]),
       ylab = expression(x[2]),
       col='red'
       )
  
  contour(x, y, z,col = 'darkgray',nlevels=25,add=T)
  
  # Darstellung der Vektoren
  if (plot_vec==TRUE){
    
    x_0 = memory[1,dim(memory)[2]]
    y_0 = memory[2,dim(memory)[2]] 
    arrows(x0=x_0,
           y0=y_0,
           x1 =x_0-gamma*gradient(x_0,y_0)[1],
           y1 =y_0-gamma*gradient(x_0,y_0)[2],
           col=1,
           length=0.08
           )
  }
  
  if (individual_vec==TRUE){
    
    x_0 = memory[1,dim(memory)[2]]
    y_0 = memory[2,dim(memory)[2]] 
    arrows(x0=x_0,
           y0=y_0,
           x1=x_0-gamma*gradient(x_0,y_0)[1],
           y1=y_0,
           col="blue",
           length=0.08
           )
    
    arrows(x0=x_0,
           y0=y_0,
           x1=x_0,
           y1=y_0-gamma*gradient(x_0,y_0)[2],
           col="blue",
           length=0.08
           )
    
  }
  
  # Darstellung der Verläufe entlang der x1- und x2-Achse
  plot(memory[1,],
       type='b',
       ylim=c(min(memory[1,]),max(memory[1,])),
       xlim=c(0,iter_global),
       xlab=expression(n),
       ylab=expression('x'[1]),
       col='red'
       )
  
  plot(memory[2,],
       type='b',
       ylim=c(min(memory[2,]),max(memory[2,])),
       xlim=c(0,iter_global),
       xlab=expression(n),
       ylab=expression('x'[2]),
       col='red'
       )
  
  # Rückgabe der Iterationsschritte
  return(memory)
}

# Auswertung der Funktion  --------------------------------------------------------------------------#  

m=gradient_descent(x0=x0,
                   gradient=gradient,
                   fun=fun,
                   x1lim=x1lim,
                   x2lim=x2lim,
                   gamma=gamma,
                   max_steps=2,
                   iter_global=10,
                   crit=0.01,
                   plot_vec=TRUE,
                   individual_vec=TRUE
                   )