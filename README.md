# Gradientenabstieg
In diesem Repository befindet sich eine R File die es erlaubt das Gradientenabstiegsverfahren: Verfahren des steilsten Abstiegs mithilfe einer beliebigen 2-dimensionalen Funktion zu illustrieren.

## Getting Started
Installiere R auf deinem Rechner

https://www.r-project.org/foundation/

gegebenenfalls auch noch R Studio

https://www.rstudio.com/

Damit kann das Skript ausgeführt werden.

## Benutzung

 Die dazugehörigen Parameter werden direkt im Skript gesetzt.

### Grenzen der Funktion für die wir uns interessieren
 ~~~ 
x1lim = c(-pi,0) # Entlang der X1-Achse
x2lim = c(-0.5*pi,0.5*pi) # Entlang der X2-Achse
 ~~~ 
 
### Starwerte
~~~ 
x0 = c(-0.2,1)
~~~ 

### Schrittweite
~~~ 
gamma = 1
~~~ 

### Maximale Anzahl Schritte
~~~ 
max_steps = 30
~~~ 

## Veränderung der Funktion

Im Skript ist die Funktion die mithilfe des Verfahrens des steilsten Abstiegs minimiert werden soll mit 

~~~ 
fun = function(x1,x2){
  #' Definition der zu minimierenden Funktion
  #' Input: x1, x2 beide floats als Argumente der Funktion
  #' Output: 1d float der den Funktionswert angibt
  
  return(sin(x1)*cos(x2))
}
~~~ 
und der Gradient mit
~~~ 
gradient = function(x1,x2){
  #' Gibt die partiellen Ableitungen der Funktion an 
  #' Input: x1, x2 beide floats als Argumente der Funktion
  #' Output: Vektor der partiellen Ableitungen

  dell_f_dell_x1=cos(x1)*cos(x2) # partielle Ableitung nach x1
  dell_f_dell_x2=-sin(x1)*sin(x2) # partielle Ableitung nach x2
  
  return(c(dell_f_dell_x1,dell_f_dell_x2))
}
~~~ 
angegeben. Wenn eine andere Funktion minimiert werden soll, so reicht es die Funktion und den Gradienten im Code zu verändern.
