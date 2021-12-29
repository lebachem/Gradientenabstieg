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
x1lim = c(-pi,0) # Entlang der X-Achse
x2lim = c(-0.5*pi,0.5*pi) # Entlang der Y-Achse
 ~~~ 
 
### Starwerte
x0 = c(-0.2,1)

### Schrittweite
gamma = 1

### Maximale Anzahl Schritte
max_steps = 30
