#install.packages("readr")
#install.packages("pracma")

library(readr)
library(pracma)

#Lectura de los datos que vienen de los archivos 
datosItatira = read.csv(file = "Itatira.csv", header = TRUE, sep = ";")
datosSantaQuiteira = read.csv(file = "Santa Quiteira.csv",header = TRUE, sep = ";")

#Datos Originales
x = seq(from = 1, to = 720, by = 1)
y = datosItatira$Temp

plot(x,y,type='l', ylab = "Temperaturas", xlab = "Horas de medicion", main = 'Datos de la estación Itatira')
legend("topright",col="black",legend ="Información Original", lwd=3, bty = "n")

diasIdeales = datosItatira$Dia
horasIdeales = datosItatira$Hora
indicesIdeales = x

#Extraccion del 70% de los datos originales
ones = rep(1, 720)
eliminate = sample.int(720, 720*0.3)

for (e in eliminate) {
  ones[e] = 0
}

newX = c()
newY = c()
i = 1
j = 1

#Obtención de las nuevas coordenadas
for (o in ones) {
  if(o == 1)
  {
    newX[i] = x[j]
    newY[i] = y[j]
    i = i + 1
  }
  j = j +1
}

#Graficar 
plot(x,y,type='l', ylab = "Temperaturas", xlab = "Horas de medicion", main = 'Datos de la estación Itatira')
legend("topright",col=c("black","red"),legend =c("Información Original","Interpolación con splines cúbicos"), lwd=3, bty = "n")

lines(spline(newX,newY,n=200),col=2)

interpolado = splinefun(newX,newY)

arregloInterpolados = c()
k = 1

error = c()

for(var in eliminate)
{
  valorReal = y[var]
  valorInterpolado = interpolado(var)
  
  error = c(error, abs((valorReal - valorInterpolado)/valorReal))
}

max(error)
min(error)
mean(error)

print("Error con el 30% de la información eliminada: ")
print(error)

maximo = 0
media = 0

for (e in error) {
  
  if(e > maximo)
    maximo = e
  
  media = media + e
  
}

minimo = 200

for (e in error) {
  
  if(e < minimo)
    minimo = e
  
}

print("Error minimo: ")
print(minimo)

print("Error medio: ")
print(media/length(errorNuevaEstacion))

print("Error máximo: ")
print(maximo)
print("")








#Desde aquí empieza la otra parte del reto

arregloDeCalculos = c()

for (i in 1:length(datosSantaQuiteira$Dia)) 
{
  
  auxDia = datosSantaQuiteira$Dia[i]
  auxHora = datosSantaQuiteira$Hora[i]
  
  for(j in 1:720)
  {
    
    if((diasIdeales[j] == auxDia) && (horasIdeales[j] == auxHora))
    {
      arregloDeCalculos = c(arregloDeCalculos,indicesIdeales[j])
    }
  }
}

plot(arregloDeCalculos,datosSantaQuiteira$Temp,type='l', ylab = "Temperaturas", xlab = "Horas de medicion", main = 'Datos de la estación Santa Quiteira')
legend("topright",col="black",legend ="Información Original", lwd=3, bty = "n")

nuevosY = c()

errorNuevaEstacion = c()

z = 1
for (variable in arregloDeCalculos) 
{
  nuevosY = c(nuevosY, interpolado(variable))
  errorNuevaEstacion = c(errorNuevaEstacion, abs((datosSantaQuiteira$Temp[z] - nuevosY[z])/datosSantaQuiteira$Temp[z]))
  z = z + 1
}

plot(arregloDeCalculos,datosSantaQuiteira$Temp, ylab = "Temperaturas", xlab = "Indices Calculados", type = 'l', main = 'Datos de la estación Santa Quiteira')
legend("topright",col=c("black","green"),legend =c("Información Original","Interpolación con splines cúbicos"), lwd=3, bty = "n")

lines(arregloDeCalculos, nuevosY, col = 3)

print("Errores calculados en la nueva estación: ")
print(errorNuevaEstacion)

maximo = 0

media = 0

for (error in errorNuevaEstacion) {
  
  if(error > maximo)
    maximo = error
  
  media = media + error
  
}

minimo = 200

for (error in errorNuevaEstacion) {
  
  if(error < minimo)
    minimo = error
  
}

qqnorm(errorNuevaEstacion)
qqline(errorNuevaEstacion)

print("Error minimo: ")
print(minimo)

print("Error medio: ")
print(media/length(errorNuevaEstacion))

print("Error máximo: ")
print(maximo)