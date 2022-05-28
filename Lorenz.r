## Un pequeño script de R, para reproducir la famosa
## animacion del atractor de Lorenz

## Cargamos los paquetes necesarios

library(magick)        ## para unir la imagenes como un gif
library(scatterplot3d) ## para crear graficos en R^3

## Parametros, para crear la "Mariposa" se tomar algunos valores por defecto
a <- 10
b <- 28
c <- 8/3 
dt <- 0.02

## creamos las funciones
dx <- function(v)  a*(v[2] - v[1])
dy <- function(v)  v[1]*(b - v[3]) - v[2]
dz <- function(v)  v[1]*v[2] - c*v[3] 

## creamos una matriz, donde el primer renglon son los valores iniciales (1, 0, 0)
data <- data.frame(x = c(1, rep(0, 500)), y = rep(0, 501), z = rep(0, 501)  )

for( i in 2:501){
    data[i, ] <- c( dt*dx( data [ i-1 ,] ) + data[i -1, 1], 
                    dt*dy( data [ i-1 ,] ) + data[i -1, 2],
                    dt*dz( data [ i-1 ,] ) + data[i -1, 3]
                  )
}


s <- seq(10, 500, by = 10)

## creamos n graficos del atractor para despues unirlas
png('lorenz%02d.png')
for( i in s){
    scatterplot3d(data[1:i, ], type = "b", lwd = 1, color = rgb(.4, .4, .6, .9), box = F  )
}
dev.off()

## Buscamos y unimos las imagenes en un gif
imgs <- list.files(getwd(), full.names = TRUE)
imgs <- imgs[ grep("png", imgs) ]
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 10)
img_animated

## Lo guardamos
image_write(img_animated, path = "Lorenz.gif", format = "gif")

## por ultimo borramos los n graficos generados
file.remove(list.files(pattern=".png"))


