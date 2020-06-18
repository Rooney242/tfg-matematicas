setwd("../datos")

#Cargamos los datos
birds <- read.csv("birds.csv", header = TRUE, sep=",")
types <- unique(birds$type)
colnames <- colnames(birds)
types

#Separamos cada tipo de pajaro
birds_by_type <- list()
for (type_s in types){
  birds_by_type[[type_s]] <- subset(birds, type == type_s, select = c(-id,-type))
  birds_by_type[[type_s]] <- na.omit(birds_by_type[[type_s]])
}

#Analizamos cada tipo y guardamos los resultados
#Analisis de la varianza
S = list()
R = list()
for (type_s in types){
  S[[type_s]] = var(birds_by_type[[type_s]])
  R[[type_s]] = cor(birds_by_type[[type_s]])
}

#Realizamos el PCA de todo
pca = list()
for (type_s in types){
  pca[[type_s]] <- prcomp(birds_by_type[[type_s]], scale. = TRUE)
}

#SO is the larger type
summary(pca$SO)
pca$SO$sdev
pca$SO$center
pca$SO$rotation


#Dado el valor de longitud y diametro vamos a diferenciar entre los dos
birds_length <- list()
birds_diameter <- list()
for (type_s in types){
  birds_length[[type_s]] <- subset(birds_by_type[[type_s]],
                                   select = endsWith(colnames(birds_by_type[[type_s]]), "l"))
  birds_diameter[[type_s]] <- subset(birds_by_type[[type_s]],
                                     select = endsWith(colnames(birds_by_type[[type_s]]), "w"))
}

#Hacemos el analisis pca de cada uno por separado
pca_l <- list()
pca_d <- list()
for (type_s in types){
  pca_l[[type_s]] <- prcomp(birds_length[[type_s]])
  pca_d[[type_s]] <- prcomp(birds_diameter[[type_s]])
}

pca_l$SO
pca_l$SO$rotation
pca_d$SO$rotation

#Graficamos los resultados en funcion del pca de SO
pca_points_l <- list()
pca_points_d <- list()
colors <- list("SW" = "blue", 
               "W" = "red",
               "T" = "yellow",
               "R" = "green",
               "P" = "pink",
               "SO"= "black")
for (type_s in types){
  pca_points_l[[type_s]] <- predict(pca_l$SO, newdata=birds_by_type[[type_s]], scale. = FALSE)
  pca_points_d[[type_s]] <- predict(pca_d$SO, newdata=birds_by_type[[type_s]], scale. = FALSE)
  points(pca_points_l[[type_s]][,1:3], col = colors[[type_s]])
}

plot(pca_l$SO$x, col=colors$SO)
points(pca_points_l$SW[,1:3], col = "red")#Calcular minimo y maximo para que se vean todos los puntos
#xlim ylim en plot
for (type_s in types){
  points(pca_points_l[[type_s]][,1:3], col = colors[[type_s]])
}

plot(pca_d$SO$x, col=colors$SO)
for (type_s in types){
  points(pca_points_d[[type_s]][,1:3], col = colors[[type_s]])
}

#Conclusiones
# Se han dividido los datos en clases de pajaros y, después, en funcion de si el parámetro
# media la longitud o el diametro del hueso en cuestión.
# 
# Viendo la matriz de covarianzas vemos que todas son positivas, por lo tanto por el teorema
# de Penron sabemos que el primer autovector será positivo y será el único que tendrá todas
# sus componentes positivas. Esta primera componente principal sera la que mida el tamaño 
# general de los pajaros, además, al escalar se puede observar que todas los parametros aportan
# igual.
# 
# La segunda componente diferencia entre los huesos de las alas y de las patas. Se puede observar
# que el húmero no es especialmente significativo, y con esta componente diferenciaremos entre
# individuos con descompensación entre el tamaño relativo de sus alas y patas