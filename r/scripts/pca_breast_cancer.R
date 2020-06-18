setwd("../datos")

#Cargamos los datos. Utilizaremos solo las medias y tenemos que diferenciar
# entre los benignos y los malignos
raw_data <- read.csv("breast-cancer.csv", header = TRUE, sep=",")
breast_cancer <- subset(raw_data, select = endsWith(colnames(raw_data), "mean"))
breast_cancer_b <- subset(raw_data, diagnosis == "B",
                          select = endsWith(colnames(raw_data), "mean"))
breast_cancer_m <- subset(raw_data, diagnosis == "M",
                          select = endsWith(colnames(raw_data), "mean"))
#Vectores de medias
# breast_cancer <- scale(breast_cancer, scale = FALSE)
# breast_cancer_b <- scale(breast_cancer_b, scale = FALSE)
#breast_cancer_m <- scale(breast_cancer_m, scale = FALSE)
mean = colMeans(breast_cancer)
mean_b = colMeans(breast_cancer_b)
mean_m = colMeans(breast_cancer_m)

#Matrices de varianza
S = var(breast_cancer)
S_b = var(breast_cancer_b)
S_m = var(breast_cancer_m)

#PCA de los benignos
pca_b <- prcomp(breast_cancer_b, scale. = TRUE)
diag(S_b)
summary(pca_b)
pca_b$sdev
pca_b$center
pca_b$rotation

#Graficamos los resultados de aplicar el pca de benignos a los malignos
pcab_malpoints <- predict(pca_b, newdata=breast_cancer_m)
plot(pcab_malpoints[,1:3], pch = 7, col = "red")
points(pca_b$x, pch = 1, col="green")

#PCA de los malignos
pca_m <- prcomp(breast_cancer_m, scale. = TRUE)
diag(S_m)
summary(pca_m)
pca_m$sdev
pca_m$center
pca_m$rotation

#Graficamos los resultados de aplicar el pca de malignos a los benignos
pcam_benpoints <- predict(pca_m, newdata=breast_cancer_b)
plot(pcam_benpoints[,1:3], pch = 1, col = "green")
points(pca_m$x, pch = 7, col="red")


#Conclusiones:
# Tanto en el análisis por componentes principales en los datos de células
#   benignas como malignas se han extraido conclusiones de las dos primeras
#   componentes principales. Para los benignos acumulan el 71.15% y para los
#   malignos el 80.38%. 
#   Se ha dado el caso de poder identificar dos de las componentes principales y 
#   analizar los autovectores en cada caso. Sin embargo, las componentes de las
#   que se va a hablar aparecen en orden inverso en los benignos y los malignos.
#   Esto nos sirve tambien para observar la importancia de ambas y la variabilidad que
#   existe entre los dos tipos de células.
#
#   La primera componente principal (segunda en los benignos) se puede entender como
#   la componente de tamaño. Todas los parametros aportan en el mismo sentido (salvo
#   la textura que apenas tiene peso) con los respectivos pesos correspondientes a la
#   tamaño de la celula.
#
#   La segunda componente principal (primera en los benignos) la podemos definir
#   como la componente de forma dado que en sentido positivo aportan a parametros 
#   como el radio, perimetro y area (que tienen que ver con el tamaño);
#   mientras que en setido negativo aportan a los valores que tienen que ver con
#   la forma, concavidad e irregularidad de la celula
#
#   Como conclusión podemos decir que las dos primeras componentes principales dan 
#   como prioridad el tamaño general de las celulas y su forma, diferenciando entre
#   más redondeada o con un mayor número de concavidades.
