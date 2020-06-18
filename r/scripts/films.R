setwd("../datos")
library(sprof)
library(softImpute)
#https://cran.r-project.org/web/packages/softImpute/softImpute.pdf

films_csv <- read.csv("encuesta_peliculas.csv", header = TRUE, sep=",", na.strings = 0)#13x7
films = list.as.matrix(films_csv)
sum(is.na(films))
set.seed(101)
films_empty <- films
films_empty[sample(1:91,10,replace=FALSE)]=NA
sum(is.na(films_empty))

fits <- list()
films_complete <- list()
for (r in 1:6){#min dimension of data matrix
  fits[[r]]=softImpute(films_empty,trace=TRUE,type="svd",rank.max = r)
  films_complete[[r]] <- complete(films_empty, fits[[r]])
}
for (r in 1:6){#min dimension of data matrix
  aux <- films - films_complete[[r]]
  aux[is.na(aux)] <- 0
  print(r)
  print(round(sum(abs(aux)), 3))
  print(round(mean(abs(aux[aux!=0])), 3))
  print(round(min(abs(aux[aux!=0])), 3))
  print(round(max(abs(aux[aux!=0])), 3))
}
films_complete[[1]]
fits[[1]]
films_complete[[2]]
fits[[2]]
films_complete[[3]]
films_complete[[4]]
films_complete[[5]]
films_complete[[6]]



###############################################################
lam0 = lambda0(films, maxit=500)
fits=softImpute(copy,trace=TRUE,type="svd",rank.max = 2, lambda = 34)
films_complete <-complete(copy, fits)
sum(initial -films_complete)
copy[is.na(copy)] <- 0
diff = copy - films_complete

M = fits$u %*% diag(fits$d) %*% t(fits$v)
diff = copy-M
diff[is.na(diff)] <- 0
sum(diff^2)/2

fitsl <- list()
for (r in 0:6){
  fitsl[r] = softImpute(copy,trace=TRUE,type="svd",rank.max = r)
}



