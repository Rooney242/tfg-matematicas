data <- na.omit(Education_vs_female_rights)
education <- data[,2:8]
rights <- data[,9:13]
CC <- cancor(education, rights)
CC$xcoef
CC$ycoef
CC$xcenter
CC$cor
