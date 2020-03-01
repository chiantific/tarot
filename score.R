# calcul du score basé sur le contrat
score <- function(data){
    calcul <- as.numeric(data[4])+25

    if(data[5] != "faite") {
        calcul <- -calcul
    }
    calcul <- switch (data[1],
                     "Petite" = calcul * 1,
                     "Garde" = calcul * 2,
                     "Garde-sans" = calcul * 4,
                     "Garde-contre" = calcul * 6)
    
    return(calcul)
}