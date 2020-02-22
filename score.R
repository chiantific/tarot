joueurs <- c("Erwan", "François", "Hubert", "Gilles", "Thibaut")

data <- c("Garde", "Hubert", "Personne", 3, "faite")
data

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

score(data = data)
v <- numeric(length(joueurs))
v
for (i in 1:length(joueurs)) {
    
    if(data[3] == "Personne") {
        
        if(joueurs[i] == data[2]) {
            v[i] <- score(data)*4    
        } else {
            v[i] <- -score(data)
        }
        
    } else {
        
        if(joueurs[i] == data[2]) {
            v[i] <- score(data)*2
        } else if(joueurs[i] == data[3]) {
            v[i] <- score(data)
        } else {
            v[i] <- -score(data)
        }
        
    }
}
v
