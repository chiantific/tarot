saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
        responses <<- rbind(responses, data)
    } else {
        responses <<- data
    }
    names(responses) <- c("Contrat", "Preneur", "Appelé", "Score", "faite")
}
saveScore <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("playersScores")) {
        playersScores <<- rbind(playersScores, tail(playersScores, 1) + data)
    } else {
<<<<<<< HEAD
        playersScores <<- data
=======
        total <<- score
    }
}

loadTotal <- function() {
    if (exists("total")) {
        total
    }
}

savePlayerScore <- function(data){
    
    for (joueur in joueurs) {
        if (joueur == data[2]){
            score <- score * 2
        } else if (joueur == data[3]){
            score <- score
        } else {
            score <- -score
        }
        
>>>>>>> 2084cb761c37b7b4b90d0aea428ead3f4d85eba8
    }
}


loadData <- function() {
    if (exists("responses")) {
        responses
    }
}

loadPlayersScores <- function() {
    if (exists("playersScores")) {
        playersScores
    }
}

resetForm <- function(session) {
    # reset values
    updateRadioButtons(session = session, inputId = "contract", selected = "Petite")
    updateSelectInput(session = session, inputId = "contract-holder",
                      selected = "Selectionner un joueur")
    updateSelectInput(session = session, inputId = "called", selected = "Personne")
    updateNumericInput(session, inputId = "score", value = 0)
    updateRadioButtons(session, "faite", selected = "faite")
}