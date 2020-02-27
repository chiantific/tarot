saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
        responses <<- rbind(responses, data)
    } else {
        responses <<- data
    }
    names(responses) <- c("Contrat", "Preneur", "Appelé", "Score", "faite")
}
scoreData <- function(data) {
    
    score <- as.numeric(data[4])
    
    if(data[5] != "faite") {
        score <- -score}
    
    score <- switch (data[1],
        "Petite" = score * 1,
        "Garde" = score * 2,
        "Garde-sans" = score * 4,
        "Garde-contre" = score * 6)
    
    if (exists("total")) {
        total <<- c(total, score)
    } else {
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
        
    }
        data <- as.data.frame(t(data))
        if (exists("playerScore")) {
            playerScore <<- rbind(playerScore, data)
        } else {
            playerScore <<- data
        }
}


loadData <- function() {
    if (exists("responses")) {
        responses
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