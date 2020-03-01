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
        playersScores <<- data
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
