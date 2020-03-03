library(shiny)
library(tidyverse)

# Define the fields we want to save from the form
fields <- c("contract", "contract-holder", "called", "score", "faite")

# define the players
joueurs <- c("Erwan", "François", "Hubert", "Gilles", "Thibaut")

# load helper files
source("IO.R")
source("score.R")

# Shiny app with 3 fields that the user can submit data for

ui <- fluidPage(
    
    # App title
    titlePanel("Compter les points au tarot"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar layout with input and output definitions ----
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            tabsetPanel(type = "pills",
                tabPanel("Enchères",
                         radioButtons(inputId = "contract", label = "Contrat",
                                                 choices = list("Petite", "Garde",
                                                                "Garde-sans", "Garde-contre"),
                                                 inline = FALSE),
                         selectInput(inputId = "contract-holder", label = "Preneur", 
                                     choices = list("Selectionner un joueur", "Erwan",
                                                    "François", "Hubert", "Gilles", "Thibaut")),
                         selectInput(inputId = "called", label = "Appelé", 
                                     choices = list("Personne", "Erwan", "François", "Hubert",
                                                    "Gilles", "Thibaut")),
                         numericInput(inputId = "score", label = "score", value = 0),
                         radioButtons(inputId = "faite", label = "Alors ?", 
                                      choices = list("faite", "chutée")),
                         actionButton("submit", "Submit")
                         ),
                tabPanel("Annonces",
                         radioButtons(inputId = "poignee",
                                      label = "La poignée (8,10 ou 13 atouts)",
                                      choices = list("pas de poignée", "8 atouts",
                                                     "10 atouts", "13 atouts")),
                         selectInput(inputId = "petit", "Le petit au bout",
                                     choices = list("pas de petit au bout", "Erwan", "François",
                                                    "Hubert", "Gilles", "Thibaut")),
                         selectInput(inputId = "chelem", label = "Le Chelem",
                                     choices = list("pas de chelem", "annoncé et réussi", 
                                     "annoncé et raté", "non-annoncé et réalisé"))
                         )
                    )
            ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Dernière partie", verbatimTextOutput("playerScore")),
                        tabPanel("Scores",
                                 DT::dataTableOutput("playerScoreTable", width = "100%")),
                        tabPanel("Contrats",
                                 DT::dataTableOutput("responses", width = "100%")),
                        tabPanel("Statistiques", verbatimTextOutput("statistics"))
                        )
            )
    )
)
    

    

server <-  function(input, output, session) {
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
        data <- sapply(fields, function(x) input[[x]])
            data
    })
    
    scoreData <- reactive({
        data <- sapply(fields, function(x) input[[x]])
        v <- numeric(length(joueurs))
        
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
        names(v) <- joueurs
        return(v)
    })
        
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
        saveData(formData())
        saveScore(scoreData())
        saveRoundScore(scoreData())
        resetForm(session)
    })
        
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
        input$submit
        loadData()
    }, options = list(dom = 't'))
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$playerScoreTable <- DT::renderDataTable({
        input$submit
        loadPlayersScores()
    }, options = list(dom = 't'))
    
    
    output$playerScore <- renderPrint({
        input$submit
        loadRoundScore()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
