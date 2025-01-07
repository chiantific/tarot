library(shiny)
library(shinyjs)
library(tidyverse)

# load helper scripts
source("calcul_score.R")

tarot_scores_file <- "tarot_scores.csv" # path to the CSV file

# Define UI
ui <- fluidPage(
    useShinyjs(),
    
    titlePanel("Tarot Score Keeper"),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                id = "entry_tabsetpanel",
                type = "pills",
                tabPanel(
                    "Enchères",
                    br(),
                    radioButtons(
                        inputId = "contrat",
                        label = "Contrat",
                        choices = list("Petite", "Garde",
                                       "Garde-sans", "Garde-contre"),
                        selected = "Garde"
                    ),
                    selectInput(
                        "preneur",
                        "Le preneur",
                        choices = c("Sélectionner un nom", players),
                        selected = "Sélectionner un nom"
                    ),
                    selectInput(
                        "appele",
                        "L'appelé",
                        choices = c("Personne", players),
                        selected = "Personne"
                    ),
                    numericInput(
                        "score",
                        "Score des meneurs",
                        value = 0,
                        min = 0,
                        max = 91, 
                        step = 1
                     ),
                     radioButtons(
                         "nb_bouts",
                         "Nombre de bouts",
                         choices = c(0, 1, 2, 3),
                         inline = TRUE
                     )
                         
                ),
                tabPanel("Annonces",
                         br(),
                         selectInput(
                             "poignee",
                             "La poignée (8,10 ou 13 atouts)",
                             choices = c("Pas de poignée", "8 atouts",
                                         "10 atouts", "13 atouts"),
                             selected = "Pas de poignée"
                         ),
                         selectInput(
                             "petit",
                             "Le petit au bout",
                             choices = c("Pas de petit au bout", players),
                             selected = "Pas de petit au bout"
                         ),
                         selectInput(
                             "chelem",
                             "Le Chelem",
                             choices = c("Pas de chelem", "annoncé et réussi",
                                         "annoncé et raté"),
                             selected = "Pas de chelem"
                         )
                )
            ),
            actionButton("entry_button_encheres", "Ajouter la partie", disabled = TRUE), 
            actionButton("reset_button_encheres", "Reset"),
            br(),
            actionButton("remove_last_button", "Supprimer la dernière partie")
        ),
        mainPanel(
            h4("Entries Table"),
            tableOutput("entryTable"),
            br(),
            h4("Cumulative Scores Table"),
            tableOutput("cumulativeScores")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Load existing entries from file or initialize empty (NEW)
    entries <- reactiveVal(
        if(file.exists(tarot_scores_file)){
            read_csv(tarot_scores_file)
        } else {
            data.frame(
                Contrat = character(0),
                Preneur = character(0),
                Appele = character(0),
                Score = numeric(0),
                Bouts = numeric(0),
                Poignee = character(0),
                Petit = character(0),
                Chelem = character(0),
                stringsAsFactors = FALSE
            )
        }
    )
    
    # Function to save entries to file
    save_entries <- function(data) {
        write_csv(data, tarot_scores_file)
    }
    
    # Enable/disable both buttons based on "preneur" selection
    observe({
        if (input$preneur == "Sélectionner un nom") {
            shinyjs::disable("entry_button_encheres")
            shinyjs::disable("entry_button_annonces")
        } else {
            shinyjs::enable("entry_button_encheres")
            shinyjs::enable("entry_button_annonces")
        }
    })
    
    # Update "appele" choices when "preneur" changes
    observeEvent(input$preneur, {
        if (input$preneur != "Sélectionner un nom") {
            updateSelectInput(
                session,
                "appele",
                choices = c("Personne", setdiff(players, input$preneur)),
                selected = "Personne"
            )
        }
    })
    
    # Add an entry when "entry_button_encheres" is clicked
    observeEvent(input$entry_button_encheres, {
        current_entries <- entries()
        new_entry <- data.frame(
            Contrat = input$contrat,
            Preneur = input$preneur,
            Appele = input$appele,
            Score = input$score,
            Bouts = input$nb_bouts,
            Poignee = input$poignee,
            Petit = input$petit,
            Chelem = input$chelem
        )
        
        updated_entries <- rbind(current_entries, new_entry)
        entries(updated_entries)
        save_entries(updated_entries) # save to file
        
        # Reset inputs to their default values
        updateRadioButtons(session, "contrat", selected = "Garde")
        updateSelectInput(session, "preneur", selected = "Sélectionner un nom")
        updateSelectInput(session, "appele", selected = "Personne")
        updateNumericInput(session, "score", value = 0)
        updateRadioButtons(session, "nb_bouts", selected = 0)
        updateSelectInput(session, "poignee", selected = "Pas de poignée")
        updateSelectInput(session, "petit", selected = "Pas de petit au bout")
        updateSelectInput(session, "chelem", selected = "Pas de chelem")
        
        # Switch back to the "Enchères" tab
        updateTabsetPanel(session, inputId = "entry_tabsetpanel", selected = "Enchères")
    })
    
    # Reset all inputs to default values when "reset_button" is clicked
    observeEvent(input$reset_button_encheres, {
        updateRadioButtons(session, "contrat", selected = "Garde")
        updateSelectInput(session, "preneur", selected = "Sélectionner un nom")
        updateSelectInput(session, "appele", selected = "Personne")
        updateNumericInput(session, "score", value = 0)
        updateRadioButtons(session, "nb_bouts", selected = 0)
        updateSelectInput(session, "poignee", selected = "Pas de poignée")
        updateSelectInput(session, "petit", selected = "Pas de petit au bout")
        updateSelectInput(session, "chelem", selected = "Pas de chelem")
        
        # Switch back to the "Enchères" tab
        updateTabsetPanel(session, inputId = "entry_tabsetpanel", selected = "Enchères")
        
        
    })
    
    # Show a confirmation dialog when "remove_last_button" is clicked
    observeEvent(input$remove_last_button, {
        current_entries <- entries()
        if (!is.null(current_entries) && nrow(current_entries) > 0) {
            showModal(modalDialog(
                title = "Confirmation",
                "Êtes-vous sûr de vouloir supprimer la dernière entrée ?",
                footer = tagList(
                    modalButton("Annuler"),
                    actionButton("confirm_delete", "Confirmer")
                )
            ))
        }
        
    })
    
    # Handle confirmation of deletion
    observeEvent(input$confirm_delete, {
        current_entries <- entries()
        if (!is.null(current_entries) && nrow(current_entries) > 0) {
            # Remove the last row
            updated_entries <- current_entries[-nrow(current_entries), ]
            entries(updated_entries)
            save_entries(updated_entries) # save to file
        }
        # Close the modal
        removeModal()
        # Switch back to the "Enchères" tab
        updateTabsetPanel(session, inputId = "entry_tabsetpanel", selected = "Enchères") 
    })
    
    # Render the table of entries
    output$entryTable <- renderTable({
        entries()
    })
}

# Run the application
shinyApp(ui = ui, server = server)