library(shiny)

# Define the fields we want to save from the form
fields <- c("contract", "contract-holder", "called", "score", "faite")

# define the players


joueurs <- c("Erwan", "François", "Hubert", "Gilles", "Thibaut")
select_taker <- c("selectionner un joueur", joueurs)
select_called <- c("Personne", joueurs)

source("IO.R")

# Shiny app with 3 fields that the user can submit data for

ui <- fluidPage(
    
    # App title
    titlePanel("Compter les points au tarot"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar layout with input and output definitions ----
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            radioButtons(inputId = "contract", label = "Enchères",
                         choices = list("Petite", "Garde", "Garde-sans", "Garde-contre"),
                         inline = FALSE),
            selectInput(inputId = "contract-holder", label = "Preneur", 
                        choices = list("Selectionner un joueur", "Erwan", "François", "Hubert",
                                       "Gilles", "Thibaut")),
            selectInput(inputId = "called", label = "Appelé", 
                        choices = list("Personne", "Erwan", "François", "Hubert",
                                       "Gilles", "Thibaut")),
            numericInput(inputId = "score", label = "score", value = 0),
            radioButtons(inputId = "faite", label = "Alors ?", 
                         choices = list("faite", "chutée")),
            actionButton("submit", "Submit")
        ),
        mainPanel(
            DT::dataTableOutput("responses", width = 300),
            helpText("Score des parties"),
            textOutput("points")
            
        )
    )

)
    

server <-  function(input, output, session) {
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
        
        data <- sapply(fields, function(x) input[[x]])
            data
    })
        
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
        saveData(formData())
        scoreData(formData())
        resetForm(session)
    })
        
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
        input$submit
        loadData()
    }, options = list(searching = FALSE))
    output$points <- renderText({
        input$submit
        loadTotal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
