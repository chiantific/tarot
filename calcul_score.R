players <- c("Erwan", "François", "Gilles", "Hubert", "Thibaut")

# Contract multipliers
contract_multipliers <- c("Petite" = 1, "Garde" = 2, "Garde-sans" = 4, "Garde-contre" = 6)

# Target scores based on bouts
target_scores <- c("0" = 56, "1" = 51, "2" = 41, "3" = 36)

calculate_tarot_scores <- function(contract, bouts, score, preneur, appele, players) {
    
    
    # Calculate the base result
    target <- target_scores[bouts] |> unname()
    result <- (score - target)
    total_score <- (abs(result) + 25) * ifelse(score >= target, 1, -1) * contract_multipliers[contract] |>
        unname()
    
    # Initialize score table
    scores <- setNames(rep(0, length(players)), players)
    
        
    if (appele == "Personne"){
        # preneur plays alone (1 vs 4)
        scores[preneur] <- total_score * 4
        for (player in players[players != preneur]){
            scores[player] <- -total_score
        }
    } else {
        # preneur plays with a partner
        scores[preneur] <- total_score * 2
        scores[appele] <- total_score
        for (player in players[!players %in% c(preneur, appele)]){
            scores[player] <- -total_score
        }
    }
    return(scores)
}

# path to the CSV file
tarot_scores_file <- "tarot_scores.csv"

# Function to save entries to file
save_entries <- function(data) {
    write_csv(data, tarot_scores_file)
}