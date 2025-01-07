players <- c("Erwan", "François", "Gilles", "Hubert", "Thibaut")

# Contract multipliers
contract_multipliers <- c("Petite" = 1, "Garde" = 2, "Garde-sans" = 4, "Garde-contre" = 6)

# Target scores based on bouts
target_scores <- c("0" = 56, "1" = 51, "2" = 41, "3" = 36)

calculate_tarot_scores <- function(contract, bouts, score, preneur, appele, players) {
    
    
    # Calculate the base result
    target <- target_scores[bouts] |> unname()
    result <- (score - target)
    adjusted_result <- (abs(result) + 25) * ifelse(score >= target, 1, -1) * contract_multipliers[contract] |> unname()
    
    # Calculate team scores
    taker_score <- adjusted_result * 2
    partner_score <- adjusted_result
    defenders_score <- -adjusted_result
    
    # Initialize score table
    scores <- setNames(rep(0, length(players)), players)
    
    # Update scores for each player
    scores[preneur] <- taker_score
    if (appele != "Personne") {
        scores[appele] <- partner_score
    }
    for (defender in setdiff(players, c(preneur, appele))) {
        scores[defender] <- defenders_score
    }
    
    return(scores)
}

# Assuming `entries` is the data.frame containing game information
# Example structure of entries:
entries <- data.frame(
    Contrat = c("Garde", "Petite"),
    Bouts = c("2", "1"),
    Score = c(45, 60),
    Preneur = c("Erwan", "Hubert"),
    Appele = c("François", "Erwan"),
    stringsAsFactors = FALSE
)

# Initialize a data.frame to store scores for each entry
score_table <- data.frame(matrix(0, nrow = 1, ncol = length(players)))
colnames(score_table) <- players

# Loop through each entry and calculate scores
for (i in seq_len(nrow(entries))) {
    entry <- entries[i, ]
    # Calculate scores for the current game
    game_scores <- calculate_tarot_scores(
        contract = entry$Contrat,
        bouts = entry$Bouts,
        score = entry$Score,
        preneur = entry$Preneur,
        appele = entry$Appele,
        players = players
    )
    # Accumulate scores: Add the last row of `score_table` to the current game scores
    cumulative_scores <- tail(score_table, 1) + game_scores
    # Append the cumulative scores to the `score_table`
    score_table <- rbind(score_table, cumulative_scores)
}

# Display cumulative scores
print(score_table)
