
nba_schedule <- nba_schedule(league_id = '00', season = 2024)

# Extract game IDs
nba_schedule <- nba_schedule %>% filter(game_status_text == 'Final')
game_ids <- nba_schedule %>% pull(game_id)

# Function to retrieve advanced stats for a single game
get_advanced_stats <- function(game_id) {
  tryCatch({
    data <- nba_boxscoreadvancedv3(game_id = game_id)
    message(paste("✅ Successfully fetched data for game ID:", game_id))
    return(data)
  }, error = function(e) {
    message(paste("❌ Error retrieving game ID:", game_id))
    return(NULL)
  })
}

# Batch processing: Fetch 100 games at a time
batch_size <- 100
num_batches <- ceiling(length(game_ids) / batch_size)
advanced_stats_list <- list()

for (i in seq_len(num_batches)) {
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, length(game_ids))
  batch_game_ids <- game_ids[start_idx:end_idx]
  
  message(paste("\n🚀 Fetching batch", i, "of", num_batches, "(", length(batch_game_ids), "games )..."))
  
  batch_data <- map(batch_game_ids, get_advanced_stats)
  advanced_stats_list <- append(advanced_stats_list, batch_data)
  
  # Pause between batches to avoid timeouts
  message("\n⏳ Pausing for 10 seconds to avoid rate limits...")
  Sys.sleep(10)
}

## Home_Players

# Extract only the first data frame from each game's list
filtered_stats_list <- map(advanced_stats_list, function(game_data) {
  if (length(game_data) >= 1) {
    return(game_data[[1]])  # Keep only the first data frame
  } else {
    return(NULL)  # Handle cases where data is missing
  }
})

# Remove NULL values (games that had missing or errored data)
filtered_stats_list <- compact(filtered_stats_list)

# Combine all extracted data frames into a single dataframe
final_advanced_stats <- bind_rows(filtered_stats_list)



#Away_Players

# Extract only the second data frame from each game's list
filtered_stats_list_2 <- map(advanced_stats_list, function(game_data) {
  if (length(game_data) >= 2) {
    return(game_data[[2]])  # Keep only the second data frame
  } else {
    return(NULL)  # Handle cases where data is missing
  }
})

# Remove NULL values (games that had missing or errored data)
filtered_stats_list_2 <- compact(filtered_stats_list_2)

# Combine all extracted data frames into a single dataframe
final_advanced_stats_2 <- bind_rows(filtered_stats_list_2)


final_advanced_statistics <- rbind(final_advanced_stats, final_advanced_stats_2)





######   Traditional Stats   ######




# Function to retrieve traditional box score stats for a game
get_traditional_stats <- function(game_id) {
  Sys.sleep(1)  # Add delay to avoid API rate limits
  
  tryCatch({
    data <- nba_boxscoretraditionalv3(game_id = game_id)
    message(paste("✅ Successfully fetched traditional stats for game ID:", game_id))
    return(data)
  }, error = function(e) {
    message(paste("❌ Error retrieving game ID:", game_id))
    return(NULL)
  })
}

# Batch processing: Fetch 100 games at a time
batch_size <- 100
num_batches <- ceiling(length(game_ids) / batch_size)
traditional_stats_list <- list()

for (i in seq_len(num_batches)) {
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, length(game_ids))
  batch_game_ids <- game_ids[start_idx:end_idx]
  
  message(paste("\n🚀 Fetching batch", i, "of", num_batches, "(", length(batch_game_ids), "games )..."))
  
  batch_data <- map(batch_game_ids, get_traditional_stats)
  traditional_stats_list <- append(traditional_stats_list, batch_data)
  
  # Pause between batches to avoid timeouts
  message("\n⏳ Pausing for 10 seconds to avoid rate limits...")
  Sys.sleep(10)
}



## Home Player Stats

# For extracting the first data frame of each list 
filtered_traditional_stats_list_1 <- map(traditional_stats_list, function(game_data) {
  if (length(game_data) >= 1) {
    return(game_data[[1]])  # Keep only the first data frame
  } else {
    return(NULL)
  }
})

# Remove NULL values (games that had missing or errored data)
filtered_traditional_stats_list_1 <- compact(filtered_traditional_stats_list_1)

# Combine all extracted data frames into a single dataframe
final_traditional_stats_1 <- bind_rows(filtered_traditional_stats_list_1)



## Away Player Stats

# For extracting the first data frame of each list 
filtered_traditional_stats_list_2 <- map(traditional_stats_list, function(game_data) {
  if (length(game_data) >= 2) {
    return(game_data[[2]])  # Keep only the first data frame
  } else {
    return(NULL)
  }
})

# Remove NULL values (games that had missing or errored data)
filtered_traditional_stats_list_2 <- compact(filtered_traditional_stats_list_2)

# Combine all extracted data frames into a single dataframe
final_traditional_stats_2 <- bind_rows(filtered_traditional_stats_list_2)


final_traditional_statistics <- rbind(final_traditional_stats_1, final_traditional_stats_2)



## Cleaning

final_advanced_statistics <- final_advanced_statistics %>% 
  select(-c(team_city, team_tricode, name_i, player_slug, position, comment, 
            jersey_num))

final_traditional_statistics <- final_traditional_statistics %>% 
  select(-c(team_city, team_tricode, name_i, player_slug, position, comment, 
            jersey_num))



nba_stats_2024 <- inner_join(final_traditional_statistics, final_advanced_statistics, by = c("game_id", "person_id"))

nba_stats_2024 <- nba_stats_2024 %>% 
  select(-c(away_team_id.y, home_team_id.y, team_id.y, team_name.y, team_slug.y,
            first_name.y, family_name.y, minutes.y))

# Assuming your data frame is named `df`
colnames(nba_stats_2024) <- gsub("\\.x", "", colnames(nba_stats_2024))



## Getting team stats from traditional

# For extracting the first data frame of each list 
filtered_traditional_stats_list_away <- map(traditional_stats_list, function(game_data) {
  if (length(game_data) >= 4) {
    return(game_data[[4]])  # Keep only the first data frame
  } else {
    return(NULL)
  }
})

# Remove NULL values (games that had missing or errored data)
filtered_traditional_stats_list_away <- compact(filtered_traditional_stats_list_away)

# Combine all extracted data frames into a single dataframe
final_traditional_stats_away <- bind_rows(filtered_traditional_stats_list_away)




## Getting team stats from advanced

# For extracting the first data frame of each list 
filtered_advanced_stats_list_away <- map(advanced_stats_list, function(game_data) {
  if (length(game_data) >= 4) {
    return(game_data[[4]])  # Keep only the first data frame
  } else {
    return(NULL)
  }
})

# Remove NULL values (games that had missing or errored data)
filtered_advanced_stats_list_away <- compact(filtered_advanced_stats_list_away)

# Combine all extracted data frames into a single dataframe
final_advanced_stats_away <- bind_rows(filtered_advanced_stats_list_away)

team_stats <- inner_join(final_traditional_stats_away, final_advanced_stats_away, by = c("game_id", "away_team_id"))

team_stats <- team_stats %>%
  select(-c(home_team_id.y, team_id.y, team_name.y, team_city.y, team_tricode.y, team_slug.y,
            minutes.y))

nba_stats_final <- inner_join(nba_stats_2024, team_stats, by = c("game_id", "away_team_id"))

nba_stats_final <- nba_stats_final %>%
  select(-c(home_team_id.x, team_id.x, team_name.x, team_city.x, team_tricode.x, team_slug.x,
            minutes.x))

colnames(nba_stats_final) <- gsub("\\.y$", "_opp_team", colnames(nba_stats_final))

nba_stats_final$name <- paste(nba_stats_final$first_name, nba_stats_final$family_name)

nba_stats_final <- nba_stats_final %>% 
  select(-c(first_name, family_name))

def <- nba_schedule %>% 
  select(c(game_date, game_id))

final <- inner_join(nba_stats_final, def, by = "game_id")

write.csv(nba_stats_final, "/Users/ameyaphansalkar/Documents/nba_stats_2024.csv")




## Function to get averages


get_player_averages <- function(final) {
  
  # Ask the user for the player name
  player_name <- readline(prompt = "Enter the player name: ")
  
  # Filter the data for the selected player
  player_data <- final[final$name == player_name, ]  # Use 'name' column in the 'final' dataframe
  
  # Check if the player exists in the data
  if (nrow(player_data) == 0) {
    cat("Player not found in the dataset. Please try again.\n")
    return(NULL)  # Return NULL if player is not found
  }
  
  # Sort the data by 'game_date' in descending order (latest games first)
  player_data <- player_data[order(player_data$game_date, decreasing = TRUE), ]
  
  # Select the latest 10 games (if there are less than 10 games, take all)
  player_data_latest_10 <- head(player_data, 10)
  
  # List of selected features
  selected_features <- c("field_goals_made.x", "field_goals_attempted.x", "three_pointers_made.x", 
                         "free_throws_made.x", "true_shooting_percentage.x", "free_throws_attempted.x",
                         "three_pointers_percentage.x", "possessions.x", "effective_field_goal_percentage.x",
                         "field_goals_percentage.x", "pace.x", "pace_per40.x", "net_rating_opp_team", 
                         "usage_percentage.x", "pie.x")
  
  # Ensure player_data_latest_10 is still a data frame after subsetting
  player_data_latest_10 <- as.data.frame(player_data_latest_10)
  
  # Filter the data to keep only the selected features
  player_data_latest_10 <- player_data_latest_10[, c("name", "game_date", selected_features), drop = FALSE]
  
  # Calculate the averages for the selected features
  averages <- colMeans(player_data_latest_10[, selected_features], na.rm = TRUE)
  
  # Convert the averages into a data frame for easy display
  averages_df <- data.frame(player_name = player_name, game_count = min(10, nrow(player_data_latest_10)), t(averages))
  
  return(averages_df)
}

# Call the function with your actual data frame (final)
get_player_averages(final)

# Display the result
print(player_averages)

predictions <- predict(rf_model, newdata = player_averages)
