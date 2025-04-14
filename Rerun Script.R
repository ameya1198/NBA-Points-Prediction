latest_game_date <- max(final$game_date)


nba_schedule_new <- nba_schedule(league_id = '00', season = 2024) %>%
  filter(game_status_text == 'Final' & game_date > latest_game_date)

# Extract new game IDs based on the latest game date
new_game_ids <- nba_schedule_new %>% pull(game_id)

# Function to retrieve advanced stats for new games
get_advanced_stats_new <- function(game_id) {
  tryCatch({
    data <- nba_boxscoreadvancedv3(game_id = game_id)
    message(paste("✅ Successfully fetched data for game ID:", game_id))
    return(data)
  }, error = function(e) {
    message(paste("❌ Error retrieving game ID:", game_id))
    return(NULL)
  })
}

# Batch processing for new games (100 games at a time)
batch_size <- 100
num_batches <- ceiling(length(new_game_ids) / batch_size)
advanced_stats_list_new <- list()

for (i in seq_len(num_batches)) {
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, length(new_game_ids))
  batch_game_ids <- new_game_ids[start_idx:end_idx]
  
  message(paste("\n🚀 Fetching batch", i, "of", num_batches, "(", length(batch_game_ids), "games)..."))
  
  batch_data <- map(batch_game_ids, get_advanced_stats_new)
  advanced_stats_list_new <- append(advanced_stats_list_new, batch_data)
  
  # Pause between batches to avoid timeouts
  message("\n⏳ Pausing for 10 seconds to avoid rate limits...")
  Sys.sleep(10)
}

# Filter and combine new advanced stats data for home and away players
filtered_stats_list_new <- map(advanced_stats_list_new, function(game_data) {
  if (length(game_data) >= 1) {
    return(game_data[[1]])  # Keep only the first data frame (home players)
  } else {
    return(NULL)
  }
})

filtered_stats_list_new_away <- map(advanced_stats_list_new, function(game_data) {
  if (length(game_data) >= 2) {
    return(game_data[[2]])  # Keep only the second data frame (away players)
  } else {
    return(NULL)
  }
})

final_advanced_stats_new <- bind_rows(compact(filtered_stats_list_new))
final_advanced_stats_new_away <- bind_rows(compact(filtered_stats_list_new_away))

final_advanced_stats_new_combined <- rbind(final_advanced_stats_new, final_advanced_stats_new_away)

# Function to retrieve traditional stats for new games
get_traditional_stats_new <- function(game_id) {
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

# Batch processing for new traditional stats
traditional_stats_list_new <- list()

for (i in seq_len(num_batches)) {
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, length(new_game_ids))
  batch_game_ids <- new_game_ids[start_idx:end_idx]
  
  message(paste("\n🚀 Fetching batch", i, "of", num_batches, "(", length(batch_game_ids), "games)..."))
  
  batch_data <- map(batch_game_ids, get_traditional_stats_new)
  traditional_stats_list_new <- append(traditional_stats_list_new, batch_data)
  
  # Pause between batches to avoid timeouts
  message("\n⏳ Pausing for 10 seconds to avoid rate limits...")
  Sys.sleep(10)
}

# Filter and combine new traditional stats data for home and away players
filtered_traditional_stats_list_new_1 <- map(traditional_stats_list_new, function(game_data) {
  if (length(game_data) >= 1) {
    return(game_data[[1]])  # Keep only the first data frame (home players)
  } else {
    return(NULL)
  }
})

filtered_traditional_stats_list_new_2 <- map(traditional_stats_list_new, function(game_data) {
  if (length(game_data) >= 2) {
    return(game_data[[2]])  # Keep only the second data frame (away players)
  } else {
    return(NULL)
  }
})

final_traditional_stats_new_1 <- bind_rows(compact(filtered_traditional_stats_list_new_1))
final_traditional_stats_new_2 <- bind_rows(compact(filtered_traditional_stats_list_new_2))

final_traditional_stats_new_combined <- rbind(final_traditional_stats_new_1, final_traditional_stats_new_2)

# Clean up the data (same as in your original script)
final_advanced_stats_new_combined <- final_advanced_stats_new_combined %>%
  select(-c(team_city, team_tricode, name_i, player_slug, position, comment, jersey_num))

final_traditional_stats_new_combined <- final_traditional_stats_new_combined %>%
  select(-c(team_city, team_tricode, name_i, player_slug, position, comment, jersey_num))

# Combine new advanced and traditional stats
nba_stats_new <- inner_join(final_traditional_stats_new_combined, final_advanced_stats_new_combined, by = c("game_id", "person_id"))

nba_stats_new <- nba_stats_new %>%
  select(-c(away_team_id.y, home_team_id.y, team_id.y, team_name.y, team_slug.y, first_name.y, family_name.y, minutes.y))

# Clean column names and combine with new team stats
colnames(nba_stats_new) <- gsub("\\.x", "", colnames(nba_stats_new))

# Get team stats from traditional and advanced (same process as before)
filtered_traditional_stats_list_away_new <- map(traditional_stats_list_new, function(game_data) {
  if (length(game_data) >= 4) {
    return(game_data[[4]])  # Keep only the first data frame for away team
  } else {
    return(NULL)
  }
})

filtered_advanced_stats_list_away_new <- map(advanced_stats_list_new, function(game_data) {
  if (length(game_data) >= 4) {
    return(game_data[[4]])  # Keep only the first data frame for away team
  } else {
    return(NULL)
  }
})

final_traditional_stats_away_new <- bind_rows(compact(filtered_traditional_stats_list_away_new))
final_advanced_stats_away_new <- bind_rows(compact(filtered_advanced_stats_list_away_new))

team_stats_new <- inner_join(final_traditional_stats_away_new, final_advanced_stats_away_new, by = c("game_id", "away_team_id"))

team_stats_new <- team_stats_new %>%
  select(-c(home_team_id.y, team_id.y, team_name.y, team_city.y, team_tricode.y, team_slug.y, minutes.y))

# Join with the new data and add to the existing final dataframe
nba_stats_new_final <- inner_join(nba_stats_new, team_stats_new, by = c("game_id", "away_team_id"))

nba_stats_new_final <- nba_stats_new_final %>%
  select(-c(home_team_id.x, team_id.x, team_name.x, team_city.x, team_tricode.x, team_slug.x, minutes.x))

# Update the column names and add the name column
colnames(nba_stats_new_final) <- gsub("\\.y$", "_opp_team", colnames(nba_stats_new_final))
nba_stats_new_final$name <- paste(nba_stats_new_final$first_name, nba_stats_new_final$family_name)

nba_stats_new_final <- nba_stats_new_final %>%
  select(-c(first_name, family_name))

# Combine new data with existing data
def_new <- nba_schedule_new %>%
  select(c(game_date, game_id))

final_new <- inner_join(nba_stats_new_final, def_new, by = "game_id")

# Combine the new data with the existing final dataset
final <- bind_rows(final, final_new)

# Save the updated dataframe
write.csv(final, "/Users/ameyaphansalkar/Documents/nba_stats_2024_updated.csv")
