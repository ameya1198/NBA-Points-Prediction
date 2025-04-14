# NBA-Points-Prediction


Original.R

Pulls complete historical data from the NBA API for the 2024 season.

Gathers both advanced and traditional stats for home and away teams.

Cleans and merges data into a unified dataset: nba_stats_final.

Outputs a CSV file with all player and team-level features.

Also includes a utility to compute average performance over last 10 games for any player.




Rerun Script.R

Designed to fetch only the latest game data (newer than the last update).

Efficiently appends this new data to the existing dataset.

Ensures the system stays up-to-date without needing a full re-run.




Results.R

Performs feature selection using Recursive Feature Elimination (RFE) with a Random Forest model.

Trains a regression model to predict a player's scoring output (points).

Accepts a player’s average performance (last 10 games) as input and outputs the predicted points.

Includes model evaluation (RMSE) and feature importance visualization.    



Technologies Used


R (caret, dplyr, randomForest)

NBA API (via custom functions)

Machine Learning (Random Forest, RFE)

Data Wrangling & Joining
