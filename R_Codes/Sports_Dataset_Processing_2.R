passing_data <- read.csv('Datasets/cfb-passing-box-scores.csv')

years_retrieved <- c(2015,2016)
year_checked <- 2017
all_pls <- unique(passing_data$player_id)
all_pls <- all_pls[all_pls > 0]
column_names <- c('Year','Team','Player_ID','Player_Name','Passes_completed',
                  'Passes_Attempted','Yards','Touchdowns','Touchdown_Category',
                  'Interceptions')

for (y in years_retrieved) {
  year_data <- subset(passing_data, subset = (season == y))
  year_data <- year_data[sort(year_data$player_id, index.return=TRUE)[[2]],]
  all_pls <- intersect(all_pls, unique(year_data$player_id))
}

year_data <- subset(passing_data, subset = (season == year_checked))
ch_ids <- intersect(all_pls, year_data$player_id)

for (y in years_retrieved) {
  year_data <- subset(passing_data, subset = (season == y))
  year_data <- year_data[sort(year_data$player_id, index.return=TRUE)[[2]],]
  
  year_data_pls <- data.frame(year = integer(),
                              team = factor(),
                              pl_id = integer(),
                              player_name = character(),
                              passes_completed = integer(),
                              passes_attempted = integer(),
                              yards = numeric(),
                              touchdowns = integer(),
                              touch_cat = factor(),
                              interceptions = integer())
  
  for (id in all_pls) {
    player_chunk <- subset(year_data, subset = (player_id == id))
    t_cat <- factor(levels = c('none','occasional','several','many'),
                    ordered = TRUE)
    if (sum(player_chunk$td) == 0) {
      t_cat <- 'none'
    } else if (sum(player_chunk$td) < 2) {
      t_cat <- 'occasional'
    } else if (sum(player_chunk$td) < 6) {
      t_cat <- 'several'
    } else {
      t_cat <- 'many'
    }
    row <- list(as.integer(y), as.factor(player_chunk$team[1]), as.integer(id), player_chunk$player_name[1],
                sum(player_chunk$yds), sum(player_chunk$td), t_cat, sum(player_chunk$int))
    
    passes <- player_chunk$comp_att
    passes <- as.integer(unlist(strsplit(passes, '/')))
    passes <- matrix(passes, ncol=2, byrow=TRUE)
    
    row <- append(row, sum(passes[,1]), after = 4)
    row <- append(row, sum(passes[,2]), after = 5)
    
    year_data_pls <- rbind(year_data_pls, row)
  }
  
  colnames(year_data_pls) <- column_names
  year_data_pls$Team <- as.factor(year_data_pls$Team)
  
  yds_threshold <- median(year_data_pls$Yards)
  year_data_pls$Yards_Category <- c(year_data_pls$Yards >= yds_threshold)
  
  first_years <- passing_data$season[match(year_data_pls$Player_ID, passing_data$player_id)]
  year_data_pls$First_Year_Recorded <- first_years
  
  binary_cat <- 'Yards'
  year_data_pls[,paste0('Any_', binary_cat)] <- as.integer(year_data_pls[,binary_cat] > 0)
  
  outcome_label <- paste0('Played_in_', as.character(year_checked))
  year_data_pls[,outcome_label] <- year_data_pls$Player_ID %in% ch_ids
  
  assign(paste0('pls_data_', as.character(y)), year_data_pls)
  write.csv(year_data_pls, paste0('Datasets/Data_By_Player_', as.character(y), '.csv'),
            row.names = FALSE, col.names = TRUE)
  
}
