passing_data <- read.csv('Datasets/cfb-passing-box-scores.csv')

all_pls <- unique(passing_data$player_id)
all_pls <- all_pls[all_pls > 0]
column_names <- c('Year','Team','Player_ID','Player_Name','Passes_Completed',
                  'Passes_Attempted','Yards','Touchdowns','Touchdown_Category',
                  'Interceptions')

all_pls_by_year <- list()

for (y in unique(passing_data$season)) {
  cat('\nCompiling year', as.character(y), '...')
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
    if (!(id %in% year_data$player_id)) {next}
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
  
  cat('\n\tUpdating column names...')
  colnames(year_data_pls) <- column_names
  year_data_pls$Team <- as.factor(year_data_pls$Team)
  
  yds_threshold <- median(year_data_pls$Yards)
  year_data_pls$Yards_Category <- c(year_data_pls$Yards >= yds_threshold)
  
  first_years <- passing_data$season[match(year_data_pls$Player_ID, passing_data$player_id)]
  year_data_pls$First_Year_Recorded <- first_years
  
  binary_cat <- 'Yards'
  year_data_pls[,paste0('Any_', binary_cat)] <- as.integer(year_data_pls[,binary_cat] > 0)
  
  #outcome_label <- paste0('Played_in_', as.character(year_checked))
  #year_data_pls[,outcome_label] <- year_data_pls$Player_ID %in% ch_ids
  
  cat('\n\tAppending to Big Data...\n')
  all_pls_by_year[[as.character(y)]] <- year_data_pls
  #names(all_pls_by_year)[length(all_pls_by_year)] <- as.character(y)
  #write.csv(year_data_pls, paste0('Datasets/Data_By_Player_', as.character(y), '.csv'),
  #          row.names = FALSE, col.names = TRUE)
  
}

expanded_data <- NULL

for (id in all_pls) {
  
  if (is.null(expanded_data)) {cat('\nCompiling players...\n')}
  
  yrs_played <- c()
  potential_t1 <- c()
  for (y in names(all_pls_by_year)) {
    #cat('\nTesting year', y, '...')
    if (id %in% all_pls_by_year[[y]][,'Player_ID']) {
      yrs_played <- c(yrs_played, as.integer(y))
      #cat('\n\tYear confirmed!')
      }
  }
  
  for (y in yrs_played) {
    if ((y+1) %in% yrs_played) {potential_t1 <- c(potential_t1, y)}
  }
  
  if (2024 %in% potential_t1) {potential_t1 <- potential_t1[-2024]}
  
  if (length(potential_t1) == 1) {
    year_t1 <- potential_t1
  } else if (length(potential_t1) > 1) {year_t1 <- sample(potential_t1, 1)} else {next}
  
  if (!is.null(year_t1)) {
    #cat('\n\t.')
    player_chunk <- subset(all_pls_by_year[[as.character(year_t1)]], subset = (Player_ID == id))
    row_t1 <- player_chunk[,c('Year','Team','Passes_Completed',
                           'Passes_Attempted','Yards','Yards_Category',
                           'Touchdowns','Touchdown_Category','Interceptions',
                           'First_Year_Recorded')]
    row_t1$Years_Played_Before <- row_t1[,'Year'] - row_t1[,'First_Year_Recorded']
    row_t1 <- row_t1[,-10]
    colnames(row_t1) <- paste0(colnames(row_t1), '_t1')
    #cat('.')
    
    player_chunk <- subset(all_pls_by_year[[as.character(year_t1 + 1)]], subset = (Player_ID == id))
    row_t2 <- player_chunk[,c('Year','Team','Passes_Completed',
                              'Passes_Attempted','Yards','Yards_Category',
                              'Touchdowns','Touchdown_Category','Interceptions')]
    colnames(row_t2) <- paste0(colnames(row_t2), '_t2')
    
    row <- cbind(row_t1, row_t2)
    row$Played_t3 <- id %in% all_pls_by_year[[as.character(year_t1 + 2)]][,'Player_ID']
    
    #cat('.')
    if (is.null(expanded_data)) {expanded_data <- row} else {
      expanded_data <- rbind(expanded_data, row)
    }
  }
  
}

# Quick Fix
expanded_data$Yards_Category_t1 <- expanded_data$Yards_t1 >= median(c(expanded_data$Yards_t1,expanded_data$Yards_t2))
expanded_data$Yards_Category_t2 <- expanded_data$Yards_t2 >= median(c(expanded_data$Yards_t1,expanded_data$Yards_t2))

# Writing to File

write.csv(expanded_data, 'Datasets/Expanded_Sports_Data.csv')