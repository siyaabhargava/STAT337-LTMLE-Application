#install.packages('ltmle')
library(ltmle)

# Retrieving Data
# For best results, set working directory to our shared "R_Codes" folder!
# By which I mean you WILL have to manually change the path otherwise

years_retrieved <- c(2015, 2016)
path <- 'Datasets/Data_By_Player_'

for (y in years_retrieved) {
  new_file <- read.csv(paste0(path, as.character(y), '.csv'), header = TRUE)
  assign(paste0('player_data_', as.character(y)), new_file)
}

# Cleaning Up
rm(path,y)

# Setting Up Treatment
treats <- 'Yards_Category'

if (startsWith(colnames(new_file)[13], 'Any_')) {
  treats <- colnames(new_file)[13]
}

# LTMLE: Setting Up Inputs

chronological_data <- data.frame(player_data_2015$First_Year_Recorded,
                            player_data_2015$Passes_completed,
                            player_data_2015$Passes_Attempted,
                            player_data_2015$Touchdowns,
                            player_data_2015$Interceptions,
                            player_data_2015[,treats],
                            player_data_2016$Passes_completed,
                            player_data_2016$Passes_Attempted,
                            player_data_2016$Touchdowns,
                            player_data_2016$Interceptions,
                            player_data_2016[,treats],
                            player_data_2015$Played_in_2017)

colnames(chronological_data) <- c('First_Year_Recorded',
                                  'Passes_Completed_2015',
                                  'Passes_Attempted_2015',
                                  'Touchdowns_2015',
                                  'Interceptions_2015',
                                  paste0(treats, '_2015'),
                                  'Passes_Completed_2016',
                                  'Passes_Attempted_2016',
                                  'Touchdowns_2016',
                                  'Interceptions_2016',
                                  paste0(treats, '_2016'),
                                  'Played_in_2017')

abar_input <- matrix(c(0,0,
                       1,0,
                       0,1,
                       1,1),
                     ncol = 2,
                     byrow = TRUE)

abar_input <- list( c(0,0), c(1,1))

for (c in colnames(chronological_data)) {
  if (is.logical(chronological_data[,c])) {
    chronological_data[,c] <- as.integer(chronological_data[,c])
  }
}

# Checks?
table(chronological_data$Yards_Category_2015, chronological_data$Touchdowns_2015)
heatmap(cor(chronological_data))
round(cor(chronological_data), 3)

# LTMLE: Running Function

result_1 <- ltmle(chronological_data,
                  Anodes = c(paste0(treats, '_2015'),
                             paste0(treats, '_2016')),
                  Lnodes = c('Passes_Completed_2015',
                             'Passes_Attempted_2015',
                             'Touchdowns_2015',
                             'Interceptions_2015',
                             'Passes_Completed_2016',
                             'Passes_Attempted_2016',
                             'Touchdowns_2016',
                             'Interceptions_2016'),
                  Ynodes = 'Played_in_2017',
                  abar = abar_input)                     #Everything

result_2 <- ltmle(chronological_data[,c('First_Year_Recorded',
                                        'Passes_Attempted_2015',
                                        'Yards_Category_2015',
                                        'Passes_Attempted_2016',
                                        'Yards_Category_2016',
                                        'Played_in_2017')],
                  Anodes = c('Yards_Category_2015',
                             'Yards_Category_2016'),
                  Lnodes = c('Passes_Attempted_2015',
                             'Passes_Attempted_2016'),
                  Ynodes = 'Played_in_2017',
                  abar = abar_input)                    #Minimal

summary(result_2)
