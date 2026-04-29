#install.packages('ltmle')
library(ltmle)

# Retrieving Data
# For best results, set working directory to our shared "R_Codes" folder!
# By which I mean you WILL have to manually change the path otherwise

path <- 'Datasets/Expanded_Sports_Data.csv'
expanded_data <- read.csv(path, header = TRUE, row.names = 1)

# Cleaning Up
rm(path)

# Setting Up Treatment
treats <- 'Yards_Category'

# LTMLE: Setting Up Inputs

chronological_data <- data.frame(expanded_data$Years_Played_Before_t1,
                                  expanded_data$Passes_Completed_t1,
                                  expanded_data$Passes_Attempted_t1,
                                  expanded_data$Touchdowns_t1,
                                  expanded_data$Interceptions_t1,
                                  expanded_data[,paste0(treats,'_t1')],
                                  expanded_data$Passes_Completed_t2,
                                  expanded_data$Passes_Attempted_t2,
                                  expanded_data$Touchdowns_t2,
                                  expanded_data$Interceptions_t2,
                                  expanded_data[,paste0(treats,'_t2')],
                                  expanded_data$Played_t3)

colnames(chronological_data) <- c('Years_Played_Before_t1',
                                  'Passes_Completed_t1',
                                  'Passes_Attempted_t1',
                                  'Touchdowns_t1',
                                  'Interceptions_t1',
                                  paste0(treats, '_t1'),
                                  'Passes_Completed_t2',
                                  'Passes_Attempted_t2',
                                  'Touchdowns_t2',
                                  'Interceptions_t2',
                                  paste0(treats, '_t2'),
                                  'Played_in_t3')

# Pick One!
abar_input <- c(0,1)

abar_input <- list( c(0,0), c(1,1))

for (c in colnames(chronological_data)) {
  if (is.logical(chronological_data[,c])) {
    chronological_data[,c] <- as.integer(chronological_data[,c])
  }
}

# Checks?
table(chronological_data$Yards_Category_2015, chronological_data$Touchdowns_2015)
round(cor(chronological_data), 3)[c(6,11,12),]

# LTMLE: Running Function

result_1 <- ltmle(chronological_data,
                  Anodes = c(paste0(treats, '_t1'),
                             paste0(treats, '_t2')),
                  Lnodes = c('Passes_Completed_t1',
                             'Passes_Attempted_t1',
                             'Touchdowns_t1',
                             'Interceptions_t1',
                             'Passes_Completed_t2',
                             'Passes_Attempted_t2',
                             'Touchdowns_t2',
                             'Interceptions_t2'),
                  Ynodes = 'Played_in_t3',
                  abar = abar_input)                     #Everything

result_2 <- ltmle(chronological_data[,c('Years_Played_Before_t1',
                                        'Passes_Attempted_t1',
                                        'Yards_Category_t1',
                                        'Passes_Attempted_t2',
                                        'Yards_Category_t2',
                                        'Played_in_t3')],
                  Anodes = c('Yards_Category_t1',
                             'Yards_Category_t2'),
                  Lnodes = c('Passes_Attempted_t1',
                             'Passes_Attempted_t2'),
                  Ynodes = 'Played_in_t3',
                  abar = abar_input)                    #Minimal

summary(result_1)
summary(result_2)

# LTMLE: Making the Models into Functions

max_model <- function(data, abar) {
  result_1 <- ltmle(data,
                    Anodes = c(paste0(treats, '_t1'),
                               paste0(treats, '_t2')),
                    Lnodes = c('Passes_Completed_t1',
                               'Passes_Attempted_t1',
                               'Touchdowns_t1',
                               'Interceptions_t1',
                               'Passes_Completed_t2',
                               'Passes_Attempted_t2',
                               'Touchdowns_t2',
                               'Interceptions_t2'),
                    Ynodes = 'Played_in_t3',
                    abar = abar)
  return(result_1)
}
min_model <- function(data, abar) {
  result_2 <- ltmle(data[,c('Years_Played_Before_t1',
                            'Passes_Attempted_t1',
                            'Yards_Category_t1',
                            'Passes_Attempted_t2',
                            'Yards_Category_t2',
                            'Played_in_t3')],
                    Anodes = c('Yards_Category_t1',
                               'Yards_Category_t2'),
                    Lnodes = c('Passes_Attempted_t1',
                               'Passes_Attempted_t2'),
                    Ynodes = 'Played_in_t3',
                    abar = abar)
  return(result_2)
}
summ_1 <- summary(result_1)

# Visualizing Estimate vs. P-Value

results_matrix <- matrix(NA, nrow = 0, ncol = 2)
a_bar_list <- list(c(0,0), c(1,0), c(0,1), c(1,1))
for (a_bar_input in a_bar_list) {
  temp_result <- max_model(chronological_data, abar = a_bar_input)
  temp_summ <- summary(temp_result)
  results_matrix <- rbind(results_matrix,
                          c(temp_summ$treatment$estimate, temp_summ$treatment$pvalue))
  
}
dimnames(results_matrix) <- list(Value_of_abar = as.character(a_bar_list),
                                 Statistics = c('Estimate','P_Value'))

# Comparison of Conditions

a_bar_combos <- list( list( c(1,1), c(0,0) ),
                      list( c(1,0), c(0,0) ),
                      list( c(0,1), c(0,0) ) )

results_matrix_max <- matrix(NA, nrow = 0, ncol = 6)
results_matrix_min <- matrix(NA, nrow = 0, ncol = 6)
results_list_max <- list()
results_list_min <- list()

for (a_bar_input in a_bar_combos) {
  #a_bar_input <- a_bar_input[[1]]
  temp_result_1 <- max_model(chronological_data, abar = a_bar_input)
  temp_summ_1 <- summary(temp_result_1)
  temp_result_2 <- min_model(chronological_data, abar = a_bar_input)
  temp_summ_2 <- summary(temp_result_2)
  
  results_matrix_max <- rbind(results_matrix_max,
                          c(temp_summ_1$effect.measures$ATE$estimate,
                            temp_summ_1$effect.measures$ATE$pvalue,
                            temp_summ_1$effect.measures$RR$estimate,
                            temp_summ_1$effect.measures$RR$pvalue,
                            temp_summ_1$effect.measures$OR$estimate,
                            temp_summ_1$effect.measures$OR$pvalue))
  results_matrix_min <- rbind(results_matrix_min,
                              c(temp_summ_2$effect.measures$ATE$estimate,
                                temp_summ_2$effect.measures$ATE$pvalue,
                                temp_summ_2$effect.measures$RR$estimate,
                                temp_summ_2$effect.measures$RR$pvalue,
                                temp_summ_2$effect.measures$OR$estimate,
                                temp_summ_2$effect.measures$OR$pvalue))
  results_list_max[[length(results_list_max) + 1]] <- temp_summ_1
  results_list_min[[length(results_list_min) + 1]] <- temp_summ_2
}
dimnames(results_matrix_max) <- list(Value_of_abar = as.character(a_bar_combos),
                                     Statistics = c('ATE_Estimate','ATE_P_Value',
                                                    'RR_Estimate','RR_P_Value',
                                                    'OR_Estimate','OR_P_Value'))
dimnames(results_matrix_min) <- dimnames(results_matrix_max)

# Output

output_path <- 'Output_Text_x.txt'
sink(file = output_path, type = 'output')

cat('SUMMARY OF COMPARISON STATISTICS\n\n',
    'OUTPUT: Maximal Model Statistics\n\n')
print(results_matrix_max)

cat('\n-\t\t-\t\t-\n\n',
    'OUTPUT: Minimal Model Statistics\n\n')
print(results_matrix_min)

cat('\n~\t\t~\t\t~\n',
    '\nCOMPLETE AND PRINTED STATISTICS\n')

for (i in 1:length(a_bar_combos)) {
  a_bar_input <- a_bar_combos[[i]]
  
  cat('\n OUTPUT: Maximal Model\n\t(abar =',
      row.names(results_matrix_max)[i], ')\n\n')
  
  print(results_list_max[[i]])
  
  if (i < length(a_bar_combos)) {
    cat('-\t\t-\t\t-\n')
  } else {cat('\n~\t\t~\t\t~\n')}
  
}

for (i in 1:length(a_bar_combos)) {
  a_bar_input <- a_bar_combos[[i]]
  
  cat('\n OUTPUT: Minimal Model\n\t(abar =',
      row.names(results_matrix_max)[i], ')\n\n')
  
  print(results_list_min[[i]])
  
  if (i < length(a_bar_combos)) {
    cat('-\t\t-\t\t-\n')
  } else {cat('\n~\t\t~\t\t~\n')}
  
}
sink(file = NULL)