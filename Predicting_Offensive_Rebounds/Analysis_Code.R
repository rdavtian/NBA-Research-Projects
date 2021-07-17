# Load libraries
library(dplyr)
library(caret)
library(usdm)
library(DMwR)

# Define log loss function
logloss = function(actual, predicted, eps = 1e-15) 
{
  predicted = pmin(pmax(predicted, eps), 1-eps) 
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
} 

# function to split a dataset into a 70/30 train, test split
train_test_split <- function(train, nrows)
{
  set.seed(42)
  train <- sample(train)[1:nrows,]
  dt <- sort(sample(nrow(train), nrow(train)*.7))
  train2 <- train[dt,]
  test2 <- train[-dt,]
  return(list(train2, test2))
}
# change directory to location of data files
setwd("C:/Users/rusla/OneDrive/OKC_Thunder/Data")

# read in player position and rebounding data 
player_positions <- read.csv("player_pos_data.csv")
player_reb <- read.csv("player_reb_data.csv")

# Create individual rebounding stats
player_stats <- player_reb %>% 
  mutate(offs_rebs_per_48_minutes = (off_rebs / minutes) * 48,
         offs_reb_success_rate = (off_rebs / off_rebchances),
         def_rebs_per_48_minutes = (def_rebs / minutes) * 48,
         def_rebs_success_rate = (def_rebs / def_rebchances),
         total_rebs = off_rebs + def_rebs,
         total_rebs_per_48_minutes = (total_rebs / minutes) * 48,
         total_rebs_success_rate = (total_rebs / (def_rebchances + off_rebchances))) %>%
  inner_join(player_positions, by = "player_id")

# Read in training player location and play by play data 
player_location <- read.csv("training_data_loc.csv")
player_play_by_play <- read.csv("training_data_pbp.csv")


# Feature Engineering and Mergeing tables
# Data Manipulation
merged <- player_location %>%
  inner_join(player_play_by_play, by = c("game_id","playbyplayorder_id","row_type")) %>%
  filter(actiondescription != "No Shot") %>%
  mutate(type_of_shot = case_when( grepl("Dunk", actiondescription) ~ "Dunk",
                                   grepl("Layup", actiondescription) ~ "Layup",
                                   grepl("Tip", actiondescription) ~ "Layup",
                                   grepl("Fade", actiondescription) ~ "Fadeaway",
                                   grepl("Free", actiondescription) ~ "JumpShot",
                                   grepl("Hook", actiondescription) ~ "Hook",
                                   grepl("Fade", actiondescription) ~ "Fadeaway",
                                   grepl("Turnaround", actiondescription) ~ "Fadeaway",
                                   grepl("Jump", actiondescription) ~ "JumpShot",
                                   grepl("Pullup", actiondescription) ~ "JumpShot",
                                   grepl("Driving", actiondescription) ~ "Layup",
                                   grepl("Running", actiondescription) ~ "Layup",
                                   TRUE ~ actiondescription)) %>%
  mutate(Made_Miss = case_when(eventdescription == "Made Shot" ~ 1,
                               eventdescription == "Missed Shot" ~ 0,
                               TRUE ~ -1))

#####################################################################################
## Find number of offensive players within a 6 ft radius around the offensive
## basket at the time of the ball hitting the rim after the shot
## Offense
radius = 6
off_player_ids <- merged %>%
  select_at(vars(playbyplayorder_id, game_id, starts_with("playerid_off"))) %>% 
  na.omit() %>%
  tidyr::gather(key = "off_player",value = "id",-playbyplayorder_id, -game_id) %>%
  mutate(off_player = substring(off_player, 14, 21)) %>%
  inner_join(player_stats[, c("player_id","minutes","offs_rebs_per_48_minutes",
                              "offs_reb_success_rate","def_rebs_per_48_minutes",
                              "def_rebs_success_rate","total_rebs_per_48_minutes",
                              "total_rebs_success_rate","position")],
             by = c("id" = "player_id"))

x_coords_at_rim_off <- merged %>% 
  select_at(vars(starts_with("AtRim_loc_x_off"), playbyplayorder_id, game_id)) %>% 
  na.omit() %>%
  tidyr::gather(key = "off_player",value = "x_coord",-playbyplayorder_id, -game_id) %>%
  mutate(off_player = substring(off_player, 17, 24))

y_coords_at_rim_off <- merged %>% 
  select_at(vars(starts_with("AtRim_loc_y_off"), playbyplayorder_id, game_id)) %>% 
  na.omit() %>%
  tidyr::gather(key = "off_player",value = "y_coord",-playbyplayorder_id, -game_id) %>%
  mutate(off_player = substring(off_player, 17, 24))

num_off_players_near_rim_ball_at_rim <- x_coords_at_rim_off %>% 
  inner_join(off_player_ids, by = c("game_id","playbyplayorder_id","off_player")) %>%
  inner_join(y_coords_at_rim_off, by = c("game_id","playbyplayorder_id","off_player")) %>%
  distinct() %>%
  mutate(near_rim_5ft_x = case_when((x_coord > -41.75 - radius) & (x_coord < -41.75 + radius) ~ 1,
  TRUE ~ 0)) %>%
  mutate(near_rim_5ft_y = case_when((y_coord > 0 - radius) & (y_coord < 0 + radius) ~ 1,
                                    TRUE ~ 0)) %>%
  mutate(near_rim = case_when((near_rim_5ft_x == 1 & near_rim_5ft_y == 1) ~ 1,
                              TRUE ~ 0)) %>%
  group_by(game_id, playbyplayorder_id) %>%
  summarize(num_off_players_near_rim_ball_at_rim = sum(near_rim),
            mean_off_rebs_per48_for_offense = weighted.mean(offs_rebs_per_48_minutes, minutes),
            mean_off_rebs_succes_for_offense = weighted.mean(offs_reb_success_rate, minutes),
            mean_def_rebs_per48_for_offense = weighted.mean(def_rebs_per_48_minutes, minutes),
            mean_def_rebs_succes_for_offense = weighted.mean(def_rebs_success_rate, minutes),
            mean_total_rebs_per48_for_offense = weighted.mean(total_rebs_per_48_minutes, minutes),
            mean_total_rebs_success_for_offense = weighted.mean(total_rebs_success_rate,minutes),
            avg_position_for_offense = weighted.mean(position, minutes))

# Find the closest offensive player to the rim at the time of the ball
# hitting the rim after the shot is taken
closest_off <- x_coords_at_rim_off %>% 
  inner_join(off_player_ids, by = c("game_id","playbyplayorder_id","off_player")) %>%
  inner_join(y_coords_at_rim_off, by = c("game_id","playbyplayorder_id","off_player")) %>%
  mutate(dist_from_rim_ball_at_rim_offense = sqrt((-41.75 - x_coord)^2 + (0 - y_coord)^2)) %>%
  group_by(game_id, playbyplayorder_id) %>% filter(dist_from_rim_ball_at_rim_offense == min(dist_from_rim_ball_at_rim_offense)) %>%
  dplyr::select(game_id, playbyplayorder_id, position, offs_rebs_per_48_minutes, offs_reb_success_rate, 
                def_rebs_per_48_minutes, def_rebs_success_rate, total_rebs_per_48_minutes, 
                total_rebs_success_rate, position)
colnames(closest_off)[c(-1,-2)] <- paste0("closest_to_rim_", colnames(closest_off)[c(-1,-2)],"_for_offense")
#############################################################################
## Find number of defensive players within a 6 ft radius around the offensive
## basket at the time of the ball hitting the rim after the shot
## Defense
radius <- 5
def_player_ids <- merged %>%
  select_at(vars(playbyplayorder_id, game_id, starts_with("playerid_def"))) %>% 
  na.omit() %>%
  tidyr::gather(key = "def_player",value = "id",-playbyplayorder_id, -game_id) %>%
  mutate(def_player = substring(def_player, 14, 21)) %>%
  inner_join(player_stats[, c("player_id","minutes","offs_rebs_per_48_minutes",
                              "offs_reb_success_rate","def_rebs_per_48_minutes",
                              "def_rebs_success_rate","total_rebs_per_48_minutes",
                              "total_rebs_success_rate","position")],
             by = c("id" = "player_id"))

x_coords_at_rim_def <- merged %>% 
  select_at(vars(starts_with("AtRim_loc_x_def"), playbyplayorder_id, game_id)) %>% 
  na.omit() %>%
  tidyr::gather(key = "def_player",value = "x_coord",-playbyplayorder_id, -game_id) %>%
  mutate(def_player = substring(def_player, 17, 24))

y_coords_at_rim_def <- merged %>% 
  select_at(vars(starts_with("AtRim_loc_y_def"), playbyplayorder_id, game_id)) %>% 
  na.omit() %>%
  tidyr::gather(key = "def_player",value = "y_coord",-playbyplayorder_id, -game_id) %>%
  mutate(def_player = substring(def_player, 17, 24))

num_def_players_near_rim_ball_at_rim <- x_coords_at_rim_def %>% 
  inner_join(def_player_ids, by = c("game_id","playbyplayorder_id","def_player")) %>%
  inner_join(y_coords_at_rim_def, by = c("game_id","playbyplayorder_id","def_player")) %>%
  distinct() %>% 
  mutate(near_rim_5ft_x = case_when((x_coord > -41.75 - radius) & (x_coord < -41.75 + radius) ~ 1,
                                    TRUE ~ 0)) %>%
  mutate(near_rim_5ft_y = case_when((y_coord > 0 - radius) & (y_coord < 0 + radius) ~ 1,
                                    TRUE ~ 0)) %>%
  mutate(near_rim = case_when((near_rim_5ft_x == 1 & near_rim_5ft_y == 1) ~ 1,
                              TRUE ~ 0)) %>%
  group_by(game_id, playbyplayorder_id) %>%
  summarize(num_def_players_near_rim_ball_at_rim = sum(near_rim),
            mean_off_rebs_per48_for_defense = weighted.mean(offs_rebs_per_48_minutes, minutes),
            mean_off_rebs_succes_for_defense = weighted.mean(offs_reb_success_rate, minutes),
            mean_def_rebs_per48_for_defense = weighted.mean(def_rebs_per_48_minutes, minutes),
            mean_def_rebs_succes_for_defense = weighted.mean(def_rebs_success_rate, minutes),
            mean_total_rebs_per48_for_defense = weighted.mean(total_rebs_per_48_minutes, minutes),
            mean_total_rebs_success_for_defense = weighted.mean(total_rebs_success_rate,minutes),
            avg_position_for_defense = weighted.mean(position, minutes))

# Find the closest defensive player to the rim at the time of the ball
# hitting the rim after the shot is taken
closest_def <- x_coords_at_rim_def %>% 
  inner_join(def_player_ids, by = c("game_id","playbyplayorder_id","def_player")) %>%
  inner_join(y_coords_at_rim_def, by = c("game_id","playbyplayorder_id","def_player")) %>%
  mutate(dist_from_rim_ball_at_rim_defense = sqrt((-41.75 - x_coord)^2 + (0 - y_coord)^2)) %>%
  group_by(game_id, playbyplayorder_id) %>% filter(dist_from_rim_ball_at_rim_defense == min(dist_from_rim_ball_at_rim_defense)) %>%
  dplyr::select(game_id, playbyplayorder_id, position, offs_rebs_per_48_minutes, offs_reb_success_rate, 
                def_rebs_per_48_minutes, def_rebs_success_rate, total_rebs_per_48_minutes, 
                total_rebs_success_rate, position)
colnames(closest_def)[c(-1,-2)] <- paste0("closest_to_rim_", colnames(closest_def)[c(-1,-2)],"_for_defense")
##########################################################################################
# Store game id, play by play id, and the response variable separately
response <- merged %>% dplyr::select(game_id, playbyplayorder_id, reboffensive)

# Store shot description variables separately along with necessary ids for joining
type_of_shots <- merged %>%
  dplyr::select(game_id, playbyplayorder_id, type_of_shot, eventdescription, fg3missed,
                fg2missed, Made_Miss)

# Create overall modeling data set by joining all tables created
modeling_df <- response %>% 
  inner_join(type_of_shots, by = c("game_id","playbyplayorder_id")) %>%
  inner_join(num_off_players_near_rim_ball_at_rim, by = c("game_id","playbyplayorder_id")) %>%
  inner_join(num_def_players_near_rim_ball_at_rim, by = c("game_id","playbyplayorder_id")) %>%
  inner_join(closest_off, by = c("game_id","playbyplayorder_id")) %>%
  inner_join(closest_def, by = c("game_id","playbyplayorder_id")) %>%
  na.omit() %>%
  mutate(fg3missed = as.factor(fg3missed),
         fg2missed = as.factor(fg3missed),
         Made_Miss = as.factor(Made_Miss),
         type_of_shot = as.factor(type_of_shot))
######################################################################################
# Balance of Classes
round(prop.table(table(modeling_df$reboffensive)),3)

# Create Differenced Data, off - def
for (i in (seq(9, 16, 1)))
{
  modeling_df[,30 + i] <- modeling_df[i] - modeling_df[i+8]
  colnames(modeling_df)[30 + i] = paste0(gsub("for.*$", "", colnames(modeling_df)[30 + i]),"diff")
}
colnames(modeling_df)[39] <- "num_players_near_rim_ball_at_rim_diff"

for (i in (seq(25, 31, 1)))
{
  modeling_df[,22 + i] <- modeling_df[i] - modeling_df[i+7]
  colnames(modeling_df)[22 + i] = paste0(gsub("for.*$", "", colnames(modeling_df)[22 + i]),"diff")
}
modeling_df$reboffensive <- as.factor(modeling_df$reboffensive)

# VIF, code to check for colinearity among the predictors 
vars <- c("num_off_players_near_rim_ball_at_rim",
          "num_def_players_near_rim_ball_at_rim")
values <- as.data.frame(modeling_df[, vars])
values$reboffensive <- as.numeric(modeling_df$reboffensive)
values = usdm::vif(values)
values = round(values$VIF,3)
vif = data.frame(matrix(nrow = 1, ncol = length(values)))
colnames(vif) = c(vars, "reboffensive")
vif = rbind(vif, values)
vif = vif[2,]
kable(vif, digits = 2, row.names = F, align = c('l','l')) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "left", fixed_thead = T)

##########################################################################################################
# xbgoost modeling
vars <- c(modeling_df %>% select_at(vars(ends_with("diff"))) %>% names(),"type_of_shot",
          "Made_Miss","fg3missed","num_off_players_near_rim_ball_at_rim",
          "num_def_players_near_rim_ball_at_rim","closest_to_rim_position_for_offense",
          "closest_to_rim_position_for_defense")
#vars <- c(colnames(modeling_df[, 6:ncol(modeling_df)]), "type_of_shot")
train <- train_test_split(modeling_df, 100000)[[1]]
test <- train_test_split(modeling_df,  100000)[[2]]
table(test$reboffensive) / nrow(test)

myFolds <- createFolds(train$reboffensive, k = 5)
train$reboffensive <- ifelse(train$reboffensive == 1, "Yes","No")
control <- trainControl(method = "cv",number = 5,
                     classProbs = TRUE, index = myFolds, savePredictions = TRUE, 
                     summaryFunction=mnLogLoss,
                     sampling = "smote")
xgboost <- train(as.formula(paste0("reboffensive ~ ", paste0(vars, collapse = " + "))), 
                 data = train, method = 'xgbTree',
                 tuneLength = 3,  
                 metric="logLoss", 
                 trControl=control)
imp = varImp(xgboost)
print(barchart(sort(rowMeans(imp$importance), decreasing = T), 
               main = "XGBoost Variable Importance", 
               xlab = "Average Level of Importance", ylab = "Variables"))

preds <- predict(xgboost, newdata = test, type = "prob")
xgboost_prob <- preds[, 2]
xgboost_raw <- predict(xgboost, newdata = test, type = 'raw')
conf_matrix <- table(test$reboffensive, xgboost_raw)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix); accuracy
precision <- conf_matrix[4] / (conf_matrix[3] + conf_matrix[4]); precision 
recall <- conf_matrix[4] / (conf_matrix[2] + conf_matrix[4]); recall
f1_score <- 2 * (precision * recall) / (precision + recall); f1_score

xgboost_raw <- as.factor(ifelse(xgboost_raw == "Yes", 1, 0))
xgboost_raw <- as.factor(ifelse(xgboost_prob >= 0.5, 1, 0))
z <- confusionMatrix(test$reboffensive, xgboost_raw); z$overall; z$byClass
logloss(as.numeric(as.character(test$reboffensive)), xgboost_prob)

test$predictions <- xgboost_prob
test[test$Made_Miss == 1,"predictions"] <- 0
logloss(as.numeric(as.character(test$reboffensive)), test$predictions)
################################################################################
# random forest modeling
vars <- c(modeling_df %>% select_at(vars(ends_with("diff"))) %>% names(),"type_of_shot",
          "Made_Miss","fg3missed","num_off_players_near_rim_ball_at_rim",
          "num_def_players_near_rim_ball_at_rim","closest_to_rim_position_for_offense",
          "closest_to_rim_position_for_defense")
#vars <- c(colnames(modeling_df[, 6:ncol(modeling_df)]), "type_of_shot")
train <- train_test_split(modeling_df, 100000)[[1]]
test <- train_test_split(modeling_df,  100000)[[2]]
table(test$reboffensive) / nrow(test)

#set.seed(42)
myFolds <- createFolds(train$reboffensive, k = 5)
train$reboffensive <- ifelse(train$reboffensive == 1, "Yes","No")
control <- trainControl(method = "cv",number = 5,
                        classProbs = TRUE, index = myFolds, savePredictions = TRUE, 
                        summaryFunction=mnLogLoss,
                        sampling = "smote")
rf <- train(as.formula(paste0("reboffensive ~ ", paste0(vars, collapse = " + "))), 
                 data = train, method = 'rf', ntrees = 200,
                 tuneLength = 4,  
                 metric="logLoss", 
                 trControl=control)
imp = varImp(rf)
print(barchart(sort(rowMeans(imp$importance), decreasing = T), 
               main = "Random Forest Variable Importance", 
               xlab = "Average Level of Importance", ylab = "Variables"))

preds <- predict(rf, newdata = test, type = "prob")
rf_prob <- preds[, 2]
rf_raw <- predict(rf, newdata = test, type = 'raw')
conf_matrix <- table(test$reboffensive, rf_raw)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix); accuracy
precision <- conf_matrix[4] / (conf_matrix[3] + conf_matrix[4]); precision 
recall <- conf_matrix[4] / (conf_matrix[2] + conf_matrix[4]); recall
f1_score <- 2 * (precision * recall) / (precision + recall); f1_score

rf_raw <- as.factor(ifelse(rf_raw == "Yes", 1, 0))
#rf_raw <- as.factor(ifelse(rf_prob >= 0.5, 1, 0))
confusionMatrix(test$reboffensive, rf_raw)
logloss(as.numeric(as.character(test$reboffensive)), rf_prob)

test$predictions <- rf_prob
test[test$Made_Miss == 1,"predictions"] <- 0
logloss(as.numeric(as.character(test$reboffensive)), test$predictions)
##################################################################################
# glmnet (penalized logistic modeling)
vars <- c(modeling_df %>% select_at(vars(ends_with("diff"))) %>% names(),"type_of_shot",
          "Made_Miss","fg3missed","num_off_players_near_rim_ball_at_rim",
          "num_def_players_near_rim_ball_at_rim","closest_to_rim_position_for_offense",
          "closest_to_rim_position_for_defense")
#vars <- c(colnames(modeling_df[, 6:ncol(modeling_df)]), "type_of_shot")
train <- train_test_split(modeling_df, 100000)[[1]]
test <- train_test_split(modeling_df,  100000)[[2]]
table(test$reboffensive) / nrow(test)

set.seed(42)
myFolds <- createFolds(train$reboffensive, k = 5)
train$reboffensive <- ifelse(train$reboffensive == 1, "Yes","No")
control <- trainControl(method = "cv",number = 5,
                        classProbs = TRUE, index = myFolds, savePredictions = TRUE, 
                        summaryFunction=mnLogLoss,
                        sampling = "smote")
#control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss,
#index = myFolds, savePredictions = TRUE)
glmnet <- train(as.formula(paste0("reboffensive ~ ", paste0(vars, collapse = " + "))), 
            data = train, method = 'glmnet', family = 'binomial', 
            preProcess = c('nzv','center','scale'),
            tuneLength = 5,  
            metric="logLoss", 
            trControl=control)
imp2 <- varImp(glmnet)
print(barchart(sort(rowMeans(imp2$importance), decreasing = T), 
               main = "GLMNET Variable Importance", 
               xlab = "Average Level of Importance", ylab = "Variables"))

preds <- predict(glmnet, newdata = test, type = "prob")
glmnet_prob <- preds[, 2]
glmnet_raw <- predict(glmnet, newdata = test, type = 'raw')
conf_matrix <- table(test$reboffensive, glmnet_raw)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix); accuracy
precision <- conf_matrix[4] / (conf_matrix[3] + conf_matrix[4]); precision 
recall <- conf_matrix[4] / (conf_matrix[2] + conf_matrix[4]); recall
f1_score <- 2 * (precision * recall) / (precision + recall); f1_score

glmnet_raw <- as.factor(ifelse(glmnet_raw == "Yes", 1, 0))
#glmnet_raw <- as.factor(ifelse(glmnet_prob >= 0.5, 1, 0))
confusionMatrix(test$reboffensive, glmnet_raw)
logloss(as.numeric(as.character(test$reboffensive)), glmnet_prob)

test$predictions <- glmnet_prob
test[test$Made_Miss == 1,"predictions"] <- 0
logloss(as.numeric(as.character(test$reboffensive)), test$predictions)

#########################################################################
# Stochastic Gradient Boosting Model

vars <- c(modeling_df %>% select_at(vars(ends_with("diff"))) %>% names(),"type_of_shot",
          "Made_Miss","fg3missed","num_off_players_near_rim_ball_at_rim",
          "num_def_players_near_rim_ball_at_rim","closest_to_rim_position_for_offense",
          "closest_to_rim_position_for_defense")
#vars <- c(modeling_df %>% select_at(vars(ends_with("diff"))) %>% names(),"type_of_shot",
#"Made_Miss","fg3missed")
vars <- c(colnames(modeling_df[, 6:ncol(modeling_df)]), "type_of_shot")
train <- train_test_split(modeling_df, 100000)[[1]]
test <- train_test_split(modeling_df,  100000)[[2]]
table(test$reboffensive) / nrow(test)

set.seed(42)
myFolds <- createFolds(train$reboffensive, k = 5)
train$reboffensive <- ifelse(train$reboffensive == 1, "Yes","No")
control <- trainControl(method = "cv",number = 5,
                        classProbs = TRUE, index = myFolds, savePredictions = TRUE, 
                        summaryFunction=mnLogLoss,
                        sampling = "smote")
#control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss,
#index = myFolds, savePredictions = TRUE)
gbm <- train(as.formula(paste0("reboffensive ~ ", paste0(vars, collapse = " + "))), 
                data = train, method = 'gbm', 
                tuneLength = 4, verbose = F,
                metric="logLoss", 
                trControl=control)
summary(gbm)
print(plot(gbm, main = 'Average LogLoss Across 5-Fold CV', 
           xlab = 'Number of Variables'))
preds <- predict(gbm, newdata = test, type = "prob")
gbm_prob <- preds[, 2]
gbm_raw <- predict(gbm, newdata = test, type = 'raw')
conf_matrix <- table(test$reboffensive, gbm_raw)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix); accuracy
precision <- conf_matrix[4] / (conf_matrix[3] + conf_matrix[4]); precision 
recall <- conf_matrix[4] / (conf_matrix[2] + conf_matrix[4]); recall
f1_score <- 2 * (precision * recall) / (precision + recall); f1_score

gbm_raw <- as.factor(ifelse(gbm_raw == "Yes", 1, 0))
#gbm_raw <- as.factor(ifelse(gbm_prob >= 0.5, 1, 0))
confusionMatrix(test$reboffensive, gbm_raw)
logloss(as.numeric(as.character(test$reboffensive)), gbm_prob)

test$predictions <- gbm_prob
test[test$Made_Miss == 1,"predictions"] <- 0
logloss(as.numeric(as.character(test$reboffensive)), test$predictions)

######################################################################################
######################################################################################
#####################################################################################
# Read in testing data files
testing_location <- read.csv("testing_data_loc.csv")
testing_play_by_play <- read.csv("testing_data_pbp.csv")

# Repeat data cleaning/wrangling/manipulation/feature engineering that created the 
# data set for modeling to the new testing data sets in order to make predictions
merged_test <- testing_location %>%
  inner_join(testing_play_by_play, by = c("game_id","playbyplayorder_id","row_type")) %>%
  filter(actiondescription != "No Shot") %>%
  mutate(type_of_shot = case_when( grepl("Dunk", actiondescription) ~ "Dunk",
                                   grepl("Layup", actiondescription) ~ "Layup",
                                   grepl("Tip", actiondescription) ~ "Layup",
                                   grepl("Fade", actiondescription) ~ "Fadeaway",
                                   grepl("Free", actiondescription) ~ "JumpShot",
                                   grepl("Hook", actiondescription) ~ "Hook",
                                   grepl("Fade", actiondescription) ~ "Fadeaway",
                                   grepl("Turnaround", actiondescription) ~ "Fadeaway",
                                   grepl("Jump", actiondescription) ~ "JumpShot",
                                   grepl("Pullup", actiondescription) ~ "JumpShot",
                                   grepl("Driving", actiondescription) ~ "Layup",
                                   grepl("Running", actiondescription) ~ "Layup",
                                   TRUE ~ actiondescription)) %>%
  mutate(Made_Miss = case_when(eventdescription == "Made Shot" ~ 1,
                               eventdescription == "Missed Shot" ~ 0,
                               TRUE ~ -1))

#############################################################################
## Offense
radius = 6
off_player_ids_test <- merged_test %>%
  select_at(vars(playbyplayorder_id, game_id, starts_with("playerid_off"))) %>% 
  tidyr::gather(key = "off_player",value = "id",-playbyplayorder_id, -game_id) %>%
  mutate(off_player = substring(off_player, 14, 21)) %>%
  inner_join(player_stats[, c("player_id","minutes","offs_rebs_per_48_minutes",
                              "offs_reb_success_rate","def_rebs_per_48_minutes",
                              "def_rebs_success_rate","total_rebs_per_48_minutes",
                              "total_rebs_success_rate","position")],
             by = c("id" = "player_id"))

x_coords_at_rim_off_test <- merged_test %>% 
  select_at(vars(starts_with("AtRim_loc_x_off"), playbyplayorder_id, game_id)) %>% 
  tidyr::gather(key = "off_player",value = "x_coord",-playbyplayorder_id, -game_id) %>%
  mutate(off_player = substring(off_player, 17, 24))

y_coords_at_rim_off_test <- merged_test %>% 
  select_at(vars(starts_with("AtRim_loc_y_off"), playbyplayorder_id, game_id)) %>% 
  tidyr::gather(key = "off_player",value = "y_coord",-playbyplayorder_id, -game_id) %>%
  mutate(off_player = substring(off_player, 17, 24))

num_off_players_near_rim_ball_at_rim_test <- x_coords_at_rim_off_test %>% 
  inner_join(off_player_ids_test, by = c("game_id","playbyplayorder_id","off_player")) %>%
  inner_join(y_coords_at_rim_off_test, by = c("game_id","playbyplayorder_id","off_player")) %>%
  distinct() %>%
  mutate(near_rim_5ft_x = case_when((x_coord > -41.75 - radius) & (x_coord < -41.75 + radius) ~ 1,
                                    TRUE ~ 0)) %>%
  mutate(near_rim_5ft_y = case_when((y_coord > 0 - radius) & (y_coord < 0 + radius) ~ 1,
                                    TRUE ~ 0)) %>%
  mutate(near_rim = case_when((near_rim_5ft_x == 1 & near_rim_5ft_y == 1) ~ 1,
                              TRUE ~ 0)) %>%
  group_by(game_id, playbyplayorder_id) %>%
  summarize(num_off_players_near_rim_ball_at_rim = sum(near_rim),
            mean_off_rebs_per48_for_offense = weighted.mean(offs_rebs_per_48_minutes, minutes),
            mean_off_rebs_succes_for_offense = weighted.mean(offs_reb_success_rate, minutes),
            mean_def_rebs_per48_for_offense = weighted.mean(def_rebs_per_48_minutes, minutes),
            mean_def_rebs_succes_for_offense = weighted.mean(def_rebs_success_rate, minutes),
            mean_total_rebs_per48_for_offense = weighted.mean(total_rebs_per_48_minutes, minutes),
            mean_total_rebs_success_for_offense = weighted.mean(total_rebs_success_rate,minutes),
            avg_position_for_offense = weighted.mean(position, minutes))

closest_off_test <- x_coords_at_rim_off_test %>% 
  inner_join(off_player_ids_test, by = c("game_id","playbyplayorder_id","off_player")) %>%
  inner_join(y_coords_at_rim_off_test, by = c("game_id","playbyplayorder_id","off_player")) %>%
  mutate(dist_from_rim_ball_at_rim_offense = sqrt((-41.75 - x_coord)^2 + (0 - y_coord)^2)) %>%
  group_by(game_id, playbyplayorder_id) %>% filter(dist_from_rim_ball_at_rim_offense == min(dist_from_rim_ball_at_rim_offense)) %>%
  dplyr::select(game_id, playbyplayorder_id, position, offs_rebs_per_48_minutes, offs_reb_success_rate, 
                def_rebs_per_48_minutes, def_rebs_success_rate, total_rebs_per_48_minutes, 
                total_rebs_success_rate, position)
colnames(closest_off_test)[c(-1,-2)] <- paste0("closest_to_rim_", colnames(closest_off_test)[c(-1,-2)],"_for_offense")
#############################################################################
## Defense
radius <- 5
def_player_ids_test <- merged_test %>%
  select_at(vars(playbyplayorder_id, game_id, starts_with("playerid_def"))) %>% 
  tidyr::gather(key = "def_player",value = "id",-playbyplayorder_id, -game_id) %>%
  mutate(def_player = substring(def_player, 14, 21)) %>%
  inner_join(player_stats[, c("player_id","minutes","offs_rebs_per_48_minutes",
                              "offs_reb_success_rate","def_rebs_per_48_minutes",
                              "def_rebs_success_rate","total_rebs_per_48_minutes",
                              "total_rebs_success_rate","position")],
             by = c("id" = "player_id"))

x_coords_at_rim_def_test <- merged_test %>% 
  select_at(vars(starts_with("AtRim_loc_x_def"), playbyplayorder_id, game_id)) %>% 
  tidyr::gather(key = "def_player",value = "x_coord",-playbyplayorder_id, -game_id) %>%
  mutate(def_player = substring(def_player, 17, 24))

y_coords_at_rim_def_test <- merged_test %>% 
  select_at(vars(starts_with("AtRim_loc_y_def"), playbyplayorder_id, game_id)) %>% 
  tidyr::gather(key = "def_player",value = "y_coord",-playbyplayorder_id, -game_id) %>%
  mutate(def_player = substring(def_player, 17, 24))

num_def_players_near_rim_ball_at_rim_test <- x_coords_at_rim_def_test %>% 
  inner_join(def_player_ids_test, by = c("game_id","playbyplayorder_id","def_player")) %>%
  inner_join(y_coords_at_rim_def_test, by = c("game_id","playbyplayorder_id","def_player")) %>%
  distinct() %>% 
  mutate(near_rim_5ft_x = case_when((x_coord > -41.75 - radius) & (x_coord < -41.75 + radius) ~ 1,
                                    TRUE ~ 0)) %>%
  mutate(near_rim_5ft_y = case_when((y_coord > 0 - radius) & (y_coord < 0 + radius) ~ 1,
                                    TRUE ~ 0)) %>%
  mutate(near_rim = case_when((near_rim_5ft_x == 1 & near_rim_5ft_y == 1) ~ 1,
                              TRUE ~ 0)) %>%
  group_by(game_id, playbyplayorder_id) %>%
  summarize(num_def_players_near_rim_ball_at_rim = sum(near_rim),
            mean_off_rebs_per48_for_defense = weighted.mean(offs_rebs_per_48_minutes, minutes),
            mean_off_rebs_succes_for_defense = weighted.mean(offs_reb_success_rate, minutes),
            mean_def_rebs_per48_for_defense = weighted.mean(def_rebs_per_48_minutes, minutes),
            mean_def_rebs_succes_for_defense = weighted.mean(def_rebs_success_rate, minutes),
            mean_total_rebs_per48_for_defense = weighted.mean(total_rebs_per_48_minutes, minutes),
            mean_total_rebs_success_for_defense = weighted.mean(total_rebs_success_rate,minutes),
            avg_position_for_defense = weighted.mean(position, minutes))

closest_def_test <- x_coords_at_rim_def_test %>% 
  inner_join(def_player_ids_test, by = c("game_id","playbyplayorder_id","def_player")) %>%
  inner_join(y_coords_at_rim_def_test, by = c("game_id","playbyplayorder_id","def_player")) %>%
  mutate(dist_from_rim_ball_at_rim_defense = sqrt((-41.75 - x_coord)^2 + (0 - y_coord)^2)) %>%
  group_by(game_id, playbyplayorder_id) %>% filter(dist_from_rim_ball_at_rim_defense == min(dist_from_rim_ball_at_rim_defense)) %>%
  dplyr::select(game_id, playbyplayorder_id, position, offs_rebs_per_48_minutes, offs_reb_success_rate, 
                def_rebs_per_48_minutes, def_rebs_success_rate, total_rebs_per_48_minutes, 
                total_rebs_success_rate, position)
colnames(closest_def_test)[c(-1,-2)] <- paste0("closest_to_rim_", colnames(closest_def_test)[c(-1,-2)],"_for_defense")

###########################################################################################

type_of_shots_test <- merged_test %>%
  dplyr::select(game_id, playbyplayorder_id, type_of_shot, eventdescription, fg3missed,
                fg2missed, Made_Miss)

final_test <-  type_of_shots_test %>%
  inner_join(num_off_players_near_rim_ball_at_rim_test, by = c("game_id","playbyplayorder_id")) %>%
  inner_join(num_def_players_near_rim_ball_at_rim_test, by = c("game_id","playbyplayorder_id")) %>%
  inner_join(closest_off_test, by = c("game_id","playbyplayorder_id")) %>%
  inner_join(closest_def_test, by = c("game_id","playbyplayorder_id")) %>%
  mutate(fg3missed = as.factor(fg3missed),
         fg2missed = as.factor(fg3missed),
         Made_Miss = as.factor(Made_Miss),
         type_of_shot = as.factor(type_of_shot)) %>% na.omit()

# Differenced Data, off - def
for (i in (seq(8, 15, 1)))
{
  final_test[,30 + i] <- final_test[i] - final_test[i+8]
  colnames(final_test)[30 + i] = paste0(gsub("for.*$", "", colnames(final_test)[30 + i]),"diff")
}
colnames(final_test)[38] <- "num_players_near_rim_ball_at_rim_diff"

for (i in (seq(24, 30, 1)))
{
  final_test[,22 + i] <- final_test[i] - final_test[i+7]
  colnames(final_test)[22 + i] = paste0(gsub("for.*$", "", colnames(final_test)[22 + i]),"diff")
}

# Make final predictions on test set
final_test$predictions <- predict(xgboost, newdata = final_test, type = "prob")[,2]
summary(final_test$predictions)
final_test[final_test$Made_Miss == 1,"predictions"] <- 0
sum(final_test$predictions >= 0.45) / nrow(final_test)
hist(final_test$predictions)

write.csv(x = final_test[, c("playbyplayorder_id","predictions")], 
          file = "testing_predictions.csv", row.names = F)
