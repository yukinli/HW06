library(ggplot2)
library(tidyverse)
library(tidymodels)
library(corrplot)
library(ggthemes)
library(ISLR)
library(ISLR2) 
library(discrim)
library(poissonreg)
library(corrr)
library(klaR) 
tidymodels_prefer()
library(glmnet)
library(janitor)
library(dplyr)
library(rpart.plot)
library(vip)
library(randomForest)
library(xgboost)

mydb<-dbConnect(RSQLite::SQLite(),"my-db.sqlite") 
pokemon<-read.csv("/Users/yukinli/Downloads/Pokemon.csv") 
view(pokemon..1)
#1
pokemon_1<-clean_names(pokemon)
filter(pokemon_1)
pokemon_2<-filter(pokemon_1,type_1=='Bug'| type_1=='Fire'| type_1=='Grass' |type_1=='Normal'|type_1=='Water'|type_1=='Psychic')
view(pokemon_2)
pokemon_2<-mutate_at(pokemon_2,vars(type_1,legendary), as.factor)
view(pokemon_2)
set.seed(3435)
pokemon_split <- initial_split(pokemon_2, prop = 0.80,
                               strata = type_1)
pokemon_train <- training(pokemon_split)
pokemon_test <- testing(pokemon_split)
pokemon_fold <- vfold_cv(pokemon_train, v = 5, strata = type_1)
pokemon_recipe <- recipe(type_1 ~ legendary+ generation+ sp_atk+ attack+speed+ defense+hp+ + sp_def ,data= pokemon_train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

#2
cor_pokemon <- pokemon_train %>%
  select(-type_1,type_2,name,legendary) %>%
  correlate()
rplot(cor_pokemon)

#3
tree_spec <- decision_tree() %>%
  set_engine("rpart")
class_tree_spec <- tree_spec %>%
  set_mode("classification")
class_tree_fit <- class_tree_spec %>%
  fit(type_1 ~ ., data = pokemon_train)

class_tree_wf <- workflow() %>%
  add_model(class_tree_spec %>% set_args(cost_complexity = tune())) %>%
  add_formula(type_1 ~ .)
augment(class_tree_fit, new_data = pokemon_test) %>%
  accuracy(truth = typy_1, estimate = .pred_class)
set.seed(3435)
param_grid <- grid_regular(cost_complexity(range = c(-3, -1)), levels = 10)
tune_res <- tune_grid(
  class_tree_wf, 
  resamples = pokemon_fold, 
  grid = param_grid, 
  metrics = metric_set(roc_auc)
)
autoplot(tune_res)

#4
collect_metrics(tune_res)
arrange(tune_res)

#5

class_tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot()
bagging_spec <- rand_forest(mtry = .cols()) %>%
  set_engine("randomForest", importance = "impurity") %>%
  set_mode("ranger")
bagging_fit <- bagging_spec %>%
  fit(type_1 ~ ., data = pokemon_train)
bagging_wf <- workflow() %>%
  add_model(bagging_spec %>% set_args(cost_complexity = tune())) %>%
  add_formula(type_1 ~ .)
tune_res <- tune_grid(
bagging_wf, 
resamples = pokemon_fold, 
grid = param_grid, 
metrics = metric_set(roc_auc)
)
param_grid <- grid_regular(cost_complexity(range = c(-3, -1)), mtry= 8)
  
#6
autoplot(bagging_spec)
  
 #7
collect_metrics(bagging_spec)
arrange(bagging_spec)
  
#8
vip(bagging_fit)
  
#9
boost_spec <- boost_tree(trees = 5000, tree_depth = 4) %>%
  set_engine("xgboost") %>%
  set_mode("regression")
boost_fit <- fit(boost_spec, sale_price ~ ., data = pokemon_train)
augment(boost_fit, new_data = ames_test) %>%
  rmse(truth = sale_price, estimate = .pred)
param_grid <- grid_regular(cost_complexity(range = c(-10, 2000)), mtry= 8)
autoplot(boost_spec)
collect_metrics(boost_spec)
arrange(boost_spec)
  
#10
best_complexity <- select_best(tune_res)
class_tree_final <- finalize_workflow(class_tree_wf, best_complexity)
class_tree_final_fit <- fit(class_tree_final, data = pokemon_test)
  
best_complexity <- select_best(tune_res)
bagging_final <- finalize_workflow(bagging_wf, best_complexity)
bagging_fit <- fit(bagging_final, data = pokemon_test)
  
best_complexity <- select_best(tune_res)
bagging_final <- finalize_workflow(boost_wf, best_complexity)
bagging_fit <- fit(bagging_final, data = pokemon_test)
  
  
  
  
  

