# shared_lib_path <- "/work/crss8030/instructor_data/shared_R_libs"

#class_packages <- c("tidymodels", "tidyverse", "vip", "ranger", "finetune", "parsnip", "reticulate", "xgboost", "doParallel", "lme4", "here")

#install.packages(class_packages, lib = shared_lib_path)

#install.packages("lightgbm")
# .libPaths(c("/work/crss8030/instructor_data/shared_R_libs", .libPaths()))

library(tidyverse)
library(lightgbm)
library(xgboost)
library(caret)
library(lubridate)
library(readr)
library(dplyr)
library(tidymodels)
library(vip)
library(ranger)
library(finetune)
library(ggridges)
library(doParallel)
library(glmnet)
library(finetune)
library(janitor)
library(xgboost)
library(lme4)
library(quarto)
library(here)
here::here()

dir.create(here("teach_output"), showWarnings = FALSE)
dir.create(here("teach_output", "png"), showWarnings = FALSE)
dir.create(here("teach_output", "csv"), showWarnings = FALSE)
dir.create(here("teach_output", "rds"), showWarnings = FALSE)


training_metadata <- read_csv(here("data", "training", "training_meta.csv"))

training_soil <- read_csv(here("data", "training", "training_soil.csv"))

training_trait <- read_csv(here("data", "training", "training_trait.csv"))

testing_submission <- read_csv(here("data", "testing", "testing_submission.csv"))

testing_meta <- read_csv(here("data", "testing", "testing_meta.csv"))

testing_soil <- read_csv(here("data", "testing", "testing_soil.csv"))

fieldweather_submission <- read_csv(here("data", "fieldweatherdata_testing.csv"))



training_soil1 <- training_soil %>%
  select(!(year)) %>%
  separate(site,
                   into = c("site", "year")) %>%
  mutate(year = as.numeric(year)) %>%
  filter(nchar(as.character(year)) == 4) 

training_metadata1<- training_metadata %>%
  separate(site, 
                  into = c("site", "x")) %>%
  select(!(x)) %>% #remove x column
  unique() %>%
  filter(
    latitude >= 24.5 & latitude <= 49.5,
    longitude >= -125 & longitude <= -66.5
  ) ##Removing sites outside US boundaries

write_csv(training_metadata1,
          here::here("data", "training", "training_metadata1.csv"))

training_trait1 <- training_trait %>%
  separate(site, 
                  into = c("site", "x")) %>%
  select(!(x)) %>% #remove x column with the -Dry, -Early, -Late
  mutate(
    across(c(date_planted, date_harvested),
           ~as.Date(.x, format = "%m/%d/%y")),
    
    day_of_year_planted = yday(date_planted),
    day_of_year_harvested = yday(date_harvested), .before = date_harvested
  )

#view(training_soil1)
#view(training_metadata1)
#view(training_trait1)

training_merged <- training_trait1 %>%
  left_join(training_soil1, by = c("site", "year")) %>%
  left_join(training_metadata1, by = c("site", "year"))%>%
  select(-replicate, -block, -previous_crop, -grain_moisture)

library(USAboundaries)
library(ggplot2)
library(dplyr)
library(sf)
site_locations <- us_states() %>%
filter(!(state_abbr %in% c("PR", "AK", "HI")))
distributions_usa <- ggplot() +
  geom_sf(data = site_locations) +
  geom_point(data = training_merged, 
             aes(x = longitude,
                 y = latitude,
                color = yield_mg_ha)
  )

ggsave(
  plot = distributions_usa,
  filename = here ("teach_output", "png", "distributions_usa.png"),
  height = 6,
  width = 9,
  dpi = 150
)
#Making a map to identify the site distributions. PR, AK, HI were removed since they did not have sites. Points exist in the Mediterranean between Corsica and Italy, i.e. Site NEH2 (which exists in the final testing_submission file).


#Making Sites Only Identical Across Training and Testing

setdiff(testing_submission$site, training_merged$site)  # sites in df1 but not df2
setdiff(training_merged$site, testing_submission$site)  # sites in df2 but not df1

#Drop sites from training_merged that to not need to be predicted to complete testing_submission. ONH1 and 2 are in proximity to one another, however to not appear in the testing_submission dataframe. ONH3 (a new site) appears in testing_submission. Potential to predict ONH3 based on ONH1 and ONH2, so will not remove them.

training_merged1 <- training_merged %>%
  filter(!(site %in% c("IAH1a" ,"IAH1b" ,"IAH1c", "IAH3" , "MOH2" , "NYH1" ,"TXH2" , "IAH1" , "KSH1" , "NEH4" , "SDH1" , "ARH1" , "ARH2",  "COH1" ,"GEH1",  "TXH3" , "TXH4" , "NYS1", "ONH1", "ONH2" ))) %>%
  mutate(id = row_number(), .before = "year") #adding an ID column at the front of the table to identify rows

write_csv(training_merged1,
          here::here("data", "training", "training_merged1.csv"))

testing_submission1 <- testing_submission %>%
  filter(!(site %in% c("ONH3")))
  
write_csv(training_merged1,
          here::here("data", "testing", "testing_submission1.csv"))
#confirm: 
setdiff(testing_submission1$site, training_merged1$site)  # sites in df1 but not df2
setdiff(training_merged1$site, testing_submission1$site)  # sites in df2 but not df1

#both have 0 differences in site now

fieldweather <- read_csv(here("data", "fieldweatherdata.csv"))

fieldweatherdata <- fieldweather %>%
  select(-gdd)

summary(fieldweatherdata)


#To explore the weather data distributions

fieldweatherdata %>% 
  pivot_longer(cols = dayl_s:gdd_rounded) %>%
  ggplot(aes(x = value)) +
  geom_density() + 
  facet_wrap(~name, scales = "free")


fieldweather1 <- fieldweatherdata %>%
  # Selecting needed variables
  dplyr::select(-tile, -altitude) %>%
# Creating a date class variable  
  mutate(date_chr = paste0(year, "/", yday)) %>% #paste0 helps to merge columns together
  mutate(date = as.Date(date_chr, "%Y/%j"))


weather_features <- training_merged1 %>%
  select(site, year, date_planted, date_harvested) %>%
  distinct() %>%
  left_join(fieldweather1, by = c("site", "year")) %>%
  filter(date >= date_planted,
         date <= date_harvested) %>%
  group_by(site, year, date_planted, date_harvested) %>%
  summarise(
    n_days = n(),
    sum_gdd = sum(gdd_rounded, na.rm = TRUE),
    across(
      c(dayl_s, srad_w_m_2, tmax_deg_c, tmin_deg_c, vp_pa),
      mean, na.rm = TRUE,
      .names = "mean_{.col}"
    ),
    sum_precip = sum(prcp_mm_day, na.rm = TRUE),
    .groups = "drop"
  )

weather_merged <- training_merged1 %>%
  filter(year != 2014) %>%
  left_join(weather_features,
            by = c("site", "year", "date_planted", "date_harvested")) %>%
  select(-id, replicate, block)


write_csv(weather_merged,
          here::here("data", "weather_merged.csv"))

#Plotting all variables by site

allplots <- weather_merged %>%
  pivot_longer(sum_gdd:sum_precip) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(data, name,
                     ~ggplot(data = .x,
       aes(x = value,
           y = site,
           fill = stat(x))
       ) +
  geom_density_ridges_gradient(scale = 3,
                               rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "C", alpha = .8) +
  theme(legend.position = "none") +
  labs(x= .y,
       y = "site")))
  

set.seed(76332)

# Splitting the Data for Training
merged_split <- initial_split(weather_merged, prop = 0.7, strata = yield_mg_ha)
merged_split

merged_train <- training(merged_split) %>%
  select(-c(date_planted, date_harvested))
merged_train

merged_test <- testing(merged_split)
merged_test

density_plot_train_to_test_set <- ggplot() +
  geom_density(data = merged_train, 
               aes(x = yield_mg_ha),
               color = "red") +
  geom_density(data = merged_test, 
               aes(x = yield_mg_ha),
               color = "blue") 


ggsave(
  plot = density_plot_train_to_test_set,
  filename = here ("teach_output", "png", "density_plot_train_to_test_set.png"),
  height = 6,
  width = 9,
  dpi = 150
)

#RECIPE

merged_recipe <- recipe(yield_mg_ha ~ ., data = merged_train) %>%
  step_novel(all_nominal_predictors()) %>% #renames "new" hybrids as "new" category so that the model does not return an error
  step_unknown(all_nominal_predictors()) %>% #handling NAs in categorical columns
  step_dummy(all_nominal_predictors()) %>%  #make dummy variables on categorical variables like previous crop
  step_impute_median(all_numeric_predictors())#imputing NA values with median


#DATA PREP
merged_prep <- merged_recipe %>%
  prep()


# helps to see all the available hyperparameter we can tune for a model
show_model_info("boost_tree")
# helps to see all the models that we can run with the package parsnip
get_from_env("models")

###setting up xgboost

xgb_spec <- 
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    learn_rate = tune()
    ) %>%
  set_engine ("xgboost") %>%
  set_mode("regression")

xgb_spec

### cross-validation
set.seed(235)
resampling_foldcv <- vfold_cv(merged_train, v = 3)

### These are removed below because the computational speed is heavily compromised. However, it would improve accuracy if left in. 

# Create leave one year out cv object from the sampling data
#resampling_fold_loyo <- group_vfold_cv(merged_train,
                                    #   group = year)

# Create leave one location out cv object from the sampling data
#resampling_fold_loso <- group_vfold_cv(merged_train,
                                      # group = site)


###Sampling using Latin-Hypercube
set.seed(12345)
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  learn_rate(),
  trees(),
  size = 20
)
xgb_grid

xgb_latin_hyper_plot <- ggplot(data = xgb_grid,
       aes(x = tree_depth, 
           y = min_n)) +
  geom_point(aes(color = factor(learn_rate),
                 size = trees),
             alpha = .5,
             show.legend = FALSE)

ggsave(
  plot = xgb_latin_hyper_plot,
  filename = here ("teach_output", "png", "xgb_latin_hyper_plot.png"),
  height = 6,
  width = 9,
  dpi = 150
)

### Adaptive Grid Search to Identify best model later
n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE"))

if (is.na(n_cores)) n_cores <- parallel::detectCores() - 1

# Start the Cluster
cl <- makePSOCKcluster(n_cores)

registerDoParallel(cl)

cat(paste0("\nFound and registered ", n_cores, " cores to work with\n"))

set.seed(76544)

#Direct tune using the only resampling method defined above (vfold, resampling_foldcv)
results_xgb <- tune_race_anova(
  object = xgb_spec,
  preprocessor = merged_recipe,
  resamples = resampling_foldcv,
  grid = xgb_grid,
  control = control_race(save_pred = TRUE,
    parallel_over = "everything"))

stopCluster(cl)

#Getting the RMSE and r2 from that resampling method
best_rmse_xgb <- results_xgb %>%
  select_best(metric = "rmse")

best_r2_xgb <- results_xgb %>%
  select_best(metric = "rsq")


#Final Specification
final_spec_xgb <- boost_tree(
  trees = best_rmse_xgb$trees,
  tree_depth = best_rmse_xgb$tree_depth,
  min_n = best_rmse_xgb$min_n,
  learn_rate = best_rmse_xgb$learn_rate
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

final_spec_xgb

###Validation
set.seed(10)
final_fit_xgb <- last_fit(final_spec_xgb,
                merged_recipe,
                split = merged_split)
final_fit_xgb %>%
  collect_predictions() #provides predictions on the test set
final_fit_xgb %>%
  collect_metrics() %>% #gives the rmse and r2 on the test set
  mutate(estimate = round(.estimate, 3))

###setting up LightGBM

lgbm_spec <- 
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    learn_rate = tune()
    ) %>%
  set_engine ("lightgbm") %>%
  set_mode("regression")

lgbm_spec

### cross-validation
set.seed(1235)
resampling_foldcv_lgbm <- vfold_cv(merged_train, v = 3)

###Sampling using Latin-Hypercube
set.seed(123456)
lgbm_latin_hyper_plot <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  learn_rate(),
  trees(),
  size = 20
)


ggplot(data = lgbm_latin_hyper_plot,
       aes(x = tree_depth, 
           y = min_n)) +
  geom_point(aes(color = factor(learn_rate),
                 size = trees),
             alpha = .5,
             show.legend = FALSE)
ggsave(
  plot = lgbm_latin_hyper_plot,
  filename = here ("teach_output", "png", "lgbm_latin_hyper_plot.png"),
  height = 6,
  width = 9,
  dpi = 150
)
### Adaptive Grid Search to Identify best model later
n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE"))

if (is.na(n_cores)) n_cores <- parallel::detectCores() - 1

# Start the Cluster
cl <- makePSOCKcluster(n_cores)

registerDoParallel(cl)

cat(paste0("\nFound and registered ", n_cores, " cores to work with\n"))

set.seed(76544)

#Direct tune using the only resampling method defined above (vfold, resampling_foldcv)
results_lgbm <- tune_race_anova(
  object = lgbm_spec,
  preprocessor = merged_recipe,
  resamples = resampling_foldcv_lgbm,
  grid = lgbm_grid,
  control = control_race(save_pred = TRUE,
    parallel_over = "everything"))

stopCluster(cl)

#Getting the RMSE and r2 from that resampling method
best_rmse_lgbm <- results_lgbm %>%
  select_best(metric = "rmse")

best_r2_lgbm <- results_lgbm %>%
  select_best(metric = "rsq")


#Final Specification
final_spec_lgbm <- boost_tree(
  trees = best_rmse_lgbm$trees,
  tree_depth = best_rmse_lgbm$tree_depth,
  min_n = best_rmse_lgbm$min_n,
  learn_rate = best_rmse_lgbm$learn_rate
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

final_spec_lgbm

###Validation
set.seed(10)
final_fit_lgbm <- last_fit(final_spec_lgbm,
                merged_recipe,
                split = merged_split)
final_fit_lgbm %>%
  collect_predictions() #provides predictions on the test set
final_fit_lgbm %>%
  collect_metrics() %>% #gives the rmse and r2 on the test set
  mutate(estimate = round(.estimate, 3))

final_spec_lgbm <- boost_tree(
  trees = best_r2_lgbm$trees,           # Number of boosting rounds (trees)
  tree_depth = best_r2_lgbm$tree_depth, # Maximum depth of each tree
  min_n = best_r2_lgbm$min_n,           # Minimum number of samples to split a node
  learn_rate = best_r2_lgbm$learn_rate  # Learning rate (step size shrinkage)
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

final_spec_lgbm

set.seed(10)
final_fit_lgbm <- last_fit(final_spec_lgbm,
                merged_recipe,
                split = merged_split)

final_fit_lgbm %>%
  collect_predictions()

final_fit_lgbm %>%
  collect_metrics()

Publication_ready_xgb <- final_fit_xgb %>%
  collect_predictions() %>%
  ggplot(aes(x = yield_mg_ha,
             y = .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(20, 40)) +
  scale_y_continuous(limits = c(20, 40)) 

ggsave(
  plot = Publication_ready_xgb,
  filename = here ("teach_output", "png", "Publication_ready_xgb.png"),
  height = 6,
  width = 9,
  dpi = 150
)

Publication_ready_lgbm <- final_fit_lgbm %>%
  collect_predictions() %>%
  ggplot(aes(x = yield_mg_ha,
             y = .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(20, 40)) +
  scale_y_continuous(limits = c(20, 40))

ggsave(
  plot = Publication_ready_lgbm,
  filename = here ("teach_output", "png", "Publication_ready_lgbm.png"),
  height = 6,
  width = 9,
  dpi = 150
)

#XGBoost
vip_xgb <- final_spec_xgb %>%
  fit(yield_mg_ha ~ .,
         data = bake(merged_prep, merged_train)) %>% 
    vi() %>%
  mutate(
    Variable = fct_reorder(Variable, 
                           Importance)
  ) %>%
  ggplot(aes(x = Importance, 
             y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) +
  theme(panel.background  = element_rect(fill = "gray82"),
        panel.grid = element_blank())

cat(paste0("\nSaving...... publication ready plot for variable importance\n"))

ggsave(
  plot = vip_xgb,
  filename = here ("teach_output", "png", "vip_xgb.png"),
  height = 6,
  width = 9,
  dpi = 150
)

#LightGBM
vip_lgbm <- final_spec_lgbm %>%
  fit(yield_mg_ha ~ .,
         data = bake(merged_prep, merged_train)) %>% 
    vi() %>%
  mutate(
    Variable = fct_reorder(Variable, 
                           Importance)
  ) %>%
  ggplot(aes(x = Importance, 
             y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) +
  theme(panel.background  = element_rect(fill = "gray82"),
        panel.grid = element_blank())

cat(paste0("\nSaving...... publication ready plot for variable importance\n"))

ggsave(
  plot = vip_lgbm,
  filename = here ("teach_output", "png", "vip_lgbm.png"),
  height = 6,
  width = 9,
  dpi = 150
)


#Make a table of the RMSE and R2 Values to compare directly

metrics_xgb <- final_fit_xgb %>%
  collect_metrics()

metrics_lgbm <- final_fit_lgbm %>%
  collect_metrics()


write_csv(metrics_xgb,
          here::here("teach_output", "csv", "xgb_metrics.csv"))

write_csv(metrics_lgbm,
          here::here("teach_output", "csv", "lgbm_metrics.csv"))


metrics_xgb <- final_fit_xgb %>%
  collect_metrics() %>%
  dplyr::mutate(model = "xgboost")

metrics_lgbm <- final_fit_lgbm %>%
  collect_metrics() %>%
  dplyr::mutate(model = "lightgbm")

all_metrics <- dplyr::bind_rows(metrics_xgb, metrics_lgbm)

comparison_table <- all_metrics %>%
  dplyr::filter(.metric %in% c("rmse", "rsq")) %>%
  tidyr::pivot_wider(names_from = .metric, values_from = .estimate)

comparison_table

write_csv(comparison_table,
          here::here("teach_output", "csv", "model_comparison_metrics.csv"))


#creating an average for the growing window so that we can clculate GDD based on this range later. Created then renamed to the same column name as training_merged1 for consistency
average_growing_window_testing_submission <- training_merged1 %>%
  group_by(year, site, hybrid) %>%
  summarise(
    day_of_year_planted = round(mean(day_of_year_planted,na.rm = TRUE)),                          day_of_year_harvested = round(mean(day_of_year_harvested,na.rm = TRUE)), .groups = "drop")%>%
  mutate(
    date_planted   = ymd(paste0(year, "-01-01")) + days(day_of_year_planted - 1),
    date_harvested = ymd(paste0(year, "-01-01")) + days(day_of_year_harvested - 1)
  ) %>%
  select(-year)

average_growing_window_testing_submission

#Wrangling testing soil to separate year from site in the site column before merging for prediction
testing_soil1 <- testing_soil %>%
  separate(site, 
                  into = c("site", "x")) %>%
  select(!(x)) %>%
  unique()


merged_submission <- testing_submission %>%
  left_join(testing_soil1, by = c("site", "year")) %>%
  left_join(testing_meta, by = c("site", "year")) %>%
  select(-previous_crop)

merged_submission_final <- merged_submission %>%
  left_join(
    average_growing_window_testing_submission) %>%
  mutate(
    date_planted   = as.Date(date_planted),
    date_harvested = as.Date(date_harvested)
  )

fieldweather_submission1 <- fieldweather_submission %>%
  # Selecting needed variables
  dplyr::select(-tile, -altitude) %>%
# Creating a date class variable  
  mutate(date_chr = paste0(year, "/", yday)) %>%
  mutate(date = as.Date(date_chr, "%Y/%j"))

merged_submission_final1 <- merged_submission_final %>%
  mutate(
    date_planted = ymd(paste0(year, "-01-01")) + days(yday(date_planted) - 1),
    date_harvested = ymd(paste0(year, "-01-01")) + days(yday(date_harvested) - 1)
  )

weather_features_submission1 <- merged_submission_final1 %>%
  distinct(site, year, date_planted, date_harvested) %>%
  
  left_join(fieldweather_submission1, by = c("site", "year")) %>%
  filter(date >= date_planted,
         date <= date_harvested) %>%
  group_by(site, year, date_planted, date_harvested) %>%
  summarise(
    n_days = n(),
    sum_gdd = sum(gdd_rounded, na.rm = TRUE),
    across(
      c(dayl_s, srad_w_m_2, tmax_deg_c, tmin_deg_c, vp_pa),
      mean, na.rm = TRUE
    ),
    sum_precip = sum(prcp_mm_day, na.rm = TRUE),
    .groups = "drop"
  )

weather_merged_submission <- merged_submission_final1 %>%
  left_join(
    weather_features_submission1,
    by = c("site", "year", "date_planted", "date_harvested")
  )
write_csv(weather_merged_submission,
          here::here("data", "weather_merged_submission.csv"))

#selecting model with best r2
comparison <- all_metrics %>%
  dplyr::filter(.metric == "rmse")

best_model <- comparison %>%
  dplyr::slice_min(.estimate, n = 1) %>%
  dplyr::pull(model)

#selecting the best model to predict based on rmse
best_fit <- if (best_model == "xgboost") {
  final_fit_xgb
} else {
  final_fit_lgbm
}

best_workflow <- best_fit %>%
  extract_workflow()

# predicting the yield for all rows
pred_yield <- predict(best_workflow, merged_submission) %>%
  dplyr::pull(.pred)

# replacing all NA values with predicted value
merged_submission_final <- merged_submission %>%
  dplyr::mutate(
    yield_mg_ha = dplyr::if_else(
      is.na(yield_mg_ha),
      pred_yield,
      yield_mg_ha
    )
  ) %>% 
  dplyr::select(year, site, hybrid, yield_mg_ha)

write_csv(comparison_table,
          here::here("teach_output", "csv", "merged_submission_final.csv"))

#knitr::purl("project_code_teach.qmd", output = "project_code_teach.R", documentation = 0)
