
optimize_params_for_model_A = function(category, categories){
  this_category = categories %>% filter(cat == category)
  location = this_category$country_area
  bc = this_category$BC
  ac = this_category$age_cat
  st = this_category$sex_type
  
  # retrieving birth data for the location
  this_loc_ave_daily_births = average_daily_births_df %>% filter(country_area == location)
  this_loc_births = official_birth_records %>% filter(country_area == location)
  # G: average gestation period for this location
  G = G_table$G[G_table$country_area == location] * 7
  
  # predicting sex
  j = which((sex_models_df$country_area == location) & (sex_models_df$BC == bc) & (sex_models_df$age_cat == ac) & (sex_models_df$sex_type == st))
  this_cat_model = sex_models[[j]]$model
  this_cat_predicted_sex = predict_daily_sex_behavior(model = this_cat_model, 
                                                      date_range = range(this_loc_ave_daily_births$date), 
                                                      country_area = location)
  
  # fixed parameters
  beta = max(this_cat_predicted_sex$sex)-1
  
  # optimization (there is no parameter to optimize, we are just computing the residuals)
  optimized_params_A = optimize_birth_param(varying_par = c(),
                                            fixed_par = list(alpha = 0, Tp = 0, beta = beta, G = G, Gsd = Gsd0),
                                            sex_df = this_cat_predicted_sex, 
                                            ave_daily_birth_df = this_loc_ave_daily_births, 
                                            actual_monthly_birth_df = this_loc_births)
  optimized_params_A = optimized_params_A %>%  mutate(country_area = location, model = "A", BC = bc, age_cat = ac, sex_type = st)
  
  optimized_params_A
}

optimize_params_for_model_B = function(location, categories){
  
  # retrieving birth data for the location
  this_loc_ave_daily_births = average_daily_births_df %>% filter(country_area == location)
  this_loc_births = official_birth_records %>% filter(country_area == location)
  # G: average gestation period for this location
  G = G_table$G[G_table$country_area == location] * 7
  
  # sex data 
  constant_sex = this_loc_ave_daily_births %>%  mutate(sex = 1)
  
  # optimization
  optimized_params_B = optimize_birth_param(
    varying_par = c("alpha", "Tp"),
    varying_par_prior = list(
      alpha = rnorm(100, mean = model_B_param_rough_estimates$amplitude[model_B_param_rough_estimates$country_area == location], sd = 0.01),
      Tp = rnorm(100, mean = model_B_param_rough_estimates$fertility_peak[model_B_param_rough_estimates$country_area == location], sd = 0.05)
      ),
    fixed_par = list(beta = 0, G = G, Gsd = Gsd0),
    n_init = 3,
    sex_df = constant_sex, 
    ave_daily_birth_df = this_loc_ave_daily_births, 
    actual_monthly_birth_df = this_loc_births, 
    verbose = FALSE)
  
  optimized_params_B = optimized_params_B %>%  mutate(model = "B") %>% 
    expand_grid(., categories %>% filter(country_area == location) %>% select(country_area, BC, age_cat, sex_type))
  
  
  optimized_params_B
}


optimize_params_for_model_C = function(category, categories, optimized_params_B){
  cat("\t",as.character(category))
  this_category = categories %>% filter(cat == category)
  location = this_category$country_area
  bc = this_category$BC
  ac = this_category$age_cat
  st = this_category$sex_type
  
  # retrieving birth data for the location
  this_loc_ave_daily_births = average_daily_births_df %>% filter(country_area == location)
  this_loc_births = official_birth_records %>% filter(country_area == location)
  # G: average gestation period for this location
  G = G_table$G[G_table$country_area == location] * 7
  
  # predicting sex
  j = which((sex_models_df$country_area == location) & (sex_models_df$BC == bc) & (sex_models_df$age_cat == ac) & (sex_models_df$sex_type == st))
  this_cat_model = sex_models[[j]]$model
  this_cat_predicted_sex = predict_daily_sex_behavior(model = this_cat_model, 
                                                      date_range = range(this_loc_ave_daily_births$date), 
                                                      country_area = location)
  
  
  beta = max(this_cat_predicted_sex$sex)-1
  
  # optimization
  optimized_params_C = optimize_birth_param(
    varying_par = c("alpha", "Tp", "beta"),
    varying_par_prior = list(
      alpha = rnorm(100, mean = optimized_params_B$alpha, sd = 0.01),
      Tp = rnorm(100, mean = optimized_params_B$Tp, sd = 0.05),
      beta = runif(100, min = 0.1 , max = 2*beta)
    ),
    fixed_par = list(G = G, Gsd = Gsd0),
    n_init = 3,
    sex_df = this_cat_predicted_sex, 
    ave_daily_birth_df = this_loc_ave_daily_births, 
    actual_monthly_birth_df = this_loc_births, verbose = FALSE)
  
  optimized_params_C = optimized_params_C %>%  
    mutate(country_area = location, model = "C", BC = bc, age_cat = ac, sex_type = st)
  
  optimized_params_C
}

