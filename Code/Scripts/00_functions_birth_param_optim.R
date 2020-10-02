


optimize_birth_param = function(varying_par = c(),
                                varying_par_prior = list(),
                                fixed_par = list(),
                                n_init = 10,
                                sex_df, 
                                ave_daily_birth_df, 
                                actual_monthly_birth_df, 
                                verbose = FALSE){
  
  par_names = c("alpha","Tp", "beta","G","Gsd")
  # alpha is the fertility amplitude,
  # Tp is the peak time of fertility
  # beta is the amplitude (i.e the difference between the highest value and the mean, divided by the mean) of the sex curve
  # G is the average gestation period
  # Gsd is the standard deviation of the gestation period
  
  if((length(varying_par) == 0) & (length(fixed_par) == 0)){
    warning("varying_par and fixed_par are empty. All parameters will be optimized.")
    varying_par = par_names
  }
  
  if(! all(varying_par %in% par_names)) stop(paste("Parameters must be any of :",par_names))
  if(! all(names(fixed_par) %in% par_names)) stop(paste("Parameters must be any of :",par_names))
  
  if(length(varying_par) == 0){
    if(verbose) cat("nothing to optimize\n")
    # we don't need to optimize anything; we just compute the residuals
    par = c(0,0)
    
    SSR = residuals_simulated_vs_actual_monthly_birth(par = c(),
                                                      sex_df = sex_df, 
                                                      ave_daily_birth_df= ave_daily_birth_df,
                                                      actual_monthly_birth_df = actual_monthly_birth_df,
                                                      alpha = 0, Tp = 0, beta = fixed_par[["beta"]], G = fixed_par[["G"]], Gsd = fixed_par[["Gsd"]])
    
    optimized_par = data.frame(SSR = SSR, 
                               alpha = fixed_par[["alpha"]], Tp = fixed_par[["Tp"]], 
                               beta = fixed_par[["beta"]], 
                               G = fixed_par[["G"]],  Gsd = fixed_par[["Gsd"]],
                               stringsAsFactors = FALSE)
  }else{
    if(verbose) cat("Optimizing ",varying_par,"\n")
    
    # we run n_init optimization with different initial conditions
    res = foreach(i = 1:n_init, .combine = bind_rows)%do%{
      if(verbose) cat("\t",i,"\n")
      

      # initial values of the parameters
      par_init = 
        c(runif(1, min = 0.01, max = 0.1), # alpha
          i/n_init, # Tp
          runif(1,min = 0.01, max = 1), # beta
          G0 + sample(-Gsd0:Gsd0,1), # G
          runif(1, min =  Gsd0/2, max = 2*Gsd0) # Gsd
        )
      lower = c(0, 0, 0, G0-Gsd0, Gsd0/2);
      upper = c(1, 1, 2, G0+Gsd0, Gsd0*2);
      
      if(length(varying_par_prior) > 0){
        for(par_name in par_names){
          if(par_name %in% names(varying_par_prior)){
            par_init[which(par_name == par_names)] = sample(varying_par_prior[[par_name]],1)
          }
        }
      }

      ix = c()
      for(par_name in par_names){
        if(par_name %in% varying_par){
          ix = c(ix, which(par_names == par_name))
          eval(parse(text = paste0(par_name," = NULL")))
        }else{
          if(! par_name %in% names(fixed_par)) 
            stop(paste("If",par_name,"is not varying, its value needs to be specified in 'fixed_par"))
          eval(parse(text = paste0(par_name," = fixed_par[['",par_name,"']]")))
        }
      }
      par_init = par_init[ix]
      lower = lower[ix]
      upper = upper[ix]
      
      if(verbose) cat(par_init, "\n")
      # optimization
      optimized_par = optim(par = par_init, lower = lower, upper = upper,
                            fn = residuals_simulated_vs_actual_monthly_birth, 
                            sex_df = sex_df, 
                            ave_daily_birth_df = ave_daily_birth_df,
                            actual_monthly_birth_df = actual_monthly_birth_df,
                            alpha = alpha, Tp = Tp, beta = beta, G = G, Gsd = Gsd,
                            method = "L-BFGS-B")
      
      opt_alpha = ifelse("alpha" %in% varying_par, optimized_par$par[which(ix == 1)],alpha)
      opt_Tp = ifelse("Tp" %in% varying_par, optimized_par$par[which(ix == 2)], Tp)
      opt_beta = ifelse("beta" %in% varying_par, optimized_par$par[which(ix == 3)], beta)
      opt_G = ifelse("G" %in% varying_par, optimized_par$par[which(ix == 4)],G)
      opt_Gsd = ifelse("Gsd" %in% varying_par, optimized_par$par[which(ix == 5)], Gsd)
      df = data.frame(value = optimized_par$value, 
                      alpha = opt_alpha, Tp = opt_Tp, 
                      beta = opt_beta,
                      G = opt_G, Gsd = opt_Gsd)
      
      return(df)
    }
    opt = which.min(res$value)
    optimized_par = data.frame(SSR = res$value[opt], 
                               alpha = res$alpha[opt], Tp = res$Tp[opt], 
                               beta = res$beta[opt], G = res$G[opt], Gsd = res$Gsd[opt],
                               stringsAsFactors = FALSE)
  }
  
  return(optimized_par)
}



residuals_simulated_vs_actual_monthly_birth = function(
  par = c(alpha, Tp, beta, G, Gsd), 
  sex_df, 
  ave_daily_birth_df, 
  actual_monthly_birth_df,
  alpha = NULL, Tp = NULL, beta = NULL, G = NULL, Gsd = NULL
){
  
  if(is.null(alpha)) alpha = par[1]
  if(is.null(Tp)) Tp = par[2]
  if(is.null(beta)) beta = par[3]
  if(is.null(G)) G = par[4]
  if(is.null(Gsd)) Gsd = par[5]
  
  df = simulated_vs_actual_monthly_birth(alpha = alpha, Tp = Tp, beta = beta, G = G, Gsd = Gsd, 
                                         sex_df = sex_df,
                                         ave_daily_birth_df = ave_daily_birth_df, 
                                         actual_monthly_birth_df = actual_monthly_birth_df)
  SSR = sum(df$sq_residuals)
  return(SSR)
}



simulated_vs_actual_monthly_birth = function(alpha, Tp, beta , G, Gsd,
                                             sex_df, 
                                             ave_daily_birth_df, 
                                             actual_monthly_birth_df){
  
  simulated_daily_births = simulate_daily_births(alpha = alpha, Tp = Tp, beta = beta, G = G, Gsd = Gsd,
                                                 sex_df = sex_df, 
                                                 ave_daily_birth_df = ave_daily_birth_df )
  simulated_monthly_births = aggregate_monthly_births(simulated_daily_births)
  df = inner_join(simulated_monthly_births %>% select(year_month, births) %>% rename(sim_births = births),
                  actual_monthly_birth_df %>%  select(year_month, births),
                  by = "year_month")
  df = df %>% mutate(
    residuals = sim_births - births,
    sq_residuals = residuals^2
  )
  # we remove the first and last 6 months of data
  df = df %>% arrange(year_month) %>% 
    head(.,-6) %>% tail(.,-6)
  return(df)
}



aggregate_monthly_births = function(daily_births){
  df = daily_births %>% 
    mutate(year_month = year(date_births) + (month(date_births)-1)/12) %>% 
    group_by(year_month) %>% 
    summarize(n_days = n(),
              uncorrected_births = sum(births),
              births = uncorrected_births/n_days*30,
              .groups = "drop")
  return(df)
}




simulate_daily_births = function(alpha, Tp, beta, G, Gsd, 
                                 sex_df, 
                                 ave_daily_birth_df){
  
  beta_o = max(sex_df$sex - 1)
  if(beta_o != 0) sex_df = sex_df %>% mutate(sex_o = sex,
                                             rel_sex = (sex-1)/max(sex-1),
                                             sex = 1 + beta * rel_sex,
                                             sex = sex %>% pmax(.,0))
  
  df = inner_join(sex_df %>% select(date, sex), 
                  ave_daily_birth_df , by = "date")
  
  df = df %>% 
    mutate(
      year_day = year(date)+(yday(date)-1)/ifelse(leap_year(date),366,365),
      fertility = 1 + alpha*cos( (year_day - Tp) * (2 * pi)),
      conceptions = sex * fertility,
      smoothed_conceptions = smooth_conceptions(conceptions, sd_gestation = Gsd),
      date_conceptions = date,
      date_births = date + days(round(G)),
      births = ave_daily_births * smoothed_conceptions)
  
  df = df %>% 
    select(country_area, date, date_births, 
           ave_daily_births, sex, fertility, 
           conceptions, smoothed_conceptions, 
           births) %>% 
    rename(date_conceptions = date)
  
  return(df)
}



smooth_conceptions = function(conceptions, sd_gestation = 9){
  x = conceptions
  if(all(is.na(x))){stop("conceptions can't be only NAs")}
  N = (sd_gestation*3.5) %>% round()
  gestation_distribution = dnorm(-N:N,mean = 0, sd = sd_gestation) 
  xx = c(rev(x[1:N]),x, rev(x[(length(x)-N+1):length(x)]))
  y = stats::filter(x = xx, filter = gestation_distribution)
  y = y[!is.na(y)]
  return(y)
}



predict_daily_sex_behavior = function(model , date_range, country_area){
  date_seq = seq(min(date_range), max(date_range), by = 1)
  df = data.frame(date = date_seq)
  df = df %>% mutate(country_area = country_area)
  
  df = augment_with_weekdays_months_and_holidays(df)
  df$sex = predict(object = model, newdata = df, type = "response")
  return(df)
}
