source("Scripts/00_functions_viz.R")


print_reproducibility_receipt = function(){

}


lu = function(x){
  length(unique(x))
}


replace_NAs_with_latest_value = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)            # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) ))  # diffing the indices + length yields how often 
}                               # they need to be repeated

# copied from: https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value

sigmoid = function(x, ix, s){ 1/(1+exp(-(x-ix)*s))}


expand_compressed_tracking = function(x){
  xx = lapply(x, rep, x$stretch_length) %>%  as.data.frame()
  xx$date = xx$start_date + ave(rep(1,nrow(xx)), xx$user_id , xx$stretch_num, FUN =cumsum) - 1
  xx$tracking = 1
  xx = dplyr::select(xx,-start_date)
  return(xx)
}




augment_with_weekdays_months_and_holidays = function(df, verbose = FALSE){
  
  if(verbose) cat("Defining weekdays and months\n")
  # first we add weekdays and months
  df = df %>% 
    mutate(weekday = wday(date, label = TRUE, abbr = TRUE, week_start = 1),
           month = month(date, label = TRUE, abbr = TRUE),
           weekday_month = str_c(weekday," - ",month))
  
  # then we get the holidays
  country_area = unique(df$country_area)
  country = str_split_fixed(country_area, " - ", 2)[1]
  if(verbose) cat("Getting extended holidays\n")
  extended_holidays = get_extended_holidays(countries = country, year_range = year(range(df$date)), hdict = dict$holidays, n_days = 3)
  
  
  # we add the holiday to the input data.frame
  df = left_join(df, extended_holidays %>% select(date,holiday_name, holiday_ID, type, additive), by = "date")
  if(verbose) cat("Defining normal days\n")
  # any day that is not a (padded) holiday is a normal day
  df = df %>% mutate(holiday_name = holiday_name %>% replace_na("normal day"),
                     holiday_ID = holiday_ID %>% replace_na("normal day"),
                     type = type  %>% replace_na("N"),
                     additive = additive %>% replace_na(FALSE))
  
  if(verbose) cat("Additive holidays\n")
  # we create a new variable that changes the weekday_month to "ref_day" when the holiday is not additive
  df = df %>% mutate(
    weekday_month_x = ifelse(((type == "H") & !additive) | (holiday_name %in% c("New Year","Christmas")) , "ref_day" , weekday_month)
  )
  

  if(verbose) cat("Checking levels\n")
  # we make sure the two variables of interest are factors with the appropriate levels
  weedays_levels = lubridate::wday(1:7,week_start = 1, label = TRUE, abbr = TRUE) %>%  levels()
  months_levels = lubridate::month(1, label = TRUE, abbr = TRUE) %>% levels()
  weekday_month_levels = str_c(rep(weedays_levels,12)," - ",rep(months_levels,each = 7))
  df = df %>% mutate(weekday_month = weekday_month %>% factor(., levels = c("ref_day",weekday_month_levels)),
                     weekday_month_x =  weekday_month_x %>% factor(., levels = c("ref_day",weekday_month_levels)),
                     holiday_ID = holiday_ID %>% factor(., levels = c("normal day", unique(extended_holidays$holiday_ID)))
                     )
  if(verbose) cat("Done\n")
  df
}

get_extended_holidays = function(countries, year_range, hdict = dict$holidays, n_days = 3){
  holidays = get_holidays(countries = countries, year_range = year_range, hdict = hdict)
  extended_holidays = extend_holidays(holidays, n_days = n_days)
  extended_holidays
}


get_holidays = function(countries, year_range, hdict){
  years = year_range[1]:year_range[2]
  all_countries_holidays = foreach(cc = countries, .combine = bind_rows) %do% {
    this_country_holiday_list = hdict %>% filter(country == cc) %>% dplyr::select(holiday_name) %>% unique() %>%  unlist() %>%  set_names(NULL)
    this_country_holidays = purrr::map_dfr(.x = this_country_holiday_list, .f = function(hh){
      if(hh == "New Year"){dates = as.Date(str_c(years,"-01-01"))} # FIXED
      if(hh == "Carnival"){ # MOVING
        c.dates = as.Date(AshWednesday(year = years));
        dates = purrr::map(.x = c.dates, .f = function(d) seq(d-5,d,by = 1)) %>% unlist() %>% as.Date()     
      }
      if(hh == "Valentine's day"){dates = as.Date(str_c(years,"-02-14"))} # FIXED
      if(hh == "Brazilian Valentine's day"){dates = as.Date(str_c(years,"-06-13"))} # FIXED
      if(hh == "Good Friday"){dates = as.Date(GoodFriday(year = years))} # MOVING
      if(hh == "Easter Monday"){dates = as.Date(EasterMonday(year = years))} # MOVING
      if(hh == "Labor day"){
        if(cc == "United States"){dates = as.Date(USLaborDay(year = years)) # FIXED
        }else{dates = as.Date(str_c(years,"-05-01"))} # FIXED
      }
      if(hh == "Early May Bank Holiday"){dates = as.Date(GBMayDay(year = years))} # MOVING
      if(hh == "Ascension"){dates = as.Date(Ascension(year = years))} # MOVING
      if(hh == "War victory"){dates = as.Date(str_c(years,"-05-08"))} # FIXED
      if(hh == "Memorial day"){dates = as.Date(USMemorialDay(year = years))} # MOVING
      if(hh == "Whit Monday"){dates = as.Date(PentecostMonday(year = years))} # MOVING
      if(hh == "Corpus Christi"){dates = as.Date(CorpusChristi(year = years))} # MOVING
      if(hh == "Spring Bank Holiday"){dates = as.Date(GBBankHoliday(year = years))} # MOVING
      if(hh == "Summer Bank Holiday"){dates = as.Date(GBSummerBankHoliday(year = years))} # MOVING
      if(hh == "National day"){
        if(cc == "Brazil"){dates = as.Date(str_c(years,"-09-07"))} # FIXED
        if(cc == "France"){dates = as.Date(FRBastilleDay(year = years))} # FIXED
        if(cc == "United States"){dates = as.Date(USIndependenceDay(year = years))} # FIXED
      }
      if(hh == "Independence day"){
        if(cc == "Brazil"){dates = as.Date(str_c(years,"-09-07"))} # FIXED
        if(cc == "United States"){dates = as.Date(USIndependenceDay(year = years))} # FIXED
      }
      if(hh == "Bastille day"){dates = as.Date(FRBastilleDay(year = years))}
      if(hh == "Nossa Senhora de Aparecida"){dates = as.Date(str_c(years,"-10-12"))} # FIXED
      if(hh == "Toussaint"){dates = as.Date(str_c(years,"-11-01"))} # FIXED
      if(hh == "Day of the dead"){dates = as.Date(str_c(years,"-11-02"))} # FIXED
      if(hh == "All souls"){dates = as.Date(str_c(years,"-11-02"))} # FIXED
      if(hh == "Armistice"){dates = as.Date(str_c(years,"-11-11"))} # FIXED
      if(hh == "Proclamation of the Republic"){dates = as.Date(str_c(years,"-11-15"))} # FIXED
      if(hh == "Thanksgiving"){dates = as.Date(USThanksgivingDay(year = years))} # MOVING
      if(hh == "Christmas"){dates = as.Date(str_c(years,"-12-25"))} # FIXED
      if(hh == "Boxing Day"){dates = as.Date(str_c(years,"-12-26"))} # FIXED
      df = data.frame(country = cc, date = dates, holiday_name = hh, stringsAsFactors = FALSE)
      df
    })
    return(this_country_holidays)
  }
  all_countries_holidays = all_countries_holidays %>% mutate(additive = hdict$additive[match(holiday_name, hdict$holiday_name)])
  all_countries_holidays
}


extend_holidays = function(holidays, n_days = 3){
  if(n_days < 0) stop("'n_days must be >= 0")
  if(n_days == 0) return(holidays %>%  mutate(type = "H") %>% arrange(country, date))
  
  initial_columns = colnames(holidays)
  
  # checking if some holidays are spanning over several days (e.g. Carnival in Brazil)
  holidays = holidays %>% 
    mutate(year = year(date)) %>% 
    group_by(country, year, holiday_name) %>% 
    dplyr::mutate(n_days = n(),
                  is_multiday = (n_days > 1)) %>% 
    ungroup()
  
  # adding the holiday ID (especially useful for multiday holidays)
  holidays = holidays %>% 
    mutate(weekday = wday(date, label = TRUE, abbr = TRUE),
           holiday_ID = str_c(holiday_name, ifelse(is_multiday,str_c(" - ",weekday),"")))
  
  # holidays_base_before is keeping the first day (first holiday ID) of each holiday
  holidays_base_before = holidays %>% 
    arrange(country, date, holiday_name) %>% group_by(country, year, holiday_name) %>% top_n(n = -1, wt = date) %>% 
    ungroup %>% dplyr::select(all_of(initial_columns), holiday_ID)
  # holidays_base_after is keeping the last day (last holiday ID) of each holiday
  holidays_base_after = holidays %>% 
    arrange(country, desc(date), holiday_name) %>% group_by(country, year, holiday_name) %>% top_n(n = 1, wt = date) %>% 
    ungroup %>% dplyr::select(all_of(initial_columns), holiday_ID)
  
  # extending the holidays. extended_holidays only has the padding days (not the holidays themselves).
  extended_holidays = foreach(da = c(-n_days:-1,1:n_days), .combine = bind_rows) %do% {
    if(da > 0){ holidays_base = holidays_base_after }else{ holidays_base = holidays_base_before }
    e_holidays = holidays_base %>% 
      mutate(date = date + da,
             holiday_ID = str_c(holiday_name, " ",ifelse(da>0,"+",""),da))
    e_holidays
  }
  extended_holidays = extended_holidays %>%  arrange(country, date) %>% mutate(weekday = wday(date, label = TRUE, abbr = TRUE))
  
  # now we are merging the holidays with the padding days
  combined_holidays = bind_rows(
    holidays %>% dplyr::select(all_of(initial_columns), holiday_ID, weekday) %>% mutate(type = "H"),
    extended_holidays %>% mutate(type = "E")
  ) %>%  arrange(country,date,type, holiday_name)
  
  # remove duplicates; giving priorities to holidays
  combined_holidays = combined_holidays %>% 
    mutate(priority_type = type %>% factor(., levels = c("H","E"))) %>% 
    arrange(country, date, priority_type) %>% 
    group_by(country, date) %>% 
    slice_head(n = 1) %>% 
    select(-priority_type) %>%  ungroup()
  
  return(combined_holidays)
}



reduce_storage_size_of_glm_model = function(model){
  model$linear.predictors = NULL
  model$R = NULL
  model$fitted.values = NULL
  model$residuals = NULL
  model$effects = NULL
  model$deviance = NULL
  model$weights = NULL
  model$prior.weights = NULL
  model$df.residual = NULL
  model$df.null = NULL
  model$y = NULL
  model$data = NULL
  model$model = NULL
  model
}


