

ggplot_user_tracking = function(t, summary = FALSE, fertility_only = FALSE){
  
  t = t[which(t$category != "weight"),]
  t$group = feature_dict$group[match(t$type, feature_dict$type)]
  t$group = factor(t$group, levels = levels(feature_dict$group))
  t$category = factor(t$category, levels = rev(levels(feature_dict$category)))
  t$type = factor(t$type, levels = levels(feature_dict$type))
  
  short_user_id = unique(t$user_id)
  short_user_id = paste0(substr(short_user_id, 1,4), "...",substr(short_user_id, nchar(short_user_id)-4,nchar(short_user_id) ))
  
  
  if(fertility_only){
    t = t[t$group %in% c("sex","period","fertility"),]
  }
  
  if(summary){
    # change type
    # change colors
    
  }else{
    colors = as.character(feature_dict$color[which(feature_dict$type %in% unique(t$type))])
  }
  
  g = ggplot(t, aes(x = date, y = category, col = type, group = group)) + 
    geom_vline(xintercept = t$date[t$cycleday == 1], col = "gray90")+
    geom_point(shape = '|', size = 3)  + 
    facet_grid(group ~ . , scale = "free_y", space = "free_y") +
    scale_color_manual(values = colors, guide = guide_legend())+
    theme(
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(angle = 0))+
    ggtitle(paste0(short_user_id," | ",unique(t$country)," | ", unique(t$age)," | ",unique(t$bmi_cat)))
  
  if(fertility_only){g = g + theme(legend.position="bottom")}else{g = g + guides(col = FALSE)}
  
  return(g)
  
}



get_model_coeficient_df = function(model){
  coef = model$coefficients
  coef = data.frame(coef_name = names(coef), value = coef); rownames(coef) = 1:nrow(coef)
  coef = coef %>% mutate(
    category = ifelse(str_detect(coef_name,"weekday_month_x"), "Weekdays",
                      ifelse(str_detect(coef_name,"holiday_ID"),"Holidays", "Intercept")),
    is_weekday = (category == "Weekdays"),
    name = ifelse(is_weekday, str_remove(coef_name, "weekday_month_x"), str_remove(coef_name,"holiday_ID")),
    weekday = ifelse(is_weekday, str_split_fixed(name, " - ", n = 2)[,1], ""),
    month = ifelse(is_weekday, str_split_fixed(name, " - ", n = 2)[,2], "") %>% 
      factor(., levels = c(levels(model$data$month),"")),
    day = ifelse(is_weekday, weekday, str_remove(name,"( [+-][0-9])")) %>%  
      str_split_fixed(.,pattern = " - ", n = 2) %>% set_colnames(c("d","f")) %>%  as.data.frame() %>%  dplyr::select(d) %>% 
      unlist() %>%  as.character(),
    offset = str_extract(name,"([+-][0-9])") %>% replace_na(0) %>%  as.integer(),
    subcat = ifelse(is_weekday,month %>% as.character(), day),
    xx = ifelse(is_weekday, day, offset),
    x = ifelse(str_detect(name, "Carnival - "), str_split_fixed(name, " - ",n =2)[,2] %>% str_sub(start = 1, end = 2) ,xx)
  )
  
  country = unique(model$data$country_area) %>% str_split_fixed(.," - ",2) %>% .[1]
  
  coef = coef %>% 
    mutate(
      subcat = ifelse(category == "Holidays", dict$holidays$holiday_name_wrapped[match(subcat, dict$holidays$holiday_name)], subcat)
    )
  
  coef = coef %>% mutate(
    day = factor(day %>% as.character(), 
                 levels = c(levels(wday(1:7, label = TRUE, abbr = TRUE, week_start = 1)),
                            "(Intercept)",
                            dict$holidays$holiday_name_wrapped[dict$holidays$country == country] %>%  unique()
                 )
    ),
    x = factor(x, levels = c(levels(wday(1:7, label = TRUE, abbr = TRUE, week_start = 1)),-3:0,c("Fr","Sa","Su","Mo","Tu","We"),1:3)),
    category = factor(category, levels = c("Intercept","Holidays","Weekdays")),
    subcat = factor(subcat, 
                    levels = c(levels(month), 
                               "(Intercept)",
                               dict$holidays$holiday_name_wrapped[dict$holidays$country == country] %>%  unique()
                    )),
    color = ifelse(is_weekday, 
                   dict$weekdays$color[match(x,dict$weekdays$abbr)] ,
                   ifelse(category == "Intercept",
                          "black",
                          dict$holidays$color[match(subcat,dict$holidays$holiday_name_wrapped)]
                   )
    )
  )
  coef
}

ggplot_sex_activity_glm_coefficient = function(model = glm_model_both_y, show_intercept = TRUE, show_weekly_patterns = TRUE, ylim = NULL){
  
  coef = get_model_coeficient_df(model = model)
  
  if(!show_intercept) coef = coef %>% filter(category != "Intercept")
  if(!show_weekly_patterns) coef = coef %>% filter(category != "Weekdays")
  if(is.null(ylim)) ylim = range(coef$value, na.rm = TRUE)
  
  
  g_coef = ggplot(coef, aes(x = x, y = value, col = color)) 
  g_coef = g_coef +
    geom_hline(yintercept = 0)+
    geom_point()+
    geom_segment(aes(xend = x, yend = 0), size = 0.8)+
    xlab("")+ylab("relative sexual activity")+
    scale_color_identity()+
    scale_y_continuous(breaks = seq(-1,1, by = 0.25), limits = ylim)+
    guides(col = FALSE, alpha = FALSE)
  
  if(!(show_intercept | show_weekly_patterns)){
    g_coef = g_coef + facet_grid(. ~  subcat , scale = "free", space = "free_x") 
  }else{
    g_coef = g_coef + facet_grid(. ~ category + subcat , scale = "free", space = "free_x")
  }
  
  g_coef = g_coef +  
    theme(strip.text.x = element_text(angle = 90, hjust = 0),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  g_coef
}

ggplot_sex_activity_data_fitted_and_residuals = function(model = glm_sex_behavior){
  data_df = glm_sex_behavior$data
  data_df$fitted_value = glm_sex_behavior$fitted.values
  data_df = data_df %>% 
    mutate(actual_value = x,
           residuals = actual_value - fitted_value)
  
  data_df_long = data_df %>% 
    filter(weekday_month != "ref_day") %>% 
    select(date, actual_value, fitted_value, residuals) %>% 
    pivot_longer(cols = c("actual_value","fitted_value","residuals"), names_to = "variable") %>% 
    mutate(type = ifelse(variable == "residuals", "residuals","x"), # rel. changes in sex behavior
           base_year = ifelse(date < as.Date("2018-07-01"),"2017-2018","2018-2019"), # potentially change this to make it more flexible to other data input
           year_lag = ifelse(base_year == "2017-2018",0,-1),
           date_aligned = date + years(year_lag))
  
  g_residuals = ggplot(data_df_long, aes(x = date_aligned, y = value, col = variable))
  g_residuals = g_residuals+
    geom_line()+
    xlab("date")+ylab("")+
    scale_color_discrete(name = "")+
    facet_grid(base_year + type ~ . , scale= "free")+
    theme(legend.position = "bottom")
  return(g_residuals)
}
