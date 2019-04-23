

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

