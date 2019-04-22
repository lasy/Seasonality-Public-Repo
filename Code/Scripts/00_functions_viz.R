

ggplot_user_tracking = function(t){
  
  t = t[which(t$category != "weight"),]
  t$group = feature_dict$group[match(t$type, feature_dict$type)]
  t$group = factor(t$group, levels = levels(feature_dict$group))
  t$category = factor(t$category, levels = rev(levels(feature_dict$category)))
  t$type = factor(t$type, levels = levels(feature_dict$type))
  
  colors = as.character(feature_dict$color[which(feature_dict$type %in% unique(t$type))])
  #colors = as.character(feature_dict$color)
  
  g = ggplot(t, aes(x = date, y = category, col = type)) + 
    geom_vline(xintercept = t$date[t$cycleday == 1], col = "gray90")+
    geom_point(shape = '|')  + 
    facet_grid(group ~ . , scale = "free_y", space = "free_y") +
    scale_color_manual(values = colors)+
    guides(col = FALSE)+ 
    theme(
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(angle = 0))
  
  return(g)
  
}

