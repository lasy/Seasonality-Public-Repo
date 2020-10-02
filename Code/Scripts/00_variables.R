par = list()
par$IO_user = Sys.getenv("LOGNAME")


source("Scripts/00_variables_IO.R")


# BIRTH MODELS VARIABLES
G0 = 38*7
Gsd0 = 10

# BREAKS -----
breaks = list()
breaks$age = seq(10,45, by = 5)
breaks$height = seq(100,240,by = 5)
breaks$bmi = c(0,seq(15,30,by = 3), Inf)


# VIZ variables

viz = list()
viz$scale = 1.1
viz$full_width = 12


# GLOBAL PARAMETERS
par$n_cores = detectCores() - 1
par$selected_countries = c("United States", "Brazil","United Kingdom","France")
par$min_n_batches = 5
par$n_users = 10

par$start_date = as.Date("2015-01-01")
par$end_date = as.Date("2017-12-31")
par$date_seq = seq(par$start_date , par$end_date, by = 1)
par$n_years = year(par$end_date) - year(par$start_date) + 1
par$dt = par$n_years / length(par$date_seq)
par$time_num = year(par$start_date) + c(0:(length(par$date_seq)-1))/length(par$date_seq)*par$n_years

par$agg_col = c("country","age_cat","bmi_cat")

par$MF = list()
par$MF$phase_date = as.Date(paste0(year(par$start_date),"-01-01")) 
par$MF$phase_num = par$time_num[par$date_seq == par$MF$phase_date] 
par$MF$rel_ampl = 0.2


# CLUE FEATURES ------
feature_dict = read_csv(file = paste0(IO$p_inputs,"clue_tracking_features_dictionary/tracking_features_dictionary.csv"))
feature_dict$group = factor(feature_dict$group, levels = unique(feature_dict$group))
feature_dict$category = factor(feature_dict$category, levels = unique(feature_dict$category))
feature_dict$type = factor(feature_dict$type, levels = unique(feature_dict$type))



# DICTIONARIES =========

# demographics ----------

dict = list()
dict$height = data.frame(bin = c("<155","155-159","160-164","165-169","170-174",">=175" ),
                         min = c(120,155,160,165,170,175),
                         max = c(154,159,164,169,174,200),
                         mean = c(150,157,162,167,172,180),
                         stringsAsFactors = FALSE)

dict$weight = data.frame(bin = c("<50","50-54","55-59","60-64","65-69","70-74","75-79",">=80"),
                         min = c(35,seq(50,80,by = 5)),
                         max = c(seq(49,79,by = 5),120),
                         mean = c(45,seq(52,77,by = 5),90),
                         stringsAsFactors = FALSE)

cols = list()
cols$age_cat <- scales::seq_gradient_pal("seagreen1", "steelblue4")(seq(0,1,length.out=5))


dict$BC_all = data.frame(birth_control = c("none","fertility_awareness_method", "condoms","pill","vaginal_ring","IUD","patch","injection","implant","vaginal_ring","other","undefined"),
                     color = c("cyan","green3","yellow3","deeppink","pink","black","blue","steelblue","orange","orange_red","gray", "gray80"),
                     type = c(rep("F",3), rep("I",7),rep("?",2)),
                     stringsAsFactors = FALSE)
dict$BC_all$type_descr = c("Potentially fertile, and thus fecundable with unprotected sex","Infertile due to contaceptive")[match(dict$BC_all$type, c("F","I"))]

dict$BC = unique(dict$BC_all %>% select(type,type_descr)) %>% 
  bind_rows(., data.frame(type = "all",type_descr = "F+I", stringsAsFactors = FALSE)) %>% 
  mutate(BC_col = c("lightsalmon1","lightseagreen","gray90","gray30")) %>% 
  rename(BC = type, BC_descr = type_descr)

dict$sex_type = data.frame(
  sex_type = c("all_sex","unprot_sex","prot_sex"),
  sex_type_col = c("mediumpurple3","pink1","lightskyblue2"),
  stringsAsFactors = FALSE
)

# countries ------------

brazil1_col = hsv(h = 0.4,s = 0.88, v = 0.80)
brazil2_col = hsv(h = 0.4,s = 0.89, v = 0.55)
US1_col = hsv(h = 0.98,s = 0.70, v = 0.95)
US2_col = hsv(h = 0.98,s = 0.80, v = 0.70)
France_col = hsv(h = 0.66,s = 0.57, v = 0.92)
UK_col = hsv(h = 0.1,s = 0.16, v = 0.65)

dict$country_area = data.frame(
  country_area = c("Brazil - Central-West","Brazil - Northeast","United States - California","United States - Northeast","France","United Kingdom"),
  country = c("Brazil","Brazil","United State","United State","France","United Kingdom"),
  area = c("Central-West","Northeast","California","Northeast","",""),
  country_area_col = c(brazil1_col,brazil2_col,
                       US1_col,US2_col,
                       France_col,
                       UK_col),
  country_col = c(rep(brazil1_col,2),rep(US1_col,2), France_col, UK_col),
  stringsAsFactors = FALSE
) %>% 
  mutate(country_area = factor(country_area, levels = country_area))



get_age_cat = function(country_area = ca){
  "all"
  # case_when(
  #   country_area == "Brazil - Central-West"      ~ "all",
  #   country_area == "Brazil - Northeast"         ~ "<= 23",
  #   country_area == "United States - California" ~ "all",
  #   country_area == "United States - Northeast"  ~ "all",
  #   country_area == "France"                     ~ "> 23",
  #   country_area == "United Kingdom"             ~ "all"
  # )  
}



# weekdays ------------
dict$weekdays = data.frame(abbr = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                           color = c(rep("gray",5),rep("gray40",2)),
                           stringsAsFactors = FALSE)


# holidays ------------

holidays = read_csv(file = paste0(IO$p_inputs,"public_holidays/public_holidays.csv"),
                    col_types = cols(
                      country = col_character(),
                      holiday_name = col_character()
                    ))

dict$holidays = holidays %>%  dplyr::select(country, holiday_name) %>%  unique()
rm(holidays)

# Holiday colors
dict$holidays$color = "black"
dict$holidays$color[dict$holidays$holiday_name == "New Year"] = "indianred2"
dict$holidays$color[dict$holidays$holiday_name == "Valentine's day"] = "deeppink"
dict$holidays$color[dict$holidays$holiday_name == "Brazilian Valentine's day"] = "deeppink"
dict$holidays$color[dict$holidays$holiday_name == "Good Friday"] = "darkolivegreen3"
dict$holidays$color[dict$holidays$holiday_name == "Easter Monday"] = "darkolivegreen4"
dict$holidays$color[dict$holidays$holiday_name == "Ascension"] = "turquoise2"
dict$holidays$color[dict$holidays$holiday_name == "Whit Monday"] = "violet"

dict$holidays$color[dict$holidays$holiday_name == "Labor day"] = "seagreen2"
dict$holidays$color[dict$holidays$holiday_name %in% c("National day", "Independence day","Bastille day")] = "blue3"

dict$holidays$color[dict$holidays$holiday_name == "Early May Bank Holiday"] = "springgreen2"
dict$holidays$color[dict$holidays$holiday_name == "Spring Bank Holiday"] = "springgreen3"
dict$holidays$color[dict$holidays$holiday_name == "Summer Bank Holiday"] = "springgreen4"


dict$holidays$color[dict$holidays$holiday_name == "Memorial day"] = "slateblue1"
dict$holidays$color[dict$holidays$holiday_name == "War victory"] = "steelblue1"

dict$holidays$color[dict$holidays$holiday_name == "Carnival"] = "limegreen"
dict$holidays$color[dict$holidays$holiday_name == "Proclamation of the Republic"] = "blue2"
dict$holidays$color[dict$holidays$holiday_name == "Nossa Senhora de Aparecida"] = "goldenrod1"
dict$holidays$color[dict$holidays$holiday_name == "Corpus Christi"] = "plum2"

dict$holidays$color[dict$holidays$holiday_name %in% c("All saints","Toussaint")] = "gray30"
dict$holidays$color[dict$holidays$holiday_name == "Armistice"] = "lavenderblush3"
dict$holidays$color[dict$holidays$holiday_name == "Thanksgiving"] = "lightsalmon3"


dict$holidays$color[dict$holidays$holiday_name == "Christmas"] = "firebrick1"
dict$holidays$color[dict$holidays$holiday_name == "Boxing Day"] = "firebrick3"

# Holiday name wrapped (for axes of reasonable lengths)
dict$holidays$holiday_name_wrapped = dict$holidays$holiday_name
dict$holidays$holiday_name_wrapped[dict$holidays$holiday_name == "Brazilian Valentine's day"] = "Brazilian\nValentine's day"
dict$holidays$holiday_name_wrapped[dict$holidays$holiday_name == "Nossa Senhora de Aparecida"] = "Nossa Senhora\nde Aparecida"
dict$holidays$holiday_name_wrapped[dict$holidays$holiday_name == "Proclamation of the Republic"] = "Proclamation of\nthe Republic"
dict$holidays$holiday_name_wrapped[dict$holidays$holiday_name == "Early May Bank Holiday"] = "Early May\nBank Holiday"
dict$holidays$holiday_name_wrapped[dict$holidays$holiday_name == "Spring Bank Holiday"] = "Spring\nBank Holiday"
dict$holidays$holiday_name_wrapped[dict$holidays$holiday_name == "Summer Bank Holiday"] = "Summer\nBank Holiday"



# Context-dependent holidays (Additive holidays) 

# Because sexual activity is structured by both day-of-week and holidays, it is important to define how the contribution of holidays is modeled in comparison to the day-of-week and seasonal effects.
# For some holidays or celebration like Valentine's day, the contribution of a holiday on sexual activity should be modeled as additive to the contribution of the week-day (additive holidays). 
# For other holidays, like New-Year, the contribution of the week-day should be removed from the modeling of sexual activity as the New-Year appears to lead to the same level of sexual activity, regardless of the day-of-week it happens (absolute holidays).
# And finally, there are holidays, such as Thanksgiving, for which their effect could be modeled either as additive, i.e. their contribution is added to the contribution of the day-of-week, or as absolute, i.e. their contribution is added to the baseline level (the intercept).
# For consistency, all national holidays, i.e. when the population gets a day off at work/school, regardless of whether they occur on a fixed weekday or a fixed date, will be modeled as absolute, which means that their coefficient should be interpreted as the increase in sexual activity from baseline, 
# while holidays or romantic celebrations, for which the population does not get a day off, will be modeled as additive. 
# Note that padding days (days leading and following a holiday) are modeled as additive.

additive_holidays = c("Valentine's day","Brazilian Valentine's day")
dict$holidays$additive = FALSE
dict$holidays$additive[dict$holidays$holiday_name %in% additive_holidays] = TRUE



