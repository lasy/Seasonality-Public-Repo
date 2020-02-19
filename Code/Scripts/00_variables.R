par = list()
par$IO_user = Sys.getenv("LOGNAME")


source("Scripts/00_variables_IO.R")


breaks = list()
breaks$age = seq(10,45, by = 5)
breaks$height = seq(100,240,by = 5)
breaks$bmi = c(0,seq(15,30,by = 3), Inf)


# VIZ variables

cols = list()
cols$age_cat <- scales::seq_gradient_pal("seagreen1", "steelblue4")(seq(0,1,length.out=5))

viz = list()
viz$scale = 1.1
viz$full_width = 12


par$n_cores = detectCores() - 1
par$selected_countries = c("United States", "Brazil","Mexico","United Kingdom","Germany","France")
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


feature_dict = read_csv(file = paste0(IO$p_inputs,"clue_tracking_features_dictionary/tracking_features_dictionary.csv"))
feature_dict$group = factor(feature_dict$group, levels = unique(feature_dict$group))
feature_dict$category = factor(feature_dict$category, levels = unique(feature_dict$category))
feature_dict$type = factor(feature_dict$type, levels = unique(feature_dict$type))


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

dict$BC = data.frame(birth_control = c("none","fertility_awareness_method", "condoms","pill","vaginal_ring","IUD","patch","injection","implant","vaginal_ring","other","undefined"),
                     color = c("cyan","green3","yellow3","deeppink","pink","black","blue","steelblue","orange","orange_red","gray", "gray80"),
                     type = c(rep("F",3), rep("I",7),rep("?",2)),
                     stringsAsFactors = FALSE)
dict$BC$type_descr = c("Potentially fertile, and thus fecundable with unprotected sex","infertility due to contaceptive")[match(dict$BC$type, c("F","I"))]



holidays = read_csv(file = paste0(IO$p_inputs,"public_holidays/public_holidays.csv"),
                    col_types = cols(
                      country = col_character(),
                      date = col_date(format = "%m/%d/%y"),
                      holiday_name = col_character()
                    ))
# year(holidays$date) = 2016
# holidays_2017 = holidays
# year(holidays_2017$date) = 2017
# holidays_2018 = holidays
# year(holidays_2018$date) = 2018
# holidays_2019 = holidays
# year(holidays_2019$date) = 2019
# holidays = rbind(holidays, holidays_2017, holidays_2018, holidays_2019)
# rm(holidays_2017, holidays_2018, holidays_2019)
holidays$date_str = holidays$date %>% month(label = TRUE, abbr = TRUE) %>% str_c(., " ", holidays$date %>%  day()) 
holidays = holidays %>% arrange(country, date)

