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


par = list()
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


feature_dict = read.csv(file = paste0(IO$public_output_data, "tracking_features_dictionary.csv"))
feature_dict$group = factor(feature_dict$group, levels = unique(feature_dict$group))
feature_dict$category = factor(feature_dict$category, levels = unique(feature_dict$category))
feature_dict$type = factor(feature_dict$type, levels = unique(feature_dict$type))










