

IO = list()

IO$public_output_data = "../Data/"
IO$out_Rdata = paste0(IO$public_output_data, "Rdata/")
IO$out_csv = paste0(IO$public_output_data, "CSV/")

if(!dir.exists(IO$public_output_data)){dir.create(IO$public_output_data)}
if(!dir.exists(IO$out_Rdata)){dir.create(IO$out_Rdata)}
if(!dir.exists(IO$out_csv)){dir.create(IO$out_csv)}

source("../../Seasonality-Restricted-Access-Repo/Scripts/00_variables_restricted_IO.R")


IO$panels = "../Figures Tables Media/Figures/panels/"

IO$input_clue = paste0(IO$input_data,"Clue_20180119_subset/")
IO$output_clue = paste0(IO$output_data,"Clue/")
IO$tmp_clue = paste0(IO$tmp_data, "Clue/")

IO$input_kindara = paste0(IO$input_data,"Kindara/")
IO$output_kindara = paste0(IO$output_data,"Kindara/")
IO$tmp_kindara = paste0(IO$tmp_data, "Kindara/")


IO$public_birth_records = paste0(IO$public_output_data,"public_birth_records/")