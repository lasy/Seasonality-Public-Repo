

IO = list()

# PUBLIC INPUTS AND OUTPUTS

IO$p_data = "../Data/"

# inputs
IO$p_inputs = paste0(IO$p_data, "inputs/")
IO$birth_records_dir = paste0(IO$p_input,"public_birth_records/")

# outputs
IO$p_outputs = paste0(IO$p_data, "outputs/")
IO$out_Rdata = paste0(IO$p_outputs, "Rdata/")
IO$out_csv = paste0(IO$p_outputs, "CSV/")
if(!dir.exists(IO$p_outputs)){dir.create(IO$public_output_data)}
if(!dir.exists(IO$out_Rdata)){dir.create(IO$out_Rdata)}
if(!dir.exists(IO$out_csv)){dir.create(IO$out_csv)}

# panels (figures)
IO$panels = "../Figures Tables Media/Figures/panels/"



# RESTRICTED INPUTS AND OUTPUTS
source("../../Seasonality-Restricted-Access-Repo/Scripts/00_variables_restricted_IO.R") # sets IO$r_data

# Clue data
IO$clue_data = paste0(IO$r_Data,"Clue_large_subset_US_BR_EU/")
IO$input_clue = paste0(IO$clue_data,"input/")
IO$output_clue = paste0(IO$clue_data,"output/")
if(!dir.exists(IO$output_clue)){dir.create(IO$output_clue)}
IO$tmp_clue = paste0(IO$clue_data, "tmp/")
if(!dir.exists(IO$tmp_clue)){dir.create(IO$tmp_clue)}

# Kindara data
IO$kindara_data = paste0(IO$r_Data,"Kindara_subset/")
IO$input_kindara = paste0(IO$kindara_data,"input/")
IO$output_kindara = paste0(IO$kindara_data,"output/")
IO$tmp_kindara = paste0(IO$kindara_data, "tmp/")


