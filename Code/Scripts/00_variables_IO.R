

IO = list()

# PUBLIC INPUTS AND OUTPUTS

IO$p_data = "../Data/"

# inputs
# IO$p_inputs = paste0(IO$p_data, "inputs/")
IO$birth_records_dir = "../Data/1_births_raw/"


# outputs
IO$p_outputs = "../Data/5_outputs/"
if(!dir.exists(IO$p_outputs)){dir.create(IO$p_outputs)}


# panels (figures)
IO$panels = "../Figures Tables Media/Figures/panels/"



# RESTRICTED INPUTS AND OUTPUTS
source("../../Seasonality-Restricted-Access-Repo/Scripts/00_variables_restricted_IO.R") # sets IO$r_data

# Clue data
IO$clue_data = paste0(IO$r_Data,"Clue_US_BR_EU/")
IO$input_clue = paste0(IO$clue_data,"input/")
IO$output_clue = paste0(IO$clue_data,"output/")
if(!dir.exists(IO$output_clue)){dir.create(IO$output_clue)}
IO$tmp_clue = paste0(IO$clue_data, "tmp/")
if(!dir.exists(IO$tmp_clue)){dir.create(IO$tmp_clue)}


