library(tidyverse)
library(janitor)
library(readxl)
library(ggstats)

full_data_filename <- "equality_objectives_data_full_25-11-26.xlsx"

# Data import/cleaning ----------------------------------------------------

# read in full dataset
data_raw <- read_excel(paste0("data/", full_data_filename))

# separate out quant data (not demographics - deal with these qs separately)
data_quant <- data_raw[c(1,3,5,7,9,11,14)]

# give quant data more sensible column names but retain lookup between new ("code") and old ("question") col names
data_quant_code_q_lookup <- names(data_quant) |> 
  as_tibble() |> 
  separate_wider_delim(value,
                       delim = " - ",
                       names = c("question", "code"),
                       too_many = "merge") |>
  mutate(code = str_to_lower(code),
         code = str_replace_all(code, "[[:punct:]]", ""),
         code = str_replace(code, "eo 1", "eo1"),
         code = str_replace(code, "eo 2", "eo2"),
         code = str_replace(code, "or ", ""),
         code = case_when(grepl("any other priorities", code) ~ "any other priorities",
                          .default = code))

# convert to snake case
data_quant_code_q_lookup$code <- make_clean_names(data_quant_code_q_lookup$code)

# apply to quant df
names(data_quant) <- data_quant_code_q_lookup$code


# Likert questions --------------------------------------------------------
gglikert(data_quant)

