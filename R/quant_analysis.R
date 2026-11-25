library(tidyverse)
library(janitor)
library(readxl)
library(ggstats)
library(bcctheme)

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
# define allowed levels
# this will be applied as a factor, leaving "Not answered" and "Don't know" as NA so effectively filtering them out
likert_levels <- c("Strongly disagree",
                   "Disagree",
                   "Neither agree nor disagree",
                   "Agree",
                   "Strongly agree")

# set up diverging colour palette using bcctheme functions
bcc_pal_purple <- bcc_pal(palette = "purple")(3)
bcc_pal_green <- bcc_pal(palette = "green")(3)

likert_pal <- c(bcc_pal_purple[1:2], "#F7F7F7", bcc_pal_green[2:1])

# Plot likert-based questions
data_quant |> 
  select(matches("eo[0-9]+")) |> 
  mutate(across(everything(), ~ factor(.x, levels = likert_levels))) |> 
  gglikert(variable_labels = c(
    eo1_reflect_importance = "EO1 - Putting our citizens first",
    eo2_reflect_importance = "EO2 - Inclusive & culturally competent leadership, partnership and organisational commitment",
    eo3_reflect_importance = "EO3 - Responsive services and citizen care",
    eo4_reflect_importance = "EO4 - Developing an inclusive & diverse workplace"
  )) +
  # scale_fill_brewer(palette = "PRGn",
  #                   direction = 1) +
  scale_fill_manual(values = likert_pal)
  ggtitle("To what extent do you agree or disagree that this equality objective reflects\nthe most important equality issues facing Birmingham today?") +
  theme_bcc(base_size = 12,
            gridline_y = F,
            legend_position = "bottom") +
  theme(panel.grid.minor.x = element_line(linetype = "solid",
                                          linewidth = 0.15,
                                          color = "#999999"))

# Other quant qs ----------------------------------------------------------

# To what extent do you think the Birmingham's current equality objectives focus on the right issues?

# summarise, factorise and plot as graph
data_quant |> 
  group_by(right_objectives) |> 
  count() |> 
  ungroup() |> 
  mutate(total = sum(n),
         pct = n/total*100,
         right_objectives = factor(right_objectives,
                                   levels = c("Not Answered",
                                              "Don't know / Not sure",
                                              "No, they do not cover the right issues",
                                              "Yes, but some important issues are missing",
                                              "Yes, they cover the right issues"))) |> 
  ggplot(aes(x = pct,
             y = right_objectives)) +
  geom_col(fill = unname(bcc_cols("purple"))) +
  theme_minimal() +
  xlab("% of respondents") +
  ylab("") +
  ggtitle("To what extent do you think that Birmingham's current equality objectives\nfocus on the right issues?") +
  theme_bcc(base_size = 12)
  
# Are there any people, communities or issues that are not reflected enough in the current equality objectives?
  
# summarise, factorise and plot as graph
data_quant |> 
  group_by(missing_people_communities_issues) |> 
  count() |> 
  ungroup() |> 
  mutate(total = sum(n),
         pct = n/total*100,
         missing_people_communities_issues = factor(missing_people_communities_issues,
                                   levels = c("Not Answered",
                                              "Not sure",
                                              "No",
                                              "Yes"))) |> 
  ggplot(aes(x = pct,
             y = missing_people_communities_issues)) +
  geom_col(fill = unname(bcc_cols("purple"))) +
  theme_minimal() +
  xlab("% of respondents") +
  ylab("") +
  ggtitle("Are there any people, communities or issues that are not reflected enough\nin the current equality objectives?") +
  theme_bcc(base_size = 12)


# Are there any other equality priorities or objectives you think Birmingham City Council should focus on over the next four years?

# summarise, factorise and plot as graph
data_quant |> 
  group_by(any_other_priorities) |> 
  count() |> 
  ungroup() |> 
  mutate(total = sum(n),
         pct = n/total*100,
         any_other_priorities = factor(any_other_priorities,
                                                    levels = c("Not Answered",
                                                               "Not sure",
                                                               "No",
                                                               "Yes"))) |> 
  ggplot(aes(x = pct,
             y = any_other_priorities)) +
  geom_col(fill = unname(bcc_cols("purple"))) +
  theme_minimal() +
  xlab("% of respondents") +
  ylab("") +
  ggtitle("Are there any other equality priorities or objectives you think\nBirmingham City Council should focus on over the next four years?") +
  theme_bcc(base_size = 12)
