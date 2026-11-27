library(tidyverse)

# uses data_raw df from quant_analysis.R

# participant type
data_raw |> 
  select(contains("Firstly, which of the following are you (or the person you are completing this on behalf of)?")) |> 
  mutate(id = 1:nrow(data_raw)) |> 
  pivot_longer(cols = !id,
               names_to = "name",
               values_to = "value") |>
  mutate(value_grouped = case_when(is.na(value) ~ NA,
                                   grepl("Other", name) ~ "Other",
                                   .default = value)) |> 
  select(-name) |> 
  filter(!is.na(value_grouped)) |> 
  group_by(value_grouped) |> 
  count() |> 
  ungroup() |> 
  mutate(pct = n/nrow(data_raw)*100) |> 
  arrange(pct) |> 
  mutate(value_grouped = fct_inorder(value_grouped)) |> 
  ggplot(aes(x = pct,
             y = value_grouped)) +
  geom_col(fill = unname(bcc_cols("purple"))) +
  theme_minimal() +
  xlab("% of respondents") +
  ylab("") +
  ggtitle("Firstly, which of the following are you (or the person\nyou are completing this on behalf of)?") +
  theme_bcc(base_size = 12)
  
# age
data_raw |> 
  select(contains("Q1.")) |> 
  rename(age = `Q1. What is your age group? - Age`) |> 
  mutate(age = case_when(age == "Prefer not to say" ~ "Prefer not to say / Not Answered",
                         is.na(age) ~ "Prefer not to say / Not Answered",
                         .default = age)) |> 
  count(age) |> 
  mutate(pct = n/nrow(data_raw)*100) |> 
  arrange(desc(age)) |> 
  mutate(age = fct_inorder(age)) |> 
  ggplot(aes(x = pct,
             y = age)) +
  geom_col(fill = unname(bcc_cols("purple"))) +
  theme_minimal() +
  xlab("% of respondents") +
  ylab("") +
  ggtitle("What is your age group?") +
  theme_bcc(base_size = 12)

# sex
data_raw |> 
  select(contains("Q2.")) |> 
  rename(sex = `Q2. What is your sex? - Sex`) |> 
  select(sex) |> 
  mutate(sex = case_when(sex == "Prefer not to say" ~ "Prefer not to say / Not Answered",
                         sex == "Not Answered" ~ "Prefer not to say / Not Answered",
                         is.na(sex) ~ "Prefer not to say / Not Answered",
                         .default = sex)) |> 
  count(sex) |> 
  mutate(pct = n/nrow(data_raw)*100) |> 
  arrange(pct) |> 
  mutate(sex = fct_inorder(sex)) |> 
  ggplot(aes(x = pct,
             y = sex)) +
  geom_col(fill = unname(bcc_cols("purple"))) +
  theme_minimal() +
  xlab("% of respondents") +
  ylab("") +
  ggtitle("What is your sex?") +
  theme_bcc(base_size = 12)

# gender
data_raw |> 
  select(contains("Q3.")) |> 
  rename(gender = `Q3. What is your gender? - Gender`) |> 
  select(gender) |> 
  mutate(gender = case_when(gender == "Prefer not to say" ~ "Prefer not to say / Not Answered",
                         gender == "Not Answered" ~ "Prefer not to say / Not Answered",
                         is.na(gender) ~ "Prefer not to say / Not Answered",
                         .default = gender)) |> 
  count(gender) |> 
  mutate(pct = n/nrow(data_raw)*100) |> 
  arrange(pct) |> 
  mutate(gender = fct_inorder(gender)) |> 
  ggplot(aes(x = pct,
             y = gender)) +
  geom_col(fill = unname(bcc_cols("purple"))) +
  theme_minimal() +
  xlab("% of respondents") +
  ylab("") +
  ggtitle("What is your gender?") +
  theme_bcc(base_size = 12)

# sexual orientation
data_raw |> 
  select(contains("Q4.")) |> 
  rename(orientation = `Q4. Which of the following best describes your sexual orientation?  - Sexual Orientation`) |> 
  select(orientation) |> 
  mutate(orientation = case_when(orientation == "Prefer not to say" ~ "Prefer not to say / Not Answered",
                            orientation == "Not Answered" ~ "Prefer not to say / Not Answered",
                            is.na(orientation) ~ "Prefer not to say / Not Answered",
                            .default = orientation)) |> 
  count(orientation) |> 
  mutate(pct = n/nrow(data_raw)*100) |> 
  arrange(pct) |> 
  mutate(orientation = fct_inorder(orientation)) |> 
  ggplot(aes(x = pct,
             y = orientation)) +
  geom_col(fill = unname(bcc_cols("purple"))) +
  theme_minimal() +
  xlab("% of respondents") +
  ylab("") +
  ggtitle("Which of the following best describes your sexual orientation?") +
  theme_bcc(base_size = 12)

# religion
data_raw |> 
  select(contains("Q5.")) |> 
  rename(religion = `Q5. What is your religion or belief? - Religion`) |> 
  select(religion) |> 
  mutate(religion = case_when(religion == "Prefer not to say" ~ "Prefer not to say / Not Answered",
                                 religion == "Not Answered" ~ "Prefer not to say / Not Answered",
                                 is.na(religion) ~ "Prefer not to say / Not Answered",
                                 .default = religion)) |> 
  count(religion) |> 
  mutate(pct = n/nrow(data_raw)*100) |> 
  arrange(pct) |> 
  mutate(religion = fct_inorder(religion)) |> 
  ggplot(aes(x = pct,
             y = religion)) +
  geom_col(fill = unname(bcc_cols("purple"))) +
  theme_minimal() +
  xlab("% of respondents") +
  ylab("") +
  ggtitle("What is your religion or belief?") +
  theme_bcc(base_size = 12)
