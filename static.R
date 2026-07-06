suppressPackageStartupMessages(library(tidyverse))
library(stringr)

# import data
phone_raw <- read.csv("user_behavior_dataset.csv")

# clean + prep data
phone <- phone_raw |>
         select(,3:11) |>
         rename("os" = "Operating.System",
                "app_usage" = "App.Usage.Time..min.day.",
                "screen_on_time" = "Screen.On.Time..hours.day.",
                "battery_drain" = "Battery.Drain..mAh.day.",
                "apps_installed" = "Number.of.Apps.Installed",
                "data_usage" = "Data.Usage..MB.day.",
                "age" = "Age",
                "gender" = "Gender",
                "behavior_class" = "User.Behavior.Class") |>
         mutate(age = ifelse(age <= 29, "18-29", 
                             ifelse(age <= 45, "30-45", "45+"))) |>
         mutate(across(c(os, age, gender), factor))

# NUMERICAL SUMMARIES
# using string name of column to prep for implementing in shiny
user_cat_var <- "age"
user_cat_var_title <- str_to_title(user_cat_var)

# Device type contingency table
table(phone$os)

# Device type by grouping variable
table(phone$os, phone[,user_cat_var])

# numeric variable summary table:
numeric_vars <- c("app_usage", "screen_on_time", "battery_drain", "apps_installed")

# making basic table
basic_summary <- phone |>
                 group_by(get(user_cat_var)) |>
                 summarize(across(all_of(numeric_vars), .fns = list("mean" = mean,
                                                                  "med" = median,
                                                                  "sd" = sd)))

# pivoting to make readable
summary_pivot <- basic_summary |>
                 pivot_longer(cols = 2:ncol(summary_tab),
                              names_to = c("num_var", ".value"),
                              names_pattern = "^(.*)_(mean|med|sd)$",
                              values_to = "value") |>
                 mutate(num_var = str_to_title(str_replace_all(num_var, pattern = "_", 
                                                             replacement = " "))) |>
                 rename("Variable" = "num_var",
                        "Mean" = "mean",
                        "Median" = "med",
                        "Std. Dev." = "sd") |>
                 rename_with(~ user_cat_var_title,
                             "get(user_cat_var)")

# final summary table code
summary_table <- phone |>
                 group_by(get(user_cat_var)) |>
                 summarize(across(all_of(numeric_vars), .fns = list("mean" = mean,
                                                                    "med" = median,
                                                                    "sd" = sd))) |>
                 pivot_longer(cols = 2:ncol(summary_tab),
                              names_to = c("num_var", ".value"),
                              names_pattern = "^(.*)_(mean|med|sd)$",
                              values_to = "value") |>
                 mutate(num_var = str_to_title(str_replace_all(num_var, pattern = "_", 
                                                                replacement = " "))) |>
                 rename("Variable" = "num_var",
                        "Mean" = "mean",
                        "Median" = "med",
                        "Std. Dev." = "sd") |>
                 rename_with(~ user_cat_var_title,
                             "get(user_cat_var)")


