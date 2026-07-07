suppressPackageStartupMessages(library(tidyverse))
library(stringr)
library(reshape2)

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
user_cat_var <- "gender"
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
                 mutate(across(-1, ~ round(.x, 2))) |>
                 pivot_longer(cols = -1,
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

# VISUAL SUMMARIES
# bar chart for median data usage by os

os_bar_data <- phone |>
               group_by(os) |>
               select(os, data_usage) |>
               summarize(median = median(data_usage))
os_bar <- ggplot(os_bar_data, aes(x = os,
                            y = median)) +
          geom_bar(stat = "identity") +
          labs(title = "Median Data Usage by Operating System",
               subtitle = "Measured in MB/Day",
               x = "Operating System",
               y = "MB/Day") +
          theme_minimal()

os_bar

# SBS Bar chart for mean app usage time by operating system
sbs_bar_data <- phone |>
                group_by(os, get(user_cat_var)) |>
                summarize(mean = mean(app_usage)) |>
                rename_with(~ user_cat_var,
                            "get(user_cat_var)")

sbs_bar <- ggplot(sbs_bar_data, aes(x = os, y = mean, fill = get(user_cat_var))) +
           geom_bar(stat = "identity", position = position_dodge()) +
           labs(title = "Mean Daily Usage Time by Operating System",
                subtitle = "Measured in mins/day",
                x = "Operating System",
                y = "Daily Usage (mins)",
                fill = user_cat_var_title) +
           theme_minimal()

sbs_bar

# Density plot for screen time
density_plot <- ggplot(phone, aes(x = screen_on_time,
                                  group = get(user_cat_var),
                                  fill = get(user_cat_var))) +
                geom_density(alpha = 0.4) +
                labs(title = "Distribution of Screen On Time",
                     subtitle = paste0("By ", user_cat_var,
                                       ", measured in hours per day"),
                     x = "Screen On Time",
                     y = "Density",
                     fill = user_cat_var_title) +
                theme_minimal()

density_plot

# Histogram for number of apps installed
histogram <- ggplot(phone, aes(x = apps_installed, 
                               fill = get(user_cat_var))) +
             geom_histogram(bins = 10, alpha = 0.7) +
             labs(title = "Number of Apps Installed",
                  x = "Number of Apps Installed",
                  y = "Number in Dataset",
                  fill = user_cat_var_title) +
             theme_minimal()

histogram

# Scatter plot for app usage time vs. Data usage
scatter <- ggplot(phone, aes(x = app_usage,
                             y = data_usage)) +
           geom_point() + 
           labs(title = "Daily Data Usage by Daily App Usage",
                subtitle = paste("Faceted by", user_cat_var),
                x = "App Usage (Mins/day)",
                y = "Data Usage (MB/day)",) +
           theme_minimal() +
           facet_wrap(~ get(user_cat_var))

scatter

# Box plot of SOT by age:
boxplot <- ggplot(phone, aes(x = get(user_cat_var),
                             fill = get(user_cat_var),
                             y = screen_on_time)) +
           geom_boxplot() +
           labs(title = "Screen on Time",
                subtitle = paste("By", user_cat_var),
                x = user_cat_var_title,
                y = "Screen on Time (Hours/day)",
                fill = user_cat_var_title) +
           theme_minimal() +
           scale_y_continuous(breaks = seq(0, 13, by = 1))

boxplot

# correlation heatmap
corr_df <- phone |>
           select(all_of(numeric_vars)) |>
           cor() |>
           round(2) |>
           melt()

heatmap <- ggplot(corr_df, aes(x = Var1,
                               y = Var2,
                               fill = value)) +
           geom_tile() +
           geom_text(aes(Var2, Var1, label = value), color = "white", size = 8) +
           labs(title = "Correlation Between Numeric Variables",
                x = NULL,
                y = NULL,
                fill = "Correlation")

heatmap



