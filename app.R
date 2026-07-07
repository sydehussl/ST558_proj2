library(shiny)
suppressPackageStartupMessages(library(tidyverse))
library(forcats)
library(shinyalert)

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
  mutate(across(c(os, gender, age), factor)) |>
  mutate(age, fct_relevel(age, "18-29", "30-45", "45+"))

numeric_vars <- c("Daily App Usage" = "app_usage",
                  "Screen On Time (hours/day)" = "screen_on_time",
                  "Battery Drain (mAh/day)" = "battery_drain",
                  "Apps Installed" = "apps_installed")


ui <- fluidPage(
  titlePanel("Phone Usage Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # user age radio button
      radioButtons("age_subset",
                   label = "User Age Range",
                   choices = c(levels(phone$age), "All"),
                   selected = "All"),
      
      # user gender radio button
      radioButtons("gender_subset",
                   label = "User Gender",
                   choices = c(levels(phone$gender), "All"),
                   selected = "All"),
      
      # dropdown for numeric variable 1
      selectInput("filter_var_1",
                  label = "Filter by numeric variable",
                  choices = c("Select a variable" = "", numeric_vars)),
      
      # slider for filter var 1
      conditionalPanel('input.filter_var_1 !== ""',
        uiOutput("range_var_1_ui")
      ),
      
      # dropdown for numeric variable 2
      selectInput("filter_var_2",
                  label = "Filter by numeric variable",
                  choices = c("Select a variable" = "", numeric_vars)),
      
      # slider for filter var 2
      conditionalPanel('input.filter_var_2 !== ""',
                       uiOutput("range_var_2_ui")
      ),
      
      # action button for subsetting data
      actionButton("filter_button",
                   "Filter Data!")
    ),
    
    mainPanel(
      
    )
  )
)

server <- function(input, output, session) {
  # figure out limits for filtering slider 1
  output$range_var_1_ui <- renderUI({
    req(input$filter_var_1 != "")
    
    sliderInput("range_var_1",
                "Select value range",
                min = min(phone[input$filter_var_1]),
                max = max(phone[input$filter_var_1]),
                value = c(min(phone[input$filter_var_1]),
                          max(phone[input$filter_var_1])))
  })
  
  # figure out limits for filtering slider 2
  output$range_var_2_ui <- renderUI({
    req(input$filter_var_2 != "")
    
    sliderInput("range_var_2",
                "Select value range",
                min = min(phone[input$filter_var_2]),
                max = max(phone[input$filter_var_2]),
                value = c(min(phone[input$filter_var_2]),
                          max(phone[input$filter_var_2])))
  })
  
  # ensure user doesn't select the same numeric variables
  observeEvent(list(input$filter_var_1, input$filter_var_2), {
    req(input$filter_var_1, input$filter_var_2)
    
    if(input$filter_var_1 == input$filter_var_2) {
      
      shinyalert(title = "Select two different filtering variables!")
      updateSelectInput(session, "filter_var_2", selected = "")
    }
  })
  
}

shinyApp(ui = ui, server = server)
