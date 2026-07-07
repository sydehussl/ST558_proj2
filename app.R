library(shiny)
suppressPackageStartupMessages(library(tidyverse))
library(forcats)
library(shinyalert)
library(bslib)

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
  mutate(age, age = fct_relevel(age, "18-29", "30-45", "45+"))

numeric_vars <- c("Daily App Usage" = "app_usage",
                  "Screen On Time (hours/day)" = "screen_on_time",
                  "Battery Drain (mAh/day)" = "battery_drain",
                  "Apps Installed" = "apps_installed")

ui <- fluidPage(
  theme = bs_theme(),
  titlePanel("Phone Usage Data Explorer"),
  
  fluidRow(
    # SIDEBAR
    column(
      width = 3,
      wellPanel(
        h4("Filtering Data"),
        
        # user age checkboxes
        checkboxGroupInput("age_subset",
                           label = "User Age Range",
                           choices = levels(phone$age),
                           selected = levels(phone$age)),
        
        # user gender checkboxes
        checkboxGroupInput("gender_subset",
                           label = "User Gender",
                           choices = levels(phone$gender),
                           selected = levels(phone$gender)),
        
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
      )
    ),
    
    # main panel
    column(
      width = 9,
      navset_tab(
        id = "main_tabs",
        selected = "Raw Data",
        
        tabPanel("Raw Data",
          DT::DTOutput("phone_table")
        ),
        
        tabPanel("Data Exploration",
          h4("Tab 2 content")
        ),
        
        tabPanel("About App",
          HTML('<h2>This app allows users to browse a dataset of synthetic phone usage behavior.</h2>
                <p>
                  The dataset is from
                  <a href="https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data" rel="noopener noreferrer">Kaggle</a>
                  and contains usage data like <strong>battery drain</strong>, <strong>screen on time</strong>, <strong>apps installed</strong>, and more.
                  The data also includes <strong>simulated demographic information</strong> to further subset analysis.
                </p>
                
                <p>
                  <strong>Important:</strong> It is <strong>NOT REAL</strong> and is purely a <strong>toy dataset</strong> I chose for this application.
                </p>
                
                <p>
                  You can view and download the raw/subsetted data via the <strong>"Raw Data"</strong> tab.
                  There are pre-made graphs and numerical summaries available in the <strong>"Data Exploration"</strong> tab, which can be customized further.
                </p>
                
                <p>
                  I rushed to get this done, so it’s not my best work.
                  I <strong>used an LLM</strong> (GPT-5.4 Nano) to search the <em>Shiny</em>/<em>bslib</em> documentation, which should explain why there’s a
                  hodgepodge of bslib and stock Shiny UI elements.
                  <strong>The app design and logic are all my own.</strong>
                </p>
                
                <p>
                  My planning document (with version history) is linked
                  <a href="https://docs.google.com/document/d/1kiX-CtG-5Y56IHK-flUB6jmYweiX84sbeAxlZh63VDg/edit?usp=sharing" rel="noopener noreferrer">here</a>.
                </p>
                
                <div>
                  <img src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fthumbs.dreamstime.com%2Fb%2Fgroup-young-people-using-mobile-phone-together-city-street-millennial-student-friends-enjoying-social-media-content-314139406.jpg&f=1&nofb=1&ipt=ee70e0f9b8f391cda07793e564edc871a2512f7d3f181f28639f04ca6d852db4" alt="App screenshot" />
                </div>'
               ),
        )
      )
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
  
  # filtered data frame reactive val
  phone_filter <- eventReactive(input$filter_button, {
    filtered <- phone |>
                  filter(age %in% input$age_subset, gender %in% input$gender_subset)
    
    if(input$filter_var_1 != "") {
      var_1 <- sym(input$filter_var_1)
      
      filtered <- filtered |>
                  filter(!!var_1 > input$range_var_1[1], 
                         !!var_1 < input$range_var_1[2])
    }
    
    if(input$filter_var_2 != "") {
      var_2 <- sym(input$filter_var_2)
      
      filtered <- filtered |>
        filter(!!var_2 > input$range_var_2[1], 
               !!var_2 < input$range_var_2[2])
    }
    
    filtered
  }, ignoreNULL = FALSE)
  
  # filtered data table obj
  output$phone_table <- DT::renderDataTable({
    if(is.null(input$filter_button)) {
      phone
    } else {
      phone_filter()
    }
  })
  
}

shinyApp(ui = ui, server = server)
