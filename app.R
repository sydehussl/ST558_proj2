library(shiny)
suppressPackageStartupMessages(library(tidyverse))
library(forcats)
library(shinyalert)
library(bslib)
library(reshape2)
library(shinycssloaders)

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
  mutate(across(c(os, gender, age, behavior_class), factor)) |>
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
        h4("Filter Data"),
        
        # user os checkboxes
        checkboxGroupInput("os_subset",
                           label = "User Operating System",
                           choices = levels(phone$os),
                           selected = levels(phone$os)),
        
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
                     "Filter Data!"),
        
        # conditional panel for data explr. tab
        conditionalPanel(
          condition = "input.main_tabs == 'data_exploration'",
          h4("Select Grouping Variable"),
          
          # dropdown for grouping variable
          selectInput("grp_var",
                      label = "Grouping Variable",
                      choices = c("Age" = "age",
                                  "Gender" = "gender",
                                  "User Behavior Class" = "behavior_class")),
          
          # checkbox for summaries shown
          checkboxGroupInput("summary_checkbox",
                             "Summary Options",
                             choices = c("Show Numeric Summaries" = "num_sums",
                                         "Show Graphs" = "graphs"),
                             selected = "graphs"),
        ),
        
        # data dictionary checkbox
        conditionalPanel(
          condition = "input.main_tabs == 'raw_data'",
          checkboxInput("show_dictionary",
                        "Show Variable Dictionary")
        )
      )
    ),
    
    # main panel
    column(
      width = 9,
      navset_tab(
        id = "main_tabs",
        selected = "about",
        
        tabPanel("Raw Data",
          value = "raw_data",
          
          # render table
          shinycssloaders::withSpinner(DT::DTOutput("phone_table")),
          
          # render download button below table bc it looks better
          downloadButton("download_table",
                         "Download as .CSV"),
          
          # show data dictionary
          conditionalPanel(
            'input.show_dictionary',
            HTML("<h3> Variable Dictionary<h3>
                  <p>
                  <strong>os</strong> = Operating system (Android/iOS)<br>
                  <strong>app_usage</strong> = Minutes spent on apps (per day)<br>
                  <strong>screen_on_time</strong> = Hours spent with screen on (per day)<br>
                  <strong>battery_drain</strong> = Battery drain (mAh/day)<br>
                  <strong>apps_installed</strong> = Number of apps installed<br>
                  <strong>data_usage</strong> = Mobile data usage (MB/day)<br>
                  <strong>age</strong> = User's age range (derived)<br>
                  <strong>gender</strong> = User's gender (Male/Female)<br>
                  <strong>behavior_class</strong> = Behavior classification, used to simulate the data (1-5)<br>
                  <p>
                 ")
          )
        ),
        
        tabPanel("Data Exploration",
          value = "data_exploration",
          
          column(12,
            # numeric summary panel
            conditionalPanel(
                'input.summary_checkbox.includes("num_sums")',
                
                card(
                  card_body(
                    headerPanel("Numeric Summaries"),
                      fluidRow(
                        column(8, 
                          card(height = 600,
                            card_body(shinycssloaders::withSpinner(DT::DTOutput("sum_table"))))),
                            
                        column(4,
                          card(height = 600,
                            card_body(
                              h4("Operating System Stats"),
                              h5("Overall"),
                              card(card_body(shinycssloaders::withSpinner(tableOutput("os_distribution")))),
                              h5("By Grouping Variable"),
                              card(card_body(shinycssloaders::withSpinner(tableOutput("os_two_way"))))
                            )
                          )
                        )
                      )
                  ),
            )),
          
          # graph panel
          conditionalPanel(
            'input.summary_checkbox.includes("graphs")',
            
             card(
                card_body(
                  headerPanel("Graphs"),
                      
                    fluidRow(
                      column(4, card(card_body(shinycssloaders::withSpinner(plotOutput("data_bar"))))),
                      column(4, card(card_body(shinycssloaders::withSpinner(plotOutput("os_usage_bar"))))),
                      column(4, card(card_body(shinycssloaders::withSpinner(plotOutput("density")))))
                    ),
                    fluidRow(
                      column(6, card(card_body(shinycssloaders::withSpinner(plotOutput("histogram"))))),
                      column(6, card(card_body(shinycssloaders::withSpinner(plotOutput("scatter")))))
                    ),
                    fluidRow(
                      column(6, card(card_body(shinycssloaders::withSpinner(plotOutput("boxplot"))))),
                      
                      # show correlation heatmap if requested
                      conditionalPanel(
                        'input.summary_checkbox.includes("corrmap")',
                        column(6, card(card_body(shinycssloaders::withSpinner(plotOutput("corrmap")))))
                      )
                    )
                )
              )
          ),
        )
      ),# end of data explr tab
        
        
        tabPanel("About App",
          value = "about",
          HTML('<br/>
          
                <h2>This app allows users to browse a dataset of synthetic phone usage behavior.</h2>
                <p>
                  The dataset is from
                  <a href="https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data" rel="noopener noreferrer">Kaggle</a>
                  and contains usage data like <strong>battery drain</strong>, <strong>screen on time</strong>, <strong>apps installed</strong>, and more.
                  The data also includes <strong>simulated demographic information</strong> to condition the data on.
                </p>
                
                <p>
                  <strong>Important:</strong> It is <strong>NOT REAL</strong> and is purely a toy dataset I chose for this application.
                </p>
                
                <p>
                  You can view and download the raw/subsetted data via the <strong>"Raw Data"</strong> tab.
                  There are pre-made graphs and numerical summaries available in the <strong>"Data Exploration"</strong> tab, which can be customized further.
                  The controls in the <strong>sidebar</strong> can be used to filter the dataset.
                </p>
                
                <p>
                  I rushed to get this done, I would spend more time on UI/UX if I could have.
                  I <strong>used an LLM</strong> (GPT-5.4 Nano) to search the <em>Shiny</em>/<em>bslib</em> documentation, which should explain why there’s a
                  hodgepodge of bslib and stock Shiny UI elements.<strong>The app design and logic are all my own.</strong> <em>An LLM would have far better UX<em>
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
  
  # hide correlation heatmap checkbox if graph checkbox isn't selected
  observeEvent(input$summary_checkbox, {
    if("graphs" %in% input$summary_checkbox) {
      updateCheckboxGroupInput(session,
                               "summary_checkbox",
                               "Summary Options",
                               choices = c("Show Numeric Summaries" = "num_sums",
                                           "Show Graphs" = "graphs",
                                           "Show Correlation Heatmap" = "corrmap"),
                               selected = input$summary_checkbox)
    } else {
      updateCheckboxGroupInput(session,
                               "summary_checkbox",
                               "Summary Options",
                               choices = c("Show Numeric Summaries" = "num_sums",
                                           "Show Graphs" = "graphs"),
                               # remove corrmap from options
                               selected = setdiff(input$summary_checkbox, "corrmap"))
    }
  })
  
  # filtered data frame reactive val
  phone_filter <- eventReactive(input$filter_button, {
    # filter categorical vars
    filtered <- phone |>
                  filter(age %in% input$age_subset, 
                         gender %in% input$gender_subset,
                         os %in% input$os_subset)
    
    # filter numeric 1
    if(input$filter_var_1 != "") {
      var_1 <- sym(input$filter_var_1)
      
      filtered <- filtered |>
                  filter(!!var_1 > input$range_var_1[1], 
                         !!var_1 < input$range_var_1[2])
    }
    
    # filter numeric 2
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
    phone_filter()
  })
  
  # download button
  output$download_table <- downloadHandler(
    filename = "phone_data.csv",
    content = function(file) {
      write.csv(phone_filter(), file)
    }
  )
  
  # user categorical variable reactive value
  user_cat <- reactiveValues(
    title = NULL,
    var_sym = NULL,
    lower = NULL
  )
  
  # update user cat var value
  observeEvent(input$grp_var, {
    req(input$grp_var)
      
    user_cat$title <- str_to_title(str_replace_all(input$grp_var, pattern = "_", 
                                                   replacement = " "))
    user_cat$sym <- sym(input$grp_var)
    user_cat$lower <- str_to_lower(str_replace_all(input$grp_var, pattern = "_", 
                                                   replacement = " "))
  }, ignoreNULL = FALSE)
  
  # VISUAL SUMMARIES:
  # bar plot for data usage
  output$data_bar <- renderPlot({
    req(phone_filter())
    
    data_bar_title <- paste("Median Data Usage by",
                            user_cat$title)
    
    # not gpp approved
    data_bar_data <- phone_filter() |>
                     group_by(!!user_cat$sym) |>
                     select(!!user_cat$sym, data_usage) |>
                     summarize(median = median(data_usage))
    
    data_bar_plot <- ggplot(data_bar_data, aes(x = !!user_cat$sym, y = median,
                                               fill = !!user_cat$sym)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = data_bar_title,
           subtitle = "Measured in MB/Day",
           x = user_cat$title,
           y = "MB/Day",
           fill = user_cat$title) +
      theme_minimal()
    
    data_bar_plot
  })
  
  # sbs bar plot for app usage time
  output$os_usage_bar <- renderPlot({
    req(phone_filter())
    
    sbs_bar_data <- phone_filter() |>
                    select(os, !!user_cat$sym, app_usage) |>
                    group_by(os, !!user_cat$sym) |>
                    summarize(mean = mean(app_usage))
    
    sbs_bar_plot <- ggplot(sbs_bar_data, aes(x = os, y = mean, fill = !!user_cat$sym)) +
                    geom_bar(stat = "identity", position = position_dodge()) +
                    labs(title = "Average Daily App Usage by OS",
                         subtitle = "Measured in mins/day",
                         x = "Operating System",
                         y = "Daily Usage (mins)",
                         fill = user_cat$title) +
                    theme_minimal()
    
    sbs_bar_plot
  })
  
  # density plot for screen on time
  output$density <- renderPlot({
    req(phone_filter())
    
    density_plot <- ggplot(phone_filter(), aes(x = screen_on_time,
                                      group = !!user_cat$sym,
                                      fill = !!user_cat$sym)) +
      geom_density(alpha = 0.4) +
      labs(title = "Distribution of Screen On Time",
           subtitle = paste0("By ", user_cat$lower,
                             ", measured in hours per day"),
           x = "Screen On Time",
           y = "Density",
           fill = user_cat$title) +
      theme_minimal()
    
    density_plot
  })
  
  # histogram of number of apps installed
  output$histogram <- renderPlot({
    histogram <- ggplot(phone, aes(x = apps_installed, 
                                   fill = !!user_cat$sym)) +
      geom_histogram(bins = 10, alpha = 0.7) +
      labs(title = "Number of Apps Installed",
           x = "Number of Apps Installed",
           y = "Number in Dataset",
           fill = user_cat$title) +
      theme_minimal()
    
    histogram
  })
  
  # scatter plot for app usage vs data usage
  output$scatter <- renderPlot({
    scatter <- ggplot(phone_filter(), aes(x = app_usage,
                                          y = data_usage)) +
      geom_point() + 
      labs(title = "Daily Data Usage by Daily App Usage",
           subtitle = paste("Faceted by", user_cat$title,
                            " (synthetic data reminder)"),
           x = "App Usage (Mins/day)",
           y = "Data Usage (MB/day)",) +
      theme_minimal() +
      facet_wrap(~ get(input$grp_var))
    
    scatter
  })
  
  # box plot of SOT by age
  output$boxplot <- renderPlot({
    boxplot <- ggplot(phone_filter(), aes(x = !!user_cat$sym,
                                          fill = !!user_cat$sym,
                                          y = screen_on_time)) +
      geom_boxplot() +
      labs(title = "Screen on Time",
           subtitle = paste("By", user_cat$title),
           x = user_cat$title,
           y = "Screen on Time (Hours/day)",
           fill = user_cat$title) +
      theme_minimal() +
      scale_y_continuous(breaks = seq(0, 13, by = 1))
    
    boxplot
  })
  
  # correlation heatmap
  output$corrmap <- renderPlot({
    req(phone_filter())
    
    corr_df <- phone_filter() |>
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
  })
  
  
  # NUMERIC SUMMARIES:
  # one way table for os version
  output$os_distribution <- renderTable({
    req(phone_filter())
    
    phone_os <- phone_filter() |>
                pull(os) |>
                table() |>
                as.data.frame()
    
    names(phone_os) <- c("Phone OS", "Count")
    
    phone_os
  })
  
  # two way table for os version + group var
  output$os_two_way <- renderTable({
    req(phone_filter(), input$grp_var)
    
    phone_os_two <- phone_filter() |>
                    select(os, !!user_cat$sym) |>
                    table() |>
                    as.data.frame()
    
    names(phone_os_two) <- c("Phone OS", user_cat$title, "Count")
    
    phone_os_two
  })
  
  # summary table
  output$sum_table <- DT::renderDataTable({
    req(phone_filter(), input$grp_var)
    
    summary_table <- phone_filter() |>
        group_by(!!user_cat$sym) |>
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
               "Std. Dev." = "sd")
    names(summary_table)[1] <- user_cat$title
    
    summary_table
  })
}

shinyApp(ui = ui, server = server)
