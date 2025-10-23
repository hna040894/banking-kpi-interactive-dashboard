library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)

# Generate synthetic dataset
set.seed(42)
n_rules <- 20
n_cases <- 5000

# Create rule performance data
rule_data <- data.frame(
  case_rule = paste0("LDSDZ_", sample(c("MuleMule", "MisInvestment", "PotentialMule", 
                                        "AccountBalanceIncreaseMule", "RiskMID", 
                                        "Playstation", "TokenCombo", "BadSubVictims",
                                        "facebk_adhoc", "DigitalGoods", "CardsAtRiskTactical",
                                        "MPS_Mules", "DetectFalcon"), n_rules, replace = TRUE)),
  TP = sample(5:1500, n_rules),
  FP = sample(10:1500, n_rules),
  FPR = round(runif(n_rules, 0.5, 99), 2),
  TPrate = round(runif(n_rules, 0.01, 0.7), 2),
  Case_closed = sample(50:1600, n_rules),
  Case_created = sample(100:3500, n_rules)
)

# Create case history data
case_history <- data.frame(
  case_rule = rep(rule_data$case_rule, each = 5),
  case_reference_num = sample(600000000:700000000, 100),
  label = sample(c("FP", "TP"), 100, replace = TRUE, prob = c(0.7, 0.3)),
  score_customer = round(runif(100, 0, 1), 6),
  account_xid = sapply(1:100, function(x) paste0(sample(0:9, 15, replace = TRUE), collapse = "")),
  customer_xid = sample(1e10:9e10, 100),
  card_udv_product = sample(c("SEDBPB", "MASTERCARD", "VISA", "AMEX", "DISCOVER"), 100, replace = TRUE)
)

# Create time series data for cases
dates <- seq(as.Date("2025-08-15"), as.Date("2025-10-01"), by = "day")
case_creation_data <- data.frame(
  partition_date = rep(dates, each = 2),
  label = rep(c("FP", "TP"), length(dates)),
  num_cases = c(rpois(length(dates), 800), rpois(length(dates), 200))
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Banking Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("DEBIT", tabName = "debit", selected = TRUE),
      menuItem("CREDIT", tabName = "credit"),
      menuItem("RETAIL BANKING", tabName = "retail")
    ),
    
    h4("Dashboard controls", style = "padding-left: 15px; margin-top: 20px;"),
    
    checkboxGroupInput("country", "Select Country:",
                       choices = c("SE" = "se", "FI" = "fi", "GB" = "gb", "DK" = "dk"),
                       selected = c("se", "fi", "gb", "dk")),
    
    checkboxGroupInput("segment", "Select Cust Segment:",
                       choices = c("RB", "BB", "VP"),
                       selected = c("RB", "BB", "VP")),
    
    dateRangeInput("daterange", "Period:",
                   start = "2025-08-07",
                   end = "2025-10-06"),
    
    checkboxGroupInput("rbflow", "Select RB Flow:",
                       choices = c("VP", "DB", "SI", "SU", "BL"),
                       selected = c("VP", "DB", "SI", "SU", "BL"))
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ecf0f5; }
        .box { margin-bottom: 20px; }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #3c8dbc; }
      "))
    ),
    
    fluidRow(
      column(width = 6,
             box(
               title = "Rule name list",
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               
               tabsetPanel(
                 id = "rule_tabs",
                 tabPanel("SPECIALIZED", value = "specialized",
                          div(style = "height: 350px; overflow-y: auto;",
                              DTOutput("specialized_table"))
                 ),
                 tabPanel("CORE", value = "core",
                          div(style = "height: 350px; overflow-y: auto;",
                              DTOutput("core_table"))
                 ),
                 tabPanel("STRATEGIC", value = "strategic",
                          div(style = "height: 350px; overflow-y: auto;",
                              DTOutput("strategic_table"))
                 ),
                 tabPanel("TACTICAL", value = "tactical", 
                          div(style = "height: 350px; overflow-y: auto;",
                              DTOutput("rule_table"))
                 ),
                 tabPanel("FALLBACK", value = "fallback",
                          div(style = "height: 350px; overflow-y: auto;",
                              DTOutput("fallback_table"))
                 )
               )
             )
      ),
      
      column(width = 6,
             box(
               title = "Case creation by day",
               width = NULL,
               solidHeader = TRUE,
               status = "info",
               plotly::plotlyOutput("time_series_plot", height = "400px")
             )
      )
    ),
    
    fluidRow(
      column(width = 6,
             box(
               title = "Case history",
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               
               tabsetPanel(
                 id = "history_tabs",
                 tabPanel("SPECIALIZED", value = "specialized_hist",
                          div(style = "height: 350px; overflow-y: auto;",
                              DTOutput("specialized_history_table"))
                 ),
                 tabPanel("CORE", value = "core_hist",
                          div(style = "height: 350px; overflow-y: auto;",
                              DTOutput("core_history_table"))
                 ),
                 tabPanel("STRATEGIC", value = "strategic_hist",
                          div(style = "height: 350px; overflow-y: auto;",
                              DTOutput("strategic_history_table"))
                 ),
                 tabPanel("TACTICAL", value = "tactical_hist", 
                          div(style = "height: 350px; overflow-y: auto;",
                              DTOutput("case_history_table"))
                 ),
                 tabPanel("FALLBACK", value = "fallback_hist",
                          div(style = "height: 350px; overflow-y: auto;",
                              DTOutput("fallback_history_table"))
                 )
               )
             )
      ),
      
      column(width = 6,
             box(
               title = "Graph",
               width = NULL,
               solidHeader = TRUE,
               status = "info",
               selectInput("graph_select", "Select graph:", 
                           choices = c("L1", "L2", "L3"), 
                           selected = "L1"),
               plotly::plotlyOutput("bar_plot", height = "360px")
             )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store selected rule
  selected_rule <- reactiveVal(NULL)
  
  # Synchronize tabs between Rule name list and Case history
  observeEvent(input$rule_tabs, {
    if (input$rule_tabs == "specialized") {
      updateTabsetPanel(session, "history_tabs", selected = "specialized_hist")
    } else if (input$rule_tabs == "core") {
      updateTabsetPanel(session, "history_tabs", selected = "core_hist")
    } else if (input$rule_tabs == "strategic") {
      updateTabsetPanel(session, "history_tabs", selected = "strategic_hist")
    } else if (input$rule_tabs == "tactical") {
      updateTabsetPanel(session, "history_tabs", selected = "tactical_hist")
    } else if (input$rule_tabs == "fallback") {
      updateTabsetPanel(session, "history_tabs", selected = "fallback_hist")
    }
  })
  
  # SPECIALIZED tables
  output$specialized_table <- renderDT({
    specialized_data <- rule_data %>% slice(1:8)
    datatable(
      specialized_data,
      options = list(
        pageLength = 15,
        lengthMenu = c(5, 10, 15, 25),
        searching = TRUE,
        dom = 'lftip',
        scrollX = TRUE
      ),
      rownames = TRUE,
      selection = 'single',
      class = 'cell-border stripe'
    )
  })
  
  # Update selected rule when row is clicked in SPECIALIZED
  observeEvent(input$specialized_table_rows_selected, {
    if (length(input$specialized_table_rows_selected) > 0) {
      specialized_data <- rule_data %>% slice(1:8)
      selected_rule(specialized_data$case_rule[input$specialized_table_rows_selected])
    }
  })
  
  output$specialized_history_table <- renderDT({
    if (!is.null(selected_rule())) {
      filtered_history <- case_history %>% filter(case_rule == selected_rule())
    } else {
      filtered_history <- case_history %>% slice(1:20)
    }
    
    datatable(
      filtered_history %>% select(-case_rule),
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25),
        searching = TRUE,
        scrollX = TRUE
      ),
      rownames = TRUE,
      class = 'cell-border stripe'
    )
  })
  
  # CORE tables
  output$core_table <- renderDT({
    core_data <- rule_data %>% slice(5:12)
    datatable(
      core_data,
      options = list(
        pageLength = 15,
        lengthMenu = c(5, 10, 15, 25),
        searching = TRUE,
        dom = 'lftip',
        scrollX = TRUE
      ),
      rownames = TRUE,
      selection = 'single',
      class = 'cell-border stripe'
    )
  })
  
  # Update selected rule when row is clicked in CORE
  observeEvent(input$core_table_rows_selected, {
    if (length(input$core_table_rows_selected) > 0) {
      core_data <- rule_data %>% slice(5:12)
      selected_rule(core_data$case_rule[input$core_table_rows_selected])
    }
  })
  
  output$core_history_table <- renderDT({
    if (!is.null(selected_rule())) {
      filtered_history <- case_history %>% filter(case_rule == selected_rule())
    } else {
      filtered_history <- case_history %>% slice(21:40)
    }
    
    datatable(
      filtered_history %>% select(-case_rule),
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25),
        searching = TRUE,
        scrollX = TRUE
      ),
      rownames = TRUE,
      class = 'cell-border stripe'
    )
  })
  
  # STRATEGIC tables
  output$strategic_table <- renderDT({
    strategic_data <- rule_data %>% slice(8:15)
    datatable(
      strategic_data,
      options = list(
        pageLength = 15,
        lengthMenu = c(5, 10, 15, 25),
        searching = TRUE,
        dom = 'lftip',
        scrollX = TRUE
      ),
      rownames = TRUE,
      selection = 'single',
      class = 'cell-border stripe'
    )
  })
  
  # Update selected rule when row is clicked in STRATEGIC
  observeEvent(input$strategic_table_rows_selected, {
    if (length(input$strategic_table_rows_selected) > 0) {
      strategic_data <- rule_data %>% slice(8:15)
      selected_rule(strategic_data$case_rule[input$strategic_table_rows_selected])
    }
  })
  
  output$strategic_history_table <- renderDT({
    if (!is.null(selected_rule())) {
      filtered_history <- case_history %>% filter(case_rule == selected_rule())
    } else {
      filtered_history <- case_history %>% slice(41:60)
    }
    
    datatable(
      filtered_history %>% select(-case_rule),
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25),
        searching = TRUE,
        scrollX = TRUE
      ),
      rownames = TRUE,
      class = 'cell-border stripe'
    )
  })
  
  # FALLBACK tables
  output$fallback_table <- renderDT({
    fallback_data <- rule_data %>% slice(13:20)
    datatable(
      fallback_data,
      options = list(
        pageLength = 15,
        lengthMenu = c(5, 10, 15, 25),
        searching = TRUE,
        dom = 'lftip',
        scrollX = TRUE
      ),
      rownames = TRUE,
      selection = 'single',
      class = 'cell-border stripe'
    )
  })
  
  # Update selected rule when row is clicked in FALLBACK
  observeEvent(input$fallback_table_rows_selected, {
    if (length(input$fallback_table_rows_selected) > 0) {
      fallback_data <- rule_data %>% slice(13:20)
      selected_rule(fallback_data$case_rule[input$fallback_table_rows_selected])
    }
  })
  
  output$fallback_history_table <- renderDT({
    if (!is.null(selected_rule())) {
      filtered_history <- case_history %>% filter(case_rule == selected_rule())
    } else {
      filtered_history <- case_history %>% slice(61:80)
    }
    
    datatable(
      filtered_history %>% select(-case_rule),
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25),
        searching = TRUE,
        scrollX = TRUE
      ),
      rownames = TRUE,
      class = 'cell-border stripe'
    )
  })
  
  # Rule performance table (TACTICAL)
  output$rule_table <- renderDT({
    datatable(
      rule_data,
      options = list(
        pageLength = 15,
        lengthMenu = c(5, 10, 15, 25),
        searching = TRUE,
        dom = 'lftip',
        scrollX = TRUE
      ),
      rownames = TRUE,
      selection = 'single',
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        columns = 1:7,
        backgroundColor = styleEqual(
          rule_data$case_rule[5],
          c('#4A90E2')
        )
      )
  })
  
  # Update selected rule when row is clicked in TACTICAL
  observeEvent(input$rule_table_rows_selected, {
    if (length(input$rule_table_rows_selected) > 0) {
      selected_rule(rule_data$case_rule[input$rule_table_rows_selected])
    }
  })
  
  # Case history table (TACTICAL)
  output$case_history_table <- renderDT({
    if (!is.null(selected_rule())) {
      filtered_history <- case_history %>% filter(case_rule == selected_rule())
    } else {
      filtered_history <- case_history
    }
    
    datatable(
      filtered_history %>% select(-case_rule),
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25),
        searching = TRUE,
        scrollX = TRUE
      ),
      rownames = TRUE,
      class = 'cell-border stripe'
    )
  })
  
  # Time series plot
  output$time_series_plot <- plotly::renderPlotly({
    p <- ggplot(case_creation_data, aes(x = partition_date, y = num_cases, fill = label)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("FP" = "#5DADE2", "TP" = "#1F618D")) +
      theme_minimal() +
      labs(x = "partition_date", y = "Number of cases", fill = "label") +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.minor = element_blank()
      ) +
      scale_x_date(date_labels = "%b %d", date_breaks = "15 days")
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(hovermode = "closest")
  })
  
  # Bar plot for label distribution - reacts to graph selection
  output$bar_plot <- plotly::renderPlotly({
    # Different data based on selection
    if (input$graph_select == "L1") {
      plot_data <- data.frame(
        category = c("Token", "Ecom"),
        FP = c(150, 220),
        TP = c(30, 50)
      )
    } else if (input$graph_select == "L2") {
      plot_data <- data.frame(
        category = c("Mobile", "Desktop", "Tablet"),
        FP = c(180, 200, 100),
        TP = c(45, 60, 25)
      )
    } else { # L3
      plot_data <- data.frame(
        category = c("Region A", "Region B", "Region C", "Region D"),
        FP = c(120, 160, 140, 190),
        TP = c(35, 40, 30, 55)
      )
    }
    
    plot_data_long <- tidyr::pivot_longer(plot_data, cols = c(FP, TP), 
                                          names_to = "label", values_to = "count")
    
    p <- ggplot(plot_data_long, aes(x = category, y = count, fill = label, 
                                    text = paste0("Category: ", category, "<br>",
                                                  "Label: ", label, "<br>",
                                                  "Count: ", count))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("FP" = "#5DADE2", "TP" = "#1F618D")) +
      theme_minimal() +
      labs(x = "", y = "Number of cases", fill = "label") +
      theme(
        legend.position = "right",
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })
}

# Run the application
shinyApp(ui = ui, server = server)