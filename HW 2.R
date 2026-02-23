library(shiny)
library(tidyverse)
library(RColorBrewer)
zara <- read.csv("zara clothes.csv",
                 sep = ";",
                 stringsAsFactors = FALSE,
                 check.names = FALSE)

zara_clean <- zara %>%
  mutate(
    `Product Position` = factor(`Product Position`),
    Promotion = factor(Promotion),
    Seasonal = factor(Seasonal),
    section = factor(section),
    terms = factor(terms),
    price = as.numeric(price),
    `Sales Volume` = as.numeric(`Sales Volume`)
  ) %>%
  drop_na(price, `Sales Volume`)

price_rng <- range(zara_clean$price, na.rm = TRUE)

strategy_choices <- c(
  "Product Position" = "Product Position",
  "Promotion" = "Promotion"
)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Zara Product Strategy Evaluation"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "strategy_var",
        "Compare by:",
        choices = strategy_choices,
        selected = "Product Position"
      ),
      
      sliderInput(
        "price_range",
        "Price range (USD):",
        min = floor(price_rng[1]),
        max = ceiling(price_rng[2]),
        value = c(floor(price_rng[1]), ceiling(price_rng[2])),
        step = 1
      )
    ),
    
    mainPanel(
      h4("Sales Distribution"),
      plotOutput("dist_plot", height = 380),
      
      br(),
      
      h4("Average Sales Ranking"),
      plotOutput("rank_plot", height = 340),
      
      br(),
      
      tableOutput("summary_table")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    zara_clean %>%
      filter(
        price >= input$price_range[1],
        price <= input$price_range[2]
      )
  })
  
  # ---- Distribution Plot ----
  output$dist_plot <- renderPlot({
    
    d <- filtered_data()
    req(nrow(d) > 0)
    
    g <- input$strategy_var
    
    ggplot(d, aes(x = .data[[g]],
                  y = `Sales Volume`,
                  color = .data[[g]])) +
      geom_boxplot(outlier.shape = NA, alpha = 0.2) +
      geom_jitter(width = 0.15, alpha = 0.6, size = 2) +
      scale_color_brewer(palette = "Set2") +
      labs(
        x = NULL,
        y = "Sales Volume",
        title = paste("Sales distribution by", g)
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "none"
      )
  })
  
  # ---- Ranking Plot ----
  output$rank_plot <- renderPlot({
    
    d <- filtered_data()
    req(nrow(d) > 0)
    
    g <- input$strategy_var
    
    rank_df <- d %>%
      group_by(.data[[g]]) %>%
      summarise(
        avg_sales = mean(`Sales Volume`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_sales))
    
    ggplot(rank_df,
           aes(x = reorder(.data[[g]], avg_sales),
               y = avg_sales,
               fill = .data[[g]])) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = round(avg_sales, 0)),
        hjust = -0.15,
        size = 5
      ) +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = NULL,
        y = "Average Sales Volume",
        title = paste("Average sales ranking by", g)
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "none"
      ) +
      expand_limits(y = max(rank_df$avg_sales) * 1.08)
  })
  
  # ---- Summary Table ----
  output$summary_table <- renderTable({
    
    d <- filtered_data()
    req(nrow(d) > 0)
    
    g <- input$strategy_var
    
    d %>%
      group_by(.data[[g]]) %>%
      summarise(
        n_products = n(),
        avg_price = round(mean(price), 2),
        avg_sales = round(mean(`Sales Volume`), 1),
        median_sales = round(median(`Sales Volume`), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_sales))
  })
}

shinyApp(ui, server)

