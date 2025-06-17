library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinydashboard)

# Load the dataset
data <- read.csv("World-happiness-report-updated_2024.csv")
data$Country.name <- iconv(data$Country.name, from = "latin1", to = "UTF-8")

# Define UI for the app
ui <- dashboardPage(
  dashboardHeader(title = "World Happiness Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home Page
      tabItem(tabName = "home",
              h2("Welcome to the World Happiness Analysis App"),
              p("This application allows you to explore various aspects of global happiness trends over the years."),
              p("Navigate to the 'Plots' page to explore visualizations and analyses of happiness data."),
              h2("Presented By"),
              p("Youssef Mohamed Abdelhamid Mohamed, for COMP3042 Data Visualisation Coursework"),
              h2("Data Sources"),
              p("World Happiness Report (2006-2023): https://www.kaggle.com/datasets/jainaru/world-happiness-report-2024-yearly-updated?resource=download&select=World-happiness-report-updated_2024.csv"),
              p("Continents for Each Country: https://www.kaggle.com/datasets/hserdaraltan/countries-by-continent")
              
              
      ),
      
      # Plots Page
      tabItem(tabName = "plots",
              tabsetPanel(
                tabPanel("Global Happiness Trends", plotlyOutput("globalHappinessPlot", height = "600px")),
                tabPanel("Happiest & Least Happy Countries", plotlyOutput("happiestCountriesPlot", height = "600px")),
                tabPanel("Significant Happiness Changes", plotlyOutput("happinessChangePlot", height = "600px")),
                tabPanel("Happiness by Continent", plotlyOutput("continentHappinessPlot", height = "600px")),
                tabPanel("Country and Continent Happiness Trends", plotlyOutput("continentCountryTrendPlot", height = "600px")),
                tabPanel("Correlation Matrix", plotlyOutput("correlationPlot", height = "600px")),
                tabPanel("Happiness Trends by Category", plotlyOutput("categoryHappinessPlot", height = "600px")),
                tabPanel("GDP vs Happiness", plotlyOutput("gdpHappinessPlot", height = "600px")),
                # World Map Tab
                tabPanel("World Map",
                         fluidRow(
                           column(3,
                                  selectInput("mapYear", 
                                              "Select Year",
                                              choices = sort(unique(data$year), decreasing = TRUE),
                                              selected = max(data$year)
                                  ),
                                  selectInput("mapAttribute", 
                                              "Select Attribute",
                                              choices = c("Life.Ladder", "Log.GDP.per.capita", "Social.support", 
                                                          "Healthy.life.expectancy.at.birth", "Freedom.to.make.life.choices", 
                                                          "Generosity", "Perceptions.of.corruption", "Positive.affect", 
                                                          "Negative.affect"),
                                              selected = "Life.Ladder"
                                  )
                           ),
                           column(9,
                                  plotlyOutput("worldMapPlot", height = "600px")
                           )
                         )
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # How has the average happiness (Life Ladder) score changed over time globally?
    
  output$globalHappinessPlot <- renderPlotly({
      global_happiness <- data %>%
        filter(year >= 2006) %>%
        group_by(year) %>%
        summarise(avg_happiness = mean(Life.Ladder, na.rm = TRUE))
      

      p <- ggplot(global_happiness, aes(x = year, y = avg_happiness)) +
        geom_line(color = "blue", size = 1) +
        geom_point(color = "blue") +
        theme_minimal() +
        labs(
          title = "Global Average Happiness Score (2006-2023)",
          x = "Year",
          y = "Average Happiness Score"
        )
      
      ggplotly(p)
  })
  
  # Which countries are the happiest and least happy on average over the full time period?
  
  output$happiestCountriesPlot <- renderPlotly({
    country_happiness <- data %>%
      group_by(Country.name) %>%
      summarize(avg_happiness = mean(Life.Ladder, na.rm = TRUE)) %>%
      arrange(desc(avg_happiness))
    
    happiest_countries <- head(country_happiness, 10)
    least_happy_countries <- tail(country_happiness, 10)
    
    p <- ggplot(rbind(happiest_countries, least_happy_countries), aes(x = reorder(Country.name, avg_happiness), y = avg_happiness)) +
      geom_bar(stat = "identity", aes(fill = avg_happiness > median(avg_happiness))) +
      coord_flip() +
      labs(title = "Top 10 Happiest and Least Happy Countries", x = "Country", y = "Average Happiness (Life Ladder)") +
      scale_fill_manual(values = c("red", "green"), labels = c("Least Happy", "Happiest")) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  # Are there specific continents where happiness has consistently increased or decreased over the years?
  
  output$continentHappinessPlot <- renderPlotly({
    continent_happiness <- data %>%
      group_by(Continent, year) %>%
      summarize(avg_happiness = mean(Life.Ladder, na.rm = TRUE))
    
    p <- ggplot(continent_happiness, aes(x = year, y = avg_happiness, color = Continent)) +
      geom_line() +
      labs(title = "Happiness Trend by Continent", x = "Year", y = "Average Happiness (Life Ladder)") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  
  # Do happiness trends in countries match the continent?
  
  output$continentCountryTrendPlot <- renderPlotly({
    countries_with_2023 <- data %>%
      filter(year == 2023) %>%
      pull(Country.name)
    
    
    average_countries <- country_averages %>%
      filter(has_2023_data) %>%
      group_by(Continent) %>%
      mutate(
        continent_avg = mean(avg_happiness),
        diff_from_avg = abs(avg_happiness - continent_avg),
        is_extreme = avg_happiness == max(avg_happiness) | 
          avg_happiness == min(avg_happiness)
      ) %>%
      filter(!is_extreme) %>%
      arrange(Continent, diff_from_avg) %>%
      group_by(Continent) %>%
      slice_head(n = 2) %>%
      mutate(country_type = "Average") %>%
      select(Continent, Country.name, country_type, avg_happiness, diff_from_avg)
    
    representative_countries_list <- bind_rows(
      country_averages %>%
        filter(has_2023_data) %>%
        group_by(Continent) %>%
        summarize(
          highest = list(Country.name[which.max(avg_happiness)]),
          lowest = list(Country.name[which.min(avg_happiness)])
        ) %>%
        tidyr::pivot_longer(
          cols = c(highest, lowest),
          names_to = "country_type",
          values_to = "Country.name"
        ) %>%
        tidyr::unnest(Country.name) %>%
        mutate(country_type = if_else(country_type == "highest", "Highest", "Lowest")),
      
      average_countries %>%
        select(Continent, Country.name, country_type)
    )
    
    continent_avg_happiness <- data %>%
      group_by(Continent, year) %>%
      summarize(avg_happiness = mean(Life.Ladder, na.rm = TRUE), .groups = "drop")
    
    label_data <- representative_data %>%
      group_by(Country.name, Continent) %>%
      filter(year == max(year)) %>%
      ungroup() %>%
      group_by(Continent) %>%
      arrange(Continent, desc(Life.Ladder)) %>%
      mutate(
        y_offset = case_when(
          Life.Ladder == max(Life.Ladder) ~ 0.35,
          Life.Ladder == min(Life.Ladder) ~ -0.35,
          row_number() == 2 ~ 0.15,
          TRUE ~ -0.15
        ),
        label_y = Life.Ladder + y_offset
      ) %>%
      ungroup()
    
    p <- ggplot() +
      geom_line(data = continent_avg_happiness,
                aes(x = year, y = avg_happiness),
                color = "black", size = 1.2, alpha = 0.5) +
      geom_line(data = representative_data,
                aes(x = year, y = Life.Ladder, 
                    color = Country.name,
                    linetype = country_type),
                size = 0.7) +
      geom_point(data = representative_data,
                 aes(x = year, y = Life.Ladder,
                     color = Country.name),
                 size = 1) +
      geom_segment(data = label_data,
                   aes(x = year, xend = year + 1,
                       y = Life.Ladder, yend = label_y,
                       color = Country.name),
                   size = 0.3,
                   show.legend = FALSE) +
      geom_text(data = label_data,
                aes(x = year + 2,
                    y = label_y,
                    label = Country.name,
                    color = Country.name),
                hjust = 0,
                size = 2.8,
                show.legend = FALSE) +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
      facet_wrap(~Continent, ncol = 1, scales = "free_y") +
      scale_y_continuous(
        breaks = function(x) pretty(x, n = 3),  
        expand = expansion(mult = c(0.05, 0.05))  
      ) +
      theme_minimal() +
      labs(
        title = "Continent and Country Hapiness Trends",
        subtitle = "Continental average (black) with highest, lowest, and two most representative countries",
        x = "Year",
        y = "Life Ladder Score"
      ) +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.margin = margin(5, 35, 5, 5)
      )
    
    ggplotly(p)
  })
  
  # Have any countries experienced significant increases or decreases in happiness over the years, and what factors might contribute to this?
  
  output$happinessChangePlot <- renderPlotly({
    happiness_change <- data %>%
      group_by(Country.name) %>%
      arrange(year) %>%
      summarize(change = last(Life.Ladder) - first(Life.Ladder))
    
    significant_changes <- happiness_change %>%
      arrange(desc(abs(change))) %>%
      top_n(10, wt = abs(change)) %>%
      mutate(change_label = factor(
        change > 0,
        levels = c(TRUE, FALSE),
        labels = c("Increase", "Decrease")
      ))
    
    p <- ggplot(significant_changes, aes(x = reorder(Country.name, change), y = change)) +
      geom_bar(stat = "identity", aes(fill = change_label)) + # Use the labeled factor
      coord_flip() +
      labs(title = "Countries with Significant Changes in Happiness", 
           x = "Country", 
           y = "Change in Happiness (Life Ladder)",
           fill = "Happiness Change") +
      scale_fill_manual(
        values = c("green", "red")
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
  # What factors have the strongest correlation with happiness over time?
  
  output$correlationPlot <- renderPlotly({
    numeric_cols <- data %>%
      select(where(is.numeric))
    
    correlation_matrix <- cor(numeric_cols, use = "complete.obs")
    correlation_data <- as.data.frame(as.table(correlation_matrix))
    
    p <- ggplot(correlation_data, aes(x = Var1, y = Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                           name = "Correlation") +
      labs(title = "Correlation Matrix of Numerical Variables",
           x = "Variables", y = "Variables") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Are the happiest countries getting happier while the least happy stagnate?
  
  output$categoryHappinessPlot <- renderPlotly({
    filtered_data <- data %>%
      filter(year >= 2006)
    
    country_happiness <- filtered_data %>%
      group_by(Country.name) %>%
      summarize(median_happiness = median(Life.Ladder, na.rm = TRUE)) %>%
      arrange(desc(median_happiness))
    
    top_percentage <- 0.25
    threshold <- quantile(country_happiness$median_happiness, probs = 1 - top_percentage)
    
    filtered_data <- filtered_data %>%
      mutate(category = ifelse(
        Country.name %in% (country_happiness %>% filter(median_happiness >= threshold) %>% pull(Country.name)),
        "Happiest", "Others"
      ))
    
    category_happiness_trends <- filtered_data %>%
      group_by(year, category) %>%
      summarize(median_happiness = median(Life.Ladder, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(category_happiness_trends, aes(x = year, y = median_happiness, color = category)) +
      geom_line(size = 1.2) +
      labs(
        title = "Happiness Trends: Happiest Countries vs Others (2006 Onward)",
        x = "Year",
        y = "Median Happiness (Life Ladder)",
        color = "Category"
      ) +
      scale_color_manual(values = c("Happiest" = "green", "Others" = "red")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Given that GDP has the highest correlation with happiness, are there countries with high GDP but low happiness, and vice versa?
  
  output$gdpHappinessPlot <- renderPlotly({
    data$text_info <- paste("Country: ", data$Country.name, "<br>Year: ", data$year)
    
    plot <- ggplot(data, aes(x = Log.GDP.per.capita, y = Life.Ladder)) +
      geom_point(aes(text = text_info), color = "blue") +  # Add hover text for each point
      geom_smooth(method = "lm", color = "red", se = FALSE) +  # Red linear regression line
      labs(title = "Relationship Between GDP and Happiness (Life Ladder)",
           x = "GDP per capita",
           y = "Life Ladder (Happiness)") +
      theme_minimal()
    
    ggplotly(plot, tooltip = "text")
  })
  
  output$worldMapPlot <- renderPlotly({
    req(input$mapYear, input$mapAttribute)
    
    map_data <- data %>%
      filter(year == input$mapYear) %>%
      select(Country.name, !!sym(input$mapAttribute))
    
    plot_ly(
      data = map_data,
      type = "choropleth",
      locations = ~Country.name,
      locationmode = "country names",
      z = map_data[[input$mapAttribute]],
      text = ~paste("Country:", Country.name, 
                    "<br>", input$mapAttribute, ":", round(map_data[[input$mapAttribute]], 2)),
      colorscale = "Viridis",
      colorbar = list(title = input$mapAttribute)
    ) %>%
      layout(
        title = list(
          text = paste(input$mapAttribute, "Map -", input$mapYear),
          x = 0.5
        ),
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "equirectangular")
        ),
        margin = list(l = 0, r = 0, t = 50, b = 0)
      ) %>%
      config(
        scrollZoom = TRUE,
        displayModeBar = TRUE,
        displaylogo = FALSE
      )
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)