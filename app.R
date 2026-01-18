# Load the packages
library(shiny) 
library(tidyverse) 
library(leaflet) 
library(sf)
library(spData)
library(shinyjs) 
library(plotly) 
library(RColorBrewer)
library(lubridate)

# Load the data
athletes <- read_csv("data/athletes.csv")
# Modify the country name in athletes
athletes$country[athletes$country == "Great Britain"] = "United Kingdom"
athletes$country[athletes$country == "Chinese Taipei"] = "Taiwan"
athletes$country[athletes$country == "IR Iran"] = "Iran"
athletes$country[athletes$country == "Türkiye"] = "Turkey"

medals <- read_csv("data/medals.csv")
medals$medal_date <- as.factor(medals$medal_date)

# Get the spatial data for the world
world_sf <- world

# Generate a blank medal data frame
medals_blank <- data.frame(
  medal_type = rep(c("Gold Medal", "Silver Medal", "Bronze Medal")),
  medal_date = as.factor(rep(unique(medals$medal_date), each = 3))
)

medals_blank$medal_type <- factor(medals_blank$medal_type, levels = c("Gold Medal", "Silver Medal", "Bronze Medal"))

# Generate a blank medal data frame
medals_blank1 <- data.frame(
  medal_type = rep(c("Gold Medal", "Silver Medal", "Bronze Medal")),
  discipline = rep(unique(medals$discipline), each = 3)
)

# UI
ui <- fluidPage(
  # Use custom JavaScript functions in shiny
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(
      HTML(
        "
      /* Adjust map size and margin */
      body { 
        margin: 5px;
        padding: 0;
      }
      .container-fluid {
        height: 100%;
        padding: 0;
      }
      /* Adjust charts margin */
      .plotly.html-widget { 
        margin-bottom: 30px;
      }
      .plotly .g-gtitle {
        margin-bottom: 30px !important;
      }
      /* Adjust map title position */
      .map-title { 
        transform: translateX(-50%);
        right: 50% !important;
      }
      /* About button background color */
      #aboutBtn { 
            background-color: rgba(255, 255, 255, 0.7);
      }
      /* Modal background color */
      .modal-backdrop { 
            opacity: 0.7 !important;
        }
      .modal-content {
          background-color: rgba(255, 255, 255, 0.8) !important;
      }
    "
      )
    ),
    tags$script(
      "
      /* Click area to set country name */
      function showDetails(CountryName) { 
        Shiny.setInputValue('clickedCountry', CountryName);
      }
    "
    )
  ),
  
  # CSS for the transparent drop down menu
  tags$style(type = "text/css", "
             #control-panel {
               border-radius: 10px;
             }
             "),
  # Set the app's background to light grey
  tags$head(tags$style(
    HTML("
    body {
      background-color: #EBEBEB;
    }
  ")
  )),
  
  # Set map width and height
  leafletOutput(
    outputId = "map",
    width = "100%",
    height = "98.5vh"
  ),
  
  # Absolute panel for drop down menu
  absolutePanel(
    id = "control-panel",
    top = 15,
    left = 70,
    width = 120,
    selectInput("selected_gender", "Select Gender:", choices = c("Male", "Female", "All"), selected = "All")
  ),
  
  # About button
  actionButton("aboutBtn", "About", style = "position: absolute; bottom: 13px; left: 10px;")
)

# Server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    # Filter data for the gender
    if(input$selected_gender != "All") {
      selected_data <- athletes[athletes$gender == input$selected_gender, ]
    } else {
      selected_data <- athletes
    }
    
    # Compute the number of athletes
    athletes_num <- selected_data %>%
      group_by(country) %>%
      count()
    
    # Join the athletes data with the spatial data
    map_data <-
      left_join(world_sf, athletes_num, by = c("name_long" = "country"))
    
    # Customise the colorQuantile function for reusing
    color_scale <- colorQuantile("Purples", map_data$n, n = 5)
    
    leaflet(data = map_data) %>%
      # Set map start view position and zoom status
      setView(lng = -4,
              lat = 50,
              zoom = 2.4) %>%
      # Set map color theme
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~color_scale(n),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        popup = ~ paste0(
          "<strong>Country:</strong> ",
          name_long,
          "<br>",
          "<strong>Athletes:</strong> ",
          n,
          "<br>",
          "<button onclick='showDetails(\"",
          name_long,
          "\")'>Details</button>"
        )
      ) %>%
      
      # Add map title
      addControl(
        html = tags$div(style = "background-color: rgba(255, 255, 255, 0.6); color: purple; padding: 5px; border-radius: 5px;
                        font-size: 24px;", "Paris 2024 Olympic Summer Games"),
        position = "topright",
        className = "map-title"
      )
    
  })
  
  # Click About button, pop out About window
  observeEvent(input$aboutBtn, {
    showModal(aboutModal())
  })
  
  # About window prints the description of this app and acknowledge from author
  aboutModal <- function() {
    modalDialog(
      title = "About this App",
      tagList(
        tags$h4("Assignment: Interactive Data Visualisation Interface using R"),
        tags$p(
          "This Shiny app visualizes Olympic Summer Games data in Paris 2024."
        ),
        tags$hr(),
        tags$h4("How to use this app?"),
        tags$p("Select gender to see the data in specified gender(the default is All)."),
        tags$p(
          "Click the country area on the map to see the game data in selected gender."
        ),
        tags$p(
          "Click 'Details' to see the detailed game data about this country."
        ),
        tags$h4("What do these charts mean?"),
        tags$p(
          "The 1st pie chart provides the proportion of participants in different events for the selected country and gender."
        ),
        tags$p(
          "The 1st histogram chart shows the age distribution for the selected country and gender."
        ),
        tags$p(
          "The 1nd line chart illustrates the number of medals over time for the selected country."
        ),
        tags$p(
          "The 1nd heatmap chart shows the number of medals in different events for the selected country."
        ),
        tags$hr(),
        tags$p(
          "Data Reference: https://www.kaggle.com/datasets/piterfm/paris-2024-olympic-summer-games"
        ),
      ),
      easyClose = TRUE,
      footer = tagList(modalButton("Close"))
    )
  }
  
  # click detail button and pop out the modal
  observeEvent(input$clickedCountry, {
    clicked_country <- input$clickedCountry
    selected_gender <- input$selected_gender
    
    # Filter the data based on the selected gender
    if(selected_gender != "All") {
      selected_data <- athletes[athletes$gender == selected_gender, ]
    } else {
      selected_data <- athletes
    }
    
    # Filter the data based on the clicked country
    selected_data <- selected_data %>%
      filter(country == clicked_country)
    
    # Filter the data based on the clicked country
    medals_data <- medals %>%
      filter(country == clicked_country)
    
    # Render the first pie chart 
    # Display the number of athletes in different events in selected gender
    output$pieChart <- renderPlotly({
      p1 <- selected_data %>%
        mutate(disciplines = str_sub(disciplines, 3, -3)) %>%
        group_by(disciplines) %>%
        count() %>%
        ungroup() %>%
        mutate(prop = n/sum(n)) %>%
        arrange(desc(n)) %>%
        filter(prop > 0.032) %>%
        mutate(disciplines = factor(disciplines, levels = disciplines)) %>%
        plot_ly(values = ~n, labels = ~ factor(disciplines),
                type = "pie", textposition = 'inside', 
                textinfo = "label+text+value+percent", hoverinfo = "label+text+value+percent",
                marker = list(colors = brewer.pal(12,"Set3"))) %>% 
        layout(title = list(text = "Events Percentage of Paris 2024 Olympic Summer Games",
                            font = list(size = 14, family = "Arial Black")))
      p1
    })
    
    # Render the first histogram chart 
    # Display the number of athletes in different events in selected gender
    output$histChart <- renderPlotly({
      p2 <- selected_data %>%
        mutate(age = 2024 - year(selected_data$birth_date)) %>%
        ggplot(aes(x = age)) +
        geom_histogram(aes(y = ..density..), 
                       color = "white", fill = "#8F74C1") +
        labs(title = "Distribution of Age of Paris 2024 Olympic Summer Games",
             x = "Age", y = "Density") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))
      if(nrow(selected_data) >= 50) {
        p2 <- p2 +  geom_density(col = "#90B5A3")
      }
      p2
    })
    
    # Render the first dot chart 
    # Display the time series of medals
    output$lineChart <- renderPlotly({
      if(nrow(medals_data) > 0) {
        
        p3 <- medals_data %>%
          mutate(medal_type = factor(medal_type, levels = c("Gold Medal", "Silver Medal", "Bronze Medal")),
                 medal_date = factor(medal_date, levels = unique(medals$medal_date))) %>%
          arrange(medal_date, medal_type) %>%
          group_by(medal_date) %>%
          mutate(count = seq(n())) %>% 
          right_join(medals_blank) %>%
          mutate(count = if_else(is.na(count), -1, count)) %>%
          ggplot(aes(x = medal_date, y = count, color = medal_type)) +
          geom_point(size = 2) + 
          labs(title = "Time Series of Medals in Paris 2024 Olympic Summer Games",
               x = "", y = "Medals Number", fill = "") +
          scale_color_manual(values = c("gold", "#C0C0C0", "#b87333")) +
          theme_bw() +
          ylim(0, 20) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                legend.title = element_text(size = 7),
                legend.text = element_text(size = 5),
                plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      } else {
        p3 <- data.frame(medal_date = factor(unique(medals_blank$medal_date)),
                 count = rep(0, 16)) %>%
          ggplot(aes(x = medal_date, y = count)) +
          geom_point(size = 1, col = "white") + 
          labs(title = "Time Series of Medals in Paris 2024 Olympic Summer Games",
               x = "", y = "Medals Number", fill = "") +
          ylim(0, 20) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      }
      p3
    })
    
    # Render the first heatmap chart 
    # Display the distribution of medals in different events
    output$heatmapChart <- renderPlotly({
      p4 <- medals_data %>%
        group_by(medal_type, discipline) %>%
        count() %>%
        ungroup() %>%
        right_join(medals_blank1) %>%
        mutate(n = if_else(is.na(n), 0, n),
               medal_type = factor(medal_type, levels = c("Gold Medal", "Silver Medal", "Bronze Medal")),
               discipline = factor(discipline)) %>%
        ggplot(aes(medal_type, discipline, fill = n)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "#342870") +
        labs(x = "Medal Type", y = "Events", 
             title = "Medals Distribution in Paris 2024 Olympic Summer Games", fill = 'Medals') +
        theme_bw()
      if(nrow(medals_data) == 0){
        p4 <- p4 + 
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
                axis.text = element_text(size = 6), legend.position = "none",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      } else {
        p4 <- p4 + 
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
                axis.text = element_text(size = 6),
                legend.title = element_text(size = 7),
                legend.text = element_text(size = 5),
                axis.ticks = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      }
      p4
    })
    
    # The detail modal shows 4 charts we implemented above
    showModal(
      modalDialog(
        title = paste0("Detailed Information for ", clicked_country),
        plotlyOutput("pieChart"),
        plotlyOutput("histChart"),
        plotlyOutput("lineChart"),
        plotlyOutput("heatmapChart"),
        easyClose = TRUE,
        size = "m",
        footer = tagList(modalButton("Close"))
      )
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)