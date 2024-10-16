
# Load necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(jsonlite)
library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinyjs)
library(RColorBrewer)
library(sf) 



 

# Read JSON file
json_data <- fromJSON("homicide-rate-by-age-of-the-victim.json")

# Convert JSON data to data frame
json_df <- as.data.frame(json_data)

# Write JSON data to a CSV file
write.csv(json_df, "homicide-rate-by-age-of-the-victim_update.csv", row.names = FALSE)

# Read spatial data from a geojson file
world <- st_read("world-administrative-boundaries.geojson")

# Read spatial data from a CSV file
spatial_data <- read.csv("world_country_and_usa_states_latitude_and_longitude_values.csv")

# Read GDP data from a CSV file
gdp <- read.csv("homicide-rate-vs-gdp-pc.csv")

# Read age data from a CSV file
age_data <- read.csv("homicide-rate-by-age-of-the-victim_update.csv")

# Read data from an Excel file, skipping the first two rows and using column names
cts_data <- read_excel("data_cts_intentional_homicide.xlsx", skip = 2, col_names = TRUE)

  


 


colnames(spatial_data)
colnames(gdp)
colnames(age_data)
colnames(cts_data)

  



 
# Remove rows with years outside the range of 1990 to 2023 and handle -1000 values for gdp_data
gdp <- subset(gdp, Year >= 1990 & Year <= 2023)

# Remove rows with years outside the range of 1990 to 2023 and handle -1000 values for age_data
age_data <- subset(age_data, Year >= 1990 & Year <= 2023)

# Remove rows with years outside the range of 1990 to 2023 for cts_data
cts_data <- subset(cts_data, Year >= 1990 & Year <= 2023)
  

 
# Drop rows where columns 4 and 5 are all zero
gdp_data <- gdp %>%
  filter(!(gdp[, 4] == 0 | gdp[, 5] == 0))

# Drop column 6
gdp_data <- gdp_data %>% select(-6)
  

 

# Get unique values from the "Region" column in cts_data
unique_regions <- unique(cts_data$Region)

# Get unique values from the "Continent" column in gdp_data
# Remove "All" prefix from country names
cts_data$Country <- gsub("^All\\s+", "", cts_data$Country)

  

 

# Create a lookup table with country names, regions, and continents from cts_data
region_lookup <- cts_data %>%
  select(Country, Region) %>%
  distinct() %>%
  filter(!is.na(Region))

# Standardize the Region column by removing additional information within parentheses
region_lookup$Region <- gsub("\\s*\\(.*\\)", "", region_lookup$Region)

# Check for unique regions after standardization
unique(region_lookup$Region)

  

 

# Merge the GDP dataset with the region lookup table to update the continent names
gdp_data_filled <- gdp_data %>%
  left_join(region_lookup, by = c("Entity" = "Country")) %>%
  mutate(Continent = ifelse(is.na(Region), Continent, Region)) %>%
  select(-matches(".x$"), -Region) #%>%


# Find rows with missing or empty continent names
missing_continent <- gdp_data_filled[which(gdp_data_filled$Continent == ""), "Entity"]

# Iterate over each country with missing continent
for (country in missing_continent) {
  # Find a row with the same country name that has a continent name
  row_with_continent <- gdp_data_filled[gdp_data_filled$Entity == country & gdp_data_filled$Continent != "", ]
  if (nrow(row_with_continent) > 0) {
    # Copy the continent name from the found row
    gdp_data_filled[gdp_data_filled$Entity == country & gdp_data_filled$Continent == "", "Continent"] <- row_with_continent$Continent
  }
}

  




 
# Create a new dataset with rows where the "Continent" column is empty
gdp_data_empty_continent <- gdp_data_filled %>%
  filter(is.na(Continent) | Continent == "")


  

 

# Remove the rows with empty "Continent" column from gdp_data_filled
gdp_all_filled <- gdp_data_filled %>%
  filter(!is.na(Continent) & Continent != "")

  



 

# Remove rows where specified columns have a value of 0.000
age_data_filtered <- age_data %>%
  filter_at(vars(starts_with("Homicide.rate")), all_vars(. != 0))

  

 
# Print unique values for each column
unique_values <- sapply(cts_data[, c("Indicator", "Dimension", "Category", "Sex", "Age", "Year", "Unit of measurement")], unique)


  


 
# Merge latitude and longitude data with cts_data
merged_cts_data <- merge(cts_data, spatial_data, by.x = "Country", by.y = "country", all.x = TRUE)
# Remove the "country_code" column
merged_cts_data <- merged_cts_data[, !(names(merged_cts_data) %in% c("country_code"))]


# Print countries without longitude and latitude information
cts_without_coords <- merged_cts_data[is.na(merged_cts_data$latitude) | is.na(merged_cts_data$longitude), "Country"]



# Filter merged dataset to include only countries with longitude and latitude information
cts_location_data <- merged_cts_data[!is.na(merged_cts_data$longitude) & !is.na(merged_cts_data$latitude), ]

# Remove the "country_code" column
cts_location_data <- cts_location_data[, !(names(cts_location_data) %in% c("country_code"))]



  


 
# Remove rows with NA values in both specified columns
gdp_all_filled <- gdp_all_filled %>%
  filter(!is.na(`Homicide.rate.per.100.000.population...Both.sexes...All.ages`) &
           !is.na(`GDP.per.capita..PPP..constant.2017.international...`))


str(gdp_all_filled)

  



 
# Prepare data, removing years with no data for the bar chart
available_years <- cts_data %>%
  filter(!is.na(Sex), Age != "Total", Age != "Unknown", Sex != "Total") %>%
  distinct(Year) %>%
  pull(Year)

# Filter GDP data for years 2020-2022 for the bubble chart
gdp_filtered <- gdp_all_filled %>%
  filter(Continent != "World")

# Filter entities to include those with "income countries" or "World"
income_groups <- unique(gdp$Entity[grepl("income countries|World", gdp$Entity)])

# Define GDP ranges programmatically
gdp_ranges <- lapply(income_groups, function(group) {
  group_data <- gdp[gdp$Entity == group, ]
  min_gdp <- min(group_data$GDP.per.capita..PPP..constant.2017.international..., na.rm = TRUE)
  max_gdp <- max(group_data$GDP.per.capita..PPP..constant.2017.international..., na.rm = TRUE)
  range <- c(min_gdp, max_gdp)
  return(range)
})

# Combine the income groups and their respective GDP ranges into a named list
gdp_ranges <- setNames(gdp_ranges, income_groups)

# Filter location data for map chart
cts_filtered <- cts_location_data %>% filter(Dimension != "Total")

# Extract unique categories from your data
unique_categories <- unique(cts_location_data$Category)

# Generate a color palette based on the number of unique categories
color_palette <- colorRampPalette(brewer.pal(12, "Paired"))

# Assign colors to categories using the generated palette
category_colors <- setNames(color_palette(length(unique_categories)), unique_categories)
  



 


# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(HTML("
      .description, .introduction, p {
        font-size: 16px;
        text-align: justify;
      }
      .section {
        background-color: #f5f5f5; /* Light gray */
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 5px;
      }
    "))
  ),
  div(style = "text-align: center;",
      titlePanel("Understanding Global Homicide Dynamics"),
      h2("Introduction", class = "section")
  ),
  
  div(class = "description section",
      tags$ul(
        tags$li("The Understanding Global Homicide Dynamics page delves into the multifaceted issue of intentional homicides worldwide, offering insights into the complex dynamics and underlying factors driving homicide rates. Through comprehensive analysis of datasets on intentional homicide victims, homicide rates, and socioeconomic indicators, this page aims to uncover patterns, trends, and correlations that shed light on the context and drivers of homicide."),
        tags$li("Our primary goal is to address pressing questions surrounding the correlation between homicide rates and various demographic and socioeconomic factors. Specifically, we seek to explore how demographic factors such as age, gender, and citizenship status, alongside economic indicators like GDP per capita, correlate with homicide rates across different regions and over time."),
        tags$li("Additionally, we aim to investigate the prevalence of different situational contexts of homicides, such as interpersonal, organized criminal, socio-political, and terrorist-related, and analyze variations across regions and continents.")
      )
  ),
  
  
  fluidRow(
    column(8,
           plotlyOutput("ageGenderPlot")
    ),
    column(4, 
           div(class = "description section",
               p(HTML("<b>Description:</b>"), class = "description"),
               tags$ul(
                 tags$li("This bar chart shows homicide rates by age and gender across the selected year range."),
                 tags$li("Users can adjust the year range using the slider below."),
                 tags$li("It aids in understanding the distribution of homicide rates across different age groups and genders over time, identifying age-specific vulnerabilities, gender disparities, and temporal trends.")
               ),
               sliderInput("yearRange", "Select Year Range:",
                           min = min(available_years),
                           max = max(available_years),
                           value = c(min(available_years), max(available_years)),
                           step = 1,
                           sep = "")
           )
    )
  ),
  
  fluidRow(
    column(4, 
           div(class = "description section",
               p(HTML("<b>Description:</b>"), class = "description"),
               tags$ul(
                 tags$li("The scatter plot helps us understand the association between economic prosperity, as measured by GDP per capita, and homicide rates across various continents."),
                 tags$li("By analyzing this relationship over time, we can gain insights into how changes in economic conditions may influence levels of violence and inform strategies for violence prevention and socio-economic development."),
                 tags$li("Users can play through the years to observe trends over time or select a year from the slider.")
               )
           ),
           p("Press Play or select at year from slider"),
           sliderInput("year","Year:",
                       min = min(gdp_filtered$Year),
                       max = max(gdp_filtered$Year),
                       value = min(gdp_filtered$Year),
                       step = 1,
                       sep = ""),
           actionButton("play", "Play")
           
    ),
    column(8, 
           plotOutput("scatterPlot")
    )
  ),
  
  fluidRow(
    column(8, 
           plotlyOutput("gdpPlot")
    ),
    column(4, 
           div(class = "description section",
               p(HTML("<b>Description:</b>"), class = "description"),
               tags$ul(
                 tags$li("This line chart displays trends of GDP per capita and homicide rates for selected income groups over time."),
                 tags$li("The chart provides insights into the relationship between economic prosperity, measured by GDP per capita, and homicide rates across different income groups. Analyzing these trends can help identify potential correlations between economic conditions and levels of violence, informing policy decisions and interventions aimed at reducing homicide rates and promoting socio-economic development."),
                 tags$li("Users can choose a specific income group from the dropdown menu and update the chart accordingly.")
               ),
               selectInput("gdp_range", "Select GDP Range:", 
                           choices = c("High-income countries", 
                                       "Upper-middle-income countries", 
                                       "Middle-income countries",
                                       "Lower-middle-income countries",
                                       "Low-income countries"))
           )
    )),
  
  fluidRow(
    column(12, 
           plotlyOutput("bubblePlot"),
           p(" Click on Bubble of Dimension to See categories Visualisation")
    )
  ),
  
  fluidRow(
    column(6, 
           plotlyOutput("areaChart")
    ),
    column(6, 
           leafletOutput("mapPlot")
    )
  ),
  
  fluidRow(
    column(12, 
           div(class = "description section",
               p(HTML("<b>Description:</b>"), class = "description"),   
               tags$ul(
                 tags$li("Clicking on any dimension in the bubble chart opens the corresponding area chart and map. The area chart dynamically displays the trends of homicides categorized by different categories within the selected dimension over time."),
                 tags$li("Simultaneously, the map provides a spatial representation of the total homicides for each country or region within the selected dimension and the category which gives the maximum impact."),
                 tags$li("These visualizations update based on user interaction with the bubble chart, enabling a deeper exploration of homicide dynamics across various dimensions and geographical locations. If we want to see selected categories we can do that by clicking on the category name in legend")
               ))
    )),
  
  h2(actionLink("toggleAbout", "About the Data"), class = "section"),
  p("Click 'about the data' to see/hide detailed information about the data sources, methodology, and any relevant notes regarding the data used in this dashboard."),
  hidden(
    div(id = "aboutData",
        p("The data is sourced from multiple datasets, including:", class = "description section"),
        tags$ul(
          tags$li("UNODC Intentional Homicide Victims Dataset: This dataset comprises 113,634 rows and 13 columns, providing insights into intentional homicide victims globally. It includes attributes such as Iso3_code, Country, Region, Year, and VALUE (count of victims), as well as categorical information on situational context, relationship to the perpetrator, and citizenship.", class = "description section"),
          tags$li("Our World in Data - Homicide Rate and Economic Indicators Dataset: With 58,654 rows and 7 columns, this dataset covers homicide rates per 100,000 population, GDP per capita, population estimates, and continent information. It spans the years 1990 to 2021 and includes attributes such as Entity, Code, Year, Homicide rate, GDP per capita, and Continent.", class = "description section"),
          tags$li("Our World in Data - Homicide Rate by Age Group Dataset: This dataset contains 4,585 rows and 9 columns, presenting homicide rates per 100,000 population across different age groups. Attributes include Entity, Year, and homicide rates for age brackets ranging from 0-4 years to 70+ years.", class = "description section"),
          tags$li("Additional Spatial Data: This dataset provides geographic coordinates for 297 countries, including country codes and names, latitude, and longitude values.", class = "description section")
        )
    )
  )
)

  




 

# Define server
server <- function(input, output, session) {
  
  observeEvent(input$toggleAbout, {
    toggle("aboutData")
  })
  
  # Filter age and gender data based on user input year range
  filtered_age_gender_data <- reactive({
    cts_data %>%
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2],
             !is.na(Sex), Age != "Total", Age != "Unknown", Sex != "Total") %>%
      group_by(Age, Sex) %>%
      summarise(HomicideRate = mean(VALUE, na.rm = TRUE)) %>%
      ungroup()
  })
  
  # Plot for Homicide Rates by Age and Gender
  output$ageGenderPlot <- renderPlotly({
    data <- filtered_age_gender_data()
    plot_ly(data, x = ~Age, y = ~HomicideRate, type = 'bar', color = ~Sex, colors = "Set2",
            text = ~paste("Age:", Age, "<br>Sex:", Sex, "<br>Homicide Rate:", HomicideRate),
            hoverinfo = 'text') %>%
      layout(title = "Homicide Rates by Age and Gender",
             xaxis = list(title = "Age Group", tickangle = 45),
             yaxis = list(title = "Homicide Rate per 100,000"),
             barmode = 'group')
  })
  
  # Reactive expression for filtering data based on selected year for scatter plot
  filtered_data <- reactive({
    gdp_filtered %>%
      filter(Year == input$year)
  })
  
  # Scatter plot function with regression line and modified legend
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = `GDP.per.capita..PPP..constant.2017.international...`,
                                y = `Homicide.rate.per.100.000.population...Both.sexes...All.ages`,
                                color = Continent,
                                label = Entity)) +
      geom_point(aes(size = `GDP.per.capita..PPP..constant.2017.international...`), alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      scale_size_continuous(range = c(2, 20), guide = "none") +
      scale_color_discrete(name = "Continent") +
      labs(title = "Scatter Plot of GDP per Capita vs. Homicide Rate",
           x = "GDP per Capita (PPP, constant 2017 international $)",
           y = "Homicide Rate (per 100,000 population)",
           caption = "Bubble size represents GDP per capita; Color represents Continent") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Reactive value to keep track of play state
  play_state <- reactiveVal(FALSE)
  
  # Automatic year update
  observeEvent(input$play, {
    play_state(!play_state())
    
    if (play_state()) {
      updateActionButton(session, "play", label = "Stop")
      year_values <- unique(gdp_filtered$Year)
      i <- 1
      autoUpdate <- reactiveTimer(500)
      observe({
        autoUpdate()
        if (play_state()) {
          updateSliderInput(session, "year", value = year_values[i])
          i <<- i + 1
          if (i > length(year_values)) {
            i <<- 1
          }
        }
      })
    } else {
      updateActionButton(session, "play", label = "Play")
    }
  })
  
  # Filter GDP data based on selected GDP range
  filtered_gdp_data <- reactive({
    range <- gdp_ranges[[input$gdp_range]]
    gdp_all_filled %>%
      filter(`GDP.per.capita..PPP..constant.2017.international...` >= range[1] &
               `GDP.per.capita..PPP..constant.2017.international...` <= range[2]) %>%
      group_by(Year) %>%
      summarize(
        `Homicide.rate.per.100.000.population...Both.sexes...All.ages` = mean(`Homicide.rate.per.100.000.population...Both.sexes...All.ages`, na.rm = TRUE),
        `GDP.per.capita..PPP..constant.2017.international...` = mean(`GDP.per.capita..PPP..constant.2017.international...`, na.rm = TRUE)
      )
  })
  
  # Create a time series plot for homicide rates and GDP
  output$gdpPlot <- renderPlotly({
    data <- filtered_gdp_data()
    
    plot_ly(data, x = ~Year) %>%
      add_lines(y = ~`Homicide.rate.per.100.000.population...Both.sexes...All.ages`, name = 'Homicide Rate', line = list(color = 'blue')) %>%
      add_lines(y = ~`GDP.per.capita..PPP..constant.2017.international...` / 1000, name = 'GDP per Capita (scaled)', line = list(color = 'red', dash = 'dash')) %>%
      layout(title = paste("GDP and Homicide Rate Trends for", input$gdp_range),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Homicide Rate"),
             yaxis2 = list(title = "GDP per Capita (scaled, in thousands)", overlaying = "y", side = "right"),
             hovermode = 'x')
  })
  
  # Render bubble chart
  output$bubblePlot <- renderPlotly({
    bubble_data <- cts_filtered %>%
      filter(Dimension != "Total") %>%
      group_by(Dimension) %>%
      summarize(Total_Homicides = sum(VALUE)) %>%
      arrange(desc(Total_Homicides))
    
    plot_ly(bubble_data, x = ~Dimension, y = ~Total_Homicides, size = ~Total_Homicides,
            type = 'scatter', mode = 'markers', marker = list(sizemode = "diameter"),
            text = ~paste("Dimension:", Dimension, "<br>Total Homicides:", Total_Homicides),
            hoverinfo = 'text', source = "bubble_click") %>%
      layout(title = "Homicides by Dimension",
             xaxis = list(title = "Dimension"),
             yaxis = list(title = "Total Homicides"),
             hovermode = 'closest')
  })
  
  # Render area chart based on bubble click
  observeEvent(event_data("plotly_click", source = "bubble_click"), {
    clicked_dim <- event_data("plotly_click", source = "bubble_click")$x
    
    area_data <- cts_filtered %>%
      filter(Dimension == clicked_dim) %>%
      group_by(Category, Year) %>%
      summarize(Total_Homicides = sum(VALUE)) %>%
      arrange(Year)
    
    output$areaChart <- renderPlotly({
      plot_ly(area_data, x = ~Year, y = ~Total_Homicides, color = ~Category,
              type = 'scatter', mode = 'lines', fill = 'tozeroy',
              text = ~paste("Year:", Year, "<br>Total Homicides:", Total_Homicides, "<br>Category:", Category),
              hoverinfo = 'text', colors = category_colors) %>%
        layout(title = paste("Homicides by Category for", clicked_dim),
               xaxis = list(title = "Year"),
               yaxis = list(title = "Total Homicides"),
               hovermode = 'closest',
               showlegend = TRUE,
               legend = list(orientation = "h", x = 0.5, y = -0.25, xanchor = "center", yanchor = "top"),
               margin = list(b = 100)) %>%
        add_annotations(
          text = "Category",
          x = 0.5,
          y = -0.85,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "center",
          yanchor = "top"
        )
    })
    
    
    # Render map based on bubble click
    map_data <- cts_location_data %>%
      filter(Dimension == clicked_dim) %>%
      group_by(Country, Category, Subregion) %>%
      summarize(Total_Homicides = sum(VALUE))
    
    # Find the category with the highest homicide value for each country
    max_homicides <- map_data %>%
      group_by(Subregion) %>%
      slice_max(Total_Homicides) %>%
      ungroup()
    
    # Load the world shapefile data
    world <- st_read("world-administrative-boundaries.geojson")  
    
    # Rename 'region' column to 'Subregion' in the 'world' dataset
    world <- world %>% rename(Subregion = region)
    
    # Filter out regions present in 'world' but not in 'cts_location_data'
    world <- world %>% filter(Subregion %in% cts_location_data$Subregion)
    
    # Filter out regions present in 'cts_location_data' but not in 'world'
    max_homicides <- max_homicides %>% filter(Subregion %in% world$Subregion)
    
    # Merge shapefile data with the homicide data
    world <- left_join(world, max_homicides, by = "Subregion")
    
    # Ensure 'Category' is a factor to match with colors
    world$Category <- factor(world$Category, levels = unique(cts_location_data$Category))
    
    # Filter out rows with NA values in the 'Category' column
    world <- world[!is.na(world$Category), ]
    
    # Define the color palette
    pal <- colorFactor(palette = category_colors, domain = unique(world$Category))
    
    output$mapPlot <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(
          data = world,
          fillColor = ~pal(Category),
          weight = 1,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          popup = ~paste(Country, ": ", Total_Homicides, " homicides, Category: ", Category)
        ) %>%
        setView(lng = 0, lat = 0, zoom = 2)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)


  

