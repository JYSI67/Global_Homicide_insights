# Global Homicide Dynamics

[View the live application here: Global Homicide Dynamics](https://globalhoimcidedynamics.shinyapps.io/Jyoti_Singh_33657149_code/)

We encourage you to explore the interactive visualizations and gain insights into global homicide trends using our live Shiny application.

## Project Overview

This Shiny application provides an interactive exploration of global homicide dynamics using various datasets. It offers a comprehensive view of intentional homicide trends, correlating them with socioeconomic factors and demographic information across different regions and time periods.

## Features

- **Age and Gender Analysis**: Visualizes homicide rates by age groups and gender, allowing users to observe demographic patterns in homicide victims.
  
- **GDP vs. Homicide Rate**: Explores the relationship between a country's economic status (GDP per capita) and its homicide rate, with an interactive scatter plot that evolves over time.
  
- **Income Group Trends**: Displays trends in GDP per capita and homicide rates for different income groups, providing insights into how economic development relates to violence levels.
  
- **Dimensional Analysis**: Offers a bubble chart representing homicides by various dimensions (e.g., situational context, relationship to perpetrator), allowing users to dive deeper into specific aspects of homicide data.
  
- **Geographic Visualization**: Presents a world map showing homicide rates and predominant categories across different regions, offering a spatial perspective on homicide distribution.

## Data Sources

- UNODC Intentional Homicide Victims Dataset
- Our World in Data - Homicide Rate and Economic Indicators Dataset
- Our World in Data - Homicide Rate by Age Group Dataset
- World Administrative Boundaries Geospatial Data

## Files in the Repository

- `app.R`: The main Shiny application file containing both UI and server logic.
- `world-administrative-boundaries.geojson`: Geospatial data for world map visualization.
- `world_country_and_usa_states_latitude_and_longitude_values.csv`: Geographical coordinates for mapping.
- `homicide-rate-vs-gdp-pc.csv`: Dataset containing homicide rates and GDP per capita information.
- `homicide-rate-by-age-of-the-victim_update.csv`: Age-specific homicide rate data.
- `data_cts_intentional_homicide.xlsx`: UNODC Intentional Homicide Victims Dataset.

## How to Run Locally

1. Clone this repository to your local machine.
2. Ensure you have R and the required packages installed (`shiny`, `dplyr`, `ggplot2`, `plotly`, etc.).
3. Open the `app.R` file in RStudio.
4. Click the "Run App" button in RStudio, or use the `runApp()` function in the R console.

## Insights and Findings

The application allows users to uncover various insights, such as:

- The relationship between economic development and homicide rates across different regions.
- Age and gender-specific vulnerabilities to homicide.
- Temporal trends in homicide rates and their correlation with GDP growth.
- Geographic hotspots of different types of homicides.

## Future Work

- Incorporate more recent data as it becomes available.
- Add predictive modeling capabilities to forecast future trends.
- Enhance the application with more detailed regional analysis features.

## Acknowledgments

This project utilizes data from the United Nations Office on Drugs and Crime (UNODC) and Our World in Data. We thank these organizations for making their data publicly available for research and analysis.

## Contact

For any queries or suggestions regarding this project, please open an issue in this GitHub repository.
