# Poland Biodiversity Dashboard
Using Poland's biodiversity data from the [Global Biodiversity Information Facility](https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165), built a Shiny dashboard that visualizes observed species on the map. The boundary shapefile from [PUL Digital Maps & Geospatial Data](https://maps.princeton.edu/catalog/stanford-xh662zc5620) has been used to create the choropleth map.

![](https://github.com/avinashsajeevan/Poland-Biodiversity-Dashboard/blob/main/Screenshots/screencapture1.png)

![](https://github.com/avinashsajeevan/Poland-Biodiversity-Dashboard/blob/main/Screenshots/screencapture2.png)

## Check It Out
- Try a demo of the app on [shinyapps.io](https://avinash93.shinyapps.io/Poland_Biodiversity_Dashboard/)

## Running the app
- Required libraries:  
library(shiny)  
library(shinydashboard)  
library(tidyverse)  
library(lubridate)  
library(leaflet)  
library(plotly)  
library(scales)  
library(sf)  
library(janitor)  

- There are two tabs: Species Occurrence Map and Total Species Choropleth Map.  
Species Occurrence Map shows the Occurrence Map, Year count, Sex Percentage, locality count and locality count by year of specific Species.  
Total Species Choropleth Map shows the Poland's choropleth map of total species count boundaries seperated by 16 provinces, Total Occurrence of Species by Year and Kingdom Percentage of Total Species.  

- Start by selecting or entering the scientific or vernacular name of the species that we need to check in the 'Species Occurrence Map' tab. All the map and plots in the tab will update and shows the observations of the species we searched for.  
![](https://github.com/avinashsajeevan/Poland-Biodiversity-Dashboard/blob/main/Screenshots/screencapture3.png)
- Total Species Choropleth Map gives the overview of the observations of the total species we have the data for.

