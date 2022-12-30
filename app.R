library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)
library(scales)
library(sf)
library(janitor)

# read biodiversity data of Poland
bio_occurrence <- read_csv("biodiversity-data_occurrence_PL.csv")

# clean data
bio_occurrence <-  bio_occurrence %>% 
  select(id, scientificName, taxonRank, kingdom, family, vernacularName, 
         individualCount, lifeStage, sex, longitudeDecimal, latitudeDecimal,
         locality, modified)

bio_occurrence <- bio_occurrence %>% 
  mutate(kingdom = if_else(is.na(kingdom), 'Undetermined',kingdom),
         vernacularName = if_else(is.na(vernacularName), 'Not Available',vernacularName),
         locality = str_remove(locality, ".*-"),
         family = str_replace_all(family, '_',' '),
         family = str_to_title(family),
         names = as.factor(paste(scientificName, "|", vernacularName)))

# dropdown choices
sv_names <- unique(bio_occurrence$names)

# colors for plot
pal <- colorFactor(c("navy", "red", "green", "yellow"),
                   domain = unique(bio_occurrence$kingdom))

mycols <- c("#93d500", "#68952d","#EFC000FF", "#CD534CFF")

# Define UI
ui <- dashboardPage(skin = "purple",
  
  dashboardHeader(title = "Biodiversity Dashboard", titleWidth = '250px'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Species Occurrence Map", tabName = "bio_occurrence"),
      menuItem("Total Species Choropleth Map", tabName = "bio_occurrence_province")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("bio_occurrence",
              fluidRow(
                box(status = 'info',
                  selectizeInput('Scientific_Vernacular_Name',
                                 label = 'Select or enter Scientific or Vernacular Name of the Species',
                                 choices = NULL,
                                 selected = NULL,
                                 options = list(placeholder = 'Enter Scientific or Vernacular Name')
                  )),
                box(status = 'info', htmlOutput("sv")),
                ),
              
              fluidRow(
                box(title = textOutput('nm1'), 
                    status = 'primary',
                    leafletOutput("bio_occurrence"),width = 7),
                box(title = textOutput('nm2'),
                    status = 'primary',
                    plotlyOutput("occurrence_timeline"),width = 5)
                ),
              
              fluidRow(
                box(title = textOutput('nm3'),
                    status = 'primary',
                    plotOutput("occurrence_sex"),width = 4),
                box(title = textOutput('nm4'),
                    status = 'primary',
                    plotlyOutput("occurrence_locality"),width = 4),
                box(title = textOutput('nm5'),
                    status = 'primary',
                    plotlyOutput("occurrence_yr_locality"),width = 4))
              ),
      
      tabItem("bio_occurrence_province",
              fluidRow(
                box(title = "Total Species Occurrence Choropleth Map", status = 'success',
                    leafletOutput("bio_occurrence_province"),width = 12)),
              
              fluidRow(
                box(title = "Total Occurrence of Species by Year", status = 'success',
                    plotlyOutput("total_occurrence_timeline")),
                box(title = "Kingdom Percentage of Total Species", status = 'success',
                    plotOutput("total_occurrence_kingdom")))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
    
  updateSelectizeInput(session, "Scientific_Vernacular_Name",
                       choices =  sv_names,
                       selected = 'Aix galericulata | Mandarin Duck',
                       server = TRUE)
  
  observeEvent(input$Scientific_Vernacular_Name,
               {
                 bio_occurrence_1 <- bio_occurrence %>% 
                   filter(names==input$Scientific_Vernacular_Name)%>% 
                   mutate(modified = dmy(modified),
                          yr = year(modified),
                          mon = month(modified))
                 
                 output$nm1 <-  renderText({ paste(str_to_title(unique(bio_occurrence_1$vernacularName)),
                                                  'Species Occurrence Map')})
                 output$nm2 <-  renderText({ paste('Occurrence of', str_to_title(unique(bio_occurrence_1$vernacularName)),
                                                   'by Year')})
                 output$nm3 <-  renderText({ paste('Sex Percentage of Observed',str_to_title(unique(bio_occurrence_1$vernacularName))
                                                   )})
                 output$nm4 <-  renderText({ paste('Occurrence of', str_to_title(unique(bio_occurrence_1$vernacularName)),
                                                   'by Locality')})
                 output$nm5 <-  renderText({ paste('Yearly Occurrence of', str_to_title(unique(bio_occurrence_1$vernacularName)),
                                                   'by Locality')})
                 
                 output$sv <-  renderText({ paste("<font color=\"#648B1A\"><b>","Total count:",
                                                  "<font color=\"#000000\"><b>",sum(bio_occurrence_1$individualCount),"<br>",
                                                  "<font color=\"#648B1A\"><b>","Scientific Name:",
                                                  "<font color=\"#000000\"><b>",unique(bio_occurrence_1$scientificName),"<br>",
                                                  "<font color=\"#648B1A\"><b>","Vernacular Name:",
                                                  "<font color=\"#000000\"><b>",unique(bio_occurrence_1$vernacularName),"<br>",
                                                  "<font color=\"#648B1A\"><b>","Kingdom:",
                                                  "<font color=\"#000000\"><b>",unique(bio_occurrence_1$kingdom),"<br>",
                                                  "<font color=\"#648B1A\"><b>","Family:",
                                                  "<font color=\"#000000\"><b>",unique(bio_occurrence_1$family),"<br>",
                                                  "<font color=\"#648B1A\"><b>","Observed Timeline:",
                                                  "<font color=\"#000000\"><b>",min(bio_occurrence_1$yr),'-',
                                                  max(bio_occurrence_1$yr)
                                                  )})
                 
               }
  )
  
  output$occurrence_timeline <- renderPlotly({
    
    ggplotly(bio_occurrence %>% 
      filter(names==input$Scientific_Vernacular_Name) %>% 
      mutate(modified = dmy(modified),
             yr = as.factor(year(modified)),
             mon = month(modified)) %>% 
      group_by(names, yr) %>% 
      summarise(Count = sum(individualCount)) %>%
      ungroup() %>% 
      ggplot() +
      geom_col(aes(yr,Count,fill=yr))+
      xlab("Year") + ylab("Count")+ 
      theme_minimal() +
      theme(
        legend.position='none',
        plot.title = element_text(color = '#648B1A', face = 'bold'),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(vjust = -2),
        panel.border = element_rect(fill=NA,
                                    size = .2)
      ),tooltip="y") %>% 
      layout(
        yaxis = list(ticks="outside", 
                     tickwidth=2, 
                     tickcolor='crimson', 
                     ticklen=5, col=1)
      ) 
  })
    
  output$occurrence_locality <- renderPlotly({
    
    ggplotly(bio_occurrence %>% 
      filter(names==input$Scientific_Vernacular_Name) %>% 
      mutate(modified = dmy(modified),
             yr = as.factor(year(modified)),
             mon = month(modified)) %>% 
      group_by(names, Locality = locality) %>% 
      summarise(Count = sum(individualCount)) %>%
      ungroup() %>%
      ggplot() +
      geom_col(aes(Locality, Count, fill=Locality))+
      xlab("Locality") + ylab("Count")+ 
      theme_minimal() +
      theme(
        legend.position='none',
        plot.title = element_text(color = '#648B1A', face = 'bold'),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45),
        axis.text.y = element_text(hjust = 1),
        panel.border = element_rect(fill=NA,
                                    size = .2)
      ),tooltip="y") %>% 
      layout(
        yaxis = list(ticks="outside", 
                     tickwidth=2, 
                     tickcolor='crimson', 
                     ticklen=5, col=1)
      ) 
  })
  
  output$occurrence_sex <- renderPlot({
    
    bio_occurrence %>% 
      filter(names==input$Scientific_Vernacular_Name) %>% 
      count(names, sex) %>% 
      rename(Count = n) %>% 
      mutate(Percentage = Count/sum(Count)) %>% 
      select(sex, Percentage) %>% 
      ggplot(aes(x="", y=Percentage, fill= sex)) +
      geom_bar(stat="identity", width=1, color="yellow") +
      coord_polar("y", start=0) +
      geom_text(aes(x = 1,label = percent(Percentage)),
                position = position_stack(vjust = 0.5),
                color = "black", size = 5)+
      scale_fill_manual(values = mycols)+ 
      theme_minimal() +
      theme(plot.title = element_text(color = '#648B1A', face = 'bold', size = 19),
            axis.text.x=element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.text = element_text(size=15),
            legend.title = element_text(size=20))
    
  })

  output$occurrence_yr_locality <- renderPlotly({
    
    
    t <- list(
      family = "Arial",
      color = "#648B1A",
      size = 19,
      face = 'bold')
    
    
    bio_occurrence_1 <- bio_occurrence %>% 
      filter(names==input$Scientific_Vernacular_Name) %>% 
      mutate(modified = dmy(modified),
             yr = as.factor(year(modified)),
             mon = month(modified)) %>% 
      group_by(names, Locality = str_trim(locality), yr) %>% 
      summarise(Count = sum(individualCount)) %>%
      ungroup()
    
    bio_occurrence_1 %>% 
      plot_ly(x = ~Locality,
              y = ~Count,
              frame = ~yr) %>% 
      add_bars() %>% 
      layout(
                     
        showlegend = F,
        yaxis = list(ticks="outside", 
                     tickwidth=2, 
                     tickcolor='crimson', 
                     ticklen=5, col=1,#rangemode = "tozero",
                     range = c(0,max(bio_occurrence_1$Count)))
      ) 
    
  })
  
  output$bio_occurrence <- renderLeaflet({
    
    bio_occurrence_1 <- bio_occurrence %>% 
      filter(names==input$Scientific_Vernacular_Name)%>% 
      mutate(modified = dmy(modified),
             yr = year(modified),
             mon = month(modified))
    
    
    leaflet(bio_occurrence_1)  %>%
      addTiles() %>%
      addMarkers(lng = ~longitudeDecimal,
                 lat = ~latitudeDecimal,
                 popup = paste("Count:", bio_occurrence_1$individualCount,"<br>",
                               "Scientific Name:", bio_occurrence_1$scientificName,"<br>",
                               "Vernacular Name:", bio_occurrence_1$vernacularName,"<br>",
                               "Locality:", bio_occurrence_1$locality,"<br>",
                               "Sex:", bio_occurrence_1$sex,"<br>",
                               "Year:", bio_occurrence_1$yr)
                 )
    
    
  })

  output$bio_occurrence_province <- renderLeaflet({
    
    sp <- sf::st_read(dsn = ".", layer = "POL_adm2")
    
    sp_1 <- sp %>% 
      group_by(NAME_1) %>%
      summarise(geometry = sf::st_union(geometry)) %>%
      ungroup() 
    
    
    df = bio_occurrence %>% 
      transmute(lon = longitudeDecimal,lat = latitudeDecimal)
    
    projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    df <- st_as_sf(x = df,                         
                   coords = c("lon", "lat"),
                   crs = projcrs)
    
    sp_2 <- st_join(df,sp_1, join = st_nearest_feature, left = T)
    
    province_coord <- sp_2 %>%
      mutate(lon = st_coordinates(.)[,1],
             lat = st_coordinates(.)[,2]) %>% 
      as.data.frame() %>% 
      select(-geometry)
    
    
    
    bio_occurrence_1 <- bio_occurrence %>% 
      bind_cols(province_coord$NAME_1) %>% 
      rename(province = `...15`)
    
    province_count_1 <- bio_occurrence_1 %>%
      group_by(province) %>%
      summarise(species_count = sum(individualCount))
    
    province_count_2 <- bio_occurrence_1 %>% 
      group_by(province, kingdom) %>% 
      summarise(species_count = sum(individualCount)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = kingdom,values_from =species_count) %>%
      # adorn_totals("col", fill = "species_count")%>%
      adorn_percentages() %>% 
      mutate_if(is.numeric, percent) %>% 
      left_join(province_count_1) %>% 
      mutate_if(is.character, ~replace_na(.," "))
    
    ##
    province_count_sf <- sp_1 %>% 
      left_join(province_count_2, by = c('NAME_1'= 'province'))
    
    pal <- colorQuantile("Blues", province_count_sf$species_count, n = 7)
    labels <- sprintf(
      "<string> Province: %s</string><br/>
      Occurrence Count: %g<br/>
      Animalia: %s<br/>
      Fungi: %s<br/>
      Plantae: %s<br/>
      Undetermined: %s<br/>",
      
      province_count_sf$NAME_1,
      province_count_sf$species_count,
      province_count_sf$Animalia,
      province_count_sf$Fungi,
      province_count_sf$Plantae,
      province_count_sf$Undetermined
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(province_count_sf)%>%
      setView(19.1451,51.9194,6) %>% 
      addTiles() %>% 
      addPolygons(
        fillColor = ~pal(species_count),
        weight = 2,
        opacity =1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight"="normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>% 
      addLegend( pal = pal, values = ~species_count, opacity = 0.7, title = "Total Occurrence",
                 position = "bottomright"
      )
    
    
    
    
    
  })

  output$total_occurrence_timeline <- renderPlotly({
    
    ggplotly(bio_occurrence %>%
               mutate(modified = dmy(modified),
                      yr = as.factor(year(modified)),
                      mon = month(modified)) %>% 
               group_by(yr) %>% 
               summarise(Count = sum(individualCount)) %>%
               ungroup() %>% 
               ggplot() +
               geom_col(aes(yr,Count,fill=yr))+
               xlab("Year") + ylab("Count")+ 
               theme_minimal() +
               theme(
                 legend.position='none',
                 plot.title = element_text(color = '#648B1A', face = 'bold'),
                 panel.grid = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 axis.text.x = element_text(vjust = -2),
                 panel.border = element_rect(fill=NA,
                                             size = .2)
               ),tooltip="y") %>% 
      layout(
        yaxis = list(ticks="outside", 
                     tickwidth=2, 
                     tickcolor='crimson', 
                     ticklen=5, col=1)
      ) 
    
  })
 
  output$total_occurrence_kingdom <- renderPlot({
    
    bio_occurrence%>% 
      group_by(kingdom) %>% 
      summarise(Count = sum(individualCount)) %>%
      ungroup() %>%  
      mutate(Percentage = Count/sum(Count)) %>% 
      select(kingdom, Percentage) %>% 
      ggplot(aes(x="", y=Percentage, fill= kingdom)) +
      geom_bar(stat="identity", width=1, color="yellow") +
      coord_polar("y", start=0) +
      geom_text(aes(x = 1,label = percent(Percentage)),
                position = position_stack(vjust = 0.5),
                color = "black", size = 5)+
      scale_fill_manual(values = mycols) +
      theme_minimal() +
      theme(plot.title = element_text(color = '#648B1A', face = 'bold', size = 19),
            axis.text.x=element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.text = element_text(size=15),
            legend.title = element_text(size=20))
    
  })

}
                 

# Run the application 
shinyApp(ui = ui, server = server)