library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(rgdal)
library(sp)
library(DT)
library(htmltools)
library(raster)

#reads the csv file optinal you could also connect to a postgres database and pull the data from it
data <- read.csv("data/complete_data.csv")
#reads the shapefile file that is important for the representation of the country borders
world <- readOGR("data/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp")
world <- subset(world, is.element(world$ISO3, data$country_code))

#Here are the bins defined for coloring the maps .
bins_hdi <- c(125, 250, 375, 500, 625, 750, 875, 1000)
pal_hdi <- colorBin("RdYlGn", domain = c(100, 1000), bins = bins_hdi)

bins_gdp <- c(0, 10, 50, 100, 200, 500, 750, 3000, 22000)
pal_gdp <- colorBin("RdYlBu", domain = c(0, 22000), bins = bins_gdp)

bins_co2 <- c(0, 1, 5, 50, 100, 1000, 5000 ,10000)
pal_co2 <- colorBin("Reds", domain = c(0, 10000), bins = bins_co2)

bins_population <- c(0, 1, 5, 10, 50, 100, 1000, 1500)
pal_population <- colorBin("Blues", domain = c(0, 1500), bins = bins_population)

#World data are derived
world_data <- data %>% 
    dplyr::select(hdi_value, year, gdp_value, co2_emissions, population_count) %>%
    group_by(year) %>% 
    summarise(median_hdi = median(hdi_value, na.rm = TRUE),
              mean_gdp = mean(gdp_value, na.rm = TRUE),
              mean_co2 = mean(co2_emissions, na.rm = TRUE),
              world_population = mean(population_count, na.rm = TRUE),
              country_name = "World")



# Define UI for application 
ui <- dashboardPage(

    skin = "black",
    dashboardHeader(title = "Country Data"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("HDI Map", tabName = "hdi"),
            menuItem("GDP Map", tabName = "gdp"),
            menuItem("CO2 Map", tabName = "co2"),
            menuItem("Population Map", tabName = "population"),
            menuItem("Analysis", tabName = "analysis"),
            menuItem("Rankings", tabName = "ranking",
                     menuSubItem("Top 10", tabName = "top10"),
                     menuSubItem(sliderInput("year_ranking", label = "Select Year: ", min = 1960, max = 2019, 
                                             value = 1990, step = 1, sep = "")))
        )
    ),
   dashboardBody(
       tabItems(
           tabItem(tabName = "hdi",
                   fluidRow(
                       box(width = 12, leafletOutput(outputId = "hdi_map"))),
                   fluidRow(
                       box(
                           plotOutput("hdi_plot")
                       ),
                       box(
                           sliderInput("year_hdi", label = "Select Year: ", min = 1990, max = 2019, 
                                       value = 2005, step = 1, sep = ""),
                           selectInput("country_hdi", "Choose a country:", choices = unique(data$country_name)),
                           h2("What Is the Human Development Index (HDI)?"),
                           "The Human Development Index (HDI) is a statistic composite index of life 
                           expectancy, education (mean years of schooling completed and expected years 
                           of schooling upon entering the education system), and per capita income 
                           indicators, which are used to rank countries into four tiers of human 
                           development. A country scores a higher HDI when the lifespan is higher, 
                           the education level is higher, and the gross national income GNI (PPP) 
                           per capita is higher.", tags$a(href="https://en.wikipedia.org/wiki/Human_Development_Index", "(Wikipedia)")
                       )
                   ) 
            ),
           tabItem(
               tabName = "gdp",
               fluidRow(
                   box(width = 12, leafletOutput(outputId = "gdp_map"))),
               fluidRow(
                   box(
                       plotOutput("gdp_plot")
                   ),
                   box(
                       sliderInput("year_gdp", label = "Select Year:", min = 1960, max = 2019, 
                                   value = 1990, step = 1, sep = ""),
                       selectInput("country_gdp", "Choose a country:", choices = unique(data$country_name)),
                       h2("What Is the Gross Domestic Product (GDP)?"),
                       "Gross domestic product (GDP) is a monetary measure of the market value of 
                       all the final goods and services produced in a specific time period. 
                       GDP (nominal) per capita does not, however, reflect differences in the cost 
                       of living and the inflation rates of the countries; therefore, using a basis of 
                       GDP per capita at purchasing power parity (PPP) may be more useful when comparing 
                       living standards between nations, while nominal GDP is more useful comparing national 
                       economies on the international market.[4] Total GDP can also be broken down into the 
                       contribution of each industry or sector of the economy.[5] The ratio of GDP to the 
                       total population of the region is the per capita GDP and the same is called Mean 
                       Standard of Living. GDP definitions are maintained by a number of national and 
                       international economic organizations. The Organisation for Economic Co-operation 
                       and Development (OECD) defines GDP as an aggregate measure of production equal to 
                       the sum of the gross values added of all resident and institutional 
                       units engaged in production and services (plus any taxes, and minus any 
                       subsidies, on products not included in the value of their outputs)",
                       tags$a(href="https://en.wikipedia.org/wiki/Gross_domestic_product", "(Wikipedia)")
                   )
               ) 
                   
            ),
           tabItem(
               tabName = "co2",
               fluidRow(
                   box(width = 12, leafletOutput(outputId = "co2_map"))),
               fluidRow(
                   box(
                       plotOutput("co2_plot")
                   ),
                   box(
                       sliderInput("year_co2", label = "Select Year: ", min = 1960, max = 2017, 
                                   value = 1990, step = 1, sep = ""),
                       selectInput("country_co2", "Choose a country:", choices = unique(data$country_name))
                   )
               ) 
           ),
           tabItem(
               tabName = "population",
               fluidRow(
                   box(width = 12, leafletOutput(outputId = "population_map"))),
               fluidRow(
                   box(
                       plotOutput("population_plot")
                   ),
                   box(
                       sliderInput("year_population", label = "Select Year: ", min = 1960, max = 2017, 
                                   value = 1990, step = 1, sep = ""),
                       selectInput("country_population", "Choose a country:", choices = unique(data$country_name))
                   )
               ) 
           ),
           tabItem(
               tabName = "analysis",
               fluidRow(
                   box(
                       selectInput("country", "Select a country", choices = unique(data$country_name), selected = "Germany", multiple = TRUE)
                   ),
                   tabBox(
                       title = "Graphs",
                       id = "analysis_plots",
                       tabPanel("HDI",
                                plotOutput("sum_plot_hdi")),
                       tabPanel("GDP",
                                plotOutput("sum_plot_gdp")),
                       tabPanel("CO2",
                                plotOutput("sum_plot_co2")),
                       tabPanel("Population",
                                plotOutput("sum_plot_population"))
                   )
               ),
               fluidRow(
                   box(width = 12, dataTableOutput(outputId = "summary_table"))
               )
           ),
           tabItem(
               tabName = "top10",
               fluidRow(
                   box(
                       plotOutput("top10_hdi")
                   ),
                   box(
                       plotOutput("top10_gdp")
                   )
               ),
               fluidRow(
                   box(
                       plotOutput("top10_population")
                   ),
                   box(
                       plotOutput("top10_co2")
                   )
               )
           )
       )
       
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data_hdi <- reactive({
        data %>% 
            filter(year == input$year_hdi)
    })
    
    labels_hdi <- reactive({
        sprintf("<strong>%s</strong><br/><div> HDI rank: %g <br/> HDI value: %s </div>",
                data_hdi()$country_name, data_hdi()$hdi_rank, data_hdi()$hdi_value) %>% 
            lapply(htmltools::HTML)
    })
    
    #HDI Map
    output$hdi_map <- renderLeaflet(
        leaflet() %>%
            addProviderTiles(providers$Stamen.Toner) %>% 
            setView(lng = 10, lat = 0, zoom = 2) %>% 
            addPolygons(data = world,
                        color = "#666",
                        weight = 1,
                        smoothFactor = 0.5, 
                        fillOpacity = 0.7,
                        fillColor = ~pal_hdi(data_hdi()$hdi_value),
                        highlightOptions = highlightOptions(
                            weight = 4,
                            color = "black",
                            bringToFront = TRUE,
                            fillOpacity = 0.7
                        ),
                        label = labels_hdi()) %>% 
            addLegend(pal = pal_hdi,
                      values = data_hdi()$hdi_value,
                      opacity = 0.5,
                      position = "bottomright",
                      title = "HDI")
    )
    
    #Data gets filtered by country
    data_country_hdi <- reactive({
        data %>% 
            filter(country_name == input$country_hdi &
                       year >= "1990")
    })
    
    
    output$hdi_plot <- renderPlot({
        data_country_hdi() %>% 
            ggplot(aes(x = year, y = hdi_value)) +
            geom_line(color = "#3493e5") + 
            labs(title = paste("HDI", data_country_hdi()$country_name), x = "Year", y = "HDI value") +
            theme_bw()
    })
    
    #---------------------------------------------------------------------------
    #Data for the GDP panel is generated
    
    data_gdp <- reactive({
        data %>% 
            filter(year == input$year_gdp)
    })
    
    labels_gdp <- reactive({
        sprintf("<strong>%s</strong><br/><div> GDP: %g Mrd.</div>",
                data_gdp()$country_name, data_gdp()$gdp_value) %>% 
            lapply(htmltools::HTML)
    })
    
    #GDP Map
    output$gdp_map <- renderLeaflet(
        leaflet() %>%
            addProviderTiles(providers$Stamen.Toner) %>% 
            setView(lng = 10, lat = 0, zoom = 2) %>% 
            addPolygons(data = world,
                        color = "#666",
                        weight = 1,
                        smoothFactor = 0.5, 
                        fillOpacity = 0.7,
                        fillColor = ~pal_gdp(data_gdp()$gdp_value),
                        highlightOptions = highlightOptions(
                            weight = 4,
                            color = "black",
                            bringToFront = TRUE,
                            fillOpacity = 0.7
                        ),
                        label = labels_gdp()) %>% 
            addLegend(pal = pal_gdp,
                      values = data_gdp()$gdp_value,
                      opacity = 0.5,
                      position = "bottomright",
                      title = "GDP in Mrd.")
    )
    
    #Data gets filtered by country
    data_country_gdp <- reactive({
        data %>% 
            filter(country_name == input$country_gdp)
    })
    
    
    output$gdp_plot <- renderPlot({
        data_country_gdp() %>% 
            ggplot(aes(x = year, y = gdp_value)) +
            geom_line(color = "#3493e5") + 
            labs(title = paste("GDP", data_country_gdp()$country_name), x = "Year", y = "GDP in Mrd.") +
            theme_bw()
    })
       
    #---------------------------------------------------------------------------
    #Data for the CO2 panel is generated
    
    data_co2 <- reactive({
        data %>% 
            filter(year == input$year_co2)
    })
    
    labels_co2 <- reactive({
        sprintf("<strong>%s</strong><br/><div> CO<sub>2</sub> emissions: %g mt</div>",
                data_co2()$country_name, data_co2()$co2_emissions) %>% 
            lapply(htmltools::HTML)
    })
    
    #CO2 Map
    output$co2_map <- renderLeaflet(
        leaflet() %>%
            addProviderTiles(providers$Stamen.Toner) %>% 
            setView(lng = 10, lat = 0, zoom = 2) %>% 
            addPolygons(data = world,
                        color = "#666",
                        weight = 1,
                        smoothFactor = 0.5, 
                        fillOpacity = 0.7,
                        fillColor = ~pal_co2(data_co2()$co2_emissions),
                        highlightOptions = highlightOptions(
                            weight = 4,
                            color = "black",
                            bringToFront = TRUE,
                            fillOpacity = 0.7
                        ),
                        label = labels_co2()) %>% 
            addLegend(pal = pal_co2,
                      values = data_co2()$co2_emissions,
                      opacity = 0.5,
                      position = "bottomright",
                      title = "CO2 emission in mt:")
    )
    
    #Data gets filtered by country
    data_country_co2 <- reactive({
        data %>% 
            filter(country_name == input$country_co2)
    })
    
    
    output$co2_plot <- renderPlot({
        data_country_co2() %>% 
            ggplot(aes(x = year, y = co2_emissions)) +
            geom_line(color = "#3493e5") + 
            labs(title = paste("CO2 Emissions", data_country_co2()$country_name), x = "Year", y = "CO2 Emissions in mt") +
            theme_bw()
    })
    
    #---------------------------------------------------------------------------
    #Data for the Population panel is generated
    
    data_population <- reactive({
        data %>% 
            filter(year == input$year_population)
    })
    
    labels_population <- reactive({
        sprintf("<strong>%s</strong><br/><div> Population: %g Mio. <br/>
                Population growth: %s &#37</div>",
                data_population()$country_name, data_population()$population_count,
                data_population()$population_growth) %>% 
            lapply(htmltools::HTML)
    })
    
    #Population Map
    output$population_map <- renderLeaflet(
        leaflet() %>%
            addProviderTiles(providers$Stamen.Toner) %>% 
            setView(lng = 10, lat = 0, zoom = 2) %>% 
            addPolygons(data = world,
                        color = "#666",
                        weight = 1,
                        smoothFactor = 0.5, 
                        fillOpacity = 0.7,
                        fillColor = ~pal_population(data_population()$population_count),
                        highlightOptions = highlightOptions(
                            weight = 4,
                            color = "black",
                            bringToFront = TRUE,
                            fillOpacity = 0.7
                        ),
                        label = labels_population()) %>% 
            addLegend(pal = pal_population,
                      values = data_population()$population_count,
                      opacity = 0.5,
                      position = "bottomright",
                      title = "Population in Mio.")
    )
    
    #Data gets filtered by country
    data_country_population <- reactive({
        data %>% 
            filter(country_name == input$country_population)
    })
    
    
    output$population_plot <- renderPlot({
        data_country_population() %>% 
            ggplot(aes(x = year, y = population_count)) +
            geom_line(color = "#3493e5") + 
            labs(title = paste("Population in", data_country_population()$country_name), x = "Year", y = "Population in Mio.") +
            theme_bw()
    })
    
    #--------------------------------------------------
    #Data for the Analysis panel is generated
    
    country <- reactive(
        data %>% 
            filter(country_name == input$country)
    )
    
    output$summary_table <- renderDataTable(country())
    
    output$sum_plot_hdi <- renderPlot({
            ggplot(data = subset(country(), year >= 1990), aes(x = year, y = hdi_value, color = country_name)) +
            geom_line() +
            geom_line(data = subset(world_data, year >= 1990), aes(y = median_hdi)) +
            labs(x = "Year", y = "HDI Value", caption = "Notice: World is a median value") +
            theme(legend.title=element_blank())
    })
    
    output$sum_plot_gdp <- renderPlot({
        ggplot(data = country(), aes(x = year, y = gdp_value, color = country_name)) +
            geom_line() +
            geom_line(data = world_data, aes(y = mean_gdp)) +
            labs(x = "Year", y = "GDP in Mrd.", caption = "Notice: World is a mean value") +
            theme(legend.title=element_blank())
    })
    
    output$sum_plot_co2 <- renderPlot({
        ggplot(data = country(), aes(x = year, y = co2_emissions, color = country_name)) +
            geom_line() +
            geom_line(data = world_data, aes(y = mean_co2)) +
            labs(x = "Year", y = "CO2 in megatons (mt)", caption = "Notice: World is a mean value") +
            theme(legend.title=element_blank())
    })
    
    output$sum_plot_population <- renderPlot({
        ggplot(data = country(), aes(x = year, y = population_count, color = country_name)) +
            geom_line() +
            geom_line(data = subset(world_data, year < 2018), aes(y = world_population)) +
            labs(x = "Year", y = "Population in Mio.", caption = "Notice: World is a mean value") +
            theme(legend.title=element_blank())
    })
    
    #---------------------------------------------------------------------
    #Plots for the Ranking panel are created
    
    data_top10_gdp <- reactive({
        data %>% 
            filter(year == input$year_ranking) %>% 
            arrange(desc(gdp_value)) %>% 
            group_by(gdp_value) %>% 
            mutate(country_name = as.factor(country_name),
                   gdp_value = round(gdp_value/1000, 2)) %>% 
            head(n = 10)
    })
    
    output$top10_gdp <- renderPlot({
        ggplot(data = data_top10_gdp(), aes(x = country_name, y = gdp_value)) + 
            geom_bar(stat="identity", fill = "#3493e5") +
            geom_text(aes(label = gdp_value, hjust = 1.1), color = "white") +
            labs(title = paste("Top 10 countries by GDP in", input$year_ranking, ":"), y = "GDP in Billion") +
            coord_flip() +
            theme(axis.title.y=element_blank())
    })
    
    data_top10_hdi <- reactive({
        data %>% 
            filter(year == input$year_ranking) %>% 
            arrange(desc(hdi_value)) %>% 
            group_by(hdi_value) %>% 
            mutate(country_name = as.factor(country_name)) %>% 
            head(n = 10)
    })
    
    output$top10_hdi <- renderPlot({
        ggplot(data = data_top10_hdi(), aes(x = country_name, y = hdi_value)) + 
            geom_bar(stat="identity", fill = "#3493e5") +
            geom_text(aes(label = hdi_value, hjust = 1.1), color = "white") +
            labs(title = paste("Top 10 countries by HDI in", input$year_ranking, ":"), y = "HDI") +
            coord_flip() +
            theme(axis.title.y=element_blank())
    })
    
    data_top10_co2 <- reactive({
        data %>% 
            filter(year == input$year_ranking) %>% 
            arrange(desc(co2_emissions)) %>% 
            group_by(co2_emissions) %>% 
            mutate(country_name = as.factor(country_name)) %>% 
            head(n = 10)
    })
    
    output$top10_co2 <- renderPlot({
        ggplot(data = data_top10_co2(), aes(x = country_name, y = co2_emissions)) + 
            geom_bar(stat="identity", fill = "#3493e5") +
            geom_text(aes(label = co2_emissions, hjust = 1.1), color = "white") +
            labs(title = paste("Top 10 countries with the highest CO2 production in", input$year_ranking, ":"), y = "CO2 emmisions in megatons") +
            coord_flip() +
            theme(axis.title.y=element_blank())
    })
    
    data_top10_population <- reactive({
        data %>% 
            filter(year == input$year_ranking) %>% 
            arrange(desc(population_count)) %>% 
            group_by(population_count) %>% 
            mutate(country_name = as.factor(country_name)) %>% 
            head(n = 10)
    })
    
    output$top10_population <- renderPlot({
        ggplot(data = data_top10_population(), aes(x = country_name, y = population_count)) + 
            geom_bar(stat="identity", fill = "#3493e5") +
            geom_text(aes(label = population_count, hjust = 1.1), color = "white") +
            labs(title = paste("Top 10 most densely populated countries in", input$year_ranking, ":"), y = "Population in Mio.") +
            coord_flip() +
            theme(axis.title.y=element_blank())
    })
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
