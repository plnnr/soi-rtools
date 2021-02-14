library(shiny)
library(tidyverse)
library(tigris)
library(sf)
library(scales)
library(shinythemes)
library(lwgeom)
library(maps)

options(
    scipen = 999,
    digits = 4,
    tigris_class = "sf",
    tigris_use_cache = T
)

source("utils.R")



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("simplex"), ##(lumen, paper, simplex, flatly, yeti)
    # Application title
    titlePanel("Net Migration by Year"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "msa_name", choices = msa_shortname$cbsa_shortname, 
                        label = "Select Region:", selected = "Portland, OR-WA",
                        multiple = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("netMigrationPlot"),
           plotOutput("netMigrationMap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    cbsafips <- reactive({
        filter(msa_shortname, cbsa_shortname == input$msa_name) %>% pull(cbsa13)
    })
    
    data2graph <- reactive({
        region_net_summarized_migration(soi_migration_data, cbsafips()) 
        })

    output$netMigrationPlot <- renderPlot({
        validate(
            need(input$msa_name %in% msa_shortname$cbsa_shortname, "Please select a valid region to graph.")
        )
        
        ##### Plot net migration and inflow migration ######
        data2graph() %>%
            filter_for_major_metros(topn = 20) %>%
            graph_net_by_year() +
            scale_y_continuous(labels = scales::comma_format()) +
            labs(x = "Year", y = "Total People (Exemptions)",
                 title = paste0("Net Regional Migration in ", input$msa_name),
                 subtitle = paste0("Top 20 Origin and Destination Regions of Residents of ", input$msa_name, ", 2011-2018"),
                 caption = "Note: Population is approximated using the number of exemptions claimed on income-earners' taxes.\nPositive values mean net migration into selected region.\nNegative values mean net migration out of selected region.\n\nSource: U.S. Internal Revenue Service (IRS), Statistics of Income (SOI) Migration Data.\n")
    })
    
    output$netMigrationMap <- renderPlot({
        
        validate(
            need(input$msa_name %in% msa_shortname$cbsa_shortname, "Please select a valid region to graph.")
        )
        
        selected_region_coord <- filter(msas, GEOID == cbsafips()) %>%
            st_centroid() %>% 
            sfc_as_cols(names = c("reg_lng", "reg_lat")) %>% 
            st_drop_geometry() %>%
            select(reg_lng, reg_lat) 
        
        data2map <- data2graph() %>%
            filter(y2 >= 2016) %>%
            filter_for_major_metros(topn = 10) %>%
            group_by(cbsa, cbsaname15) %>% 
            summarize(n2.in = sum(n2.in, na.rm = T),
                      n2.out = sum(n2.out, na.rm = T),
                      n2.net = n2.in - n2.out,
                      n2.throughput = n2.in + n2.out) %>%
            filter(n2.net > 5 | n2.net < -5) %>%
            inner_join(., select(msas, GEOID), by = c("cbsa" = "GEOID")) %>%
            inner_join(., msa_shortname, by = c("cbsa" = "cbsa13")) %>%
            st_as_sf() %>% st_centroid(of_largest_polygon = TRUE) %>%
            sfc_as_cols(names = c("region_lat", "region_lng")) %>%
            mutate(reg_lng = selected_region_coord$reg_lng,
                   reg_lat = selected_region_coord$reg_lat)
        
        usMap <- borders("state", colour="grey", fill="white")

        ggplot() +
            usMap +
            geom_curve(data = data2map,
                       aes(y = reg_lat, x = reg_lng, 
                           yend = region_lng, xend = region_lat, 
                           size = n2.throughput, col = n2.net),
                       alpha = 0.75,
                       curvature=0.15) +
            scale_color_distiller("Net In-Migration", palette = "Spectral") + 
            geom_point(data=data2map,
                       aes(y=reg_lat, x=reg_lng), 
                       colour="gray30",
                       size=3) +
            geom_point(data=data2map,
                       aes(y=region_lng, x=region_lat), 
                       colour="gray70",
                       size = 2) +
            labs(size = "Migration Flows") +
            geom_sf_text(data = data2map, 
                         aes(x = region_lat, y = region_lng, label = cbsa_shortname),
                         size = 3) +
            theme_minimal() +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.ticks=element_blank(),
                  plot.title=element_text(hjust=0.5, size=12)) +
            coord_equal(ylim=c(as.matrix(st_bbox(data2map))[2,1] - 2, as.matrix(st_bbox(data2map))[4,1] + 2), 
                        xlim=c(as.matrix(st_bbox(data2map))[1,1] - 2, as.matrix(st_bbox(data2map))[3,1] + 2)) +
            labs(title = paste0("Top 10 Region-to-Region Flows in ", input$msa_name, ", 2015-2018"))
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
