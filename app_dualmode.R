################################################################################
# Author: Gabriele Midolo
# Email: midolo@fzp.czu.cz
# Last update: 31.07.2025
################################################################################

#### 1. Prepare data ####

# Load packages
suppressPackageStartupMessages({
  library(shiny)
  library(sf)
  library(dplyr)
  library(leaflet)
  library(viridis)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(base64enc)
})


#### 2. Define UI ####

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
    /* Target Leaflet tile layers and apply brightness filter */
    .leaflet-tile-container img {
      filter: brightness(95%); /* Adjust percentage (e.g., 70% for darker, 90% for lighter) */
    }
  "))
  ),
  
  
  titlePanel(
    tags$h3('Geographic patterns of community mean EIV change in European plant communities from 1960 to 2020',
            style = 'font-size: 24px; font-weight: bold; color: #333;')
  ),
  sidebarLayout(
    sidebarPanel(
      
      selectInput('eiv_choice', 'Ecological Indicator Value:',
                  choices = c('Light', 'Temperature', 'Soil Moisture', 'Soil Nitrogen', 'Soil Reaction')),
      
      selectInput('mode', 'Mapping mode:',
                  choices = list(
                    'CMEIV change' = 'CMEIV change', 
                    'CMEIV per year' = 'CMEIV per year'
                  ),
                  selected = 'CMEIV change', 
                  selectize = T
      ),
      # JavaScript to customize the selectize.js rendering
      tags$script(HTML("
        $(document).ready(function() {
          // Function to format the text with subscript EIV
          function formatModeText(item) {
            var text = item.text || item.value; // Get the text or value
            if (text.includes('CMEIV')) {
              return text.replace('CMEIV', 'CM<sub>EIV</sub>');
            }
            return text;
          }

          // Initialize selectize with custom renderers
          $('#mode').selectize({
            render: {
              option: function(item, escape) { // For items in the dropdown list
                return '<div>' + formatModeText(item) + '</div>';
              },
              item: function(item, escape) { // For the selected item in the input box
                return '<div>' + formatModeText(item) + '</div>';
              }
            },
            // Ensure plugin 'remove_button' or others are added if needed,
            // otherwise, default selectize behavior might be lost.
            // plugins: ['remove_button'] // Example if you had specific plugins
          });

          // Manually trigger update for initial selection
          var selectizeInstance = $('#mode')[0].selectize;
          if (selectizeInstance) {
            var initialValue = selectizeInstance.getValue();
            var initialText = selectizeInstance.options[initialValue].text;
            $('#select2-mode-container').html(formatModeText({text: initialText}));
          }
        });
      ")),

      selectInput('habitat', 'Habitat:',
                  choices = c('Forest', 'Grassland', 'Scrub', 'Wetland')),
      
      div(style = 'font-size: 1.0em; font-style: italic; margin-top: 2px; margin-bottom: 15px;',
          textOutput('plot_size_note')),
      
      conditionalPanel(
        condition = "input.mode == 'CMEIV change'",
        sliderInput('years', 'Time period:',
                    min = 1960, max = 2020, value = c(1960, 2020), sep = '')
      ),
      conditionalPanel(
        condition = "input.mode == 'CMEIV per year'",
        sliderInput('single_year', 'Select Year:',
                    min = 1960, max = 2020, value = 1960, sep = '')
      ),
      div(style = 'margin-top: 40px; text-align: left; font-size: 0.9em; color: #555;',
          p(HTML('Author: <strong>Gabriele Midolo</strong><a href="https://orcid.org/0000-0003-1316-2546" target="_blank" style="margin-left: 5px;"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" width="15" style="vertical-align: middle;"/></a> (<a href="mailto:midolo@fzp.czu.cz">midolo@fzp.czu.cz</a>) <br> Department of Spatial Sciences, Faculty of Environmental Sciences, Czech University of Life Sciences Prague, Kamýcká 129, 165 00, Praha - Suchdol, Czech Republic')),
          p("Date: 01.08.2025")
      )
    ),
    mainPanel(
      leafletOutput('map', height = '800px')
    )
  )
)


#### 3. Define server ####

server <- function(input, output, session) {
  
  output$plot_size_note <- renderText({
    req(input$habitat)
    sizes <- c(
      'Forest' = '225 m²',
      'Grassland' = '16 m²',
      'Scrub' = '50 m²',
      'Wetland' = '25 m²')
    habitat_name <- c(
      'Forest' = 'forests',
      'Grassland' = 'grasslands',
      'Scrub' = 'scrub',
      'Wetland' = 'wetlands')
    paste0('Predictions at ', sizes[input$habitat], ' for ', habitat_name[input$habitat], '.')
  })
  
  eiv_data <- reactive({
    req(input$eiv_choice)
    codes <- data.frame(
      eiv_choice = c('Light', 'Temperature', 'Soil Moisture', 'Soil Nitrogen', 'Soil Reaction'),
      code = c('L', 'T', 'M', 'N', 'R')
    )
    code_i <- codes[which(input$eiv_choice == codes$eiv_choice), 2]
    read_rds(paste0('./data/', code_i, '_data.rds'))
  })
  
  dat_with_id <- reactive({
    req(eiv_data())
    eiv_data() %>%
      mutate(geometry_id = paste0(x, '_', y, '_', habitat))
  })
  
  dat_sf <- reactive({
    dat_with_id() %>%
      st_as_sf(coords = c('x', 'y'), crs = 25832) %>%
      filter(habitat == input$habitat) %>%
      st_transform(4326)
  })
  
  change_data <- reactive({
    req(input$years[1] < input$years[2])
    dat_hab <- dat_sf()
    y1 <- paste0('eiv_pred_', input$years[1])
    y2 <- paste0('eiv_pred_', input$years[2])
    
    dat_hab <- dat_hab %>%
      mutate(change_value = .data[[y2]] - .data[[y1]]) %>%
      mutate(
        change_cat = cut(
          change_value,
          breaks = c(-Inf, -1, -0.5, -0.1, 0.1, 0.5, 1, Inf),
          labels = c(
            '< -1',
            '-1 – -0.5',
            '-0.5 – -0.1',
            '-0.1 – +0.1',
            '+0.1 – +0.5',
            '+0.5 – +1',
            '> +1'
          ),
          include.lowest = T
        )
      )
    
    dat_hab
  })
  
  snapshot_data <- reactive({
    dat_hab <- dat_sf()
    
    yr_col <- paste0('eiv_pred_', input$single_year)
    
    req(yr_col %in% names(st_drop_geometry(dat_hab)))
    
    dat_hab %>%
      dplyr::select(geometry_id, n, habitat, all_of(yr_col)) %>%
      rename(current_eiv_value = all_of(yr_col))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldGrayCanvas') %>%
      setView(lng = 15, lat = 57, zoom = 4)
  })
  
  prev_mode <- reactiveVal(NULL)
  
  observeEvent(input$mode, {
    if (!is.null(prev_mode()) && prev_mode() != input$mode) {
      leafletProxy('map') %>% clearMarkers()
    }
    prev_mode(input$mode)
    leafletProxy('map') %>% clearPopups()
  }, ignoreNULL = F, ignoreInit = T)
  
  observeEvent(input$habitat,
               {
                 leafletProxy('map') %>% clearPopups()
               },
               ignoreNULL = F,
               ignoreInit = T)

  observeEvent(input$years,
               {if (input$mode == 'CMEIV change') { 
                 leafletProxy('map') %>% clearPopups()}
               },
               ignoreNULL = F,
               ignoreInit = T)
  
  observeEvent(input$single_year,
               {
                 if (input$mode == 'CMEIV per year') {
                   leafletProxy('map') %>% clearPopups()
                 }
               },
               ignoreNULL = F,
               ignoreInit = T)

  
  observe({
    input$habitat
    input$eiv_choice
    
    leafletProxy('map') %>% clearControls()
    leafletProxy('map') %>% clearMarkers()
    
    current_zoom <- input$map_zoom
    if (is.null(current_zoom))
      current_zoom <- 4
    
    fac = 0.2
    zoomy = input$map_zoom
    scale_factor = zoomy * fac
    
    if (input$mode == 'CMEIV change') { 
      dat_plot <- change_data()
      
      levels_lab <- c(
        '< -1',
        '-1 – -0.5',
        '-0.5 – -0.1',
        '-0.1 – +0.1',
        '+0.1 – +0.5',
        '+0.5 – +1',
        '> +1'
      )
      
      # # RdYlBu
      # cols <- hcl.colors(length(levels_lab), 'RdYlBu', rev = T)
      # Spectral
      cols <- hcl.colors(length(levels_lab), 'Spectral', rev = T)
      
      pal <- colorFactor(palette = cols, levels = levels_lab)
      
      leafletProxy('map') %>%
        addCircleMarkers(
          data = dat_plot,
          layerId = ~ paste0('point_', geometry_id),
          radius = ~ (log(n) + 2) * scale_factor,
          color = ~ pal(change_cat),
          fillOpacity = 0.7,
          stroke = F
        ) %>%
        addLegend(
          'topright',
          pal = pal,
          values = dat_plot$change_cat,
          title = HTML(paste0(input$eiv_choice, ' change')),
          opacity = 0.9
        )
      
    } else if(input$mode == 'CMEIV per year'){ 
      
      dat_plot <- snapshot_data()
      
      req(nrow(dat_plot) > 0)
      req(any(!is.na(dat_plot$current_eiv_value)))
      
      eiv_min <- floor(min(dat_plot$current_eiv_value, na.rm = T))
      eiv_max <- ceiling(max(dat_plot$current_eiv_value, na.rm = T))
      
      pal_continuous <- colorNumeric(
        palette = "viridis",
        domain = c(eiv_min, eiv_max)
      )
      
      leafletProxy('map') %>%
        addCircleMarkers(
          data = dat_plot,
          layerId = ~ paste0('point_', geometry_id),
          radius = ~ (log(n) + 2) * scale_factor,
          color = ~ pal_continuous(current_eiv_value),
          fillOpacity = 0.7,
          stroke = F
        ) %>%
        addLegend(
          'topright',
          pal = pal_continuous,
          values = c(eiv_min, eiv_max),
          title = HTML(paste0(input$eiv_choice, ' CM<sub>EIV</sub>')),
          opacity = 0.9
        )
    }
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)
    
    clicked_id_raw <- sub('point_', '', click$id)
    id_parts <- strsplit(clicked_id_raw, '_')[[1]]
    
    req(length(id_parts) >= 3)
    
    clicked_x <- as.numeric(id_parts[1])
    clicked_y <- as.numeric(id_parts[2])
    clicked_habitat <- id_parts[3]
    
    point_data_for_plot <- eiv_data() %>%
      filter(x == clicked_x, y == clicked_y, habitat == clicked_habitat)
    
    req(nrow(point_data_for_plot) == 1)
    
    n_plots_at_point <- point_data_for_plot$n
    
    current_value_text <- ''
    graph_html <- ''
    
    if (input$mode == 'CMEIV change') { # Condition uses the internal value
      y1_col <- paste0('eiv_pred_', input$years[1])
      y2_col <- paste0('eiv_pred_', input$years[2])
      
      if (y1_col %in% names(point_data_for_plot) &&
          y2_col %in% names(point_data_for_plot)) {
        eiv_pred_y1 <- point_data_for_plot[[y1_col]]
        eiv_pred_y2 <- point_data_for_plot[[y2_col]]
        
        change_value <- eiv_pred_y2 - eiv_pred_y1
        
        val_display <- ifelse(change_value > 0,
                              paste0('+', round(change_value, 2)),
                              round(change_value, 2))
        
        current_value_text <- paste0(
          '<strong>', input$eiv_choice, ' </strong> ',
          '(from ',
          input$years[1],
          ' to ',
          input$years[2],
          ') = ',
          val_display,
          ' (&#916;CM<sub>EIV</sub>)'
        )
      }
      
      plot_data_long <- point_data_for_plot %>%
        dplyr::select(starts_with('eiv_pred_')) %>%
        pivot_longer(
          cols = starts_with('eiv_pred_'),
          names_to = 'prediction_year_str',
          values_to = 'eiv_prediction',
          names_prefix = 'eiv_pred_'
        ) %>%
        mutate(prediction_year = as.numeric(prediction_year_str)) %>%
        filter(prediction_year >= input$years[1] &
                 prediction_year <= input$years[2]) %>%
        arrange(prediction_year)
      
      if (nrow(plot_data_long) > 0) {
        span_yrs <- input$years[2] - input$years[1]
        p <- ggplot(plot_data_long,
                    aes(x = prediction_year, y = eiv_prediction)) +
          geom_line(color = 'midnightblue') +
          geom_point(
            data = plot_data_long %>%
              filter(
                prediction_year == max(prediction_year) |
                  prediction_year == min(prediction_year)
              ),
            size = 3,
            color = 'midnightblue'
          ) +
          scale_x_continuous(breaks = pretty_breaks(n = ifelse(span_yrs >= 5, 5, span_yrs))) +
          labs(
            title = paste0('Local trend (', clicked_habitat, ')'),
            x = 'Year',
            y = bquote(.(input$eiv_choice) ~ CM[EIV])
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 12, face = 'bold'),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 'cm'),
            axis.text.x = element_text(
              angle = 45,
              vjust = 1,
              hjust = 1
            )
          )
        
        temp_file <- tempfile(fileext = '.png')
        ggsave(
          temp_file,
          plot = p,
          width = 4,
          height = 3,
          units = 'in',
          dpi = 150
        )
        img_b64 <- base64encode(temp_file)
        unlink(temp_file)
        graph_html <- paste0("<img src='data:image/png;base64,",
                             img_b64,
                             "' width='250px'><br>")
      } else {
        graph_html <- '<strong>No trend data for the selected period to plot.</strong><br>'
      }
      
    } else if (input$mode == 'CMEIV per year') { 
      single_year_col <- paste0('eiv_pred_', input$single_year)
      if (single_year_col %in% names(point_data_for_plot)) {
        current_eiv_value <- point_data_for_plot[[single_year_col]]
        current_value_text <- paste0(
          '<strong>', input$eiv_choice, ' CM<sub>EIV</sub> (',
          input$single_year,
          '):</strong> ',
          round(current_eiv_value, 2)
        )
      } else {
        current_value_text <- paste0('<strong>Data not available for ', input$single_year, '.</strong>')
      }
      graph_html <- ''
    }

    popup_content <- paste0(
      current_value_text,
      '<br>Number of plots: ',
      n_plots_at_point,
      '<br>',
      graph_html
    )
    
    leafletProxy('map') %>%
      clearPopups() %>%
      addPopups(
        lat = click$lat,
        lng = click$lng,
        popup = popup_content,
        layerId = click$id
      )
  })
  
}


#### 4. Run the app ####

shinyApp(ui, server)
