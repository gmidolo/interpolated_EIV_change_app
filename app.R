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
  
  # define background brightness
  tags$head(
    tags$style(HTML("
    .leaflet-tile-container img {
      filter: brightness(75%); /* Adjust percentage (e.g., 80% for darker, 120% for lighter) */
    }
  "))
  ),
  
  # title
  titlePanel(
    tags$h3('Geographic patterns of community-mean ecological indicator value (eiv) change in Europe from 1960 to 2020',
            style = 'font-size: 24px; font-weight: bold; color: #333;')
  ),
  
  # menu structure
  sidebarLayout(
    sidebarPanel(
      
      # General Description Section
      div(style = 'margin-bottom: 20px; font-size: 1.1em; line-height: 1.4;',
          p(HTML("We interpolated spatiotemporal changes in community-mean ecological indicator values (sourced from the EIVE database; <a href='https://doi.org/10.3897/VCS.98324' target='_blank'>Dengler et al. 2023</a>) between 1960 and 2020 using Random Forests. Predictions are obtained over 606,818 European vegetation plots available on the <a href='https://euroveg.org/eva-database/' target='_blank'>European Vegetation Archive</a> (<a href='https://doi.org/10.1111/avsc.12519' target='_blank'>Chytrý et al. 2020</a>) and <a href='https://euroveg.org/resurvey/' target='_blank'>ReSurveyEurope</a> (<a href='https://doi.org/10.1111/jvs.13235' target='_blank'>Knollová et al. 2024</a>).")),
          p(HTML("Predictions of &#916;CM<sub>EIV</sub> from individual plots are aggregated onto a 10 km &times; 10 km grid. On the map, point size represents the number of plots within each grid cell. Areas with denser sampling in space and time are more likely to show more accurate trends.")),
          p(HTML('<strong>Article repository</strong>: 
                  <img src="https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png" width="15" style="vertical-align: middle; margin-right: 3px;"/> <a href="https://github.com/gmidolo/interpolated_EIV_change" target="_blank">github.com/gmidolo/interpolated_EIV_change</a>')),
          p(HTML('<strong>App repository</strong>: 
                  <img src="https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png" width="15" style="vertical-align: middle; margin-right: 3px;"/> <a href="https://github.com/gmidolo/interpolated_EIV_change_app" target="_blank">github.com/gmidolo/interpolated_EIV_change_app</a>'))
      ),
      
      selectInput('eiv_choice', 'Ecological Indicator Value:',
                  choices = c('Light', 'Temperature', 'Soil Moisture', 'Soil Nitrogen', 'Soil Reaction')),
      
      div(style = 'font-size: 1.0em; margin-top: 2px; margin-bottom: 15px;',
          htmlOutput('eiv_choice_note')),
      
      selectInput('habitat', 'Habitat:',
                  choices = c('Forest', 'Grassland', 'Scrub', 'Wetland')),
      
      div(style = 'font-size: 1.0em; font-style: italic; margin-top: 2px; margin-bottom: 15px;',
          textOutput('plot_size_note')),
      
      sliderInput('years', 'Time period:',
                  min = 1960, max = 2020, value = c(1960, 2020), sep = ''),
      div(style = 'margin-top: 40px; text-align: left; font-size: 0.9em; color: #555;',
          p(HTML('Author: <strong>Gabriele Midolo</strong><a href="https://orcid.org/0000-0003-1316-2546" target="_blank" style="margin-left: 5px;"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" width="15" style="vertical-align: middle;"/></a> (<a href="mailto:midolo@fzp.czu.cz">midolo@fzp.czu.cz</a>) <br> Department of Spatial Sciences, Faculty of Environmental Sciences, Czech University of Life Sciences Prague, Kamýcká 129, 165 00, Praha - Suchdol, Czech Republic')),
          p("Date: 01.08.2025"),
          p(HTML('Project contributors:
                  Petr Keil <a href="https://orcid.org/0000-0003-3017-1858" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Adam Thomas Clark <a href="https://orcid.org/0000-0002-8843-3278" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Milan Chytrý <a href="https://orcid.org/0000-0002-8122-3075" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Franz Essl <a href="https://orcid.org/0000-0001-8253-2112" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Stefan Dullinger <a href="https://orcid.org/0000-0003-3919-0887" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Ute Jandt <a href="https://orcid.org/0000-0002-3177-3669" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Helge Bruelheide <a href="https://orcid.org/0000-0003-3135-0356" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Jürgen Dengler <a href="https://orcid.org/0000-0003-3221-660X" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Irena Axmanová <a href="https://orcid.org/0000-0001-9440-7976" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Svetlana Aćić <a href="https://orcid.org/0000-0001-6553-3797" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Olivier Argagnon <a href="https://orcid.org/0000-0003-2069-7231" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Idoia Biurrun <a href="https://orcid.org/0000-0002-1454-0433" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Gianmaria Bonari <a href="https://orcid.org/0000-0002-5574-6067" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Alessandro Chiarucci <a href="https://orcid.org/0000-0003-1160-235X" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Renata Ćušterevska <a href="https://orcid.org/0000-0002-3849-6983" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Pieter De Frenne <a href="https://orcid.org/0000-0002-8613-0943" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Michele De Sanctis <a href="https://orcid.org/0000-0002-7280-6199" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Jan Divíšek <a href="https://orcid.org/0000-0002-5127-5130" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Tetiana Dziuba <a href="https://orcid.org/0000-0001-8621-0890" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Rasmus Ejrnæs <a href="https://orcid.org/0000-0003-2538-8606" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Emmanuel Garbolino <a href="https://orcid.org/0000-0002-4954-6069" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Anke Jentsch <a href="https://orcid.org/0000-0002-2345-8300" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Borja Jiménez-Alfaro <a href="https://orcid.org/0000-0001-6601-9597" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Jonathan Lenoir <a href="https://orcid.org/0000-0003-0638-9582" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Jesper Erenskjold Moeslund <a href="https://orcid.org/0000-0001-8591-7149" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Francesca Napoleone <a href="https://orcid.org/0000-0002-3807-7180" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Sabine Rumpf <a href="https://orcid.org/0000-0001-5909-9568" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Jens-Christian Svenning <a href="https://orcid.org/0000-0002-3415-0862" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Grzegorz Swacha <a href="https://orcid.org/0000-0002-6380-2954" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Irina Tatarenko <a href="https://orcid.org/0000-0001-6835-2465" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Martin Večeřa <a href="https://orcid.org/0000-0001-8507-791X" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
                  Denys Vynokurov <a href="https://orcid.org/0000-0001-7003-6680" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>
                  '))
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
    # habitat_name <- c(
    #   'Forest' = 'forests',
    #   'Grassland' = 'grasslands',
    #   'Scrub' = 'scrub',
    #   'Wetland' = 'wetlands')
    paste0('Predictions at fixed plot size: ', sizes[input$habitat], '.')
  })
  
  output$eiv_choice_note <- renderText({
    req(input$eiv_choice)
    eiv_description <- c(
      'Light' = 'canopy/herb density and preferences for light intensity (<span style="color:#A51122;"><strong>lighter</strong></span> vs. <span style="color:#324DA0;"><strong>shadier</strong></span>)',
      'Temperature' = 'preferences for temperature (<span style="color:#A51122;"><strong>warmer</strong></span> vs. <span style="color:#324DA0;"><strong>colder</strong></span>)',
      'Soil Moisture' = 'preferences for moisture in the soil/substrate (<span style="color:#A51122;"><strong>wetter</strong></span> vs. <span style="color:#324DA0;"><strong>drier</strong></span>)',
      'Soil Nitrogen' = 'nutrient (mainly nitrogen) availability and productivity (<span style="color:#A51122;"><strong>nutrient increase</strong></span> vs. <span style="color:#324DA0;"><strong>nutrient decrease</strong></span>)',
      'Soil Reaction' = 'preferences for soil/substrate pH (<span style="color:#A51122;"><strong>increasing alkalinity</strong></span> vs. <span style="color:#324DA0;"><strong>increasing acidity</strong></span>)'
    )
    HTML(paste0('<strong>','Description','</strong>',': changes in ', eiv_description[input$eiv_choice], '.'))
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
          breaks = c(-Inf, -1, -0.5, -0.25, -0.1, 0.1, 0.25, 0.5, 1, Inf),
          labels = c(
            '< -1.00',
            '-1.00 – -0.50',
            '-0.50 – -0.25',
            '-0.25 – -0.10',
            '-0.10 – +0.10',
            '+0.10 – +0.25',
            '+0.25 – +0.50',
            '+0.50 – +1.00',
            '> +1.00'
          ),
          include.lowest = T
        )
      )
    
    dat_hab
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldGrayCanvas') %>%
      setView(lng = 15, lat = 57, zoom = 4)
  })
  
  observeEvent(input$habitat,
               {
                 leafletProxy('map') %>% clearPopups()
               },
               ignoreNULL = F,
               ignoreInit = T)
  
  observeEvent(input$years,
               {
                 leafletProxy('map') %>% clearPopups()
               },
               ignoreNULL = F,
               ignoreInit = T)
  
  
  observe({
    input$habitat
    input$eiv_choice
    
    leafletProxy('map') %>% clearControls()
    leafletProxy('map') %>% clearMarkers()
    
    current_zoom <- input$map_zoom
    # if (is.null(current_zoom))
    #   {current_zoom <- 4}
    
    fac = 0.1
    zoomy = input$map_zoom
    scale_factor =  zoomy * fac + (zoomy * fac)^2
    
    dat_plot <- change_data()
    
    levels_lab <- c(
      '< -1.00',
      '-1.00 – -0.50',
      '-0.50 – -0.25',
      '-0.25 – -0.10',
      '-0.10 – +0.10',
      '+0.10 – +0.25',
      '+0.25 – +0.50',
      '+0.50 – +1.00',
      '> +1.00'
    )
    
    cols <- hcl.colors(length(levels_lab), 'RdYlBu', rev = T)
    
    pal <- colorFactor(palette = cols, levels = levels_lab)
    
    leafletProxy('map') %>%
      addCircleMarkers(
        data = dat_plot,
        layerId = ~ paste0('point_', geometry_id),
        radius = ~ (log(n) + 2) * scale_factor,
        color = ~ pal(change_cat),
        fillOpacity = 0.9,
        stroke = F
      ) %>%
      addLegend(
        'topright',
        pal = pal,
        values = dat_plot$change_cat,
        title = HTML(paste0(input$eiv_choice, ' (&#916;CM<sub>EIV</sub>)')),
        opacity = 0.9
      )
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
        geom_line(color = 'black') +
        geom_point(
          data = plot_data_long %>%
            filter(
              prediction_year == max(prediction_year) |
                prediction_year == min(prediction_year)
            ),
          size = 3,
          color = 'black'
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
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 'cm'),
          axis.text.x = element_text(
            angle = 45, vjust = 1, hjust = 1
          )
        )
      
      temp_file <- tempfile(fileext = '.png')
      ggsave(
        temp_file, plot = p, width = 4, height = 3, units = 'in', dpi = 150
      )
      img_b64 <- base64encode(temp_file)
      unlink(temp_file)
      graph_html <- paste0("<img src='data:image/png;base64,",
                           img_b64,
                           "' width='250px'><br>")
    } else {
      graph_html <- '<strong>No trend data for the selected period to plot.</strong><br>'
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