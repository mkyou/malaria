library(shiny)
library(DT)
library(tidyverse)
library(leaflet)

reactiveConsole(T)

setwd(dir = 'C:/Users/moise/OneDrive/Documentos/Projetos/TCC/malaria')

#get spatial data
micro = sf::read_sf('data/spatial_data/sph_files/microrreg.shp') |>
  sf::st_transform('+proj=longlat +datum=WGS84')

errors_df = micro |> 
  mutate(CD_MICRO = as.numeric(CD_MICRO)) |>
  inner_join(
    read_csv('results/general_errors.csv'),
    join_by(CD_MICRO == cod_micro_res, NM_MICRO == nome_micro_res) 
  ) |>
  rename(
    cod_micro_res = CD_MICRO,
    nome_micro_res = NM_MICRO,
    `Bell` = difs_bell,
    `Negative Binomial` = difs_nbinomial,
    `Poisson` = difs_poisson,
    `Corrected model (only in 2018)` = new_difs
  )

test_metric = read_csv('results/test_metrics_microrregion_falciparum.csv') |>
  mutate('type' = 'Falciparum') |>
  union_all(
    read_csv('results/test_metrics_microrregion_vivax.csv') |>
      mutate('type' = 'Vivax')
  )

# Define UI for application that draws a histogram
ui = navbarPage(
  'Spatio-temporal malaria modelling',
  tabPanel('Disease evolution',
           fluidPage(
             fluidRow('gráfico temporal'),
             fluidRow(
               column(4, 'menu para estados'),
               column(8, 'gráfico espacial')
             )
           )
  ),
  tabPanel(
    'Errors analysis',
    fluidPage(
      titlePanel('Error Analysis'),
      sidebarLayout(
        sidebarPanel(
          p('Choose the malaria type, model, year, month, 
            and error metric you want to evaluate.'),
          selectInput(
            'type',
            'Malaria type',
            choices = c(
              'Vivax',
              'Falciparum'
            )
          ),
          selectInput(
            'model',
            'Model',
            choices = c(
              'Negative Binomial',
              'Bell',
              'Poisson',
              'Corrected model (only in 2018)'
            )
          ),
          numericInput(
            'month',
            'Month',
            value = 1,
            min = 1,
            max = 12,
            step = 1
          ),
          numericInput(
            'year',
            'Year',
            value = 2016,
            min = 2016,
            max = 2018,
            step = 1
          ),
          selectInput(
            'metric',
            'Error metric',
            choices = c(
              'Ordinary Error',
              'Root Squared Logarithmic Error'
            )
          ),
          p('
          Use this graph to compare the evolution of errors for each model 
          over time.\n 
          Comparisons between models should not be made based on colors, 
          as the palette is different from model to model (corrected models, 
          for example, have a smaller range of errors than the others).\n\n
          
          The performance of the models can be compared using the table 
          alongside.\n
          
          As a last note, the corrected model, 
          when the malaria type is "Vivax",
          is the corrected Bell model.\n 
          The corrected model for the "Falciparum" type is the corrected 
          Poisson model.\n
          This choice was made taking into account the metrics of 
          phase 1 models, as explained in the text.
          ')
        ),
        mainPanel(
          leafletOutput('error', height = 500),
          p(),
          DTOutput('table')
        )
      )
    )
  ),
  tabPanel(
    'Predictions',
    fluidPage(
      titlePanel('Choose between Vivax or Falciparum malaria'),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            'type2',
            'Malaria type',
            choices = c(
              'Vivax',
              'Falciparum'
            )
          ),
          numericInput(
            'month2',
            'Month',
            value = 1,
            min = 1,
            max = 12,
            step = 1
          ),
        ),
        mainPanel(
          'preds x real'
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server = function(input, output) {
  
  #plot
  output$error = renderLeaflet({
    
    #choosing df based on malaria type
    mini_df = errors_df |> 
      filter(
        type == input$type,
        metric == input$metric) |>
      rename(
        errors = input$model
      ) |>
      select(errors, geometry, nome_micro_res, ano, mes)
   
    #creating pallete before filter.
    #this way, the errors colors from a same model will be 
    #comparable along the time
    pal = colorNumeric('YlOrRd', 
                       domain = mini_df$errors)
    
    #filtering month and year
    mini_df = mini_df |>
      filter(
        ano == input$year,
        mes == input$month
      )
    
    leaflet(mini_df) |>
      addTiles() |>
      #setView(42, 16, 4) |>
      addPolygons(
        stroke = T,
        color = 'black',
        weight = 1,
        fillOpacity = 1,
        fillColor = ~pal(errors),
        label = ~paste0(nome_micro_res, ': ', 
                        formatC(errors, big.mark = ','))) |>
      addLegend(
        'bottomright', pal = pal, values = ~errors  , opacity = 1) 
    
  })
  
  #table
  output$table = renderDataTable({
    test_metric |>
      filter(
        type == input$type
      ) |>
      select(-type) |>
      datatable() |>
      formatRound(columns = c('mbe', 'nrmse', 'rae', 'rmsle', 'rse', 'cor'), 
                  digits = 4)
    })
  
}

# Run the application
shinyApp(ui = ui, server = server)
