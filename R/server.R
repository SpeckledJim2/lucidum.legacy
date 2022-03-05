#' @import data.table
#' @import rhandsontable
#' @import leaflet
server <- function(input, output, session) {
  d <- reactiveVal(NULL) # holds the dataset
  GlimmaR_model_index <- reactiveVal(0)
  BoostaR_model_index <- reactiveVal(0)
  original_cols <- reactiveVal(NULL)
  RVs <- reactiveValues(dataset_name=NULL,
                        column_summary=NULL,
                        lucidum_cols=NULL,
                        numerical_cols = NULL,
                        original_feature_spec=NULL,
                        original_kpi_spec=NULL,
                        original_filter_spec=NULL,
                        feature_spec=NULL,
                        kpi_spec=NULL,
                        filter_spec=NULL,
                        feature_specifications=NULL,
                        train_test_filter=NULL,
                        user_filter=NULL,
                        GlimmaR_models=list(),
                        BoostaR_models=list(),
                        dt_update=0)
  # run on startup
  init_lucidum(session)
  # run on close browser - stops server
  session$onSessionEnded(function() {
    stopApp()
  })
  # observeEvents
  observeEvent(input$dataset, ignoreInit = TRUE, {
    d(load_dataset(input$dataset))
    RVs$dataset_name <- input$dataset
    original_cols(paste0(names(d()),'')) # no idea why needed but without paste0 problems below
    RVs$original_feature_spec <- load_specification(d(), input$dataset, SPECIFICATION_PATH, 'feature')
    RVs$original_kpi_spec <- load_specification(d(), input$dataset, SPECIFICATION_PATH, 'kpi')
    RVs$original_filter_spec <- load_specification(d(), input$dataset, SPECIFICATION_PATH, 'filter')
  })
  observeEvent(c(input$response, input$weight, RVs$kpi_spec, RVs$train_test_filter, RVs$user_filter), ignoreInit = TRUE, {
    # set sidebar_kpi to 'User defined' if response and denominator don't tie up with row in kpi_spec
    if(!is.null(input$response) &
       input$response != 'select feature' &
       !is.null(input$weight) &
       !is.null(input$sidebar_kpi) &
       !is.null(RVs$kpi_spec)
       ){
      kpi_components <- kpi_numerator_denominator(input$sidebar_kpi, RVs$kpi_spec)
      if(!is.null(kpi_components)){
        if(length(kpi_components$numerator)>0 & length(kpi_components$denominator)>0){
          if(input$response!=kpi_components$numerator | input$weight!=kpi_components$denominator){
            if(input$sidebar_kpi!='User defined'){
              updateSelectInput(session, inputId = 'sidebar_kpi', selected = 'User defined')
            }
          }
        }
      }
    }
    # update the response and weight labels
    if(input$response %in% names(d()) &
       input$weight %in% c('N', 'no weights', names(d())) &
       !is.null(RVs$train_test_filter) &
       !is.null(RVs$user_filter)
       ){
      rows_to_summarise <- which(RVs$train_test_filter*RVs$user_filter==1)
      if(input$weight=='N'){
        wtd_mean <- d()[rows_to_summarise, lapply(.SD, mean, na.rm = TRUE), .SDcols = input$response][[1]]
        wt_sum <- length(rows_to_summarise)
      } else if(input$weight=='no weights'){
        wtd_mean <- d()[rows_to_summarise, lapply(.SD, sum, na.rm = TRUE), .SDcols = input$response][[1]]
        wt_sum <- length(rows_to_summarise)
      } else {
        wt_sum <- d()[rows_to_summarise, lapply(.SD, sum, na.rm = TRUE), .SDcols = input$weight][[1]]
        wtd_mean <- d()[rows_to_summarise, lapply(.SD, sum, na.rm = TRUE), .SDcols = input$response][[1]]/wt_sum
      }
      wtd_mean_formatted <- apply_kpi_format(wtd_mean, input$response, input$weight, RVs$kpi_spec)
      updateSelectInput(session, inputId = 'response', label = paste0('Response = ', wtd_mean_formatted))
      updateSelectInput(session, inputId = 'weight', label = paste0('Weight = ', format(wt_sum, nsmall=0, big.mark=',')))
    }
  })
  observeEvent(input$sidebar_kpi, ignoreInit = TRUE, {
    if(input$sidebar_kpi!='User defined'){
      if(is.null(RVs$kpi_spec)){
        ks <- RVs$original_kpi_spec
      } else if(nrow(RVs$kpi_spec)==0){
        ks <- RVs$original_kpi_spec
      } else {
        ks <- RVs$kpi_spec
      }
      kpi_components <- kpi_numerator_denominator(input$sidebar_kpi, ks)
      if(kpi_components$numerator %in% names(d()) & kpi_components$denominator %in% c('N', 'no weights', names(d()))){
        updateSelectInput(session, inputId = 'response', selected = kpi_components$numerator)
        updateSelectInput(session, inputId = 'weight', selected = kpi_components$denominator)
      } else {
        confirmSweetAlert(session = session,
                          type = 'error',
                          inputId = "kpi_error",
                          title = 'Features not in dataset',
                          text = 'Check numerator and denominator in KPI definition',
                          btn_labels = c('OK')
        )
        updateSelectInput(session, inputId = 'sidebar_kpi', selected = 'User defined')
      }
    }
  })
  observeEvent(c(d(), RVs$dt_update) , ignoreInit = TRUE, {
    RVs$lucidum_cols <- setdiff(names(d()), original_cols())
    RVs$numerical_cols <- numerical_cols(d())
    table_lists <- return_global_env_tables()
    updateSelectInput(session,
                      inputId = 'dataset',
                      selected = input$dataset,
                      choices=list('Datasets with PostcodeArea column for MappaR' = table_lists[[1]],
                                   'Other datasets' = table_lists[[2]])
    )
    numerical_cols <- numerical_cols(d())
    response_selected <- ifelse(input$response %in% numerical_cols, input$response, numerical_cols[[1]])
    weight_selected <- ifelse(input$weight %in% numerical_cols, input$weight, 'N')
    updateSelectInput(session, inputId = 'response', choices = numerical_cols, selected = response_selected)
    updateSelectInput(session, inputId = 'weight', choices = c('N', 'no weights', numerical_cols), selected = weight_selected)
  })
  # servers
  DataR_server(input, output, session, d, RVs)
  ChartaR_server(input, output, session, d, RVs)
  MappaR_server(input, output, session, d, RVs)
  BoostaR_server(input, output, session, d, RVs)
  GlimmaR_server(input, output, session, d, RVs)
}

