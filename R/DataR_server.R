#' @import shinyAce
DataR_server <- function(input, output, session, d, RVs){
  shinyAce_text_size <- reactiveVal(12)
  feature_spec <- edit_specification_server('DataR_feature_specification', reactive(RVs$original_feature_spec), 'feature')
  kpi_spec <- edit_specification_server('DataR_kpi_specification', reactive(RVs$original_kpi_spec), 'kpi')
  filter_spec <- edit_specification_server('DataR_filter_specification', reactive(RVs$original_filter_spec), 'filter')
  output$DataR_dataset_viewer <- DT::renderDT (server = TRUE, {
    if(!is.null(d()) & !is.null(RVs$train_test_filter) & !is.null(RVs$user_filter)){
      RVs$dt_update
      rows_to_summarise <- which(RVs$train_test_filter*RVs$user_filter==1)
      d_filter <- d()[rows_to_summarise, ]
      if(input$DataR_dataset_transpose=='No'){
        if(nrow(d_filter)>100){
          if(input$DataR_dataset_sample=='Head'){
            d_display <- utils::head(d_filter, 100)
          } else if(input$DataR_dataset_sample=='Sample') {
            d_display <- d_filter[sample(1:.N, 100, replace = FALSE), ]
          }
        } else {
          d_display <- copy(d_filter)
        }
        pg_length <- min(100, nrow(d_filter))
      } else if (input$DataR_dataset_transpose=='Yes'){
        if(nrow(d_filter)>10){
          if(input$DataR_dataset_sample=='Head'){
            d_display <- utils::head(d_filter, 10)
            idx <- 1:10
          } else if(input$DataR_dataset_sample=='Sample') {
            idx <- sort(sample(1:nrow(d_filter), 10, replace = FALSE))
            d_display <- d_filter[idx, ]
          }
        } else {
          idx <- 1:nrow(d_filter)
          d_display <- copy(d_filter)
        }
        d_display <- cbind(data.table(col = names(d_display), t(d_display)))
        names(d_display) <- c('dataset_column', as.character(idx))
        pg_length <- min(1000, nrow(d_display))
      }
      d_display %>%
        DT::datatable(rownames= FALSE,
                      extensions = 'Buttons',
                      class = 'white-space: nowrap',
                      options = list(pageLength = pg_length,
                                     dom = 'rti',
                                     scrollX = T,
                                     scrollY = 'calc(100vh - 316px)',
                                     searchHighlight=TRUE
                      )
        ) %>%
        DT::formatStyle(1:ncol(d_display), lineHeight='30%', fontSize = '85%')
      }
    })
  observe({RVs$feature_spec <- RVs$original_feature_spec})
  observe({RVs$kpi_spec <- RVs$original_kpi_spec})
  observe({RVs$filter_spec <- RVs$original_filter_spec})
  observe({RVs$feature_spec <- feature_spec()})
  observe({RVs$kpi_spec <- kpi_spec()})
  observe({RVs$filter_spec <- filter_spec()})
  observeEvent(RVs$kpi_spec, ignoreInit = TRUE, ignoreNULL = FALSE, {
    if(RVs$kpi_spec[['kpi_name']][1]=='User defined'){
      choices = c(RVs$kpi_spec[['kpi_name']])
    } else {
      choices = c(RVs$kpi_spec[['kpi_name']], 'User defined')
    }
    updateSelectInput(session, inputId = 'sidebar_kpi', label = 'Select KPI', choices = choices)
  })
  observeEvent(input$DataR_refresh_column_summary,{
    if(!is.null(d())){
      # setting to NULL forces a recalc in output$DataR_column_summary below
      RVs$column_summary <- NULL
    }
  })
  observeEvent(input$DataR_take_feature_to_ChartaR, {
    last_clicked <- RVs$column_summary[[1]][input$DataR_column_summary_cell_clicked$row]
    if(last_clicked %in% names(d())){
      updateSelectInput(session, inputId = 'ChartaR_x_axis_feature-selectInput', selected = last_clicked)
      updateTabItems(session, inputId = 'tabs', selected = 'ChartaR')
      updateNavbarPage(session = session, inputId = "ChartaR_tabsetPanel", selected = "One way summary")
    }
  })
  output$DataR_column_summary <- DT::renderDT({
    input$dataset
    # calculation below goes here
    # as it will take place under the shinycss spinner for DataR_column_summary (see DataR_ui)
    RVs$column_summary <- dataset_column_summary_2(isolate(d()), FALSE)
    if(is.null(RVs$column_summary)){
      name <- NULL
      dt <- data.table(name = 'Select dataset',
                       class = '',
                       type = '',
                       mean = '',
                       min = '',
                       max = '',
                       NAs = '')
    } else {
      dt <- RVs$column_summary
    }
    dt %>%
      DT::datatable(rownames= FALSE,
                    #class = 'white-space: nowrap',
                    extensions = 'Buttons',
                    selection=list(mode="single", target="row"),
                    options = list(pageLength = nrow(dt),
                                   dom = 'Bfrti',
                                   scrollX = T,
                                   scrollY = 'calc(100vh - 330px)',
                                   searchHighlight=TRUE,
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = list(list(extend='copy'),
                                                      list(extend='csv',filename = ''),
                                                      list(extend='excel',filename = ''),
                                                      list(extend='pdf',filename= '')),
                                       text = 'Save')
                                     )
                    )
      ) %>%
      DT::formatStyle(columns = colnames(dt), lineHeight='40%', fontSize = '85%') %>%
      DT::formatStyle(c('mean','min','max'), 'white-space'='nowrap')
  })
  output$DataR_feature_summary <- DT::renderDT({
    if(is.null(RVs$column_summary)){
      dt <- data.table(col='No dataset loaded')
    } else {
      col <- RVs$column_summary[input$DataR_column_summary_rows_selected,name]
      if(length(col)==0){
        dt <- data.table(col='select feature')
      } else {
        dt <- feature_summary(d()[[col]])
      }
    }
    dt %>%
      DT::datatable(rownames= FALSE,
                    extensions = 'Buttons',
                    selection=list(mode="single", target="row"),
                    options = list(pageLength = max(11, nrow(dt)),
                                   dom = 'Bfrti',
                                   scrollX = T,
                                   scrollY = 'calc(100vh - 330px)',
                                   searchHighlight=TRUE,
                                   buttons =
                                     list(
                                       list(
                                       extend = 'collection',
                                       buttons = list(list(extend='copy'),
                                                      list(extend='csv',filename = ''),
                                                      list(extend='excel',filename = ''),
                                                      list(extend='pdf',filename= '')),
                                       text = 'Save')
                                     )
                    )
      ) %>%
      DT::formatStyle(columns = colnames(dt), lineHeight='40%', fontSize = '85%')
  })
  output$DataR_feature <- renderUI({
    col <- RVs$column_summary[input$DataR_column_summary_rows_selected,name]
    if(is.data.table(col)) col <- 'Select feature'
    p(col, style = 'font-size: 20px; margin-top: 18px; margin-bottom:-15px')
    })
  shinyAce_text_size <- reactiveVal(12)
  observeEvent(input$shinyAce_evaluate, {
    result <- tryCatch({eval(parse(text = input$shinyAce_code))}, error = function(e){e})
    if(class(result)[1]=='simpleError'){
      # something went wrong
      confirmSweetAlert(session = session,
                        type = 'error',
                        inputId = "DataR_error",
                        title = 'Evaluation error',
                        text = result$message,
                        btn_labels = c('OK'))
    } else {
      output$shinyAce_output <- renderPrint({result})
      RVs$dt_update <- RVs$dt_update + 1 # as changes to data.table d() aren't detected by shiny
    }
  })
  observeEvent(input$shinyAce_textsize_minus, {
    shinyAce_text_size(pmax(8,shinyAce_text_size()-1))
    shinyAce::updateAceEditor(session, editorId = 'shinyAce_code', fontSize = shinyAce_text_size())
  })
  observeEvent(input$shinyAce_textsize_plus, {
    shinyAce_text_size(min(30,shinyAce_text_size()+1))
    shinyAce::updateAceEditor(session, editorId = 'shinyAce_code', fontSize = shinyAce_text_size())
  })
}

