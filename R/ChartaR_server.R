ChartaR_server <- function(input, output, session, d, RVs){
  ChartaR_1W_banding <- reactiveVal(1)
  residual_error_results <- reactiveVal(NULL)
  observeEvent(RVs$filter_spec, ignoreInit = TRUE, {
    choices <- RVs$filter_spec[[1]]
    choices <- choices[!is.na(choices)]
    if(!is.null(choices)){
      if(choices[1]!='no filter'){
        choices <- c('no filter', choices)
      }
      updateSelectInput(session, "ChartaR_filter_list", choices = choices, selected = 'no filter')
    }
  })
  observeEvent(input$ChartaR_1W_banding, ignoreInit = TRUE, {
    x <- input$ChartaR_1W_banding
    b <- ChartaR_1W_banding()
    sel <- character(0)
    if(!is.null(x)){
      if(length(x)>0){
        if(x=='<'){
          ChartaR_1W_banding(modify_banding_level(b, -1))
        } else if (x=='>'){
          ChartaR_1W_banding(modify_banding_level(b, +1))
        } else if (x=='lock'){
          # do nothing - leave banding unchanged
          sel <- 'lock'
        } else if (x %in% c('0.01','0.1','1','5','10','50','100')){
          ChartaR_1W_banding(as.numeric(x))
        }
        date_response <- class(d()[[input[['ChartaR_x_axis_feature-selectInput']]]])[1] %in% c('IDate','Date','POSIXct')
        t <- banding_displayed(ChartaR_1W_banding(), date_response)
        updateRadioGroupButtons(session,
                                inputId = 'ChartaR_1W_banding',
                                label = paste0('Banding (',t,')'),
                                selected = sel
        )
      }
    }
  })
  observe({
    if(!is.null(d())){
      train_test <- NULL # to pass R CMD check, nothing more, train_test below refers to column in d()
      if(!('train_test' %in% names(d()))){
        RVs$train_test_filter <- rep(1,nrow(d()))
      } else {
        if(input$ChartaR_training_test=='All'){
          RVs$train_test_filter <- rep(1,nrow(d()))
        } else if (input$ChartaR_training_test=='Train'){
          RVs$train_test_filter <- 1 - d()[, train_test]
        } else if (input$ChartaR_training_test=='Test'){
          RVs$train_test_filter <- d()[, train_test]
        }
      }
      if(!is.null(input$ChartaR_filter_list) & nrow(d())>0){
        if(input$ChartaR_filter_list[1]=='no filter'){
          RVs$user_filter <- rep(1, nrow(d()))
        } else {
          filter_formula <- combine_filters(input$ChartaR_filter_list, input$filter_operation)
          RVs$user_filter <- tryCatch({d()[, eval(parse(text=filter_formula))]}, error = function(e){e})
          #RVs$user_filter <- tryCatch({d()[, eval(parse(text=input$ChartaR_filter_list))]}, error = function(e){e})
        }
        if(class(isolate(RVs$user_filter))[1]=='simpleError'){
          # something went wrong trying to evaluate the expression
          confirmSweetAlert(session = session,
                            type = 'error',
                            inputId = "filter_error",
                            title = 'Filter error',
                            text = isolate(RVs$user_filter$message),
                            btn_labels = c('OK')
          )
          RVs$user_filter <- rep(1, nrow(d()))
        } else if (min(isolate(RVs$user_filter))<0 | max(isolate(RVs$user_filter)>1) | length(unique(isolate(RVs$user_filter)))>2){
          # result does not evaluate to all 0's or 1's
          confirmSweetAlert(session = session,
                            type = 'error',
                            inputId = "filter_error",
                            title = 'Filter error',
                            text = 'Filter does not evaluate to TRUE/FALSE',
                            btn_labels = c('OK')
          )
          RVs$user_filter <- rep(1, nrow(d()))
        }
      }
    }
  })
  observeEvent(c(RVs$train_test_filter, RVs$user_filter, input$weight), {
    if(!is.null(d()) & !is.null(input$weight) & !is.null(RVs$user_filter)){
      if(input$ChartaR_training_test=='All'){
        st <- 'primary'
      } else if(input$ChartaR_training_test=='Train') {
        st <- 'warning'
      } else if (input$ChartaR_training_test=='Test') {
        st <- 'danger'
      }
      if(input$weight %in% c('N','no weights','')){
        value <- round(sum(RVs$train_test_filter*RVs$user_filter), 0)
        total <- round(sum(RVs$train_test_filter), 0)
      } else {
        value <- round(sum(d()[[input$weight]]*RVs$train_test_filter*RVs$user_filter), 0)
        total <- round(sum(d()[[input$weight]]*RVs$train_test_filter), 0)
      }
      updateProgressBar(session = session,
                        id = "ChartaR_pbar",
                        value = value,
                        total = total,
                        status = st
      )
    }
  })
  output$ChartaR_histogram_summary_table <- DT::renderDataTable({
    # only proceed if d and the filters have the same length
    if(nrow(d())==length(RVs$user_filter) &
       nrow(d())==length(RVs$train_test_filter) &
       input$response %in% names(d()) &
       input$weight %in% c(names(d()), 'N')
       ){
      stats_table <- make_histogram_summary_table(
        d(),
        input$response,
        input$weight,
        RVs$kpi_spec,
        RVs$user_filter,
        RVs$train_test_filter
      )
      return_histogram_summary_table_DT(stats_table)
    }
  })
  output$histogram <- plotly::renderPlotly({
    if(!is.null(input$response)){
      # only proceed if d and the filters have the same length
      if(nrow(d())==length(RVs$user_filter) &
         nrow(d())==length(RVs$train_test_filter) &
         input$response %in% names(d()) &
         input$weight %in% c(names(d()), 'N')
      ){
        if(input$weight=='N'){
          values <- d()[[input$response]]
        } else {
          values <- d()[[input$response]]/d()[[input$weight]]
        }
        values <- values[as.logical(RVs$train_test_filter * RVs$user_filter)]
        # sample down if selected
        if(input$ChartaR_histogram_use_sample=='Use 100k'){
          num_rows <- length(values)
          if(num_rows>100000){
            values <- values[sample(1:num_rows, 100000, replace = FALSE)]
          }
        }
        # divide by weight
        if(input$weight=='N'){
          title <- input$response
        } else {
          title <- paste(input$response, '/', input$weight)
        }
        # if there is an active kpi use that in the title
        kpi <- isolate(input$sidebar_kpi)
        if(!is.null(kpi)){
          if(kpi!='User defined'){
            title <- paste(kpi, '=', title)
          }
        }
        # make title bold
        title <- paste('<b>', title, '</b>')
        # set the number of bins, if NULL then plotly will automatically pick the number of bins
        num_bins <- as.numeric(input$ChartaR_histogram_num_bins)
        num_bins <- min(10000,num_bins)
        # parse the log scale choice
        x_scale <- ifelse(input$hist_log_scale %in% c('X axis','Both'), 'log','')
        y_scale <- ifelse(input$hist_log_scale %in% c('Y axis','Both'), 'log','')
        # render the plot
        p <- plot_ly(x = ~values,
                     type = "histogram",
                     nbinsx = num_bins,
                     histnorm = input$hist_normalise,
                     cumulative = list(enabled=as.logical(as.numeric(input$hist_inc_cum)))) %>%
          layout(hovermode = 'x') %>%
          layout(margin = list(l = 50, r = 50, b = 10, t = 70, pad = 4)) %>%
          layout(xaxis = list(title = '', type = x_scale)) %>%
          layout(yaxis = list(title = NULL, type = y_scale)) %>%
          layout(title = list(text = title, y = 0.95, font = list(size = 14))) %>%
          layout(bargap=0.1)
      }
    }
  })
  output$ChartaR_residual_error_table <- DT::renderDataTable({
    if(!is.null(residual_error_results())){
      if(input$ChartaR_training_test=='All'){
        dt <- residual_error_results()$all
      } else if(input$ChartaR_training_test=='Train'){
        dt <- residual_error_results()$train
      } else if (input$ChartaR_training_test=='Test'){
        dt <- residual_error_results()$test
      }
      num_rows <- nrow(dt)
      dt %>%
        DT::datatable(extensions = 'Buttons',
                      rownames= FALSE,
                      selection = 'single',
                      options = list(pageLength = num_rows,
                                     dom = 'Bfrtip',
                                     scrollX = T,
                                     scrollY = 'calc(100vh - 400px)',
                                     searchHighlight=TRUE,
                                     columnDefs=list(list(width="100px",targets="_all")),
                                     buttons =
                                       list('colvis', 'copy', list(
                                         extend = 'collection',
                                         buttons = list(list(extend='csv',filename = ''),
                                                        list(extend='excel',filename = ''),
                                                        list(extend='pdf',filename= '')
                                         ),
                                         text = 'Download'
                                       )
                                       )
                      )) %>%
        DT::formatStyle(columns = colnames(dt), fontSize = '80%', lineHeight='40%')
    }
  })
  observeEvent(input$ChartaR_run_A_vs_E_analysis, {
    if(!is.null(d())){
      if(is.null(A_vs_E_analysis_feature())){
        confirmSweetAlert(session = session, type = 'error', inputId = "build_error", title = 'No column selected', btn_labels = c('OK'))
      } else {
        # check for random columns, if not present then create them
        if(!('rand01' %in% names(d()))){
          set.seed(42)
          rand_cols <- paste0('rand',formatC(1:10,width = 2, flag='0'))
          d()[ , (rand_cols) := 0]
          d()[, (rand_cols) := lapply(.SD,stats::runif), .SDcols = rand_cols]
          RVs$dt_update <- RVs$dt_update + 1
        }
        if(is.null(RVs$feature_spec) | input$ChartaR_A_vs_E_feature_spec=='All features'){
          # if there is no feature specification or "All features" selected
          # run all columns through the residual error calc expect for the active feature & weight
          f <- setdiff(names(d()), c(input$response, input$weight))
        } else {
          feature <- NULL
          fs <- RVs$feature_spec
          f <- fs[fs[[input$ChartaR_A_vs_E_feature_spec]]=='feature', feature]
          f <- intersect(f, names(d()))
        }
        if (input$ChartaR_A_vs_E_apply_filter){
          idx <- which(RVs$user_filter==1)
        } else {
          idx <- 1:nrow(d())
        }
        res_errs <- residual_error(d(), idx, f, input$response, A_vs_E_analysis_feature(), input$weight, as.numeric(input$ChartaR_residual_error_min_exposure), input$ChartaR_A_vs_E_method)
        residual_error_results(res_errs)
      }
    }
  })
  observeEvent(input$ChartaR_A_vs_E_select_feature, {
    last_clicked <- input$ChartaR_residual_error_table_cell_clicked$value
    # extracts first feature in the supplied GLM term
    if(!is.null(last_clicked)){
      if(last_clicked %in% names(d())){
        updateSelectInput(session, inputId = 'ChartaR_x_axis_feature-selectInput', selected = last_clicked)
        updateTabItems(session, inputId = 'tabs', selected = 'ChartaR')
        updateNavbarPage(session = session, inputId = "ChartaR_tabsetPanel", selected = "One way summary")
      }
    }
  })
  observeEvent(RVs$feature_spec, {
    RVs$feature_specifications <- extract_feature_specifications(RVs$feature_spec)
    if(is.null(RVs$feature_specifications)){
      updateSelectInput(inputId = 'ChartaR_A_vs_E_feature_spec', choices = 'All features')
    } else {
      updateSelectInput(inputId = 'ChartaR_A_vs_E_feature_spec', choices = c(RVs$feature_specifications, 'All features'))
    }
  })
  one_way_x_axis_feature <- selectInput_server(
    id = "ChartaR_x_axis_feature",
    all_cols = reactive(names(d())),
    feature_spec = reactive(RVs$feature_spec),
    initial_selected = TRUE,
    update = reactive(RVs$dt_update)
  )
  one_way_add_columns <-  selectInput_server(
    id = "ChartaR_1W_add_columns",
    all_cols = reactive(RVs$numerical_cols),
    feature_spec = reactive(RVs$feature_spec),
    initial_selected = FALSE,
    update = reactive(RVs$dt_update)
  )
  observeEvent(one_way_x_axis_feature(), ignoreInit = TRUE, {
    if(!is.null(d())){
      sel <- character(0)
      if(is.numeric(d()[[one_way_x_axis_feature()]]) | class(d()[[one_way_x_axis_feature()]])[1] %in% c('IDate','Date','POSIXct')){
        if(!is.null(input$ChartaR_1W_banding)){
          if(input$ChartaR_1W_banding=='lock'){
            b <- ChartaR_1W_banding()
            sel <- 'lock'
          } else {
            b <- banding_guesser(d()[[one_way_x_axis_feature()]])
          }
        } else {
          b <- banding_guesser(d()[[one_way_x_axis_feature()]])
        }
        ChartaR_1W_banding(b)
        date_response <- class(d()[[input[['ChartaR_x_axis_feature-selectInput']]]])[1] %in% c('IDate','Date','POSIXct')
        t <- banding_displayed(ChartaR_1W_banding(), date_response)
        updateRadioGroupButtons(session,
                                inputId = 'ChartaR_1W_banding',
                                label = paste0('Banding (',t,')'),
                                selected = sel
        )
      } else {
        ChartaR_1W_banding(1)
        updateRadioGroupButtons(session,
                                inputId = 'ChartaR_1W_banding',
                                label = paste0('Non-numerical feature'),
                                selected = sel
        )
      }
    }
  })
  one_way_summary <- one_way_table_server(
     "ChartaR_one_way_table",
     isolate(d),
     reactive(input$response),
     one_way_add_columns,
     reactive(input$weight),
     one_way_x_axis_feature,
     ChartaR_1W_banding,
     reactive(input$ChartaR_1W_sort_order),
     reactive(input$ChartaR_1W_group_low_exposure),
     reactive(input$ChartaR_A_vs_E_show_partial_dependencies),
     reactive(input$ChartaR_1W_y_transform),
     reactive(RVs$train_test_filter),
     reactive(RVs$user_filter),
     reactive(RVs$kpi_spec),
     reactive(RVs$feature_spec),
     reactive(RVs$dt_update),
     reactive(input$BoostaR_objective),
     reactive(input$GlimmaR_objective)
     )
  one_way_chart_server(
     "ChartaR_one_way_chart",
     one_way_summary,
     reactive(input$response),
     reactive(input$weight),
     reactive(input$ChartaR_A_vs_E_show_labels),
     reactive(input$ChartaR_show_response),
     reactive(input$ChartaR_1W_error_bars),
     reactive(RVs$kpi_spec),
     RVs$GlimmaR_models,
     reactive(input$ChartaR_training_test),
     reactive(input$ChartaR_filter_list),
     reactive(input$filter_operation)
     )
  A_vs_E_analysis_feature <- selectInput_server(
    id = "ChartaR_A_vs_E_feature",
    all_cols = reactive(names(d())),
    feature_spec = reactive(RVs$feature_spec),
    initial_selected = TRUE,
    update = reactive(RVs$dt_update)
  )
}

