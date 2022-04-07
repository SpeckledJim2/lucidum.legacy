#' @import lightgbm
#' @import DiagrammeR
#' @import stringr
#' @import rjson
BoostaR_server <- function(input, output, session, d, RVs){
  # gbm_features holds whatever features are selected in input$BoostaR_features
  BoostaR_feature_table <- reactiveVal(NULL) # holds the contents of input$BoostaR_features
  BoostaR_gain_summary <- reactiveVal(NULL)
  gbm_features <- reactiveVal(NULL)
  BoostaR_model_index <- reactiveVal(0)
  BoostaR_feature_table_selected <- reactiveVal(NULL)
  SHAP_feature_1_banding <- reactiveVal(1)
  SHAP_feature_2_banding <- reactiveVal(1)
  output$BoostaR_num_features <- renderUI({h3('Features')})
  observeEvent(input$BoostaR_navigate, {
    updateTabItems(session, inputId = 'tabs', selected = 'BoostaR')
    updateNavbarPage(session = session, inputId = "BoostaR_tabsetPanel", selected = "Model history")
  })
  observeEvent(c(input$response, input$weight), ignoreInit = TRUE, {
    if(!is.null(d())){
      guess <- objective_guesser(d(), input$response, input$weight, 'gbm')
      updateSelectInput(session, inputId = 'BoostaR_objective', selected = guess)
    }
  })
  observeEvent(input$BoostaR_prev_model, {
    if(length(RVs$BoostaR_models)>0){
      b <- BoostaR_model_index()
      BoostaR_model_index(max(1,b-1))
    }
  })
  observeEvent(input$BoostaR_next_model, {
    if(length(RVs$BoostaR_models)>0){
      b <- BoostaR_model_index()
      n <- length(RVs$BoostaR_models)
      BoostaR_model_index(min(n,b+1))
    }
  })
  observeEvent(BoostaR_model_index(), ignoreInit = TRUE, {
    # update tables and charts
    if(length(RVs$BoostaR_models)>0){
      # update the feature table
      BoostaR_model <- RVs$BoostaR_models[[BoostaR_model_index()]]
      dt <- BoostaR_model$feature_table
      output$BoostaR_features <- renderRHandsontable({update_BoostaR_feature_grid(dt, input$dimension[2] - 500)})
      # update the parameters
      update_GBM_parameters(session, output, BoostaR_model)
      # update the ChartaR secondary column
      lgbm_prediction <- NULL
      d()[, lgbm_prediction := .SD, .SDcols = BoostaR_model$pred_col_name]
      if(BoostaR_model$pred_col_name %in% names(d())){
        updateSelectInput(session, inputId = 'ChartaR_1W_add_columns-selectInput', selected = BoostaR_model$pred_col_name)
      }
      # update the response and weight
      use_kpi <- FALSE
      # if(!is.null(BoostaR_model$kpi)){
      #   if(BoostaR_model$kpi %in% RVs$kpi_spec[['kpi_name']]){
      #     use_kpi <- TRUE
      #   }
      # }
      if(use_kpi){
        updateSelectInput(session, inputId = 'sidebar_kpi', selected = BoostaR_model$kpi)
      } else {
        updateSelectInput(session, inputId = 'response', selected = BoostaR_model$response)
        updateSelectInput(session, inputId = 'weight', selected = BoostaR_model$weight)
        updateSelectInput(session, inputId = 'sidebar_kpi', selected = BoostaR_model$kpi)
      }
      # update the SHAP cols
      if(!is.null(BoostaR_model$SHAP_cols)){
        SHAP_cols <- names(d())[grep('_SHAP_', names(d()))] # get rid of any existing LP columns
        if(length(SHAP_cols)>0){
          d()[, (SHAP_cols) := NULL]
        }
        # append on new LP cols
        SHAP_rows <- BoostaR_model$SHAP_rows
        SHAP_cols <- BoostaR_model$SHAP_cols
        SHAP_names <- names(SHAP_cols[,2:ncol(SHAP_cols)])
        d()[SHAP_rows, (SHAP_names) := SHAP_cols[,2:ncol(SHAP_cols)]]
      }
    }
  })
  observeEvent(input$BoostaR_model_summary_cell_clicked$row, {
    model_index <- input$BoostaR_model_summary_cell_clicked$row
    BoostaR_model_index(model_index)
    #BoostaR_model <- RVs$BoostaR_models[[model_index]]
  })
  observeEvent(input$BoostaR_features, {
    include <- NULL
    feature <- NULL
    BoostaR_feature_table(setDT(rhandsontable::hot_to_r(input$BoostaR_features)))
    features <- BoostaR_feature_table()[include==TRUE, feature]
    gbm_features(features)
    int_group_2 <- BoostaR_feature_table()[, sum(include),interaction_grouping][order(interaction_grouping)]
    int_group_2 <- int_group_2[interaction_grouping!='',]
    if(nrow(int_group_2)>0){
      choices <- as.list(int_group_2[['interaction_grouping']])
      names(choices) <- paste0(choices, ' (', int_group_2[['V1']], ')')
    } else {
      choices <- NULL
    }
    updateSelectInput(session, inputId = 'BoostaR_interaction_contraints', choices = choices, selected = input$BoostaR_interaction_contraints)
    num_features <- BoostaR_feature_table()[,sum(include)]
    output$BoostaR_num_features <- renderUI({h3(paste(sep = '', 'Features (', num_features, ')'))})
  })
  observeEvent(c(input$BoostaR_feature_specification, RVs$feature_spec), ignoreInit = TRUE, {
    fs <- RVs$feature_spec
    if(!is.null(input$BoostaR_feature_specification) & !is.null(fs)){
      features <- fs[fs[[input$BoostaR_feature_specification]]=='feature', feature]
    } else {
      features <- NULL
    }
    dt <- populate_BoostaR_feature_grid(names(d()), features, fs, BoostaR_feature_table())
    output$BoostaR_features <- renderRHandsontable({update_BoostaR_feature_grid(dt, input$dimension[2] - 500)})
  })
  observeEvent(RVs$feature_spec, {
      RVs$feature_specifications <- extract_feature_specifications(RVs$feature_spec)
      if(is.null(RVs$feature_specifications)){
        updateSelectInput(inputId = 'BoostaR_feature_specification', choices = 'no feature specification')
      } else {
        updateSelectInput(inputId = 'BoostaR_feature_specification', choices = RVs$feature_specifications)
      }
  })
  observeEvent(input$BoostaR_goto_ChartaR, {
    r <- input$BoostaR_features_select$select$r
    if(!is.null(r)){
      last_clicked <- input$BoostaR_features$data[[r]][[1]]
      if(last_clicked %in% names(d()) & 'lgbm_prediction' %in% names(d())){
        updateSelectInput(session, inputId = 'ChartaR_x_axis_feature-selectInput', selected = last_clicked)
        updateSelectInput(session, inputId = 'ChartaR_1W_add_columns-selectInput', selected = 'lgbm_prediction')
        updateTabItems(session, inputId = 'tabs', selected = 'ChartaR')
        updateNavbarPage(session = session, inputId = "ChartaR_tabsetPanel", selected = "One way summary")
      }
    }
  })
  observeEvent(input$BoostaR_clear_features, {
    if(!is.null(BoostaR_feature_table())){
      dt <- BoostaR_feature_table()
      dt[, include := FALSE]
      output$BoostaR_features <- renderRHandsontable({update_BoostaR_feature_grid(dt, input$dimension[2] - 500)})
      updateSelectInput(session, inputId = 'BoostaR_feature_specification', selected = character(0))
    }
  })
  observeEvent(input$BoostaR_clear_interaction_groups, {
    if(!is.null(BoostaR_feature_table())){
      dt <- BoostaR_feature_table()
      groups <- input$BoostaR_interaction_contraints
      if(!is.null(groups)){
        interaction_grouping <- NULL
        dt[interaction_grouping %in% groups, include := FALSE]
      }
      output$BoostaR_features <- renderRHandsontable({update_BoostaR_feature_grid(dt, input$dimension[2] - 500)})
    }
  })
  observeEvent(input$BoostaR_grid_search, {
    if(input$BoostaR_grid_search=='Off'){
      learning_rate <- 0.3
      num_leaves <- 2
      max_depth <- 4
      col_sample_rate <- 1
      row_sample_rate <- 1
    } else {
      learning_rate <- c(0.1,0.5)
      num_leaves <- c(1,5)
      max_depth <- c(3,6)
      col_sample_rate <- c(0.5,1.0)
      row_sample_rate <- c(0.5,1.0)
    }
    output$BoostaR_learning_rate_UI <- renderUI({
      sliderInput(
        inputId = 'BoostaR_learning_rate',
        label = 'Learning rate',
        min = 0.01,
        max = 1,
        value = learning_rate,
        step = 0.01,
        ticks = FALSE,
        width = '100%'
      )
    })
    output$BoostaR_num_leaves_UI <- renderUI({
      sliderInput(
        inputId = 'BoostaR_num_leaves',
        label = 'Number of leaves',
        min = 2,
        max = 10,
        value = num_leaves,
        step = 1,
        ticks = FALSE,
        width = '100%'
      )
    })
    output$BoostaR_max_depth_UI <- renderUI({
      sliderInput(
        inputId = 'BoostaR_max_depth',
        label = 'Max depth',
        min = 2,
        max = 10,
        value = max_depth,
        step = 1,
        ticks = FALSE,
        width = '100%'
      )
    })
    output$BoostaR_column_sample_rate_UI <- renderUI({
      sliderInput(
        inputId = 'BoostaR_column_sample_rate',
        label = 'Column sample rate',
        min = 0,
        max = 1,
        value = col_sample_rate,
        step = 0.05,
        ticks = FALSE,
        width = '100%'
      )
    })
    output$BoostaR_row_sample_rate_UI <- renderUI({
      sliderInput(
        inputId = 'BoostaR_row_sample_rate',
        label = 'Row sample rate',
        min = 0,
        max = 1,
        value = row_sample_rate,
        step = 0.05,
        ticks = FALSE,
        width = '100%'
      )
    })
  })
  observeEvent(input$BoostaR_build_model, {
    # set model features
    original_feature_table <- BoostaR_feature_table()
    features <- BoostaR_feature_table()[include==TRUE, feature]
    if(length(features)==0){
      # show error message
      confirmSweetAlert(session = session,
                        type = 'error',
                        inputId = "BoostaR_error",
                        title = "Error",
                        text = 'No features selected',
                        btn_labels = c('OK'))
    } else {
      # set the monotonicity constraints
      # not all objectives support monotonicity constraints so need to check
      objective <- NULL
      montonicity_possible <- lgbm_objectives[objective==input$BoostaR_objective][['montonicity_possible']]
      if(montonicity_possible){
        monotonicity <- NULL
        monotonicity_constraints <- convert_monotonicity_column(BoostaR_feature_table()[include==TRUE, monotonicity])
      }
      # set the feature interaction constraints
      groups_to_constrain <- input$BoostaR_interaction_contraints
      if(!is.null(groups_to_constrain)){
        #feature_interaction_constraints <- set_feature_interaction_constraints(features, RVs$feature_spec, groups_to_constrain)
        feature_interaction_constraints <- set_feature_interaction_constraints_new(original_feature_table, groups_to_constrain)
      } else {
        feature_interaction_constraints <- NULL
      }
      # loop over parameter combinations
      if(input$BoostaR_grid_search=='Off'){
        main_params_combos <- setDT(extract_main_lgbm_parameters(input))
      } else if(input$BoostaR_grid_search=='On') {
        main_params_combos <- setDT(get_main_params_combos(input))
      }
      # assemble the model parameters
        for(i in 1:nrow(main_params_combos)){
          withProgress(message = 'BoostaR', detail = 'training', {
          if(nrow(main_params_combos)==1){
            message <- 'BoostaR'
          } else {
            message <- paste0('BoostaR (', i,'/',nrow(main_params_combos),')')
          }
          setProgress(value = 0, message = message)
          # assemble parameters
          main_params <- main_params_combos[i,]
          #main_params <- extract_main_lgbm_parameters(input)
          main_params$metric <- metric_from_objective(main_params$objective)
          additional_params <- extract_additional_lgbm_parameters(input$BoostaR_additional_parameters)
          all_params <- c(main_params, additional_params)
          if(montonicity_possible){
            all_params <- c(all_params, monotone_constraints = list(monotonicity_constraints))
          }
          if(!is.null(feature_interaction_constraints)){
            all_params <- c(all_params, interaction_constraints = list(feature_interaction_constraints))
          }
          # build the model
          model_index <- length(RVs$BoostaR_models) + 1
          lgbm_results <- build_lgbm(d(), input$BoostaR_initial_score, input$response, input$weight, features, all_params, input$BoostaR_calculate_SHAP_values, model_index)
          if(lgbm_results$message!='success'){
            # show error message
            confirmSweetAlert(session = session,
                              type = 'error',
                              inputId = "BoostaR_error",
                              title = "Error",
                              text = lgbm_results$message,
                              btn_labels = c('OK'))
          } else {
            # extract feature importances and make predictions
            importances <- lgb.importance(lgbm_results$model, percentage = TRUE)
            importances[, 2:4] <- 100 * importances[, 2:4]
            # extract the evaluation log
            evaluation_log <- make_evaluation_log(lgbm_results, main_params)
            # refresh the feature grid with the model importances
            dt <- post_model_update_BoostaR_feature_grid(original_feature_table, importances)
            output$BoostaR_features <- renderRHandsontable({update_BoostaR_feature_grid(dt, input$dimension[2] - 500)})
            # extract the tree table
            tree_table <- lgb.model.dt.tree(lgbm_results$model)
            # extract the gain summarised by tree's feature combinations
            gain_summary <- create_gain_summary_from_tree_summary(tree_table)
            gain_summary <- gain_summary[order(-gain_summary$gain),]
            # create feature interaction constraint SHAP cols
            BoostaR_create_SHAP_indices(d(), feature_interaction_constraints, model_index)
            # add the model and associated stats to RVs$BoostaR_models
            BoostaR_model <- list(idx = model_index,
                                  run_time = lgbm_results$run_time,
                                  dataset = RVs$dataset_name,
                                  kpi = input$sidebar_kpi,
                                  response = input$response,
                                  weight = input$weight,
                                  offset = input$BoostaR_initial_score,
                                  lgbm = lgbm_results$model,
                                  rules = lgbm_results$rules,
                                  tree_table = tree_table,
                                  gain_summary = gain_summary,
                                  feature_table = dt,
                                  features = features,
                                  importances = importances,
                                  interaction_constraints = feature_interaction_constraints,
                                  main_params = main_params,
                                  additional_params = additional_params,
                                  evaluation_log = evaluation_log,
                                  time = Sys.time(),
                                  pred_col_name = lgbm_results$pred_col_name,
                                  SHAP_rows = lgbm_results$SHAP_rows,
                                  SHAP_cols = lgbm_results$SHAP_cols
            )
            # append model to list and set model_index to latest model
            RVs$BoostaR_models <- c(RVs$BoostaR_models, list(BoostaR_model))
            BoostaR_model_index(model_index)
          }
          })
        }
      # d has been updated, need to flag this
      RVs$dt_update <- RVs$dt_update + 1
    }
  })
  observe({
    model_index <- isolate(BoostaR_model_index())
    volumes <- c('Home' = fs::path_home(), shinyFiles::getVolumes()())
    shinyFileSave(input, 'BoostaR_save_model', roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$BoostaR_save_model)
    isolate({
      if (nrow(fileinfo) > 0) {
        if(!is.null(model_index)){
          lgb.save(RVs$BoostaR_models[[model_index]]$lgbm, fileinfo$datapath)
          confirmSweetAlert(session = session,
                            type = 'success',
                            inputId = "temp",
                            title = 'LightGBM saved',
                            btn_labels = c('OK')
                            )
        } else {
          confirmSweetAlert(session = session,
                            type = 'error',
                            inputId = "temp",
                            title = 'No model selected',
                            btn_labels = c('OK')
          )
        }
      }
    })
  })
  observeEvent(c(BoostaR_model_index(), RVs$BoostaR_models), ignoreInit = TRUE, {
    # update the GBM model summary table
    dt <- BoostaR_model_summary(RVs$BoostaR_models)
    model_index <- BoostaR_model_index()
    updateActionButton(session, inputId = 'BoostaR_navigate', label = paste0('GBMs (', BoostaR_model_index(),'/',length(RVs$BoostaR_models),')'))
    output$BoostaR_model_summary <- DT::renderDT({
      dt %>%
        DT::datatable(rownames= FALSE,
                      extensions = 'Buttons',
                      selection=list(mode="single", target="row", selected = c(model_index)),
                      options = list(pageLength = nrow(dt),
                                     initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                     dom = 'Bfrt',
                                     scrollX = T,
                                     scrollY = 'calc(20vh)',
                                     searchHighlight=TRUE,
                                     buttons =
                                       list('colvis', 'copy', list(
                                         extend = 'collection',
                                         buttons = list(list(extend='csv',filename = ''),
                                                        list(extend='excel',filename = ''),
                                                        list(extend='pdf',filename= '')),
                                         text = 'Download')
                                       )
                      )
        ) %>%
        DT::formatStyle(columns = colnames(dt), lineHeight='40%', fontSize = '80%')
    })
    # update the SHAP feature selectInputs to contain the features from the most recent model
    features <- RVs$BoostaR_models[[model_index]][['importances']][['Feature']]
    f1_sel <- input$BoostaR_SHAP_feature_1
    f2_sel <- input$BoostaR_SHAP_feature_2
    if(!is.null(f1_sel)){
      if(!(f1_sel %in% features)){
        f1_sel <- NULL
      }
    }
    if(!is.null(f2_sel)){
      if(!(f2_sel %in% c('none', features))){
        f2_sel <- NULL
      }
    }
    updateSelectInput(session, inputId = 'BoostaR_SHAP_feature_1', choices = features, selected = f1_sel)
    updateSelectInput(session, inputId = 'BoostaR_SHAP_feature_2', choices = c('none', features), selected = f2_sel)
  })
  observeEvent(c(d(), RVs$dt_update), {
    updateSelectInput(session, inputId = 'BoostaR_initial_score', choices = c('none', numerical_cols(d())))
  })
  output$BoostaR_evaluation_plot <- plotly::renderPlotly({
    if(BoostaR_model_index()>0){
      evaluation_plot(RVs$BoostaR_models[[BoostaR_model_index()]]$evaluation_log)
    }
  })
  output$BoostaR_tree_diagram <-   renderGrViz({
    #if(!is.null(input$BoostaR_model_summary_cell_clicked$row)){
    if(length(RVs$BoostaR_models)>0){
      #model_index <- input$BoostaR_model_summary_cell_clicked$row
      model_index <- BoostaR_model_index()
      updateSliderInput(session, 'BoostaR_tree_selector', max = RVs$BoostaR_models[[model_index]]$lgbm$best_iter-1)
      if(!is.na(model_index)){
        if(model_index <= length(RVs$BoostaR_models)){
          tree_index <- NULL
          tree_table <- RVs$BoostaR_models[[model_index]]$tree_table
          rules <- RVs$BoostaR_models[[model_index]]$rules
          tree <- tree_table[tree_index==input$BoostaR_tree_selector,]
          my_graph <- BoostaR_render_tree_graph(tree, rules)
          DiagrammeR::render_graph(my_graph)
        }
      }
    }
  })
  output$BoostaR_SHAP_SD <- renderUI({
    SHAP <- NULL
    SHAP_1 <- NULL
    SHAP_2 <- NULL
    cor <- NULL
    BoostaR_model_index() # so recalcs when this is changed
    f1 <- input$BoostaR_SHAP_feature_1
    f2 <- input$BoostaR_SHAP_feature_2
    weight <- input$weight
    if(!is.null(d()) & !is.null(f1) & !is.null(weight)){
      if(weight=='N'){
        idx <- 1:nrow(d())
      } else {
        idx <- which(d()[[weight]]>0)
      }
      if(is.null(f2)) f2 <- 'none'
      if(f2=='none'){
        col1 <- paste0('lgbm_SHAP_', f1)
        cols <- c(col1)
        d_cols <- d()[idx,.SD,.SDcols=cols]
        setnames(d_cols, 'SHAP')
        SHAP_sd <- d_cols[, stats::sd(SHAP)]
        text <- paste0('<b>sd: </b>', round(SHAP_sd, 4))
        result <- p(HTML(text), style = 'font-size: 14px; margin-top: 15px')
      } else {
        col1 <- paste0('lgbm_SHAP_', f1)
        col2 <- paste0('lgbm_SHAP_', f2)
        cols <- c(col1, col2)
        d_cols <- d()[idx,.SD,.SDcols=cols]
        setnames(d_cols, c('SHAP_1','SHAP_2'))
        d_cols[, SHAP := SHAP_1+SHAP_2]
        SHAP_sd <- d_cols[, stats::sd(SHAP)]
        SHAP_corr <- cor(d_cols[['SHAP_1']], d_cols[['SHAP_2']])
        text <- paste0('<b>sd: </b>', round(SHAP_sd, 4), '<b> cor: </b>', round(SHAP_corr,4))
        result <- p(HTML(text), style = 'font-size: 14px; margin-top: 15px')
      }
    } else {
      SHAP_sd <- 0
      text <- paste0('<b>sd: </b>', round(SHAP_sd, 4))
      result <- p(HTML(text), style = 'font-size: 14px; margin-top: 15px')
    }
    result
  })
  output$BoostaR_SHAP_plot <- plotly::renderPlotly({
    # plot for most recent model
    BoostaR_model_index() # so recalcs when this is changed
    if(length(RVs$BoostaR_models)>0){
      lgbm <- RVs$BoostaR_models[[length(RVs$BoostaR_models)]]
      viz_SHAP_chart(d(),
                     lgbm$weight,
                     input$BoostaR_SHAP_feature_1,
                     input$BoostaR_SHAP_feature_2,
                     SHAP_feature_1_banding(),
                     SHAP_feature_2_banding(),
                     input$BoostaR_SHAP_feature_1_factor,
                     input$BoostaR_SHAP_feature_2_factor,
                     input$BoostaR_SHAP_quantile,
                     input$dimension)
    }
  })
  observeEvent(input$BoostaR_SHAP_feature_1, ignoreInit = TRUE, {
    if(!is.null(d()) & input$BoostaR_SHAP_feature_1 %in% numerical_cols(d())){
      b <- banding_guesser(d()[[input$BoostaR_SHAP_feature_1]])
      SHAP_feature_1_banding(b)
      banding_text <- format(SHAP_feature_1_banding(), big.mark=',', scientific = FALSE)
      updateRadioGroupButtons(session,
                              inputId = 'BoostaR_SHAP_feature_1_banding',
                              label = paste0('Feature 1 banding (',banding_text,')'),
                              selected = character(0)
      )
    }
  })
  observeEvent(input$BoostaR_SHAP_feature_2, ignoreInit = TRUE, {
    if(!is.null(d()) & input$BoostaR_SHAP_feature_2 %in% numerical_cols(d())){
      b <- banding_guesser(d()[[input$BoostaR_SHAP_feature_2]])
      SHAP_feature_2_banding(b)
      banding_text <- format(SHAP_feature_2_banding(), big.mark=',', scientific = FALSE)
      updateRadioGroupButtons(session,
                              inputId = 'BoostaR_SHAP_feature_2_banding',
                              label = paste0('Feature 2 banding (',banding_text,')'),
                              selected = character(0)
      )
    }
  })
  observeEvent(input$BoostaR_SHAP_feature_1_banding, ignoreInit = TRUE, {
    x <- input$BoostaR_SHAP_feature_1_banding
    b <- SHAP_feature_1_banding()
    if(!is.null(x)){
      if(length(x)>0){
        if(x=='<'){
          SHAP_feature_1_banding(modify_banding_level(b, -1))
        } else if (x=='>'){
          SHAP_feature_1_banding(modify_banding_level(b, +1))
        } else if (x %in% c('0.01','0.1','1','5','10','50','100')){
          SHAP_feature_1_banding(as.numeric(x))
        }
        banding_text <- format(SHAP_feature_1_banding(), big.mark=',', scientific = FALSE)
        updateRadioGroupButtons(session,
                                inputId = 'BoostaR_SHAP_feature_1_banding',
                                label = paste0('Feature 1 banding (',banding_text,')'),
                                selected = character(0)
        )
      }
    }
  })
  observeEvent(input$BoostaR_SHAP_feature_2_banding, ignoreInit = TRUE, {
    x <- input$BoostaR_SHAP_feature_2_banding
    b <- SHAP_feature_2_banding()
    if(!is.null(x)){
      if(length(x)>0){
        if(x=='<'){
          SHAP_feature_2_banding(modify_banding_level(b, -1))
        } else if (x=='>'){
          SHAP_feature_2_banding(modify_banding_level(b, +1))
        } else if (x %in% c('0.01','0.1','1','5','10','50','100')){
          SHAP_feature_2_banding(as.numeric(x))
        }
        banding_text <- format(SHAP_feature_2_banding(), big.mark=',', scientific = FALSE)
        updateRadioGroupButtons(session,
                                inputId = 'BoostaR_SHAP_feature_2_banding',
                                label = paste0('Feature 2 banding (',banding_text,')'),
                                selected = character(0)
        )
      }
    }
  })
  observeEvent(input$BoostaR_gain_table_goto_ChartaR, {
    # this will go wrong if the table has been sorted
    if(length(RVs$BoostaR_models)>0){
      model_index <- BoostaR_model_index()
      rows_selected <- input$BoostaR_gain_summary_rows_selected
      if(length(rows_selected)==1){
        last_clicked <- RVs$BoostaR_models[[model_index]]$gain_summary[[1]][rows_selected]
        int_order <- str_count(last_clicked, ' x ') + 1
      }
      if(is.null(rows_selected)){
        confirmSweetAlert(session = session,
                          type = 'error',
                          inputId = "build_error",
                          title = 'Error',
                          text = 'Please select a 1D or 2D interaction row from the gain summary table',
                          btn_labels = c('OK'))
      } else if (int_order>2) {
        confirmSweetAlert(session = session,
                          type = 'error',
                          inputId = "build_error",
                          title = 'Error',
                          text = 'Please select a 1D or 2D interaction row from the gain summary table',
                          btn_labels = c('OK'))
      } else {
        if(int_order==1){
          f1 <- last_clicked
          updateSelectInput(session, inputId = 'ChartaR_x_axis_feature-selectInput', selected = f1)
          updateSelectInput(session, inputId = 'ChartaR_1W_add_columns-selectInput', selected = 'lgbm_prediction')
          updateTabItems(session, inputId = 'tabs', selected = 'ChartaR')
          updateNavbarPage(session = session, inputId = "ChartaR_tabsetPanel", selected = "One way summary")
        } else if(int_order==2){
          # extract features from table
          char_pos <- as.numeric(gregexpr(' x ', last_clicked))
          f1 <- substr(last_clicked, char_pos+3, nchar(last_clicked))
          f2 <- substr(last_clicked, 1,char_pos-1)
          updateSelectInput(session, inputId = 'BoostaR_SHAP_feature_1', selected = f1)
          updateSelectInput(session, inputId = 'BoostaR_SHAP_feature_2', selected = f2)
          showTab(inputId = 'BoostaR_tabsetPanel', target = 'SHAP viewer', select = TRUE)
        }
      }
    }
  })
  output$BoostaR_gain_summary <- DT::renderDataTable({
    #if(!is.null(input$BoostaR_model_summary_cell_clicked$row)){
    if(length(RVs$BoostaR_models)>0){
      #model_index <- input$BoostaR_model_summary_cell_clicked$row
      model_index <- BoostaR_model_index()
      if(!is.na(model_index)){
        gain_summary <- RVs$BoostaR_models[[model_index]]$gain_summary
        n_rows <- pmin(100, nrow(gain_summary))
        gain_summary[1:n_rows,] %>%
          DT::datatable(rownames= FALSE,
                        selection = 'single',
                        options = list(pageLength = n_rows,
                                       dom = 'rt',
                                       scrollX = T,
                                       scrollY = 'calc(80vh - 380px)'
                        )
          ) %>%
          DT::formatRound('gain', 0) %>%
          DT::formatPercentage('%', 1) %>%
          DT::formatStyle(columns = colnames(gain_summary), fontSize = '80%', lineHeight='70%')
      }
    }
  })
}
