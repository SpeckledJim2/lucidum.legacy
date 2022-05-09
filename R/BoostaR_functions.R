populate_BoostaR_feature_grid <- function(all_features, selected_features, feature_spec, current_grid){
  feature <- NULL
  include <- NULL
  interaction_grouping <- NULL
  monotonicity <- NULL
  gain <- NULL
  if(!is.null(feature_spec) & !is.null(all_features)){
    dt <- data.table(feature = all_features, include = FALSE)
    dt[feature %in% selected_features, include := TRUE]
    setkey(dt, feature)
    setkey(feature_spec, feature)
    dt <- feature_spec[, c('feature','monotonicity','interaction_grouping')][dt]
    dt[is.na(dt)] <- ''
    if(!is.null(current_grid)){
      # merge on gains
      setkey(current_grid, feature)
      dt <- current_grid[, c('feature','gain')][dt]
      dt[is.na(gain), gain := 0]
      setorder(dt, -gain, -include, feature)
    } else {
      dt[, gain := 0]
      setorder(dt, -include, feature)
    }
    setcolorder(dt, c('feature','gain','include','interaction_grouping','monotonicity'))
  } else {
    dt <- data.table(feature = all_features, gain = 0, include = TRUE, interaction_grouping = '', monotonicity = '')
  }
  dt
}
post_model_update_BoostaR_feature_grid <- function(original_feature_grid, feature_importances){
  gain <- NULL
  feature <- NULL
  include <- NULL
  feature_importances <- feature_importances[, c('Feature','Gain')]
  names(feature_importances) <- c('feature','gain')
  setkey(original_feature_grid, feature)
  setkey(feature_importances, feature)
  original_feature_grid[, gain := NULL]
  dt <- feature_importances[original_feature_grid]
  dt[is.na(gain), gain := 0]
  setorder(dt, -include, -gain, feature)
  setcolorder(dt, c('feature','gain','include','interaction_grouping','monotonicity'))
  return(dt)
}
extract_main_lgbm_parameters <- function(input){
  list(
    objective = input$BoostaR_objective,
    num_iterations = as.numeric(input$BoostaR_num_rounds),
    early_stopping_round = as.numeric(input$BoostaR_early_stopping),
    learning_rate = input$BoostaR_learning_rate,
    num_leaves = input$BoostaR_num_leaves,
    max_depth = input$BoostaR_max_depth,
    feature_fraction = input$BoostaR_column_sample_rate,
    bagging_fraction = input$BoostaR_row_sample_rate,
    bagging_freq = 1
  )
}
build_lgbm <- function(d, init_score, response, weight, features, params, SHAP_sample, idx){
  include <- NULL
  feature <- NULL
  lgbm_prediction <- NULL
  ..features <- NULL
  objective <- NULL
  cat_features <- setdiff(features, numerical_cols(d))
  # perform checks here on whether it's possible to build an lgbm with the info supplied
  lgbm <- NULL
  if(!(response %in% names(d))) {
    message <- 'Response not in dataset'
  } else if(!(weight %in% c('N', names(d)))) {
    message <- 'Weight not in dataset'
  } else if(!('train_test' %in% names(d))){
    message <- 'No train_test column'
  } else {
    # only include non-zero weight rows in modelling dataset
    if(weight=='N'){
      rows_idx <- 1:nrow(d)
      rows_include <- rep(TRUE, nrow(d))
    } else {
      rows_idx <- which(d[[weight]]>0)
      rows_include <- d[[weight]]>0
    }
    if(weight != 'N' & anyNA(d[[weight]][rows_idx])){
      message <- 'NAs in weight'
    } else if (anyNA(d[[response]][rows_idx])){
      # there are NAs in the response - can't proceed
      message <- 'NAs in response'
    } else if (length(features)==0){
      # no features selected - can't proceed
      message <- 'No features selected'
    } else {
      # split into train and test
      train_test_non_zero_rows <- d[['train_test']][rows_include]
      train_ids <- which(d[['train_test']]==0 & rows_include)
      test_ids <- which(d[['train_test']]==1 & rows_include)
      all_response <- d[[response]][rows_include]
      train_response <- d[[response]][train_ids]
      test_response <- d[[response]][test_ids]
      # make lgbm datasets
      d_convert <- lightgbm::lgb.convert_with_rules(data = d[rows_include, ..features], rules = NULL)
      d_train <- d_convert$data[train_test_non_zero_rows==0]
      d_test <- d_convert$data[train_test_non_zero_rows==1]
      l_train <- lgb.Dataset(as.matrix(d_train), label=train_response, free_raw_data = FALSE, categorical_feature = cat_features)
      l_test <- lgb.Dataset.create.valid(l_train, as.matrix(d_test), label = test_response)
      # set the initial score if there is one
      link <- lgbm_objectives[objective==params$objective][['link']]
      offset <- NULL
      if(init_score!='none'){
        offset <- d[[init_score]]
      } else if (weight!='N'){
        offset <- d[[weight]]
      }
      if(!is.null(offset)){
        if(link=='log'){
          offset <- log(offset)
        } else if (link=='logit'){
          offset <- log(offset/(1-offset))
        }
        offset[is.infinite(offset)] <- 0
        lightgbm::set_field(l_train, 'init_score', offset[rows_include][train_test_non_zero_rows==0])
        lightgbm::set_field(l_test, 'init_score', offset[rows_include][train_test_non_zero_rows==1])
      }
      # build the model
      start_time <- Sys.time()
      lgbm <- tryCatch({
        lightgbm::lgb.train(
        params = params,
        data = l_train,
        valids = list('train'=l_train,'test'=l_test), # so we score both train and test data
        callbacks = list(cb.print.period(params$num_iterations)) # callback to enable progressbar to update
        )},
        error = function(e){e}
        )
      run_time <- Sys.time() - start_time
      if('simpleError' %in% class(lgbm)){
        # something went wrong
        message <- lgbm$message
        lgbm <- NULL
      } else {
        # get predictions
        incProgress(0.05, detail = 'predicting')
        pred_col_name <- paste0(response, '_lgbm_', idx)
        residual_col_name <- paste0(response, '_lgbm_res', idx)
        preds <- stats::predict(lgbm, as.matrix(d_convert$data), rawscore = TRUE)
        if(!is.null(offset)){
          # add on the offset
          preds <- preds + offset[rows_include]
        }
        if(link=='log'){
          preds <- exp(preds)
        } else if (link=='logit'){
          preds <- 1/(1+exp(-preds))
        }
        y <- d[[response]]
        if(any(!rows_include)){
          d[rows_idx, lgbm_prediction := preds]
          d[rows_idx, (pred_col_name) := preds]
          d[rows_idx, (residual_col_name) := all_response - preds]
          d[!rows_idx, lgbm_prediction := 0]
          d[!rows_idx, (pred_col_name)  := 0]
          d[!rows_idx, (residual_col_name)  := 0]
        } else {
          d[, lgbm_prediction := preds]
          d[, (pred_col_name) := preds]
          d[, (residual_col_name) := all_response - preds]
        }
        # get SHAP values and append to d
        incProgress(0.05, detail = 'SHAP values')
        SHAP_cols <- BoostaR_extract_SHAP_values(d_convert$data, lgbm, features, SHAP_sample, rows_idx)
        SHAP_rows <- SHAP_cols[['idx']]
        # get rid of any existing SHAP output columns
        existing_SHAP_cols <- names(d)[grep('_SHAP_', names(d))] # get rid of any existing SHAP columns
        if(length(existing_SHAP_cols)>0){
          d[, (existing_SHAP_cols) := NULL]
        }
        # append on new SHAP cols
        if(!is.null(SHAP_cols)){
          SHAP_names <- names(SHAP_cols[,2:ncol(SHAP_cols)])
          d[SHAP_rows, (SHAP_names) := SHAP_cols[,2:ncol(SHAP_cols)]]
        }
      }
    }
  }
  if(is.null(lgbm)){
    return(
      list(message = message,
           model = NULL,
           rules = NULL,
           pred_col_name = NULL,
           SHAP_rows = NULL,
           SHAP_cols = NULL,
           run_time = NULL
           )
      )
  } else {
    return(
      list(message = 'success',
           model = lgbm,
           rules = d_convert$rules,
           pred_col_name = pred_col_name,
           SHAP_rows = SHAP_rows,
           SHAP_cols = SHAP_cols,
           run_time = run_time
           )
    )
  }
}
cb.print.period <- function(n) {
  # callback function to output iteration
  callback <- function(env = parent.frame()) {
    incProgress(0.9/n) # leave a bit for incProgresses below
  }
  attr(callback, 'call') <- match.call()
  attr(callback, 'name') <- 'cb.print.period'
  callback
}
convert_numerics <- function(x){
  converted <- as.numeric(x)
  ifelse(is.na(converted),x,converted)
}
extract_additional_lgbm_parameters <- function(x){
  # lgbm parameters
  x <- unlist(strsplit(x, '\n'))
  x <- gsub(' ','', x)
  if(length(grep('#', x))>0){
    x <- x[-grep('#', x)]
  }
  x <- strsplit(x, ':')
  result <- lapply(x, utils::tail, n = -1)
  names(result) <- lapply(x, utils::head, n = 1)
  result <- lapply(result, convert_numerics)
}
make_custom_fics <- function(x, features){
  x <- unlist(strsplit(x, '\n'))
  x <- gsub(' ','', x)
  if(length(grep('#', x))>0){
    x <- x[-grep('#', x)]
  }
  fics <- strsplit(x, 'x')
  # include all the features as one-way terms
  # so these will still be included in the model
  fics <- c(fics, features)
  # only keep fics that involve features
  keep <- sapply(fics, function(x){all(x %in% features)})
  fics <- fics[keep]
}
update_BoostaR_feature_grid <- function(dt, height){
  rhandsontable(
    dt,
    #outsideClickDeselects = FALSE,
    selectCallback = TRUE,
    readOnly = FALSE,
    rowHeaders = FALSE,
    columnSorting = TRUE,
    colWidths = c(40,8,8,20,15),
    height = height) %>%
    hot_table(stretchH = 'all', highlightRow = TRUE) %>%
    hot_col(c('gain','include'), valign='htCenter')  %>%
    hot_col('gain', format = "0.0000") %>%
    hot_col(c('feature','gain'), readOnly = TRUE) %>%
    hot_cols(manualColumnResize = TRUE)
}
convert_monotonicity_column <- function(x){
  m <- rep(0, length(x))
  m[x %in% c('Increasing','increasing','1')] <- 1
  m[x %in% c('Decreasing','decreasing','-1')] <- -1
  return(m)
}
set_feature_interaction_constraints <- function(model_features, feature_spec, groups_to_constrain = NULL){
  feature <- NULL
  interaction_grouping <- NULL
  include <- NULL
  if(is.null(feature_spec) | !('interaction_grouping' %in% names(feature_spec))){
    NULL
  } else {
    # get two columns from feature_spec and identify any model features not in the spec
    f <- feature_spec[, c('feature', 'interaction_grouping')]
    # keep rows from the kpi spec that involve model_features
    f <- f[feature %in% model_features,]
    # append any model_features not in f with no interaction grouping
    not_in_f <- setdiff(model_features, f$feature)
    if(length(not_in_f)>0){
      f <- rbind(f, cbind('feature' = not_in_f, 'interaction_grouping' = ''))
    }
    # delete the grouping where we don't want to use it
    # i.e. these features will be included alongside the non-constrained features
    f[!(interaction_grouping %in% groups_to_constrain), interaction_grouping := '']
    # split features into individual tables for each interaction grouping
    feature_groups <- split(f, by = 'interaction_grouping')
    # convert to list of character vectors
    c <- function(d){d[[1]]}
    lapply(feature_groups, c)
  }
}
set_feature_interaction_constraints_new <- function(feature_table, groups){
  include <- NULL
  interaction_grouping <- NULL
  if(is.null(groups)){
    fics <- NULL
  } else {
    features <- feature_table[include==TRUE, c('feature','interaction_grouping')]
    features[!(interaction_grouping %in% groups), 'interaction_grouping' := 'non_grouped']
    feature_groups <- split(features, by = 'interaction_grouping')
    c <- function(d){d[[1]]}
    fics <- lapply(feature_groups, c)
  }
return(fics)
}
metric_from_objective <- function(x){
  # define the objective, metric and initial score
  if(x=='mean_squared_error'){
    metric <- 'rmse'
  } else if(x=='binary'){
    metric <- 'binary_logloss'
  } else if(x %in% c('poisson','quasipoisson')){
    metric <- 'poisson'
  } else if(x=='gamma'){
    metric <- 'gamma'
  } else if(x=='tweedie'){
    metric <- 'tweedie'
  } else if(x=='mean_absolute_error'){
    metric <- 'l1'
  } else if(x=='mean_absolute_percentage_error'){
    metric <- 'mape'
  } else if(x=='huber'){
    metric <- 'huber'
  } else if(x=='fair'){
    metric <- 'fair'
  }
}
evaluation_plot <- function(evaluation_log){
  if(!is.null(evaluation_log)){
    train <- evaluation_log$train_log
    test <- evaluation_log$test_log
    # get into single table
    eval_results <- data.frame(iter = 1:length(train), model_train_error = train, model_test_error = test)
    # if we plot too many points it can slow down the browser - limit to 100 rows
    # always keep the first and last row
    ex_rows <- 1:2
    if(nrow(eval_results)>100){
      # make sure first and last row are kept
      rows_to_keep <- c(1, floor(1:100 * nrow(eval_results)/100), evaluation_log$best_iteration, nrow(eval_results))
      rows_to_keep <- unique(rows_to_keep)
      eval_results <- eval_results[rows_to_keep,]
      ex_rows <- 1:5
    }
    y_min <- min(eval_results$model_train_error[-ex_rows], eval_results$model_test_error[-ex_rows])
    y_max <- max(eval_results$model_train_error[-ex_rows], eval_results$model_test_error[-ex_rows])
    y_range <- y_max - y_min
    plot_ly(eval_results, hovertemplate = paste('(%{x}, %{y})')) %>%
      add_trace(x = ~iter, y = ~model_train_error, type = 'scatter', name = 'train', marker = list(color =  grDevices::rgb(255/255,0/255,0/255))) %>%
      add_trace(x = ~iter, y = ~model_test_error, type = 'scatter', name = 'test', marker = list(color =  grDevices::rgb(0/255,0/255,0/255))) %>%
      config(displayModeBar = FALSE) %>%
      layout(legend = list(orientation = 'v', x = 1.05, y = 0.6)) %>%
      layout(hovermode = 'x') %>%
      layout(margin = list(r = 25, l = 10, t = 50),
             title = list(text = paste0('<b>',
                                        'evaluation metric: ',
                                        evaluation_log$metric,
                                        '<br>',
                                        'test metric: ',
                                        signif(evaluation_log$test_err, 6),
                                        ', best iteration: ',
                                        evaluation_log$best_iteration,
                                        '</b>'),
                          y = 0.95,
                          xref = "plot",
                          font = list(size = 12, face='bold')
                          ),
             xaxis = list(titlefont = list(size=12)),
             yaxis = list(title = '', range = c(y_min - 0.05*y_range,y_max + 0.05*y_range)))
  }
}
BoostaR_model_summary <- function(BoostaR_models){
  # takes the key info from the BoostaR_models
  # and makes a summary data table
  rows <- lapply(BoostaR_models, BoostaR_model_summary_row)
  rbindlist(rows)
}
BoostaR_model_summary_row <- function(BoostaR_model){
  num_ICs <- ifelse(is.null(BoostaR_model$interaction_constraints),0,length(BoostaR_model$interaction_constraints)-1)
  x <- data.table(idx = BoostaR_model$idx,
                  time = round(BoostaR_model$run_time, 1),
                  data = BoostaR_model$dataset,
                  response = BoostaR_model$response,
                  weight = BoostaR_model$weight,
                  offset = BoostaR_model$offset,
                  obj = BoostaR_model$main_params$objective,
                  best_iter = BoostaR_model$evaluation_log$best_iteration,
                  test_err = signif(BoostaR_model$evaluation_log$test_err, 6),
                  train_err = signif(BoostaR_model$evaluation_log$train_err, 6),
                  lr = BoostaR_model$main_params$learning_rate,
                  leaves = BoostaR_model$main_params$num_leaves,
                  depth = BoostaR_model$main_params$max_depth,
                  row_smp = BoostaR_model$main_params$bagging_fraction,
                  col_smp = BoostaR_model$main_params$feature_fraction,
                  n_feat = length(BoostaR_model$features),
                  ICs = num_ICs
                  )
  return(x)
}
BoostaR_extract_SHAP_values <- function(d, lgbm, features, sample, rows_idx){
  if(sample=='No'){
    SHAP_cols <- NULL
  } else if (sample=='10k'){
    n_sample <- min(10000,nrow(d))
    idx <- sample(rows_idx, n_sample, replace = FALSE)
    SHAP_cols <- stats::predict(lgbm, as.matrix(d[idx,]), predcontrib = TRUE, num_iteration = lgbm$best_iter)
    SHAP_cols <- as.data.table(SHAP_cols)
    names(SHAP_cols) <- paste(sep = '_', 'lgbm_SHAP', c(features, 'base_score'))
    SHAP_cols <- cbind(idx = idx, SHAP_cols)
  } else if (sample=='All') {
    idx <- rows_idx
    SHAP_cols <- stats::predict(lgbm, as.matrix(d), predcontrib = TRUE, num_iteration = lgbm$best_iter)
    SHAP_cols <- as.data.table(SHAP_cols)
    names(SHAP_cols) <- paste(sep = '_', 'lgbm_SHAP', c(features, 'base_score'))
    SHAP_cols <- cbind(idx = idx, SHAP_cols)
  }
  return(SHAP_cols)
}
BoostaR_update_SHAP_cols <- function(d, SHAP_cols){

}
replace_lgbm_levels_with_names <- function(x, feature_name, rules){
  lvls <- rules[[feature_name]]
  result <- strsplit(x,'||', fixed = TRUE)
  result <- lapply(result, as.numeric)
  levels_to_names <- function(x){names(lvls)[as.numeric(x)]}
  result <- lapply(result, levels_to_names)
  result <- lapply(result, paste, collapse = '\n')
}
BoostaR_render_tree_graph <- function(dt, rules = NULL){
  Value <- NULL
  leaf_value <-  NULL
  internal_value <- NULL
  Quality <- NULL
  Feature <- NULL
  Cover <- NULL
  internal_count <- NULL
  leaf_count <- NULL
  Node <- NULL
  split_index <- NULL
  leaf_index <- NULL
  ID <- NULL
  Tree <- NULL
  parent <- NULL
  node_parent <- NULL
  leaf_parent <- NULL
  Yes <- NULL
  No <- NULL
  default_left <- NULL
  Missing <- NULL
  label <- NULL
  shape <- NULL
  filledcolor <- NULL
  Split <- NULL
  decision_type <- NULL
  setnames(dt, old = c('tree_index','split_feature','threshold','split_gain'), new = c('Tree','Feature','Split','Quality'))
  dt[, Value := leaf_value]
  dt[is.na(Value), Value := internal_value]
  dt[is.na(Quality), Quality := leaf_value]
  dt[is.na(Feature), Feature := 'Leaf']
  dt[, Cover := internal_count][Feature=='Leaf', Cover := leaf_count]
  dt[, c('leaf_count', 'internal_count','leaf_value','internal_value'):= NULL]
  dt[, Node := split_index]
  max_node <- max(dt[['Node']], na.rm = TRUE)
  dt[is.na(Node), Node := max_node + leaf_index +1]
  dt[, ID := paste(Tree, Node, sep = '-')]
  dt[, c('depth','leaf_index') := NULL]
  dt[, parent := node_parent][is.na(parent), parent := leaf_parent]
  dt[, c('node_parent', 'leaf_parent','split_index') := NULL]
  dt[, Yes := dt$ID[match(dt$Node, dt$parent)]]
  dt <- dt[nrow(dt):1,]
  dt[, No := dt$ID[match(dt$Node, dt$parent)]]
  dt[default_left==TRUE, Missing := Yes]
  dt[default_left==FALSE, Missing := No]
  dt[, c('parent', 'default_left') := NULL]
  setcolorder(dt, c('Tree','Node','ID','Feature','decision_type','Split','Yes','No','Missing','Quality','Cover','Value'))

  dt[, label:= paste0(Feature,
                      "\nCover: ", Cover,
                      ifelse(Feature == "Leaf", "", "\nGain: "), ifelse(Feature == "Leaf", "", round(Quality, 4)),
                      "\nValue: ", round(Value, 4)
  )]
  dt[Node == 0, label := paste0("Tree ", Tree, "\n", label)]
  dt[, shape:= "rectangle"][Feature == "Leaf", shape:= "oval"]
  dt[, filledcolor:= "Beige"][Feature == "Leaf", filledcolor:= "Khaki"]
  # in order to draw the first tree on top:
  dt <- dt[order(-Tree)]

  nodes <- DiagrammeR::create_node_df(
    n         = nrow(dt),
    ID        = dt$ID,
    label     = dt$label,
    fillcolor = dt$filledcolor,
    shape     = dt$shape,
    data      = dt$Feature,
    fontcolor = "black")

  # format the edge labels
  numeric_idx <- !is.na(as.numeric(dt[['Split']]))
  dt[numeric_idx, Split := round(as.numeric(Split),4)]

  # replace indices with feature levels if rules supplied
  if(!is.null(rules)){
    for (f in names(rules)){
      dt[Feature==f & decision_type == '==',
         Split := replace_lgbm_levels_with_names(Split, f, rules)]
    }
  }

  # replace long split names
  dt[nchar(Split)>500, Split := 'Split too long to render']

  edges <- DiagrammeR::create_edge_df(
    from  = match(dt[Feature != "Leaf", c(ID)] %>% rep(2), dt$ID),
    to    = match(dt[Feature != "Leaf", c(Yes, No)], dt$ID),
    label = dt[Feature != "Leaf", paste(decision_type, Split)] %>%
      c(rep("", nrow(dt[Feature != "Leaf"]))),
    style = dt[Feature != "Leaf", ifelse(Missing == Yes, "bold", "solid")] %>%
      c(dt[Feature != "Leaf", ifelse(Missing == No, "bold", "solid")]),
    rel   = "leading_to")

  graph <- DiagrammeR::create_graph(
    nodes_df = nodes,
    edges_df = edges,
    attr_theme = NULL
  ) %>%
    DiagrammeR::add_global_graph_attrs(
      attr_type = "graph",
      attr  = c("layout", "rankdir"),
      value = c("dot", "LR")
    ) %>%
    DiagrammeR::add_global_graph_attrs(
      attr_type = "node",
      attr  = c("color", "style", "fontname"),
      value = c("DimGray", "filled", "Helvetica")
    ) %>%
    DiagrammeR::add_global_graph_attrs(
      attr_type = "edge",
      attr  = c("color", "arrowsize", "arrowhead", "fontname"),
      value = c("DimGray", "1.5", "vee", "Helvetica"))



}
create_gain_summary_from_tree_summary <- function(trees){
  split_feature <- NULL
  tree_index <- NULL
  . <- NULL
  split_gain <- NULL
  split_features <- NULL
  gain_proportion <- NULL
  # get number of features in tree - i.e. interaction order
  int_order <- trees[, sum(!is.na(split_feature)), by = tree_index]
  max_int_depth <- max(int_order$V1)
  # split out features
  # sort trees by alphabetical feature
  setorder(trees, tree_index, split_feature)
  features <- trees[, .(split_features = toString(stats::na.omit(unique(split_feature)))), by = list(tree_index)]
  # gain
  gain <- trees[, list(gain = sum(split_gain, na.rm = TRUE)), by = tree_index]
  total_gain <- sum(gain$gain)
  # bind columns together
  summary <- cbind(features, gain = gain[[2]])
  # summarise by feature combinations, sorted by decreasing gain
  summary <- summary[, list(gain = sum(gain)), by = split_features]
  summary[, int_order := 1 + stringr::str_count(split_features, ',')]
  summary[, gain_proportion := gain/total_gain]
  summary[, split_features := gsub(', ',' x ', split_features)]
  setorder(summary, -gain)
  setcolorder(summary, c(1,3,2,4))
  names(summary) <- c('tree_features','dim','gain','%')
  return(summary)
}
BoostaR_predict <- function(d, RVs, idx, predict_col_name){
  result <- FALSE
  if(idx<=length(RVs$BoostaR_models) & class(d)[1]=='data.table'){
    b <- RVs$BoostaR_models[[idx]]
    model_features <- rjson::fromJSON(b$lgbm$dump_model())$feature_names
    if(all(model_features %in% names(d))){
      dat <- lgb.convert_with_rules(d[,.SD, .SDcols = model_features], rules = b$rules)
      d[, (predict_col_name) := stats::predict(b$lgbm, as.matrix(dat$data))]
      result <- TRUE
    }
  }
  return(result)
}
band_var <- function(x, q, b){
  lower_cutoff <- stats::quantile(x, prob = q, na.rm = TRUE)
  upper_cutoff <- stats::quantile(x, prob = 1-q, na.rm = TRUE)
  pmax(lower_cutoff, pmin(upper_cutoff, floor(x/b) * b))
}
viz_SHAP_chart <- function(d, weight, feature_1, feature_2, banding_1, banding_2, factor_1, factor_2, SHAP_quantile, dimensions){
  # SHAP cols is all the column names containing lgbm_SHAP_
  # idx is the index of the rows used to build the LGBM
  # below is default for
  chart_height <- as.numeric(dimensions[2]) - 270
  if(SHAP_quantile=='-'){
    q <- 0
  } else {
    q <- as.numeric(substr(SHAP_quantile,1,nchar(SHAP_quantile)-1))/100
  }
  p <- plotly_empty(height = chart_height, type = "scatter", mode = "markers") %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = 'No plot to show',yref = "paper", y = 0.5)
    )
  if(!is.null(d) & !is.null(weight) & !is.null(feature_1)){
    c1 <- class(d[[feature_1]])
    if(factor_1){
      c1 <- 'factor'
    }
    if(is.null(feature_2)){
      feature_2 <- 'none'
    } else {
      if(feature_2!='none'){
        c2 <- class(d[[feature_2]])
        if(factor_2){
          c2 <- 'factor'
        }
      }
    }
    if(weight=='N'){
      idx <- 1:nrow(d)
    } else {
      idx <- which(d[[weight]]>0)
    }
    if(feature_2=='none'){
      # 1D chart
      if(c1 %in% c('integer','numeric') & !factor_1){
        # flame chart by bands
        p <- SHAP_flame(d[idx], weight, feature_1, banding_1, q, chart_height)
      } else {
        # box and whisker in descending order by mean SHAP
        p <- SHAP_box_and_whisker(d[idx], weight, feature_1, banding_1, factor_1, q, chart_height)
      }
    } else {
      # 2D chart
      if(c1 %in% c('integer','numeric') & c2 %in% c('integer','numeric')){
        # surface plot
        p <- SHAP_surface(d[idx], weight, feature_1, feature_2, banding_1, banding_2, q, chart_height)
      } else if(!(c1 %in% c('integer','numeric')) & !(c2 %in% c('integer','numeric'))){
        # heat map, sorted on c1 something
        p <- SHAP_heatmap(d[idx], weight, feature_1, feature_2, banding_1, banding_2, factor_1, factor_2, q, chart_height)
      } else {
        # average SHAP value in bands for numerical feature cut by non-numerical feature
        p <- SHAP_lines(d[idx], weight, feature_1, feature_2, banding_1, banding_2, factor_1, factor_2, q, chart_height)
      }
    }
  }
  return(p)
}
SHAP_flame <- function(d, weight, feature_1, banding_1, q, chart_height){
  col1 <- paste0('lgbm_SHAP_', feature_1)
  banded <- band_var(d[[feature_1]], q, banding_1)
  SHAP_summary <- d[,c(min = lapply(.SD, min, na.rm = TRUE),
                       perc_5 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.05),
                       perc_25 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.25),
                       mean = lapply(.SD, mean, na.rm = TRUE),
                       perc_75 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.75),
                       perc_95 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.95),
                       max = lapply(.SD, max, na.rm = TRUE)
  ),
  banded,
  .SDcols = col1]
  names(SHAP_summary)[2:8] <- c('min','perc_5','perc_25','mean','perc_75','perc_95','max')
  setorderv(SHAP_summary, names(SHAP_summary)[1])
  p <- plot_ly(height = chart_height)
  p <- p %>%
    add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['mean']], type = 'scatter', mode = 'lines', yaxis = "y1",
              line = list(color = 'rgba(200, 50, 50, 1.0)', dash = 'dot'),
              showlegend = TRUE, name = 'SHAP_mean')
  # 5th-95th percentiles
  p <- p %>%
    add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['perc_5']], type = 'scatter', mode = 'lines', yaxis = "y1",
              fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
              showlegend = FALSE, name = 'SHAP_5') %>%
    add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['perc_95']], type = 'scatter', mode = 'lines', yaxis = "y1",
              fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
              showlegend = TRUE, name = 'SHAP_5_95')
  # 25th-75th percentiles
  p <- p %>%
    add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['perc_25']], type = 'scatter', mode = 'lines', yaxis = "y1",
              fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
              showlegend = FALSE, name = 'SHAP_25') %>%
    add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['perc_75']], type = 'scatter', mode = 'lines', yaxis = "y1",
              fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.2)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
              showlegend = TRUE, name = 'SHAP_25_75')
  # min to max SHAP
  p <- p %>%
    add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['min']], type = 'scatter', mode = 'lines', yaxis = "y1",
              fillcolor='rgba(200, 50, 50, 0.1)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
              showlegend = FALSE, name = 'SHAP_min') %>%
    add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['max']], type = 'scatter', mode = 'lines', yaxis = "y1",
              fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.1)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
              showlegend = TRUE, name = 'SHAP_min_max')
  # formatting
  p <- p %>% layout(title = list(y= 0.98, text = boldify(paste0('SHAP flame plot: ',feature_1)), font = list(size = 16, face='bold')))
  return(p)
}
SHAP_box_and_whisker <- function(d, weight, feature_1, banding_1, factor_1, q, chart_height){
  banded_1 <- NULL
  col1 <- paste0('lgbm_SHAP_', feature_1)
  cols <- c(feature_1, col1)
  d_cols <- d[,.SD,.SDcols=cols]
  setnames(d_cols, c('feature_1','SHAP'))
  if(factor_1 & (class(d[[feature_1]]) %in% c('integer','numeric'))){
    d_cols[, banded_1 := band_var(feature_1, q, banding_1)]
  } else {
    d_cols[, banded_1 := feature_1]
  }
  mean_SHAP <- d_cols[, lapply(.SD, mean), by = banded_1, .SDcols = 'SHAP']
  # reorder plot by mean_SHAP
  setorderv(mean_SHAP, 'SHAP', order = -1)
  xform <- list(autotick = TRUE,
                categoryorder = 'array',
                categoryarray = mean_SHAP[[1]])
  if(nrow(mean_SHAP)>500){
    p <- plotly_empty(height = chart_height, type = "scatter", mode = "markers") %>%
      config(displayModeBar = FALSE) %>%
      layout(title = list(text = 'Too many levels to display (>500)',yref = "paper", y = 0.5)
      )
  } else {
    if(nrow(d_cols)>50000){
      set.seed(42)
      d_cols <- d_cols[sample(1:.N, 50000, replace = FALSE),]
    }
    p <- plotly::plot_ly(
      height = chart_height,
      y = d_cols[[2]],
      color = if(factor_1){as.factor(d_cols[[3]])} else {d_cols[[3]]},
      boxmean = TRUE,
      boxpoints = FALSE,
      type = 'box',
    ) %>%
      layout(legend = list(title=list(text='
<b>Box and whisker plot</b>
Each box spans Q1 to Q3
Median is the solid line within box
Mean is the dashed line
Whiskers extend to min/max <br>'),
                           x = 1.05,
                           y = 1.05,
                           size = 50,
                           font = list(size = 10)
      )
      ) %>%
      layout(xaxis = xform,
             title = list(text = boldify(paste0('SHAP box plot: ',feature_1)), font = list(size = 16, face='bold'))
      )
  }

  return(p)
}
SHAP_surface <- function(d, weight, feature_1, feature_2, banding_1, banding_2, q, chart_height){
  SHAP <- NULL
  banded_1 <- NULL
  banded_2 <- NULL
  # two way summary
  col1 <- paste0('lgbm_SHAP_', feature_1)
  col2 <- paste0('lgbm_SHAP_', feature_2)
  cols <- c(feature_1, feature_2, col1, col2)
  d_cols <- d[,.SD,.SDcols=cols]
  setnames(d_cols, c('feature_1','feature_2','col1','col2'))
  d_cols[, SHAP := col1 + col2]
  d_cols[, banded_1 := band_var(feature_1, q, banding_1)]
  d_cols[, banded_2 := band_var(feature_2, q, banding_2)]
  #d_cols[, banded_1 := floor(feature_1/banding_1) * banding_1]
  #d_cols[, banded_2 := floor(feature_2/banding_2) * banding_2]
  d_summary <- d_cols[, list(SHAP = mean(SHAP)), by = c('banded_1', 'banded_2')]
  d_summary <- dcast(d_summary, stats::as.formula('banded_1 ~ banded_2'), value.var = 'SHAP')
  p <- plot_ly(height = chart_height,
               x = names(d_summary)[-1],
               y = d_summary[[1]],
               z = ~as.matrix(d_summary[,-1])) %>%
    add_surface(color = 'RdYlBlu') %>%
    layout(scene = list(xaxis = list(title = feature_2),
                        yaxis = list(title = feature_1),
                        zaxis = list(title = 'SHAP'),
                        camera = list(eye = list(x=0.8, y=-0.3, z=2))
    )
    )
  return(p)
}
SHAP_heatmap <- function(d, weight, feature_1, feature_2, banding_1, banding_2, factor_1, factor_2, q, chart_height){
  # two way summary
  SHAP <- NULL
  banded_1 <- NULL
  banded_2 <- NULL
  col1 <- paste0('lgbm_SHAP_', feature_1)
  col2 <- paste0('lgbm_SHAP_', feature_2)
  cols <- c(feature_1, feature_2, col1, col2)
  d_cols <- d[,.SD,.SDcols=cols]
  setnames(d_cols, c('feature_1','feature_2','col1','col2'))
  d_cols[, SHAP := col1 + col2]
  if(factor_1 & is.numeric(d[[feature_1]])){
    d_cols[, banded_1 := band_var(feature_1, q, banding_1)]
  } else {
    d_cols[, banded_1 := feature_1]
  }
  if(factor_2 & is.numeric(d[[feature_2]])){
    d_cols[, banded_2 := band_var(feature_2, q, banding_2)]
  } else {
    d_cols[, banded_2 := feature_2]
  }
  d_summary <- d_cols[, list(SHAP = mean(SHAP)), by = c('banded_1', 'banded_2')]
  d_summary <- dcast(d_summary, stats::as.formula('banded_1 ~ banded_2'), value.var = 'SHAP')

  p <- plotly::plot_ly(
    height = chart_height,
    # x = d_summary[[1]],
    # y = d_summary[[2]],
    # z = d_summary[['SHAP']],
    x = names(d_summary)[-1],
    y = d_summary[[1]],
    z = as.matrix(d_summary[,-1]),
    colors = grDevices::colorRamp(c('green', 'white', 'red')),
    type = "heatmap") %>%
    # add_annotations(
    #   x = names(d_summary)[-1],
    #   y = d_summary[[1]],
    #   text =  as.matrix(d_summary[,-1]),
    #   showarrow = FALSE) %>%
    layout(plot_bgcolor='rgb(200, 200, 200)') %>%
    layout(xaxis = list(showgrid = FALSE, title = feature_2), yaxis = list(showgrid = FALSE, title = feature_1))
  return(p)
}
SHAP_lines <- function(d, weight, feature_1, feature_2, banding_1, banding_2, factor_1, factor_2, q, chart_height){
  SHAP <- NULL
  banded_1 <- NULL
  banded_2 <- NULL
  col1 <- paste0('lgbm_SHAP_', feature_1)
  col2 <- paste0('lgbm_SHAP_', feature_2)
  if(factor_1 | (!factor_2 & is.numeric(d[[feature_2]]))){
    # swap around and make feature_1 the numeric feature
    temp <- feature_1
    feature_1 <- feature_2
    feature_2 <- temp
    temp <- banding_1
    banding_1 <- banding_2
    banding_2 <- temp
    temp <- factor_1
    factor_1 <- factor_2
    factor_2 <- temp
  }
  cols <- c(feature_1, feature_2, col1, col2)
  d_cols <- d[,.SD,.SDcols=cols]
  setnames(d_cols, c('feature_1','feature_2','col1','col2'))
  d_cols[, SHAP := col1 + col2]
  d_cols[, banded_1 := band_var(feature_1, q, banding_1)]
  if(is.numeric(d[[feature_2]])){
    d_cols[, banded_2 := band_var(feature_2, q, banding_2)]
  } else {
    d_cols[, banded_2 := feature_2]
  }
  d_summary <- d_cols[, list(SHAP = mean(SHAP)), by = c('banded_1', 'banded_2')]
  setorderv(d_summary, names(d_summary)[1])
  p <- plot_ly(d_summary,
               x = d_summary[[1]],
               y = d_summary[['SHAP']],
               color = as.factor(d_summary[[2]]),
               type = 'scatter',
               mode = 'lines',
               hovertemplate = paste('(%{x}, %{y:.3f})'),
               height = chart_height
  ) %>%
    plotly::layout(
      title = list(text = boldify(paste0('SHAP lines plot: ',feature_1, ' x ', feature_2)), font = list(size = 14), y= 0.98),
      xaxis = list(title = feature_1),
      yaxis = list(title = 'SHAP'),
      legend = list(title=list(text=boldify(feature_2))),
      hovermode = 'x'
    )
  return(p)
}
update_GBM_parameters <- function(session, output, BoostaR_model){
  updateTextInput(session, inputId = 'BoostaR_num_rounds', value = BoostaR_model$main_params$num_iterations)
  updateTextInput(session, inputId = 'BoostaR_early_stopping', value = BoostaR_model$main_params$early_stopping_round)
  updateSelectInput(session, inputId = 'BoostaR_objective', selected = BoostaR_model$main_params$objective)
  updateSelectInput(session, inputId = 'BoostaR_initial_score', selected = BoostaR_model$offset)
  updateRadioGroupButtons(session, inputId = 'BoostaR_grid_search', selected = 'Off')
  # updateSliderInput(session, inputId = 'BoostaR_learning_rate', value = BoostaR_model$main_params$learning_rate)
  # updateSliderInput(session, inputId = 'BoostaR_num_leaves', value = BoostaR_model$main_params$num_leaves)
  # updateSliderInput(session, inputId = 'BoostaR_max_depth', value = BoostaR_model$main_params$max_depth)
  # updateSliderInput(session, inputId = 'BoostaR_column_sample_rate', value = BoostaR_model$main_params$feature_fraction)
  # updateSliderInput(session, inputId = 'BoostaR_row_sample_rate', value = BoostaR_model$main_params$bagging_fraction)
  output$BoostaR_learning_rate_UI <- renderUI({
    sliderInput(
      inputId = 'BoostaR_learning_rate',
      label = 'Learning rate',
      min = 0.01,
      max = 1,
      value = BoostaR_model$main_params$learning_rate,
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
      value = BoostaR_model$main_params$num_leaves,
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
      value = BoostaR_model$main_params$max_depth,
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
      value = BoostaR_model$main_params$feature_fraction,
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
      value = BoostaR_model$main_params$bagging_fraction,
      step = 0.05,
      ticks = FALSE,
      width = '100%'
    )
  })
}
BoostaR_create_SHAP_indices <- function(d, feature_interaction_constraints, BoostaR_model_index){
  for(g in names(feature_interaction_constraints)){
    if(g!=''){
      new_col <- paste0(g, '_index_lgbm_', BoostaR_model_index)
      sum_cols <- paste0('lgbm_SHAP_', feature_interaction_constraints[[g]])
      d[, (new_col) := rowSums(.SD), .SDcols = sum_cols]
    }
  }
}
get_main_params_combos <- function(input){
  num_combos <- as.numeric(input$BoostaR_grid_combinations)
  learning_rate <- input$BoostaR_learning_rate
  num_leaves <- input$BoostaR_num_leaves
  max_depth <- input$BoostaR_max_depth
  feature_fraction <- input$BoostaR_column_sample_rate
  bagging_fraction <- input$BoostaR_row_sample_rate
  if((learning_rate[1]*20-floor(learning_rate[1]*20)==0)&learning_rate[2]*20-floor(learning_rate[2]*20)==0){
    lr_inc <- 0.05
  } else {
    lr_inc <- 0.01
  }
  learning_rate <- seq(learning_rate[1], learning_rate[2], lr_inc)
  num_leaves <- seq(num_leaves[1], num_leaves[2], 1)
  max_depth <- seq(max_depth[1], max_depth[2], 1)
  feature_fraction <- seq(feature_fraction[1], feature_fraction[2], 0.05)
  bagging_fraction <- seq(bagging_fraction[1], bagging_fraction[2], 0.05)
  params_grid <- expand.grid(objective = input$BoostaR_objective,
                             num_iterations = as.numeric(input$BoostaR_num_rounds),
                             early_stopping_round = as.numeric(input$BoostaR_early_stopping),
                             learning_rate = learning_rate,
                             num_leaves = num_leaves,
                             max_depth = max_depth,
                             feature_fraction = feature_fraction,
                             bagging_fraction = bagging_fraction,
                             bagging_freq = 1,
                             stringsAsFactors = FALSE
  )
  params_grid <- unique(params_grid)
  if(nrow(params_grid)>num_combos){
    rows_idx <- sample(1:nrow(params_grid), num_combos, replace = FALSE)
    params_grid <- params_grid[rows_idx,]
  }
  setorderv(params_grid, cols = names(params_grid)[4:8])
  return(params_grid)
}
make_evaluation_log <- function(lgbm_results, main_params){
  train_log <- lgb.get.eval.result(lgbm_results$model, "train", main_params$metric)
  test_log <- lgb.get.eval.result(lgbm_results$model, "test", main_params$metric)
  train_err <- train_log[lgbm_results$model$best_iter]
  test_err <- test_log[lgbm_results$model$best_iter]
  evaluation_log <- list(train_log = train_log,
                         test_log = test_log,
                         train_err = train_err,
                         test_err = test_err,
                         best_iteration = lgbm_results$model$best_iter,
                         metric = main_params$metric)
}
