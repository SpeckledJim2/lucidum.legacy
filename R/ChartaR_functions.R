make_histogram_summary_table <- function(d, response, weight, kpi_spec, filter, train_test_filter){
  Value_temp <- NULL
  Value <- NULL
  Statistic <- NULL
  if(!is.null(response) & !is.null(d)){
    if(response %in% names(d) & weight %in% c(names(d),'N')){
      if(weight=='N'){
        value <- d[[response]][as.logical(filter*train_test_filter)]
        weighted_mean <- mean(value, na.rm = TRUE)
      } else {
        value <- d[[response]][as.logical(filter*train_test_filter)]/d[[weight]][as.logical(filter*train_test_filter)]
        value <- value[!is.infinite(value)]
        value <- value[!is.nan(value)]
        weighted_mean <- sum(d[[response]][as.logical(filter*train_test_filter)], na.rm = TRUE)/sum(d[[weight]][as.logical(filter*train_test_filter)], na.rm = TRUE)
      }
      # sample down if selected
      modes <- modes(value)
      # empty data frame to hold the data summary
      x <- data.frame(Statistic=character(), Value = numeric())
      # statistics
      x <- rbind(x, data.frame(Statistic='Numeric count', Value = sum(ifelse(!is.na(value),1,0))))
      x <- rbind(x, data.frame(Statistic='NA count', Value = sum(ifelse(is.na(value),1,0))))
      x <- rbind(x, data.frame(Statistic='Zero count', Value = sum(ifelse(value==0,1,0), na.rm = TRUE)))
      x <- rbind(x, data.frame(Statistic='Number of modes', Value = length(modes)))
      x <- rbind(x, data.frame(Statistic='Mean', Value = mean(value, na.rm = TRUE)))
      x <- rbind(x, data.frame(Statistic='Weighted mean', Value = weighted_mean))
      x <- rbind(x, data.frame(Statistic='First mode', Value = modes[1]))
      x <- rbind(x, data.frame(Statistic='Std deviation', Value = stats::sd(value, na.rm = TRUE)))
      # percentiles
      x <- rbind(x, data.frame(Statistic='Minimum', Value = min(value, na.rm = TRUE)))
      x <- rbind(x, data.frame(Statistic='0.1st percentile', Value = stats::quantile(value, prob = 0.001, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='0.5th percentile', Value = stats::quantile(value, prob = 0.005, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='1st percentile', Value = stats::quantile(value, prob = 0.01, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='5th percentile', Value = stats::quantile(value, prob = 0.05, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='10th percentile', Value = stats::quantile(value, prob = 0.1, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='25th percentile', Value = stats::quantile(value, prob = 0.25, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='Median', Value = stats::quantile(value, prob = 0.5, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='75th percentile', Value = stats::quantile(value, prob = 0.75, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='90th percentile', Value = stats::quantile(value, prob = 0.9, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='95th percentile', Value = stats::quantile(value, prob = 0.95, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='99th percentile', Value = stats::quantile(value, prob = 0.99, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='99.5th percentile', Value = stats::quantile(value, prob = 0.995, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='99.9th percentile', Value = stats::quantile(value, prob = 0.999, na.rm = TRUE)[[1]]))
      x <- rbind(x, data.frame(Statistic='Maximum', Value = max(value, na.rm = TRUE)))
      data.table::setDT(x)
      rows_to_exclude <- c('Zero count','Numeric count','Number of modes','NA count')
      x[, Value_temp := apply_kpi_format(Value, response, weight, kpi_spec)]
      x[Statistic %in% rows_to_exclude, Value_temp := as.character(format(Value, big.mark = ','))]
      x[, Value := NULL]
      setnames(x, 'Value_temp','Value')
      return(x)
    }
  }
}
return_histogram_summary_table_DT <- function(d){
  d %>%
    DT::datatable(extensions = 'Buttons',
                  editable = TRUE,
                  rownames= FALSE,
                  options = list(pageLength = nrow(d),
                                 dom = 'Brt',
                                 scrollX = T,
                                 searchHighlight=TRUE,
                                 columnDefs=list(list(width="100px",targets="_all"),
                                                 list(className = 'dt-right', targets = 1)),
                                 buttons =
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = list(list(extend='csv',filename = ''),
                                                    list(extend='excel',filename = ''),
                                                    list(extend='pdf',filename= '')),
                                     text = 'Download')
                                   )
                  )) %>%
    DT::formatStyle(columns = colnames(d), fontSize = '85%', lineHeight='40%')
}
modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
banding_displayed <- function(b, date_response){
  if(!date_response){
    format(b, big.mark=',', scientific = FALSE)
  } else {
    if(b<=1){
      'day'
    } else if (b==2){
      'week'
    } else if (b==4){
      'month'
    } else if (b==5){
      'quarter'
    } else if (b>=10){
      'year'
    }
  }
}
residual_error <- function(d, idx, features, actual_col, fitted_col, weight_col, min_weight, method){
  grouping_col <- NULL
  count <- NULL
  wtd_res_squared <- NULL
  weight <- NULL
  actual <- NULL
  fitted <- NULL
  train_test <- NULL
  V1 <- NULL

  r <- data.frame(feature=as.character(), error=as.double(), stringsAsFactors = FALSE)
  r_train <- copy(r)
  r_test <- copy(r)
  c <- data.frame(feature=as.character(), level = as.character(), weight = as.numeric(), actual = as.numeric(), fitted = as.numeric(), ind = as.numeric())
  c_train <- copy(c)
  c_test <- copy(c)
  row_index <- 1
  random_cols <- c('rand01', 'rand02', 'rand03', 'rand04', 'rand05', 'rand06','rand07', 'rand08', 'rand09', 'rand10')
  features <- union(features, random_cols)
  withProgress(message = 'ChartaR', value = 0,{
    for (feature in features){
      quantile <- 0
      incProgress(1/length(features), detail = feature)
      # if xvar is numeric with >200 levels then replace xvar with the deciles of xvar
      if (feature %in% random_cols){
        # quicker than taking quantiles
        cuts <- floor(d[[feature]]*10)/10
      } else if(is.numeric(d[[feature]]) & nlevels(as.factor(d[[feature]])) > 200){
        # use the deciles for the calculation
        quantile <- 10
        cuts <- as.integer(cut(d[[feature]], unique(quantile(d[[feature]], probs=0:quantile/quantile, na.rm = TRUE)), include.lowest=TRUE))
      } else {
        cuts <- d[[feature]]
      }
      # get the columns we need to summarise
      if(weight_col == 'N'){
        cols_to_summarise <- c(actual_col, fitted_col)
      } else {
        cols_to_summarise <- c(weight_col, actual_col, fitted_col)
      }
      # make residual error summary
      cols <- c(cols_to_summarise, 'train_test')
      #d_cols <- d[, ..cols]
      d_cols <- d[, .SD, .SDcols = cols]
      d_cols[, grouping_col := cuts]
      if(length(idx)==nrow(d)){
        # cube summarises like a normal data.table summary but also leaves in total rows, marked by NAs
        d_summary <- cube(d_cols, j = c(list(count = .N), lapply(.SD, sum, na.rm = TRUE)), by = c('grouping_col', 'train_test'), .SDcols = cols_to_summarise)
      } else {
        d_summary <- cube(d_cols[idx], j = c(list(count = .N), lapply(.SD, sum, na.rm = TRUE)), by = c('grouping_col', 'train_test'), .SDcols = cols_to_summarise)
      }
      # remove count column if we have a weight specified and change names
      if(weight_col != 'N'){
        d_summary[, count := NULL]
      }
      names(d_summary) <- c('grouping_col','train_test','weight','actual','fitted')
      # delete rows with less than min weight
      d_summary <- d_summary[weight>min_weight,]
      # create residual error
      if(method=='Absolute'){
        d_summary[, wtd_res_squared := weight*(actual/weight-fitted/weight)^2]
      } else if (method=='Percentage'){
        d_summary[, wtd_res_squared := weight*(actual/fitted-1)^2]
      }
      # summarise by train_test
      summary <- d_summary[, sqrt(1/.N*sum(wtd_res_squared)/sum(weight)), by = 'train_test']
      # finish tables
      r[row_index, 'feature'] <- feature
      r[row_index, 'error'] <- signif(summary[is.na(train_test), V1],6)
      r_train[row_index, 'feature'] <- feature
      r_train[row_index, 'error'] <- signif(summary[train_test==0, V1],6)
      r_test[row_index, 'feature'] <- feature
      r_test[row_index, 'error'] <- signif(summary[train_test==1, V1],6)
      row_index <- row_index + 1
    }
  })
  # new column relative to randoms
  rand_error <- mean(r[substr(r$feature,1,4)=='rand','error'])
  rand_error_train <- mean(r_train[substr(r_train$feature,1,4)=='rand','error'])
  rand_error_test <- mean(r_test[substr(r_test$feature,1,4)=='rand','error'])
  rand_sd <- stats::sd(r[substr(r$feature,1,4)=='rand','error'])
  rand_sd_train <- stats::sd(r_train[substr(r_train$feature,1,4)=='rand','error'])
  rand_sd_test <- stats::sd(r_test[substr(r_test$feature,1,4)=='rand','error'])
  r$relative_error <- signif(r$error / rand_error,6)
  r_train$relative_error <- signif(r_train$error / rand_error_train,6)
  r_test$relative_error <- signif(r_test$error / rand_error_test,6)
  r$prob <- signif(stats::pnorm(r$error, rand_error, rand_sd),6)
  r_train$prob <- signif(stats::pnorm(r_train$error, rand_error_train, rand_sd_train),6)
  r_test$prob <- signif(stats::pnorm(r_test$error, rand_error_test, rand_sd_test),6)
  r <- r[order(-r$error),]
  r_train <- r_train[order(-r_train$error),]
  r_test <- r_test[order(-r_test$error),]
  return(list('all' = r, 'train' = r_train, 'test' = r_test))
}
combine_filters <- function(filters, operation){
  if(operation=='Logical AND'){
    op <- ' & '
  } else if (operation=='Logical OR'){
    op <- ' | '
  }
  if(is.null(filters)){
    filters
  } else if(length(filters)==1){
    filters
  } else {
    paste(filters, collapse = op)
  }
}
