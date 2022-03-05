high_level_dataset_summary <- function(d, lucidum_cols){
  value <- NULL
  dataset <- c('R memory usage',
               'Number of rows',
               'Total columns',
               'Original columns',
               'lucidum columns',
               'Train/test'
               )
  summary <- data.table(dataset,value=NULL)
  train_test_present <- 'train_test' %in% names(d)
  if(!is.null(d)){
    number_of_lucidum_cols <- length(lucidum_cols)
    summary[dataset=='R memory usage', value := format(utils::object.size(d), units = 'MB')]
    summary[dataset=='Number of rows', value := nrow(d)]
    summary[dataset=='Total columns', value := ncol(d)]
    summary[dataset=='Original columns', value := ncol(d) - number_of_lucidum_cols]
    summary[dataset=='lucidum columns', value := number_of_lucidum_cols]
    summary[dataset=='Train/test', value := ifelse(train_test_present,'Present','Not present')]
  }
  summary
}
dataset_column_summary <- function(d, sample){
  if(sample & nrow(d)>10000){
    d <- d[sample(1:.N,10000, replace = FALSE),]
  }

  countNAs <- function(x){sum(is.na(x))}
  min2 <- function(x){
    if(class(x)=='factor'){
      x <- as.character(x)
    }
    min(x, na.rm = TRUE)
  }
  max2 <- function(x){
    if(class(x)=='factor'){
      x <- as.character(x)
    }
    max(x, na.rm = TRUE)
  }
  if(is.null(d)){
    names_col <- 'No dataset'
  } else {
    names_col <- names(d)
  }
  summary <- data.table(name = names_col,
                        class = sapply(d, class),
                        type = sapply(d, typeof),
                        mean = sapply(d, mean, na.rm = TRUE),
                        min = sapply(d, min2),
                        max = sapply(d, max2),
                        NAs = sapply(d, countNAs)
  )
}
dataset_column_summary_2 <- function(d, sample){
  if(is.null(d)){
    result <- data.table(class = 'select dataset from top right',
                         type = '',
                         mean = '',
                         max = '',
                         min = '',
                         countNAs = '')
  } else {
    names_col <- names(d)
    if(sample & nrow(d)>10000){
      d <- d[sample(1:.N,10000, replace = FALSE),]
      }
    result <- d[, sapply(.SD,
                          function(x) c(class = class(x)[1],
                                        type = typeof(x)[1],
                                        mean = mean2(x),
                                        min = min2(x),
                                        max = max2(x),
                                        NAs = countNAs(x)
                                        )
                          )
                 ]
    result <- rbind(name = names(d), result)
    result <- as.data.table(t(result))
  }
  result
}
feature_summary <- function(x){
  # to prevent R CMD check notes
  value <- NULL
  count <- NULL
  if(is.null(x)){
  } else if(class(x)[1] %in% c('numeric','logical','integer','POSIXct','Date','IDate')){
    metrics <- c('Min',
                 '1st percentile',
                 '5th percentile',
                 '25th percentile',
                 'Median',
                 'Mean',
                 '75th percentile',
                 '95th percentile',
                 '99th percentile',
                 'Max',
                 'Standard deviation'
    )
    summary <- data.table(metrics,value)
    summary[metrics=='Min', value := min(x,na.rm=TRUE)]
    summary[metrics=='1st percentile', value := stats::quantile(x,prob=0.01,na.rm=TRUE)]
    summary[metrics=='5th percentile', value := stats::quantile(x,prob=0.05,na.rm=TRUE)]
    summary[metrics=='25th percentile', value := stats::quantile(x,prob=0.25,na.rm=TRUE)]
    summary[metrics=='Median', value := stats::quantile(x,prob=0.50,na.rm=TRUE)]
    summary[metrics=='Mean', value := mean(x, na.rm = TRUE)]
    summary[metrics=='75th percentile', value := stats::quantile(x,prob=0.75,na.rm=TRUE)]
    summary[metrics=='95th percentile', value := stats::quantile(x,prob=0.95,na.rm=TRUE)]
    summary[metrics=='99th percentile', value := stats::quantile(x,prob=0.99,na.rm=TRUE)]
    summary[metrics=='Max', value := max(x,na.rm=TRUE)]
    summary[metrics=='Standard deviation', value := stats::sd(x,na.rm=TRUE)]
    if(class(x)[1] %in% c('numeric')){
      summary[, value := signif(value, 6)]
    }
  } else {
    frequencies <- sort(table(x, useNA = 'ifany'),decreasing=TRUE)
    summary <- as.data.table(frequencies)
    names(summary) <- c('value','count')
    num_levels <- nrow(summary)
    if(num_levels>10000){
      other_total <- summary[10001:.N, sum(count)]
      summary <- summary[1:10000,]
      summary <- rbind(data.table(value='Levels outside top 10k', count = other_total), summary)
    }
  }
  if(class(x)[1] %in% c('IDate','POSIXct','Date')){
    summary[, value := as.character(as.Date(value))]
  }
  summary
}
numerical_cols <- function(d){
  names(d)[as.numeric(which(sapply(d, is.numeric)==TRUE))]
}
load_specification <- function(d, dataset_name, specification_path, spec_type){
  path <- paste0(specification_path, '/', spec_type, '_specifications')
  files <- list.files(path = path, pattern = '*.csv', recursive = FALSE, all.files = TRUE)
  feature_spec_search <- paste(sep='',dataset_name,'.csv')
  if(feature_spec_search %in% files){
    # read file and check it has the required cols
    spec <- data.table::fread(paste0(path,'/',feature_spec_search), sep = ',')
    valid_spec <- check_specification(spec, spec_type)
    if(valid_spec){
      # valid specification
      # make logical columns character - otherwise rhandontable will render as logical
      logical_cols <- names(spec)[which(as.vector(spec[,lapply(.SD, class)]) == "logical")]
      if(length(logical_cols)>0){
        spec[, (logical_cols):= lapply(.SD, as.character), .SDcols = logical_cols]
      }
      if(spec_type=='filter'){
        if(spec[[1]][1]!='no filter'){
          spec <- rbind(data.table(filter='no filter'), spec)
        }
      }
    } else {
      # invalid specification - use default
      spec <- specification_template(d, spec_type)
    }
  } else {
    # create blank specification
    spec <- specification_template(d, spec_type)
  }
  return(spec)
}
specification_template <- function(d, spec_type){
  if(spec_type=='feature'){
    spec <- data.table(feature=names(d),
                       base_level='',
                       min='',
                       max='',
                       banding='',
                       monotonicity='',
                       interaction_grouping='',
                       scenario1='',
                       scenario2='',
                       scenario3='')
  } else if (spec_type=='kpi'){
    spec <- data.table(kpi_name='User defined',
                       kpi_numerator='Numerator',
                       kpi_denominator='Denominator',
                       kpi_dp=0,
                       kpi_signif=0,
                       kpi_divisor=1,
                       kpi_prefix='',
                       kpi_suffix='')
  } else if (spec_type=='filter'){
    spec <- data.table(filter='no filter')
  } else {
    spec <- NULL
  }
  return(spec)
}
check_specification <- function(d, spec_type){
  required_cols <- list(feature = c('feature','base_level','min','max','interaction_grouping'),
                        kpi = c('kpi_name','kpi_numerator','kpi_denominator','kpi_dp','kpi_signif','kpi_divisor','kpi_prefix','kpi_suffix'),
                        filter = c('filter'))
  all(required_cols[[spec_type]] %in% names(d))
}
extract_feature_specifications <- function(d){
  if(is.null(d)){
    f_specs <- c('no feature specification')
  } else {
    if(nrow(d)>0){
      # get column headers, remove special columns used by Toolkit
      f_specs <- names(d)[2:ncol(d)]
      f_specs <- f_specs[!(f_specs %in% c('base_level','min','max','banding','use_mid_point','monotonicity','interaction_grouping'))]
    } else {
      f_specs <- c('no feature specification')
    }
  }
  f_specs
}
kpi_numerator_denominator <- function(kpi, kpi_spec){
  kpi_name <- NULL
  kpi_numerator <- NULL
  kpi_denominator <- NULL
  if(is.null(kpi)){
    components <- NULL
  } else {
    if(nrow(kpi_spec)>0){
      numerator <- kpi_spec[kpi_name==kpi, kpi_numerator]
      denominator <- kpi_spec[kpi_name==kpi, kpi_denominator]
      components <- list(numerator=numerator, denominator=denominator)
    } else {
      components <- NULL
    }
  }
  return(components)
}
countNAs <- function(x){sum(is.na(x))}
mean2 <- function(x){
  if(class(x)[1] %in% c('numeric','logical','integer')){
    as.character(signif(mean(x, na.rm = TRUE), 6))
  } else if (class(x)[1] %in% c('IDate','POSIXct','Date')) {
    as.character(as.Date(mean(x, na.rm = TRUE)))
  } else {
    NA
  }
}
min2 <- function(x){
  if(class(x)[1] %in% c('numeric','logical','integer')){
    as.character(signif(min(x, na.rm = TRUE), 6))
  } else if (class(x)[1] %in% c('IDate','POSIXct','Date')) {
    as.character(as.Date(min(x, na.rm = TRUE)))
  } else {
    NA
  }
}
max2 <- function(x){
  if(class(x)[1] %in% c('numeric','logical','integer')){
    as.character(signif(max(x, na.rm = TRUE), 6))
  } else if (class(x)[1] %in% c('IDate','POSIXct','Date')) {
    as.character(as.Date(max(x, na.rm = TRUE)))
  } else {
    NA
  }
}
