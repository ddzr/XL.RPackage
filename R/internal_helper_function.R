#' calculate single column mean of target by column value
#'
#' Output a data.table that contains single column mean target by column value
#' @param mean_target_colname single column name that needs mean target encoding
#' @param target_colname target column name, should be numeric
#' @param data  dataset used to calculate mean target, usually is the training dataset
#' @param min_samples_leaf minimum quantity per column value we consider needs to be calculate mean.
#' below two settings introduced here https://www.kaggle.com/ogrellier/python-target-encoding-for-categorical-features
#' @param smoothing used to adjust column value mean target from grand target value, default value 1 is to ignore grand target value
#' @return a data.table contains mean target per column value
#' @keywords internal
#' @import data.table


.calculate_single_colum_mean <-
  function(mean_target_colname,
           target_colname,
           data,
           min_samples_leaf = 0,
           smoothing = 1) {

    if (!is.data.table(data)) {
      data <- data.table::copy(data)
      setDT(data)
    }


    mean_target_table <-
      data[, .(mean_target = mean(get(target_colname)), count = .N), by = mean_target_colname]
    mean_target_table$smoothing_col <-
      1 / (1 + exp(-(
        mean_target_table[["count"]] - min_samples_leaf
      ) / smoothing)) # this smoothing is not exactly working

    if (sum(is.na(data[[target_colname]])) > 0) {
      warning(paste0("there're missing values in target column", target_colname))
    }
    mean_target_table$prior = mean(data[[target_colname]], na.rm = T)

    mean_target_table[, mean_target_adj := (prior * (1 - smoothing_col) + mean_target * smoothing_col)]
    mean_target_table$min_samples_leaf <- min_samples_leaf
    mean_target_table$smoothing <- smoothing
    setnames(mean_target_table, mean_target_colname, 'var_value')
    mean_target_table[, var_name := mean_target_colname]
    return(mean_target_table)
  }


#' calculate list of columns mean of target by column value
#'
#' Output a data.table that contains list of columns mean target by column value
#' @param mean_target_colnames list of column names that needs mean target encoding
#' @param target_colname target column name, should be numeric
#' @param data  dataset used to calculate mean target, usually is the training dataset
#' @param min_samples_leaf minimum quantity per column value we consider needs to be calculate mean.
#' @param smoothing used to adjust column value mean target from grand target value, default value 1 is to ignore grand target value
#' @return a data.table contains mean target per column value for list of columns
#' @keywords internal
#' @import data.table
.calculate_columns_means <-
  function(mean_target_colnames,
           target_colname,
           data,
           min_samples_leaf = 0,
           smoothing = 1) {
    if (!(all(c(mean_target_colnames, target_colname) %in% names(data))))
      stop(paste0('input columns ', setdiff(c(mean_target_colnames, target_colname) , names(data)),
                  'not in dataset'))

    return(data.table::rbindlist(
      lapply(
        mean_target_colnames,
        .calculate_single_colum_mean,
        target_colname = target_colname,
        data = data,
        min_samples_leaf = min_samples_leaf,
        smoothing = smoothing
      )
    ))

  }

