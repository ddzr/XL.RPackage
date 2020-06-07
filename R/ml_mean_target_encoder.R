#' @title Mean Target encoding is the process of replacing a categorical value with the mean of the target variable.
#' @description This function convert input columns to mean target encoding columns, inspired from https://www.kaggle.com/ogrellier/python-target-encoding-for-categorical-features
#' @param mean_target_colnames list of column names that needs mean target encoding
#' @param target_colname target column name, should be numeric column
#' @param training_data dataset we used to build mean targeting table
#' @param test_data dataset we used to apply mean target encoding
#' @param fold list of fold index for training dataset, suggest to use caret::createFolds
#' @param min_samples_leaf minimum quantity per column value we consider needs to be calculate mean.
#' @param smoothing used to control rate of transition between the particular column level’s posterior probability and the grand target mean, default is set to 1 to ignore this effect
#' @param impute_missing determine if we want to impute missing when new dataste levels could not be found in mapping table, defualt is to impute
#' @return a list that contains datasets that haven been applied mean target encoding and also a mapping table named calculate_columns_mean_table to use for ml_mean_target_apply function ta apply mean targeting encoding on new dataset
#' @import data.table
#' @export
#' @seealso \link{ml+mean_target_apply}


ml_mean_target_encoder <-
  function(mean_target_colnames,
           target_colname,
           training_data,
           test_data = NULL,
           fold = NULL,
           min_samples_leaf = 0,
           smoothing = 1,
           impute_missing=T) {

    if (!is.data.table(training_data)) {
      training_data <- data.table::copy(training_data)
      setDT(training_data)
    }

    if (!is.null(test_data) & !is.data.table(test_data)) {
      test_data <- data.table::copy(test_data)
      setDT(test_data)
    }
    if (is.null(fold)) {
      calculate_columns_mean_table <-
        .calculate_columns_means(
          mean_target_colnames = mean_target_colnames,
          target_colname = target_colname,
          data = training_data,
          min_samples_leaf = min_samples_leaf,
          smoothing = smoothing
        )


      training_target_encoding_cols <-
        ml_mean_target_apply(calculate_columns_mean_table = calculate_columns_mean_table,
                             applying_dataset = training_data)

      if (is.null(test_data)) {
        return(
          list(
            training_target_encoding_cols = training_target_encoding_cols,
            calculate_columns_mean_table = calculate_columns_mean_table
          )
        )
      } else {



        test_target_encoding_cols <-
          ml_mean_target_apply(calculate_columns_mean_table = calculate_columns_mean_table,
                               applying_dataset =
                                 test_data, impute_missing=impute_missing)


        return(
          list(
            training_target_encoding_cols = training_target_encoding_cols,
            test_target_encoding_cols = test_target_encoding_cols,
            calculate_columns_mean_table = calculate_columns_mean_table
          )
        )

      }

    } else {
      mean_target_table_oof <-
        matrix(nrow = nrow(training_data),
               ncol = length(mean_target_colnames))
      mean_target_table_oof[] <-
        mean(training_data[[target_colname]], na.rm = T)
      mean_target_table_oof <- as.data.table(mean_target_table_oof)

      mean_target_table_infold <- list()

      for (i in seq_along(fold)) {
        mean_target_table_infold[[i]] <-
          .calculate_columns_means(
            mean_target_colnames,
            target_colname = target_colname,
            data = training_data[-fold[[i]], ],
            min_samples_leaf = min_samples_leaf,
            smoothing = smoothing
          )

        mean_target_table_oof[fold[[i]], ] <-
          ml_mean_target_apply(mean_target_table_infold[[i]],
                               applying_dataset = training_data[fold[[i]], ], impute_missing=impute_missing)
      }



      mapping_for_test_dataset <-
        data.table::rbindlist(mean_target_table_infold, idcol = 'fold')[, .(
          mean_target = mean(mean_target),
          count =
            mean(count),
          smoothing_col =
            mean(smoothing_col),
          prior =
            mean(prior),
          mean_target_adj =
            mean(mean_target_adj)
        ),
        by =
          c('var_value', 'var_name', 'min_samples_leaf', 'smoothing')]


      setnames(
        mean_target_table_oof,
        names(mean_target_table_oof),
        paste0('mean_target_encoding_', mean_target_colnames)
      )



      if (is.null(test_data)) {
        return(
          list(
            training_target_encoding_cols = mean_target_table_oof,
            calculate_columns_mean_table = mapping_for_test_dataset,
            fold_calcualted_columns_mean_table = mean_target_table_infold
          )
        )
      } else {
        test_dataset_encoding <-
          ml_mean_target_apply(mapping_for_test_dataset,
                               applying_dataset = test_data, impute_missing=impute_missing)

        return(
          list(
            training_target_encoding_cols = mean_target_table_oof,
            test_target_encoding_cols = test_dataset_encoding,
            calculate_columns_mean_table = mapping_for_test_dataset,
            fold_calcualted_columns_mean_table = mean_target_table_oof
          )
        )

      }

    }

  }
