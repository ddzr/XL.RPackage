#' @title Apply mean target encoding
#' @description This function is used to apply mean target encoding mapping table that was output from function ml_mean_target_encoder
#' @param calculate_columns_mean_table mean target encoding mapping table outputed from function ml_mean_target_encoder
#' @param applying_dataset new dataset that you wish to apply same mean target encoding
#' @param impute_missing determine if we want to impute missing when new dataste levels could not be found in mapping table, defualt is to impute
#' @return dataset that has been applied mean target encoding
#' @import data.table
#' @export
#' @seealso \link{ml_mean_target_encoder}

ml_mean_target_apply <-
  function(calculate_columns_mean_table,
           applying_dataset,impute_missing=T) {

    if (!is.data.table(data))
      calculate_columns_mean_table <-
        data.table(calculate_columns_mean_table)

    if (!is.data.table(applying_dataset)) {
      applying_dataset <- data.table::copy(applying_dataset)
      setDT(applying_dataset)
    }


    mean_target_col_list <-
      unique(calculate_columns_mean_table$var_name)

    if (!(all(mean_target_col_list %in% names(applying_dataset)))) {
      mean_target_col_list <-
        intersect(mean_target_col_list, names(applying_dataset))
      warning(
        paste0(
          'not all iput columns presents in inuput data, using below available columns ',
          mean_target_col_list
        )
      )
    }



    applied_mean_target_dt <- dcast(
      data = merge(
        melt(applying_dataset[, ..mean_target_col_list][, row_id := .I], id = "row_id"),
        calculate_columns_mean_table,
        by.x = c('value', 'variable'),
        by.y = c('var_value', 'var_name'),
        all.x = T
      ),
      row_id ~ variable ,
      value.var = 'mean_target_adj'
    )[order(row_id)][, row_id := NULL]

    setnames(
      applied_mean_target_dt,
      names(applied_mean_target_dt),
      paste0('mean_target_encoding_', names(applied_mean_target_dt))
    )

    #if new dataste levels could not be found in mapping table, default is to impute prior mean intead of leaving the mapping value to NA
    if (impute_missing){
      avg_impute_mean<-mean(calculate_columns_mean_table$mean_target_adj,na.rm=T)

      for (i in names(applied_mean_target_dt))
        applied_mean_target_dt[is.na(get(i)), (i):=avg_impute_mean]


    }


    return(applied_mean_target_dt)


  }

