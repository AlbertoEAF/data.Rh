

#' Same as melt but melts all columns in the data.table
#'
#' @param data - data.table
#' @param variable.factor (default=FALSE) - same as in data.table but with new default
#' @param ...  - all other params of the
melt_all <- function(data, variable.factor=F, ...) {
  assertthat::assert_that(is.data.table(data))
  melt(data, measure.vars=names(data), variable.factor=variable.factor, ...)
}

#' Returns the column types for a data.table
#'
#' @param data - a data.table
#'
#' @return column types data.table with columns:
#'     variable  - name of the variable
#'     type      - type of the variable
#'     is_factor - TRUE/FALSE if the variable column is a factor
#'
#' @examples
#'     coltypes(data.table(iris))
#'     coltypes(data.table(iris))[type=="double"]
#'
#' @export
#'
coltypes <- function(data) {
  assertthat::assert_that(is.data.table(data))
  types    <- melt_all(data[, lapply(.SD, typeof   )], value.name="type")
  factors  <- melt_all(data[, lapply(.SD, is.factor)], value.name="is_factor")
  numerics <- melt_all(data[, lapply(.SD, is.numeric)], value.name="is_numeric")
  merge(merge(types,factors), numerics)
}


#' Applies a function to columns of a data.table
#'
#' You can apply a function independently to many columns at the same time.
#' You can apply a function to:
#'  - all columns
#'  - specified columns (with cols=c("col1","col2", ...))
#'  - cols whose name match cols_pattern
#'
#' @param data   - must be a data.table
#' @param fn     - function to apply to the columns
#' @param by     - same as in data.table
#' @param cols   - a character vector. If used only columns with such names will be used.
#' @param cols_pattern - a character specifying the cols name pattern to which the function will be applied
#' @param coltype_filter_fn - a function that filters the coltypes object (applied at the end). Takes a coltypes data.table and returns it filtered.
#' @param prefix - the prefix to apply to the resulting column names
#' @param suffix - the suffix to apply to the resulting column names
#' @param inplace (default==FALSE) - TRUE/FALSE
#'
#' @return data.table with remapped columns
#'
#' @examples
#'     capply(data.table(iris), typeof)
#'     capply(data.table(iris), mean, cols_pattern="Length|Width")
#'     capply(data.table(iris), unique, cols=c("Species"))
#'     capply(data.table(iris), unique, c("Species")) # the same - cols is the first optional argument
#'     capply(data.table(iris), unique, "Species") # if the vector has dimension 1, this is equivalent
#'     capply(data.table(iris), function(x){x^2}, cols_pattern="Length|Width", inplace=T) # square all numeric columns
#'
#' @export
#'
colapply <- function(data, fn,
                     cols=c(), cols_pattern=c(),
                     prefix="", suffix="",
                     by=NULL, coltypes_filter_fn=NULL,
                     inplace=F) {
  assertthat::assert_that(data.table::is.data.table(data),
                          msg = "capply requires a data.table")
  assertthat::assert_that(is.null(cols) | is.null(cols_pattern),
                          msg = "Either specify cols or cols_pattern, not both.")

  all_cols <- names(data)

  if (is.null(cols) & is.null(cols_pattern))
    selected_cols <- all_cols
  else if (!is.null(cols))
    selected_cols <- cols
  else
    selected_cols <- all_cols[grep(cols_pattern, all_cols)]


  if (!missing(coltypes_filter_fn)) {
    coltypes_filtered_vars <- coltypes_filter_fn(coltypes(data))$variable
    selected_cols <- selected_cols[selected_cols %in% coltypes_filtered_vars]
  }

  renamed_selected_cols <- paste(prefix, selected_cols, suffix, sep="")


  if (inplace)
    data[, (renamed_selected_cols) := lapply(.SD, fn), .SDcols=selected_cols, by=by]
  else

    data[, { # rename the result list after applying the function
      result <- lapply(.SD, fn)
      names(result) <- renamed_selected_cols
      result
      }, .SDcols=selected_cols, by=by]
}



