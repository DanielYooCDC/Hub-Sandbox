library(dplyr)
library(lubridate)
library(readr)

# helper function
arrange_cols <- function(df, cols) {
  # Similar to `dplyr::arrange()`, but using base tools. 
  # The `do.call()` technique is a way to use the columns of a data frame
  # (or items of a list) as separate arguments to a function.
  df[do.call(order, df[cols]), ]
}

# Where we'll save things
time_series_file <- paste0(here::here("target-data"), "/time-series.csv")
oracle_output_file <- paste0(here::here("target-data"), "/oracle-output.csv")

# get target data
base_target_data <- readr::read_csv(file = paste0(here::here("target-data"), "/chile-target.csv"))

current_as_of <- #### GitHub action run date

base_target_data <- cbind(
  base_target_data,
  data.frame(as_of = as.Date(current_as_of))
) %>% mutate(target="SARI") |> 
  dplyr::select(target_end_date = date, age_group = target_group, observation = value, as_of, target) 

# get existing time series data
if (file.exists(time_series_file)) {
  existing_time_series <- readr::read_csv(file = time_series_file)
} else {
  existing_time_series <- data.frame(matrix(ncol = length(colnames(base_target_data)), nrow = 0))
  colnames(existing_time_series) <- colnames(base_target_data)
}

# if the existing time series already has entries for the as_of date we're using,
# remove those entries (to avoid duplicates if this script is re-run)
existing_time_series <- existing_time_series |>
  dplyr::filter(.data[["as_of"]] != as.Date(current_as_of))

# update time series
updated_time_series <- rbind(existing_time_series, base_target_data)
time_series_col_order <- c("as_of", "target", "target_end_date", "age_group")
updated_time_series <- arrange_cols(updated_time_series, time_series_col_order)
updated_time_series <- updated_time_series |>
  dplyr::select(all_of(time_series_col_order), everything())

# Specify sort order for target data files (not absolutely necessary, but helps human readibility and diffs)
oracle_output_cols <- c(
  "target", "age_group", "horizon", "target_end_date", "output_type", "output_type_id", "oracle_value", "as_of")
oracle_include_after = "2026-05-30"
oracle_output <- base_target_data[base_target_data$target_end_date >= oracle_include_after, ] |> 
  rename(oracle_value = observation) |> 
  dplyr::cross_join(
    # add a row for each horizon defined in the modeling task
    # (except horizon -1, which is not used for scoring/viz)
    data.frame(horizon = 0:3)
  ) |>
  dplyr::mutate(
    output_type = "quantile",
    output_type_id = NA,
  )
oracle_output <- arrange_cols(oracle_output, oracle_output_cols)
oracle_output <- oracle_output |>
  dplyr::select(all_of(oracle_output_cols), everything())

# write time series
write.csv(updated_time_series, file = time_series_file, row.names = FALSE)
write.csv(oracle_output, file = oracle_output_file, row.names = FALSE)
