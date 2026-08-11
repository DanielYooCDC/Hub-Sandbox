# Splits each uploaded auxiliary/micro-hub CSV into one file per model,
# dropping the "model" column, and writes results to
# model-output/MicroHub-<Model>/<file-date>-MicroHub-<Model>.csv

# --- Read the list of changed files from the environment ---------------------
raw <- Sys.getenv("CHANGED_FILES", unset = "")

files <- strsplit(raw, "\n", fixed = TRUE)[[1]]
files <- trimws(files)
files <- files[nzchar(files)]

if (length(files) == 0) {
  message("No files to process. Exiting.")
  quit(status = 0)
}

# --- Helper: extract YYYY-MM-DD from a filename ------------------------------
extract_date <- function(path) {
  fname <- basename(path)
  m <- regmatches(fname, regexpr("\\d{4}-\\d{2}-\\d{2}", fname))
  if (length(m) == 0) {
    stop(sprintf("Could not find a YYYY-MM-DD date in filename: %s", fname))
  }
  m
}

# --- Helper: turn a model value into a filesystem-safe token ----------------
# "Regular Baseline" -> "Regular-Baseline", "Ensemble" -> "Ensemble"
model_token <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", "_", x)
  x
}

# --- Process each file -------------------------------------------------------
for (path in files) {
  
  if (!file.exists(path)) {
    message(sprintf("Skipping missing file: %s", path))
    next
  }
  
  message(sprintf("Processing %s", path))
  
  file_date <- extract_date(path)
  
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE) |> 
    dplyr::mutate(reference_date = as.Date(lubridate::parse_date_time(reference_date, orders = c("Ymd", "mdy", "mdY"))),
                  target_end_date = as.Date(lubridate::parse_date_time(target_end_date, orders = c("Ymd", "mdy", "mdY"))))
  
  models <- unique(df[["model"]])
  
  for (mdl in models) {
    sub <- df[df[["model"]] == mdl, , drop = FALSE]
    
    # Drop the model column, keep all remaining columns.
    sub <- sub[, setdiff(names(sub), "model"), drop = FALSE] 
    sub <- cbind(sub[1], target = "SARI", sub[-1])
    names(sub)[names(sub) == "target_group"] <- "age_group"
    
    token   <- model_token(mdl)
    out_dir <- file.path("model-output", paste0("MicroHub-", token))
    
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      message(sprintf("Created directory: %s", out_dir))
    }
    
    out_file <- file.path(
      out_dir,
      sprintf("%s-MicroHub-%s.csv", file_date, token)
    )
    
    write.csv(sub, out_file, row.names = FALSE)
  }
}
