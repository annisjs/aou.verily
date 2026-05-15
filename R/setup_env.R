#' Setup environment variables in Verily workbench
#'
#' @param bucket optional character string to set WORKSPACE_BUCKET environment variable
#' @param cdr optional character string to set WORKSPACE_CDR environment variable
#' @param google_project optional character string to set GOOGLE_PROJECT environment variable
#'
#' @details
#' If any or all arguments are not provided, an attempt is made to discover the defaul environment variables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Try to automatically setup environment variables
#' setup_env()
#'
#' # Set workspace bucket environment variable manually. Automatically set all others.
#' bucket <- "gs://..."
#' setup_env(bucket=bucket)
#' }
setup_env <- function(bucket = NULL, cdr = NULL, google_project = NULL)
{
  if (is.null(bucket))
  {
    bucket <- get_bucket()
  }

  if (is.null(cdr))
  {
    cdr <- get_workspace_cdr()
  }

  if (is.null(google_project))
  {
    google_project <- get_google_project()
  }

  Sys.setenv("WORKSPACE_BUCKET" = bucket)
  Sys.setenv("WORKSPACE_CDR" = cdr)
  Sys.setenv("GOOGLE_PROJECT" = google_project)

  cat("\n WORKSPACE_BUCKET =", Sys.getenv("WORKSPACE_BUCKET"))
  cat("\n WORKSPACE_CDR =", Sys.getenv("WORKSPACE_CDR"))
  cat("\n GOOGLE_PROJECT =", Sys.getenv("GOOGLE_PROJECT"))
}

get_google_project <- function()
{
  GOOGLE_CLOUD_PROJECT <- system("wb workspace describe --format=json | jq -r '.googleProjectId'", intern = TRUE)
  GOOGLE_CLOUD_PROJECT
}

get_bucket <- function()
{
  resource_list_res <- jsonlite::fromJSON(system("wb resource list --format=json --type=GCS_BUCKET", intern = TRUE))
  bucket <- resource_list_res$bucketName
  bucket <- paste0("gs://", bucket)
  bucket
}

get_workspace_cdr <- function() {

  # Get resources
  resources_json <- system("wb resource list --format=json", intern = TRUE)
  resources <- jsonlite::fromJSON(resources_json)

  # Find BigQuery datasets
  is_bq <- resources$resourceType %in% c("BQ_DATASET", "BIGQUERY_DATASET")
  bq_datasets <- resources[is_bq, ]

  if (nrow(bq_datasets) == 0) {
    message("Warning: No BigQuery datasets found")
    return(NA)
  }

  # Filter ONLY main CDR datasets (e.g., C2024Q3R8)
  is_cdr <- grepl("^C[0-9]{4}Q[0-9]+R[0-9]+$", bq_datasets$datasetId)
  cdr_datasets <- bq_datasets[is_cdr, ]

  if (nrow(cdr_datasets) == 0) {
    message("Warning: No main CDR dataset found")
    return(NA)
  }

  # If multiple CDR datasets, show options
  if (nrow(cdr_datasets) > 1) {
    message(paste("Found", nrow(cdr_datasets), "CDR datasets. Using first one:"))

    for (i in 1:nrow(cdr_datasets)) {
      ds_name <- paste0(cdr_datasets$projectId[i], ".", cdr_datasets$datasetId[i])
      message(paste("  ", i, ":", ds_name))
    }
  }

  # Use first matching CDR dataset
  WORKSPACE_CDR <- paste0(cdr_datasets$projectId[1], ".", cdr_datasets$datasetId[1])

  return(WORKSPACE_CDR)
}
