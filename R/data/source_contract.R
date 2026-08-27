# Shared logical source registry for BASE and migrated Wally features.

BASE_DATA_SOURCE_CONTRACT_FILE <- TEAM_CONFIG$data$source_contract_file

base_load_data_source_contract <- function(path = BASE_DATA_SOURCE_CONTRACT_FILE) {
  if (!file.exists(path)) stop("BASE data-source contract was not found at ", path)
  contract <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  required <- c("schema_version", "primary_season", "sources", "feature_routes")
  missing <- setdiff(required, names(contract))
  if (length(missing)) {
    stop("BASE data-source contract is missing: ", paste(missing, collapse = ", "))
  }
  contract
}

BASE_DATA_SOURCE_CONTRACT <- base_load_data_source_contract()

base_data_source <- function(source_id) {
  source <- BASE_DATA_SOURCE_CONTRACT$sources[[source_id]]
  if (is.null(source)) stop("Unknown BASE data source: ", source_id)
  source
}

base_feature_sources <- function(feature_id) {
  route <- BASE_DATA_SOURCE_CONTRACT$feature_routes[[feature_id]]
  if (is.null(route)) stop("Unknown BASE feature route: ", feature_id)
  unlist(route, use.names = FALSE)
}

base_data_source_label <- function(source_id, default = source_id) {
  source <- base_data_source(source_id)
  label <- source$label
  if (is.null(label) || !nzchar(label)) label <- source$authority
  if (is.null(label) || !nzchar(label)) default else label
}

base_data_source_development_path <- function(source_id) {
  source <- base_data_source(source_id)
  path <- source$development_path
  if (is.null(path) || !nzchar(path)) return(NA_character_)
  base_project_path(path)
}

base_validate_feature_routes <- function(contract = BASE_DATA_SOURCE_CONTRACT) {
  known <- names(contract$sources)
  referenced <- unique(unlist(contract$feature_routes, use.names = FALSE))
  unknown <- setdiff(referenced, known)
  if (length(unknown)) {
    stop("Feature routes reference unknown sources: ", paste(unknown, collapse = ", "))
  }
  invisible(TRUE)
}

base_validate_feature_routes()
