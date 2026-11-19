# Player-specific aliases shared by imports, editors, and embedded workspaces.
base_correct_player_names <- function(x) {
  x <- as.character(x)
  x <- gsub("\\bJustin\\s+Antcil\\b", "Justin Anctil", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\bAntcil,\\s*Justin\\b", "Anctil, Justin", x, ignore.case = TRUE, perl = TRUE)
  # Match this full player name only: Cadyn Shaw and other Byrds stay unchanged.
  x[grepl("^\\s*Cadyn\\s+Byrd\\s*$", x, ignore.case = TRUE, perl = TRUE)] <- "Cayden Byrd"
  x[grepl("^\\s*Byrd,\\s*Cadyn\\s*$", x, ignore.case = TRUE, perl = TRUE)] <- "Byrd, Cayden"
  x
}

base_correct_player_name_columns <- function(rows) {
  keys <- tolower(gsub("[^a-zA-Z]", "", names(rows)))
  columns <- names(rows)[keys %in% c("pitcher", "batter", "hitter", "catcher", "player", "name",
    "playername", "athletename", "fieldedby", "manualfieldedby", "tmname", "brefname") |
    grepl("^(P|C|1B|2B|3B|SS|LF|CF|RF)_Name$", names(rows), ignore.case = TRUE)]
  for (column in columns) rows[[column]] <- base_correct_player_names(rows[[column]])
  for (pair in list(c("first", "last"), c("firstname", "lastname"))) {
    index <- match(pair, keys)
    if (anyNA(index)) next
    original <- paste(rows[[index[1]]], rows[[index[2]]])
    corrected <- base_correct_player_names(original)
    changed <- !is.na(corrected) & corrected != original
    rows[[index[1]]][changed] <- sub(" .*$", "", corrected[changed])
    rows[[index[2]]][changed] <- sub("^[^ ]+ ", "", corrected[changed])
  }
  rows
}
