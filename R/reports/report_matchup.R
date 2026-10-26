# Resolve opponents from team codes, never display labels or file names.
base_report_matchup <- function(d, team_column = "PitcherTeam") {
  first <- function(nm) {
    if (!nm %in% names(d)) return("")
    x <- trimws(as.character(d[[nm]])); x <- x[!is.na(x) & nzchar(x)]
    if(length(x)) x[[1]] else ""
  }
  team <- first(team_column)
  if (!nzchar(team)) team <- TEAM_CONFIG$data_code
  home <- first("HomeTeam"); away <- first("AwayTeam")
  is_home <- identical(toupper(team),toupper(home))
  opponent <- if(is_home) away else if(identical(toupper(team),toupper(away))) home else ""
  if (!nzchar(opponent)) {
    other <- if(team_column == "PitcherTeam") "BatterTeam" else "PitcherTeam"
    opponent <- first(other)
  }
  list(team=team,home=home,away=away,opponent=opponent,
       separator=if(is_home) "vs." else "@",
       external=nzchar(opponent) && toupper(opponent)!=toupper(team))
}
.base_report_logo_cache <- new.env(parent=emptyenv())
base_report_opponent_image <- function(matchup) {
  if (!isTRUE(matchup$external) || !exists("team_palette",mode="function")) return(NULL)
  code <- matchup$opponent
  if(exists(code,.base_report_logo_cache,inherits=FALSE)) return(get(code,.base_report_logo_cache))
  palette <- team_palette(code)
  url <- palette$logo_url
  if(!length(url)||is.na(url)||!nzchar(url)) return(NULL)
  image <- tryCatch({
    if(grepl("\\.svg($|\\?)",url,ignore.case=TRUE)) {
      # NCAA's bgd marks are white on transparency. Give them the team's color
      # before rasterizing so they remain visible on the white pitching report.
      svg <- paste(readLines(url,warn=FALSE),collapse="\n")
      color <- palette$primary
      if(is.null(color)||!grepl("^#[0-9A-Fa-f]{6}$",color)) color <- "#501214"
      svg <- sub("(<svg[^>]*>)",paste0("\\1<rect width='100%' height='100%' fill='",color,"'/>"),svg)
      tmp <- tempfile(fileext=".png");on.exit(unlink(tmp),add=TRUE)
      if(requireNamespace("rsvg",quietly=TRUE)) rsvg::rsvg_png(charToRaw(svg),tmp,width=512)
      else if(requireNamespace("magick",quietly=TRUE)) {
        im<-magick::image_read(charToRaw(svg));magick::image_write(magick::image_resize(im,"512x512"),tmp,format="png")
      } else return(NULL)
      png::readPNG(tmp)
    } else if(exists("fetch_team_logo",mode="function")) fetch_team_logo(code,url) else NULL
  },error=function(e) NULL)
  # Failed network requests remain retryable.
  if(!is.null(image)) assign(code,image,.base_report_logo_cache)
  image
}
