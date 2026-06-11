# =========================================================
#  TXST BASEBALL ANALYTICS — Pitch Type Breakdown Page
#  Summarizes each pitcher's performance by pitch type
#  (Corrected: true Whiff% = Whiffs / Swings, simplified Zone%, improved ordering)
# =========================================================
library(shiny)
library(DT)
library(dplyr)
library(stringr)

pitch_type_breakdown_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "main-container",
      div(class = "txst-header", "Pitch Type Breakdown"),
      div(
        class = "page-subtitle",
        "Each pitcher's performance by pitch type"
      ),
      tags$hr(),
      div(
        class = "leaderboard-card-full",
        DTOutput(ns("pitch_type_table"))
      )
    )
  )
}

pitch_type_breakdown_page_server <- function(id, pitch_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    safe_num   <- function(x) suppressWarnings(as.numeric(x))
    safe_round <- function(x, d = 1) suppressWarnings(round(safe_num(x), d))
    
    summarize_pitch_types <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(tibble())
      
      # --- Normalize names a bit for safety ---
      names(df) <- trimws(names(df))
      
      # Clean pitch type labels so each "arsenal" bucket is neat
      df <- df %>%
        mutate(
          Pitcher = str_squish(as.character(Pitcher)),
          TaggedPitchType = str_squish(as.character(TaggedPitchType)),
          TaggedHitTypeNorm = str_to_upper(
            str_replace_all(str_squish(as.character(TaggedHitType)), "\\s+", "")
          ),
          TaggedPitchType = ifelse(
            is.na(TaggedPitchType) | TaggedPitchType == "",
            NA_character_,
            TaggedPitchType
          ),
          TaggedPitchType = str_to_title(TaggedPitchType),
          TaggedPitchType = str_replace_all(TaggedPitchType, "\\s+", "")  # "Four Seam" -> "Fourseam"
        )
      
      # ---------- Zone logic (simplified + consistent) ----------
      # InZone defined as the strike-zone rectangle:
      # Side: [-0.83, 0.83], Height: [1.6, 3.5] (feet)
      df <- df %>%
        mutate(
          PlateLocHeight = safe_num(PlateLocHeight),
          PlateLocSide   = safe_num(PlateLocSide)
        ) %>%
        mutate(
          InZone = case_when(
            is.na(PlateLocHeight) | is.na(PlateLocSide) ~ NA,
            PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
              PlateLocHeight >= 1.6 & PlateLocHeight <= 3.5 ~ TRUE,
            TRUE ~ FALSE
          )
        )
      
      # ---------- PitchCall sets ----------
      # True Whiff% = whiffs / swings (not whiffs / pitches)
      # Adjust if your TrackMan PitchCall levels differ.
      swing_calls <- c("InPlay", "FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging")
      whiff_calls <- c("StrikeSwinging")
      csw_calls   <- c("StrikeCalled", "StrikeSwinging")
      
      df %>%
        filter(!is.na(Pitcher), Pitcher != "", !is.na(TaggedPitchType)) %>%
        group_by(Pitcher, TaggedPitchType) %>%
        summarise(
          Pitches = n(),
          
          Swings = sum(PitchCall %in% swing_calls, na.rm = TRUE),
          Whiffs = sum(PitchCall %in% whiff_calls, na.rm = TRUE),
          InPlay = sum(PitchCall == "InPlay", na.rm = TRUE),
          GroundBalls = sum(
            PitchCall == "InPlay" & TaggedHitTypeNorm == "GROUNDBALL",
            na.rm = TRUE
          ),
          
          `Whiff%` = if_else(Swings > 0, (Whiffs / Swings) * 100, NA_real_),
          `CSW%`   = mean(PitchCall %in% csw_calls, na.rm = TRUE) * 100,
          `GB%`    = if_else(InPlay > 0, (GroundBalls / InPlay) * 100, NA_real_),
          
          # Zone% computed using ONLY pitches with location (common for tracking data).
          ZoneDen = sum(!is.na(InZone)),
          `Zone%` = if_else(
            ZoneDen > 0,
            (sum(InZone %in% TRUE, na.rm = TRUE) / ZoneDen) * 100,
            NA_real_
          ),
          
          .groups = "drop"
        ) %>%
        rename(PitchType = TaggedPitchType) %>%
        mutate(
          `Whiff%` = safe_round(`Whiff%`, 1),
          `CSW%`   = safe_round(`CSW%`, 1),
          `GB%`    = safe_round(`GB%`, 1),
          `Zone%`  = safe_round(`Zone%`, 1)
        ) %>%
        select(Pitcher, PitchType, Pitches, `Whiff%`, `CSW%`, `GB%`, `Zone%`) %>%
        arrange(Pitcher, desc(Pitches))
    }
    
    output$pitch_type_table <- renderDT({
      req(pitch_data())
      df <- summarize_pitch_types(pitch_data())
      
      percent_cols <- c("Whiff%", "CSW%", "GB%", "Zone%")
      percent_idx  <- which(names(df) %in% percent_cols) - 1  # DT is 0-based
      
      datatable(
        df,
        rownames   = FALSE,
        class      = "stripe hover compact",
        extensions = "Buttons",
        options = list(
          scrollX    = TRUE,
          pageLength = 100,
          dom        = "Bfrtip",
          buttons    = list(
            list(extend = "copy",  text = "📋 Copy",  className = "txst-dt-btn"),
            list(extend = "csv",   text = "💾 CSV",   className = "txst-dt-btn"),
            list(extend = "excel", text = "📊 Excel", className = "txst-dt-btn")
          ),
          # Default sort: Pitcher asc, then Pitches desc (usage within pitcher)
          order = list(list(0, "asc"), list(2, "desc")),
          columnDefs = list(
            list(
              targets = percent_idx,
              render = JS("
                function(data, type, row, meta) {
                  if (type !== 'display') return data;
                  if (data == null || data === '') return '';
                  var val = parseFloat(data);
                  if (isNaN(val)) return data;
                  return val.toFixed(1) + '%';
                }
              ")
            )
          )
        )
      )
    })
  })
}
