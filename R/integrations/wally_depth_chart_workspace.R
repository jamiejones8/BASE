# Keep the original Roster Studio client and model isolated from BASE's CSS and
# input IDs. Its save validation, revision checks, and roster logic run unchanged.
BASE_DEPTH_ROOT <- base_project_path("WallyApps", "DepthChartApp")

base_rosters_environment <- function(state_path = Sys.getenv("TXST_DEPTH_STATE", unset = file.path(
    if (dir.exists("/base-data")) "/base-data/app_state" else base_project_path("app_state"),
    "rosters", "depth-chart-state.rds"))) {
  dir.create(dirname(state_path), recursive=TRUE, showWarnings=FALSE)
  if (!file.exists(state_path)) {
    # Seed once, retaining the entire imported state including its revision.
    lock <- paste0(state_path,".seed-lock")
    if (!dir.create(lock,showWarnings=FALSE)) stop("Roster initialization is in progress. Please retry.")
    on.exit(unlink(lock,recursive=TRUE),add=TRUE)
    if (!file.exists(state_path)) {
      tmp <- tempfile(tmpdir=dirname(state_path))
      on.exit(unlink(tmp),add=TRUE)
      if (!file.copy(file.path(BASE_DEPTH_ROOT,"data","depth-chart-state.rds"),tmp) ||
          !file.rename(tmp,state_path)) stop("Could not initialize saved rosters.")
    }
  }
  env <- new.env(parent=asNamespace("shiny"))
  sys.source(file.path(BASE_DEPTH_ROOT,"model.R"),envir=env)
  read_original_roster <- env$read_roster
  env$read_roster <- function(path) read_original_roster(file.path(BASE_DEPTH_ROOT,"data","roster.csv"))
  env$state_path <- state_path
  # Evaluate only the two original closures: no working-directory changes or
  # second Shiny application, and no edits to the imported app's save logic.
  for (expr in parse(file.path(BASE_DEPTH_ROOT,"app.R"))) {
    if (is.call(expr) && identical(expr[[1]],as.name("<-")) &&
        as.character(expr[[2]]) %in% c("read_state","server")) eval(expr,env)
  }
  env
}

base_rosters_workspace_ui <- function() {
  shiny::addResourcePath("base-roster-studio",file.path(BASE_DEPTH_ROOT,"www"))
  htmltools::tagList(
    htmltools::tags$script(src="base-roster-studio/base-bridge.js"),
    htmltools::tags$div(class="hub-main base-page", style="max-width:1700px",
      htmltools::tags$iframe(id="base-roster-frame",src="base-roster-studio/base.html",
        title="Rosters & Recruiting",style="width:100%;height:calc(100vh - 150px);min-height:600px;border:0;display:block;",loading="eager")))
}

base_rosters_workspace_server <- function() {
  shiny::moduleServer("rosters",function(input,output,session) {
    workspace <- base_rosters_environment()
    bridge <- list(onFlushed=session$onFlushed,
      sendCustomMessage=function(type,message) session$sendCustomMessage(paste0("rosters-",type),message))
    workspace$server(input,output,bridge)
  })
}
