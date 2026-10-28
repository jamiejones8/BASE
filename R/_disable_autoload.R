# Shiny recognizes this filename and skips automatic sourcing of R/*.R.
# app.R explicitly sources R/app_main.R, which loads its dependencies in order.
# Without this marker, the entire application initializes twice per process.
