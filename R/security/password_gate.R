# Session-scoped password gate for the BASE application.
#
# The credential is supplied only through BASE_APP_PASSWORD. It is never sent
# to the browser: submitted values are compared on the Shiny server and the
# application UI is not rendered until the session is authenticated.

base_auth_password <- function() {
  Sys.getenv("BASE_APP_PASSWORD", unset = "")
}

base_auth_is_configured <- function(password = base_auth_password()) {
  length(password) == 1L && !is.na(password) && nzchar(trimws(password))
}

base_constant_time_equal <- function(candidate, expected) {
  if (length(candidate) != 1L || length(expected) != 1L ||
      is.na(candidate) || is.na(expected)) {
    return(FALSE)
  }

  candidate_raw <- charToRaw(enc2utf8(candidate))
  expected_raw <- charToRaw(enc2utf8(expected))
  compare_length <- max(length(candidate_raw), length(expected_raw), 1L)
  candidate_padded <- c(candidate_raw, raw(compare_length - length(candidate_raw)))
  expected_padded <- c(expected_raw, raw(compare_length - length(expected_raw)))
  differences <- bitwXor(as.integer(candidate_padded), as.integer(expected_padded))

  length(candidate_raw) == length(expected_raw) && sum(differences) == 0L
}

base_password_gate_ui <- function(configured = base_auth_is_configured()) {
  shiny::tags$main(
    id = "base-auth-screen",
    class = "base-auth-screen",
    shiny::tags$section(
      class = "base-auth-card",
      `aria-labelledby` = "base-auth-title",
      shiny::tags$div(
        class = "base-auth-brand",
        shiny::tags$img(
          src = base_supercat_logo_url(),
          alt = paste(TEAM_CONFIG$full_name, "logo")
        ),
        shiny::tags$div(
          class = "base-auth-wordmark",
          shiny::tags$span(class = "base-auth-product", "BASE"),
          shiny::tags$span(class = "base-auth-team", TEAM_CONFIG$organization)
        )
      ),
      shiny::tags$div(class = "base-auth-eyebrow", "Restricted access"),
      shiny::tags$h1(id = "base-auth-title", "Welcome back"),
      shiny::tags$p(
        class = "base-auth-intro",
        if (configured) {
          "Enter the team password to open the analytics workspace."
        } else {
          "Password access has not been configured for this deployment."
        }
      ),
      if (configured) {
        shiny::tagList(
          shiny::tagAppendAttributes(
            shiny::passwordInput(
              "base_auth_password",
              label = "Password",
              placeholder = "Enter password",
              width = "100%"
            ),
            autocomplete = "current-password"
          ),
          shiny::uiOutput("base_auth_message"),
          shiny::actionButton(
            "base_auth_submit",
            "Open BASE",
            class = "base-auth-submit"
          ),
          shiny::tags$p(
            class = "base-auth-help",
            "Access is retained for this browser session."
          )
        )
      } else {
        shiny::tags$div(
          class = "base-auth-config-error",
          role = "alert",
          shiny::tags$strong("Configuration required"),
          shiny::tags$span("Set BASE_APP_PASSWORD in the deployment environment, then restart BASE.")
        )
      }
    ),
    shiny::tags$script(shiny::HTML("
      $(document).on('keydown', '#base_auth_password', function(event) {
        if (event.key === 'Enter') {
          event.preventDefault();
          $('#base_auth_submit').trigger('click');
        }
      });
    "))
  )
}

base_password_gate_server <- function(input, output, session, application_ui) {
  authenticated <- shiny::reactiveVal(FALSE)
  auth_message <- shiny::reactiveVal(NULL)
  failed_attempts <- shiny::reactiveVal(0L)
  locked_until <- shiny::reactiveVal(as.POSIXct(NA))

  max_attempts <- max(1L, base_env_int("BASE_AUTH_MAX_ATTEMPTS", 5L))
  lockout_seconds <- max(1L, base_env_int("BASE_AUTH_LOCKOUT_SECONDS", 30L))

  output$base_app_root <- shiny::renderUI({
    if (isTRUE(authenticated())) application_ui else base_password_gate_ui()
  })

  output$base_auth_message <- shiny::renderUI({
    message <- auth_message()
    if (is.null(message)) return(NULL)
    shiny::tags$div(
      class = paste("base-auth-message", paste0("is-", message$type)),
      role = "alert",
      `aria-live` = "polite",
      message$text
    )
  })

  shiny::observeEvent(input$base_auth_submit, {
    expected <- base_auth_password()
    if (!base_auth_is_configured(expected)) {
      auth_message(list(type = "error", text = "Password access is not configured."))
      return()
    }

    now <- Sys.time()
    lock_expires <- locked_until()
    if (!is.na(lock_expires) && now < lock_expires) {
      seconds_left <- max(1L, ceiling(as.numeric(difftime(lock_expires, now, units = "secs"))))
      auth_message(list(
        type = "error",
        text = paste0("Too many attempts. Try again in ", seconds_left, " seconds.")
      ))
      return()
    }

    if (!is.na(lock_expires) && now >= lock_expires) {
      failed_attempts(0L)
      locked_until(as.POSIXct(NA))
    }

    candidate <- input$base_auth_password
    if (is.null(candidate)) candidate <- ""
    if (base_constant_time_equal(candidate, expected)) {
      auth_message(NULL)
      failed_attempts(0L)
      authenticated(TRUE)
      return()
    }

    attempt_count <- failed_attempts() + 1L
    failed_attempts(attempt_count)
    if (attempt_count >= max_attempts) {
      locked_until(now + lockout_seconds)
      auth_message(list(
        type = "error",
        text = paste0("Too many attempts. Try again in ", lockout_seconds, " seconds.")
      ))
    } else {
      remaining <- max_attempts - attempt_count
      auth_message(list(
        type = "error",
        text = paste0(
          "That password is not correct. ", remaining, " attempt",
          if (remaining == 1L) "" else "s", " remaining."
        )
      ))
    }
  }, ignoreInit = TRUE)

  authenticated
}

