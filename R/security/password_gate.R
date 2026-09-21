# Password gate for the BASE application.
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

base_auth_remember_hours <- function(value = Sys.getenv("BASE_AUTH_REMEMBER_HOURS", unset = "48")) {
  hours <- suppressWarnings(as.integer(value))
  if (length(hours) != 1L || is.na(hours)) hours <- 48L
  max(0L, min(hours, 24L * 7L))
}

base_auth_cookie_secret <- function(password = base_auth_password()) {
  configured <- Sys.getenv("BASE_AUTH_COOKIE_SECRET", unset = "")
  if (nzchar(configured)) configured else password
}

base_auth_remember_signature <- function(expiry, password = base_auth_password()) {
  if (!requireNamespace("digest", quietly = TRUE) || !base_auth_is_configured(password)) return(NA_character_)
  key <- paste0(base_auth_cookie_secret(password), "\n", password)
  digest::hmac(
    key = key,
    object = paste0("BASE-AUTH-V1|", expiry),
    algo = "sha256",
    serialize = FALSE
  )
}

base_auth_issue_remember_token <- function(password = base_auth_password(), now = Sys.time(),
                                           hours = base_auth_remember_hours()) {
  hours <- suppressWarnings(as.integer(hours))
  if (!base_auth_is_configured(password) || length(hours) != 1L || is.na(hours) || hours <= 0L) return("")
  expiry <- format(floor(as.numeric(now) + hours * 3600), scientific = FALSE, trim = TRUE)
  signature <- base_auth_remember_signature(expiry, password)
  if (is.na(signature) || !nzchar(signature)) return("")
  paste(expiry, signature, sep = ".")
}

base_auth_validate_remember_token <- function(token, password = base_auth_password(), now = Sys.time()) {
  if (length(token) != 1L || is.na(token) || !nzchar(token) || !base_auth_is_configured(password)) return(FALSE)
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 2L || !grepl("^[0-9]+$", parts[[1]]) || !grepl("^[0-9a-f]{64}$", parts[[2]])) return(FALSE)
  expiry <- suppressWarnings(as.numeric(parts[[1]]))
  if (!is.finite(expiry) || as.numeric(now) > expiry) return(FALSE)
  expected <- base_auth_remember_signature(parts[[1]], password)
  !is.na(expected) && base_constant_time_equal(parts[[2]], expected)
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

base_password_gate_head <- function() {
  shiny::tags$script(shiny::HTML("
    (function() {
      var storageKey = 'base_auth_remember';

      function cookieValue(name) {
        var prefix = name + '=';
        var values = document.cookie ? document.cookie.split(';') : [];
        for (var i = 0; i < values.length; i++) {
          var value = values[i].trim();
          if (value.indexOf(prefix) === 0) return decodeURIComponent(value.substring(prefix.length));
        }
        return '';
      }

      function persistentToken() {
        try {
          var stored = window.localStorage.getItem(storageKey);
          if (stored) return stored;
        } catch (error) {}
        return cookieValue(storageKey);
      }

      function storeToken(message) {
        var token = message && message.token ? String(message.token) : '';
        var maxAge = message && Number.isFinite(Number(message.maxAge)) ? Number(message.maxAge) : 0;
        if (!token || maxAge <= 0) return;
        try { window.localStorage.setItem(storageKey, token); } catch (error) {}
        var secure = window.location.protocol === 'https:' ? '; Secure' : '';
        document.cookie = storageKey + '=' + encodeURIComponent(token) +
          '; Path=/; Max-Age=' + Math.floor(maxAge) + '; SameSite=Lax' + secure;
      }

      function submitStoredToken() {
        if (!window.Shiny || typeof window.Shiny.setInputValue !== 'function') return;
        window.Shiny.setInputValue('base_auth_remember_token', persistentToken(), {priority: 'event'});
      }

      function installHandlers() {
        if (!window.Shiny || !window.jQuery) {
          window.setTimeout(installHandlers, 50);
          return;
        }
        if (!window.baseAuthStorageHandlerInstalled) {
          window.Shiny.addCustomMessageHandler('base-auth-store', storeToken);
          window.baseAuthStorageHandlerInstalled = true;
        }
        window.jQuery(document)
          .off('keydown.baseAuth', '#base_auth_password')
          .on('keydown.baseAuth', '#base_auth_password', function(event) {
            if (event.key === 'Enter') {
              event.preventDefault();
              window.Shiny.setInputValue('base_auth_enter', {
                password: this.value.replace(/[\\r\\n]+$/, ''),
                nonce: Date.now()
              }, {priority: 'event'});
            }
          });
        submitStoredToken();
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', installHandlers, {once: true});
      } else {
        installHandlers();
      }
      if (window.jQuery) {
        window.jQuery(document)
          .off('shiny:connected.baseAuth')
          .on('shiny:connected.baseAuth', installHandlers);
      }
    })();
  "))
}

base_password_gate_ui <- function(configured = base_auth_is_configured()) {
  remember_hours <- base_auth_remember_hours()
  remember_label <- if (remember_hours == 24L) {
    "Access is retained on this device for up to 1 day."
  } else if (remember_hours > 0L && remember_hours %% 24L == 0L) {
    paste0("Access is retained on this device for up to ", remember_hours %/% 24L, " days.")
  } else if (remember_hours > 0L) {
    paste0("Access is retained on this device for up to ", remember_hours, " hours.")
  } else {
    "Access is retained for this browser session."
  }
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
            remember_label
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
    )
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

  shiny::observeEvent(input$base_auth_remember_token, {
    if (isTRUE(authenticated())) return()
    expected <- base_auth_password()
    if (base_auth_validate_remember_token(input$base_auth_remember_token, expected)) {
      auth_message(NULL)
      failed_attempts(0L)
      authenticated(TRUE)
    }
  }, ignoreInit = FALSE)

  authenticate_candidate <- function(candidate) {
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

    if (is.null(candidate)) candidate <- ""
    candidate <- sub("[\r\n]+$", "", candidate)
    if (base_constant_time_equal(candidate, expected)) {
      auth_message(NULL)
      failed_attempts(0L)
      remember_hours <- base_auth_remember_hours()
      remember_token <- base_auth_issue_remember_token(expected, hours = remember_hours)
      if (nzchar(remember_token)) {
        session$sendCustomMessage(
          "base-auth-store",
          list(token = remember_token, maxAge = as.integer(remember_hours * 3600L))
        )
      }
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
  }

  shiny::observeEvent(input$base_auth_submit, {
    authenticate_candidate(input$base_auth_password)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$base_auth_enter, {
    submitted <- input$base_auth_enter
    candidate <- if (is.list(submitted) && !is.null(submitted$password)) submitted$password else ""
    authenticate_candidate(candidate)
  }, ignoreInit = TRUE)

  authenticated
}

# Run the protected application server exactly once after authentication. A
# plain observe is intentional: a remembered token may authenticate during the
# first reactive flush, before an observeEvent(ignoreInit = TRUE) sees a change.
base_auth_initialize_server <- function(authenticated, initialize) {
  stopifnot(is.function(initialize))
  initialized <- FALSE

  shiny::observe({
    shiny::req(isTRUE(authenticated()))
    if (isTRUE(initialized)) return()
    initialized <<- TRUE
    shiny::isolate(initialize())
  })
}
