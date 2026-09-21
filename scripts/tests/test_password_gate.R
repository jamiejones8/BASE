#!/usr/bin/env Rscript

source("R/security/password_gate.R", local = FALSE)

TEAM_CONFIG <- list(full_name = "Test Team", organization = "Test Organization")
base_supercat_logo_url <- function() ""
base_env_int <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, unset = as.character(default))))
  if (is.na(value)) default else value
}

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_true(!base_auth_is_configured(""), "An empty password must not enable access.")
assert_true(!base_auth_is_configured("   "), "A whitespace-only password must not enable access.")
assert_true(base_auth_is_configured("team-secret"), "A non-empty password should enable access.")

assert_true(
  base_constant_time_equal("team-secret", "team-secret"),
  "Matching passwords were rejected."
)
assert_true(
  !base_constant_time_equal("team-secret", "wrong-secret"),
  "Different passwords were accepted."
)
assert_true(
  !base_constant_time_equal("team-secret", "team-secret-longer"),
  "Passwords with different lengths were accepted."
)
assert_true(
  base_constant_time_equal("Bóbcats-⚾", "Bóbcats-⚾"),
  "UTF-8 passwords should compare correctly."
)
assert_true(
  !base_constant_time_equal(character(), "team-secret"),
  "Missing submitted passwords must be rejected."
)

fixed_now <- as.POSIXct("2026-09-16 12:00:00", tz = "UTC")
remember_token <- base_auth_issue_remember_token("team-secret", now = fixed_now, hours = 48L)
assert_true(nzchar(remember_token), "A remember-device token was not issued.")
assert_true(
  base_auth_validate_remember_token(remember_token, "team-secret", now = fixed_now + 3600),
  "A valid remember-device token was rejected."
)
assert_true(
  !base_auth_validate_remember_token(remember_token, "team-secret", now = fixed_now + 49 * 3600),
  "An expired remember-device token was accepted."
)
assert_true(
  !base_auth_validate_remember_token(paste0(remember_token, "0"), "team-secret", now = fixed_now),
  "A modified remember-device token was accepted."
)
assert_true(
  !base_auth_validate_remember_token(remember_token, "new-secret", now = fixed_now),
  "A remember-device token survived a password change."
)

gate_html <- paste(as.character(base_password_gate_ui(TRUE)), collapse = "")
head_html <- paste(as.character(base_password_gate_head()), collapse = "")
assert_true(
  grepl("base_auth_enter", head_html, fixed = TRUE) && grepl("password: this.value", head_html, fixed = TRUE),
  "The Enter-key login path does not submit the field value directly."
)
assert_true(
  grepl("localStorage.setItem", head_html, fixed = TRUE) &&
    grepl("localStorage.getItem", head_html, fixed = TRUE) &&
    grepl("base-auth-store", head_html, fixed = TRUE) &&
    grepl("base_auth_remember_token", head_html, fixed = TRUE),
  "The static password-gate script does not persist and restore remember tokens."
)
assert_true(
  !grepl("localStorage", gate_html, fixed = TRUE),
  "Remember-token wiring is still embedded in dynamically rendered login UI."
)
assert_true(grepl("up to 2 days", gate_html, fixed = TRUE), "The default remember-device duration is not shown.")

app_source <- paste(readLines("R/app_main.R", warn = FALSE), collapse = "\n")
assert_true(
  grepl("base_password_gate_head()", app_source, fixed = TRUE),
  "The static application shell does not install the password persistence script."
)

old_password <- Sys.getenv("BASE_APP_PASSWORD", unset = NA_character_)
Sys.setenv(BASE_APP_PASSWORD = "team-secret")
on.exit({
  if (is.na(old_password)) Sys.unsetenv("BASE_APP_PASSWORD") else Sys.setenv(BASE_APP_PASSWORD = old_password)
}, add = TRUE)

shiny::testServer(function(input, output, session) {
  auth_state <- base_password_gate_server(input, output, session, shiny::tags$div("App"))
}, {
  session$flushReact()
  session$setInputs(base_auth_enter = list(password = "team-secret", nonce = 1))
  session$flushReact()
  assert_true(auth_state(), "Submitting the correct password with Enter did not authenticate.")
})

shiny::testServer(function(input, output, session) {
  auth_state <- base_password_gate_server(input, output, session, shiny::tags$div("App"))
}, {
  session$flushReact()
  session$setInputs(base_auth_password = "team-secret", base_auth_submit = 1)
  session$flushReact()
  assert_true(auth_state(), "Clicking the login button with the correct password did not authenticate.")
})

cat("Password gate tests passed.\n")
