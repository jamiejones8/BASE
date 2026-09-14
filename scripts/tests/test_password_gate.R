#!/usr/bin/env Rscript

source("R/security/password_gate.R", local = FALSE)

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

cat("Password gate tests passed.\n")
