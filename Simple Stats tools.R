# Simple Stats tools.R
# Convenience shim that re-exports the richer interactive helpers
# defined in `Interactive Stat Tools Extended.R`. This keeps the
# behavior in sync without maintaining duplicate logic.

load_extended_tools <- function(script = "Interactive Stat Tools Extended.R") {
  if (!file.exists(script)) {
    stop(sprintf("Cannot locate '%s' in the current working directory.", script))
  }
  source(script, local = FALSE)
  message(
    "Interactive Stat Tools Extended helpers loaded.\n",
    "Use summary_stats(), frequency_table(), etc. directly."
  )
}

load_extended_tools()
