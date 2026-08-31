# Hitting workspace integration

Wally's complete HittingApp now runs inside BASE as the `team_hitting`
workspace. It is evaluated in the same Shiny process and session as BASE, not
inside an iframe or a second application server.

## Preserved behavior

All ten top-level Wally workflows remain present in their existing order:

1. Performance;
2. Lineup Builder;
3. Damage Heat Map;
4. Whiff Zones;
5. Team Report;
6. Game Reports (AAR);
7. Leaderboard;
8. Swing Decisions;
9. Ball Flight;
10. Contact Point.

Nested tabs, filters, calculations, tables, plots, PDF builders, and download
handlers remain owned by Wally's server. The standalone app still
uses its original loader and page shell when run outside BASE, preserving the
locked comparison baseline.

Internal IDs that overlapped Pitching are workspace-prefixed; visible labels,
tab order, calculations, and downloads are unchanged. This prevents
cross-workspace reactive coupling in BASE's shared Shiny session.

## Folder data route

The BASE host now builds Hitting's payload exclusively from these files in
`WallyApps/HittingApp/data` before Wally's feature engineering runs:

- `2025 Season -cleaned.csv` maps to the 2025 Season checkbox;
- `2025 Fall -cleaned.csv` maps to the 2025 Fall checkbox;
- `2026 Squads - cleaned.csv` maps to the 2026 Squads checkbox; and
- `2026 Season - cleaned.csv` maps to the 2026 Season checkbox.

The integrated Hitting workspace no longer blends these rows with the shared
BASE startup partition or NCAA master. Duplicate event IDs across the folder
files are still removed, and each row retains its source filename.

This route is intentionally separate from national opponent scouting. Hitting
evaluates Texas State hitters; Opponent Scouting continues to query all NCAA
Division I players on demand.

## Navigation and appearance

The Hitting card on Home now opens the complete workspace. Wally's internal
page and tab hierarchy is preserved, while its standalone navbar, body
watermarks, global selectors, and full-page CSS are replaced by a scoped BASE
wrapper. Typography, surfaces, tabs, tables, controls, colors, spacing, and
responsive behavior inherit BASE's design system without affecting other
workspaces.

## Loading behavior

Hitting initializes only on its first visit. Its source and read-only prepared
application state are cached once, while each user session receives its own
server reactives. The temporary prepared-data cache is cleared after startup.

## Verification

`scripts/tests/test_wally_hitting_integration.R` verifies:

- resolution of the four required HittingApp folder files;
- duplicate removal across folder files;
- all ten Wally top-level tabs and the isolated BASE layout;
- successful registration of Wally's original server graph; and
- live Performance and Lineup Builder fixture payloads.

The Wally golden parity suite remains the calculation and rendering gate for
the standalone HittingApp, PitchingApp, and DefenseApp.
