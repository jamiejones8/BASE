# Pitching workspace integration

The first complete application slice mounts Wally's PitchingApp inside BASE as
the `team_pitching` workspace. It is not an iframe and does not launch a second
Shiny process. Its UI and server execute in an isolated R environment inside
the existing BASE session.

## Preserved behavior

All twelve top-level Wally tabs remain present and in their existing order:

1. Performance;
2. Pitch Metrics;
3. Season Summary;
4. Pitch Decay;
5. Locations;
6. Whiffs / Chases / Called Strikes / Barrels;
7. Stuff+;
8. AAR;
9. Bullpens;
10. Leaderboard;
11. Team Report;
12. Team Trends.

Nested tabs, output IDs, calculations, PDF builders, filters, and download
handlers remain owned by Wally's server. The standalone application still
loads its original files when run by itself, so the locked comparison baseline
continues to test the original workflow.

## Folder data route

When hosted by BASE, `R/integrations/wally_pitching_workspace.R` injects one
prepared payload built exclusively from these files in
`WallyApps/PitchingApp/data` before Wally's feature engineering runs:

- `2025 Season -cleaned.csv` maps to the 2025 Season checkbox;
- `2025 Fall -cleaned.csv` maps to the 2025 Fall checkbox;
- `2026 Squads - cleaned.csv` maps to the 2026 Squads checkbox;
- `2026 Season - cleaned.csv` maps to the 2026 Season checkbox; and
- `Bullpens - cleaned.csv` supplies bullpen mode and the Bullpens tab.

The integrated Pitching workspace no longer blends these rows with the shared
BASE startup partition or NCAA master. Duplicate pitch IDs across the folder
files are still removed, and each row retains its source filename.

## Navigation and appearance

The compatibility `navbarPage` still owns routing, but its global navbar is
hidden. Eight ordered cards on Home open the approved workspaces, and a fixed
Home control remains in the upper-left corner. Existing tool pages remain as
hidden compatibility routes while their parent workspaces are migrated.

The Pitching app receives a scoped BASE theme: BASE typography, surfaces,
spacing, controls, tabs, tables, borders, and shadows. Wally's global body and
watermark CSS is not injected into BASE, so it cannot restyle unrelated pages.

## Loading behavior

The entire Pitching source, data preparation, UI, and server graph initialize
only on the first visit to Pitching. The isolated environment is then shared as
read-only application state, while every session receives its own Wally server
reactives. The temporary prepared-data cache is cleared after initialization.

## Verification

`scripts/tests/test_wally_pitching_integration.R` verifies:

- resolution of the five required PitchingApp folder files;
- duplicate removal across folder files;
- retention of a unique bullpen event;
- exposure of all twelve Wally tabs;
- successful registration of the original server graph; and
- a live Pitch Decay payload plus table and Plotly output.

The existing Wally golden suite remains the calculation and rendering parity
gate for HittingApp, PitchingApp, and DefenseApp.
