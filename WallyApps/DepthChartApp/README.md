# Texas State Roster Studio

Run from the parent TxStApps folder:

```r
shiny::runApp("DepthChartApp")
```

Requires R and Shiny. Uses the included copy of `2026 Fall Roster Template.csv` (38 players). No other apps are modified.

## Planning rules

- Fall 2026 classes are the starting classes for spring 2027. Classes advance once each season through a fifth year, then disappear. This implements the requested five-year planning assumption; it does not calculate individual eligibility exceptions.
- Five season tabs start at the current baseball season. August 1 rolls forward to the next spring season. Set `TXST_SEASON_ROLLOVER_MONTH` to change the month.
- Initial assignments follow CSV order and the supplied positions. Commas, slashes, and semicolons separate positions; surrounding whitespace and trailing separators are ignored. INF expands to all four infield positions; OF expands to all three outfield positions where the template still uses those groups. These are provisional depth orders, not coaching recommendations.
- Four starter slots, three Long Relief slots, Left Handed and Right Handed relief lists, and three Closer slots. Existing saved bullpen players migrate by throwing hand, retaining their relative order; long relief starts empty. Pitching groups do not duplicate players. Two-way players can also occupy field positions.
- Use the up/down arrows beside a player to reorder a depth stack. Arrows swap entries without creating slots. Select a plate to change groups, edit the profile, or remove the player. Moves and Save changes carry the selected season’s roles and depth order into later seasons. Earlier seasons are unchanged. Future-only arrivals are retained; expired players drop off. All depth slots stay fixed, leaving vacancies when eligibility ends. Explicitly saved later-season plans take precedence over projections from an earlier season. Saving an earlier season updates future projections until the next explicit season plan.
- The lineup selects the first unused player for C, 1B, 2B, 3B, SS, LF, CF, RF, DH, in that order. DH displays only players who are not starting at another field position or in the starting rotation. Hidden DH candidates retain their saved order and return when they leave the starting lineup. Remaining position players appear once on the bench. Use lineup up/down buttons to set batting order. Each General, v LHP, and v RHP plan has independent depth and batting orders. New matchup plans start from General.
- Recruiting classes initially have no profiles. Enter the spring entry season and class on arrival. Recruiting profiles stay off depth charts; Offered has yellow diagonal stripes, Committed has green dashed diagonals, Signed has no overlay. Base class colors continue to advance. Edit profiles to change status and Bats/Throws.

Names use red for left-handed pitchers (Throws) and left-handed hitters (Bats), and green for switch hitters. These colors appear on the field, lineup, bench, and recruiting page while class background colors and recruiting status patterns remain visible. Two-way players use Throws in pitching groups and Bats in hitting roles. Unknown handedness stays in the normal text color.

## Matchup plans and roster history

The three navigation buttons sit below the color legend and above the depth-chart heading. The lineup panel aligns with the field and has a scrollable bench. General, v LHP, and v RHP buttons above the lineup switch both the field plan and batting order. Moves update projections for that plan only. Explicit later-season saves remain independent. The first load of this update carries the current saved General chart forward and splits the legacy bullpen by throwing hand. Save changes to persist the migration; the server backs up the previous saved state before the first version-2 save.

Use **Manage roster**, **Remove player…** after selecting a depth-chart plate, or **Remove…** in recruiting classes. A dialog asks which season removal starts in and defaults to the selected season plus one (or the recruit's entry year if later). Confirmation removes the player from that season and every subsequent matchup/depth chart and stops counting their allocation. Previous charts and allocations remain intact. Profiles are retained as historical records rather than erased. Save changes to persist. When the current season advances, a Past seasons selector keeps seasons from 2027 onward available.

The large pie below the field shows LHP, RHP, Catchers, Outfielders, Middle infielders, and Corner infielders plus Available budget. Each player counts once: an assigned pitcher uses throwing hand, a field starter uses their assigned position, and a bench hitter uses their first listed position. A DH-only profile without a field position is shown as Unassigned. With spending above budget, the pie shows total allocations and the overage is stated explicitly. Offered and Committed packages are included in planning totals.

## Two-way players and pitcher release chart

Tanner Carson and Ryley Leininger retain their field positions and also appear in the right-handed pitching staff. The model maintains field and pitching membership independently, so two-way players can be assigned in both. Their allocation still counts once. The four relief groups run side by side across the page below both field and lineup/bench; narrow screens can scroll the row horizontally.

Use **Edit profile** in Manage roster, after selecting a field/pitching plate, or in recruiting classes. The arm chart also provides a pitcher selector and clickable plotted points. Profiles include optional **Height (inches)**, **Release height (ft)**, and **Release side (ft from center)**. All three and a known throwing hand are required for plotting. Clearing a measurement excludes the point without removing the player from the roster.

The arm chart appears below the four pitching groups. Its x-coordinate is the magnitude of release side, negative for left-handers and positive for right-handers. Its shoulder anchor is `(0, (heightIn - 10) / 12)`. The class-colored line ends at `(signed releaseSideFt, releaseHeightFt)`. Tooltip arm angle is `atan2(releaseHeightFt - anchorHeightFt, abs(releaseSideFt))` in degrees from horizontal. Both axes use equal distance scaling so the drawn line agrees with the inferred angle. Offered lines use long dashes; Committed lines use short dashes; Signed lines are solid. Missing measurements, expired players, and players removed for the selected season are excluded.

Initial measurements are a bundled snapshot from **PitchingApp/data/2026 Fall - cleaned.csv** (means across pitches with valid release height and side), joined by first/last name to the roster. Heights come from the existing **PitchingApp/data/player_heights.csv**. Exact-name matches only: unmatched records remain blank. The snapshot has 16 matching release records and can be rebuilt with `python3 scripts/import-pitcher-measurements.py` from the parent workspace. Current-player edits override the defaults and are saved alongside the roster. Imported defaults have source/sample-count tooltips.

The budget pie now labels each nonzero slice directly; the legend retains exact dollar values. Small slices use leader lines and spaced callouts.

## Scholarships and NIL

Open **Scholarships and NIL** beside **View Recruiting Classes**. Use the season tabs to set a total annual budget and each active player's annual allocation. Existing players start at $0; the total budget is unset until entered. Amounts and budgets inherit forward from their most recent entry. Editing a later season changes that season and later seasons; an explicit override in a still-later season takes precedence. Zero is a valid allocation or budget.

Recruiting profiles include **Offer Package ($ per year)**. This is their default annual allocation from their entry season through remaining eligibility. A player-specific amount entered on Scholarships and NIL overrides that default from the chosen season onward. Offered, Committed, and Signed players count against projected usage; Recruiting profiles do not. These totals reserve offered amounts for planning rather than tracking actual payments.

The REV Share Budget panel shows budget, used, remaining, and percentage used above every field. The five-season outlook also shows incoming-class allocations. Each player counts once regardless of duplicate depth-chart positions. Expired players stop counting automatically. Over-budget amounts remain visible in red; the progress bar caps visually at 100% while its label reports actual usage. Zero or unset budgets never cause division errors.

Depth-chart, lineup, and bench plates display abbreviated amounts such as $65k and $5k. Hover over an amount for the exact dollars. Use **Save changes** to persist budgets, allocations, and offer packages with the rest of the roster.

## Saving

Save changes persists all season charts and recruiting profiles in `data/depth-chart-state.rds`. Reload opens the current season. Unsaved edits prompt before leaving. Saves use a temporary file and rename, with a lock and revision checks to prevent silent overwrites between sessions. If another session has saved, reload before editing again.

For hosted deployments, configure `TXST_DEPTH_STATE` to a persistent writable storage path. The default local file should be backed up; ephemeral hosting filesystems do not guarantee persistence across redeployments. Access control is supplied by your Shiny hosting environment.

## Verification

From this app directory:

```sh
Rscript tests/test-model.R
Rscript tests/test-save.R
node tests/test-depth-model.js
node tests/test-funding.js
node tests/test-roster-plans.js
node tests/test-arm.js
Rscript tests/test-pitcher-profiles.R
Rscript tests/test-plans-save.R
Rscript tests/test-funding-save.R
```

Roster positions were refreshed from the updated template in the parent TxSt_Shiny_App folder. The app reads data/roster.csv for each new session. Reload after a roster update. Saved assignments were reconciled while retaining valid order; the previous saved state is backed up beside the state file.

## BASE integration

BASE presents this copy as **Rosters & Recruiting**, between Defensive Analytics
and HomeBASE. The imported model, saved roster, measurements, and saved-state
backups are retained. `base-theme.css` applies BASE's fonts, colors, and buttons;
the iframe keeps the original application's styles and IDs isolated.

`R/integrations/wally_depth_chart_workspace.R` runs the original server closures
and forwards the client through a namespaced BASE session. On first use it copies
`data/depth-chart-state.rds` into `/base-data/app_state/rosters/` on the deployment
volume, or `app_state/rosters/` locally. Existing state is never reseeded. Set
`TXST_DEPTH_STATE` to override this path. This copy is independent of the original
Desktop app after migration. The original standalone entry point remains usable.
