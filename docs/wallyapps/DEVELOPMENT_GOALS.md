# Development Goals

Hitting and Pitching each offer a Development Goals tab with an independent
single-player and season selector. Season values use the apps' existing numeric
summaries and D1 reference distributions, with the exact eleven metrics requested
for each side. Pitcher xwOBA uses the shared expected-contact grid and the same
BB/HBP/K treatment as Hitting; its empirical distribution qualifies pitchers at
50 completed PAs. Lower allowed wOBA, xwOBA, OPS, BB%, and Barrel% rank better.

Each objective contains a title, a selected season stat/current value, a written
goal, one to three supporting actions, and zero to five optional drills. Empty
action fields are omitted from the preview and PDF; filled actions share the
available width within their objective's column. Export
is available when every required field is complete. Adding/removing Objective 2
preserves Objective 1. **Save and Generate PDF** validates the plan, generates
the PDF and commits the goals, actions and drills to persistent storage. Returning
to a player/baseline season restores that saved plan. **Reload Saved Goals** pulls
the most recently saved version; concurrent edits are rejected rather than
silently overwriting another session.

Both apps share the same Build Goals and Track Goals implementation. Saves are
isolated by team, hitting/pitching mode, player and baseline season in SQLite.
`BASE_DEVELOPMENT_GOALS_FILE` can override the path; the default is
`development-goals.sqlite` beside the configured team-season-import directory,
normally `/base-data/app_state` on Railway or local `app_state/`. It must reside
on the writable persistent volume to survive redeploys. Local state is excluded
from Git, Docker and Railway uploads.

`start-railway.sh` runs `scripts/checks/check_development_storage.R` after runtime
storage is attached and before Shiny starts. On Railway, it requires the platform's
`RAILWAY_VOLUME_MOUNT_PATH` to be an actual mounted, writable volume and the existing
database path to be inside it. A missing mount or temporary/repository storage path
stops startup instead of allowing goals to be saved to an ephemeral container.
The database location is not changed by these checks; keep
`BASE_DEVELOPMENT_GOALS_FILE` stable across updates.

When the database exists, startup takes a verified SQLite online backup in the
adjacent `development-goals-backups/` directory, with a unique timestamped filename.
It includes committed WAL journal pages, preserves every table and saved payload,
and checks database integrity and goal-record counts before starting the app.
Snapshots are append-only; old backups and the source database are not replaced.
Backup failures stop startup. A fresh installation with no database does not seed
an empty file over an existing one. The persistent volume and its backups must be
retained across redeployments; a source push contains neither of them.

Tracking begins with S26 as the baseline and F26 as the tracking season, followed
by F26 → PS27 → S27 → F27. Plans carry forward until replaced; a newer season's
plan does not overwrite the historical season. The tracking season defaults to
the latest season with available team data. Each season compares against the
immediately preceding season, using native player summaries, not the original
plan's older baseline. An original baseline snapshot is retained as a fallback
when that source is unavailable.

The Track Goals table starts with a team total: hit goals divided by all assigned
goals, counting one-objective plans once. Each player has Goal 1/Goal 2 progress
bars and one ✅/❌ per assigned goal. Green means the current metric meets the
target, red means it is farther from the target than last season, and black
means improving or unchanged. Missing data is labeled explicitly and never
treated as a zero statistic. Progress is the fraction of the baseline-to-target
gap closed, clamped to 0–100%. Clicking a player expands current-season,
game-by-game charts for their assigned objectives.

Charts plot the season-to-date value after each game, ordered by date and game
start time. Doubleheader games remain separate points, labeled G1/G2. The owning
app recalculates its native metrics from all recorded pitches through each game,
retaining its PA/swing/contact rules rather than averaging game percentages.
The x-axis shows game dates. D1 reference lines
reuse each Performance page's benchmarks (pitcher xwOBA uses its qualified-player
reference mean). Green segments begin/end at the exact numeric goal crossing,
with the appropriate higher/lower direction. A numeric chart-goal override
handles written goals containing multiple numbers.

The browser preview and PDF share the same renderer in
`WallyApps/shared/development_goals.R`. Single objectives are centered; two use
equal widths and margins. Empty drills are omitted. Text wraps and scales within
the boxes, with limits disclosed if entered text would become illegible.
The PDFs use original-photo cutouts on the left, game-by-game charts in the center and
one column of red/blue percentile rankings at the far right. Each rounded text
box sits directly on the ballpark background; there is no large white goal panel. Missing photos use initials, and unavailable values/ranks display N/A.
With two objectives, their charts sit side by side at half width, with wrapped
benchmark captions. The larger player name is bold gold. Percentile labels and
values are bold white without outlines. A maroon footer matches the top bar;
its bold white values statement has gold C, A, T and S initials.

Portrait sources are recorded in `WallyApps/shared/portraits.csv`; run
`python3 scripts/fetch_development_references.py` to refresh them from the official
[TXST roster](https://txst.com/sports/baseball/roster). Refreshing requires curl,
R, and magick because SIDEARM sometimes serves WebP bytes under JPEG URLs.
The app itself reads the normalized JPEGs without a runtime download.
`build_development_portrait_masks.R` removes the edge-connected backdrop and
feathers the alpha edge; alpha masks preserve every original RGB byte. Faces
are never generated or retouched. The runtime applies the small masks to native
rasters, keeping its bounded image cache small.

Both apps use the supplied Bobcat Ballpark template, copied unchanged to
`assets/development-ballpark-template.png`. Its maroon background, gold ballpark
linework and corner logos replace the earlier treated photograph. The renderer
places it at 90% opacity between the matching top and bottom bars. Text, stat
values, bars and borders remain vector elements in the PDF.

The editor page in both apps also uses that template through the shared Shiny
`base-development-assets` resource path. Maroon translucent editor panels keep
the outline visible; the live PNG preview and exported PDF use the same renderer.

### Hitter Defensive Goals

Hitting has an optional **Defensive Goals** editor below the hitting goal/drill
editors. It contains one written goal, one to three supporting actions and up
to five optional drills. All defensive boxes use gold backgrounds, maroon text
and white outlines in both the editor and graphic. A completely empty defensive
section is omitted and does not block existing hitting-only plans.

With two hitting objectives and a defensive plan, the PDF reserves three equal
columns: hitting goals 1 and 2 on the left/middle and defense on the right. Every
action and drill stays inside its goal's column. With one hitting goal, it uses
the left two thirds while defense retains the right third. Chart captions wrap
to keep their titles inside the chart viewport.

Defense is saved in the plan's separate `defense` field with schema version 2;
existing version-1 plans remain valid. It reloads and carries forward with the
numeric hitting objectives. Track Goals includes the written plan, actions and
drills when a player is expanded. Automatic goal-hit totals continue to measure
the numeric stat goals. Pitching keeps its original one/two-objective structure.

## Full-game score recovery

The player-filtered internal exports omit some historical PAs and players. The
compact `PitchingApp/reference/pa_scores_2026.csv` restores PA-start home/away
scores from complete game context in the D1 parquet, without adding national
pitches to a player's statistics. Raw explicit score snapshots take precedence.
The qualifying bands remain +1 to +3 for Tight Lead, -1 to -3 for Trailing Close,
and +1 to +3 in inning 7 or later for Save Situations.

Rebuild both this score reference and the pitcher xwOBA distribution with:

```
Rscript scripts/build_development_references.R /path/to/D1.parquet
```

The provenance JSON records source size, qualification, grid and row counts.
For the supplied 2026 source, Cade Smith has 49 completed PAs in Tight Lead and
18 in Save Situations. PAs still lacking verified context remain unclassified.

`scripts/tests/test_development_goals.R` verifies exact metric sets, required
fields, one/two-objective transitions, real portrait decoding, xwOBA outcomes,
and the Cade score regression. The existing Hitting and Pitching integration
suites verify both new tabs. Browser QA exercises both actual PDF downloads;
the PDFs are rendered and visually inspected for single/two-column alignment.
`test_development_tracking.R` covers persistent saves across new sessions,
revision conflicts, historical carry-forward, both metric directions, missing
data, one/two-goal team denominators and expandable tracking charts.
