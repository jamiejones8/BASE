# WallyApps import hygiene

`WallyApps/` is a migration working copy used to compare Austin Wallace's
standalone applications with BASE. The production context includes only the
migrated Pitching, Hitting, Defense, and Scouting sources, their required
visual assets, and the small approved reference tables. Private season data,
model artifacts, and unmigrated applications remain excluded from Docker and
Railway uploads.

## Version-control policy

Application source, test fixtures, JUCO source registries, and documentation
remain visible to Git. The root `.gitignore` excludes:

- Chrome/Playwright browser profiles and browser debug captures;
- `rsconnect` deployment metadata;
- R session/history files and Python caches;
- CatBoost training output;
- raw team, D1, catching, defense, and generated JUCO data;
- unreviewed pickle model artifacts.

These exclusions do not delete local files. D1 and team data are candidates
for the existing private BASE data bucket after source-of-truth and access
decisions are approved. Model pickles remain quarantined until their provenance,
runtime versions, validation results, and long-term storage location are
documented.

## Artifact manifest

`artifact-manifest.json` records SHA-256 hashes, sizes, CSV headers and row
counts, and Parquet schema metadata for the imported datasets and models. It
does not contain player rows. Pickle and `.RData` files are hashed as opaque
bytes and are never deserialized. Browser profiles and deployment metadata are
not inspected at all.

Regenerate the manifest after an intentionally approved artifact update:

```sh
python3 scripts/checks/build_wallyapps_artifact_manifest.py
```

Verify that the checked-in manifest still describes the local working copy:

```sh
python3 scripts/checks/build_wallyapps_artifact_manifest.py --check
```

The manifest is provenance evidence, not authorization to commit or distribute
the underlying data or models.

## Integration baseline

Step 2's synthetic fixtures, locked runtime, metric decision register, golden
tables/plots/PDFs, and one-command parity check are documented in
[`BASELINE.md`](BASELINE.md). These artifacts preserve standalone behavior while
shared schemas and metric definitions are reviewed before extraction into BASE.

## Data-source decision

Step 3 selects the Wally full Division I pitching/hitting Parquet as the build
input for one shared 2026 national runtime, while explicitly preventing its
small incomplete 2025 slice from being treated as national coverage. Catching
derives from that master, defense remains a narrow keyed companion, and Texas
State historical/internal data becomes a labeled supplement. See
[`DATA_SOURCE_PLAN.md`](DATA_SOURCE_PLAN.md) and the aggregate
[`source-audit.json`](source-audit.json).

## Application structure

Step 4 organizes the integrated product around seven Home workspaces rather
than a flat global navbar. Postgame Reports contains only the three existing
BASE PDF generators; separate Pitching and Hitting workspaces own team
analysis; Opponent Scouting retains national pitcher/hitter search; Defense
owns interactive catcher framing; HomeBASE provides national player search and
roster-card profiles; and Data Processing owns retagging. See
[`APP_STRUCTURE.md`](APP_STRUCTURE.md).

## Performance foundation

Heavy workspace servers and large model assets now initialize on first use,
and a bounded shared cache is available for migrated modules. This improves
startup behavior without changing Wally's internal tabs or calculations. See
[`PERFORMANCE_FOUNDATION.md`](PERFORMANCE_FOUNDATION.md).

## Pitching integration

The unified Home shell and first complete Wally workspace are documented in
[`PITCHING_INTEGRATION.md`](PITCHING_INTEGRATION.md). Pitching preserves the
original Wally tab/server structure while receiving its data and visual shell
from BASE.

## Hitting integration

The second complete Wally workspace is documented in
[`HITTING_INTEGRATION.md`](HITTING_INTEGRATION.md). Hitting preserves all ten
original workflows—including Lineup Builder, AAR, Leaderboard, team reporting,
ball flight, and contact point—while receiving shared data, lazy loading, and a
scoped BASE visual shell.

## Defense integration

The third complete Wally workspace is documented in
[`DEFENSE_INTEGRATION.md`](DEFENSE_INTEGRATION.md). Defense preserves all seven
original workflows, including interactive game and season catcher receiving,
while the existing catcher PDF generator remains under Postgame Reports.

## Scouting integration

Opponent Scouting now hosts Wally's complete ScoutingApp in place of the old
two-card landing page. Its hitter and pitcher cards, pitch-type tables, heat
maps, Stuff Sheet, Matchup Grid, ordering, previews, and PDF exports remain
owned by the original server. See
[`SCOUTING_INTEGRATION.md`](SCOUTING_INTEGRATION.md).
