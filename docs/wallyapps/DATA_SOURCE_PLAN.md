# Step 3 data-source decision and feature routing

## Decision

Use WallyApps' `D1 Pitching:Hitting File.parquet` as the build input for the
single canonical 2026 NCAA Division I pitch-event runtime. Despite being
described conversationally as a CSV, the selected file is Parquet: 2,051,176
rows, 212 columns, and approximately 1.06 GB.

The 2026 slice is suitable for the required national scope:

- 2,041,154 rows dated February 13 through June 21, 2026;
- 351 distinct team codes across pitcher and batter team fields;
- 2,051,176 unique `PitchUID` values and no duplicates in the unfiltered file;
- all 42 BASE core TrackMan columns;
- catcher, trajectory, release, approach-angle, confidence, model-pitch-type,
  home/away, stadium, level, and league fields beyond the current BASE core.

The file also contains 4,043 rows dated 2025. That slice is incomplete and
must be excluded from national season selectors and national comparisons. A
complete national 2025 archive is not required. Texas State's separate 2025,
fall, squad, and bullpen records may be retained only as a clearly labeled
team supplement.

The evidence is generated in
[`source-audit.json`](source-audit.json) by
`scripts/checks/audit_wally_data_sources.py`. It contains aggregate counts and
schemas only, never player names or pitch rows.

## One-copy architecture

The production bucket should contain one large, query-optimized 2026 pitch
dataset. Every BASE feature and every migrated Wally feature must query that
same runtime through shared access functions.

```text
Wally D1 master (staging/build input)
                  |
                  | select Date year = 2026
                  | require unique, nonblank PitchUID
                  v
Canonical partitioned NCAA D1 runtime  <--- pitch-retag override database
       |             |             |
 compact catalogs   TXST cache    feature queries
 (small)             (small)      (hitting/pitching/catching/defense)
```

During migration, staging and runtime copies may coexist long enough to prove
schema, counts, identity, and output parity. After cutover, the raw master must
move to cold archive or be removed from the mounted production bucket; that
destructive choice requires explicit approval. It must not be copied into each
app directory.

The existing hash-partition strategy remains appropriate because one physical
pitch row can support both pitcher and hitter queries. Compact catalogs tell a
query which partitions to inspect. The Texas State startup cache duplicates
only the small team slice, not the national file.

## Source-to-feature routing

| Feature family | Authoritative event source | Additional source | Rule |
|---|---|---|---|
| Texas State 2026 game reports | Canonical 2026 D1 runtime | TXST internal supplement | Primary master wins on matching `PitchUID`; internal competition stays labeled |
| NCAA opponent pitcher scouting | Canonical 2026 D1 runtime | Versioned metric references | All teams remain available; query selected players on demand |
| NCAA opponent hitter scouting | Canonical 2026 D1 runtime | Versioned metric references | Reuse pitcher partitions through the hitter bucket catalog |
| National leaderboards | Canonical 2026 D1 runtime | Versioned metric references | Never mix the incomplete 2025 slice into 2026 populations |
| Catching | Canonical 2026 D1 runtime | Catcher percentile reference | Derive from catcher/throw/location columns already in the master |
| Defense | Narrow D1 alignment companion | Canonical 2026 D1 runtime | Alignment source adds fielder positions; master supplies pitch/contact/outcome context |
| TXST history, fall, squads, bullpens | Deduplicated TXST supplement | Versioned metric references | Never present it as national coverage |
| JUCO statistics | JUCO aggregate source | None | Separate domain; do not union into NCAA pitch events |

The executable form of this matrix is
[`config/baseball_data_sources.json`](../../config/baseball_data_sources.json).
`R/data/source_contract.R` exposes it to BASE, and
`scripts/checks/check_data_source_contract.R` rejects invalid or duplicate
routes.

## Duplicate decisions

- Do not deploy `D1 Catching File.parquet`. Its 1,505,940 unique `PitchUID`
  values have 100% overlap with the selected pitch master, whose schema already
  includes the catching fields.
- Keep `D1 Defense File.parquet` only as a narrow alignment source. Of its
  2,411,699 unique `PitchUID` values, 530,472 are not in the pitch master, so it
  cannot safely be discarded or reduced to matched rows only.
- Normalize the separate Wally hitting and pitching team exports into one
  Texas State supplement. Do not retain app-specific production copies.
- Keep small reference populations and model artifacts separate because they
  are versioned metric inputs, not pitch-event duplicates.
- Keep pitch retags as keyed overrides. Never rewrite or fork the master to
  represent manual corrections.

## Wiring boundary

BASE now loads the source contract before its data-access modules. The existing
pitcher and hitter query functions label rows from the selected D1 source, and
the configuration exposes the national master, national defense source, shared
runtime, and Texas State supplement paths independently.

The standalone Wally apps remain unchanged while their Step 2 goldens are the
comparison oracle. As each Wally feature moves into BASE, it must use the route
in the contract rather than its standalone `data/` directory loader. A feature
is not considered migrated until its selectors, tables, plots, reports, and
downloads all pass through the shared route and its approved metric decision.

## Cutover gates

1. Build the runtime from the selected Wally master with `--season 2026`.
2. Require exactly 2,041,154 selected rows and the same number of unique,
   nonblank `PitchUID` values.
3. Rebuild pitcher, hitter, and team catalogs and confirm all 351 team codes are
   reachable for opponent scouting.
4. Normalize the Texas State supplement and quarantine rows that cannot be
   assigned a stable event key or competition label.
5. Join defense by unique `PitchUID`, then the audited game/pitch/date fallback;
   preserve unmatched alignments.
6. Run BASE checks and Wally synthetic/golden parity for every migrated feature.
7. Switch deployment paths atomically. Only after validation and explicit
   approval, retire redundant mounted/app-local large files.

## Known data caveats

- Source metadata says its query window begins February 13, 2026 even though
  4,043 rows are dated 2025. Runtime selection therefore uses row-level `Date`,
  not the source's query metadata.
- The separate Texas State app exports contain many rows without `PitchUID`.
  They require fallback identity and competition-label validation before union.
- The standalone Pitching UI loading defect and metric conflicts recorded in
  [`BASELINE.md`](BASELINE.md) remain feature-migration issues; selecting a data
  source does not silently change those calculations.
