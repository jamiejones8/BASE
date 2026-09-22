# Preliminary performance foundation

This work improves final BASE responsiveness without changing Wally's visible
tabs, page layouts, calculations, or standalone comparison baselines.

## Why internal restructuring is worthwhile

The imported applications currently total 32,056 lines in three top-level R
files:

| App | Lines | Source size | Reactive/render registrations |
|---|---:|---:|---:|
| HittingApp | 9,160 | 344 KB | 77 |
| PitchingApp | 19,823 | 775 KB | 203 |
| DefenseApp | 3,073 | 131 KB | 53 |

File count alone is not a runtime problem. The expensive behaviors are eager
data/model loading, repeated filtering and aggregation, and registering every
workspace's reactive graph at session startup.

## Implemented now

### Lazy workspace server initialization

`R/performance/lazy_workspace.R` registers a workspace callback against the
main navigation input and runs it once, on the first visit. A failed
initialization can be retried by leaving and reopening the workspace.

The current BASE app now defers these server graphs:

- Leaderboards;
- opposing-pitcher scouting;
- opposing-hitter scouting;
- Defensive Analytics;
- the postgame pitcher card;
- the season pitcher card;
- HomeBASE's predecessor page.

Postgame Reports also initializes only the visible AAR workspace. Opening the
default Pitching AAR no longer sources the Hitting and Defense applications in
the same session. Those workspaces still initialize normally if their AAR tab
or full workspace is opened.

### Player-level Cape queries

The Cape Cod Parquet source remains an Arrow dataset instead of expanding the
entire 199-column file into an R data frame at startup. Pitcher and hitter
pages collect only the selected player's rows, retain the same ID/name matching
and source labels, and reuse a bounded 16-player process cache.

The full Pitching and Hitting workspaces follow the same deferred pattern, so
their reactive graphs are not constructed until a user opens the corresponding
workspace or AAR.

### Lazy model deserialization

Four legacy compatibility assets previously deserialized at application source
time even though their card tab no longer uses them. The two large files alone
are approximately 167 MB and 78 MB. They now use delayed bindings and load only
if legacy code actually requests them.

The postgame pitching engine's 8.5 MB BrewStuff model and its two reference
tables now use the same first-use behavior. Opening Home, Hitting, Defense, or
opponent scouting no longer pays that pitching-report startup cost.

The superseded upload-scouting module is no longer sourced by the unified app;
the active Opponent Scouting adapter retains its existing workflows and lazy
data access without paying for the unused module's SQLite/XGBoost runtime.

### Shared bounded cache primitive

The performance layer includes a dependency-free LRU cache with explicit size,
key order, eviction, and clear operations. Migrated Wally modules can cache one
prepared player/team payload and share it among their existing tabs without
allowing per-session memory to grow indefinitely.

BASE's current national pitcher, hitter, and defense loaders already use
bounded caches. They now enforce both entry and byte ceilings: 64 MiB each for
pitcher and hitter rows, and 128 MiB for defense-team rows. An oversized current
selection remains usable, but older entries are evicted and transparently
re-read from Parquet when needed.

## Required migration pattern

Every migrated Wally workspace should follow this sequence:

1. initialize its server graph only on first visit;
2. request the smallest team/player/date slice from the shared source;
3. normalize that slice once;
4. expose one prepared reactive payload to the existing tabs;
5. cache expensive summaries by player, date range, split, and metric version;
6. keep hidden outputs suspended unless a report download needs them;
7. invalidate cached values when a retag revision or source version changes.

Splitting files into `ui`, `server`, `data`, `metrics`, `plots`, and `reports`
is still valuable for testing and maintainability, but it is not counted as a
speed optimization by itself.

## Verification

`scripts/tests/test_lazy_workspace.R` covers once-only initialization, retry
after failure, late output/observer registration, LRU reads, recency updates,
eviction, and clearing. The BASE configuration, source contract, retag tests,
workspace/feature contracts, service tests, startup smoke test, and all Wally
integration suites are available through `scripts/checks/run_all.R`.

Deployment-grade before/after measurements must still be captured with the
production dependency image and mounted 2026 runtime. The important metrics
are process startup time, first Home render, first workspace open, repeat
workspace open, selected-player query time, and per-session memory.
