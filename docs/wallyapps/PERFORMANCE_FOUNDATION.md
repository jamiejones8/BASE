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

This pattern will wrap the final Pitching and Hitting workspaces so their full
reactive graphs are not constructed for a user who only opens a postgame
report.

### Lazy model deserialization

Four legacy compatibility assets previously deserialized at application source
time even though their card tab no longer uses them. The two large files alone
are approximately 167 MB and 78 MB. They now use delayed bindings and load only
if legacy code actually requests them.

The postgame pitching engine's 8.5 MB BrewStuff model and its two reference
tables now use the same first-use behavior. Opening Home, Hitting, Defense, or
opponent scouting no longer pays that pitching-report startup cost.

The older upload-scouting module's model bundle and xwOBA grid also load and
cache on first scoring use rather than when the R file is sourced.

### Shared bounded cache primitive

The performance layer includes a dependency-free LRU cache with explicit size,
key order, eviction, and clear operations. Migrated Wally modules can cache one
prepared player/team payload and share it among their existing tabs without
allowing per-session memory to grow indefinitely.

BASE's current national pitcher, hitter, and defense loaders already use
bounded caches. They remain unchanged until the common primitive can replace
their local implementations with parity tests.

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

The local system R does not contain Arrow, so a full BASE runtime timing was not
available in this environment. Deployment-grade before/after measurements must
be captured with the production dependency image and mounted 2026 runtime. The
important metrics are process startup time, first Home render, first workspace
open, repeat workspace open, selected-player query time, and per-session memory.
