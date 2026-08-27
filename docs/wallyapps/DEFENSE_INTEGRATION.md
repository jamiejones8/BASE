# Defense workspace integration

Wally's complete DefenseApp now runs inside BASE as the
`defense_workspace`. It shares the main Shiny process and session; it is not an
iframe or a separately launched application.

## Preserved behavior

The seven original top-level workflows remain in their existing order:

1. Leaderboard;
2. Opportunities;
3. OF OAA;
4. IF OAA;
5. OAA;
6. Catcher Reports; and
7. Catcher Season.

Their nested tabs, filters, calculations, plots, tables, percentile views, and
downloads remain owned by Wally's server. The standalone DefenseApp retains
its original loader and page shell for locked golden comparisons.

Internal Shiny IDs that overlapped Pitching or Hitting are workspace-prefixed.
This does not alter visible labels or behavior, but prevents one workspace's
filters and outputs from driving another workspace in the shared session.

## Shared data routes

BASE injects three bounded inputs before Wally's standardization runs:

- the joined Texas State defense runtime supplies positioning, batted-ball
  context, and canonical event keys without loading another season CSV;
- the canonical Texas State slice of the shared 2026 NCAA Division I pitch
  source supplies interactive catching rows; and
- the small catcher-framing reference population is read from
  `BASE_CATCHER_FRAMING_REFERENCE_FILE` when configured.

The deployment path defaults to
`/base-data/derived2026/reference/d1_catcher_framing_metrics.csv`. This is a
small versioned metric table, not a second pitch-event archive.

Local Wally CSVs are development fallbacks only and are excluded from the
production image. The large national master is never loaded by DefenseApp.

## Catching ownership

Interactive catcher game and season analysis lives in Defensive Analytics.
The existing BASE Catching Report remains unchanged under Postgame Reports as
one of the three PDF generators. These are two entry points over the canonical
catching source, not duplicate season datasets.

## Navigation, appearance, and loading

The Defense Home card opens the complete Wally workspace. Its internal page
hierarchy is preserved while the standalone navbar, global body styling, and
watermarks are replaced by a scoped BASE shell. The workspace initializes only
on its first visit and reuses its prepared read-only data thereafter.

Data Processing deep-links directly to the existing persistent BASE
pitch-retag controls. It does not create a second retagger or copy pitch data;
the later visual-refinement pass can extract those controls into a dedicated
page without changing their SQLite override layer.

## Verification

`scripts/tests/test_wally_defense_integration.R` verifies the seven-workflow
UI contract, embedded shell isolation, fixture standardization, defense
summary, catcher preparation, season filtering, and catcher selector. The
standalone golden suite remains the calculation/rendering parity gate.
