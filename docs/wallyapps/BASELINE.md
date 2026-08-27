# Step 2 baseline: WallyApps behavior before integration

Step 2 creates a reproducible comparison oracle around the untouched standalone
apps. It does not move Wally code into BASE and it does not use imported player
rows. Every checked-in fixture identity, event, date, identifier, measurement,
and reference-population row is generated synthetically.

## What is locked

- `tests/baselines/wallyapps/renv.lock` pins the R version and direct/transitive
  R package graph used by the standalone apps.
- `tests/baselines/wallyapps/runtime-manifest.json` records the observed R,
  Python, Node, OS, package versions, and hashes of the three app source files.
- `tests/fixtures/wallyapps/` contains deterministic synthetic CSVs whose
  headers match the imported app inputs.
- `tests/baselines/wallyapps/golden/` contains calculation tables and rendered
  PNG/PDF output plus a live-browser shell capture for Hitting, Pitching, and
  Defense. Browser observations are recorded in `ui-shell-observations.json`.
- `tests/baselines/wallyapps/metric-register.json` records formula conflicts and
  approval decisions; `METRIC_REGISTER.md` is its readable summary.

The ignored `.baseline-runtime/` directory is deliberately separate from the
BASE production runtime.

## Reproduce and verify

From the BASE project root:

```sh
Rscript scripts/baselines/wallyapps/install_r_dependencies.R
python3 scripts/baselines/wallyapps/build_fixtures.py
python3 scripts/baselines/wallyapps/validate_fixtures.py
python3 scripts/baselines/wallyapps/capture_runtime_manifest.py --check
python3 scripts/baselines/wallyapps/check_goldens.py
```

The last command performs privacy validation, sources each untouched app in an
isolated temporary working directory, regenerates all outputs, compares CSV,
JSON, and PNG bytes, and compares the first rendered page of every PDF.

When an approved change is supposed to alter standalone parity, regenerate and
review the changed artifacts explicitly:

```sh
python3 scripts/baselines/wallyapps/check_goldens.py --update
python3 scripts/baselines/wallyapps/check_goldens.py
```

Do not update goldens merely to make a failing check green. The metric decision
or defect fix that explains the change must be approved and recorded first.

## Current baseline coverage

| App | Calculation goldens | Visual goldens |
|---|---|---|
| Hitting | overall line, pitch-type table, pitch usage, swing decisions | strike-zone PNG/PDF |
| Pitching | stat line, process metrics, pitch-type performance, Called Stuff | strike-zone and movement PNG/PDF |
| Defense | player-position summary, leaderboard, catcher framing percentiles | SS opportunities, 360-degree range, catcher framing PNG/PDF |

Each app also has a 1280x720 `app-shell.jpg` captured from a fresh local browser
session. These screenshots are manually refreshed because the calculation
runtime intentionally does not install or depend on Playwright.

The standalone source smoke counts are Hitting 736 rows, Pitching 460 rows, and
Defense 672 exploded player-position rows. Hitting's 736 is intentionally
preserved evidence of its `data`/`Data` double-load behavior on case-insensitive
filesystems, not the desired future count.

## Confirmed observations requiring deliberate treatment

- Hitting loads the same four 92-row files twice on the current macOS filesystem.
- Pitching's `count_statline()` recognizes `BB`/`IBB`, but its source and the
  fixture use `KorBB="Walk"`; the golden stat line therefore records zero walks.
- In two isolated fresh-browser runs, Pitching left 48 loading indicators after
  seven seconds and emitted six Shiny client errors around the
  `performance_percentiles` output state. Hitting and Defense rendered with no
  console errors. The blank Pitching shell is preserved as evidence, not fixed
  during baseline capture. Pitching did discover the synthetic pitcher, season,
  and game, and its pure calculation goldens completed, so this is not a
  missing-player-data condition; the exact UI lifecycle cause remains a Step 3
  diagnostic item.
- The standalone and BASE definitions of wOBA, Barrel, strike zone, PA identity,
  and pitch-type precedence are not interchangeable.
- xRV, Called Stuff, pitch-metric, and catcher percentile reference data need a
  production provenance contract before migration.
- The offline app goldens consume precomputed reference CSVs, so CatBoost,
  scikit-learn, and Playwright are not part of this calculation runtime. Model
  retraining and full browser journey tests belong to later integration steps.

## Privacy boundary

`validate_fixtures.py` checks all fixture CSV cells for local paths, email
addresses, known owner names, Texas State/Bobcats names, and non-synthetic values
in identity columns. The two filenames beginning with `BobcatsDefense` are kept
only because the untouched Defense loader requires those exact filenames; their
contents are synthetic.

Imported raw data and model artifacts remain outside this baseline. The
artifact manifest documents their hashes and schemas without authorizing them
for source control or production use.
