# WallyApps metric decision register

This register is the contract boundary for integration. It records current
behavior; it does not declare either application correct. Jamie Jones and
Austin Wallace must approve any item marked `decision_required` before a shared
implementation replaces the standalone behavior.

| ID | Area | Status | Integration consequence |
|---|---|---|---|
| METRIC-001 | wOBA weights | decision required | Different hit weights change every wOBA comparison. |
| METRIC-002 | Barrel | decision required | Hitting, Pitching, and BASE currently use three formulas; Pitching also has historical calibration/exclusion logic. |
| METRIC-003 | Strike zone | decision required | Performance, framing, and BASE grid boundaries differ. Unit conversion must be centralized. |
| METRIC-004 | PA identity | decision required | PA construction differs, so counting and rate denominators can diverge. |
| METRIC-005 | Walk counting | confirmed defect | Pitching ignores `KorBB="Walk"` in `count_statline()` and its fallback PA builder. |
| METRIC-006 | Pitch type | decision required | Tagged, automatic, custom, and model classifications use inconsistent precedence. |
| METRIC-007 | xRV | decision required | Hard-coded D1 mean/SD need a model/version manifest. |
| METRIC-008 | FIP | decision required | The constant `3.214` needs a season and competition definition. |
| METRIC-009 | Catcher framing | candidate for reuse | The pure Defense calculations can be extracted once the zone and reference population are approved. |
| METRIC-010 | Defense/OAA | decision required | Event rows and player-position opportunities need separate stable keys. |
| METRIC-011 | Percentile references | provenance required | Production references need population, date range, sample threshold, generator, and hash. |
| METRIC-012 | Input deduplication | confirmed defect | Hitting double-loads `data`/`Data` on case-insensitive filesystems. |

The machine-readable source of truth, including formulas and file/line evidence,
is [metric-register.json](../../tests/baselines/wallyapps/metric-register.json).

The safe extraction order is: normalize events and units; establish stable game,
PA, pitch, and opportunity keys; centralize approved metric constants; then move
pure aggregators and plots behind BASE modules. Standalone golden outputs remain
the comparison oracle until each decision is signed off.
