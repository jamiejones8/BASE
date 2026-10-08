# Fall defense sources

Updated October 8, 2026 from the desktop TxSt app:

- `2026 Fall Defense.csv`: player positioning and manual fielding tags, 1,029 pitches.
- `2026 Fall Batted Balls.csv`: the TxSt defense app's companion TrackMan export.
- `2026 Fall Batted Balls - Oct 2.csv` and `2026 Fall Batted Balls - Oct 6.csv`: matching game exports from the desktop Pitch Retagger data folder. These supply the 138 positioning pitch IDs absent from the companion export.

CSV line endings are normalized, and Justin Anctil's spelling is retained from the BASE cleanup. The source desktop files are unchanged.

The BASE adapter excludes spring 2026 positioning because its fielding tags are incompatible. The embedded app retains the TxSt defensive layouts and calculations, with BASE's navigation, game labels, and catcher postgame integration. Positioning is enriched by PitchUID, with guarded fallback keys; missing contact measurements remain missing. All 1,029 fall positioning pitches have a matching contact-source row.

## Season uploads

In Data Processing, use either **2026 fall player positioning** or **2027 Scrimmages player positioning**, select a game CSV, then click Validate and append. Each card writes to its own cumulative positioning file. Fall positions match the 2026 Fall TrackMan source; 2027 scrimmage positions match the 2027 Pre Season TrackMan source. Existing stored filenames are preserved. Upload order does not matter: positions match when their companion TrackMan data arrives. Reload BASE after an import if Defense is already open.

Managed season TrackMan exports take precedence over bundled contact copies. The legacy positioning source remains a fall-2026 fallback; new uploads use the selected season explicitly.
