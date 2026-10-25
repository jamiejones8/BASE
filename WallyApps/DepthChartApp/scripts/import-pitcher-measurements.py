"""Snapshot Fall 2026 release means and matching pitcher-profile heights for the app."""
import csv, json, math
from collections import defaultdict
from pathlib import Path
app=Path(__file__).resolve().parents[1]
root=app.parent
heights={}
for r in csv.DictReader((root/'PitchingApp/data/player_heights.csv').open()):
    heights[(r['player_first'].strip().lower(),r['Player_last'].strip().lower())]=float(r['Player_height'])
release=defaultdict(list)
for r in csv.DictReader((root/'PitchingApp/data/2026 Fall - cleaned.csv').open()):
    name=r['Pitcher'].split(',')
    if len(name)!=2:continue
    try: h=float(r['RelHeight']);side=float(r['RelSide'])
    except (ValueError,TypeError):continue
    if math.isfinite(h) and math.isfinite(side):release[(name[1].strip().lower(),name[0].strip().lower())].append((h,side))
with (app/'data/roster.csv').open(newline='') as f:
    next(f);next(f);roster=list(csv.DictReader(f))
result={}
for i,r in enumerate(roster,1):
    key=(r['First'].strip().lower(),r['Last'].strip().lower())
    data={}
    if key in heights:data['heightIn']=heights[key]
    values=release.get(key)
    if values:
        data.update(releaseHeightFt=round(sum(v[0] for v in values)/len(values),4),releaseSideFt=round(sum(abs(v[1]) for v in values)/len(values),4),releaseSource='Fall 2026 · mean of valid tracked pitches',releaseSampleCount=len(values))
    if data:result[f'roster-{i}']=data
(app/'data/pitcher-measurements.json').write_text(json.dumps(result,indent=2)+'\n')
print(f'Imported {len(result)} matching pitcher profiles; {sum("releaseHeightFt" in p for p in result.values())} have release measurements.')
