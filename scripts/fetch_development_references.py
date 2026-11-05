#!/usr/bin/env python3
"""Refresh public TXST portraits for Development Goals."""
import concurrent.futures
import csv
import html
import json
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
ASSETS = ROOT / "WallyApps/shared/assets"
CACHE = Path(tempfile.gettempdir()) / "base-txst-roster"
ASSETS.mkdir(parents=True, exist_ok=True)
CACHE.mkdir(parents=True, exist_ok=True)

def fetch(url, path):
    if not url.startswith("https://txst.com/"):
        raise ValueError("Only official Texas State URLs are allowed")
    subprocess.run(["curl", "--fail", "--location", "--silent", "--show-error", "--retry", "2", url, "-o", str(path)], check=True)
    return path

roster = fetch("https://txst.com/sports/baseball/roster", CACHE / "roster.html").read_text()
people = []
for raw in re.findall(r'<script[^>]+type="application/ld\+json"[^>]*>(.*?)</script>', roster, re.S):
    obj = json.loads(raw)
    if isinstance(obj, dict) and isinstance(obj.get("item"), list):
        people.extend(x for x in obj["item"] if x.get("@type") == "Person")
records = []
jobs = []
for person in people:
    name = person["name"]
    if not person.get("image"):
        records.append({"Name": name, "File": "", "SourceURL": "https://txst.com/sports/baseball/roster"})
        continue
    url = person["image"]["url"].replace("http://", "https://")
    filename = "portrait-" + re.sub(r"[^a-z0-9]+", "-", name.lower()).strip("-") + ".jpg"
    records.append({"Name": name, "File": filename, "SourceURL": url})
    jobs.append((url, ASSETS / filename))
with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
    list(pool.map(lambda job: fetch(*job), jobs))
# SIDEARM may return WebP bytes for a .jpg URL. Normalize the container only;
# no portrait retouching, cropping or other visual edits are performed.
subprocess.run(["Rscript", "-e", 'for(p in list.files(commandArgs(TRUE)[1],pattern="^portrait-.*[.]jpg$",full.names=TRUE)) magick::image_write(magick::image_read(p),p,format="jpeg",quality=95)', str(ASSETS)], check=True)
with (ASSETS.parent / "portraits.csv").open("w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=["Name", "File", "SourceURL"], lineterminator="\n")
    writer.writeheader()
    writer.writerows(records)
subprocess.run(["Rscript", str(ROOT / "scripts/build_development_portrait_masks.R")], cwd=ROOT, check=True)
print(f"Saved {len(records)} official portrait references and transparent masks.")
