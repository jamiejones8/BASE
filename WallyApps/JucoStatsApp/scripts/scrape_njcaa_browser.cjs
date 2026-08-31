#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process");

function requirePlaywright() {
  try {
    return require("playwright");
  } catch (error) {
    const bundledPath = path.join(
      process.env.HOME || "",
      ".cache",
      "codex-runtimes",
      "codex-primary-runtime",
      "dependencies",
      "node",
      "node_modules",
      "playwright"
    );
    return require(bundledPath);
  }
}

function parseArgs(argv) {
  const args = {
    season: "2025-26",
    division: "1",
    baseUrl: "https://njcaa.prestosports.com",
    mode: "sources",
    maxPages: 30,
    outDir: path.join("JucoStatsApp", "data", "raw_njcaa"),
    output: path.join("JucoStatsApp", "data", "juco_player_stats_latest.csv"),
    registry: path.join("JucoStatsApp", "data", "program_registry.csv"),
    sources: path.join("JucoStatsApp", "data", "program_sources.csv"),
    profileDir: path.join("JucoStatsApp", ".browser-profile"),
    debugDir: path.join("JucoStatsApp", "data", "debug_njcaa"),
    headless: true,
    timeoutMs: 90000,
    delayMs: 750,
    parse: true,
    continueOnError: true,
    limit: 0,
    programFilter: "",
    sourceGroupFilter: "",
    sourceKindFilter: "",
  };

  for (const arg of argv) {
    if (!arg.startsWith("--")) continue;
    const [rawKey, ...rest] = arg.slice(2).split("=");
    const key = rawKey.replace(/-([a-z])/g, (_, c) => c.toUpperCase());
    const value = rest.length ? rest.join("=") : "true";

    if (["maxPages", "timeoutMs", "delayMs", "limit"].includes(key)) {
      args[key] = Number(value);
    } else if (["headless", "parse", "continueOnError"].includes(key)) {
      args[key] = !["false", "0", "no"].includes(String(value).toLowerCase());
    } else if (key === "headed") {
      args.headless = false;
    } else {
      args[key] = value;
    }
  }

  return args;
}

function parseCsv(text) {
  const rows = [];
  let row = [];
  let field = "";
  let inQuotes = false;

  for (let index = 0; index < text.length; index += 1) {
    const char = text[index];
    const next = text[index + 1];
    if (char === '"' && inQuotes && next === '"') {
      field += '"';
      index += 1;
    } else if (char === '"') {
      inQuotes = !inQuotes;
    } else if (char === "," && !inQuotes) {
      row.push(field);
      field = "";
    } else if ((char === "\n" || char === "\r") && !inQuotes) {
      if (char === "\r" && next === "\n") index += 1;
      row.push(field);
      if (row.some((value) => value.trim() !== "")) rows.push(row);
      row = [];
      field = "";
    } else {
      field += char;
    }
  }
  row.push(field);
  if (row.some((value) => value.trim() !== "")) rows.push(row);

  const headers = rows.shift() || [];
  return rows.map((values) =>
    Object.fromEntries(headers.map((header, index) => [header.trim(), (values[index] || "").trim()]))
  );
}

function csvEscape(value) {
  const text = String(value ?? "");
  return /[",\n\r]/.test(text) ? `"${text.replace(/"/g, '""')}"` : text;
}

function buildUrl({ season, division }, pos, page, sort) {
  const baseUrl = (arguments[0].baseUrl || "https://njcaa.prestosports.com").replace(/\/+$/, "");
  return `${baseUrl}/sports/bsb/${season}/div${division}/players?pos=${pos}&r=${page}&sort=${sort}&view=`;
}

function statConfigs() {
  return [
    { statType: "hitting", pos: "h", sort: "avg" },
    { statType: "pitching", pos: "p", sort: "era" },
    { statType: "fielding", pos: "f", sort: "pb" },
  ];
}

function isChallengeText(text) {
  return /just a moment|enable javascript and cookies|checking your browser|performing security verification|security service to protect against malicious bots|cf_chl|challenge-platform/i.test(
    text
  );
}

async function waitForStatsTable(page, timeoutMs, headless) {
  const started = Date.now();
  let lastStatus = "";

  while (Date.now() - started < timeoutMs) {
    let html = "";
    try {
      html = await page.content();
    } catch (error) {
      const status = "navigating";
      if (status !== lastStatus) {
        console.log(`  page status: ${status}`);
        lastStatus = status;
      }
      await page.waitForTimeout(3000);
      continue;
    }
    const plainText = html.replace(/<[^>]+>/g, " ");
    const lowerHtml = html.toLowerCase();
    const status = isChallengeText(plainText)
      ? "challenge"
      : lowerHtml.includes("<table") && lowerHtml.includes("name") && lowerHtml.includes("team")
        ? "table"
        : /player stats/i.test(plainText)
          ? "player-stats-no-table"
          : /site has been disabled per owner request/i.test(plainText)
            ? "disabled"
            : "loading";

    if (status === "table") return;
    if (status === "disabled") {
      throw new Error("The source host returned a disabled-site page. Try --base-url=https://njcaa.prestosports.com.");
    }
    if (status !== lastStatus) {
      console.log(`  page status: ${status}`);
      if (status === "challenge" && !headless) {
        console.log("  complete the browser verification window if it appears; this profile will be reused later");
      }
      lastStatus = status;
    }
    await page.waitForTimeout(3000);
  }

  throw new Error(
    headless
      ? `Timed out waiting for a rendered stats table after ${timeoutMs} ms. Headless Chromium is likely being challenged; try --headless=false once to seed the browser profile.`
      : `Timed out waiting for a rendered stats table after ${timeoutMs} ms.`
  );
}

async function waitForPlayerLineupTable(page, timeoutMs, headless) {
  const started = Date.now();
  let lastStatus = "";

  while (Date.now() - started < timeoutMs) {
    let html = "";
    try {
      html = await page.content();
    } catch (error) {
      const status = "navigating";
      if (status !== lastStatus) {
        console.log(`  page status: ${status}`);
        lastStatus = status;
      }
      await page.waitForTimeout(3000);
      continue;
    }
    const plainText = html.replace(/<[^>]+>/g, " ");
    const hasNameHeader = /<th[\s\S]{0,1200}>\s*name\s*</i.test(html);
    const status = isChallengeText(plainText)
      ? "challenge"
      : /site has been disabled per owner request/i.test(plainText)
        ? "disabled"
        : hasNameHeader
          ? "player-table"
          : "loading";

    if (status === "player-table") return;
    if (status === "disabled") {
      throw new Error("The source host returned a disabled-site page.");
    }
    if (status !== lastStatus) {
      console.log(`  page status: ${status}`);
      if (status === "challenge" && !headless) {
        console.log("  complete the browser verification window if it appears; this profile will be reused later");
      }
      lastStatus = status;
    }
    await page.waitForTimeout(3000);
  }

  throw new Error(`Timed out waiting for a player lineup table after ${timeoutMs} ms.`);
}

async function waitForSidearmStatsTable(page, timeoutMs) {
  const started = Date.now();
  let lastStatus = "";

  while (Date.now() - started < timeoutMs) {
    let html = "";
    try {
      html = await page.content();
    } catch (error) {
      const status = "navigating";
      if (status !== lastStatus) {
        console.log(`  page status: ${status}`);
        lastStatus = status;
      }
      await page.waitForTimeout(3000);
      continue;
    }
    const plainText = html.replace(/<[^>]+>/g, " ");
    const hasPlayerHeader = /<th[\s\S]{0,1200}>\s*player\s*</i.test(html);
    const hasStatsAnchors = /individual-overall-batting|individual-overall-pitching|individual-overall-fielding/i.test(html);
    const status = isChallengeText(plainText)
      ? "challenge"
      : hasPlayerHeader || hasStatsAnchors
        ? "sidearm-stats"
        : "loading";

    if (status === "sidearm-stats") return;
    if (status !== lastStatus) {
      console.log(`  page status: ${status}`);
      lastStatus = status;
    }
    await page.waitForTimeout(3000);
  }

  throw new Error(`Timed out waiting for SIDEARM stats tables after ${timeoutMs} ms.`);
}

async function saveDebugArtifacts(page, args, config, pageNumber) {
  fs.mkdirSync(args.debugDir, { recursive: true });
  const baseName = `${config.statType}_page_${pageNumber}_${Date.now()}`;
  const htmlPath = path.join(args.debugDir, `${baseName}.html`);
  const pngPath = path.join(args.debugDir, `${baseName}.png`);

  try {
    fs.writeFileSync(htmlPath, await page.content(), "utf8");
    await page.screenshot({ path: pngPath, fullPage: true });
    console.log(`  saved debug artifacts: ${htmlPath}, ${pngPath}`);
  } catch (error) {
    console.log(`  failed to save debug artifacts: ${error.message}`);
  }
}

async function scrapePage(page, args, config, pageNumber, rawDir) {
  const url = buildUrl(args, config.pos, pageNumber, config.sort);
  console.log(`Scraping ${config.statType} page ${pageNumber}: ${url}`);

  try {
    await page.goto(url, { waitUntil: "domcontentloaded", timeout: args.timeoutMs });
    await waitForStatsTable(page, args.timeoutMs, args.headless);
  } catch (error) {
    await saveDebugArtifacts(page, args, config, pageNumber);
    throw error;
  }

  const html = await page.content();
  if (isChallengeText(html)) {
    await saveDebugArtifacts(page, args, config, pageNumber);
    throw new Error(
      args.headless
        ? "Headless Chromium reached a Cloudflare challenge page, not rendered stats HTML. Run once with --headless=false to seed the browser profile."
        : "Browser reached a Cloudflare challenge page, not rendered stats HTML."
    );
  }

  if (/Site has been disabled per owner request/i.test(html)) {
    await saveDebugArtifacts(page, args, config, pageNumber);
    throw new Error("The source host returned a disabled-site page. Try --base-url=https://njcaa.prestosports.com.");
  }

  const outputPath = path.join(rawDir, `${config.statType}_page_${pageNumber}.html`);
  fs.writeFileSync(outputPath, html, "utf8");
  return outputPath;
}

async function scrapeAll(args) {
  const { chromium } = requirePlaywright();
  const seasonDir = args.season.replace(/[^A-Za-z0-9._-]/g, "_");
  const rawDir = path.join(args.outDir, seasonDir);
  fs.mkdirSync(rawDir, { recursive: true });
  fs.mkdirSync(args.profileDir, { recursive: true });

  const context = await chromium.launchPersistentContext(args.profileDir, {
    headless: args.headless,
    viewport: { width: 1440, height: 1000 },
    userAgent:
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 " +
      "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36",
  });

  try {
    const page = context.pages()[0] || (await context.newPage());
    const saved = [];

    for (const config of statConfigs()) {
      for (let pageNumber = 0; pageNumber <= args.maxPages; pageNumber += 1) {
        try {
          saved.push(await scrapePage(page, args, config, pageNumber, rawDir));
        } catch (error) {
          if (pageNumber === 0) throw error;
          console.log(`  stopping ${config.statType} after page ${pageNumber - 1}: ${error.message}`);
          break;
        }
        await page.waitForTimeout(args.delayMs);
      }
    }

    return { rawDir, saved };
  } finally {
    await context.close();
  }
}

function slugify(value) {
  return String(value || "source")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-|-$/g, "");
}

async function scrapeSourcePage(page, args, source, rawDir) {
  const programSlug = slugify(source.program_name || source.stats_team_name || source.source_url);
  const sourceSlug = slugify(source.source_kind || "source");
  const outputPath = path.join(rawDir, `${programSlug}-${sourceSlug}.html`);
  console.log(`Scraping source for ${source.program_name}: ${source.source_url}`);

  await page.goto(source.source_url, { waitUntil: "domcontentloaded", timeout: args.timeoutMs });
  if (source.source_kind === "sidearm_stats") {
    await waitForSidearmStatsTable(page, args.timeoutMs);
  } else {
    await waitForPlayerLineupTable(page, args.timeoutMs, args.headless);
  }

  const html = await page.content();
  fs.writeFileSync(outputPath, html, "utf8");
  return { ...source, file: outputPath };
}

async function scrapeSources(args) {
  if (!fs.existsSync(args.sources)) {
    throw new Error(`Source file not found: ${args.sources}`);
  }

  let sourceRows = parseCsv(fs.readFileSync(args.sources, "utf8")).filter((row) => row.include !== "FALSE" && row.source_url);
  if (args.programFilter) {
    const pattern = new RegExp(args.programFilter, "i");
    sourceRows = sourceRows.filter((row) => pattern.test(row.program_name || "") || pattern.test(row.stats_team_name || ""));
  }
  if (args.sourceGroupFilter) {
    const pattern = new RegExp(args.sourceGroupFilter, "i");
    sourceRows = sourceRows.filter((row) => pattern.test(row.source_group || ""));
  }
  if (args.sourceKindFilter) {
    const pattern = new RegExp(args.sourceKindFilter, "i");
    sourceRows = sourceRows.filter((row) => pattern.test(row.source_kind || ""));
  }
  if (args.limit > 0) {
    sourceRows = sourceRows.slice(0, args.limit);
  }
  if (sourceRows.length === 0) {
    throw new Error(`No active source_url rows found in ${args.sources}`);
  }

  const { chromium } = requirePlaywright();
  const seasonDir = args.season.replace(/[^A-Za-z0-9._-]/g, "_");
  const rawDir = path.join(args.outDir, "sources", seasonDir);
  fs.mkdirSync(rawDir, { recursive: true });
  fs.mkdirSync(args.profileDir, { recursive: true });

  const context = await chromium.launchPersistentContext(args.profileDir, {
    headless: args.headless,
    viewport: { width: 1440, height: 1000 },
    userAgent:
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 " +
      "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36",
  });

  try {
    let page = context.pages()[0] || (await context.newPage());
    const manifestRows = [];
    const failedRows = [];
    for (const source of sourceRows) {
      try {
        if (page.isClosed()) page = await context.newPage();
        manifestRows.push(await scrapeSourcePage(page, args, source, rawDir));
        await page.waitForTimeout(args.delayMs);
      } catch (error) {
        failedRows.push({ ...source, error: error.message });
        console.log(`  source failed for ${source.program_name} (${source.source_kind}): ${error.message}`);
        if (!args.continueOnError) throw error;
        if (!page.isClosed()) {
          try {
            await page.close({ runBeforeUnload: false });
          } catch (_) {
            // A failing navigation can leave the page half-closed; a fresh page is enough.
          }
        }
        page = await context.newPage();
      }
    }

    const manifestPath = path.join(rawDir, "source_manifest.csv");
    const headers = ["file", "program_name", "stats_team_name", "source_group", "njcaa_region", "state", "source_kind", "source_url"];
    fs.writeFileSync(
      manifestPath,
      `${headers.join(",")}\n${manifestRows
        .map((row) =>
          headers
            .map((header) => csvEscape(header === "stats_team_name" ? row.stats_team_name : row[header]))
            .join(",")
        )
        .join("\n")}\n`,
      "utf8"
    );

    if (failedRows.length > 0) {
      const failedPath = path.join(rawDir, "failed_sources.csv");
      const failedHeaders = ["program_name", "stats_team_name", "source_kind", "source_url", "error"];
      fs.writeFileSync(
        failedPath,
        `${failedHeaders.join(",")}\n${failedRows
          .map((row) => failedHeaders.map((header) => csvEscape(row[header])).join(","))
          .join("\n")}\n`,
        "utf8"
      );
      console.log(`Logged ${failedRows.length} failed source(s) to ${failedPath}`);
    }

    return { rawDir, manifestPath, saved: manifestRows.map((row) => row.file) };
  } finally {
    await context.close();
  }
}

function parseSnapshots(args, rawDir, manifestPath = "") {
  const rArgs = [
    path.join("JucoStatsApp", "scripts", "scrape_njcaa_stats.R"),
    `--fixture-dir=${rawDir}`,
    `--registry=${args.registry}`,
    `--output=${args.output}`,
  ];
  if (manifestPath) rArgs.push(`--manifest=${manifestPath}`);

  console.log(`Parsing saved browser snapshots to ${args.output}`);
  const result = spawnSync("Rscript", rArgs, { stdio: "inherit" });
  if (result.status !== 0) {
    throw new Error(`R parser failed with exit code ${result.status}`);
  }
}

(async function main() {
  const args = parseArgs(process.argv.slice(2));
  const { rawDir, saved, manifestPath } = args.mode === "national" ? await scrapeAll(args) : await scrapeSources(args);
  console.log(`Saved ${saved.length} rendered HTML snapshots in ${rawDir}`);

  if (args.parse) {
    parseSnapshots(args, rawDir, manifestPath);
  }
})().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
