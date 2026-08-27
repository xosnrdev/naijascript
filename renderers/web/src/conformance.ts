/**
 * Headless conformance runner.
 *
 * For every `conformance/fixtures/<name>/golden.json` at the repo root it:
 *   1. builds the retained mount tree,
 *   2. applies each ok step's patch in order,
 *   3. asserts base/target revisions strictly increase and chain correctly,
 *   4. deep-compares the resulting tree to the fixture's `final_tree`.
 *
 * It prints a per-fixture PASS/FAIL summary and exits non-zero on any
 * failure. The deep comparison is exact: ids, kind/primitive, props,
 * handlers, text, and children order all matter.
 */

import { readdirSync, readFileSync, statSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import type { GoldenFixture } from "./protocol.js";
import { applyPatch, buildTreeFromMount } from "./tree.js";

const here = dirname(fileURLToPath(import.meta.url));
const packageRoot = dirname(here);
// Fixtures live at the repo root, two levels up from `renderers/web`.
const fixturesDir = resolve(packageRoot, "../../conformance/fixtures");

interface FixtureResult {
  name: string;
  ok: boolean;
  detail: string;
}

function listFixtures(): string[] {
  let entries: string[];
  try {
    entries = readdirSync(fixturesDir);
  } catch (err) {
    throw new Error(
      `unable to read fixtures dir ${fixturesDir}: ${(err as Error).message}`,
    );
  }
  return entries
    .filter((name) => {
      const full = join(fixturesDir, name);
      try {
        return statSync(full).isDirectory();
      } catch {
        return false;
      }
    })
    .sort();
}

function runFixture(name: string): FixtureResult {
  const goldenPath = join(fixturesDir, name, "golden.json");
  const fixture = JSON.parse(readFileSync(goldenPath, "utf8")) as GoldenFixture;

  const tree = buildTreeFromMount(fixture.mount);
  let lastRevision = fixture.mount.tree_revision;

  let stepIndex = 0;
  for (const step of fixture.steps) {
    if (step.status !== "ok" || step.patch === undefined) {
      stepIndex += 1;
      continue;
    }
    const patch = step.patch;

    // Revision discipline: chain and strict monotonicity.
    if (patch.base_revision !== lastRevision) {
      return {
        name,
        ok: false,
        detail: `step ${stepIndex}: base_revision ${patch.base_revision} does not chain from ${lastRevision}`,
      };
    }
    if (patch.target_revision <= patch.base_revision) {
      return {
        name,
        ok: false,
        detail: `step ${stepIndex}: target_revision ${patch.target_revision} not > base_revision ${patch.base_revision}`,
      };
    }

    try {
      applyPatch(tree, patch);
    } catch (err) {
      return {
        name,
        ok: false,
        detail: `step ${stepIndex}: patch application threw: ${(err as Error).message}`,
      };
    }
    lastRevision = patch.target_revision;
    stepIndex += 1;
  }

  const diff = firstDiff(tree.root, fixture.final_tree, "$");
  if (diff !== null) {
    return {
      name,
      ok: false,
      detail: `final tree mismatch at ${diff}`,
    };
  }

  return { name, ok: true, detail: `revision ${lastRevision}` };
}

/**
 * Structural comparison. Returns the JSON-ish path of the first mismatch,
 * or null when the two nodes are deep-equal. Object key order is ignored;
 * array (children) order is significant.
 */
function firstDiff(a: unknown, b: unknown, path: string): string | null {
  if (a === b) {
    return null;
  }
  if (typeof a !== typeof b) {
    return `${path} (type ${typeof a} vs ${typeof b})`;
  }
  if (a === null || b === null) {
    return `${path} (${JSON.stringify(a)} vs ${JSON.stringify(b)})`;
  }
  if (typeof a === "number" && typeof b === "number") {
    return a === b ? null : `${path} (${a} vs ${b})`;
  }
  if (typeof a !== "object") {
    return a === b ? null : `${path} (${JSON.stringify(a)} vs ${JSON.stringify(b)})`;
  }

  const aArr = Array.isArray(a);
  const bArr = Array.isArray(b);
  if (aArr !== bArr) {
    return `${path} (array vs object)`;
  }
  if (aArr && bArr) {
    if (a.length !== b.length) {
      return `${path}.length (${a.length} vs ${b.length})`;
    }
    for (let i = 0; i < a.length; i += 1) {
      const d = firstDiff(a[i], b[i], `${path}[${i}]`);
      if (d !== null) {
        return d;
      }
    }
    return null;
  }

  const aObj = a as Record<string, unknown>;
  const bObj = b as Record<string, unknown>;
  const keys = new Set<string>([...Object.keys(aObj), ...Object.keys(bObj)]);
  for (const key of keys) {
    const inA = key in aObj;
    const inB = key in bObj;
    if (inA !== inB) {
      return `${path}.${key} (present ${inA} vs ${inB})`;
    }
    const d = firstDiff(aObj[key], bObj[key], `${path}.${key}`);
    if (d !== null) {
      return d;
    }
  }
  return null;
}

function main(): void {
  const names = listFixtures();
  if (names.length === 0) {
    console.error(`No fixtures found under ${fixturesDir}`);
    process.exit(1);
  }

  const results: FixtureResult[] = [];
  for (const name of names) {
    try {
      results.push(runFixture(name));
    } catch (err) {
      results.push({
        name,
        ok: false,
        detail: `runner error: ${(err as Error).message}`,
      });
    }
  }

  console.log(`NaijaScript web renderer conformance`);
  console.log(`fixtures dir: ${fixturesDir}`);
  console.log("");
  let failures = 0;
  for (const r of results) {
    const tag = r.ok ? "PASS" : "FAIL";
    if (!r.ok) {
      failures += 1;
    }
    console.log(`  ${tag}  ${r.name}  (${r.detail})`);
  }
  console.log("");
  console.log(
    `${results.length - failures}/${results.length} fixtures passed`,
  );

  process.exit(failures === 0 ? 0 : 1);
}

main();
