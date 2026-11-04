#!/usr/bin/env node

// Run with: node scripts/tests/test_home_countdown.cjs
// Exercise the production script rendered by R with a controlled browser clock.
const assert = require('node:assert/strict');
const { execFileSync } = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const vm = require('node:vm');

const root = path.resolve(__dirname, '../..');
const preview = fs.mkdtempSync(path.join(os.tmpdir(), 'base-countdown-test-'));
try {
  execFileSync('Rscript', ['scripts/tests/test_schedule.R'], {
    cwd: root,
    env: { ...process.env, BASE_SCHEDULE_PREVIEW_DIR: preview },
    stdio: 'pipe'
  });
  const html = fs.readFileSync(path.join(preview, 'opener.html'), 'utf8');
  const script = html.match(/<script>([\s\S]*?)<\/script>/)[1];
  const target = Date.parse('2027-02-19T18:00:00-06:00');
  assert.match(script, new RegExp(`var target=${target};`));

  const elements = new Map();
  const groups = new Map();
  for (const prefix of ['d', 'h', 'm', 's']) {
    const group = {
      children: [],
      appendChild(child) {
        child.parentElement = this;
        this.children.push(child);
      },
      get lastElementChild() { return this.children.at(-1); }
    };
    for (const digit of ['1', '2']) {
      const id = `cd-${prefix}${digit}`;
      assert.ok(html.includes(`id="${id}"`), `Missing countdown digit ${id}`);
      const element = makeElement();
      group.appendChild(element);
      elements.set(id, element);
    }
    groups.set(prefix, group);
  }
  function makeElement() {
    return {
      textContent: '0',
      remove() {
        const siblings = this.parentElement.children;
        siblings.splice(siblings.indexOf(this), 1);
      }
    };
  }
  let now = Date.parse('2026-11-04T12:00:00-06:00');
  let tick;
  const context = vm.createContext({
    document: {
      getElementById: id => elements.get(id),
      createElement: () => makeElement()
    },
    Date: { now: () => now },
    window: {},
    setInterval(callback) { tick = callback; return 1; },
    clearInterval() {}
  });
  vm.runInContext(script, context);
  const display = () => [...groups.values()].map(group =>
    group.children.map(child => child.textContent).join('')
  );
  assert.deepEqual(display(), ['107', '06', '00', '00']);
  now += 1000;
  tick();
  assert.deepEqual(display(), ['107', '05', '59', '59']);
  for (const [remaining, expected] of [
    [1000 * 86400000, ['1000', '00', '00', '00']],
    [100 * 86400000, ['100', '00', '00', '00']],
    [100 * 86400000 - 1000, ['99', '23', '59', '59']],
    [86400000, ['01', '00', '00', '00']],
    [1000, ['00', '00', '00', '01']],
    [0, ['00', '00', '00', '00']],
    [-1000, ['00', '00', '00', '00']]
  ]) {
    now = target - remaining;
    tick();
    assert.deepEqual(display(), expected);
  }
  console.log('Countdown passed: Oakland timestamp, 107/1000-day totals, ticking, 100-to-99 rollover, and expiry.');
} finally {
  fs.rmSync(preview, { recursive: true, force: true });
}
