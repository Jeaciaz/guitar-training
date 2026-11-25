// Browser checks for what elm-test can't see: the real build, ports,
// localStorage, the embedded video surviving re-renders, navigation.
//
//   npm run e2e            (builds first)
//   CHROME=/path/to/chrome npm run e2e
//
// Serves dist/ with `vite preview`, drives headless Chrome over the DevTools
// protocol at a phone-sized viewport, and exits non-zero on any failed check.
// Audio itself (click, count-in sound, pulse sync) still needs a real phone.

import { spawn } from "node:child_process";
import { copyFileSync, existsSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const PORT = 4317;
const CDP_PORT = 9333;
const BASE = `http://localhost:${PORT}/guitar-training/`;

const chromePath =
  process.env.CHROME ??
  ["/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "/usr/bin/google-chrome", "/usr/bin/chromium"].find(
    existsSync,
  );
if (!chromePath) throw new Error("Chrome not found; set CHROME=/path/to/chrome");

for (const file of ["seed.html", "v1-export.json"]) copyFileSync(join(root, "e2e", file), join(root, "dist", file));

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const profile = mkdtempSync(join(tmpdir(), "guitar-e2e-"));
const server = spawn("npx", ["vite", "preview", "--port", String(PORT), "--strictPort"], { cwd: root, stdio: "ignore" });
const chrome = spawn(
  chromePath,
  ["--headless=new", "--disable-gpu", `--remote-debugging-port=${CDP_PORT}`, `--user-data-dir=${profile}`, "--autoplay-policy=no-user-gesture-required", "about:blank"],
  { stdio: "ignore" },
);

const failures = [];
try {
  await waitUntil(async () => (await fetch(BASE)).ok);
  const target = await waitUntil(async () =>
    (await (await fetch(`http://127.0.0.1:${CDP_PORT}/json`)).json()).find((t) => t.type === "page"),
  );
  const cdp = await connect(target.webSocketDebuggerUrl);
  await cdp.send("Page.enable");
  await cdp.send("Runtime.enable");
  await cdp.send("Network.enable");
  // The service worker would answer seed.html with the app shell.
  await cdp.send("Network.setBypassServiceWorker", { bypass: true });
  await cdp.send("Emulation.setDeviceMetricsOverride", { width: 390, height: 844, deviceScaleFactor: 2, mobile: true });

  const seed = async (query) => {
    await cdp.send("Page.navigate", { url: `${BASE}seed.html?${query}` });
    await sleep(2500);
  };
  const run = (name, body) => cdp.scenario(name, body);
  const expect = (scenario, key, actual, expected) => {
    const ok = typeof expected === "function" ? expected(actual) : actual === expected;
    if (!ok) failures.push(`${scenario}: ${key} = ${JSON.stringify(actual)}`);
  };

  await seed("theme=lamp&live=0");
  let r = await run("play, tempo, video", `
    q('.pill-side').click();
    await waitFor(() => q('.video iframe'));
    const iframe = q('.video iframe');
    q('.ring').click();
    await waitFor(() => q('.ring').classList.contains('on'));
    out.countIn = q('.hint').textContent;
    byText('.key', '+5').click();
    await waitFor(() => q('.bpm').textContent === '98');
    out.iframeKeptAfterBump = q('.video iframe') === iframe;
    byText('.chip', 'сделать').click();
    await waitFor(() => q('.chips').textContent.includes('старт 98'));
    out.chips = q('.chips').textContent;
    q('.pill-main').click();
    await waitFor(() => q('.search input'));
    out.iframeKeptUnderSheet = q('.video iframe') === iframe;
  `);
  expect("play", "countIn", r.countIn, (s) => s.startsWith("отсчёт"));
  expect("play", "iframeKeptAfterBump", r.iframeKeptAfterBump, true);
  expect("play", "chips", r.chips, (s) => !s.includes("сделать"));
  expect("play", "iframeKeptUnderSheet", r.iframeKeptUnderSheet, true);

  r = await run("add and pick", `
    const input = q('.search input');
    type(input, 'Новый лик');
    await waitFor(() => q('.add'));
    q('.add').click();
    await waitFor(() => !q('.sheet') && q('.pill-name').textContent === 'Новый лик');
    out.bpm = q('.bpm').textContent;
    out.videoClosed = !q('.video iframe');
    out.exercises = store().exercises.map(e => e.name + '@' + e.startBpm);
    q('.pill-main').click();
    await waitFor(() => q('.search input'));
    type(q('.search input'), 'sla'); await sleep(200); submit(q('.search')); await sleep(500);
    out.partialKeepsSheet = !!q('.sheet');
    type(q('.search input'), "SLASH'S SEQUENCE"); await sleep(200); submit(q('.search'));
    await waitFor(() => !q('.sheet') && q('.pill-name').textContent === "Slash's sequence");
    out.count = store().exercises.length;
  `);
  expect("add", "bpm", r.bpm, "98");
  expect("add", "videoClosed", r.videoClosed, true);
  expect("add", "exercises", r.exercises, (xs) => xs?.includes("Новый лик@98") && xs.includes("Avatar — Crying Fire@98"));
  expect("add", "partialKeepsSheet", r.partialKeepsSheet, true);
  expect("add", "count (no junk from Enter)", r.count, 6);

  r = await run("free play, stats, back", `
    q('.pill-main').click();
    await waitFor(() => byText('.row-main', 'Без упражнения'));
    byText('.row-main', 'Без упражнения').click();
    await waitFor(() => !q('.sheet') && q('.pill-name').textContent === 'Без упражнения');
    byText('.chip', 'клик').click();
    await waitFor(() => byText('.chip', 'клик выкл'));
    q('.ring').click();
    await waitFor(() => q('.ring').classList.contains('silent'));
    await sleep(2200);
    out.timer = q('.timers b').textContent;
    q('.ring').click();
    await waitFor(() => q('.ring').classList.contains('off'));
    q('a[href="#/stats"]').click();
    await waitFor(() => q('.tiles'));
    history.back();
    await waitFor(() => !q('.sheet'));
    q('.pill-main').click();
    await waitFor(() => q('.search input'));
    byText('button', 'подсказать').click();
    await waitFor(() => q('.suggestion .row-name'));
  `);
  expect("free", "timer", r.timer, (s) => /^0:0[1-9]$/.test(s));

  await seed("theme=sand&run=1");
  r = await run("reload", `
    out.ring = q('.ring').className;
    out.timers = q('.timers').textContent;
    q('a[href="#/stats"]').click();
    await waitFor(() => q('.tiles'));
    out.today = q('.tile b').textContent;
  `);
  expect("reload", "ring paused", r.ring, "ring off");
  expect("reload", "timers", r.timers, (s) => s?.startsWith("4:12") && s.includes("7:12"));
  expect("reload", "today", r.today, "7 мин");

  await seed("mode=v1&route=stats");
  r = await run("migration", `
    await waitFor(() => q('.tiles'));
    const s = store();
    out.totalMs = s.sessions.flatMap(x => x.segments).reduce((a, x) => a + x.durationMs, 0);
    out.legacyKept = !!localStorage.getItem('guitar-training');
  `);
  expect("migration", "totalMs", r.totalMs, 5563202);
  expect("migration", "legacyKept", r.legacyKept, true);

  const colorScheme = (value) =>
    cdp.send("Emulation.setEmulatedMedia", { features: [{ name: "prefers-color-scheme", value }] });
  await colorScheme("light");
  await seed("theme=forest&live=0");
  r = await run("themes", `
    const shown = () => document.documentElement.dataset.theme;
    out.start = shown();
    q('a[href="#/settings"]').click();
    await waitFor(() => q('.themes'));
    out.swatches = document.querySelectorAll('.swatch').length;
    byText('.swatch', 'Бумага').click();
    await waitFor(() => shown() === 'paper');
    byText('.segmented button', 'Вечером').click();
    await waitFor(() => shown() === 'forest');
    byText('.swatch', 'Ночь').click();
    await waitFor(() => shown() === 'night');
    history.back();
    await waitFor(() => !q('.sheet'));
    out.closedInLightMode = shown();
    out.saved = [store().settings.dayTheme, store().settings.nightTheme].join('/');
  `);
  expect("themes", "old single theme for both", r.start, "forest");
  expect("themes", "swatches", r.swatches, 10);
  expect("themes", "light mode shows day theme", r.closedInLightMode, "paper");
  expect("themes", "saved", r.saved, "paper/night");
  await colorScheme("dark");
  await sleep(500);
  expect("themes", "dark mode shows evening theme", await cdp.evaluate("document.documentElement.dataset.theme"), "night");

  await cdp.send("Page.navigate", { url: BASE });
  await sleep(1500);
  await cdp.evaluate(`localStorage.setItem('guitar-training-v2', '{"version":2,"broken":true}')`);
  await cdp.send("Page.reload");
  await sleep(2000);
  r = await run("unreadable data", `
    out.notice = !!q('.notice');
    q('.ring').click(); await sleep(400); q('.ring').click(); await sleep(300);
    q('a[href="#/stats"]').click(); await waitFor(() => q('.tiles'));
    history.back(); await waitFor(() => !q('.sheet'));
    out.noticeStays = !!q('.notice');
    out.untouched = localStorage.getItem('guitar-training-v2') === '{"version":2,"broken":true}';
  `);
  expect("unreadable", "notice", r.notice, true);
  expect("unreadable", "noticeStays", r.noticeStays, true);
  expect("unreadable", "untouched", r.untouched, true);

  cdp.close();
} catch (err) {
  failures.push(String(err?.stack ?? err));
} finally {
  server.kill();
  // Chrome keeps writing its profile for a moment after the kill signal.
  const exited = new Promise((resolve) => chrome.once("exit", resolve));
  chrome.kill();
  await Promise.race([exited, sleep(5000)]);
  try {
    rmSync(profile, { recursive: true, force: true, maxRetries: 5, retryDelay: 200 });
  } catch {
    console.warn(`e2e: could not remove ${profile}`);
  }
}

if (failures.length > 0) {
  console.error(`e2e: ${failures.length} failed\n  ${failures.join("\n  ")}`);
  process.exit(1);
}
console.log("e2e: all checks passed");

// --- helpers ---

async function waitUntil(probe, ms = 15000) {
  const start = Date.now();
  while (Date.now() - start < ms) {
    try {
      const value = await probe();
      if (value) return value;
    } catch {}
    await sleep(200);
  }
  throw new Error("timed out waiting for the server or Chrome");
}

async function connect(url) {
  const ws = new WebSocket(url);
  await new Promise((resolve, reject) => {
    ws.onopen = resolve;
    ws.onerror = reject;
  });
  let id = 0;
  const pending = new Map();
  ws.onmessage = (m) => {
    const msg = JSON.parse(m.data);
    pending.get(msg.id)?.(msg);
    pending.delete(msg.id);
  };
  const send = (method, params = {}) =>
    new Promise((resolve) => {
      id += 1;
      pending.set(id, resolve);
      ws.send(JSON.stringify({ id, method, params }));
    });
  const evaluate = async (expression) => {
    const r = await send("Runtime.evaluate", { expression, awaitPromise: true, returnByValue: true });
    if (r.result.exceptionDetails) throw new Error(r.result.exceptionDetails.exception?.description);
    return r.result.result.value;
  };
  // Runs `body` in the page with small DOM helpers; `out` is returned.
  // A step that times out fails the scenario with its source line.
  const scenario = async (name, body) => {
    const result = await evaluate(`(async () => {
      const out = {};
      const sleep = ms => new Promise(r => setTimeout(r, ms));
      const q = s => document.querySelector(s);
      const byText = (sel, t) => [...document.querySelectorAll(sel)].find(e => e.textContent.includes(t));
      const store = () => JSON.parse(localStorage.getItem('guitar-training-v2'));
      const type = (input, value) => { input.value = value; input.dispatchEvent(new Event('input', { bubbles: true })); };
      const submit = form => form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
      const waitFor = async (cond, ms = 5000) => {
        const t = Date.now();
        while (Date.now() - t < ms) { try { if (cond()) return; } catch {} await sleep(50); }
        throw new Error('timed out: ' + cond.toString());
      };
      try { ${body} } catch (e) { out.error = String(e.message || e); }
      return out;
    })()`);
    if (result.error) failures.push(`${name}: ${result.error}`);
    return result;
  };
  return { send, evaluate, scenario, close: () => ws.close() };
}
