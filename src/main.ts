import "@fontsource-variable/nunito";
import "./style.css";
import { Elm } from "./Main.elm";
import { Audio, parseMetronomeCommand } from "./audio";

// v2 lives under its own key; the v1 key is only ever read (migrated on
// first launch), so a failed migration can't destroy the old data.
const STORAGE_KEY = "guitar-training-v2";
const LEGACY_KEY = "guitar-training";

const app = Elm.Main.init({
  flags: {
    stored: localStorage.getItem(STORAGE_KEY),
    legacy: localStorage.getItem(LEGACY_KEY),
    now: Date.now(),
  },
});

app.ports.saveState.subscribe((state) => {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
});

// Theme: style.css owns the palette; the browser bar takes the top colour of
// the theme's gradient.

app.ports.theme.subscribe((name) => {
  document.documentElement.dataset.theme = name;
  const color = getComputedStyle(document.documentElement).getPropertyValue("--bg1").trim();
  document.querySelector('meta[name="theme-color"]')?.setAttribute("content", color);
});

// Audio

const audio = new Audio();

// Browsers only start audio from a user gesture; unlock on the first touch.
document.addEventListener("pointerdown", () => audio.unlock(), { capture: true });

app.ports.metronome.subscribe((value) => {
  audio.apply(parseMetronomeCommand(value));
});

// Beat pulse: the ring's arc steps a quarter turn per beat and flashes, and
// the subdivision dots light up one per click. Elm renders `.arc` and the
// dots but never styles them, so this is the only writer.
let lastBeat = -1;

audio.onTick = (beat, sub, subdivision) => {
  const dot = document.querySelectorAll<HTMLElement>(".ring .subs i")[sub];
  dot?.animate([{ opacity: 1, transform: "scale(1.5)" }, { opacity: 0.28, transform: "scale(1)" }], {
    duration: 180,
    easing: "ease-out",
  });
  if (sub !== 0) return;
  const arc = document.querySelector<HTMLElement>(".ring .arc");
  if (arc === null) return;
  // Always turn forward; on a restart jump back without animating.
  const restart = beat < lastBeat;
  lastBeat = beat;
  if (restart) arc.style.transition = "none";
  arc.style.transform = `rotate(${beat * 90 - 45}deg)`;
  if (restart) {
    void arc.offsetWidth;
    arc.style.transition = "";
  }
  arc.animate([{ opacity: 1, filter: "brightness(1.6)" }, { opacity: 0.55, filter: "brightness(1)" }], {
    duration: subdivision > 1 ? 160 : 220,
    easing: "ease-out",
  });
};

// Screen wake lock while playing

let wantWakeLock = false;
let sentinel: WakeLockSentinel | null = null;

async function syncWakeLock(): Promise<void> {
  if (!("wakeLock" in navigator)) return;
  if (wantWakeLock && sentinel === null && document.visibilityState === "visible") {
    try {
      sentinel = await navigator.wakeLock.request("screen");
      sentinel.addEventListener("release", () => {
        sentinel = null;
      });
    } catch (err) {
      console.warn("Wake lock unavailable", err);
    }
  } else if (!wantWakeLock && sentinel !== null) {
    await sentinel.release();
    sentinel = null;
  }
}

app.ports.wakeLock.subscribe((on) => {
  wantWakeLock = on;
  void syncWakeLock();
});

// The lock is dropped whenever the page is hidden; take it back on return.
document.addEventListener("visibilitychange", () => void syncWakeLock());

void navigator.storage?.persist?.();
