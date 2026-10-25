import "./style.css";
import { Elm } from "./Main.elm";
import { Audio, parseMetronomeCommand } from "./audio";

const STORAGE_KEY = "guitar-training";

const app = Elm.Main.init({
  flags: { stored: localStorage.getItem(STORAGE_KEY), now: Date.now() },
});

app.ports.saveState.subscribe((state) => {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
});

// Audio

const audio = new Audio();

// Browsers only start audio from a user gesture; unlock on the first touch.
document.addEventListener("pointerdown", () => audio.unlock(), { capture: true });

app.ports.metronome.subscribe((value) => {
  audio.apply(parseMetronomeCommand(value));
});

app.ports.chime.subscribe((volume) => {
  audio.chime(volume);
  navigator.vibrate?.([200, 100, 200]);
});

// Screen wake lock while a session is open

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
