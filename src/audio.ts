// Web Audio metronome with a lookahead scheduler: a coarse JS timer schedules
// clicks slightly ahead on the sample-accurate audio clock.

export type MetronomeCommand =
  | { running: false }
  | { running: true; bpm: number; subdivision: number; volume: number };

type Running = Extract<MetronomeCommand, { running: true }>;

const LOOKAHEAD_S = 0.12;
const TIMER_MS = 25;

export function parseMetronomeCommand(value: unknown): MetronomeCommand {
  if (typeof value !== "object" || value === null || !("running" in value)) {
    throw new Error(`Bad metronome command: ${JSON.stringify(value)}`);
  }
  if (value.running === false) return { running: false };
  if (
    "bpm" in value && typeof value.bpm === "number" && value.bpm > 0 &&
    "subdivision" in value && typeof value.subdivision === "number" && value.subdivision >= 1 &&
    "volume" in value && typeof value.volume === "number"
  ) {
    return { running: true, bpm: value.bpm, subdivision: value.subdivision, volume: value.volume };
  }
  throw new Error(`Bad metronome command: ${JSON.stringify(value)}`);
}

export class Audio {
  private ctx: AudioContext | null = null;
  private timer: ReturnType<typeof setInterval> | null = null;
  private config: Running | null = null;
  private nextTime = 0;
  private tick = 0;

  /** Create/resume the context; must run inside a user gesture at least once. */
  unlock(): AudioContext {
    this.ctx ??= new AudioContext({ latencyHint: "interactive" });
    if (this.ctx.state === "suspended") void this.ctx.resume();
    return this.ctx;
  }

  apply(cmd: MetronomeCommand): void {
    if (!cmd.running) {
      this.config = null;
      if (this.timer !== null) clearInterval(this.timer);
      this.timer = null;
      return;
    }
    const previous = this.config;
    this.config = cmd;
    if (previous === null) {
      const ctx = this.unlock();
      this.nextTime = ctx.currentTime + 0.05;
      this.tick = 0;
      this.timer = setInterval(() => this.schedule(), TIMER_MS);
      this.schedule();
    } else if (previous.subdivision !== cmd.subdivision) {
      // Realign so the next click after the current beat is a downbeat.
      this.tick = 0;
    }
  }

  private schedule(): void {
    const ctx = this.ctx;
    const cfg = this.config;
    if (ctx === null || cfg === null) return;
    // If we fell behind (tab throttled), skip ahead instead of bursting clicks.
    if (this.nextTime < ctx.currentTime) this.nextTime = ctx.currentTime + 0.02;
    const interval = 60 / cfg.bpm / cfg.subdivision;
    while (this.nextTime < ctx.currentTime + LOOKAHEAD_S) {
      const onBeat = this.tick % cfg.subdivision === 0;
      this.click(ctx, this.nextTime, onBeat ? cfg.volume : cfg.volume * 0.4, onBeat ? 1800 : 1200);
      this.nextTime += interval;
      this.tick += 1;
    }
  }

  private click(ctx: AudioContext, at: number, volume: number, freq: number): void {
    if (volume <= 0) return;
    const osc = ctx.createOscillator();
    const gain = ctx.createGain();
    osc.type = "square";
    osc.frequency.value = freq;
    gain.gain.setValueAtTime(0, at);
    gain.gain.linearRampToValueAtTime(volume * 0.5, at + 0.001);
    gain.gain.exponentialRampToValueAtTime(0.0001, at + 0.035);
    osc.connect(gain).connect(ctx.destination);
    osc.start(at);
    osc.stop(at + 0.04);
  }

  /** Soft two-note chime for "time's up" on a preset item. */
  chime(volume: number): void {
    const ctx = this.unlock();
    const start = ctx.currentTime + 0.02;
    [880, 1318.5].forEach((freq, i) => {
      const at = start + i * 0.18;
      const osc = ctx.createOscillator();
      const gain = ctx.createGain();
      osc.type = "sine";
      osc.frequency.value = freq;
      gain.gain.setValueAtTime(0, at);
      gain.gain.linearRampToValueAtTime(Math.max(0.15, volume) * 0.6, at + 0.01);
      gain.gain.exponentialRampToValueAtTime(0.0001, at + 0.6);
      osc.connect(gain).connect(ctx.destination);
      osc.start(at);
      osc.stop(at + 0.65);
    });
  }
}
