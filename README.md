# Guitar training

A personal practice PWA (UI in Russian): a metronome that remembers what you
play. Elm app with thin strict-TypeScript ports (Web Audio metronome, beat
pulse, wake lock, theme, localStorage).

Live: https://jeaciaz.github.io/guitar-training/ (on Android Chrome: menu → "Install app").

## How it works

- **One screen.** The app opens on the metronome. Tap the ring to start or
  pause; with the click on, a one-bar count-in plays first. −5/−1/+1/+5
  change the tempo, the chip below cycles subdivisions, "клик выкл" mutes the
  click while time keeps running (for video or free play).
- **Exercises** are a flat list: name, start BPM, subdivision, optional
  YouTube link. Tap the exercise name → pick one, or type a new name to add it
  (its start BPM is the current tempo). ± in the list moves a start BPM by 5,
  ✎ edits. "Без упражнения" is free play. 🎲 suggests something you haven't
  played for a while, only when asked.
- **Start BPM** only changes when you change it. When the tempo on screen
  differs, "старт 88 · сделать 93" makes the current tempo the new start.
- **Time.** "на упражнении" is the time on the current exercise in this
  session; "всего" is the session. A session starts on the first tap and is
  closed after 30 minutes on pause. Only playing time counts; a killed app is
  paused at its last heartbeat (≤5 s).
- **Video.** A YouTube link is embedded above the metronome ("▶ видео"); it
  keeps playing while you change tempo or open other panels. Other links open
  in a new tab.
- **Stats**: today / 7 / 30 days, the last 14 days, and time and latest top
  BPM (held ≥10 s) per exercise over 30 days.
- **Themes**: ten (light: Песок, Бумага, Мята; warm dark: Лампа, Шоколад,
  Вино, Хвоя; cold dark: Графит, Ночь, Сталь). Settings pick one for the
  day and one for the evening; they follow the phone's light/dark mode.
- **Data** lives in the phone's localStorage under `guitar-training-v2`.
  Settings → Export / Import file for backups.

## Migration from v1 (presets)

On first launch v2 reads the old `guitar-training` key (never writes it): all
practice time carries over as "без упражнения", exercises and presets are
dropped. Importing a v1 backup file does the same to the history. If the v2
data ever fails to parse, the app shows a notice and stops saving instead of
overwriting it.

## Adding exercises with Claude

Settings → Paste JSON merges exercises by name (case-insensitive):

```json
{ "exercises": [
  { "name": "Avatar — Crying Fire", "bpm": 88, "video": "https://youtu.be/…" },
  { "name": "Fours (A minor)", "bpm": 80, "subdivision": 2 }
] }
```

`subdivision`: 1 quarters, 2 eighths, 3 triplets, 4 sixteenths. Everything
except `name` is optional. A full backup (with `"version": 2`) replaces
everything.

## Development

```sh
npm install
npm run dev      # vite dev server
npm test         # elm-test
npm run review   # elm-review (unused code)
npm run build    # typecheck + production build
npm run e2e      # build, then browser checks in headless Chrome (CHROME=… to override)
```

Pushing to `main` runs tests, review and build, then deploys to GitHub Pages
via `.github/workflows/deploy.yml`. `e2e` is local only; audio (click,
count-in, pulse sync) still needs a check on the phone.

- `src/Session.elm`: the deck and practice-time accounting; both the deck and the
  live session are opaque, so switching exercise always closes its time first
- `src/Store.elm`: persisted state and the operations that keep it consistent
  (switching exercise, start-BPM sync), JSON codec, v1 migration, paste import
- `src/Exercise.elm`, `src/Bpm.elm`, `src/Theme.elm`: small types (BPM is
  always 20–400; bad stored values are clamped or defaulted, never fatal)
- `src/Stats.elm`, `src/Format.elm`: aggregates and Russian display text
- `src/Main.elm`: pages, update, views, ports
- `src/main.ts`, `src/audio.ts`: ports — storage, metronome + beat/subdivision pulse, theme, wake lock

`tests/` covers the session maths, the store invariants, the v1 migration
(on a real export) and a pinned sample of the v2 storage format.
