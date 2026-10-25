# Guitar training

A personal practice-session manager PWA: exercises with a metronome, presets
with time targets, and practice stats. Elm app with thin strict-TypeScript
ports (Web Audio metronome, wake lock, localStorage).

Live: https://jeaciaz.github.io/guitar-training/ (on Android Chrome: menu → "Install app").

## How it works

- **Practice**: tap a preset (or "Quick session") and you're in a session.
  Switching exercise always starts paused; Play gives a one-bar count-in when
  the click is on. Only unpaused time counts.
- **Presets**: when an exercise reaches its target time you get a chime and
  vibration and "Next" glows; you advance when you're ready.
- **BPM**: an exercise's start BPM only changes when you edit it. During a
  session the top BPM you actually *played* (held for 10 s, so mis-taps don't
  count) is recorded; the end-of-session summary offers a one-tap "make N the
  start BPM".
- **Forgot to finish?** A session paused for 30+ minutes is saved and closed
  automatically.
- **Data** lives in the phone's localStorage. Settings → Export backup /
  Import file for backups.

## Adding exercises/presets with Claude

Settings → Paste JSON merges exercises and presets by name (case-insensitive)
and never touches history. Ask Claude to produce JSON in this shape and paste it:

```json
{
  "exercises": [
    { "name": "Sweeps Am", "bpm": 90, "subdivision": 3, "metronome": true, "notes": "5-string shape" }
  ],
  "presets": [
    { "name": "Daily", "items": [ { "exercise": "Sweeps Am", "minutes": 10 } ] }
  ]
}
```

- `subdivision`: clicks per beat — 1 quarters, 2 eighths, 3 triplets, 4 sixteenths.
- All exercise fields except `name` are optional. An existing exercise with
  the same name is updated with the fields you give.
- A preset with the same name is replaced. Preset items reference exercises
  by name (existing or in the same paste).
- Pasting a full backup (it has a `version` field) replaces everything.

## Development

```sh
npm install
npm run dev      # vite dev server
npm test         # elm-test
npm run build    # typecheck + production build
```

Pushing to `main` deploys to GitHub Pages via `.github/workflows/deploy.yml`.

- `src/Domain.elm`: session/time accounting and stats (pure, tested)
- `src/Store.elm`: persisted state, JSON codec, paste-import merge
- `src/Main.elm`: app, routing, views, ports
- `src/main.ts`, `src/audio.ts`: ports — storage, metronome, chime, wake lock
