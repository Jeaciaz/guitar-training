// Types for the compiled Elm module (vite-plugin-elm). Keep in sync with the
// ports and Flags in src/Main.elm.

interface ElmOutPort<T> {
  subscribe(callback: (value: T) => void): void;
}

interface ElmFlags {
  stored: string | null;
  now: number;
}

interface ElmApp {
  ports: {
    saveState: ElmOutPort<unknown>;
    metronome: ElmOutPort<unknown>;
    wakeLock: ElmOutPort<boolean>;
    chime: ElmOutPort<null>;
  };
}

declare module "*/Main.elm" {
  export const Elm: {
    Main: {
      init(options: { flags: ElmFlags }): ElmApp;
    };
  };
}
