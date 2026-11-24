import { defineConfig } from "vite";
import elm from "vite-plugin-elm";
import { VitePWA } from "vite-plugin-pwa";

export default defineConfig(({ command }) => ({
  base: "/guitar-training/",
  plugins: [
    // No Elm debugger in dev (its badge covers the timers); --optimize
    // only for production builds.
    elm({ debug: false, optimize: command === "build" }),
    VitePWA({
      registerType: "autoUpdate",
      includeAssets: ["icon.svg"],
      // Fonts too, so the app looks the same offline.
      workbox: { globPatterns: ["**/*.{js,css,html,svg,png,woff2}"] },
      manifest: {
        name: "Гитара: тренировки",
        short_name: "Гитара",
        description: "Метроном, который помнит, что ты играешь",
        lang: "ru",
        display: "standalone",
        orientation: "portrait",
        background_color: "#1f1b18",
        theme_color: "#1f1b18",
        icons: [
          { src: "icon-192.png", sizes: "192x192", type: "image/png" },
          { src: "icon-512.png", sizes: "512x512", type: "image/png" },
          { src: "icon-maskable-512.png", sizes: "512x512", type: "image/png", purpose: "maskable" },
        ],
      },
    }),
  ],
}));
