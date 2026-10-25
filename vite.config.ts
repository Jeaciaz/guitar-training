import { defineConfig } from "vite";
import elm from "vite-plugin-elm";
import { VitePWA } from "vite-plugin-pwa";

export default defineConfig({
  base: "/guitar-training/",
  plugins: [
    elm({ optimize: true }),
    VitePWA({
      registerType: "autoUpdate",
      includeAssets: ["icon.svg"],
      manifest: {
        name: "Guitar training",
        short_name: "Guitar",
        description: "Practice session manager with metronome",
        display: "standalone",
        orientation: "portrait",
        background_color: "#111214",
        theme_color: "#111214",
        icons: [
          { src: "icon-192.png", sizes: "192x192", type: "image/png" },
          { src: "icon-512.png", sizes: "512x512", type: "image/png" },
          { src: "icon-maskable-512.png", sizes: "512x512", type: "image/png", purpose: "maskable" },
        ],
      },
    }),
  ],
});
