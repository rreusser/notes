import { observable, config } from "@observablehq/notebook-kit/vite";
import { defineConfig } from "vite";
import { resolve } from "path";

export default defineConfig(async ({ command }) => {
  const isDev = command === "serve";

  const { debugNotebook } = isDev
    ? await import("@rreusser/mcp-observable-notebook-kit-debug").catch(
        () => ({})
      )
    : {};

  return {
    ...config(),
    plugins: [isDev && debugNotebook?.(), observable()],
    root: "notebooks",
    build: {
      outDir: resolve(import.meta.dirname, "dist"),
      emptyOutDir: true,
      rollupOptions: {
        input: [resolve(import.meta.dirname, "notebooks/aaa/index.html")],
      },
    },
    server: {
      host: true,
    },
  };
});
