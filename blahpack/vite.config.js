import { observable, config } from "@observablehq/notebook-kit/vite";
import { defineConfig } from "vite";

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
    server: {
      host: true,
    },
  };
});
