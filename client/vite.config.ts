// SPDX-License-Identifier: LGPL-3.0-only
import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import { fileURLToPath, URL } from 'node:url'

// Tauri expects a fixed dev port and no terminal clearing.
export default defineConfig({
  plugins: [vue()],
  clearScreen: false,
  server: { port: 1420, strictPort: true },
  // App.vue imports from '@ipc'; the desktop build points it at the Tauri bridge.
  resolve: { alias: { '@ipc': fileURLToPath(new URL('./src/ipc.ts', import.meta.url)) } },
})
