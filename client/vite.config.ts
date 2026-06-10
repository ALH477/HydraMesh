// SPDX-License-Identifier: LGPL-3.0-only
import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

// Tauri expects a fixed dev port and no terminal clearing.
export default defineConfig({
  plugins: [vue()],
  clearScreen: false,
  server: { port: 1420, strictPort: true },
})
