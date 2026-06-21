// SPDX-License-Identifier: LGPL-3.0-only
// Web build: the same App.vue as the desktop client, with `@ipc` aliased to the
// WASM/WebSocket transport, bundled (with the base64-inlined wasm) into ONE
// self-contained index.html via vite-plugin-singlefile.
import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import { viteSingleFile } from 'vite-plugin-singlefile'
import { fileURLToPath, URL } from 'node:url'

export default defineConfig({
  plugins: [vue(), viteSingleFile()],
  resolve: {
    alias: {
      // App.vue imports from '@ipc'; the web build points it at the WASM transport.
      '@ipc': fileURLToPath(new URL('./src/wasm-ipc.ts', import.meta.url)),
    },
  },
  build: {
    target: 'es2020',
    assetsInlineLimit: 100000000, // inline everything
    chunkSizeWarningLimit: 100000,
  },
})
