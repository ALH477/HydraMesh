// SPDX-License-Identifier: LGPL-3.0-only
import { createApp } from 'vue'
// Reuse the desktop client's redesigned shell verbatim. Its `@ipc` import is
// aliased to ./wasm-ipc.ts by vite.config.ts, so the same UI drives the WASM
// codec + WebSocket bridge here.
import App from '../../client/src/App.vue'
createApp(App).mount('#app')
