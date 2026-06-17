"""DCF <-> Matrix application-service bridge.

Bridges a Matrix room to the DCF mesh so a phone running Element (over the VPN)
can chat with an LLM agent that lives on the mesh.

  Element (phone) --CS API--> Matrix homeserver --AS txn--> THIS bridge
        ^                                                      |
        |  m.room.message                                      v  dcf_text.packetize
        +---------- CS API send <-- DcfTextNode.on_message <-- UDP DeModFrame mesh
                                                               ^
                                            LLM agent (mesh_mcp.py) replies here

Matrix carries the human-readable chat; every message crosses the mesh as 17-byte
DeModFrame DATA frames (optionally SuperPacked).  The agent never talks Matrix —
it only speaks DeModFrame via the MCP server, so the wire quantum is the contract
between the chat world and the agent.

Run:  python3 bridge.py config.json
Requires: aiohttp  (pip install -r requirements.txt)
"""
import asyncio
import json
import sys
import time

from aiohttp import ClientSession, web

from dcf_node import DcfTextNode


class MatrixBridge:
    def __init__(self, cfg):
        self.hs_url = cfg["homeserver_url"].rstrip("/")
        self.as_token = cfg["as_token"]
        self.hs_token = cfg["hs_token"]
        self.bot_user = cfg["bot_user_id"]          # e.g. @dcf_bot:home.lan
        self.room_id = cfg["room_id"]
        self.channel = cfg.get("channel", "agent")
        self._seen_txns = set()
        self._loop = None
        self._http = None

        self.node = DcfTextNode(
            node_id=cfg.get("node_id", 0x00B1),
            bind_host=cfg.get("bind_host", "0.0.0.0"),
            port=cfg.get("udp_port", 7777),
            channel=self.channel,
            use_superpack=cfg.get("use_superpack", True),
        )
        for p in cfg.get("peers", []):              # the agent node(s)
            self.node.add_peer(p["name"], p["host"], p["port"])
        self.node.on_message = self._on_mesh_message

    # ── mesh -> Matrix ─────────────────────────────────────────────────────────
    def _on_mesh_message(self, src, dst, text, flags):
        # Called on the node's RX thread; hand off to the asyncio loop.
        if self._loop is None:
            return
        body = f"[mesh {src:#06x}] {text}"
        asyncio.run_coroutine_threadsafe(self._matrix_send(body), self._loop)

    async def _matrix_send(self, body):
        txn = f"dcf{int(time.time() * 1000)}"
        url = (f"{self.hs_url}/_matrix/client/v3/rooms/{self.room_id}"
               f"/send/m.room.message/{txn}")
        params = {"access_token": self.as_token, "user_id": self.bot_user}
        payload = {"msgtype": "m.text", "body": body}
        try:
            async with self._http.put(url, params=params, json=payload) as r:
                if r.status >= 300:
                    print("matrix send failed", r.status, await r.text())
        except Exception as e:
            print("matrix send error:", e)

    # ── Matrix -> mesh (application-service transaction endpoint) ──────────────
    async def _on_transaction(self, request):
        if request.query.get("access_token") != self.hs_token:
            return web.json_response({"errcode": "M_FORBIDDEN"}, status=403)
        txn_id = request.match_info["txn_id"]
        if txn_id in self._seen_txns:
            return web.json_response({})            # idempotent replay
        self._seen_txns.add(txn_id)

        data = await request.json()
        for ev in data.get("events", []):
            if ev.get("type") != "m.room.message":
                continue
            if ev.get("sender") == self.bot_user:    # don't echo our own posts
                continue
            content = ev.get("content", {})
            if content.get("msgtype") != "m.text":
                continue
            body = content.get("body", "")
            if not body:
                continue
            # A Matrix event can exceed the 4092-byte mesh cap; chunk if needed.
            for chunk in _chunks(body, 4092):
                self.node.send_text(chunk, flags=0x01)   # FLAG_AGENT
        return web.json_response({})

    async def _on_query_user(self, request):
        if request.query.get("access_token") != self.hs_token:
            return web.json_response({"errcode": "M_FORBIDDEN"}, status=403)
        return web.json_response({})

    async def _on_query_room(self, request):
        if request.query.get("access_token") != self.hs_token:
            return web.json_response({"errcode": "M_FORBIDDEN"}, status=403)
        return web.json_response({})

    def build_app(self):
        app = web.Application()
        # Support both the modern (/_matrix/app/v1/...) and legacy AS routes.
        app.router.add_put("/_matrix/app/v1/transactions/{txn_id}", self._on_transaction)
        app.router.add_put("/transactions/{txn_id}", self._on_transaction)
        app.router.add_get("/_matrix/app/v1/users/{user_id}", self._on_query_user)
        app.router.add_get("/users/{user_id}", self._on_query_user)
        app.router.add_get("/_matrix/app/v1/rooms/{room_alias}", self._on_query_room)
        app.router.add_get("/rooms/{room_alias}", self._on_query_room)
        return app

    async def run(self, listen_host, listen_port):
        self._loop = asyncio.get_running_loop()
        self._http = ClientSession()
        self.node.start()
        runner = web.AppRunner(self.build_app())
        await runner.setup()
        site = web.TCPSite(runner, listen_host, listen_port)
        await site.start()
        print(f"DCF<->Matrix bridge up: AS on {listen_host}:{listen_port}, "
              f"mesh UDP :{self.node.port} channel={self.channel}")
        try:
            while True:
                await asyncio.sleep(3600)
        finally:
            await self._http.close()
            self.node.stop()


def _chunks(s, n):
    """Split on UTF-8 byte budget n so each piece fits one mesh message."""
    n = 4092 if not isinstance(n, int) else n
    b = s.encode("utf-8")
    if len(b) <= n:
        return [s]
    out, i = [], 0
    while i < len(b):
        piece = b[i:i + n]
        # back off to a valid UTF-8 boundary
        while piece and (piece[-1] & 0xC0) == 0x80:
            piece = piece[:-1]
        out.append(piece.decode("utf-8", "ignore"))
        i += len(piece)
    return out


def main():
    if len(sys.argv) < 2:
        print("usage: python3 bridge.py config.json")
        sys.exit(2)
    with open(sys.argv[1]) as f:
        cfg = json.load(f)
    bridge = MatrixBridge(cfg)
    asyncio.run(bridge.run(cfg.get("listen_host", "0.0.0.0"),
                           cfg.get("listen_port", 9000)))


if __name__ == "__main__":
    main()
