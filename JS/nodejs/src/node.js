// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// dcfnode (Node.js) — a real UDP DeModFrame mesh node, dependency-free (stdlib
// `dgram`). It speaks the SAME dialect as the Python DcfTextNode
// (matrix-bridge/dcf_node.py): bare 17-byte DeModFrame text frames, batched into
// 32-byte SuperPacks, with a frequency-channel rendezvous on the frame dst. So a
// Node.js node meshes with the Python node.
//
//   node src/node.js recv --follow [--channel duet] [--port 7801]
//   node src/node.js send "msg" --channel duet --peers host:port,host:port
//
// Lower latency by default: consecutive frames are paired into one SuperPack
// datagram (one packet instead of two) — toggle with --no-superpack.

const dgram = require('dgram');
const sp = require('./superpack.js');
const text = require('./text.js');

class DcfTextNode {
  constructor({ nodeId = 1, port = 7801, bind = '0.0.0.0', channel = null, useSuperpack = true, acceptAll = false } = {}) {
    this.nodeId = nodeId & 0xFFFF;
    this.port = port;
    this.bind = bind;
    this.channel = text.channelId(channel);
    this.useSuperpack = useSuperpack;
    this.acceptAll = acceptAll;
    this.peers = new Map();
    this.packetId = 0;
    this.sock = null;
    this.rx = new text.TextReassembler(acceptAll ? null : this.channel);
  }

  addPeer(id, host, port) {
    this.peers.set(id, [host, port]);
  }

  nextPacketId() {
    const pid = this.packetId;
    this.packetId = (this.packetId + 1) % (text.MAX_PACKET_ID + 1);
    return pid;
  }

  // Pair consecutive frames into 32-byte SuperPacks; a lone trailing frame goes raw.
  batch(frames) {
    if (!this.useSuperpack) return frames;
    const out = [];
    let i = 0;
    for (; i + 1 < frames.length; i += 2) out.push(sp.pack(frames[i], frames[i + 1]));
    if (i < frames.length) out.push(frames[i]);
    return out;
  }

  sendText(msg, { channel = null, flags = 0, to = null } = {}) {
    const dst = channel === null ? this.channel : text.channelId(channel);
    const pid = this.nextPacketId();
    const tsUs = Number(BigInt(Date.now()) * 1000n & 0xFFFFFFn);
    const frames = text.packetize(msg, pid, tsUs, this.nodeId, dst, flags);
    const datagrams = this.batch(frames);
    const targets = to ? [this.peers.get(to)] : [...this.peers.values()];
    for (const [host, port] of targets) {
      for (const dg of datagrams) this.sock.send(dg, port, host);
    }
    return datagrams.length * Math.max(1, targets.length);
  }

  feed(datagram, onMessage) {
    let frames;
    if (sp.isSuperpack(datagram)) {
      try {
        frames = sp.unpack(datagram);
      } catch (e) {
        return;
      }
    } else {
      frames = [datagram];
    }
    for (const f of frames) {
      for (const m of this.rx.push(f)) onMessage(m);
    }
  }

  start(onMessage) {
    return new Promise((resolve, reject) => {
      this.sock = dgram.createSocket('udp4');
      this.sock.on('message', (msg, rinfo) => this.feed(msg, (m) => onMessage(m, rinfo)));
      this.sock.on('error', reject);
      this.sock.bind(this.port, this.bind, () => {
        this.port = this.sock.address().port;
        resolve();
      });
    });
  }

  stop() {
    if (this.sock) this.sock.close();
  }
}

// ── CLI ───────────────────────────────────────────────────────────────────────
function parseFlags(argv) {
  const o = { _: [] };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === '--follow' || a === '--no-superpack') o[a.slice(2)] = true;
    else if (a.startsWith('--')) o[a.slice(2)] = argv[++i];
    else o._.push(a);
  }
  return o;
}

function parsePeers(spec) {
  return (spec || '').split(',').filter(Boolean).map((p) => {
    const i = p.lastIndexOf(':');
    return [p.slice(0, i), parseInt(p.slice(i + 1), 10)];
  });
}

async function main() {
  const [cmd, ...rest] = process.argv.slice(2);
  const o = parseFlags(rest);
  if (cmd === undefined || cmd === 'version' || cmd === '--version' || cmd === '-v' || cmd === 'help' || cmd === '--help') {
    console.log('dcfnode(js) 0.3.0 — DCF Node.js node (17-byte DeModFrame + SuperPack, UDP, stdlib dgram)');
    console.log('usage: dcf-node-js <recv|send> ...  (recv --follow | send "msg" --peers host:port)');
    process.exit(0);
  }
  if (cmd === 'recv') {
    const channel = o.channel || process.env.DCF_CHANNEL || 'duet';
    const port = parseInt(o.port || '7801', 10);
    const node = new DcfTextNode({ port, channel, acceptAll: false });
    await node.start((m, rinfo) => {
      console.log(`  ${('0x' + m.src.toString(16).padStart(4, '0'))} @ ${rinfo.address}:${rinfo.port}  ${m.text}`);
      if (!o.follow) {
        node.stop();
        process.exit(0);
      }
    });
    console.error(`dcfnode(js) listening on :${port} channel ${channel}${o.follow ? ' (follow)' : ''}`);
  } else if (cmd === 'send') {
    const msg = o._[0];
    if (!msg) {
      console.error('usage: node node.js send "text" --peers host:port[,host:port] [--channel duet]');
      process.exit(2);
    }
    const channel = o.channel || process.env.DCF_CHANNEL || 'duet';
    const peers = parsePeers(o.peers || process.env.DCF_PEERS || '');
    if (peers.length === 0) {
      console.error('--peers host:port is required');
      process.exit(2);
    }
    const node = new DcfTextNode({ nodeId: 0x00b1, port: 0, channel, useSuperpack: !o['no-superpack'] });
    await node.start(() => {});
    peers.forEach(([h, p], i) => node.addPeer(`peer${i}`, h, p));
    const n = node.sendText(msg, { channel });
    console.log(`sent ${n} datagram(s) on channel ${channel} to ${peers.map(([h, p]) => `${h}:${p}`).join(', ')}`);
    setTimeout(() => { node.stop(); process.exit(0); }, 300);
  } else {
    console.error('usage: node node.js <recv|send> ...');
    process.exit(2);
  }
}

if (require.main === module) main();

module.exports = { DcfTextNode };
