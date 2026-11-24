#!/usr/bin/env node
// Go Pyramid online server: two players, one game, hidden hands.
//
//   node web/server.mjs                 # http://localhost:8000 (+ your Wi-Fi address)
//   node web/server.mjs --tunnel        # also a public https link via Cloudflare
//   node web/server.mjs --port 9000
//
// No dependencies. The server owns the deck, the middle and both hands, checks
// every move, and streams each player only their own view (Server-Sent Events).
// The first two browsers to click "Play online" get the two seats; a browser
// keeps its seat across refreshes. Restarting the server starts over.

import http from 'node:http';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import crypto from 'node:crypto';
import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const ROOT = path.dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const PORT = Number(args[args.indexOf('--port') + 1]) || Number(process.env.PORT) || 8000;
const TUNNEL = args.includes('--tunnel');
let publicUrl = null;   // set once the tunnel is up
const lanUrls = [];

// ------------------------------------------------------------------ game rules
const SUITS = ['♠', '♥', '♣', '♦'];
const SUPPORTS = { 5: [1, 2], 6: [2, 3], 7: [3, 4], 8: [5, 6], 9: [6, 7], 10: [8, 9] };
const RANK_NAMES = { 1: 'A', 11: 'J', 12: 'Q', 13: 'K' };
const cardName = c => (RANK_NAMES[c.rank] || String(c.rank)) + c.suit;
const canPlace = (rank, pyr) => rank <= 10 && !pyr[rank] && (SUPPORTS[rank] || []).every(r => pyr[r]);
const sortHand = h => h.sort((a, b) => a.rank - b.rank || SUITS.indexOf(a.suit) - SUITS.indexOf(b.suit));

function shuffle(a) {
  for (let i = a.length - 1; i > 0; i--) {
    const j = crypto.randomInt(i + 1);
    [a[i], a[j]] = [a[j], a[i]];
  }
  return a;
}

function newGame(first) {
  const deck = [];
  let id = 0;
  for (const suit of SUITS) for (let rank = 1; rank <= 13; rank++) deck.push({ id: id++, rank, suit });
  shuffle(deck);
  const middle = deck.splice(0, 5);
  const hands = [[], []];
  for (let i = 0; i < 5; i++) { hands[first].push(deck.shift()); hands[1 - first].push(deck.shift()); }
  return {
    deck, middle, discard: [], hands: hands.map(sortHand), pyr: [{}, {}],
    order: [first, 1 - first], turnIdx: 0, round: 1, over: false, winner: null, moveNo: 0, log: [],
  };
}

// ------------------------------------------------------------------ room
const room = {
  seats: [null, null],   // { token, name }
  game: null,
  games: 0,
  clients: new Set(),    // { res, seat }
};
const nameOf = s => (room.seats[s] && room.seats[s].name) || `Player ${s + 1}`;
const log = msg => { room.game.log.push(msg); if (room.game.log.length > 200) room.game.log.shift(); };

function startGame() {
  const first = room.games % 2;            // alternate who moves first
  room.games++;
  room.game = newGame(first);
  log(`Game ${room.games}: ${nameOf(first)} moves first.`);
}

function currentSeat(g) { return g.order[g.turnIdx]; }

function drawCard(g) {
  if (!g.deck.length) {
    if (!g.discard.length) return null;
    g.deck = shuffle(g.discard);
    g.discard = [];
    log('Deck empty: discard pile shuffled into a new deck.');
  }
  return g.deck.shift();
}

function afterMove(g) {
  g.moveNo++;
  if (g.turnIdx === 0) { g.turnIdx = 1; return; }
  for (const s of g.order) {
    while (g.hands[s].length < 5) {
      const c = drawCard(g);
      if (!c) break;
      g.hands[s].push(c);
    }
    sortHand(g.hands[s]);
  }
  const done = [0, 1].map(s => !!g.pyr[s][10]);
  if (done[0] || done[1]) {
    g.over = true;
    g.winner = done[0] && done[1] ? -1 : done[0] ? 0 : 1;
    log(g.winner < 0 ? "It's a draw!" : `${nameOf(g.winner)} wins!`);
    return;
  }
  g.round++;
  g.turnIdx = 0;
  log(`Round ${g.round}: hands refilled to 5.`);
}

// move = { type: 'swap' } or { type: 'play', placed: [ids in order], discarded: [ids] }
function applyMove(seat, move) {
  const g = room.game;
  if (!g || g.over) return 'The game is not running.';
  if (currentSeat(g) !== seat) return "It's not your turn.";
  const hand = g.hands[seat];
  if (move.type === 'swap') {
    g.hands[seat] = sortHand(g.middle);
    g.middle = hand;
    log(`${nameOf(seat)} swapped hand with the middle.`);
    afterMove(g);
    return null;
  }
  if (move.type !== 'play') return 'Unknown move.';
  const placed = Array.isArray(move.placed) ? move.placed : [];
  const discarded = Array.isArray(move.discarded) ? move.discarded : [];
  const ids = [...placed, ...discarded];
  if (new Set(ids).size !== ids.length) return 'A card was used twice.';
  const byId = new Map(hand.map(c => [c.id, c]));
  if (!ids.every(id => byId.has(id))) return "That card isn't in your hand.";
  const pyr = { ...g.pyr[seat] };
  for (const id of placed) {
    const c = byId.get(id);
    if (!canPlace(c.rank, pyr)) return `${cardName(c)} can't be placed there.`;
    pyr[c.rank] = c;
  }
  g.pyr[seat] = pyr;
  g.hands[seat] = hand.filter(c => !ids.includes(c.id));
  g.discard.push(...discarded.map(id => byId.get(id)));
  const parts = [];
  if (placed.length) parts.push('placed ' + placed.map(id => cardName(byId.get(id))).join(' '));
  if (discarded.length) parts.push('discarded ' + discarded.map(id => cardName(byId.get(id))).join(' '));
  log(`${nameOf(seat)} ${parts.length ? parts.join(', ') : 'passed'}.`);
  afterMove(g);
  return null;
}

// What one player is allowed to see.
function viewFor(seat) {
  const g = room.game;
  const opp = 1 - seat;
  const v = {
    seat, you: nameOf(seat), oppName: nameOf(opp),
    oppJoined: !!room.seats[opp],
    oppOnline: [...room.clients].some(c => c.seat === opp),
    started: !!g,
  };
  if (!g) return v;
  const list = pyr => Object.values(pyr);
  return Object.assign(v, {
    turnId: `${room.games}-${g.moveNo}`,
    game: room.games,
    current: currentSeat(g), round: g.round, over: g.over,
    result: g.over ? (g.winner < 0 ? 'draw' : g.winner === seat ? 'win' : 'loss') : null,
    hand: g.hands[seat],
    pyr: [list(g.pyr[0]), list(g.pyr[1])],
    oppHandCount: g.hands[opp].length,
    oppHand: g.over ? g.hands[opp] : null,
    middleCount: g.middle.length,
    deckCount: g.deck.length,
    discardCount: g.discard.length,
    discardTop: g.discard[g.discard.length - 1] || null,
    log: g.log.slice(-60),
  });
}

function broadcast() {
  for (const c of room.clients) c.res.write(`data: ${JSON.stringify(viewFor(c.seat))}\n\n`);
}

const seatOf = token => room.seats.findIndex(s => s && s.token === token);

// ------------------------------------------------------------------ http
function json(res, code, body) {
  res.writeHead(code, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' });
  res.end(JSON.stringify(body));
}

function readBody(req) {
  return new Promise(resolve => {
    let data = '';
    req.on('data', d => { data += d; if (data.length > 1e5) req.destroy(); });
    req.on('end', () => { try { resolve(JSON.parse(data || '{}')); } catch { resolve({}); } });
  });
}

const TYPES = { '.html': 'text/html; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.css': 'text/css', '.png': 'image/png', '.svg': 'image/svg+xml' };

const server = http.createServer(async (req, res) => {
  const url = new URL(req.url, 'http://x');
  const p = url.pathname;

  if (p === '/api/hello') return json(res, 200, { game: 'go-pyramid', publicUrl, lanUrl: lanUrls[0] || null, tunnel: TUNNEL });

  if (p === '/api/join' && req.method === 'POST') {
    const body = await readBody(req);
    const name = String(body.name || '').trim().slice(0, 20);
    let seat = seatOf(body.token);
    if (seat < 0) {
      seat = room.seats.indexOf(null);
      if (seat < 0) return json(res, 409, { error: 'This game already has two players.' });
      room.seats[seat] = { token: crypto.randomUUID(), name };
    } else if (name) {
      room.seats[seat].name = name;
    }
    if (!room.game && room.seats[0] && room.seats[1]) startGame();
    json(res, 200, { token: room.seats[seat].token, seat });
    broadcast();
    return;
  }

  if (p === '/api/events') {
    const seat = seatOf(url.searchParams.get('token'));
    if (seat < 0) return json(res, 403, { error: 'Unknown player.' });
    res.writeHead(200, { 'Content-Type': 'text/event-stream', 'Cache-Control': 'no-store', Connection: 'keep-alive', 'X-Accel-Buffering': 'no' });
    res.write('retry: 2000\n\n');
    const client = { res, seat };
    room.clients.add(client);
    const ping = setInterval(() => res.write(': ping\n\n'), 20000);
    req.on('close', () => { clearInterval(ping); room.clients.delete(client); broadcast(); });
    broadcast();
    return;
  }

  if (p === '/api/move' && req.method === 'POST') {
    const body = await readBody(req);
    const seat = seatOf(body.token);
    if (seat < 0) return json(res, 403, { error: 'Unknown player.' });
    const err = applyMove(seat, body.move || {});
    json(res, err ? 400 : 200, err ? { error: err } : { ok: true });
    if (!err) broadcast();
    return;
  }

  if (p === '/api/new' && req.method === 'POST') {
    const body = await readBody(req);
    const seat = seatOf(body.token);
    if (seat < 0) return json(res, 403, { error: 'Unknown player.' });
    if (!room.seats[0] || !room.seats[1]) return json(res, 400, { error: 'Waiting for the second player.' });
    if (room.game && !room.game.over && !body.force) return json(res, 400, { error: 'The game is still running.' });
    startGame();
    if (room.game.log.length) room.game.log.unshift(`${nameOf(seat)} started a new game.`);
    json(res, 200, { ok: true });
    broadcast();
    return;
  }

  if (p === '/api/leave' && req.method === 'POST') {
    const body = await readBody(req);
    const seat = seatOf(body.token);
    if (seat >= 0) {
      room.seats[seat] = null;
      room.game = null;
      for (const c of [...room.clients]) if (c.seat === seat) { c.res.end(); room.clients.delete(c); }
      broadcast();
    }
    return json(res, 200, { ok: true });
  }

  // static files from web/ (flat names only)
  const file = p === '/' ? 'index.html' : p.slice(1);
  if (!/^[\w.-]+$/.test(file) || file.startsWith('.')) { res.writeHead(404); return res.end('Not found'); }
  fs.readFile(path.join(ROOT, file), (err, data) => {
    if (err) { res.writeHead(404); return res.end('Not found'); }
    res.writeHead(200, { 'Content-Type': TYPES[path.extname(file)] || 'application/octet-stream', 'Cache-Control': 'no-cache' });
    res.end(data);
  });
});

server.listen(PORT, () => {
  console.log(`Go Pyramid server running.\n`);
  console.log(`  You:            http://localhost:${PORT}`);
  for (const addrs of Object.values(os.networkInterfaces())) {
    for (const a of addrs || []) {
      if (a.family === 'IPv4' && !a.internal) {
        lanUrls.push(`http://${a.address}:${PORT}/`);
        console.log(`  Same Wi-Fi:     http://${a.address}:${PORT}`);
      }
    }
  }
  if (TUNNEL) startTunnel();
  else console.log(`\n  For a friend on another network, restart with --tunnel.`);
  console.log(`\nBoth players open their link and click "Play a friend online". Ctrl+C to stop.\n`);
});

// Free Cloudflare quick tunnel: public https link, no account needed.
function startTunnel() {
  console.log(`\n  Starting a public link (Cloudflare quick tunnel)...`);
  let cf;
  try {
    cf = spawn('cloudflared', ['tunnel', '--no-autoupdate', '--url', `http://localhost:${PORT}`], { stdio: ['ignore', 'pipe', 'pipe'] });
  } catch (e) { return tunnelMissing(); }
  cf.on('error', tunnelMissing);
  let found = false;
  const scan = buf => {
    const m = !found && String(buf).match(/https:\/\/[a-z0-9-]+\.trycloudflare\.com/);
    if (m) {
      found = true;
      publicUrl = m[0] + '/';
      console.log(`\n  ===> Send this link to your friend:  ${m[0]}\n`);
    }
  };
  cf.stdout.on('data', scan);
  cf.stderr.on('data', scan);
  cf.on('exit', code => { if (!found) console.log(`  cloudflared exited (code ${code}) before giving a link.`); });
  const stop = () => { cf.kill(); process.exit(0); };
  process.on('SIGINT', stop);
  process.on('SIGTERM', stop);
}

function tunnelMissing() {
  console.log(`
  "cloudflared" isn't installed, so there's no public link yet. Install it, then run again with --tunnel:
    macOS:    brew install cloudflared
    Windows:  winget install --id Cloudflare.cloudflared
    Linux:    https://developers.cloudflare.com/cloudflare-one/connections/connect-networks/downloads/
  (Or use any other tunnel, e.g. "ngrok http ${PORT}", and send your friend the https link it prints.)`);
}
