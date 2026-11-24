# Go Pyramid in the browser

`index.html` is the whole game. Open it directly to play the computer (Easy,
Medium or Hard), or run the small server to play a friend online.

## Play a friend online

You need [Node.js](https://nodejs.org) (version 18 or newer). No other installs.

**1. Get a public link tool (once).** `cloudflared` gives you a free public link,
no account needed:

```bash
brew install cloudflared
```
(Windows: `winget install --id Cloudflare.cloudflared`; Linux: see
[Cloudflare's downloads page](https://developers.cloudflare.com/cloudflare-one/connections/connect-networks/downloads/).)

**2. Start the server** from the repository folder:

```bash
node web/server.mjs --tunnel
```

It prints something like:

```
  You:            http://localhost:8000
  Same Wi-Fi:     http://192.168.1.23:8000
  ===> Send this link to your friend:  https://some-words.trycloudflare.com
```

**3. Play.** Open `http://localhost:8000` yourself, type your name and click
**Play a friend online**. Send your friend the `trycloudflare.com` link; they
open it and click the same button. The waiting screen also shows the link to send.

Notes:

* Keep the terminal open while you play; Ctrl+C stops the server. You get a new
  public link each time you start it.
* The server deals the cards and keeps both hands and the middle on your computer;
  each browser only ever receives its own cards, so nobody can peek.
* Refreshing the page keeps your seat. **New game** offers a rematch (whoever went
  second moves first next time) or leaving to play the computer.
* Friend on the same Wi-Fi? You can skip `--tunnel` and send the "Same Wi-Fi" link.
* Prefer ngrok? Run `node web/server.mjs`, then `ngrok http 8000`, and send the
  https link ngrok prints.
* `--port 9000` changes the port.

## Computer opponents

* **Easy** places what fits and throws everything else away.
* **Medium** is `keepNextRow` plus middle swapping from `AI/Naive.hs`.
* **Hard** plays a Discounted CFR strategy trained offline; see [`cfr/README.md`](cfr/README.md).
