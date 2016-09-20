# PdfServer

This is still work in progress. Basic functionality is here and I do run this in
productiuon for a while but do expect quirks and specific leftover from my the
project I use this for until 1.0.

Still I find this thingy useful enough to publish. 

## Aim

This package is here to prove a ready-to-deploy PDF server. It exposes an API that lets you

- feed HTML and a few options in
- _MISSING:_ send an URL as parameter where to retrieve HTML
  - whitlisting
  - 
  
and ultimately answers with either a PDF ready to download or a Link where to donwload it.

# Installation

Assuming a bash shell

```
mix deps.get
mix compile
mix phoenix.digest
mix release
PORT=4004 rel/pdf_server/bin/pdf_server start
```

Now, `rel/pdf_server/bin/pdf_server ping` should respond with a nice "pong". If
not, start the server as `pdf_server console` and look at the crash dump. Make
sure that the `PORT` emvironment variable is set (in fish shell, try `set -lx
PORT 4004`)! 

or just

```
mix deps.get
iex -S mix phoenix.server
```

Running the server in dev mode will automatically set the listening to 4004.

## as a service

A precompiled Linux64 release is provided. That's why the large `rel` directory
is there. Still, I assume a systemd-based system. Tested on Ubuntu 16.04 and
Debian 8.0. 

If you don't like /var/www-apps/pdf-server as directory, make sure to fix paths
in shell scripts and systemsd unit file in /etc.

```
mkdir /var/www-appps
cd /var/www-apps
git clone https://github.com/gutschilla/elixir-pdf-server.git 
mv elixir-pdf-server pdf_server
cd exlir-pdf-server
.bash ./etc/install.sh
systemctl start phoenix-pdf-server_backend
```
# Usage

Send a POST request containing some JSON with a key "html" to
`/api/v1/generate`. Get some PDF body back. That's all to it.

```
curl http://127.0.0.1:4004/api/v1/generate \
  -H "Content-Type: application/json" \
  --data-binary '{"html": "<html><body><h1>Hi there!</h1></body></html>"}' \
  > hi-there.pdf

```

