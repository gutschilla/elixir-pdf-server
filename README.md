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

```
mix deps.get
mix compile
mix phoenix.digest
mix release
```

or just

```
mix deps.get
iex -S mix phoenix.server
```
Now you can visit [`localhost:4004`](http://localhost:4004) from your browser.

## as a service

A precompiled Linux64 release is provided. That's why the large `rel` directory
is there. Still, I assume a systemd-based system. Tested on Ubuntu 16.04 and
Debian 8.0

```
mkdir /var/www-appps
cd /var/www-apps
git clone https://github.com/gutschilla/elixir-pdf-server.git 
cd exlir-pdf-server
.bash ./etc/install.sh
systemctl start phoenix-pdf-server_backend
```
