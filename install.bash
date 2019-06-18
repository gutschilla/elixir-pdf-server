#!/bin/bash

tee /etc/cron.daily/upgrade << 'EOF'
#!/bin/bash
sudo apt-get update
sudo apt-get -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade
EOF
chmod ug+x /etc/cron.daily/upgrade
/etc/cron.daily/upgrade

apt-get -y install\
        curl\
        wget\
        apt-utils\
        gnupg\
        git

echo "deb http://binaries.erlang-solutions.com/debian bionic contrib" >> /etc/apt/sources.list.d/erlang-solutions.list
wget https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc
apt-key add erlang_solutions.asc

apt-get update
export DEBIAN_FRONTEND=noninteractive

apt-get install -y \
        erlang \
        elixir

export MIX_ENV=prod
export PORT=4000
apt-get -y install locales locales-all \
    && update-locale LC_ALL=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export V=1
git clone https://github.com/gutschilla/elixir-pdf-server.git \
    && cd elixir-pdf-server \
    && mix local.rebar --force \
    && mix local.hex --force \
    && mix deps.get \
    && mix compile \
    && mix release

curl -sL https://deb.nodesource.com/setup_10.x | bash -
curl -sL https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
echo 'deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main' > /etc/apt/sources.list.d/chrome.list
echo 'deb http://de.archive.ubuntu.com/ubuntu/ bionic universe' >> /etc/apt/sources.list
apt-get update
export DEBIAN_FRONTEND=noninteractive
apt-get install -y \
        nodejs \
        chromium-chromedriver

export PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=TRUE
npm -g install \
    puppeteer \
    chrome-headless-render-pdf

apt-get -y install xfonts-base xfonts-75dpi \
    && wget https://downloads.wkhtmltopdf.org/0.12/0.12.5/wkhtmltox_0.12.5-1.bionic_amd64.deb \
    && dpkg -i wkhtmltox_0.12.5-1.bionic_amd64.deb

apt-get -y install locales locales-all \
    && update-locale LC_ALL=en_US.UTF-8
export LC_ALL=en_US.UTF-8

apt-get -y install nginx

cp -r /root/elixir-pdf-server/_build/prod/rel/elixir_pdf_server /var/www/

mkdir /var/www/pdf-printer;
mkdir /var/www/pdf-printer/root;

# configure nginx frontend server
cd /root
git clone https://github.com/letsencrypt/letsencrypt

tee /etc/nginx/snippets/ssl.include <<'EOF'
ssl_protocols TLSv1.2 TLSv1.1 TLSv1;
ssl_ciphers EECDH+AESGCM:EDH+AESGCM:EECDH:EDH:!MD5:!RC4:!LOW:!MEDIUM:!CAMELLIA:!ECDSA:!DES:!DSS:!3DES:!NULL;
ssl_prefer_server_ciphers on;
ssl_ecdh_curve secp384r1;
ssl_session_cache shared:SSL:10m;
ssl_session_timeout 10m;
EOF

tee /etc/nginx/snippets/proxy-settings.include <<'EOF'
proxy_cache_revalidate on;
proxy_cache_min_uses 3;
proxy_cache_use_stale error timeout updating http_500 http_502 http_503 http_504;
proxy_cache_background_update on;
proxy_cache_lock on;
EOF

# server to host letsencrypt files
mkdir /var/www/pdf-printer/
mkdir /var/www/pdf-printer/root
tee /etc/nginx/conf.d/letsencrypt.conf <<'EOF'
server {
  listen 80;
  listen [::]:80; #IPv6
  server_name pdf-printer-2.remoteceros.com;
  location /.well-known {
    root /var/www/pdf-printer/root;
  }
}
EOF
systemctl reload nginx.service

# get certs
tee /etc/cron.weekly/renew-certs <<'EOF'
#!/bin/bash
/root/letsencrypt/letsencrypt-auto \
    certonly \
    --non-interactive \
    --agree-tos \
    --email dobberstein@yorck.de \
    --domain pdf-printer-2.remoteceros.com \
    --renew-by-default \
    --webroot -w /var/www/pdf-printer/root/
systemctl reload nginx
EOF
chmod ug+x /etc/cron.weekly/renew-certs
bash /etc/cron.weekly/renew-certs

# use certs for reverse proxy
tee /etc/nginx/conf.d/backend.conf <<'EOF'
upstream phoenix_backend {
  server 127.0.0.1:4000;
}
server {
  listen 443 ssl;
  listen [::]:443 ssl;
  server_name pdf-printer-2.remoteceros.com;
  ssl on;
  include /etc/nginx/snippets/ssl.include;
  ssl_certificate     /etc/letsencrypt/live/pdf-printer-2.remoteceros.com/fullchain.pem;
  ssl_certificate_key /etc/letsencrypt/live/pdf-printer-2.remoteceros.com/privkey.pem;

  location / {
    include /etc/nginx/snippets/proxy-settings.include;
    add_header X-Cache-Status $upstream_cache_status;
    proxy_set_header Host            $host;
    proxy_set_header X-Real-IP       $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_pass http://phoenix_backend;
  }
  location ~ /(:?images|js|css|favicon.ico|robots.txt) {
    root /var/www/elixir_pdf_server/apps/pdf_server_web/priv/static;
  }
  location /socket {
      proxy_pass http://phoenix_backend;
      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection "Upgrade";
  }
}
EOF
systemctl reload nginx.service

# install systemd service
tee /lib/systemd/system/elixir-pdf-server.service <<EOF
[Unit]
Description=PDF Printer Service

[Service]
Type=simple
User=www-data
Group=www-data
Restart=on-failure
Environment=LANG=en_US.UTF-8
Environment=HOME=/var/www/elixir_pdf_server
WorkingDirectory=/var/www/elixir_pdf_server
ExecStart=/var/www/elixir_pdf_server/bin/elixir_pdf_server foreground
ExecStop=/var/www/elixir_pdf_server/bin/elixir_pdf_server stop

[Install]
WantedBy=multi-user.target
EOF
systemctl enable elixir-pdf-server.service
systemctl start elixir-pdf-server.service
