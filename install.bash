apt-get update \
    && apt-get -y install \
               curl \
               wget \
               apt-utils \
               gnupg \
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

export MIX_ENV=prod
export PORT=4000
apt-get -y install locales locales-all \
    && update-locale LC_ALL=en_US.UTF-8
export LC_ALL=en_US.UTF-8

