FROM ubuntu:bionic AS builder

RUN \
    apt-get update \
    && apt-get -y install \
    curl \
    wget \
    apt-utils \
    gnupg \
    git

RUN echo "deb http://binaries.erlang-solutions.com/debian bionic contrib" >> /etc/apt/sources.list.d/erlang-solutions.list
RUN wget https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc
RUN apt-key add erlang_solutions.asc

RUN curl -sL https://deb.nodesource.com/setup_10.x | bash - 
RUN curl -sL https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - 
RUN echo 'deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main' > /etc/apt/sources.list.d/chrome.list
RUN echo 'deb http://de.archive.ubuntu.com/ubuntu/ bionic universe' >> /etc/apt/sources.list
RUN apt-get update
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get install -y \
     nodejs \
     chromium-chromedriver \
     erlang \
     elixir

ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD TRUE
RUN npm -g install \
    puppeteer \
    chrome-headless-render-pdf

ENV MIX_ENV prod
ENV PORT 4000
RUN update-locale LC_ALL=en_US.UTF-8
RUN \
    git clone https://github.com/gutschilla/elixir-pdf-server.git \
    && mix local.rebar --force \
    && mix local.hex --force \
    && mix deps.get \
    && mix compile \
    && mix release