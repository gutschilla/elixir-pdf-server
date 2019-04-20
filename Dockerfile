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

# RUN curl -sL https://deb.nodesource.com/setup_10.x | bash - 
# RUN curl -sL https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - 
# RUN echo 'deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main' > /etc/apt/sources.list.d/chrome.list
# RUN echo 'deb http://de.archive.ubuntu.com/ubuntu/ bionic universe' >> /etc/apt/sources.list

RUN apt-get update
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get install -y \
#      nodejs \
#      chromium-chromedriver \
      erlang \
      elixir
 
# ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD TRUE
# RUN npm -g install \
#     puppeteer \
#     chrome-headless-render-pdf
# 
# RUN \
#     apt-get -y install xfonts-base xfonts-75dpi \
#     && wget https://downloads.wkhtmltopdf.org/0.12/0.12.5/wkhtmltox_0.12.5-1.bionic_amd64.deb \
#     && dpkg -i wkhtmltox_0.12.5-1.bionic_amd64.deb

ENV MIX_ENV prod
ENV PORT 4000
RUN \
    apt-get -y install locales locales-all \
    && update-locale LC_ALL=en_US.UTF-8
ENV LC_ALL en_US.UTF-8
ENV V 1
RUN git clone https://github.com/gutschilla/elixir-pdf-server.git \
    && cd elixir-pdf-server \
    && mix local.rebar --force \
    && mix local.hex --force \
    && mix deps.get \
    && mix compile \
    && mix release

FROM ubuntu:latest  
RUN \
    apt-get update \
    && apt-get -y install \
    curl \
    wget \
    apt-utils \
    gnupg \
    git

RUN curl -sL https://deb.nodesource.com/setup_10.x | bash - 
RUN curl -sL https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - 
RUN echo 'deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main' > /etc/apt/sources.list.d/chrome.list
RUN echo 'deb http://de.archive.ubuntu.com/ubuntu/ bionic universe' >> /etc/apt/sources.list
RUN apt-get update
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get install -y \
     nodejs \
     chromium-chromedriver

ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD TRUE
RUN npm -g install \
    puppeteer \
    chrome-headless-render-pdf

RUN \
    apt-get -y install xfonts-base xfonts-75dpi \
    && wget https://downloads.wkhtmltopdf.org/0.12/0.12.5/wkhtmltox_0.12.5-1.bionic_amd64.deb \
    && dpkg -i wkhtmltox_0.12.5-1.bionic_amd64.deb

ENV MIX_ENV prod
ENV PORT 4000
RUN \
    apt-get -y install locales locales-all \
    && update-locale LC_ALL=en_US.UTF-8
ENV LC_ALL en_US.UTF-8

WORKDIR /root/
COPY --from=0 /elixir-pdf-server/_build/prod/rel/elixir_pdf_server .
EXPOSE 4000/tcp
CMD ["./bin/elixir_pdf_server", "foreground"]  
