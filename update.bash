#!/bin/bash

export MIX_ENV=prod
export PORT=4000
cd /root\
    && cd elixir-pdf-server\
    && git pull\
    && mix local.rebar --force\
    && mix local.hex --force\
    && mix deps.get\
    && mix compile\
    && mix release

systemctl stop elixir-pdf-server.service
cp -r /root/elixir-pdf-server/_build/prod/rel/elixir_pdf_server /var/www/
systemctl start elixir-pdf-server.service
