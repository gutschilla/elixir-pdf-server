#!/bin/bash

USER=phoenix-pdf-server
ROOT=/var/www-apps/pdf_server
APP=pdf_server

# create and chown tmp and log directories
test -d              $ROOT/rel/$APP/log \
        || mkdir     $ROOT/rel/$APP/log
chown -R $USER:$USER $ROOT/rel/$APP/log

test -d              $ROOT/rel/$APP/tmp \
            || mkdir $ROOT/rel/$APP/tmp
chown -R $USER:$USER $ROOT/rel/$APP/tmp

# make sure the binary is executable
chmod ag+rx $ROOT/rel/$APP/bin/$APP
find        $ROOT/rel/$APP/releases/ -iname $APP.sh | xargs chmod a+rx

# return proper code
exit 0

