#!/bin/sh

./configure \
    --prefix=/usr/share/nginx \
    --sbin-path=/usr/sbin/nginx \
    --conf-path=/etc/nginx/nginx.conf \
    --pid-path=/var/run/nginx.pid \
    --lock-path=/var/lock/nginx.lock \
    --error-log-path=/var/log/nginx/error.log \
    --http-log-path=/var/log/access.log \
    --user=ubuntu \
    --group=ubuntu \
    --with-ipv6 \
    --with-http_ssl_module \
    --add-module=/tmp/chunkin
