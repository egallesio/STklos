# STklos Dockerfile 
#
# Build an STklos image on top of Alpine Linux
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date:  3-Jun-2019 11:28
# Last file update:  5-Jun-2020 16:33 (eg)

# ---------- Build stage

FROM alpine:latest AS build
RUN apk update && \
    apk add build-base gc-dev gmp-dev libffi-dev pcre-dev git

WORKDIR /build
RUN git clone https://github.com/egallesio/STklos.git stklos
WORKDIR /build/stklos
RUN env CFLAGS=-O3 ./configure \
    --without-provided-bignum \
    --without-provided-gc     \
    --without-provided-regexp \
    --without-provided-ffi
RUN env TERM="dumb" make all install

# ---------- Running stage
FROM alpine:latest
RUN apk update && \
    apk add gc gmp libffi pcre readline
COPY --from=build /usr/local/ /usr/local/
RUN ln -s /usr/lib/libreadline.so.8.0 /usr/lib/libreadline.so
CMD ["stklos", "--utf8-encoding=yes"]

# Local Variables:
# mode: dockerfile
# coding: utf-8
# End:
