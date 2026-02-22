# Stage 1: Build snake-duel-stablew frontend as ES module
FROM docker.io/library/node:22-alpine AS frontend
WORKDIR /frontend
COPY hecate-app-snake-duel-stablew/package.json hecate-app-snake-duel-stablew/package-lock.json* ./
RUN npm ci
COPY hecate-app-snake-duel-stablew/ .
RUN npx svelte-kit sync 2>/dev/null || true
RUN npm run build:lib

# Stage 2: Build snake-duel-stabled Erlang release
FROM docker.io/library/erlang:27-alpine AS backend
WORKDIR /build
RUN apk add --no-cache git curl bash build-base cmake
RUN curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3
COPY hecate-app-snake-duel-stabled/rebar.config hecate-app-snake-duel-stabled/rebar.lock* ./
COPY hecate-app-snake-duel-stabled/config/ config/
COPY hecate-app-snake-duel-stabled/src/ src/
COPY hecate-app-snake-duel-stabled/include/ include/
RUN rebar3 get-deps && rebar3 compile
COPY --from=frontend /frontend/dist priv/static/
RUN rebar3 as prod release

# Stage 3: Runtime
FROM docker.io/library/alpine:3.22
RUN apk add --no-cache ncurses-libs libstdc++ libgcc openssl ca-certificates
WORKDIR /app
COPY --from=backend /build/_build/prod/rel/hecate_app_snake_duel_stabled ./
ENTRYPOINT ["/app/bin/hecate_app_snake_duel_stabled"]
CMD ["foreground"]
