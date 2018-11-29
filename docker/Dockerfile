FROM erlang:19

WORKDIR /work

COPY config config
COPY rebar.config .
COPY src src
COPY test test
RUN rebar3 compile
