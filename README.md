# Kitsune

An Erlang/OTP application for backing up the GitHub repositories of a particular user. Named after the fox of [Japanese folklore](https://en.wikipedia.org/wiki/Kitsune), for no particular reason.

## Building and Testing

### Prerequisites

* [Erlang/OTP](http://www.erlang.org) R18 or higher
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

To download the dependencies, build the application, and run the test suite, use `rebar3` as follows:

```
$ rebar3 ct
```

### Using Docker

```shell
$ cd docker
$ docker-compose build
$ docker-compose run kitsune
> rebar3 ct
```

## Deploying

### Docker

The application is easily deployed using [Docker](https://www.docker.com), as
there is a provided `Dockerfile` and `docker-compose.yml` file for building and
running the application in Docker.

1. Write a configuration file, named `user_env.config`, at the base of the source tree.
    * See `example.config` in the `config` directory.
1. Build the release: `rebar3 release`
1. Copy the contents of `_build/default/rel` to the desired installation location.
