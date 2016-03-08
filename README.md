# Kitsune

An Erlang/OTP application for backing up the GitHub repositories of a particular user. Named after the fox of [Japanese folklore](https://en.wikipedia.org/wiki/Kitsune), for no particular reason.

## Building and Testing

### Prerequisites

* [Erlang/OTP](http://www.erlang.org) R18 or higher
* [rebar](https://github.com/rebar/rebar/) for compiling and testing
* [relx](https://github.com/erlware/relx) for building a release

To download the dependencies, build the application, and run the test suite, use `make` as follows:

```
$ make test
```

### Deploying

1. Write a configuration file, named `user_env.config`, at the base of the source tree.
    * See `example.config` in the `docs` directory.
1. Build the release: `make release`
1. Copy the contents of `_rel` to the desired installation location (e.g. `/opt`).
1. Start it up, likely using `sudo`.
1. Occasionally check the log files in `/opt/kitsune/log`.

For example:

```shell
$ cp ~/kitsune.config user_env.config
$ make release
$ sudo mkdir -p /opt
$ sudo cp -R _rel/kitsune /opt
$ sudo /opt/kitsune/bin/kitsune -detached
```

### BSD daemon

See the `config/kitsune.rc` file for an example of managing the kitsune application as a daemon via `rc.d` in BSD systems (in particular FreeBSD, and likely NetBSD as well). You will need to build and deploy the application as described above, and then use the `service` command to start it, as illustrated in `kitsune.rc`.
