# bech32 & bech32m encoder/decoder

## Setup

### Install Erlang

```shell
$ sudo dnf install erlang
```
or follow these [instructions](https://www.erlang.org/downloads).

### Install rebar3

[Download rebar3 script](https://s3.amazonaws.com/rebar3/rebar3)
and follow these [instructions](https://rebar3.org/docs/getting-started/).

For some reason, it maybe practical to make `rebar3` global:
```shell
$ sudo cp ~/.cache/rebar3/bin/rebar3 /usr/local/bin
```

### Library was initially created this way 

```shell
$ rebar3 new lib bech
```

### Build

```shell
$ rebar3 compile
```

### Test

```shell
$ rebar3 eunit
```



