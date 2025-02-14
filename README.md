# Bech32 and Bech32m encoder and decoder

![Code coverage][coverage-badge]
[![MIT licensed][mit-badge]][mit-url]
[![Apache 2.0 licensed][apache-badge]][apache-url]
[![Contributor Covenant][coc-badge]][coc-url]

[coverage-badge]: https://img.shields.io/badge/Code%20coverage-93%25-green.svg
[mit-badge]: https://img.shields.io/badge/License-MIT-blue.svg
[mit-url]: LICENSE-MIT
[mit-license-url]: https://github.com/DecisionToolkit/dsntk-rs/blob/main/LICENSE-MIT
[apache-badge]: https://img.shields.io/badge/License-Apache%202.0-blue.svg
[apache-url]: https://www.apache.org/licenses/LICENSE-2.0
[apache-license-url]: https://github.com/DecisionToolkit/dsntk-rs/blob/main/LICENSE
[apache-notice-url]: https://github.com/DecisionToolkit/dsntk-rs/blob/main/NOTICE
[coc-badge]: https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg
[coc-url]: CODE_OF_CONDUCT.md
[repository-url]: https://github.com/DariuszDepta/bech 

## Setup

### Install Erlang

```shell
$ sudo dnf install erlang
```
or follow these [instructions](https://www.erlang.org/downloads).

### Install rebar3

[Download rebar3 script](https://s3.amazonaws.com/rebar3/rebar3)
and follow these [instructions](https://rebar3.org/docs/getting-started/).

### This library was created this way

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

> See the task commands for more details.

## License

Licensed under either of

- [MIT license][mit-url] (see [LICENSE-MIT][mit-license-url]) or
- [Apache License, Version 2.0][apache-url] (see [LICENSE][apache-license-url] and [NOTICE][apache-notice-url])

at your option.

## Contribution

Any contributions to [**bech**][repository-url] are greatly appreciated.
All contributions intentionally submitted for inclusion in the work by you,
shall be dual licensed as above, without any additional terms or conditions.
