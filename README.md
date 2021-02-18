# binary-data-schema

[![Crates.io](https://img.shields.io/crates/v/binary-data-schema.svg)](https://crates.io/crates/binary-data-schema)
[![Docs.rs](https://docs.rs/binary-data-schema/badge.svg)](https://docs.rs/binary-data-schema)
[![CI](https://github.com/wintechis/binary-data-schema/workflows/Continuous%20Integration/badge.svg)](https://github.com/wintechis/binary-data-schema/actions)
[![Coverage Status](https://coveralls.io/repos/github/wintechis/binary-data-schema/badge.svg?branch=master)](https://coveralls.io/github/wintechis/binary-data-schema?branch=master)

An expansion of [Web of Things Thing Description](https://www.w3.org/TR/wot-thing-description/)'s [Data Schema](https://www.w3.org/2019/wot/json-schema) to be able to serialize for content type `application/octet-stream`.

## CLI Tool

To build the CLI tool you need to have the Rust toolchain installed and up to date. To ensure you have the latest version installed run:

```
rustup update
```

The binary is built with Rust's package manager `cargo`:

```
cargo build
```

The binary is stored in `binary-data-schema/target/debug/binary-data-schema` and has a simple CLI documented in the `--help` option.

To check the program run:

```
./binary-data-schema -s binary-data-schema/examples/led-rgb/schema.json -i binary-data-schema/examples/led-rgb/valid-1.json encode
```

Alternatively you can use `cargo run -- {flags for binary-data-schema}` to run the tool.

The result should be `7e000503ff10ff00ef`.

## License

Licensed under [AGPL v3](LICENSE). For a commercial license contact the author, please.

## Contribution

See [CONTRIBUTING.md](CONTRIBUTING.md).
