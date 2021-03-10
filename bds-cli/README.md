# CLI Tool for Binary Data Schema

To build the CLI tool you need to have the Rust toolchain installed and up to date. To ensure you have the latest version installed run:

```
rustup update
```

The binary is built with Rust's package manager `cargo`:

```
cargo build
```

The binary is stored in `bds-cli/target/debug/bds-cli` and has a simple CLI documented in the `--help` option.

To check the program run:

```
./bds-cli -s examples/led-rgb/schema.json -i examples/led-rgb/valid-1.json encode
```

Alternatively you can use `cargo run -- {flags for binary-data-schema}` to run the tool.

The result should be `7e000503ff10ff00ef`.

## License

Licensed under [AGPL v3](LICENSE). For a commercial license contact the author, please.

## Contribution

See [CONTRIBUTING.md](CONTRIBUTING.md).
