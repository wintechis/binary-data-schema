# Binary Data Schema

[![Crates.io](https://img.shields.io/crates/v/binary-data-schema.svg)](https://crates.io/crates/binary-data-schema)
[![Docs.rs](https://docs.rs/binary-data-schema/badge.svg)](https://docs.rs/binary-data-schema)
[![CI](https://github.com/wintechis/binary-data-schema/workflows/Continuous%20Integration/badge.svg)](https://github.com/wintechis/binary-data-schema/actions)
[![Coverage Status](https://coveralls.io/repos/github/wintechis/binary-data-schema/badge.svg?branch=master)](https://coveralls.io/github/wintechis/binary-data-schema?branch=master)

An expansion of [Web of Things Thing Description](https://www.w3.org/TR/wot-thing-description/)'s [Data Schema](https://www.w3.org/2019/wot/json-schema) to be able to serialize for content type `application/octet-stream`.

A thorough documentation of the features is provided in the [Rust docs](https://docs.rs/binary-data-schema).

## CLI Tool

For quick testing a CLI tool is provided under [bds-cli/](bds-cli/README.md).

## Linked Data Processing

A [vocabulary and a JSON-LD context](ld/README.md) are provided to allow the interpretation of Binary Data Schemata as RDF.

## License

Licensed under [AGPL v3](LICENSE). For a commercial license contact the author.

## Contribution

See [CONTRIBUTING.md](CONTRIBUTING.md).
