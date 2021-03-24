# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]

### Bugfixes

- Check for schemata with `tillend` length encoding in object and array schema.

## 0.2.0

### Breaking Changes

- Split up error types into validation, encoding and decoding.
- CLI tool is now separated from the crate.
- `"minLength"` and `"maxLength"` are now related to the decoded JSON string.
- Feature `"const"` is replaced by `"default"` as this is closer to the implementation in BDS.

### Features

- Thorough documentation.
- string and array schema have now the same `"lengthEncoding"` options.
- [Vocabulary and JSON-LD](../ld/README.md) context to allow interpretation as RDF.
- boolean schema are now recognized as bitfields so they can be merged with others in object schemata.

### Bugfixes

- `"jsonld:context"` only valid for object schema.

## 0.1.1

### Features

- Recognize `"jsonld:context"` to provide JSON-LD context to decoded JSON.

### Bugfixes

- string schema: binary format and endpattern work now.
