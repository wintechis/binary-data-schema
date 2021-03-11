# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.2.0 [unreleased]

### Breaking Changes

- Split up error types into validation, encoding and decoding.
- CLI tool is now separated from the crate.
- `"minLength"` and `"maxLength"` are now related to the decoded JSON string.

### Features

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
