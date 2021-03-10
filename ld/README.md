# Linked Data Documents

This folder contains [the vocabulary](bds.ttl) and a [JSON-LD context](context.jsonld) to help interpreting Binary Data schemata as RDF.

The files are only stored here to record their history.
For better interaction with Linked Data tools they are hosted on the [SoLID POD](https://solidproject.org/) of the Chair of Technical Information Systems at the Friedrich-Alexander-University Erlangen-Nuremberg.

| File             | Hosted as                                               |
| ---------------- | ------------------------------------------------------- |
| `bds.ttl`        | https://solid.ti.rw.fau.de/public/ns/bds.ttl            |
| `context.jsonld` | https://solid.ti.rw.fau.de/public/ns/bds/context.jsonld |

As the context is publicly hosted you can simply add https://solid.ti.rw.fau.de/public/ns/bds/context.jsonld to the [context](https://www.w3.org/TR/json-ld/#the-context) of your Binary Data Schema description to make it machine-readable.
For example:

```json
{
    "@context": [
        "https://www.w3.org/2019/wot/td/v1",
        "https://solid.ti.rw.fau.de/public/ns/bds/context.jsonld"
    ],
    "title": "Some Thing Description.",
    "properties": {
        "temperature": {
            "type": "number",
            "length": 2,
            "scale": 0.025,
            "unit": "degree celcius",
            "description": "Some temperature that is encoded as 2-byte integer and a scale of 0.025 °C but thanks to BDS the decoded value is a floating-point number in °C.",
            "forms": [{ "href": "http://example.org/temp" }]
        }
    }
}
```