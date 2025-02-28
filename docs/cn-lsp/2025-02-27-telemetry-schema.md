# Introduction

This document describes the layout of telemetry directories, and the schema of
JSON-encoded telemetry events, in the CN language server (`cn-lsp`). This
document is current as of today, 27 February 2025.

# Directory Layout

If a user provided `dir` as their telemetry directory, here's how that directory
might be laid out:
```
dir
├── profile.json
├── session-X
│   └── source-id
│       ├── events.json
│       └── source.json
└── session-Y
    └── source-id
        ├── events.json
        └── source.json
```

## `profile.json`

Optional.

Contains any profile information the user provides. If present, it should
contain a single JSON object:
```json
{
  "id": "<email-address>"
}
```


## `session-{X,Y}`

One or more.

Each `session-X` directory contains telemetry associated with a single session.
`X` here represents a session identifier. Currently, all session identifiers are
calendar dates, encoded as `<year>-<month>-<day>`. (A session created today
would be stored under `session-2025-2-27`.)


## `source-id`

One or more.

A filename-friendly encoding of a particular source. (The full name is stored in
this directory at `source.json`.)


## `events.json`

One exactly.

A sequence of JSON-encoded telemetry events. Individual events are JSON objects
that obey the schema specified [here](./2025-02-27-telemetry-schema.json). The
"true" schema for these events is specified indirectly by the
automatically-generated JSON serializers of the OCaml data structures specified
in [`serverTelemetry.ml`](../../cn-lsp/lib/serverTelemetry.ml), but this schema
should mirror that precisely.

These events are stored sequentially in this file, separated by newlines. This
means that, while each line contains a valid JSON object, these files themselves
are *not*, strictly speaking, valid JSON. Consumers must split the file contents
into lines and interpret each line as JSON.


## `source.json`

One exactly.

The full name of the source of these events. For telemetry generated from
`cn-lsp`, this source will be the literal `"cn-lsp-server"`.
