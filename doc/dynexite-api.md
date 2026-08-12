# Dynexite builder WebSocket protocol (reverse-engineered)

> **Note:** Unlike the other files in `doc/`, this is *technical developer information*, not
> user-level documentation. It records what we reverse-engineered about Dynexite's internal
> question-editor protocol so that the `uploadDynexite` feature can be understood and maintained.
> Dynexite is closed-source (RWTH), so everything here was observed empirically and may break when
> Dynexite changes. There is no official/public API.

## What this enables

Uploading a Moodle/STACK question XML into an existing Dynexite item programmatically, i.e. what a
human does manually via *edit question → `</>` → paste XML*. Implemented in
`externalsystems/Dynexite.scala` (`uploadQuestionXML`), driven from
`MarkdownAssessment.mainUploadDynexite` (run option `uploadDynexite`).

`scripts/dynexite_replay.py` is a standalone Python experiment tool for poking at the protocol.

## Connection

- **URL:** `wss://dynexite.rwth-aachen.de/t/api/sub/builder/<itemId>`
  - `<itemId>` is the id from a question-edit URL `…/items/<itemId>/edit`. Stored per problem in
    the `dynexiteQuestionId` tag.
- **Headers:**
  - `Cookie: dyn-orbit-teacher=<token>` — the teacher session cookie (= password; expires).
  - `Origin: https://dynexite.rwth-aachen.de` — required.
- **Handshake rejection:** a bad/expired/malformed cookie makes the server reject the WebSocket
  upgrade with an HTTP 4xx (observed: **401** for a wrong token, **400** for a cookie sent without
  its `dyn-orbit-teacher=` name). Our code treats any handshake rejection as an auth failure,
  discards the stored cookie, and re-prompts.
- The channel is **collaborative**: several clients (e.g. your browser and this tool) can be
  subscribed to the same item at once and receive each other's messages.

## Message envelope

All frames are JSON text. Common fields:

- `action` — message type (see below). Absent/`""` on some server pushes.
- `data` — payload (shape depends on `action`).
- `msgId` — string. Client picks an (incrementing) id for its requests; the server echoes it back
  in the matching `OK`. Server-initiated frames use `""`.
- `hasErrors`, `errorMap`, `timestamp`, `fingerprint` — present on various server frames.

### Server → client actions

- **`INIT`** — sent right after connect. Carries the full item state under `data.item`:
  - `data.item.itemId`, `data.item.name` (the human-readable question name shown in listings),
    `data.item.blocks` (array), plus `pointsOpenMax`/`pointsClosedMax`, `fingerprint`, etc.
  - Each block: `{ uuid, type ("stack"), blockVersion, version, task, logic{…} }`.
  - `data.item.blocks` is **`null`** (not `[]`) for a brand-new item that has no blocks yet.
- **`USER:JOIN`** — a user (possibly yourself) subscribed. `data` = user id.
- **`VALIDATION`** — result of server-side re-parsing/rendering after a change (see below). Only
  emitted when content actually changed. Has `hasErrors`, `errorMap`, `generatedContentMap`.
- **`OK`** — acknowledgement of a client message; `msgId` equals the client's. **Always** sent.
  For some requests it carries a payload under `data` (e.g. `BLOCK:CREATE` returns the new block as
  `data.block`).
- **`ERROR`** — a request failed; `data` is a short string (e.g. `"unable to find block with uuid"`
  when `ELEMENT:CHANGED` references a block that isn't attached to the item). `msgId` matches.

### Client → server actions

- **`ELEMENT:CHANGED`** — submit a modified block. Envelope:
  ```json
  { "action": "ELEMENT:CHANGED",
    "data": { "editor": <payload>, "users": <payload> },
    "msgId": "1001" }
  ```
  where `<payload>` is
  ```json
  { "uuid": "<blockUuid>", "changeCategory": "configuration",
    "changes": <block>, "solutionPath": null, "task": "<title>" }
  ```
  `<block>` is the whole block object (as received in `INIT`) with the fields you want to change
  edited in place. Both `data.editor` and `data.users` carry the same payload.
- **`BLOCK:CREATE`** — create a new (empty) block. `{ "action": "BLOCK:CREATE",
  "data": { "type": "stack" }, "msgId": "…" }`. The server replies `OK` with the freshly minted
  block (including a server-assigned `uuid`) under `data.block`. This only *mints* the block; it is
  not yet attached to the item.
- **`BLOCK:ADD`** — attach a block to the item. `{ "action": "BLOCK:ADD",
  "data": { "block": <block> }, "msgId": "…" }`, where `<block>` is the object returned by
  `BLOCK:CREATE` plus a `solutionPath: null` field. The server replies `OK`, and (because a
  freshly created block is empty and hence invalid) also a `VALIDATION` with `hasErrors: true` —
  that failing validation is expected and can be ignored; fill the block with a following
  `ELEMENT:CHANGED`.

  **Creating a question from scratch** (item exists but has no blocks) is therefore a three-step
  sequence: `BLOCK:CREATE` → `BLOCK:ADD` → `ELEMENT:CHANGED` (using the `uuid` from the created
  block). This is what `uploadQuestionXML` does when `data.item.blocks` is `null`.

## The block `logic` object

Fields observed: `questionXML`, `questionRender`, `questionSampleSolutionText`, `questionInputs`,
`questionAssets`, `questionAssetUrls`, `questionVariantSeeds`, `seed`, `renderHash`, `resultData`,
`points`, `errors`, `answers`, `validations`, `language`.

Key insight: **the server parses and renders server-side.** The client is essentially a *dumb echo*
— it stores whatever the server last sent and re-uploads it. So to change a question you only need
to set:

- **`logic.questionXML`** — the Moodle/STACK quiz XML (exactly what `MoodleStack.Quiz.prettyXml`
  produces). The server re-parses it and recomputes `questionRender`, `questionInputs`,
  `questionSampleSolutionText`, `renderHash`, … . You may leave those stale; `renderHash` is the
  server's staleness check and forces a recompute when the XML changes.
- **`logic.language`** — `"en"` or `"de"` (the German/English toggle in the editor). Our upload
  always forces `"en"`.
- **`block.task`** — the block **title** (the bold heading in the editor). It is **Markdown**
  (e.g. `"Stabilizer **State**"`). Our upload sets it from the assessment name, escaped via
  `Plaintext(name).toMarkdown` so plaintext renders verbatim.

## Upload sequence (what `uploadQuestionXML` does)

1. Connect (cookie + origin). On 4xx handshake rejection → discard cookie, re-prompt, retry once.
2. Receive `INIT`. Verify `data.item.name` equals the expected name (`dynexiteQuestionName`, else
   the assessment name) — guards against a wrong `dynexiteQuestionId`. Then, on `data.item.blocks`:
   - `null`/empty → create a STACK block (`BLOCK:CREATE` + `BLOCK:ADD`) and use it.
   - exactly one block whose `type` is not `"stack"` → error.
   - more than one block → error.
   - exactly one STACK block → use it.
3. Take that block, set `logic.questionXML`, `logic.language = "en"`, `task = <title>`.
4. Send `ELEMENT:CHANGED` with `msgId` (e.g. `"1001"`).
5. Read frames until our `OK` (matching `msgId`) arrives:
   - If a `VALIDATION` with `hasErrors: true` arrives first, the upload was rejected — throw.
   - **Do not block waiting for `VALIDATION`**: if nothing changed (identical re-upload), the
     server sends no `VALIDATION`, only `OK`.
   - Ignore unrelated frames (`USER:JOIN`, other clients' acks, clean `VALIDATION`).
6. Close.

## Validation errors

`VALIDATION.errorMap` maps block-uuid → list of `{ reason, source, line, column, message }`.
`uploadQuestionXML` pretty-prints this. Examples seen:

- Invalid XML (e.g. plain text instead of Moodle XML) → `reason: "stack"`,
  `message: "cat.blocks.stack.error.not-supported"`, empty render.
- `Stack Error: CAS failed to return any data due to timeout.` — a **transient** server-side
  STACK/Maxima timeout, not an XML problem; usually a re-upload succeeds.

## Editor URL

The human-facing editor URL for an item is
`https://dynexite.rwth-aachen.de/t/companies/cpsippjadbec73a3unm0/items/<itemId>/edit`
(`Dynexite.editUrl`). `uploadQuestionXML`'s caller prints it on success.

## Not (yet) reverse-engineered

- Creating a **new item** (we can add blocks to an existing item, but the item id itself must
  already exist).
- Items with **multiple blocks** (the uploader requires exactly one STACK block).
- Semantics of `version` / `blockVersion` (the server appears to own them; we echo them unchanged).
- Uploading question assets/images (`questionAssets`, `questionAssetUrls`).
- Variant generation (`hasVariantGeneration`, `questionVariantSeeds`).
