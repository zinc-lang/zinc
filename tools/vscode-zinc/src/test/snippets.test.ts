import assert from "node:assert/strict";
import { describe, it } from "node:test";
import fs from "node:fs";
import path from "node:path";
import { extensionRoot } from "./helpers";

describe("snippets/zinc.json", () => {
  const snippets = JSON.parse(
    fs.readFileSync(path.join(extensionRoot(), "snippets/zinc.json"), "utf8")
  ) as Record<
    string,
    { prefix: string | string[]; body: string | string[]; description?: string }
  >;

  it("includes println / print / f-string helpers", () => {
    assert.ok(snippets.println);
    assert.ok(snippets.print);
    assert.ok(snippets["println-fstring"]);
    assert.ok(snippets.fstring);
  });

  it("never emits Rust println! or {} format placeholders in print snippets", () => {
    const bodies = Object.values(snippets).flatMap((s) =>
      Array.isArray(s.body) ? s.body : [s.body]
    );
    for (const body of bodies) {
      assert.doesNotMatch(body, /println!\s*\(/);
      assert.doesNotMatch(body, /"[^"]*\{\}[^"]*"/);
    }
    assert.match(String(snippets.println.body), /^println\(\$\{1:expr\}\);$/);
    assert.match(
      String(snippets["println-fstring"].body),
      /\$\(\$\{2:expr\}\)/
    );
  });

  it("every snippet has a prefix and non-empty body", () => {
    for (const [name, snip] of Object.entries(snippets)) {
      assert.ok(snip.prefix, `${name} missing prefix`);
      assert.ok(snip.body, `${name} missing body`);
    }
  });
});
