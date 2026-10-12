import { ZINC_KEYWORDS } from "./keywords";
import type { ZincSymbol } from "./symbols";
import { wordAtPosition } from "./symbols";

export interface CompletionItem {
  label: string;
  kind: "keyword" | "function" | "struct" | "enum" | "trait" | "module" | "type" | "const" | "variable" | "text";
  detail?: string;
}

function symbolKindToCompletion(kind: ZincSymbol["kind"]): CompletionItem["kind"] {
  return kind;
}

export function completeAt(
  source: string,
  line: number,
  character: number,
  symbols: ZincSymbol[]
): CompletionItem[] {
  const prefix = wordAtPosition(source, line, Math.max(0, character - 1))
    ?? wordAtPosition(source, line, character)
    ?? "";

  // If cursor is mid-identifier, use the prefix up to the cursor.
  const lineText = source.split(/\r?\n/)[line] ?? "";
  let actualPrefix = "";
  {
    let i = Math.min(character, lineText.length) - 1;
    while (i >= 0 && /[A-Za-z0-9_]/.test(lineText[i])) {
      actualPrefix = lineText[i] + actualPrefix;
      i--;
    }
  }
  const filter = actualPrefix.length > 0 ? actualPrefix : prefix;

  const items: CompletionItem[] = [];
  const seen = new Set<string>();

  const push = (item: CompletionItem) => {
    if (seen.has(item.label)) {
      return;
    }
    if (filter && !item.label.startsWith(filter)) {
      return;
    }
    seen.add(item.label);
    items.push(item);
  };

  for (const kw of ZINC_KEYWORDS) {
    push({ label: kw, kind: "keyword", detail: "keyword" });
  }

  for (const sym of symbols) {
    push({
      label: sym.name,
      kind: symbolKindToCompletion(sym.kind),
      detail: sym.kind,
    });
  }

  return items;
}
