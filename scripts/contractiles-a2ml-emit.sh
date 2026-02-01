#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

out_dir="${1:-$root/build/contractiles}"
mkdir -p "$out_dir"

emit_one() {
  local input="$1"
  local output="$2"
  python3 - "$input" > "$output" <<'PY'
import json
import re
import sys
from pathlib import Path

path = Path(sys.argv[1])
name = path.name

section = None
item = None

if name == "Mustfile.a2ml":
    result = {"type": "mustfile", "parameters": {}, "checks": []}
elif name == "Trustfile.a2ml":
    result = {"type": "trustfile", "inputs": {}, "verifications": []}
elif name == "Dustfile.a2ml":
    result = {"type": "dustfile", "sections": {}}
elif name == "Intentfile.a2ml":
    result = {"type": "intentfile", "future": {}}
else:
    raise SystemExit(f"Unsupported file: {name}")

def flush_item():
    global item
    if item is None:
        return
    if name == "Mustfile.a2ml":
        result["checks"].append(item)
    elif name == "Trustfile.a2ml":
        result["verifications"].append(item)
    elif name == "Dustfile.a2ml":
        result["sections"].setdefault(section, []).append(item)
    item = None

lines = path.read_text(encoding="utf-8").splitlines()
for raw in lines:
    line = raw.rstrip()
    if line.startswith("## "):
        flush_item()
        section = line[3:].strip()
        continue
    if line.startswith("### "):
        flush_item()
        item = {"name": line[4:].strip()}
        continue
    if not line.startswith("- "):
        continue

    content = line[2:].strip()
    if ":" in content:
        key, value = content.split(":", 1)
        key = key.strip()
        value = value.strip()
        if name == "Mustfile.a2ml" and section == "Parameters":
            result["parameters"][key] = value
        elif name == "Trustfile.a2ml" and section == "Inputs":
            result["inputs"][key] = value
        elif name == "Dustfile.a2ml" and item is not None:
            item[key] = value
        elif name in {"Mustfile.a2ml", "Trustfile.a2ml"} and item is not None:
            item[key] = value
        elif name == "Intentfile.a2ml":
            result["future"].setdefault(section, []).append(content)
    else:
        if name == "Intentfile.a2ml" and section is not None:
            result["future"].setdefault(section, []).append(content)

flush_item()
print(json.dumps(result, indent=2, sort_keys=True))
PY
}

emit_one "$root/contractiles/must/Mustfile.a2ml" "$out_dir/mustfile.json"
emit_one "$root/contractiles/trust/Trustfile.a2ml" "$out_dir/trustfile.json"
emit_one "$root/contractiles/dust/Dustfile.a2ml" "$out_dir/dustfile.json"
emit_one "$root/contractiles/lust/Intentfile.a2ml" "$out_dir/intentfile.json"

echo "Wrote: $out_dir/mustfile.json"
echo "Wrote: $out_dir/trustfile.json"
echo "Wrote: $out_dir/dustfile.json"
echo "Wrote: $out_dir/intentfile.json"
