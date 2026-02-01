#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

tool="$root/scripts/contractiles-a2ml-tool.py"
fixtures="$root/tests/contractiles/fixtures"
expected="$root/tests/contractiles/expected"

python3 "$tool" validate \
  "$fixtures/Mustfile.a2ml" \
  "$fixtures/Trustfile.a2ml" \
  "$fixtures/Dustfile.a2ml" \
  "$fixtures/Intentfile.a2ml"

python3 "$tool" emit "$fixtures/Mustfile.a2ml" "$expected/mustfile.json.tmp"
python3 "$tool" emit "$fixtures/Trustfile.a2ml" "$expected/trustfile.json.tmp"
python3 "$tool" emit "$fixtures/Dustfile.a2ml" "$expected/dustfile.json.tmp"
python3 "$tool" emit "$fixtures/Intentfile.a2ml" "$expected/intentfile.json.tmp"

diff -u "$expected/mustfile.json" "$expected/mustfile.json.tmp"
diff -u "$expected/trustfile.json" "$expected/trustfile.json.tmp"
diff -u "$expected/dustfile.json" "$expected/dustfile.json.tmp"
diff -u "$expected/intentfile.json" "$expected/intentfile.json.tmp"

rm -f "$expected/mustfile.json.tmp" \
  "$expected/trustfile.json.tmp" \
  "$expected/dustfile.json.tmp" \
  "$expected/intentfile.json.tmp"

echo "Contractiles A2ML v1 tests: OK"
