#!/usr/bin/env bash
set -euo pipefail

mkdir -p dist/a2ml-cli

cp -v bin/a2ml dist/a2ml-cli/
cp -v docs/a2ml.1 dist/a2ml-cli/
cp -v docs/CLI.adoc dist/a2ml-cli/

# Include compiled JS if present
if [ -f prototype/rescript/src/Cli.bs.js ]; then
  cp -v prototype/rescript/src/Cli.bs.js dist/a2ml-cli/
fi

(cd dist && tar -czf a2ml-cli.tar.gz a2ml-cli)

echo "Created dist/a2ml-cli.tar.gz"
