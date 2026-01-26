#!/usr/bin/env bash
set -euo pipefail

DDRAIG_REPO="/var/mnt/eclipse/repos/ddraig-ssg"

if [ ! -d "$DDRAIG_REPO" ]; then
  echo "ddraig-ssg repo not found at $DDRAIG_REPO" >&2
  exit 1
fi

idris2 -i "$DDRAIG_REPO" prototype/ddraig/DemoBuild.idr -o build/ddraig-demo
./build/ddraig-demo
