#!/usr/bin/env python3
import argparse
import json
import sys
from pathlib import Path

SPEC_VERSION = "1.0.0"

REQUIRED = {
    "Mustfile.a2ml": {
        "sections": ["Parameters", "Checks"],
        "parameters": ["gateway_port", "schema_version"],
        "item_fields": ["description", "run"],
    },
    "Trustfile.a2ml": {
        "sections": ["Inputs", "Verifications"],
        "inputs": [
            "policy_path",
            "policy_hash_path",
            "schema_path",
            "schema_sig_path",
            "schema_pub_path",
            "driver_paths",
            "migrations_path",
            "migrations_sig_path",
            "migrations_pub_path",
        ],
        "item_fields": ["description", "command"],
    },
    "Dustfile.a2ml": {
        "sections": ["Logs", "Policy", "Gateway", "Dust-Events"],
        "fields_by_section": {
            "Logs": ["path", "reversible", "handler"],
            "Policy": ["path", "rollback"],
            "Gateway": ["event", "undo"],
            "Dust-Events": ["source", "transform"],
        },
    },
    "Intentfile.a2ml": {
        "sections": ["Trust-Engine", "Control-Plane", "Pipeline", "Introspection"],
    },
}


def parse_a2ml(path: Path):
    section = None
    item = None
    data = {
        "sections": {},
        "items": {},
        "bullets": {},
    }

    def flush_item():
        nonlocal item
        if item is None:
            return
        data["items"].setdefault(section, []).append(item)
        item = None

    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.rstrip()
        if line.startswith("## "):
            flush_item()
            section = line[3:].strip()
            data["sections"].setdefault(section, True)
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
            if item is not None:
                item[key] = value
            else:
                data["bullets"].setdefault(section, []).append({key: value})
        else:
            data["bullets"].setdefault(section, []).append(content)

    flush_item()
    return data


def validate(path: Path):
    name = path.name
    if name not in REQUIRED:
        return [f"Unsupported file: {name}"]

    req = REQUIRED[name]
    parsed = parse_a2ml(path)
    errors = []

    for section in req.get("sections", []):
        if section not in parsed["sections"]:
            errors.append(f"Missing section: {section}")

    if name == "Mustfile.a2ml":
        params = {}
        for entry in parsed["bullets"].get("Parameters", []):
            if isinstance(entry, dict):
                params.update(entry)
        for key in req.get("parameters", []):
            if key not in params or params[key] == "":
                errors.append(f"Missing parameter: {key}")

        checks = parsed["items"].get("Checks", [])
        seen = set()
        for check in checks:
            cname = check.get("name", "")
            if not cname:
                errors.append("Check missing name")
                continue
            if cname in seen:
                errors.append(f"Duplicate check name: {cname}")
            seen.add(cname)
            for field in req.get("item_fields", []):
                if field not in check or check[field] == "":
                    errors.append(f"Check '{cname}' missing field: {field}")

    elif name == "Trustfile.a2ml":
        inputs = {}
        for entry in parsed["bullets"].get("Inputs", []):
            if isinstance(entry, dict):
                inputs.update(entry)
        for key in req.get("inputs", []):
            if key not in inputs or inputs[key] == "":
                errors.append(f"Missing input: {key}")

        verifications = parsed["items"].get("Verifications", [])
        for verification in verifications:
            vname = verification.get("name", "")
            if not vname:
                errors.append("Verification missing name")
                continue
            for field in req.get("item_fields", []):
                if field not in verification or verification[field] == "":
                    errors.append(f"Verification '{vname}' missing field: {field}")

    elif name == "Dustfile.a2ml":
        for section, fields in req.get("fields_by_section", {}).items():
            items = parsed["items"].get(section, [])
            if not items:
                errors.append(f"Section '{section}' has no entries")
                continue
            for entry in items:
                ename = entry.get("name", "")
                if not ename:
                    errors.append(f"{section} entry missing name")
                    continue
                for field in fields:
                    if field not in entry or entry[field] == "":
                        errors.append(f"{section} '{ename}' missing field: {field}")

    elif name == "Intentfile.a2ml":
        for section in req.get("sections", []):
            bullets = parsed["bullets"].get(section, [])
            if not bullets:
                errors.append(f"Section '{section}' must include at least one item")

    return errors


def emit(path: Path):
    name = path.name
    parsed = parse_a2ml(path)

    if name == "Mustfile.a2ml":
        parameters = {}
        for entry in parsed["bullets"].get("Parameters", []):
            if isinstance(entry, dict):
                parameters.update(entry)
        checks = parsed["items"].get("Checks", [])
        return {
            "type": "mustfile",
            "spec_version": SPEC_VERSION,
            "parameters": parameters,
            "checks": checks,
        }

    if name == "Trustfile.a2ml":
        inputs = {}
        for entry in parsed["bullets"].get("Inputs", []):
            if isinstance(entry, dict):
                inputs.update(entry)
        verifications = parsed["items"].get("Verifications", [])
        return {
            "type": "trustfile",
            "spec_version": SPEC_VERSION,
            "inputs": inputs,
            "verifications": verifications,
        }

    if name == "Dustfile.a2ml":
        sections = {}
        for section in REQUIRED[name]["sections"]:
            sections[section] = parsed["items"].get(section, [])
        return {
            "type": "dustfile",
            "spec_version": SPEC_VERSION,
            "sections": sections,
        }

    if name == "Intentfile.a2ml":
        future = {}
        for section in REQUIRED[name]["sections"]:
            items = parsed["bullets"].get(section, [])
            future[section] = [item if isinstance(item, str) else list(item.values())[0] for item in items]
        return {
            "type": "intentfile",
            "spec_version": SPEC_VERSION,
            "future": future,
        }

    raise SystemExit(f"Unsupported file: {name}")


def main():
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="cmd", required=True)

    vcmd = sub.add_parser("validate")
    vcmd.add_argument("files", nargs="+")

    ecmd = sub.add_parser("emit")
    ecmd.add_argument("input")
    ecmd.add_argument("output")

    args = parser.parse_args()

    if args.cmd == "validate":
        all_errors = []
        for f in args.files:
            errors = validate(Path(f))
            for err in errors:
                all_errors.append(f"{Path(f).name}: {err}")
        if all_errors:
            for err in all_errors:
                print(err, file=sys.stderr)
            return 1
        return 0

    if args.cmd == "emit":
        output = emit(Path(args.input))
        Path(args.output).write_text(json.dumps(output, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        return 0

    return 1


if __name__ == "__main__":
    raise SystemExit(main())
