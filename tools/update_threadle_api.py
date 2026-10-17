"""Regenerate tools/threadle-api.json from a running Threadle backend.

Asks the backend for `about` and `help` over its JSON interface, then parses each
command's Syntax string into structured parameters. The command name is taken from
the backend's command registry (the name users actually type), not from the Syntax text.

Usage:
    python3 tools/update_threadle_api.py                       # uses `threadle` on PATH
    python3 tools/update_threadle_api.py --threadle "dotnet path/to/threadle.dll"
"""
import argparse
import json
import re
import shlex
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from parse_threadle_help import parse_param, split_top_level  # noqa: E402

SYNTAX = re.compile(r"^(?:\s*\[([^\]]+)\]\s*=\s*)?([a-zA-Z_]\w*)\s*\((.*)\)\s*$", re.S)


def query_backend(threadle_cmd):
    requests = '{"command":"about"}\n{"command":"help"}\n'
    out = subprocess.run(shlex.split(threadle_cmd) + ["--json", "--silent"], input=requests,
                         capture_output=True, text=True, check=True).stdout
    about, help_ = [json.loads(line) for line in out.splitlines() if line.startswith("{")][:2]
    return about["Payload"], help_["Payload"]


def parse_command(entry):
    issues = []
    m = SYNTAX.match(entry["Syntax"])
    if not m:
        return {"name": entry["Command"], "syntax": entry["Syntax"], "params": [],
                "description": entry["Description"]}, ["unparseable syntax"]
    returns, syntax_name, args = m.groups()
    if syntax_name != entry["Command"]:
        issues.append(f"syntax names the command '{syntax_name}', registered as '{entry['Command']}'")
    params = []
    for token in split_top_level(args):
        if re.search(r"\]\s*\|\s*\w+\s*=", token):  # a = [..] | b = [..]
            alts = [parse_param(t.strip(), issues.append) for t in token.split("|")]
            params.append({"one_of": alts, "required": alts[0]["required"]})
        else:
            params.append(parse_param(token, issues.append))
    return {
        "name": entry["Command"],
        "returns": returns,
        "to_assign": returns is not None,
        "params": params,
        "syntax": entry["Syntax"],
        "description": entry["Description"],
    }, issues


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--threadle", default="threadle", help="command that starts the Threadle CLI")
    ap.add_argument("--out", default=str(Path(__file__).resolve().parent / "threadle-api.json"))
    opts = ap.parse_args()

    about, help_entries = query_backend(opts.threadle)
    commands, report = [], {}
    for entry in sorted(help_entries, key=lambda e: e["Command"]):
        cmd, issues = parse_command(entry)
        commands.append(cmd)
        if issues:
            report[cmd["name"]] = issues

    schema = {"threadle_version": about["CliVersion"], "core_version": about["CoreVersion"],
              "commands": commands}
    with open(opts.out, "w", encoding="utf-8") as f:
        json.dump(schema, f, indent=2, ensure_ascii=False)
        f.write("\n")

    print(f"Threadle {about['CliVersion']}: {len(commands)} commands -> {opts.out}")
    for name, issues in report.items():
        for msg in issues:
            print(f"  syntax issue in {name}: {msg}")


if __name__ == "__main__":
    main()
