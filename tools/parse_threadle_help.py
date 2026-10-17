"""Convert Threadle's help dump (commands.tsv) into a JSON API schema.

Input columns: returns (assigned type, empty if none) | command | syntax | description
Usage: python parse_threadle_help.py commands.tsv threadle-api.json
"""
import csv
import json
import re
import sys


def split_top_level(s, sep=","):
    """Split on `sep` only when outside [], (), and quotes."""
    parts, buf, depth, quote = [], "", 0, None
    for ch in s:
        if quote:
            buf += ch
            if ch == quote:
                quote = None
            continue
        if ch in "'\"":
            quote = ch
        elif ch in "[(":
            depth += 1
        elif ch in "])":
            depth = max(depth - 1, 0)
        elif ch == sep and depth == 0:
            parts.append(buf.strip())
            buf = ""
            continue
        buf += ch
    if buf.strip():
        parts.append(buf.strip())
    return parts


def coerce(v):
    v = v.strip().strip("'\"")
    if v in ("true", "false"):
        return v == "true"
    for t in (int, float):
        try:
            return t(v)
        except ValueError:
            pass
    return v


def parse_spec(spec, warn):
    """Parse the part after `name =`, e.g. [int(default:0)] or ['a','b'(default)]."""
    p = {}
    spec = spec.strip()
    if spec.startswith('"') and spec.endswith('"'):
        p["quoted"] = True
        spec = spec[1:-1]
    if not spec.startswith("["):
        warn(f"spec does not start with '[': {spec!r}")
        return {"type": "unknown", "raw": spec}
    if not spec.endswith("]"):
        trailing = spec[spec.rfind("]") + 1:] if "]" in spec else ""
        warn(f"spec not closed with ']' (trailing {trailing!r}): {spec!r}")
        spec = spec[: spec.rfind("]") + 1] if trailing else spec + "]"
    inner = spec[1:-1]
    if inner.count("(") != inner.count(")"):
        warn(f"unbalanced parentheses: {spec!r}")

    items = split_top_level(inner)
    is_enum = any(re.match(r"^'[^']*'|^(true|false)\b", it) for it in items)
    if is_enum:
        p["type"] = "enum"
        p["choices"] = []
        for it in items:
            m = re.match(r"^(\w+)\((default[:=]?([^)]*)|[^)]*)\)$", it)
            if m:  # typed alternative inside enum, e.g. float(default:1)
                p.setdefault("also_accepts", []).append(m.group(1))
                if m.group(3):
                    p["default"] = coerce(m.group(3))
                continue
            m = re.match(r"^'?([^'(]*)'?(?:\(([^)]*)\))?$", it)
            if not m:
                warn(f"unparsed enum item {it!r}")
                continue
            val, mark = m.groups()
            p["choices"].append(coerce(val) if val in ("true", "false") else val)
            if mark == "default":
                p["default"] = p["choices"][-1]
            elif mark is not None:
                warn(f"non-standard default marker ({mark}) on {it!r}")
                p["default"] = p["choices"][-1]
        return p

    alts = [a.strip() for a in inner.split("|")]
    types = []
    for a in alts:
        m = re.match(r"^([\w:\- ]+?)\s*(?:\((?:default\s*([:=])\s*)?(.*?)\)?)?$", a)
        if not m:
            warn(f"unparsed type {a!r}")
            types.append(a)
            continue
        t, sep, d = m.groups()
        types.append(t)
        if d is not None and "(" in a:
            if sep is None:
                warn(f"default without 'default:' marker: {a!r}")
            elif sep == "=":
                warn(f"'default=' instead of 'default:': {a!r}")
            p.setdefault("default", coerce(d))
    p["type"] = types[0] if len(types) == 1 else types
    if p["type"] == "str" and len(items) > 1:  # e.g. [str,'~','~documents']
        p["type"] = "str"
        p["examples"] = [coerce(x) for x in items[1:]]
    return p


def parse_param(token, warn):
    prefix = ""
    if token[:1] in "*+":
        prefix, token = token[0], token[1:].strip()
    m = re.match(r"^(\w+)\s*=\s*(.*)$", token, re.S)
    name, spec = (m.groups() if m else (None, token))
    p = {"name": name}
    p.update(parse_spec(spec, warn))
    p["required"] = prefix == ""
    if prefix == "+":
        p["conditional"] = True  # Threadle marks some params with '+'; meaning to confirm
    return p


def parse_row(row):
    returns, name, syntax, description = (row + [""] * 4)[:4]
    issues = []
    warn = lambda msg: issues.append(msg)
    params = []
    for token in split_top_level(syntax):
        if re.search(r"\]\s*\|\s*\w+\s*=", token):  # a = [..] | b = [..]
            warn(f"alternative parameters: {token!r}")
            alts = [parse_param(t.strip(), warn) for t in token.split("|")]
            params.append({"one_of": alts, "required": alts[0]["required"]})
        else:
            params.append(parse_param(token, warn))
    cmd = {
        "name": name,
        "returns": returns.strip("[]") or None,
        "to_assign": bool(returns),
        "params": params,
        "description": description,
    }
    return cmd, issues


def main(src, dst):
    with open(src, newline="", encoding="utf-8-sig") as f:
        rows = list(csv.reader(f, delimiter="\t", quoting=csv.QUOTE_NONE))
    commands, report = [], {}
    for row in rows:
        cmd, issues = parse_row(row)
        commands.append(cmd)
        if issues:
            report[cmd["name"]] = issues
    with open(dst, "w", encoding="utf-8") as f:
        json.dump({"commands": commands}, f, indent=2, ensure_ascii=False)
    print(f"{len(commands)} commands -> {dst}")
    print(f"{len(report)} commands with syntax issues:")
    for name, issues in report.items():
        for msg in issues:
            print(f"  {name}: {msg}")


if __name__ == "__main__":
    main(*sys.argv[1:3])
