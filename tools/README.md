# API sync tools

These scripts keep threadleR in step with the Threadle backend. They are
development tools only and are excluded from the package build (`.Rbuildignore`).

| File | What it is | Maintained by |
|---|---|---|
| `threadle-api.json` | Snapshot of the backend's commands and parameters | generated |
| `threadle-mapping.yaml` | What the code cannot tell: commands left unwrapped on purpose, planned wrappers | by hand |
| `check_api.R` | Compares the R wrappers with the snapshot | — |
| `update_threadle_api.py`, `parse_threadle_help.py` | Regenerate the snapshot from a running backend | — |
| `api-coverage.md` | Coverage table, last generated report | generated |

## When the backend changes

```bash
# 1. Regenerate the snapshot (uses `threadle` on PATH, or pass --threadle "dotnet path/to/threadle.dll")
python3 tools/update_threadle_api.py

# 2. See what changed in the backend
git diff tools/threadle-api.json

# 3. Check the R wrappers against it, and refresh the coverage table
Rscript tools/check_api.R --report tools/api-coverage.md
```

`check_api.R` exits with status 1 on errors, so it also runs in CI
(`.github/workflows/api-check.yaml`) against the committed snapshot.

## What is checked

The mapping between R functions and backend commands is read from the code:
a wrapper's command comes from `cmd <- "..."` or `.th_call(cmd = "...")`, and its
arguments are sent under their R names by `.th_args()`, minus any `drop =` and
`args$x <- NULL`, plus any `args$x <- value`.

- **Errors** (break calls): a wrapper sends a command the backend does not have,
  sends an argument the backend does not know, or never sends a required argument;
  `threadle-mapping.yaml` lists a command the backend does not have.
- **Warnings** (may mislead users): different defaults or allowed values, an argument
  optional in the backend but required in R, a backend command with no wrapper that is
  not listed in `threadle-mapping.yaml`.
- **Notes**: R supplies a default for an argument the backend requires. Usually a
  deliberate convenience; worth stating in the R documentation.

The snapshot only knows what the backend's `help` output says. The backend's own
consistency between its help text and its code is checked in the Threadle repository.
