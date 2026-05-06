# S2P
Space 2 Place impact evaluation

## Overleaf paper setup

The main manuscript is not tracked directly in this repository. It lives in a
separate Overleaf Git repository cloned inside this workspace at:

```text
paper/
```

This creates a nested Git setup:

```text
/workspace/S2P/        # main S2P data/code/materials repository
/workspace/S2P/paper/  # separate Overleaf manuscript repository
```

The outer repository ignores `paper/` through `.gitignore`, so manuscript files
are not accidentally committed to the S2P data/code repository.

The Overleaf connection is not automatic. Changes move only through Git:

```text
Overleaf edits -> git pull inside paper/ -> local manuscript files
local manuscript edits -> git commit + git push inside paper/ -> Overleaf
```

If someone edits in Overleaf, run `git pull` from inside `paper/` before making
local manuscript edits. If manuscript files are edited locally, commit and push
from inside `paper/` before expecting the changes to appear in Overleaf.

Use the outer repository for project data, code, and notes:

```bash
cd /workspace/S2P
git status
```

Use the nested `paper/` repository for the Overleaf manuscript:

```bash
cd /workspace/S2P/paper
git pull
git status
git add <changed-files>
git commit -m "Revise manuscript text"
git push
```

Before editing the manuscript, pull inside `paper/` to get coauthor changes from
Overleaf. After editing, commit and push from inside `paper/` so Overleaf is
updated. Running `git pull` or `git push` from `/workspace/S2P` affects the main
S2P repository, not the Overleaf manuscript.

## Setup on a new machine

The two repositories must be cloned separately. Order matters: clone the outer
GitHub repository first, then clone Overleaf *inside* it at `paper/`.

```bash
cd ~/workspace
git clone https://github.com/bjvca/S2P.git
cd S2P
git clone https://git@git.overleaf.com/689c50cbe6633bb708937a0a paper
```

After cloning, verify both remotes are correct. This is the single most useful
sanity check — if either URL is wrong, every subsequent push goes to the wrong
place.

```bash
cd ~/workspace/S2P
git remote -v
# expected:
# origin  https://github.com/bjvca/S2P.git (fetch)
# origin  https://github.com/bjvca/S2P.git (push)

cd ~/workspace/S2P/paper
git remote -v
# expected:
# origin  https://git@git.overleaf.com/689c50cbe6633bb708937a0a (fetch)
# origin  https://git@git.overleaf.com/689c50cbe6633bb708937a0a (push)
```

If a remote URL is wrong, fix it with:

```bash
git remote set-url origin <correct-url>
```

## Common failure modes

- **Wrong remote URL.** `git push` from the outer repo to Overleaf, or from
  `paper/` to GitHub, will produce confusing "non-fast-forward" or
  "refusing to merge unrelated histories" errors. Always run `git remote -v`
  in the directory where you are running git commands before pushing or
  pulling. The two repositories share no history.
- **Working directory drift.** Running `git` commands chained after a `cd
  paper` (e.g. in shell agents) will operate on the Overleaf repo even when
  you think you are in the outer one. Prefer absolute paths and explicit
  `cd ~/workspace/S2P` or `cd ~/workspace/S2P/paper` at the start of any
  multi-step git workflow.
- **`paper/` accidentally committed to the outer repo.** The outer
  `.gitignore` contains `paper/` precisely to prevent this. Do not remove
  that line.
- **Local untracked analysis files lost.** The outer repo tracks
  `endline/`, `recommendations/`, `replication_package/`, `sampling/`,
  `listing/`, `PAP/`, etc. Manuscript work is *not* in the outer repo. Do
  not run `git clean -fdx` at the outer root without checking what would be
  removed; it will not affect tracked files but can wipe untracked work.

## Knowledge base (`literature/`)

The `literature/` folder is also gitignored from the outer repo. It contains a
local LLM-maintained knowledge base scaffolded with `/kb-init` and populated
with `/ingest`. See `literature/CLAUDE.md` for its layout and rules. It is not
synchronised across machines through this repository.
