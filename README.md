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
