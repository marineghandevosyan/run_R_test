# Git Hooks for R Tests

This repository includes Git hooks to automatically run **R unit tests** before each commit.

## 📂 Hook Location

Hooks are stored inside the `.git-hooks` folder.

## ⚙️ Setup

To make Git use these hooks, run the following command from the project root:

```bash
git config core.hooksPath .git-hooks

