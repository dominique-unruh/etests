#!/usr/bin/env python3
"""Sync helper for the etests project repo and the exam repos checked out under exams/.

Default run (no args):
    For the project repo and every git repo that is an immediate subdir of exams/,
    fetch (without merging), then report and interactively offer to:
      * commit  (if there are uncommitted changes)  -> opens `git gui`
      * push    (if there are unpushed commits)
      * merge   (if origin has commits not merged in) -> ff-only or real merge;
                a conflicting real merge opens `git gui`

    y/n prompts are answered with a single keypress (no Enter).

With the `clone` argument:
    python scripts/sync.py clone
        Presents a checkbox list of the exams from exams/exams.yaml and clones the
        selected ones into exams/<key>/ (checked out at the configured `revision`).

Requires: GitPython, PyYAML. TUI uses only the standard library.
"""
from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path

try:
    import git  # GitPython
    from git import Repo, GitCommandError
except ImportError:
    sys.exit("This script needs GitPython. Install it with:  pip install GitPython")

try:
    import yaml
except ImportError:
    sys.exit("This script needs PyYAML. Install it with:  pip install PyYAML")


# --------------------------------------------------------------------------- paths

PROJECT_DIR = Path(__file__).resolve().parent.parent
EXAMS_DIR = PROJECT_DIR / "exams"
EXAMS_YAML = EXAMS_DIR / "exams.yaml"


# ------------------------------------------------------------------------ terminal

class C:
    """ANSI colours (disabled when stdout is not a tty)."""
    _on = sys.stdout.isatty()
    RESET = "\033[0m" if _on else ""
    BOLD = "\033[1m" if _on else ""
    DIM = "\033[2m" if _on else ""
    RED = "\033[31m" if _on else ""
    GREEN = "\033[32m" if _on else ""
    YELLOW = "\033[33m" if _on else ""
    BLUE = "\033[34m" if _on else ""
    CYAN = "\033[36m" if _on else ""


def read_key() -> str:
    """Read a single keypress without waiting for Enter (POSIX raw mode)."""
    if not sys.stdin.isatty():
        line = sys.stdin.readline()
        return line.strip()[:1]
    import termios
    import tty
    fd = sys.stdin.fileno()
    old = termios.tcgetattr(fd)
    try:
        tty.setraw(fd)
        ch = sys.stdin.read(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old)
    if ch == "\x03":  # Ctrl-C
        raise KeyboardInterrupt
    return ch


def ask_yn(prompt: str, default: bool | None = None) -> bool:
    """Single-keypress y/n prompt. Returns True for yes."""
    hint = "[y/n]" if default is None else ("[Y/n]" if default else "[y/N]")
    sys.stdout.write(f"{prompt} {hint} ")
    sys.stdout.flush()
    while True:
        ch = read_key()
        if ch in ("y", "Y"):
            print("y")
            return True
        if ch in ("n", "N"):
            print("n")
            return False
        if ch in ("\r", "\n") and default is not None:
            print("y" if default else "n")
            return default


def checkbox(title: str, items: list[tuple[str, bool]]) -> list[int]:
    """curses checkbox multi-select.

    `items` is a list of (label, selectable). Non-selectable rows are shown
    (dimmed, without a checkbox) but cannot be toggled or navigated onto.
    Returns indices of selected items ([] if cancelled or nothing selectable).
    """
    import curses

    selectable = [sel for _, sel in items]
    if not any(selectable):
        return []

    def _first() -> int:
        return next(i for i, s in enumerate(selectable) if s)

    def _move(pos: int, step: int) -> int:
        n = len(items)
        for _ in range(n):
            pos = (pos + step) % n
            if selectable[pos]:
                return pos
        return pos

    def _run(stdscr):
        curses.curs_set(0)
        pos = _first()
        checked = [False] * len(items)
        help_line = "↑/↓ move · space toggle · a all · enter confirm · q cancel"
        while True:
            stdscr.erase()
            stdscr.addstr(0, 0, title, curses.A_BOLD)
            for i, (label, sel) in enumerate(items):
                if sel:
                    mark = "[x]" if checked[i] else "[ ]"
                    attr = curses.A_REVERSE if i == pos else curses.A_NORMAL
                    stdscr.addstr(2 + i, 2, f"{mark} {label}", attr)
                else:
                    stdscr.addstr(2 + i, 2, f"    {label}", curses.A_DIM)
            stdscr.addstr(3 + len(items), 0, help_line, curses.A_DIM)
            stdscr.refresh()
            k = stdscr.getch()
            if k in (curses.KEY_UP, ord("k")):
                pos = _move(pos, -1)
            elif k in (curses.KEY_DOWN, ord("j")):
                pos = _move(pos, +1)
            elif k == ord(" "):
                checked[pos] = not checked[pos]
            elif k == ord("a"):
                new = not all(checked[i] for i, s in enumerate(selectable) if s)
                for i, s in enumerate(selectable):
                    if s:
                        checked[i] = new
            elif k in (curses.KEY_ENTER, 10, 13):
                return [i for i, c in enumerate(checked) if c]
            elif k in (ord("q"), 27):
                return []

    if not items:
        return []
    return curses.wrapper(_run)


# --------------------------------------------------------------------------- status

class RepoStatus:
    def __init__(self, repo: Repo, label: str):
        self.repo = repo
        self.label = label
        self.branch: str | None = None
        self.detached = False
        self.dirty = False
        self.untracked: list[str] = []
        self.ahead = 0          # unpushed commits
        self.behind = 0         # unmerged origin commits
        self.upstream: str | None = None
        self.has_upstream = False
        self.fetch_error: str | None = None

    @property
    def uncommitted(self) -> bool:
        return self.dirty or bool(self.untracked)

    @property
    def ff_only(self) -> bool:
        """A pending merge is fast-forward iff we have no local-only commits."""
        return self.behind > 0 and self.ahead == 0


def fetch_and_inspect(repo: Repo, label: str) -> RepoStatus:
    st = RepoStatus(repo, label)

    # 1. fetch without merging
    if repo.remotes:
        try:
            repo.remotes.origin.fetch()
        except (GitCommandError, AttributeError) as e:
            st.fetch_error = str(e).splitlines()[0] if str(e) else "fetch failed"

    # branch / detached
    try:
        st.branch = repo.active_branch.name
    except TypeError:
        st.detached = True

    # uncommitted
    st.dirty = repo.is_dirty(untracked_files=False)
    st.untracked = repo.untracked_files

    # upstream resolution: tracking branch, else origin/<branch>
    upstream = None
    if not st.detached:
        tb = repo.active_branch.tracking_branch()
        if tb is not None:
            upstream = tb.name
            st.has_upstream = True
        else:
            candidate = f"origin/{st.branch}"
            if candidate in [r.name for r in repo.refs]:
                upstream = candidate
    st.upstream = upstream

    # ahead / behind
    if upstream is not None:
        st.ahead = sum(1 for _ in repo.iter_commits(f"{upstream}..HEAD"))
        st.behind = sum(1 for _ in repo.iter_commits(f"HEAD..{upstream}"))

    return st


def print_status(st: RepoStatus) -> None:
    head = st.branch if not st.detached else "(detached HEAD)"
    print(f"\n{C.BOLD}{C.CYAN}{st.label}{C.RESET}  {C.DIM}[{head}]{C.RESET}")
    if st.fetch_error:
        print(f"  {C.YELLOW}! fetch: {st.fetch_error}{C.RESET}")
    if st.uncommitted:
        bits = []
        if st.dirty:
            bits.append("modified")
        if st.untracked:
            bits.append(f"{len(st.untracked)} untracked")
        print(f"  {C.RED}● uncommitted changes{C.RESET} ({', '.join(bits)})")
    if st.upstream is None and not st.detached:
        print(f"  {C.YELLOW}● no upstream configured{C.RESET}")
    if st.ahead:
        print(f"  {C.YELLOW}● {st.ahead} unpushed commit(s){C.RESET}")
    if st.behind:
        kind = "fast-forward" if st.ff_only else "REAL merge (branches diverged)"
        print(f"  {C.BLUE}● {st.behind} unmerged origin commit(s){C.RESET} → {kind}")


# --------------------------------------------------------------------------- actions

def do_commit(st: RepoStatus) -> None:
    """Open git gui so the user can stage and enter a commit message."""
    print(f"  opening {C.BOLD}git gui{C.RESET} …")
    try:
        git.Git(st.repo.working_tree_dir).gui()
    except GitCommandError as e:
        print(f"  {C.RED}git gui failed: {str(e).splitlines()[0]}{C.RESET}")


def do_push(st: RepoStatus) -> None:
    try:
        if st.has_upstream:
            infos = st.repo.remotes.origin.push()
        else:
            # first push of this branch: set upstream
            infos = st.repo.remotes.origin.push(
                refspec=f"{st.branch}:{st.branch}", set_upstream=True
            )
        for info in infos:
            if info.flags & info.ERROR:
                print(f"  {C.RED}push rejected: {info.summary.strip()}{C.RESET}")
            else:
                print(f"  {C.GREEN}pushed: {info.summary.strip()}{C.RESET}")
    except GitCommandError as e:
        print(f"  {C.RED}push failed: {str(e).splitlines()[0]}{C.RESET}")


def do_merge(st: RepoStatus) -> None:
    g = st.repo.git
    try:
        if st.ff_only:
            g.merge("--ff-only", st.upstream)
            print(f"  {C.GREEN}fast-forwarded to {st.upstream}{C.RESET}")
        else:
            g.merge(st.upstream)
            print(f"  {C.GREEN}merged {st.upstream}{C.RESET}")
    except GitCommandError as e:
        if st.repo.index.unmerged_blobs():
            print(f"  {C.YELLOW}merge conflict — opening git gui to resolve …{C.RESET}")
            try:
                git.Git(st.repo.working_tree_dir).gui()
            except GitCommandError as ge:
                print(f"  {C.RED}git gui failed: {str(ge).splitlines()[0]}{C.RESET}")
        else:
            print(f"  {C.RED}merge failed: {str(e).splitlines()[0]}{C.RESET}")


def handle_repo(st: RepoStatus) -> None:
    """Interactively offer commit / push / merge for one repo."""
    if st.uncommitted and ask_yn("  commit changes?"):
        do_commit(st)
        # refresh after commit so a subsequent push offer is accurate
        st_new = fetch_and_inspect(st.repo, st.label)
        st.ahead, st.behind = st_new.ahead, st_new.behind
        st.dirty, st.untracked = st_new.dirty, st_new.untracked

    if st.ahead and ask_yn(f"  push {st.ahead} commit(s)?"):
        do_push(st)

    if st.behind:
        kind = "fast-forward" if st.ff_only else "real merge"
        if ask_yn(f"  merge {st.behind} origin commit(s) [{kind}]?"):
            do_merge(st)


# ----------------------------------------------------------------------- discovery

def discover_repos() -> list[tuple[Repo, str]]:
    repos: list[tuple[Repo, str]] = []
    try:
        repos.append((Repo(PROJECT_DIR), f"project ({PROJECT_DIR.name})"))
    except git.InvalidGitRepositoryError:
        print(f"{C.YELLOW}warning: {PROJECT_DIR} is not a git repo{C.RESET}")
    if EXAMS_DIR.is_dir():
        for sub in sorted(p for p in EXAMS_DIR.iterdir() if p.is_dir()):
            if (sub / ".git").exists():
                try:
                    repos.append((Repo(sub), f"exams/{sub.name}"))
                except git.InvalidGitRepositoryError:
                    pass
    return repos


# --------------------------------------------------------------------------- clone

def load_exams() -> dict:
    if not EXAMS_YAML.is_file():
        sys.exit(f"{EXAMS_YAML} not found")
    data = yaml.safe_load(EXAMS_YAML.read_text()) or {}
    exams = data.get("exams") or {}
    if not exams:
        sys.exit(f"no exams listed in {EXAMS_YAML}")
    return exams


def clone_flow() -> None:
    exams = load_exams()
    keys = list(exams.keys())
    items = []
    for k in keys:
        e = exams[k] or {}
        exists = (EXAMS_DIR / k / ".git").exists()
        suffix = "  (already cloned)" if exists else ""
        items.append((f"{k}  —  {e.get('name', '?')}{suffix}", not exists))

    chosen = checkbox("Select exam repos to clone into exams/:", items)
    if not chosen:
        print("nothing selected.")
        return

    for i in chosen:
        k = keys[i]
        e = exams[k] or {}
        url = e.get("repository")
        rev = e.get("revision")
        dest = EXAMS_DIR / k
        if not url:
            print(f"{C.RED}{k}: no repository url — skipped{C.RESET}")
            continue
        if dest.exists() and any(dest.iterdir()):
            print(f"{C.YELLOW}{k}: {dest} already exists — skipped{C.RESET}")
            continue
        print(f"\n{C.BOLD}cloning {k}{C.RESET} from {url} → {dest}")
        try:
            repo = Repo.clone_from(url, dest)
            if rev:
                repo.git.checkout(rev)
                print(f"  {C.GREEN}checked out {rev}{C.RESET}")
        except GitCommandError as ex:
            print(f"  {C.RED}clone failed: {str(ex).splitlines()[0]}{C.RESET}")


# ----------------------------------------------------------------------------- main

def sync_flow() -> None:
    repos = discover_repos()
    if not repos:
        print("no repositories found.")
        return
    print(f"{C.DIM}fetching {len(repos)} repo(s) …{C.RESET}")
    statuses = [fetch_and_inspect(repo, label) for repo, label in repos]
    # A repo is worth showing only if it needs attention: pending commit/push/merge,
    # a fetch problem, or a missing upstream. Up-to-date repos are shown nothing.
    def noteworthy(st: RepoStatus) -> bool:
        return bool(st.uncommitted or st.ahead or st.behind or st.fetch_error
                    or (st.upstream is None and not st.detached))
    shown = [st for st in statuses if noteworthy(st)]
    if not shown:
        print(f"{C.GREEN}all repos clean and in sync.{C.RESET}")
        return
    for st in shown:
        print_status(st)
    actionable = [st for st in shown if st.uncommitted or st.ahead or st.behind]
    if not actionable:
        return
    print()
    for st in actionable:
        print(f"{C.BOLD}{st.label}{C.RESET}")
        handle_repo(st)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("command", nargs="?", choices=["clone"],
                        help="'clone': pick exams from exams.yaml and clone them into exams/")
    args = parser.parse_args()
    try:
        if args.command == "clone":
            clone_flow()
        else:
            sync_flow()
    except KeyboardInterrupt:
        print("\naborted.")
        sys.exit(130)


if __name__ == "__main__":
    main()
