# Upstreaming Panther Packages to Guix

This document is the practical playbook for moving a package from this
channel (`px/packages/`) into upstream Guix (`gnu/packages/`). It
distills the relevant bits of
`/home/franz/git/guix/doc/contributing.texi` and the Guix Cookbook into
the steps that actually matter.

The canonical reference is the Guix manual; when in doubt, read
`info guix Contributing` or the `contributing.texi` source directly.

---

## TL;DR

1. Pick a package that's a good fit (free software, real upstream, no
   bundling problems, useful to a broader audience).
2. Port `px/packages/foo.scm` to the right file in `gnu/packages/` —
   adjust modules, drop `px-` prefixes, add/update the copyright header.
3. For Rust apps: re-import deps into `gnu/packages/rust-crates.scm`.
4. `guix lint`, `guix style`, `guix build` — clean.
5. One package per commit, ChangeLog-style message.
6. Push via AGit to `refs/for/master` on
   [codeberg.org/guix/guix](https://codeberg.org/guix/guix).

---

## 1. Is the package a good candidate?

Not every panther package belongs upstream. A package is a reasonable
upstream candidate when:

- It is **free software** under a license Guix accepts
  (`(guix licenses)` — `expat`, `asl2.0`, `gpl3+`, etc.).
  See *Software Freedom* in `contributing.texi`.
- The upstream project is **maintained and reachable** — Codeberg,
  GitHub, GitLab, sourcehut, crates.io, PyPI, an actual release tarball.
  Avoid packages that only live in panther because their source is
  unstable, vendored, or ours.
- It has **broader appeal** than just Panther/IOTA-internal work.
  IOTA-specific tooling stays in the channel.
- It does **not bundle** copies of libraries Guix already packages, or
  you are willing to unbundle them in a `snippet`.
- It **builds reproducibly** — `guix build --rounds=2 <pkg>` succeeds.

If any of those is shaky, the package stays in panther. That's fine —
the channel exists for exactly that reason.

---

## 2. Set up your Guix checkout

You should have `~/git/guix` already. First time:

```bash
cd ~/git/guix
guix shell --development guix
./bootstrap
./configure --localstatedir=/var
make -j$(nproc)
```

After that, `./pre-inst-env` runs Guix commands against the local
checkout:

```bash
./pre-inst-env guix build <package>
./pre-inst-env guix lint  <package>
./pre-inst-env guix style <package>
```

Set Git identity for this repo if it differs from `~/.gitconfig`:

```bash
git -C ~/git/guix config user.name  "Franz Geffke"
git -C ~/git/guix config user.email "franz.geffke@iota.org"
```

Guix installs a `commit-msg` hook during `make` that adds a `Change-Id`
trailer. Keep it. Do not strip it across rebases.

---

## 3. Copyright headers

Every `.scm` file in `gnu/packages/` starts with a standard GPLv3+
header followed by per-author copyright lines. Format:

```scheme
;;; Copyright © 2026 Franz Geffke <franz.geffke@iota.org>
```

Rules:

- **First time you edit a file**: add your line. Insert it in roughly
  chronological order (existing lines tend to be loosely sorted by year,
  then alphabetical within a year — match what's there).
- **Subsequent edits in a new year**: extend the year on your existing
  line, do not duplicate it. Two forms in use:
  - Comma list: `Copyright © 2025, 2026 Franz Geffke …`
  - Range: `Copyright © 2025-2026 Franz Geffke …`
  Pick whichever is already used; switch a comma list to a range once
  it gets long.
- **Subsequent edits in the same year**: nothing to do.
- The `©` is a real Unicode character (U+00A9), not `(C)`.
- Use the email address you want on the public record.

Example, before:

```scheme
;;; Copyright © 2024 Some Maintainer <them@example.org>
```

After your first edit:

```scheme
;;; Copyright © 2024 Some Maintainer <them@example.org>
;;; Copyright © 2026 Franz Geffke <franz.geffke@iota.org>
```

A year later, on a second edit:

```scheme
;;; Copyright © 2024 Some Maintainer <them@example.org>
;;; Copyright © 2026, 2027 Franz Geffke <franz.geffke@iota.org>
```

When adding a copyright line, it is conventional to include it in the
**same commit** as the actual change. If a new file is created
(unusual — most categories already exist), include the full GPLv3+
header block; copy it from a sibling file.

---

## 4. Port the package definition

### 4.1 Pick the destination file

Guix groups packages by domain, not by language (with a few exceptions
like `rust-apps.scm`, `python-xyz.scm`, `golang-xyz.scm`). For a new
package, **match the convention upstream already uses for similar
software**:

- Domain files: `terminals.scm`, `networking.scm`, `databases.scm`,
  `audio.scm`, `video.scm`, `image.scm`, `web.scm`, `text-editors.scm`,
  `version-control.scm`, etc.
- Language bins/libs: `python-xyz.scm`, `golang-xyz.scm`, `ruby.scm`,
  `haskell-xyz.scm`, `rust-apps.scm`.
- Special: `rust-crates.scm` (auto-managed), `rust-sources.scm` (manual
  full pkgs).

Read a few existing entries in the candidate file before adding yours
— the surrounding style sets expectations.

### 4.2 Translate panther → guix

Going from `px/packages/foo.scm` to `gnu/packages/<category>.scm`:

| Panther                                      | Guix upstream                                |
|----------------------------------------------|----------------------------------------------|
| `(define-module (px packages tools) …)`      | `(define-module (gnu packages tools) …)`     |
| `(use-modules (px packages rust-crates))`    | `(use-modules (gnu packages rust-crates))`   |
| `(px-cargo-inputs 'foo)`                     | `(cargo-inputs 'foo)`                        |
| Panther header                               | Standard Guix `;;; GNU Guix --- …` header    |
| Free-form layout                             | Result of `guix style`                       |

Other than the module/header swap and `px-` → upstream removal, the
package definition itself should not need structural changes.

### 4.3 Naming

From *Package Naming* in `contributing.texi`:

- Lowercase upstream project name, `_` → `-`.
- No `lib` prefix unless upstream already uses it.
- Single-character or ambiguous names get disambiguated (`s` →
  `s-shell`).
- Font packages — `font-<foundry>-<family>` (see *Fonts*).
- Python — `python-<name>` (use `pyproject-build-system`, not the
  legacy `python-build-system` for new packages).
- Perl — `perl-<lowercased-class-name>`, `::` → `-`.
- Java — `java-<name>`.
- **Rust apps** — plain name, no `rust-` prefix.
- **Rust libraries** — `rust-<name>-<major>` (e.g. `rust-clap-2`,
  `rust-rand-0.6`). Versioned suffix is the left-most non-zero digit.

### 4.4 Synopsis and description

These must be **literal strings** — no `string-append`, no `format` —
so `xgettext` can extract them for translation.

- **Synopsis**: starts with a capital, no trailing period, no leading
  "A" / "The", under 80 chars. Describes *what it is* or *what it's
  for*.
- **Description**: 5–10 lines, full sentences, factual. Avoid marketing
  language ("powerful", "next-generation", "world-leading"). Texinfo
  markup is allowed (`@code{...}`, `@dfn{...}`).

Our existing panther descriptions are usually close — they may need a
once-over to drop hype words and meet the length minimum.

### 4.5 Source

- Prefer release tarballs over generated archives.
- For GitHub: `url-fetch` of a release tarball when one exists.
  Otherwise `git-fetch` with a tagged commit — GitHub-generated
  `.tar.gz` archives are not guaranteed byte-stable.
- Use `mirror://` URLs where available (GNU, savannah, sourceforge).
- Do not embed the `name` field into the URL — if upstream renames, the
  URL won't follow automatically.
- Verify GPG signatures when upstream provides them, before locking the
  hash.

---

## 5. Build systems

The right `build-system` line and a couple of conventions per system:

### 5.1 Ordinary builds (the common case)

- **`gnu-build-system`** — autotools (`./configure && make && make
  install`). The default for most C/C++ projects. Tests run by default
  (`#:tests? #t`).
- **`cmake-build-system`** — CMake projects. Often needs
  `-DBUILD_SHARED_LIBS=ON` and `-DUSE_SYSTEM_*=ON` flags to unbundle.
- **`meson-build-system`** — Meson/Ninja. Use this for newer GNOME-ish
  projects.
- **`copy-build-system`** — pre-built or trivial install (rare for
  upstream, more common in panther).

Inputs:

- `native-inputs` — build-time only (`pkg-config`, compilers, code
  generators, test frameworks).
- `inputs` — runtime libraries the binary links against.
- `propagated-inputs` — anything users of *this* package also need
  installed (headers exposed by your headers, Python modules imported
  by yours).

### 5.2 Go

- **`go-build-system`** — single-binary Go apps.
- Library modules go into `golang-xyz.scm` named `go-<import-path>`
  (e.g. `go-github-com-spf13-cobra`). They are *real* packages, unlike
  Rust crates.
- Dependencies are listed by name as `inputs`; the build system handles
  the Go module dance.

### 5.3 Python

- **`pyproject-build-system`** for anything with `pyproject.toml`.
- Generate the initial definition with:

  ```bash
  ./pre-inst-env guix import pypi <name>
  ```

  Then clean up — the importer is a starting point, not the final
  product.

- Dependency mapping (per *Python Modules* in `contributing.texi`):
  - Runtime imports → `propagated-inputs`.
  - Build / test deps (e.g. `pytest`, `setuptools`, `wheel`) →
    `native-inputs`.
  - Native libraries (C deps for extensions) → `inputs`.
- `python-build-system` is deprecated for new packages — only use it
  for legacy setup.py projects with no `pyproject.toml`.

### 5.4 Rust

Rust applications get a separate workflow because library crates are
not packaged individually (see §6). The package itself looks like:

```scheme
(define-public foo
  (package
    (name "foo") (version "…") (source …)
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'foo))
    …))
```

Conventions:

- `#:install-source? #f` — applications don't ship sources.
- For Cargo workspaces, set `#:cargo-install-paths`.
- For tests that need network/timing leeway, skip with
  `#:cargo-test-flags '("--release" "--" "--skip=…")` rather than
  disabling all tests.
- If the upstream `Cargo.toml` pins git deps or vendors crates, unbundle
  in a `snippet` (see existing examples in `rust-apps.scm`). The Rust
  team won't merge silently bundled sources.

### 5.5 Other systems

`emacs-build-system`, `julia-build-system`, `ocaml-build-system`,
`haskell-build-system`, `r-build-system`, etc. Each has its own
conventions documented in `contributing.texi` under *Packaging
Guidelines* — read the relevant subsection before submitting.

---

## 6. Rust dependencies — `rust-crates.scm`

For Rust applications, the dependency tree is imported wholesale from
the upstream `Cargo.lock` into `gnu/packages/rust-crates.scm`. This is
**Rust-specific** — no equivalent step exists for other languages.

```bash
cd ~/git/guix

# Get the source somewhere with a Cargo.lock present
guix build --source <package>   # or clone the repo manually

# Import into rust-crates.scm
./pre-inst-env guix import -i gnu/packages/rust-crates.scm \
    crate -f /path/to/Cargo.lock <package-name>
```

This adds (or updates):

- `crate-source` definitions near the top of the file.
- A `[<package-name>]` entry inside `lookup-cargo-inputs`.

**Do not hand-edit `rust-crates.scm`** beyond what the importer
produces. The only structural maintenance permitted is moving
`crate-source` blocks above `lookup-cargo-inputs` if a previous import
left them out of order ("used before definition" errors).

A small number of crates need full package definitions (workspaces,
crates with non-Rust build inputs). Those go in
`gnu/packages/rust-sources.scm` with `#:skip-build? #t`. This is rare —
only reach for it when `rust-crates.scm` alone won't do.

---

## 7. Pre-submission checklist

For every package, before pushing:

```bash
cd ~/git/guix

# Format
./pre-inst-env guix style <package>

# Lint — fix everything it reports
./pre-inst-env guix lint <package>

# Build (plus at least one round of dependents)
./pre-inst-env guix build <package>
./pre-inst-env guix build --dependents=1 <package>

# Reproducibility
./pre-inst-env guix build --rounds=2 <package>

# Make sure guix pull against your checkout still works
guix pull --url=$PWD --profile=/tmp/guix.test --disable-authentication
```

For Rust packages, also confirm `cargo audit` is clean on the upstream
`Cargo.lock` and that `cargo license` shows nothing problematic.

---

## 8. Commit messages

Guix uses **ChangeLog format**. One package per commit. First line is a
short summary; the body lists exactly which top-level definitions
changed.

### Non-Rust: new package

```
gnu: Add foo.

* gnu/packages/<category>.scm (foo): New variable.

Change-Id: I…
```

### Non-Rust: update

```
gnu: foo: Update to 1.2.3.

* gnu/packages/<category>.scm (foo): Update to 1.2.3.

Change-Id: I…
```

### Non-Rust: fix or change with details

```
gnu: foo: Fix build with newer glib.

* gnu/packages/<category>.scm (foo)[arguments]: Pass
GLIB_DISABLE_DEPRECATION_WARNINGS via #:make-flags.
[inputs]: Add glib-utils.

Change-Id: I…
```

### Rust: new app

```
gnu: Add binsider.

* gnu/packages/rust-apps.scm (binsider): New variable.
* gnu/packages/rust-crates.scm (lookup-cargo-inputs)[binsider]:
New entry.

Change-Id: I…
```

### Rust: update

```
gnu: watchexec: Update to 2.3.2.

* gnu/packages/rust-apps.scm (watchexec): Update to 2.3.2.
* gnu/packages/rust-crates.scm (lookup-cargo-inputs)[watchexec]:
Update entry.

Change-Id: I…
```

### Rust: update touching rust-sources

```
gnu: typst: Update to 0.14.0.

* gnu/packages/rust-apps.scm (typst): Update to 0.14.0.
[arguments]<#:rust>: Use rust-1.88.
* gnu/packages/rust-crates.scm (lookup-cargo-inputs)[typst]:
Update entry.
[rust-syntect-5]: Rename to rust-syntect-5.2.
[rust-syntect-5.3]: New entry.
* gnu/packages/rust-sources.scm (rust-syntect-5): Rename to
rust-syntect-5.2.
(rust-syntect-5.3): New variable.

Change-Id: I…
```

Notes:

- **No** `Co-Authored-By`, no AI attribution, no "Generated by …".
  Match the existing log style.
- Keep the `Change-Id` trailer — the hook adds it. Preserve it across
  rebases so reviewers can match revisions of the same PR.
- `git log gnu/packages/<file>.scm` is the best reference for tone in a
  given area.

---

## 9. Submitting the pull request

Guix moved off email patches on 2026-01-01. Contributions go through
[codeberg.org/guix/guix](https://codeberg.org/guix/guix). Two ways to do
it; the **AGit workflow** is preferred — no fork, no extra branch on
Codeberg.

### 9.1 AGit via `git push` (canonical)

This is the form documented in the Guix manual. From your local branch,
with the commits ready:

```bash
git push origin HEAD:refs/for/master \
  -o topic=<topic> \
  -o title="<title>" \
  -o description="<description>"
```

- `<topic>` — short identifier, like a branch name (e.g. `add-binsider`,
  `update-watchexec`). Reusing the same topic later updates the same
  PR.
- `<title>` / `<description>` — optional; if omitted, the last commit
  message is used.

To update an existing AGit PR after a rebase or fixup:

```bash
git push origin HEAD:refs/for/master \
  -o topic=<topic> -o force-push=yes
```

Codeberg matches the topic and updates the PR. Reviewer comments stay
attached to the same PR.

### 9.2 AGit via `forgejo-cli` (convenience)

The panther channel ships `forgejo-cli` (binary: `fj`). It wraps the
AGit push and adds review-cycle commands. Install it once:

```bash
guix install -L ~/git/panther forgejo-cli
```

Authenticate against Codeberg (one-time):

```bash
fj auth login --host codeberg.org
# follow the prompts; you'll need a Codeberg application token
```

From inside `~/git/guix`, with your commits on the current branch:

```bash
# Single-commit PR — title/body taken from the commit
fj pr create --agit --base master --autofill

# Or pass title/body explicitly
fj pr create --agit --base master \
  --title "gnu: Add binsider." \
  --body-file ./pr-body.md
```

`--autofill` matches Guix's "first line = ChangeLog summary" convention
nicely: a single commit becomes a PR with that title and body; multiple
commits get a branch-named title and a commit list as the body.

Useful follow-ups:

```bash
fj pr view <num>      # show PR contents
fj pr status <num>    # CI / mergeability
fj pr edit <num>      # update title / body
fj pr comment <num>   # reply to reviewers
fj pr browse <num>    # open in browser
```

Updating an existing PR — re-run `fj pr create --agit` with the same
topic, or push directly with `force-push=yes` as in §9.1. `fj` reuses
the branch name as the AGit topic by default.

### 9.3 Drafts / Work-in-progress

Two ways to mark a PR as not-ready-for-review:

- **Prefix the title with `WIP: `** — works everywhere, recognized by
  both Forgejo and `fj pr create` (it explicitly notes this in
  `--help`). Drop the prefix once the PR is ready.
- **Forgejo's draft toggle** — set after creation via the web UI or
  `fj pr edit`. Equivalent in effect; the `WIP:` form is more visible
  in the PR list.

Useful when you want CI to run but reviewers to hold off — e.g. while
confirming a build on a slower architecture.

### 9.4 Fork-based PR

If you prefer the conventional flow: fork `guix/guix` on Codeberg, push
your branch to the fork, open a PR against `master`. Standard
[Forgejo PR flow](https://docs.codeberg.org/collaborating/pull-requests-and-git-flow/).
Slightly heavier on disk but easier if you want to iterate locally with
your own branch namespace.

### 9.5 PR description

Reviewers like context. Cover:

- What the package is and why it's worth packaging.
- How you tested (`guix build`, `--rounds=2`, dependents, etc.).
- Anything non-obvious — unbundled deps, skipped tests, patched
  shebangs, license clarification.

Examples worth reading:

- [pulls/8491](https://codeberg.org/guix/guix/pulls/8491)
- [pulls/8327](https://codeberg.org/guix/guix/pulls/8327)

---

## 10. After submitting

- Watch the PR for CI status and reviewer comments. Rust changes are
  routed to the **Rust team** — expect a Rust team reviewer to look at
  any `rust-crates.scm` / `rust-sources.scm` touch.
- For merge conflicts in `rust-crates.scm`: **drop your changes to that
  file**, rebase, and re-run the lockfile importer. Don't try to merge
  the crate list by hand. See *Resolving merge conflicts in Pull
  Requests* in `contributing.texi`.
- Keep `Change-Id` stable so revisions stay grouped on the same PR.

Once merged, you can drop the panther-side definition (or keep it
pinned if you depend on a specific version) and note it in `CHANGELOG.md`
if relevant.

---

## 11. Quick reference

```bash
# Working in the Guix checkout
cd ~/git/guix
./pre-inst-env guix lint  <pkg>
./pre-inst-env guix style <pkg>
./pre-inst-env guix build <pkg>
./pre-inst-env guix build --dependents=1 <pkg>
./pre-inst-env guix build --rounds=2 <pkg>

# Import Python deps (starter, needs cleanup)
./pre-inst-env guix import pypi <pkg> > /tmp/pkg.scm

# Import Rust deps
./pre-inst-env guix import -i gnu/packages/rust-crates.scm \
    crate -f /path/to/Cargo.lock <pkg>

# Push via AGit (raw git)
git push origin HEAD:refs/for/master \
  -o topic=<topic> \
  -o title="<title>"

# Update an existing AGit PR
git push origin HEAD:refs/for/master \
  -o topic=<topic> -o force-push=yes

# Push via forgejo-cli
fj auth login --host codeberg.org           # one-time
fj pr create --agit --base master --autofill
fj pr view <num>
fj pr status <num>
```

### Where things live

- Guix packages: `~/git/guix/gnu/packages/*.scm`
- Rust crates index: `~/git/guix/gnu/packages/rust-crates.scm`
- Rust sources (full pkgs): `~/git/guix/gnu/packages/rust-sources.scm`
- Panther packages: `~/git/panther/px/packages/*.scm`
- Manual: `info guix Contributing` (source:
  `~/git/guix/doc/contributing.texi`)
- PRs: <https://codeberg.org/guix/guix/pulls>
