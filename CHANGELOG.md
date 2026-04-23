# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

The long lines are not hard-wrapped.  Use softwrapping in your editor for reading.

## [Unreleased]

### Added
- Add `arei-logging` module for nREPL message logging.
- Add `arei-testing` module with test runner, progress reporting via minibuffer, interactive filtering, `arei-testing-load-project-tests`, and `arei-testing-load-module-tests` commands.
- Add `global-arei-mode` to activate `arei-mode` in existing and new `scheme-mode` buffers; arei now activates only when the user explicitly turns it on.
- Add `arei-evaluate-sexp-and-print-to-comment` command (bound to `C-p` in `arei-evaluation-map`) that inserts evaluation results as `;; => value` comments, with support for multi-line values, datum comments, and proper indentation alignment.
- Show a `⤵` symbol in overlay for eval-and-print-to-comment instead of the value itself.
- Add option to apply ANSI colors to evaluation output (`arei-client-ansi-out`, defined with `defcustom`).
- Add `.gitignore` file.

### Changed
- Move all lisp files into `lisp/` subdirectory.
- Swap `n`/`p` with `M-n`/`M-p` in the debugger for easier access to the most frequent functionality. - Rename `arei-evaluation-keymap` to `arei-evaluation-map` for consistency with other keymap variable names.
- Quote all submaps inside `arei-mode-map`.
- Use `ares.exception` field instead of `err`.
- Use guix time-machine to build HTML README in Makefile.

### Fixed
- Handle `\r` (carriage return) in stdout output in `arei-evaluation`.
- Do not handle `\r` as a new line in `arei-nrepl`.
- Restore buffer-local value of `sesman-system` when disabling `arei-mode`.
- Fix compiler warnings.
- Don't set undefined `arei--last-error` variable.
- Make sure request is cleaned up, even if there is an error in callback.
- Remove prototype's stale functions.
- Correct `global-arei-mode` configuration in README.
- Ignore `emacs-arei-autoloads.el` and `emacs-arei-pkg.el` anywhere.

## [0.9.6] - 2025-07-28

## [0.9.5] - 2024-05-27

## [0.9.4] - 2024-04-30

## [0.9.3] - 2024-03-14

## [0.9.2] - 2024-01-31

## [0.9.1] - 2023-12-04

## [0.9.0] - 2023-12-04

[Unreleased]: https://git.sr.ht/~abcdw/emacs-arei
[0.9.6]: https://git.sr.ht/~abcdw/emacs-arei/refs/0.9.6
[0.9.5]: https://git.sr.ht/~abcdw/emacs-arei/refs/0.9.5
[0.9.4]: https://git.sr.ht/~abcdw/emacs-arei/refs/0.9.4
[0.9.3]: https://git.sr.ht/~abcdw/emacs-arei/refs/0.9.3
[0.9.2]: https://git.sr.ht/~abcdw/emacs-arei/refs/0.9.2
[0.9.1]: https://git.sr.ht/~abcdw/emacs-arei/refs/0.9.1
[0.9.0]: https://git.sr.ht/~abcdw/emacs-arei/refs/0.9.0
