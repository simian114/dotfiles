# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Emacs Configuration Guidelines

### Execution Commands
- Configuration changes take effect when Emacs reloads the files
- No specific build/lint/test commands are required
- Use `M-x eval-buffer` to evaluate changes in Emacs

### Code Style
- Use 2-space indentation throughout all Elisp files
- Comments: `;;;` for section headers, `;;` for important comments, `;` for regular comments
- Use `use-package` for package management with `:ensure t` for auto-installation
- Configure packages with `:bind`, `:config`, `:hook` sections
- Use platform-specific code via `(eq system-type 'darwin)` checks
- Maintain modular structure with separate files (init.el, meow.el, ui.el)
- Follow Emacs Lisp naming conventions (hyphenated-names, not camelCase)
- Follow existing patterns for error handling (using condition-case)
- Keep configurations lightweight and well-documented