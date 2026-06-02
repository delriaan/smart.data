---
engines:
- path: /usr/share/positron/resources/app/quarto/share/extension-subtrees/julia-engine/\_extensions/julia-engine/julia-engine.js
toc-title: Table of contents
---

# smart.data Version 0.1.4

## 0.1.4

### Updates

- Updated several internal methods for coercion
- Object `.___SMART___` created on invocation of `smart.start()` will
  detach when the packge is unloaded.
- Added feature overview to `pkgdown` landing page.

### Bug fixes

- `smart.start()` properly checks for the existence of ".\_**SMART**\_"
  before attaching a new environment.

# smart.data Version 0.1.3

## 0.1.3.1

- Adding History marker
