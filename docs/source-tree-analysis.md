# Source Tree Analysis

project root: `/Users/ygpark2/pjt/ygpark2.github.io`

```
.
├── src/                      # Haskell code
│   ├── Site.hs               # Hakyll site generator rules (posts/tags/archive/sitemap/feed/templates)
│   ├── Server.hs             # Snap HTTP server, serves _site, redirects RSS
│   ├── FileServe.hs          # Static file serving helpers for Snap
│   ├── XmlHtmlWriter.hs      # XML/HTML writer utilities (used by Hakyll pipeline)
│   └── Post.hs               # CLI to scaffold new posts (cmdargs)
├── templates/                # Hakyll templates (prod/dev variants)
│   ├── *.html                # Layout, list, post, archive, tags, map, route-planner
│   └── parts/                # Partial templates (post card, list items)
├── posts/YYYY/MM/DD/*.md     # Content posts (Markdown/HTML)
├── pages/                    # Standalone static pages (map, route planner)
├── assets/
│   ├── css/                  # Stylesheets (site, syntax highlight)
│   ├── js/                   # JS assets (highlight.js build, d3, topojson, materialize)
│   ├── font[s]/              # Icon/Roboto fonts
│   └── img/ media/ ...       # Static assets (if present)
├── index.md, about.md, ...   # Site markdown pages
├── stack.yaml                # Stack configuration (lts-7.24)
├── ainsyl.cabal              # Cabal project (executables: ainsyl, post, server)
└── docs/                     # BMM workflow/status outputs (current documentation)
```

Key notes:
- No database or API routes; purely static site generation plus static file serving.
- Highlight.js vendor sources/tests are present under `assets/js/highlight.js` but treated as third-party assets.
- Build output `_site/` is produced by Hakyll (not committed by default).
