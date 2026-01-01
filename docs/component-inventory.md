# Component Inventory

## Overview
- UI는 Hakyll 템플릿 기반으로 구성되며 별도 JS 컴포넌트 라이브러리는 없습니다.
- 주요 템플릿:
  - `templates/default*.html`, `post*.html`, `list*.html`, `archive*.html`, `tags*.html`, `map*.html`, `route-planner*.html`
  - `templates/parts/_post-archive.html`, `_post-list-archive.html`
- 자산:
  - CSS: `assets/css/*`
  - JS: `assets/js` (highlight.js 빌드, d3, topojson, materialize 등 벤더 스크립트)
  - Fonts: `assets/font`, `assets/fonts`

## Notes
- 컴포넌트 설계/카탈로그는 별도로 정의되어 있지 않습니다.
