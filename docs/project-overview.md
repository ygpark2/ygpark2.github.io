# Project Overview

**프로젝트명**: ygpark2.github.io  
**유형**: 정적 웹사이트/블로그 (Hakyll)  
**리포지토리 형태**: 모노리스  
**주 언어**: Haskell  

## Executive Summary
- Hakyll로 정적 사이트를 생성하고 `_site/`를 Snap 서버로 서빙 가능.
- 데이터베이스나 API 라우트는 없음. 모든 콘텐츠는 Markdown/HTML 포스트와 템플릿 기반.
- 포스트 생성용 CLI(`Post.hs`)가 있으며 cmdargs로 제목/파일명을 받아 템플릿 글을 생성.
- 배포/CI 설정은 확인되지 않음. 수동 빌드/배포가 필요해 보임.

## Tech Stack
- **언어**: Haskell
- **빌드/패키징**: Stack(`stack.yaml`), Cabal(`ainsyl.cabal`)
- **정적 사이트**: Hakyll (템플릿, 포스트/태그/아카이브/피드/사이트맵 생성)
- **서버**: Snap (정적 `_site` 서빙, RSS 리다이렉트)
- **CLI**: cmdargs 기반 포스트 생성기

## Repository Structure (요약)
- `src/` — Hakyll 사이트 생성 로직, Snap 서버, 포스트 생성 CLI
- `templates/` — 레이아웃/리스트/포스트/아카이브/태그/지도/루트플래너 템플릿(프로드/개발 모드 분기)
- `posts/` — Markdown/HTML 포스트 컨텐츠
- `pages/` — 단독 정적 페이지들 (예: map, route planner)
- `assets/` — CSS/JS/폰트 등 정적 자산 (highlight.js 빌드 포함, 소스/테스트는 벤더)
- 루트 Markdown 페이지: `index.md`, `about.md`, `latest.md`, `projects.md`, `resume.md`, `404.md`, `robots.txt`

## Getting Started (개발/빌드)
1. Stack 환경 준비 (GHC lts-7.24).  
2. 정적 사이트 빌드: Hakyll 실행(예: `stack run ainsyl` 또는 cabal로 실행).  
3. 출력물 `_site/` 생성 확인.  
4. 로컬 서빙(옵션): `stack run server`로 Snap 서버 실행.  
5. 새 포스트 생성(옵션): `stack run post -- --title "제목" --file my-post` → `/posts/YYYY/MM/DD/my-post.md` 생성.

## Deployment
- 전용 배포 스크립트/CI 설정은 없음.  
- 정적 사이트 결과물 `_site/`를 GitHub Pages 등에 업로드하는 수동/커스텀 파이프라인 필요.

## Known Gaps / Risks
- 테스트/CI 미구현.  
- 빌드/배포 자동화 부재.  
- highlight.js 소스/테스트가 리포지토리에 포함되어 있어 저장소 부피 증가 가능(자산 정리 고려).

## Links
- 소스 트리: `docs/source-tree-analysis.md`
- 상태/스캔 리포트: `docs/project-scan-report.json`
