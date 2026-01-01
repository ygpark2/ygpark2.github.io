# Architecture Overview

## Context
- 정적 사이트/블로그: 콘텐츠는 Markdown/HTML 포스트와 템플릿 기반으로 Hakyll이 `_site/`를 생성.
- 서버: Snap이 `_site/`를 정적으로 서빙하며 RSS 리다이렉트만 수행.
- 데이터베이스/외부 API: 없음.
- 상태 관리: 없음 (정적 렌더링).

## Runtime Components
- **Hakyll 빌더 (ainsyl 실행파일)**: 템플릿/포스트를 읽어 `_site/`를 생성.
- **Snap 서버 (server 실행파일, 선택적)**: 로컬 혹은 배포 후 정적 파일 서빙 및 RSS 리다이렉트.
- **CLI 포스트 생성기 (post 실행파일)**: cmdargs 기반, 새 포스트 스캐폴드 생성.

## Build & Deploy Flow
1) `stack build` 또는 `stack run ainsyl`로 정적 사이트 빌드 → `_site/` 생성  
2) (옵션) `stack run server`로 `_site/` 서빙  
3) 배포: `_site/`를 GitHub Pages 등 정적 호스팅으로 업로드 (CI 스크립트는 미구현)

## Assets
- `assets/css`, `assets/js`(highlight.js 빌드 포함), `assets/font[s]`  
- highlight.js 소스/테스트는 벤더 자산이며, 빌드 결과만 사용 가능.

## Notes
- 아키텍처상 데이터 계층/서비스 통합이 없어 복잡도는 낮음.
- 추가 기능(예: 댓글, 검색, 다국어)을 넣으려면 외부 서비스 연동 또는 정적 빌드 확장이 필요.
