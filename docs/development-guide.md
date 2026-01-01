# Development Guide

## Prerequisites
- GHC via Stack (resolver: lts-7.24)
- Cabal metadata: `ainsyl.cabal`

## Build
```bash
# Build all executables
stack build

# Generate static site (Hakyll)
stack run ainsyl
```
- 출력물: `_site/`
- 추가 생성물:
  - `_site/assets/data/posts-index.json`: 게시글 메타/요약 JSON(크기 예산 5MB, 빌드 시 `log/posts-json-metrics.txt`에 기록)
  - `_site/posts/**.json`: 각 포스트의 본문 HTML/메타 JSON (정적 HTML 실패 시 클라이언트 렌더 폴백용)
  - 로그: `log/posts-json-metrics.txt` (posts-index.json 크기·포스트 수 기록, 5MB 초과 시 경고 출력)
- 폴백 렌더: 포스트 페이지는 정적 HTML이 비어 있을 경우 `post-fallback.js`가 `$url$index.json`을 불러와 본문을 렌더링합니다.

### Incremental Build (mtime)
```bash
chmod +x scripts/incremental-build.sh
FULL=0 scripts/incremental-build.sh   # mtime 기준 변경 시에만 빌드
FULL=1 scripts/incremental-build.sh   # 강제 전체 빌드
```
- 기준 시각: `log/last-build.timestamp`
- 변경 목록/결과 로그: `log/incremental-build.log`

## Serve (옵션)
```bash
stack run server
# serves ./_site, redirects /rss to feed.rss
```

## Create a new post (옵션)
```bash
stack run post -- --title "제목" --file my-post
# 생성 경로: posts/YYYY/MM/DD/my-post.md
```

## Testing
- 별도 테스트 스위트/CI 설정은 없음.

## Deployment
- CI/배포 스크립트 없음. `_site/`를 GitHub Pages 등 정적 호스팅에 업로드 필요.
