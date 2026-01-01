---
stepsCompleted:
  - 1
  - 2
  - 3
  - 4
  - 9
  - 8
  - 7
  - 10
inputDocuments:
  - docs/index.md
  - docs/project-overview.md
  - docs/architecture.md
  - docs/source-tree-analysis.md
  - docs/development-guide.md
  - docs/component-inventory.md
  - docs/api-contracts.md
  - docs/data-models.md
documentCounts:
  briefs: 0
  research: 0
  brainstorming: 0
  projectDocs: 8
workflowType: 'prd'
lastStep: 10
project_name: 'ygpark2.github.io'
user_name: 'ygpark2.github.io'
date: '2025-12-13T00:22:39+09:00'
---

# Product Requirements Document - ygpark2.github.io

**Author:** ygpark2.github.io  
**Date:** 2025-12-13T00:22:39+09:00

## Executive Summary

블로그 포스트가 많아지며 Hakyll 전체 렌더 비용이 선형/초과로 증가해 빌드와 페이지 로드가 느려지고 있습니다. 포스트 메타/본문(일부)을 JSON으로 추출하고, 경량 SPA(Svelte 등)로 목록/태그/검색/정렬을 클라이언트 렌더하면 성능을 크게 개선할 수 있습니다. 기존 정적 HTML(SEO/og/meta)과 Snap 서빙은 유지하여 점진적으로 전환합니다.

### What Makes This Special

- 대량 포스트를 위한 경량 데이터 소스(JSON)로 빌드/렌더 모두 단순화
- 클라이언트 측 필터/검색/정렬로 체감 속도 향상
- SEO·기존 템플릿을 유지하며 점진적 SPA 기능 도입

## Project Classification

**Technical Type:** web_app  
**Domain:** general (content publishing)  
**Complexity:** low  
**Project Context:** Brownfield – 기존 Hakyll/Snap 위에 JSON+클라이언트 렌더 추가  
**Project Classification Notes:** Hakyll 정적 사이트 + Snap 서빙 기반. 목표는 포스트 메타/본문을 JSON으로 추출하고, 빠른 프론트엔드로 목록/검색/세부 페이지를 렌더링하는 구조로 확장.

## Success Criteria

### User Success
- 목록/태그/검색 페이지: 메타 JSON 로드 후 즉시 렌더, 전체 페이지 로드 < 1.5초
- 검색/필터: 새로고침 없이 즉시 결과 갱신

### Business Success
- SEO 유지: 기존 정적 HTML/og/meta 보존, CSR-only 금지
- 탐색 효율: 포스트 리스트/검색 이탈률 감소(추적 예정)

### Technical Success
- 전체 빌드 시간: 3분 이하
- 증분 빌드 시간: 1분 이하 (mtime 기반 변경 파일만 재생성 고려)
- 메타/본문 JSON 총량: 5MB 이하
- 번들: 초기 JS 번들 경량화(코드 스플릿, highlight 지연 로드)
- 캐시: 파일명 해시/버전, CDN 캐시 무효화 지원

### Measurable Outcomes
- 빌드: 전체 < 3분, 증분 < 1분
- JSON: 총량 ≤ 5MB
- 페이지: 최초 리스트 로드 < 1.5초, 검색/필터 즉시
- SEO: 기존 정적 HTML 유지(지표는 유지/개선 확인)

## Product Scope

### MVP - Minimum Viable Product
- Hakyll → 메타/요약 JSON 생성
- 홈/목록/태그/검색을 경량 SPA(Svelte 등)로 렌더
- 정적 HTML/SEO 유지, CDN+해시 적용

### Growth Features (Post-MVP)
- 검색어 하이라이트/정렬 옵션 확대
- Lazy highlight, 코드 스플릿
- 증분 빌드 스크립트화

### Vision (Future)
- 전면 경량화 파이프라인(정적+클라이언트 하이브리드)
- 필요 시 SSR/SSG 고려

## User Journeys

### Journey 1: 독자 – 빠른 탐색과 읽기
- 상황: 많은 포스트 중 필요한 글을 빨리 찾고 싶다.
- 흐름: 홈/목록 접속 → 메타 JSON 로드로 1.5초 이내 첫 화면 표시 → 태그/검색으로 즉시 필터링(리로드 없음) → 게시글 클릭 시 본문/코드 하이라이트 지연 로드
- 목표/감정: “바로 찾고 바로 읽는다”는 즉시성, 로딩 지연 최소.
- 실패/회복: 검색 무반응·느림 → 로컬 필터·캐시된 메타 사용, 필요 시 정적 HTML 폴백.

### Journey 2: 작성자 – 새 포스트 발행
- 상황: 새 글을 작성하고 빠르게 배포하고 싶다.
- 흐름: 로컬에서 Markdown 작성 → JSON 생성 포함 빌드(전체 3분↓, 변경분 1분↓) → 메타/요약 JSON에 새 글 반영, 검색/태그에 즉시 노출 → 정적 HTML도 생성해 SEO 보존
- 목표/감정: “빌드가 길지 않고, 검색·목록에 바로 뜬다.”
- 실패/회복: 빌드 실패/시간 초과 → 변경 감지 재시도, 마지막 성공본으로 폴백.

### Journey 3: 운영자 – 배포/모니터링
- 상황: 빌드·배포 파이프라인을 관리하며 성능 지표를 본다.
- 흐름: CI/스크립트 실행 → 빌드 타임/JSON 크기/번들 크기 로그 확인 → CDN 캐시 무효화(해시 파일명) 후 배포 → 헬스체크: 메타 JSON 크기 ≤5MB, 최초 로드 시간 모니터링
- 목표/감정: “지표가 목표 내에 있고 캐시 갱신이 확실하다.”
- 실패/회복: 캐시 안 바뀜 → 버전/해시 확인, 강제 무효화; 크기 초과 → 스키마 축소, 요약 길이 단축.

### Journey Requirements Summary
- 독자: 메타 JSON 1.5초 이내 로드, 클라이언트 필터/검색, 지연 하이라이트, 정적 HTML 폴백.
- 작성자: 전체 빌드 <3분, 증분 <1분, JSON/HTML 동시 생성, 빌드 실패 시 롤백.
- 운영자: 빌드/JSON/번들 크기 측정·로그, 캐시 무효화(해시), 헬스체크/모니터링 지표.

## Web App Specific Requirements

### Project-Type Overview
- 형태: SPA + 정적 HTML(SEO) 혼합
- 브라우저: 모던 브라우저만 지원 (구형 IE/레거시 제외)
- 실시간: 없음

### Technical Architecture Considerations
- SEO 필수 → 정적 HTML 유지, CSR-only 금지
- 접근성: WCAG AA 목표
- 성능: 인터랙션 응답 < 10ms, 최초 로드 메타 JSON < 1.5초(상위 정의 준수)

### Project-Type Requirements
- browser_matrix: 모던 브라우저만, 레거시 제외
- responsive_design: SPA 템플릿/컴포넌트 반응형 유지
- performance_targets: 인터랙션 < 10ms, 최초 화면 < 1.5초, 빌드/JSON 목표 상위 섹션 준수
- seo_strategy: 정적 HTML/og/meta 유지, CSR-only 금지
- accessibility_level: WCAG AA

### Implementation Considerations
- SPA 라우팅은 SEO 영향 없도록 정적 페이지/프리렌더 유지
- JS 번들 경량화 및 코드 스플릿, 지연 로드
- 접근성: 키보드 내비게이션, 명확한 콘트라스트, ARIA 준수

## Functional Requirements

### Content Discovery
- FR1: 독자는 제목·태그·본문 전체를 검색할 수 있다(리로드 없이 결과 업데이트).
- FR2: 독자는 최신순/제목/태그/인기 기준으로 정렬할 수 있다.
- FR3: 독자는 태그/검색 필터를 조합하여 목록을 좁힐 수 있다.

### Content Viewing
- FR4: 독자는 정적 HTML로 본문을 열람할 수 있다(기본 경로).
- FR5: 본문 정적 HTML이 불가할 경우, JSON을 불러와 클라이언트 렌더로 대체할 수 있다.
- FR6: 코드 하이라이트는 지연 로드 방식으로 제공된다.

### Build & Publish
- FR7: 작성자는 빌드 시 메타/요약 JSON과 정적 HTML을 동시에 생성할 수 있다.
- FR8: 운영자는 파일 변경(mtime) 기준으로 증분 빌드를 실행할 수 있다.
- FR9: 빌드 실패 시 마지막 성공본으로 롤백할 수 있다.

### Operations & Monitoring
- FR10: 운영자는 빌드 시간, JSON 크기, 번들 크기를 확인할 수 있다.
- FR11: 운영자는 해시 파일명과 CDN 무효화로 캐시를 갱신할 수 있다.
- FR12: 운영자는 최초 로드 시간과 LCP를 수집/모니터링할 수 있다.

### Accessibility & SEO
- FR13: 서비스는 WCAG AA 기준을 충족하는 접근성을 제공한다.
- FR14: 서비스는 SEO를 위해 정적 HTML 및 og/meta 태그를 유지하며 CSR-only를 허용하지 않는다.

## Non-Functional Requirements

### Performance
- 빌드: 전체 < 3분, 증분 < 1분
- 데이터: 메타/본문 JSON 총량 ≤ 5MB
- 페이지: 최초 리스트 로드 < 1.5초, 인터랙션 < 10ms
- 모니터링: 최초 로드 시간, LCP를 수집·모니터링

### Security
- 정적 사이트 수준 보안: HTTPS 사용, 정적 자산 무결성/캐시 무효화 준수
- 민감 데이터/결제/계정 관리 없음(현재 범위)

### Accessibility
- WCAG AA 목표 유지 (키보드 내비게이션, 명확한 콘트라스트, ARIA 준수)

### SEO
- 정적 HTML 및 og/meta 유지, CSR-only 금지

### Scalability/Integration
- 특별한 확장성·외부 통합 요구 없음(현 범위에서 생략)

## Project Scoping & Phased Development

### MVP Strategy & Philosophy
- MVP Approach: Problem-Solving — 빌드/로드 성능 문제를 최소 기능으로 해결하는 데 집중 (ADR: B안을 Phase 1으로 확정; C안은 Vision에서 검토)

### MVP Feature Set (Phase 1)
- 독자: 메타 JSON 로드·검색·필터(리로드 없음), 본문 정적 HTML 폴백, 초기 로드 <1.5초, 인터랙션 <10ms
- 작성자: Hakyll 빌드 파이프라인(전체 <3분, 증분 <1분), 메타/요약 JSON + 정적 HTML 동시 생성, 빌드 실패 시 롤백
- 운영자: 빌드/JSON/번들 크기 로그 및 모니터링, 해시 기반 캐시 무효화, 간단한 헬스체크(메타 JSON ≤5MB, 최초 로드 시간 확인)
- 공통: SEO 유지(CSR-only 금지), WCAG AA, JS 번들 경량화/코드 스플릿/지연 로드

### Post-MVP Features (Phase 2)
- 검색 하이라이트/정렬 확장
- 증분 빌드 자동화(변경 감지, 스크립트화)
- 번들/자산 최적화 고도화(캐시 전략 고도화 포함)

### Vision (Future)
- 정적+클라이언트 하이브리드 파이프라인 완성
- 필요 시 SSR/SSG 검토 및 적용

### Risk Mitigation Strategy
- 기술: 빌드 타임/JSON/번들 목표 초과 시 → 스키마 축소(요약 길이 제한), 불필요 자산 제거, 증분 빌드 강화
- 리소스: 작은 팀일 경우 → Phase 1에서 목록/검색/빌드 파이프라인만 우선, 하이라이트/정렬·자동화는 Phase 2로 유지
- 시장/사용성: 성능 체감 확인 위해 → 최초 로드/LCP·검색 응답 측정, 개선 없을 시 JSON 크기/번들 추가 최적화
- ADR 메모: 선택지 A(Hakyll 최적화), B(JSON+경량 SPA, SEO 유지, 권장), C(완전 SPA/SSR, Vision에서 검토); 캐시/배포는 해시 파일명+CDN 무효화, CSR-only 금지; 빌드 실패 시 마지막 성공본 롤백, 목표 미달 시 스키마 축소·자산 정리 후 재시도
