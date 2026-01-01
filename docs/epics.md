---
stepsCompleted:
  - 1
  - 2
  - 3
inputDocuments:
  - docs/prd.md
  - docs/architecture.md
  - docs/ux-design-specification.md
workflowType: 'create-epics-stories'
lastStep: 3
---

# ygpark2.github.io - Epic Breakdown

## Overview

This document provides the complete epic and story breakdown for ygpark2.github.io, decomposing the requirements from the PRD, UX Design if it exists, and Architecture requirements into implementable stories.

## Requirements Inventory

### Functional Requirements
FR1: 독자는 제목·태그·본문 전체를 검색할 수 있다(리로드 없이 결과 업데이트).
FR2: 독자는 최신순/제목/태그/인기 기준으로 정렬할 수 있다.
FR3: 독자는 태그/검색 필터를 조합하여 목록을 좁힐 수 있다.
FR4: 독자는 정적 HTML로 본문을 열람할 수 있다(기본 경로).
FR5: 본문 정적 HTML이 불가할 경우, JSON을 불러와 클라이언트 렌더로 대체할 수 있다.
FR6: 코드 하이라이트는 지연 로드 방식으로 제공된다.
FR7: 작성자는 빌드 시 메타/요약 JSON과 정적 HTML을 동시에 생성할 수 있다.
FR8: 운영자는 파일 변경(mtime) 기준으로 증분 빌드를 실행할 수 있다.
FR9: 빌드 실패 시 마지막 성공본으로 롤백할 수 있다.
FR10: 운영자는 빌드 시간, JSON 크기, 번들 크기를 확인할 수 있다.
FR11: 운영자는 해시 파일명과 CDN 무효화로 캐시를 갱신할 수 있다.
FR12: 운영자는 최초 로드 시간과 LCP를 수집/모니터링할 수 있다.
FR13: 서비스는 WCAG AA 기준을 충족하는 접근성을 제공한다.
FR14: 서비스는 SEO를 위해 정적 HTML 및 og/meta 태그를 유지하며 CSR-only를 허용하지 않는다.

### NonFunctional Requirements
NFR1: 빌드 전체 < 3분, 증분 < 1분
NFR2: 메타/본문 JSON 총량 ≤ 5MB
NFR3: 최초 리스트 로드 < 1.5초, 인터랙션 < 10ms
NFR4: 최초 로드 시간, LCP를 수집·모니터링
NFR5: 정적 사이트 수준 보안(HTTPS, 자산 무결성/캐시 무효화), 민감 데이터/결제/계정 없음
NFR6: WCAG AA 준수(포커스, 대비, 키보드 내비, ARIA)
NFR7: 정적 HTML/og/meta 유지, CSR-only 금지
NFR8: 확장성/외부 통합 특이 요구 없음(현 범위)

### Additional Requirements
- 아키텍처: Hakyll 빌더로 `_site` 생성, Snap 정적 서빙(선택), DB/API 없음, 벤더 자산 highlight.js는 빌드 결과만 사용
- 빌드/배포: stack build/run, 정적 호스팅 대상(GitHub Pages 등), 캐시 무효화는 해시 파일명 사용
- UX/디자인: Tailwind 테마블, Light 기본 + 옵션 Dark, Noto Sans CJK, 8px 스페이싱, 카드 중심 목록, 상단 고정 내비+검색/정렬 바+TOC, 반응형(데스크톱 12컬럼/모바일 단일 컬럼)
- 접근성/상태: 정렬·필터 상태/빈 결과/에러/로딩 명확히 표시, TOC/앵커 제공, 키보드 내비/포커스/ARIA 준수
- 폴백: 본문 정적 HTML 우선, 실패 시 JSON→클라이언트 렌더

### FR Coverage Map

FR1: Epic 1 - 검색/필터/정렬을 즉시 수행해 원하는 포스트를 찾는다
FR2: Epic 1 - 최신/제목/태그/인기 정렬로 목록을 재배열한다
FR3: Epic 1 - 태그/검색 필터 조합으로 목록을 좁힌다
FR4: Epic 2 - 정적 HTML로 본문을 열람한다
FR5: Epic 2 - 정적 HTML이 안 될 때 JSON 기반 렌더로 폴백한다
FR6: Epic 2 - 코드 하이라이트를 지연 로드해 가독성을 유지한다
FR7: Epic 3 - 빌드 시 메타/요약 JSON과 정적 HTML을 동시에 생성한다
FR8: Epic 3 - 파일 변경(mtime) 기반 증분 빌드를 실행한다
FR9: Epic 3 - 빌드 실패 시 마지막 성공본으로 롤백한다
FR10: Epic 4 - 빌드/JSON/번들 크기를 확인한다
FR11: Epic 4 - 해시 파일명과 CDN 무효화로 캐시를 갱신한다
FR12: Epic 4 - 최초 로드 시간과 LCP를 수집/모니터링한다
FR13: Epic 4 - WCAG AA 접근성을 제공한다
FR14: Epic 4 - SEO를 위해 정적 HTML/og/meta를 유지하고 CSR-only를 금지한다

## Epic List

### Epic 1: 빠른 콘텐츠 탐색
검색/필터/정렬을 즉시 반응시키고, 태그/검색 조합으로 원하는 포스트를 빠르게 찾는다.
**FRs covered:** FR1, FR2, FR3

### Epic 2: 끊김 없는 본문 읽기
선택한 포스트를 정적 HTML로 즉시 읽고, 필요 시 JSON 렌더로 폴백하며 코드 하이라이트를 지연 로드해 가독성을 유지한다.
**FRs covered:** FR4, FR5, FR6

### Epic 3: 안정적인 빌드·배포 파이프라인
메타/요약 JSON과 정적 HTML을 동시에 생성하고, 증분 빌드·롤백으로 안정적 배포를 보장한다.
**FRs covered:** FR7, FR8, FR9

### Epic 4: 운영·신뢰성·접근성·SEO
빌드/자산 지표를 모니터링하고 캐시를 제어하며, 접근성(WCAG AA)과 SEO(정적 HTML/og/meta)를 유지한다.
**FRs covered:** FR10, FR11, FR12, FR13, FR14

## Epic 1: 빠른 콘텐츠 탐색

### Story 1.1: 제목/태그/본문 통합 검색
As a 독자,
I want 제목·태그·본문 전체를 입력 즉시 필터링하고 빈 결과를 알려주고 싶다,
So that 리로드 없이 원하는 글을 바로 찾는다.

**Acceptance Criteria:**
**Given** 게시글 목록이 로드됨
**When** 사용자가 검색어를 입력하거나 지움
**Then** 제목·태그·본문을 모두 대상으로 결과가 즉시 업데이트된다(페이지 리로드 없음)
**And** 결과가 없을 때 빈 상태 메시지가 표시된다

### Story 1.2: 정렬 옵션 제공
As a 독자,
I want 최신/제목/태그/인기순 정렬을 전환할 수 있다,
So that 탐색 목적에 맞게 목록을 재배열한다.

**Acceptance Criteria:**
**Given** 검색/필터가 적용된 목록
**When** 정렬 옵션(최신/제목/태그/인기) 중 하나를 선택
**Then** 목록이 선택한 기준으로 재정렬된다(필터 상태 유지)
**And** 현재 정렬 상태가 UI에 명확히 표시된다(기본: 최신)

### Story 1.3: 태그/검색 조합 필터
As a 독자,
I want 태그 배지를 클릭/해제하여 검색과 함께 필터링하고 초기화할 수 있다,
So that 관심 주제만 좁혀서 본다.

**Acceptance Criteria:**
**Given** 태그 배지들이 보임
**When** 하나 이상의 태그를 선택/해제하거나 "필터 초기화"를 클릭
**Then** 목록이 선택된 태그와 검색어를 모두 반영해 즉시 갱신된다
**And** 선택된 태그 상태가 시각적으로 구분되고 초기화 시 기본 상태로 복원된다

## Epic 2: 끊김 없는 본문 읽기

### Story 2.1: 정적 HTML 본문 열람
As a 독자,
I want 선택한 포스트를 정적 HTML로 바로 읽을 수 있다,
So that 별도 요청 없이 빠르게 본문을 본다.

**Acceptance Criteria:**
**Given** 게시글 링크를 클릭
**When** 본문 페이지가 열릴 때
**Then** 정적 HTML이 기본 경로에서 서빙되고 TOC/앵커를 포함한다
**And** 클라이언트 추가 데이터 패치 없이 전체 본문이 표시된다

### Story 2.2: JSON 렌더 폴백
As a 독자,
I want 정적 HTML이 없거나 실패할 때 JSON으로 렌더링된 본문을 본다,
So that 콘텐츠 접근이 끊기지 않는다.

**Acceptance Criteria:**
**Given** 정적 HTML 로드에 실패하거나 누락된 포스트
**When** 클라이언트가 해당 포스트의 JSON을 요청
**Then** JSON을 사용해 본문/메타/TOC를 렌더링하고 동일한 스타일로 표시한다
**And** JSON 요청 실패 시 에러 메시지와 재시도/HTML 링크 폴백을 제공한다

### Story 2.3: 지연 코드 하이라이트
As a 독자,
I want 코드 블록이 콘텐츠 표시 후에 점진적으로 하이라이트된다,
So that 초기 로드가 빠르면서도 가독성을 확보한다.

**Acceptance Criteria:**
**Given** 본문에 코드 블록이 포함됨
**When** 페이지가 로드된 후
**Then** highlight.js 자산이 지연 로드되고 코드 블록에 순차 적용된다
**And** 하이라이트 로딩 전에도 코드가 기본 스타일로 읽을 수 있다

## Epic 3: 안정적인 빌드·배포 파이프라인

### Story 3.1: 메타 JSON 및 정적 HTML 동시 생성
As a 작성자,
I want 빌드 시 각 포스트의 메타/요약 JSON과 정적 HTML을 함께 생성한다,
So that 클라이언트 검색·목록과 본문 뷰가 모두 준비된다.

**Acceptance Criteria:**
**Given** 빌드 명령을 실행
**When** 빌드가 완료될 때
**Then** 모든 포스트의 정적 HTML과 대응하는 메타/요약 JSON이 생성되어 `_site`에 저장된다
**And** 생성된 JSON 총량과 포스트 수가 로그에 기록된다(5MB 예산 확인)

### Story 3.2: mtime 기반 증분 빌드
As a 운영자,
I want 변경된 파일만 감지해 1분 이내에 증분 빌드를 끝낸다,
So that 빌드 시간을 최소화한다.

**Acceptance Criteria:**
**Given** 소스 파일들의 mtime이 기록됨
**When** 증분 빌드를 실행
**Then** 변경된 포스트/자산만 재빌드되고 대상 파일 목록과 경과 시간이 로그에 남는다
**And** 감지 실패 시 전체 빌드로 폴백하되 실패 원인을 로그로 남긴다

### Story 3.3: 실패 시 롤백 및 캐시 리셋
As a 운영자,
I want 빌드 실패 시 마지막 성공 아티팩트로 롤백하고 캐시를 무효화한다,
So that 사용자는 항상 정상 버전만 본다.

**Acceptance Criteria:**
**Given** 최근 성공한 빌드 아티팩트가 보관됨
**When** 최신 빌드가 실패
**Then** 사이트가 마지막 성공본으로 교체되고 해시 자산/캐시가 갱신된다
**And** 실패 원인과 롤백 결과가 로그로 남고 경고가 표시된다

## Epic 4: 운영·신뢰성·접근성·SEO

### Story 4.1: 빌드·자산·성능 모니터링
As a 운영자,
I want 빌드 시간, JSON/번들 크기, 최초 로드 시간, LCP를 수집·경고한다,
So that 예산을 초과하기 전에 대응한다.

**Acceptance Criteria:**
**Given** 빌드를 실행하거나 페이지를 로드
**When** 수집 스크립트/측정이 완료
**Then** 빌드 시간, JSON/번들 크기, LCP, 최초 로드 시간이 기록된다
**And** 설정된 임계치(빌드<3분/증분<1분/JSON≤5MB/로드<1.5초)를 넘으면 경고 로그를 남긴다

### Story 4.2: 해시 자산 및 CDN 무효화
As a 운영자,
I want 해시된 파일명으로 캐시를 제어하고 변경분만 CDN 무효화한다,
So that 최신 자산이 즉시 반영된다.

**Acceptance Criteria:**
**Given** 빌드 결과가 생성됨
**When** 정적 자산이 출력될 때
**Then** CSS/JS/JSON/이미지에 해시가 파일명에 포함되고 매니페스트가 남는다
**And** 변경된 해시만 CDN 무효화 대상으로 로그에 기록된다

### Story 4.3: 접근성(WCAG AA) 보장
As a 독자,
I want 키보드 내비, 포커스 표시, ARIA 라벨, 대비를 갖춘 UI를 사용한다,
So that 검색/정렬/필터/카드/TOC를 누구나 이용할 수 있다.

**Acceptance Criteria:**
**Given** 검색창, 정렬 토글, 태그 필터, 카드, TOC가 화면에 있음
**When** 키보드만으로 이동/선택하거나 스크린리더를 사용할 때
**Then** 포커스 링과 ARIA 라벨이 제공되고 대비가 WCAG AA를 충족한다
**And** 상태(로딩/빈 결과/에러)가 텍스트와 아이콘으로 모두 전달된다

### Story 4.4: SEO 정적 HTML 및 메타 유지
As a 운영자,
I want 모든 포스트가 정적 HTML과 og/meta 태그를 포함하고 CSR-only를 막는다,
So that 검색 엔진에 안정적으로 색인된다.

**Acceptance Criteria:**
**Given** 포스트 페이지를 빌드/배포
**When** 크롤러가 페이지를 요청
**Then** 정적 HTML 본문과 제목/설명/og/meta가 포함되어 응답된다
**And** 필수 콘텐츠가 클라이언트 렌더링에만 의존하지 않고 SSR/정적 형태로 제공된다
