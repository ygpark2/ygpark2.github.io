

# deploy command description

https://gist.github.com/cobyism/4730490

codex resume 019b1301-983a-7c53-a4f4-94536e8cbe4f


• Hakyll 빌트인 워치/프리뷰 서버로 돌리면 됩니다:

  # (필요 시) 한 번 빌드
  stack build

  # 파일 변경 감지 + 프리뷰 서버(기본 8000) 동시에 실행
  stack run ainsyl -- watch          # 기본 host: 127.0.0.1, port: 8000

  # 포트/호스트 지정 예시
  stack run ainsyl -- watch --host 0.0.0.0 --port 8000

  별도 Snap 서버를 쓰려면 stack build --flag ainsyl:server 후 stack run --flag
  ainsyl:server server지만, 라이브 리로드는 Hakyll watch가 담당합니다.


• 새 포스트 생성:

  # 제목과 파일명(확장자 제외)
  stack run post post -- --title "My Title" --file "my-post"

  # 지도용 포스트 생성
  stack run post map -- --title "Trip to Seoul" --file "seoul-trip"

  # 정토 템플릿 포스트 생성
  stack run post jungto -- --title "Jungto" --file "jungto-day" --thousand 10 --hundred 2 --day 3

  # 생성 위치: posts/YYYY/MM/DD/my-post.md
  # 동일 파일이 있으면 실패합니다.
  # 파일명은 slugify 처리됩니다.


• 핵심 흐름:

  - 빌드/워치는 `src/Site.hs`가 Hakyll 규칙을 등록해 전체 사이트를 생성합니다.
  - 템플릿은 `templates/themes/<theme>/` 아래에서 로딩됩니다.
  - 포스트/페이지/태그/아카이브/피드/사이트맵은 Hakyll 규칙으로 생성됩니다.
  - `src/Post.hs`는 새 포스트 파일을 생성하는 CLI 도구입니다.

• 테마 설정:

  - `settings.yml`의 `site.theme`/`site.themeDevelopment`로 테마를 선택합니다.

• JS 라이브러리 업데이트:

  - `package.json`에서 버전 관리 후 `make vendor-js`로 내려받아 `assets/js`에 반영합니다.

• 빌드(옵션):

  - `make build`는 `scripts/incremental-build.sh`를 사용해 변경 없으면 빌드를 건너뜁니다.

• 지도(GIS) 포스트 표시:

  - 글 front matter에 좌표를 추가하면 `/map/`에 자동 표시됩니다.

  예시:
  ---
  title: "Seoul trip"
  lat: 37.5665
  lng: 126.9780
  location: "Seoul"
  ---

  - `lat`/`lng`가 있는 글만 `assets/data/visited.geojson`으로 자동 생성됩니다.
  - 지도 팝업에서 제목을 클릭하면 해당 글로 이동합니다.
