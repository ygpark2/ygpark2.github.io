

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


