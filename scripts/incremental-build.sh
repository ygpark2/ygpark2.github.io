#!/usr/bin/env bash
set -euo pipefail

# Incremental build helper using mtime. If no changes since the last build,
# it skips Hakyll. Force full build with FULL=1.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STAMP="$ROOT/log/last-build.timestamp"
LOG="$ROOT/log/incremental-build.log"
SOURCES=(
  posts
  assets
  pages
  templates
  src
)

mkdir -p "$ROOT/log"

force=${FULL:-0}

if [[ $force -ne 0 ]]; then
  echo "[info] FULL=1, 전체 빌드를 실행합니다." | tee -a "$LOG"
  (cd "$ROOT" && stack build)
  (cd "$ROOT" && stack run ainsyl -- build)
  date -u +"%Y-%m-%dT%H:%M:%SZ" > "$STAMP"
  exit 0
fi

if [[ ! -f "$STAMP" ]]; then
  echo "[info] 첫 빌드: 기준 타임스탬프가 없어 전체 빌드를 실행합니다." | tee -a "$LOG"
  (cd "$ROOT" && stack build)
  (cd "$ROOT" && stack run ainsyl -- build)
  date -u +"%Y-%m-%dT%H:%M:%SZ" > "$STAMP"
  exit 0
fi

changed=()
for path in "${SOURCES[@]}"; do
  while IFS= read -r f; do
    changed+=("$f")
  done < <(cd "$ROOT" && find "$path" -type f -newer "$STAMP" -print)
done

if [[ ${#changed[@]} -eq 0 ]]; then
  echo "[info] 변경 없음: 빌드를 건너뜁니다." | tee -a "$LOG"
  exit 0
fi

echo "[info] 변경된 파일 수: ${#changed[@]}" | tee -a "$LOG"
for f in "${changed[@]}"; do
  echo " - $f" >> "$LOG"
done

(cd "$ROOT" && stack build)
(cd "$ROOT" && stack run ainsyl -- build)
date -u +"%Y-%m-%dT%H:%M:%SZ" > "$STAMP"
echo "[info] 빌드 완료" | tee -a "$LOG"
