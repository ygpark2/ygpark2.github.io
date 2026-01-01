# Project Documentation Index

## Project Overview
- **Type:** monolith
- **Primary Language:** Haskell
- **Architecture:** Static site (Hakyll) + optional Snap server

## Quick Reference
- **Tech Stack:** Hakyll, Snap, cmdargs, Stack/Cabal
- **Entry Point:** `stack run ainsyl` (build), `stack run server` (serve)
- **Architecture Pattern:** Static site generation

## Generated Documentation
- [Project Overview](./project-overview.md)
- [Architecture](./architecture.md)
- [Source Tree Analysis](./source-tree-analysis.md)
- [Component Inventory](./component-inventory.md)
- [Development Guide](./development-guide.md)
- [API Contracts](./api-contracts.md)
- [Data Models](./data-models.md)

## Existing Documentation
- [README](../README.md)

## Getting Started
1) `stack build`  
2) `stack run ainsyl` → `_site/` 생성  
3) (옵션) `stack run server`로 로컬 서빙  
4) (옵션) 새 포스트: `stack run post -- --title "제목" --file my-post`
