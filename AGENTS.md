# AGENTS.md - Development Guide for Ainsyl Blog Engine

This file provides build commands, code style guidelines, and development practices for agentic coding agents working on this Haskell-based static site generator.

## Project Overview

**Ainsyl** is a Hakyll-based static site generator for personal blogs with GIS mapping capabilities, theme support, and automated content management.

### Architecture
- **Main Entry**: `src/Site.hs` - Hakyll rules and site generation
- **CLI Tool**: `src/Post.hs` - Post creation utility  
- **Web Server**: `src/Server.hs` - Optional Snap server
- **Modules**: `src/Site/` - Core functionality modules
- **Templates**: `templates/themes/` - Theme templates (default, minimal-mistakes)
- **Content**: `posts/` - Blog posts organized by date (YYYY/MM/DD/)

## Build & Development Commands

### Essential Commands
```bash
# Build and start development server with live reload
stack build
stack run ainsyl -- watch                    # Default: 127.0.0.1:8000
stack run ainsyl -- watch --host 0.0.0.0 --port 8000

# Create new content
make post TITLE="My Title" FILE="my-post"                    # Regular post
make post-map TITLE="Seoul Trip" FILE="seoul-trip"          # Map post with coordinates  
make post-jungto TITLE="Jungto" FILE="jungto-day" THOUSAND=10 HUNDRED=2 DAY=3

# Build commands
make build                                    # Smart incremental build
FULL=1 make build                            # Force full rebuild
make rewatch                                  # Clean rebuild + watch
make vendor-js                               # Update frontend assets

# Testing (no automated tests currently)
# Manual testing: check site at http://localhost:8000 after `stack run ainsyl -- watch`
```

### Build System Details
- **Primary**: Stack with LTS-24.24 resolver
- **Alternative**: Cabal (with sandbox support)
- **Frontend**: npm for JS/CSS asset management
- **Compiler Options**: `-threaded -Wall -fwarn-tabs -funbox-strict-fields -O2 -fno-warn-unused-do-bind`

### Single File Testing
No automated test suite exists. For testing changes:
1. Make changes to source files
2. Run `stack build` to check compilation
3. Run `stack run ainsyl -- watch` to test locally
4. Verify changes at `http://localhost:8000`

### Development Mode
Use the `development` flag for additional debugging:
```bash
stack build --flag ainsyl:development
```

## Code Style Guidelines

### 1. Code Formatting
- **Indentation**: 2 spaces consistently (no tabs)
- **Line Length**: Generally under 80-100 characters
- **Function Layout**: Type signatures on separate lines for complex functions
- **Alignment**: Proper alignment for multi-line function calls

```haskell
-- Example formatting
postCtx :: Context String
postCtx =
    dateFieldWith timeLocale "date" "%A, %e %B %Y, %R" `mappend`
    dateFieldWith defaultTimeLocale "post-date" "%Y-%m-%dT%H:%M:%S%z" `mappend`
    field "url" (return . identifierToUrl . toFilePath . itemIdentifier)
```

### 2. Import Organization
Order imports by group with blank lines between:
1. **Language Pragmas** (top of file)
2. **Standard Library Imports** (alphabetical)
3. **Third-party Library Imports** (qualified when appropriate)
4. **Local Module Imports** (alphabetical)

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE CPP               #-}

import Control.Monad (forM_, filterM, foldM, when)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Text as T
import Hakyll hiding (dateFieldWith, getItemUTC)
import Site.Utils (identifierToUrl, unwrap)
```

### 3. Naming Conventions
- **Functions**: `camelCase` - `getPostJson`, `parseTemplateArg`
- **Variables**: `camelCase` - `postTitle`, `templateType`
- **Types/Constructors**: `PascalCase` - `PostOptions`, `PageMetadata`
- **Modules**: Hierarchical `Site.Context`, `Site.Utils`
- **Record Fields**: Descriptive `postJsonTitle`, `metaDescription`

### 4. Error Handling
- Use pattern matching with explicit failure cases
- Prefer `Maybe` handling with cases over partial functions
- Use Hakyll's `fail` for compilation errors
- Include context in error messages

```haskell
-- Error handling pattern
getItemUTC locale id' = do
    parsed <- sequence [...]
    maybe empty' return $ msum parsed
  where
    empty' = fail $ "Could not parse time for " ++ show id'
```

### 5. Type Annotations
- **Always annotate** top-level functions
- **Annotate complex** local expressions for clarity
- **Explicit types** for polymorphic functions

```haskell
main :: IO ()
postCtx :: Context String  
buildPostJson :: Identifier -> Compiler PostJson
```

### 6. Comments & Documentation
- **Module headers**: Brief purpose description
- **Function comments**: Explain non-obvious logic
- **Implementation notes**: Describe algorithms or special cases
- **TODO comments**: Mark future work clearly

```haskell
-- | Contains web handlers to serve files from a directory.
module FileServe (serveDirectory, serveFile) where

-- Resolve repo root, create dated directory, and write a new post file.
exec templateType opts@PostOptions{..} = do
```

### 7. Module Organization
- **Explicit export lists** for clean APIs
- **Hierarchical structure** with `Site.*` modules
- **Clear dependencies** with no circular imports
- **Consistent file organization**

```haskell
module Site.Context
    ( timeLocale
    , tagsContext
    , postCtx
    , FacebookType(..)
    , PageMetadata(..)
    ) where
```

## Language Extensions

### Common Extensions
```haskell
{-# LANGUAGE OverloadedStrings #-}     -- Text/ByteString literals
{-# LANGUAGE RecordWildCards #-}      -- Record pattern matching  
{-# LANGUAGE CPP #-}                   -- Conditional compilation
{-# LANGUAGE FlexibleContexts #-}     -- Type class constraints
{-# LANGUAGE DeriveDataTypeable #-}   -- For CLI option types
```

## Special Patterns

### RecordWildCards Usage
Used consistently when accessing multiple record fields:
```haskell
optionHandler templateType opts@PostOptions{..} = do
    when (null title) $ putStrLn "--title is blank!"
    when (null file) $ putStrLn "--file is blank!"
```

### Conditional Compilation
Use `DEVELOPMENT` flag for dev vs production builds:
```haskell
#ifdef DEVELOPMENT
    -- Development-specific code
#endif
```

### Unsafe Operations
`unsafePerformIO` used sparingly for settings caching with justification comments.

## File Structure Standards

```
src/
├── Site.hs              -- Main Hakyll configuration
├── Post.hs              -- CLI post creation tool
├── Server.hs            -- Optional Snap server  
├── FileServe.hs         -- File serving utilities
├── XmlHtmlWriter.hs     -- Custom Pandoc writer
└── Site/
    ├── Context.hs       -- Template contexts
    ├── Compat.hs        -- Hakyll compatibility
    ├── Theme.hs         -- Theme management
    └── Utils.hs         -- General utilities
```

## Configuration Files

- **`settings.yml`**: Site configuration (theme, URL, metadata)
- **`stack.yaml`**: Stack build configuration (LTS-24.24)
- **`ainsyl.cabal`**: Cabal project definition with 3 executables
- **`package.json`**: Frontend asset dependencies
- **`Makefile`**: Convenient commands for common tasks

## Content Creation Patterns

### Post File Organization
- **Location**: `posts/YYYY/MM/DD/filename.md`
- **Front Matter**: YAML metadata with title, date, optional coordinates
- **Map Posts**: Include `lat`, `lng`, `location` fields for GIS integration
- **Templates**: Different templates for post types (post, map, jungto)

### Template Usage
- **Template Selection**: Automatic based on creation command
- **Theme Integration**: Templates use theme-specific parts
- **Metadata Context**: Rich metadata available in templates

## Deployment

### Local Deployment
```bash
make deploy-local MSG="Deploy message"
```
Builds site and deploys to gh-pages branch using git worktree.

## Development Practices

### Before Making Changes
1. **Read existing patterns** in similar files
2. **Follow established naming** and formatting conventions
3. **Check existing implementations** before adding new functions
4. **Consider performance** implications of new features

### Code Quality
- **Type safety** is paramount - avoid partial functions
- **Error messages** should be informative and contextual
- **Documentation** should explain the "why", not just "what"
- **Modularity** - keep functions focused and reusable

### Testing Workflow
1. **Compile check**: `stack build`
2. **Local test**: `stack run ainsyl -- watch`
3. **Visual verification**: Check generated site locally
4. **Content testing**: Verify post creation, theme switching, map features

## Common Gotchas

### Hakyll-Specific
- **Compiler Monad**: Use `fail` for errors, not `error`
- **Identifier Handling**: Use `identifierToUrl` for consistent paths
- **Template Context**: Build contexts monoidically with `mappend`
- **Dependencies**: Declare dependencies correctly with `rulesExtraDependencies`

### Stack/Cabal
- **Flag management**: Use `--flag ainsyl:development` for dev builds
- **Dependency resolution**: Stack handles this automatically via LTS resolver
- **Build artifacts**: Clean with `stack run ainsyl -- clean` when needed

### Content Management
- **File naming**: Posts are automatically slugified and dated
- **Template switching**: Development vs production templates via settings
- **Asset vendoring**: Use `make vendor-js` after updating package.json

This guide should be referenced for all code modifications to maintain consistency and quality across the codebase.