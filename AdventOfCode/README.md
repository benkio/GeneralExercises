# AdventOfCode

Contains all the advent of code years in haskell.

## Tool Required

- `stack`
- `ormolu`
- `fd`
- `hlint`
- `apply-refactor`

## Setting Up Environment

You can easily create a `nix-shell` with all the necessary dependencies

``` sh
nix-shell
```

## Compiling

`stack build`

## Running

Execute the Main in `app/Main.hs`:
`stack run`

otherwise use `ghci` to run the code interpreted

## Linting

To see the fix suggestions:
`hlint src/`

To apply them in place on single file:
`hlint <<filepath>> --refactor --refactor-options="--inplace"`

Or on the whole project:
`fd . -e hs src/ -x bash -c 'hlint {} "$1" "$2"' bash "--refactor" "--refactor-options='--inplace'"`

## Formatting

Format all files:
`fourmolu -c -i src/`

## Testing

Run all tests with `cabal test`

### Run a specific test by name
`cabal test --test-option="-p" --test-option="TwentyFifteen-December04-solution1"`
Or using the short form:
`cabal test --test-option="-p/TwentyFifteen-December04-solution1"`
### Run all tests for a specific year
`cabal test --test-option="-p" --test-option="TwentyFifteen"`
Run all tests for a specific day
`cabal test --test-option="-p" --test-option="December04"`
### Pattern matching
The -p flag accepts patterns, so you can use:

- Match any test containing "December04" :: `cabal test --test-option="-p/December04"`
- Match any test starting with "TwentyFifteen" :: `cabal test --test-option="-p/^TwentyFifteen"`

### Alternative: Using test group names
Since tests are organized in groups (e.g., "TwentyFifteen", "TwentySixteen"), you can also filter by group:

`cabal test --test-option="-p" --test-option="TwentyFifteen"`

The pattern matching is flexible, so you can use any substring of the test name to filter which tests run.
