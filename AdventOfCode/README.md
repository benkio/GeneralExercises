# AdventOfCode

Contains all the advent of code years in haskell.

## Tool Required

- `stack`
- `ormolu`
- `fd`
- `hlint`
- `apply-refactor`

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
