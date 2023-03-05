
# Renaming

Here is the documentation of how modules or compiler steps rename variables. The goal is to avoid name clashes because two distinct renaming functions use incompatible renaming policies.


## Renamer

Renames variables

Syntax: `<orig name>@<idx>` with `@<idx>` omitted if idx == 0 (code is in `GlobalVarsCtx`)

`x` --> `x`, `x@1`, `x@2`, ...


## Desugarer

Creates new variables

Syntax: `$<idx>`

`$0`, `$1`, `$2`, ...


## Path

Transform assignments to variables into definition of new values

Syntax: `<orig name>%<idx>` with `%<idx>` omitted if idx == 0 (code in `VarsCtx`)

`x` --> `x`, `x%1`, `x%2`, ...


## PathVerifier

Creates new variables

Syntax: `%<idx>`

`%0`, `%1`, `%2`, ...
