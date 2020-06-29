# StrRegex

[pkg-url]: https://github.com/JuliaString/StrRegex.jl.git

[julia-url]:    https://github.com/JuliaLang/Julia
[julia-release]:https://img.shields.io/github/release/JuliaLang/julia.svg

[release]:      https://img.shields.io/github/release/JuliaString/StrRegex.jl.svg
[release-date]: https://img.shields.io/github/release-date/JuliaString/StrRegex.jl.svg

[license-img]:  http://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat
[license-url]:  LICENSE.md

[gitter-img]:   https://badges.gitter.im/Join%20Chat.svg
[gitter-url]:   https://gitter.im/JuliaString/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge

[travis-url]:   https://travis-ci.org/JuliaString/StrRegex.jl
[travis-s-img]: https://travis-ci.org/JuliaString/StrRegex.jl.svg
[travis-m-img]: https://travis-ci.org/JuliaString/StrRegex.jl.svg?branch=master

[codecov-url]:  https://codecov.io/gh/JuliaString/StrRegex.jl
[codecov-img]:  https://codecov.io/gh/JuliaString/StrRegex.jl/branch/master/graph/badge.svg

[contrib]:    https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat

[![][release]][pkg-url] [![][release-date]][pkg-url] [![][license-img]][license-url] [![contributions welcome][contrib]](https://github.com/JuliaString/StrRegex.jl/issues)

| **Julia Version** | **Unit Tests** | **Coverage** |
|:------------------:|:------------------:|:---------------------:|
| [![][julia-release]][julia-url] | [![][travis-s-img]][travis-url] | [![][codecov-img]][codecov-url]
| Julia Latest | [![][travis-m-img]][travis-url] | [![][codecov-img]][codecov-url]

The `StrRegex` package adds Regex support to the `Strs` package, as well as fix some issues present in the base Regex support.

* Thread-safe support
* Allows the whole range of compile and match options
* Supports both UTF and non-UTF strings
* Supports strings with 8, 16, and 32-bit codeunit sizes
* Correctly sets the NO_CHECK_UTF flag based on the string type

It is working on both the release version (v0.6.2) and the latest master (v0.7.0-DEV).

This uses a `R"..."` macro, or `RegexStr` constructor, instead of `r"..."` and `Regex` as in Base.

Some changes might be needed in Base to make it work with the `r"..."` regex string macro and `Regex` type, because there are some fields missing that would be needed to handle arbitrary abstract string types).
