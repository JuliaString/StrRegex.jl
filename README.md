# StrRegex

| **Package Status** | **Package Evaluator** | **Coverage**      |
|:------------------:|:---------------------:|:-----------------:|
| [![License](http://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat)](LICENSE.md) [![Build Status](https://travis-ci.org/JuliaString/StrRegex.jl.svg?branch=master)](https://travis-ci.org/JuliaString/StrRegex.jl) | [![StrRegex](http://pkg.julialang.org/badges/StrRegex_0.6.svg)](http://pkg.julialang.org/?pkg=StrRegex) [![StrRegex](http://pkg.julialang.org/badges/StrRegex_0.7.svg)](http://pkg.julialang.org/?pkg=StrRegex) | [![Coverage Status](https://coveralls.io/repos/github/JuliaString/StrRegex.jl/badge.svg?branch=master)](https://coveralls.io/github/JuliaString/StrRegex.jl?branch=master)
[![codecov.io](http://codecov.io/github/JuliaString/StrRegex.jl/coverage.svg?branch=master)](http://codecov.io/github/JuliaString/StrRegex.jl?branch=master) |

The `StrRegex` package adds Regex support to the `Strs` package, as well as fix some issues present in the base Regex support.

* Thread-safe support
* Allows the whole range of compile and match options
* Supports both UTF and non-UTF strings
* Supports strings with 8, 16, and 32-bit codeunit sizes
* Correctly sets the NO_CHECK_UTF flag based on the string type

It is working on both the release version (v0.6.2) and the latest master (v0.7.0-DEV).

This uses a `R"..."` macro, or `RegexStr` constructor, instead of `r"..."` and `Regex` as in Base.

Some changes might be needed in Base to make it work with the `r"..."` regex string macro and `Regex` type, because there are some fields missing that would be needed to handle arbitrary abstract string types).
