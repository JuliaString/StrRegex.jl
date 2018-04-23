#=
Regex functions for Str strings

Copyright 2018 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on julia/base/regex.jl
=#

module StrRegex

using Strs
using Strs: V6_COMPAT, MaybeSub, _not_found, bytoff, boundserr, basecse
using Strs: _LatinCSE, Latin_CSEs, Binary_CSEs, UCS2_CSEs, UTF8_CSEs, UTF32_CSEs
import Strs: find, occurs_in, _occurs_in

using Base.PCRE
using Base.PCRE: NO_UTF_CHECK, UTF, CASELESS, MULTILINE, DOTALL, EXTENDED

# UTF and NO_UTF_CHECK are based on the string type
const DEFAULT_COMPILER_OPTS = PCRE.ALT_BSUX
const DEFAULT_MATCH_OPTS    = 0

import Base: Regex, match, compile, eachmatch, show, getindex, eltype, start, done, next, ==
import Base: hash, IteratorSize

export RegexStr, RegexStrMatch

const Binary_Regex_CSEs = Union{ASCIICSE,Latin_CSEs,Binary_CSEs}
const Regex_CSEs = Union{Binary_Regex_CSEs,UTF8_CSEs}

# Default is to act as if validated UTF8
_cvt_compile(::Type{<:CSE}, co) = UInt32(co) | NO_UTF_CHECK | UTF
_cvt_match(::Type{<:CSE}, co)   = UInt32(co) | NO_UTF_CHECK

_cvt_compile(::Type{Binary_Regex_CSEs}, co) = UInt32(co) & ~UTF

_cvt_compile(::Type{<:Str{C}}, co) where {C<:CSE} = _cvt_compile(C, co)
_cvt_match(::Type{<:Str{C}}, co) where {C<:CSE}   = _cvt_match(C, co)

# String type is not validated
_cvt_compile(::Type{String}, co) = (UInt32(co) & ~NO_UTF_CHECK) | UTF
_cvt_match(::Type{String}, co)   = (UInt32(co) & ~NO_UTF_CHECK)

@noinline _compile_opts(::Type{C}, options) where {C} =
    (options & ~PCRE.COMPILE_MASK) == 0 ? _cvt_compile(C, options) :
    throw(ArgumentError("invalid regex compile options: $options"))
@noinline _match_opts(::Type{C}, options) where {C} =
    (options & ~PCRE.EXECUTE_MASK) == 0 ? _cvt_match(C, options) :
    throw(ArgumentError("invalid regex match options: $options"))

_match_type(::Type{<:UTF8_CSEs})   = 1
_match_type(::Type{UTF16CSE})      = 2
_match_type(::Type{<:UTF32_CSEs})  = 3
_match_type(::Type{ASCIICSE})      = 4
_match_type(::Type{<:Binary_CSEs}) = 4
_match_type(::Type{<:Latin_CSEs})  = 4
_match_type(::Type{Text2CSE})      = 5
_match_type(::Type{<:UCS2_CSEs})   = 5
_match_type(::Type{Text4CSE})      = 6

function finalize! end

mutable struct RegexStr
    pattern::AbstractString
    compile_options::UInt32
    match_options::UInt32
    regex::Ptr{Cvoid} # Current compiled regex
    match_type::Int
    ovec::Vector{Csize_t}
    match_data::Ptr{Cvoid}

    # There are 6 combinations of CodeUnit size (8,16,32) and UTF/no-UTF
    # 1 - UTF8
    # 2 - UTF16
    # 3 - UTF32/_UTF32
    # 4 - Text1/ASCII/Binary - Latin/_Latin need (*UCP), but not (*UTF)
    # 5 - Text2 - UCS2/_UCS2 need (*UCP), but not (*UTF)
    # 6 - Text4
    table::NTuple{6, Ptr{Cvoid}}

    function RegexStr(pattern::T, compile_options::Integer, match_options::Integer
                      ) where {T<:AbstractString}
        re = compile(cse(T),
                     new(pattern,
                         _compile_opts(T, compile_options), _match_opts(T, match_options),
                         C_NULL, 0, Csize_t[], C_NULL,
                         (C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL)))
        @static V6_COMPAT ? finalizer(re, finalize!) : finalizer(finalize!, re)
        re
    end
end

function _update_table(t, re, n)
    if n < 4
        (n == 1 ? (re, t[2], t[3], t[4], t[5], t[6])
         : n == 2 ? (t[1], re, t[3], t[4], t[5], t[6]) : (t[1], t[2], re, t[4], t[5], t[6]))
    else
        (n == 4 ? (t[1], t[2], t[3], re, t[5], t[6])
         : n == 5 ? (t[1], t[2], t[3], t[4], re, t[6]) : (t[1], t[2], t[3], t[4], t[5], re))
    end
end

function finalize!(re)
    if re.regex != C_NULL
        for reg in re.table
            reg == C_NULL || PCRE.free_re(reg)
        end
        re.regex = C_NULL
        re.table = (C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL)
    end
    re.match_data == C_NULL || PCRE.free_match_data(re.match_data)
end

const RegexTypes = Union{Regex, RegexStr}

function _update_compile_opts(flags)
    options = DEFAULT_COMPILER_OPTS
    for f in flags
        options |= f=='i' ? PCRE.CASELESS  :
                   f=='m' ? PCRE.MULTILINE :
                   f=='s' ? PCRE.DOTALL    :
                   f=='x' ? PCRE.EXTENDED  :
                   throw(ArgumentError("unknown regex flag: $f"))
    end
    options
end

RegexStr(pattern::AbstractString) =
    RegexStr(pattern, DEFAULT_COMPILER_OPTS, DEFAULT_MATCH_OPTS)
RegexStr(pattern::AbstractString, flags::AbstractString) =
    RegexStr(pattern, _update_compile_opts(flags), DEFAULT_MATCH_OPTS)

#=
Regex(pattern::MaybeSub{<:Str}, co, mo) = RegexStr(pattern, co, mo)
Regex(pattern::MaybeSub{<:Str}, flags::AbstractString) = RegexStr(pattern, flags)
Regex(pattern::MaybeSub{<:Str}) = RegexStr(pattern)
=#

# Yes, this is type piracy, but it is needed to make all string types work together easily
Regex(pattern::AbstractString, co::Integer, mo::Integer) = RegexStr(pattern, co, mo)
Regex(pattern::AbstractString, flags::AbstractString) = RegexStr(pattern, flags)
Regex(pattern::AbstractString) = RegexStr(pattern)

export @R_str

macro R_str(pattern, flags...) RegexStr(pattern, flags...) end

function show(io::IO, re::RegexStr)
    imsx = PCRE.CASELESS|PCRE.MULTILINE|PCRE.DOTALL|PCRE.EXTENDED
    opts = re.compile_options
    if (opts & ~imsx) == DEFAULT_COMPILER_OPTS
        print(io, 'R')
        Base.print_quoted_literal(io, re.pattern)
        if (opts & PCRE.CASELESS ) != 0; print(io, 'i'); end
        if (opts & PCRE.MULTILINE) != 0; print(io, 'm'); end
        if (opts & PCRE.DOTALL   ) != 0; print(io, 's'); end
        if (opts & PCRE.EXTENDED ) != 0; print(io, 'x'); end
    else
        print(io, "RegexStr(")
        show(io, re.pattern)
        print(io, ',')
        show(io, opts)
        print(io, ')')
    end
end

struct RegexStrMatch{T<:AbstractString,M<:RegexTypes}
    match::SubString{T}
    captures::Vector{Union{Nothing,SubString{T}}}
    offset::Int
    offsets::Vector{Int}
    regex::M
end

function show(io::IO, m::RegexStrMatch{T,M}) where {T,M}
    print(io, "RegexStrMatch{$T,$M}(")
    show(io, m.match)
    idx_to_capture_name = PCRE.capture_names(m.regex.regex)
    if !is_empty(m.captures)
        print(io, ", ")
        for i = 1:length(m.captures)
            # If the capture group is named, show the name.
            # Otherwise show its index.
            print(io, get(idx_to_capture_name, i, i), "=")
            show(io, m.captures[i])
            i < length(m.captures) && print(io, ", ")
        end
    end
    print(io, ")")
end

# Capture group extraction
getindex(m::RegexStrMatch, idx::Integer) = m.captures[idx]
function getindex(m::RegexStrMatch, name::Symbol)
    idx = PCRE.substring_number_from_name(m.regex.regex, name)
    idx <= 0 && error("no capture group named $name found in regex")
    m[idx]
end

getindex(m::RegexStrMatch, name::AbstractString) = m[Symbol(name)]

function compile(::Type{C}, regex::RegexStr) where {C<:Regex_CSEs}
    (nm = _match_type(C)) == regex.match_type && return
    regex.match_type = nm
    if (re = regex.table[nm]) == C_NULL
        regex.compile_options = cvtcomp = _cvt_compile(C, regex.compile_options)
        regex.pattern = pat = convert(Str{C}, regex.pattern)
        regex.regex = re = PCRE.compile(pat, cvtcomp)
        regex.table = _update_table(regex.table, re, nm)
        PCRE.jit_compile(re)
    end
    if regex.match_data == C_NULL
        regex.match_data = PCRE.create_match_data(re)
        regex.ovec = PCRE.get_ovec(regex.match_data)
    end
    regex
end

function compile(::Type{C}, regex::Regex) where {C<:Regex_CSEs}
    re = regex.regex
    # ASCII is compatible with all (current) types, don't recompile
    C == ASCIICSE && re != C_NULL && return
    oldopt = regex.compile_options
    cvtcomp = _cvt_compile(C, oldopt)
    if cvtcomp != oldopt
        regex.compile_options = cvtcomp
        re == C_NULL || (PCRE.free_re(re); regex.regex = re = C_NULL)
    end
    if re == C_NULL
        regex.pattern = pat = convert(Str{C}, regex.pattern)
        regex.regex = re = PCRE.compile(pat, cvtcomp)
        PCRE.jit_compile(re)
    end
    if regex.match_data == C_NULL
        regex.match_data = PCRE.create_match_data(re)
        regex.ovec = PCRE.get_ovec(regex.match_data)
    end
    regex
end

function exec(re, subject, offset, options, match_data)
    pnt = pointer(subject)
    siz = sizeof(subject)
    loc = bytoff(eltype(pnt), offset)
    loc < siz || boundserr(subject, offset)
    rc = ccall((:pcre2_match_8, PCRE.PCRE_LIB), Cint,
               (Ptr{Cvoid}, Ptr{UInt8}, Csize_t, Csize_t, Cuint, Ptr{Cvoid}, Ptr{Cvoid}),
               re, pnt, siz, loc, options, match_data, PCRE.MATCH_CONTEXT[])
    # rc == -1 means no match, -2 means partial match.
    rc < -2 && error("StrRegex.exec error: $(PCRE.err_message(rc))")
    rc >= 0
end

function _match(::Type{C}, re, str, idx, add_opts) where {C<:CSE}
    compile(C, re)
    exec(re.regex, str, idx-1, _cvt_match(C, re.match_options | add_opts), re.match_data) ||
        return nothing
    ovec = re.ovec
    n = div(length(ovec),2) - 1
    mat = SubString(str, ovec[1]+1, prevind(str, ovec[2]+1))
    cap = Union{Nothing,SubString{typeof(str)}}[ovec[2i+1] == PCRE.UNSET ?
                                               nothing :
                                               SubString(str, ovec[2i+1]+1,
                                                         prevind(str, ovec[2i+2]+1)) for i=1:n]
    off = Int[ ovec[2i+1]+1 for i=1:n ]
    RegexStrMatch(mat, cap, Int(ovec[1]+1), off, re)
end

match(re::T, str::MaybeSub{<:Str{C}}, idx::Integer,
      add_opts::UInt32=UInt32(0)) where {T<:RegexTypes,C<:CSE} =
    error("$T not supported yet on strings with codeunit == UInt16 or UInt32")

match(re::T, str::MaybeSub{<:Str{C}}, idx::Integer, add_opts::UInt32=UInt32(0)
      ) where {T<:RegexTypes,C<:Regex_CSEs} =
    _match(basecse(C), re, str, Int(idx), add_opts)

match(re::Regex, str::MaybeSub{<:Str})   = match(re, str, 1)
match(re::RegexStr, str::AbstractString) = match(re, str, 1)

@inline function __find(::Type{C}, re, str, idx) where {C}
    compile(C, re)
    (exec(re.regex, str, idx, _cvt_match(C, re.match_options), re.match_data)
     ? ((Int(re.ovec[1]) + 1) : prevind(str, Int(re.ovec[2]) + 1)) : _not_found)
end

@inline _find(::Type{C}, re, str) where {C} = __find(C, re, str, 0)

@inline _find(::Type{C}, re, str, idx) where {C} =
    (idx-1 <= ncodeunits(str)
     ? __find(C, re, str, idx-1)
     : (@boundscheck boundserr(str, idx) ; return _not_found))

find(::Type{Fwd}, re::RegexTypes, str::AbstractString, idx::Integer) =
    throw(ArgumentError("regex search is only available for the String or Str types with " *
                        "codeunit == UInt8, or substrings of those types, " *
                        "use UTF8Str to convert"))

find(::Type{Fwd}, re::RegexTypes, str::MaybeSub{<:Str{C}}, idx::Integer) where {C<:Regex_CSEs} =
    _find(C, re, str, idx)
find(::Type{Fwd}, re::RegexTypes, str::MaybeSub{<:Str{_LatinCSE}}, idx::Integer) =
    _find(LatinCSE, re, str, idx)
find(::Type{Fwd}, re::RegexTypes, str::MaybeSub{String}, idx::Integer) =
    _find(RawUTF8CSE, re, str, idx)

find(::Type{First}, re::RegexTypes, str::AbstractString) = find(Fwd, re, str, 1)

find(::Type{First}, re::RegexTypes, str::MaybeSub{<:Str{C}}) where {C<:Regex_CSEs} =
    __find(C, re, str, 0)
find(::Type{First}, re::RegexTypes, str::MaybeSub{<:Str{_LatinCSE}}) =
    __find(LatinCSE, re, str, 0)
find(::Type{First}, re::RegexTypes, str::MaybeSub{String}) = 
    __find(RawUTF8CSE, re, str, 0)

struct RegexStrMatchIterator{R<:RegexTypes,T<:AbstractString}
    regex::R
    string::T
    overlap::Bool

    RegexStrMatchIterator{R,T}(regex::R, string::T, ovr::Bool=false
                               ) where {R<:RegexTypes,T<:AbstractString} =
        new{R,T}(regex, string, ovr)
end
compile(itr::RegexStrMatchIterator) = (compile(itr.regex); itr)
eltype(::Type{RegexStrMatchIterator}) = RegexStrMatch
start(itr::RegexStrMatchIterator) = match(itr.regex, itr.string, 1, UInt32(0))
done(itr::RegexStrMatchIterator, prev_match) = (prev_match === nothing)
IteratorSize(::Type{RegexStrMatchIterator}) = SizeUnknown()

# Assumes prev_match is not nothing
function next(itr::RegexStrMatchIterator, prev_match)
    opts_nonempty = UInt32(PCRE.ANCHORED | PCRE.NOTEMPTY_ATSTART)
    if isempty(prev_match.match)
        offset = prev_match.offset + (itr.overlap ? 0 : ncodeunits(prev_match.match))
        while ((mat = match(itr.regex, itr.string, offset, opts_nonempty) === nothing) &&
               prevempty && offset <= sizeof(itr.string))
            offset = nextind(itr.string, offset)
            prevempty = false
        end
    else
        offset = (itr.overlap
                  ? nextind(itr.string, prev_match.offset)
                  : prev_match.offset + ncodeunits(prev_match.match))
        while ((mat = match(itr.regex, itr.string, offset, UInt32(0)) === nothing) &&
               prevempty && offset <= sizeof(itr.string))
            offset = nextind(itr.string, offset)
            prevempty = false
        end
    end
    (prev_match, mat)
end

eachmatch(re::RegexStr, str::AbstractString; overlap = false) =
    RegexStrMatchIterator(re, str, overlap)

## comparison ##

==(a::RegexTypes, b::RegexTypes) =
    a.pattern == b.pattern &&
    a.compile_options == b.compile_options &&
    a.match_options == b.match_options

## hash ##
hash(r::RegexStr, h::UInt) =
    hash(r.match_options,
         hash(r.compile_options,
              hash(r.pattern, h + UInt === UInt64 ? 0x67e195eb8555e72d : 0xe32373e4)))

_occurs_in(r::RegexTypes, s::AbstractString, off::Integer) =
    (compile(UTF8CSE, r) ; exec(r.regex, UTF8Str(s), off, r.match_options, r.match_data))
_occurs_in(r::RegexTypes, s::MaybeSub{<:Str{C}}, off::Integer) where {C<:Regex_CSEs} =
    (compile(C, r) ; exec(r.regex, s, off, r.match_options, r.match_data))

occurs_in(needle::RegexStr, hay::AbstractString; off::Integer=0) = _occurs_in(needle, hay, off)
occurs_in(needle::RegexTypes, hay::Str; off::Integer=0)          = _occurs_in(needle, hay, off)
occurs_in(needle::RegexTypes, hay::SubString{<:Str}; off::Integer=0) =
    _occurs_in(needle, hay, off)
end # module
