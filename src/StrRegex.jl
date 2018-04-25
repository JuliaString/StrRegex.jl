#=
Regex functions for Str strings

Copyright 2018 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on julia/base/regex.jl and julia/base/pcre.jl
=#

__precompile__()
module StrRegex

using Strs
using Strs: V6_COMPAT, MaybeSub, _not_found, bytoff, boundserr, basecse, CodeUnitTypes
using Strs: _LatinCSE, Latin_CSEs, Binary_CSEs, UCS2_CSEs, UTF8_CSEs, UTF32_CSEs
using Strs: is_latin, is_bmp, is_unicode
import Strs: find, occurs_in, _occurs_in

@static V6_COMPAT && (const Nothing = Void ; const Cvoid = Void )

using Base: RefValue

using PCRE2
const PCRE = PCRE2

const MATCH_CONTEXT = [(C_NULL,C_NULL,C_NULL)]

codeunit_index(::Type{UInt8})  = 1
codeunit_index(::Type{UInt16}) = 2
codeunit_index(::Type{UInt32}) = 3

match_context(::Type{T}, tid) where {T<:CodeUnitTypes} =
    MATCH_CONTEXT[tid][codeunit_index(T)]

const JIT_STACK_START_SIZE = 32768
const JIT_STACK_MAX_SIZE = 1048576

function __init__()
    if (n = Base.Threads.nthreads()) != 1
        resize!(MATCH_CONTEXT, n)
        fill!(MATCH_CONTEXT, (C_NULL, C_NULL, C_NULL))
    end
end

# UTF and NO_UTF_CHECK are based on the string type
const DEFAULT_COMPILER_OPTS = PCRE.ALT_BSUX
const DEFAULT_MATCH_OPTS    = 0

import Base: Regex, match, compile, eachmatch, show, getindex, eltype, start, done, next, ==
import Base: hash, IteratorSize

export RegexStr, RegexStrMatch

const Binary_Regex_CSEs = Union{ASCIICSE,BinaryCSE,Text1CSE,Text2CSE,Text4CSE}
const Regex_CSEs = Union{Binary_Regex_CSEs,Latin_CSEs,UTF8_CSEs,UCS2_CSEs,UTF16CSE, UTF32_CSEs}

# Default is to act as if validated UTF
_cvt_compile(::Type{<:CSE}, opt) = UInt32(opt) | PCRE.NO_UTF_CHECK | PCRE.UTF | PCRE.UCP
_cvt_match(::Type{<:CSE}, opt)   = UInt32(opt) | PCRE.NO_UTF_CHECK

_cvt_compile(::Type{Binary_Regex_CSEs}, opt) = UInt32(opt) & ~(PCRE2.UTF | PCRE.UCP)

_cvt_compile(::Type{<:Latin_CSEs}, opt) = UInt32(opt) | PCRE.NO_UTF_CHECK | PCRE.UCP
_cvt_match(::Type{<:Latin_CSEs}, opt)   = UInt32(opt) | PCRE.NO_UTF_CHECK
_cvt_compile(::Type{<:UCS2_CSEs}, opt)  = UInt32(opt) | PCRE.NO_UTF_CHECK | PCRE.UCP
_cvt_match(::Type{<:UCS2_CSEs}, opt)    = UInt32(opt) | PCRE.NO_UTF_CHECK

# String type is not validated
_cvt_compile(::Type{RawUTF8CSE}, opt) = (UInt32(opt) & ~PCRE.NO_UTF_CHECK) | PCRE.UTF | PCRE.UCP
_cvt_match(::Type{RawUTF8CSE}, opt)   = (UInt32(opt) & ~PCRE.NO_UTF_CHECK)

# Handle being passed string type
_cvt_compile(::Type{T}, opt) where {T<:AbstractString} = _cvt_compile(cse(T), opt)
_cvt_match(::Type{T}, opt) where {T<:AbstractString} = _cvt_match(cse(T), opt)


@noinline _compile_opts(::Type{C}, options) where {C} =
    (options & ~PCRE.COMPILE_MASK) == 0 ? _cvt_compile(C, options) :
    throw(ArgumentError("invalid regex compile options: $options"))
@noinline _match_opts(::Type{C}, options) where {C} =
    (options & ~PCRE.MATCH_MASK) == 0 ? _cvt_match(C, options) :
    throw(ArgumentError("invalid regex match options: $options"))

_match_type(::Type{UTF8CSE})       = 1
_match_type(::Type{UTF16CSE})      = 2
_match_type(::Type{<:UTF32_CSEs})  = 3
_match_type(::Type{ASCIICSE})      = 4
_match_type(::Type{<:Binary_CSEs}) = 4
_match_type(::Type{Text2CSE})      = 5
_match_type(::Type{Text4CSE})      = 6
_match_type(::Type{<:Latin_CSEs})  = 7
_match_type(::Type{<:UCS2_CSEs})   = 8
_match_type(::Type{RawUTF8CSE})    = 9


function finalize! end

fin(re) = (@static V6_COMPAT ? finalizer(re, finalize!) : finalizer(finalize!, re) ; re)

#=
# Want to split things up so that we can better handle threads, 3 sizes,
# and UTF/no UTF, UCP/no UCP
# When done via a macro, want to compile for UTF8,ASCIIStr,LatinStr,UCS2Str,UTF32Str

mutable struct _Regex{T<:AbstractString}
    pattern::T
    compile_options::UInt32
    match_options::UInt32
    regex::Ptr{Cvoid} # Current compiled regex
end

# There are 9 valid combinations of size (8,16,32), UTF/no-UTF, UCP/no-UCP, and NO_UTF_CHECK
# 
# 1 - UTF8                 8,UTF,UCP
# 2 - UTF16               16,UTF,UCP
# 3 - UTF32/_UTF32        32,UTF,UCP

# 4 - Text1/Binary/ASCII   8
# 5 - Text2               16
# 6 - Text4               32

# 7 - Latin/_Latin         8,UCP
# 8 - UCS2/_UCS2          16,UCP
# 9 - RawUTF8CSE           8,UTF,UCP,NO_UTF_CHECK

mutable struct RegexStr
    table::Vector{}
    ovec::Vector{Csize_t}   # This needs to be thread specific, can share in this regex
    match_data::Ptr{Cvoid}  # This needs to be thread specific

    RegexStr(::Type{C}, pattern::AbstractString, comp_opt::UInt32, match_opt::UInt32) where {C} =
        fin(new(pattern, _compile_opts(C, comp_opt), _match_opts(C, match_opt),
                C_NULL, 0, Csize_t[], C_NULL,
                (C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL)))
end
=#

mutable struct RegexStr
    pattern::AbstractString
    compile_options::UInt32
    match_options::UInt32
    regex::Ptr{Cvoid} # Current compiled regex
    match_type::Int
    ovec::Vector{Csize_t}
    match_data::Ptr{Cvoid}
    table::NTuple{9, Ptr{Cvoid}}

    function RegexStr(pattern::AbstractString, comp_opt::UInt32, match_opt::UInt32)
        c = cse(pattern)
        re = new(pattern, _compile_opts(c, comp_opt), _match_opts(c, match_opt),
                 C_NULL, 0, Csize_t[], C_NULL,
                 (C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL))
        fin(re)
        re
    end
end

RegexStr(pattern::AbstractString, compile_options::Integer, match_options::Integer) =
    compile(cse(pattern), RegexStr(pattern, UInt32(compile_options), UInt32(match_options)))

_update_match(t, re, n) = n == 1 ? (re, t[2], t[3]) : n == 2 ? (t[1], re, t[3]) : (t[1], t[2], re)

_update_table(t, re, n) =
    (n == 1 ? re : t[1], n == 2 ? re : t[2], n == 3 ? re : t[3],
     n == 4 ? re : t[4], n == 5 ? re : t[5], n == 6 ? re : t[6],
     n == 7 ? re : t[7], n == 8 ? re : t[8], n == 9 ? re : t[9])

cf(typ, reg) = reg == C_NULL || PCRE.code_free(typ, reg)

const tabtype = (UInt8,UInt16,UInt32,UInt8,UInt16,UInt32,UInt8,UInt16,UInt8)

function finalize!(re)
    re.match_data == C_NULL ||
        (PCRE.match_data_free(codeunit(re.pattern), re.match_data); re.match_data = C_NULL)
    if re.regex != C_NULL
        for i = 1:length(re.table)
            (reg = re.table[i]) == C_NULL || PCRE.code_free(tabtype[i], reg)
        end
        re.regex = C_NULL
        re.table = (C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL)
    end
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

Regex(pattern::MaybeSub{<:Str}, co, mo) = RegexStr(pattern, co, mo)
Regex(pattern::MaybeSub{<:Str}, flags::AbstractString) = RegexStr(pattern, flags)
Regex(pattern::MaybeSub{<:Str}) = RegexStr(pattern)

#=
# Yes, this is type piracy, but it is needed to make all string types work together easily
Regex(pattern::AbstractString, co::Integer, mo::Integer) = RegexStr(pattern, co, mo)
Regex(pattern::AbstractString, flags::AbstractString) = RegexStr(pattern, flags)
Regex(pattern::AbstractString) = RegexStr(pattern)
=#

function cmp_all(re::RegexStr)
    pat = re.pattern
    is_bmp(pat)   && compile(UCS2CSE,  re)
    is_latin(pat) && compile(LatinCSE, re)
    if is_unicode(pat)
        compile(RawUTF8CSE, re)
        compile(UTF32CSE, re)
        compile(UTF16CSE, re)
        compile(UTF8CSE,  re)
    end
    is_ascii(pat) && compile(ASCIICSE, re)
    re
end

export @R_str

macro R_str(pattern, flags...) cmp_all(RegexStr(pattern, flags...)) end

function show(io::IO, re::RegexStr)
    imsx = PCRE.CASELESS|PCRE.MULTILINE|PCRE.DOTALL|PCRE.EXTENDED
    opts = re.compile_options
    if (opts & ~imsx) == _cvt_compile(typeof(re.pattern), DEFAULT_COMPILER_OPTS)
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

struct RegexStrMatch{T<:AbstractString}
    match::SubString{T}
    captures::Vector{Union{Nothing,SubString{T}}}
    offset::Int
    offsets::Vector{Int}
    regex::RegexStr
end

function show(io::IO, m::RegexStrMatch{T}) where {T}
    print(io, "RegexStrMatch{$T}(")
    show(io, m.match)
    idx_to_capture_name = PCRE.capture_names(codeunit(T), m.regex.regex)
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
function getindex(m::RegexStrMatch{T}, name::Symbol) where {T}
    idx = PCRE.substring_number_from_name(codeunit(T), m.regex.regex, name)
    idx <= 0 && error("no capture group named $name found in regex")
    m[idx]
end

getindex(m::RegexStrMatch, name::AbstractString) = m[Symbol(name)]

function compile(::Type{C}, regex::RegexStr) where {C<:Regex_CSEs}
    T = codeunit(C)
    if (nm = _match_type(C)) != regex.match_type
        regex.match_type = nm
        if (re = regex.table[nm]) == C_NULL
            regex.compile_options = cvtcomp = _cvt_compile(C, regex.compile_options)
            regex.pattern = pat = convert(Str{C,Nothing,Nothing,Nothing}, regex.pattern)
            #println("nm=$nm, comp=$cvtcomp, $(typeof(pat))")
            regex.regex = re = PCRE.compile(pat, cvtcomp)
            regex.table = _update_table(regex.table, re, nm)
            #println(" => $re, $(regex.table)")
            PCRE.jit_compile(T, re)
        end
        if regex.match_data == C_NULL
            regex.match_data = PCRE.match_data_create_from_pattern(T, re, C_NULL)
            regex.ovec = PCRE.get_ovec(T, regex.match_data)
        end
    end
    regex
end

function compile(::Type{C}, regex::Regex) where {C<:Regex_CSEs}
    re = regex.regex
    # ASCII is compatible with all (current) types, don't recompile
    C == ASCIICSE && re != C_NULL && return regex
    oldopt = regex.compile_options
    cvtcomp = _cvt_compile(C, oldopt)
    T = codeunit(C)
    S = codeunit(regex.pattern)
    if cvtcomp != oldopt || T !== S
        regex.compile_options = cvtcomp
        re == C_NULL || (PCRE.code_free(S, re); regex.regex = re = C_NULL)
    end
    if re == C_NULL
        regex.pattern = pat = convert(Str{C,Nothing,Nothing,Nothing}, regex.pattern)
        regex.regex = re = PCRE.compile(pat, cvtcomp)
        PCRE.jit_compile(T, re)
    end
    if regex.match_data == C_NULL
        regex.match_data = PCRE.match_data_create_from_pattern(T, re, C_NULL)
        regex.ovec = PCRE.get_ovec(T, regex.match_data)
    end
    regex
end

"""Get a thread-specific match context"""
function get_match_context(::Type{T}) where {T<:CodeUnitTypes}
    tid = Threads.threadid()
    if (mc = match_context(T, tid)) == C_NULL
        mc = PCRE.match_context_create(T, C_NULL)
        js = PCRE.jit_stack_create(T, JIT_STACK_START_SIZE, JIT_STACK_MAX_SIZE, C_NULL)
        PCRE.jit_stack_assign(T, mc, C_NULL, js)
        _update_match(MATCH_CONTEXT[tid], mc, codeunit_index(T))
    end
    mc
end

function exec(C, re, subject, offset, options)
    #print("exec($re, \"$subject\", $offset, $options, match_data)")
    md = re.match_data
    PCRE.@preserve subject begin
        pnt = pointer(subject)
        siz = sizeof(subject)
        T = eltype(pnt)
        loc = bytoff(T, offset)
        0 <= loc <= siz || boundserr(subject, offset)
        rc = PCRE.match(T, re.regex, pnt, siz, loc, _cvt_match(C, re.match_options | options), md,
                        get_match_context(T))
        if rc < -2
            println("exec($C, $re, \"$subject\", $offset, $(Strs.outhex(options)) => $rc")
            println("$T $pnt $siz $loc")
            dump(re)
        end
        # rc == -1 means no match, -2 means partial match.
        rc < -2 && error("StrRegex.exec error: $(PCRE.err_message(rc))")
        rc >= 0
    end
end

function _match(::Type{C}, re, str, idx, add_opts) where {C<:CSE}
    #println("_match($C, $re, $str, $idx, $add_opts)")
    exec(C, compile(C, re), str, idx - 1, add_opts) || return nothing
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

regex_type_error() = throw(ArgumentError(
    "regex matching is only available for String or Str types " *
    "(and SubString of those types); use UniStr(s) to convert"))

match(r::RegexStr, s::AbstractString, i::Integer, add_opts::UInt32=UInt32(0)) = regex_type_error()

match(re::Regex, str::MaybeSub{<:Str})   = match(re, str, 1)
match(re::RegexStr, str::AbstractString) = match(re, str, 1)

@inline __find(::Type{C}, re, str, idx) where {C} =
    (exec(C, compile(C, re), str, idx, 0)
     ? ((Int(re.ovec[1]) + 1) : prevind(str, Int(re.ovec[2]) + 1))
     : _not_found)

@inline _find(::Type{C}, re, str) where {C} = __find(C, re, str, 0)

@inline _find(::Type{C}, re, str, idx) where {C} =
    (idx-1 <= ncodeunits(str)
     ? __find(C, re, str, idx-1)
     : (@boundscheck boundserr(str, idx) ; return _not_found))

find(::Type{Fwd}, re::RegexTypes, str::AbstractString, idx::Integer) = regex_type_error()

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

struct RegexStrMatchIterator{T<:AbstractString}
    regex::RegexStr
    string::T
    overlap::Bool
end
RegexStrMatchIterator(r::RegexStr, s::AbstractString) = RegexStrMatchIterator(r, s, false)

compile(itr::RegexStrMatchIterator{T}) where {T} = (compile(cse(T), itr.regex); itr)
eltype(::Type{RegexStrMatchIterator{T}}) where {T} = RegexStrMatch{T}
start(itr::RegexStrMatchIterator) = match(itr.regex, itr.string, 1, UInt32(0))
done(itr::RegexStrMatchIterator, prev_match) = (prev_match === nothing)
IteratorSize(::Type{RegexStrMatchIterator{T}}) where {T<:AbstractString} = Base.SizeUnknown()

# Assumes prev_match is not nothing
@static if true
function next(itr::RegexStrMatchIterator, prev_match)
    prevempty = isempty(prev_match.match)

    if itr.overlap
        if !prevempty
            offset = nextind(itr.string, prev_match.offset)
        else
            offset = prev_match.offset
        end
    else
        offset = prev_match.offset + ncodeunits(prev_match.match)
    end

    opts_nonempty = UInt32(PCRE.ANCHORED | PCRE.NOTEMPTY_ATSTART)
    while true
        mat = match(itr.regex, itr.string, offset,
                    prevempty ? opts_nonempty : UInt32(0))

        if mat === nothing
            if prevempty && offset <= sizeof(itr.string)
                offset = nextind(itr.string, offset)
                prevempty = false
                continue
            else
                break
            end
        else
            return (prev_match, mat)
        end
    end
    (prev_match, nothing)
end
else
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
end

eachmatch(re::RegexStr, str::AbstractString; overlap = false) =
    RegexStrMatchIterator(re, str, overlap)

using Strs: __split, __rsplit, __replace, splitarr, checkkeep
import Strs: split, rsplit, replace

split(str::MaybeSub{<:Str{C}}, splitter::RegexStr;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) where {C<:CSE} =
    __split(str, splitter, limit, checkkeep(keepempty, keep, :split), splitarr(C))
rsplit(str::MaybeSub{<:Str{C}}, splitter::RegexStr;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) where {C<:CSE} =
    __rsplit(str, splitter, limit, checkkeep(keepempty, keep, :rsplit), splitarr(C))
replace(str::MaybeSub{<:Str}, pat_repl::Pair{RegexStr}; count::Integer=typemax(Int)) =
    __replace(str, pat_repl; count=count)

split(str::MaybeSub{<:Str{C}}, splitter::Regex;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) where {C<:CSE} =
    __split(str, splitter, limit, checkkeep(keepempty, keep, :split), splitarr(C))
rsplit(str::MaybeSub{<:Str{C}}, splitter::Regex;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) where {C<:CSE} =
    __rsplit(str, splitter, limit, checkkeep(keepempty, keep, :rsplit), splitarr(C))
replace(str::MaybeSub{<:Str}, pat_repl::Pair{Regex}; count::Integer=typemax(Int)) =
    __replace(str, pat_repl; count=count)

split(str::String, splitter::RegexStr;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) =
    __split(str, splitter, limit, checkkeep(keepempty, keep, :split), splitarr(RawUTF8CSE))
rsplit(str::String, splitter::RegexStr;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) =
    __rsplit(str, splitter, limit, checkkeep(keepempty, keep, :rsplit), splitarr(RawUTF8CSE))
replace(str::String, pat_repl::Pair{RegexStr}; count::Integer=typemax(Int)) =
    __replace(str, pat_repl; count=count)

split(str::SubString{String}, splitter::RegexStr;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) =
    __split(str, splitter, limit, checkkeep(keepempty, keep, :split), splitarr(RawUTF8CSE))
rsplit(str::SubString{String}, splitter::RegexStr;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) =
    __rsplit(str, splitter, limit, checkkeep(keepempty, keep, :rsplit), splitarr(RawUTF8CSE))
replace(str::SubString{String}, pat_repl::Pair{RegexStr}; count::Integer=typemax(Int)) =
    __replace(str, pat_repl; count=count)


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
    exec(UTF8CSE, compile(UTF8CSE, r), UTF8Str(s), off, 0)
_occurs_in(r::RegexTypes, s::MaybeSub{<:Str{C}}, off::Integer) where {C<:Regex_CSEs} =
    exec(C, compile(C, r), s, off, 0)

occurs_in(needle::RegexStr, hay::AbstractString; off::Integer=0) = _occurs_in(needle, hay, off)
occurs_in(needle::Regex, hay::MaybeSub{<:Str}; off::Integer=0)   = _occurs_in(needle, hay, off)

end # module
