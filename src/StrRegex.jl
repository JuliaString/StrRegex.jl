__precompile__()
using Strs

"""
Regex functions for Str strings

Copyright 2018 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on julia/base/regex.jl and julia/base/pcre.jl
"""
module StrRegex

using Strs
using Strs: V6_COMPAT, MaybeSub, _not_found, bytoff, boundserr, basecse, CodeUnitTypes
using Strs: _LatinCSE, Latin_CSEs, Binary_CSEs, UCS2_CSEs, UTF8_CSEs, UTF32_CSEs
using Strs: is_latin, is_bmp, is_unicode
import Strs: find, occurs_in, _occurs_in

@static V6_COMPAT && (const Nothing = Void ; const Cvoid = Void )

using Base: RefValue, replace_err, SubstitutionString

using PCRE2
const PCRE = PCRE2

const deb = RefValue(false)

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

import Base: Regex, match, compile, eachmatch, show, getindex, eltype, start, done, next, ==, hash
@static if V6_COMPAT
    import Base: iteratorsize
    const IteratorSize = iteratorsize
else
    import Base: IteratorSize
end

export RegexStr, RegexStrMatch

const Binary_Regex_CSEs = Union{ASCIICSE,BinaryCSE,Text1CSE,Text2CSE,Text4CSE}
const Regex_CSEs = Union{Binary_Regex_CSEs,Latin_CSEs,UTF8_CSEs,UCS2_CSEs,UTF16CSE, UTF32_CSEs}

const _VALID = PCRE.NO_UTF_CHECK
const _UTF   = PCRE.UTF
const _UCP   = PCRE.UCP

_clear_opts(opt) = UInt32(opt) & ~(_VALID | _UTF | _UCP)

const comp_add   = UInt32[_VALID|_UTF|_UCP, _VALID|_UTF|_UCP, _VALID|_UTF|_UCP,
                          0, 0, 0, _VALID|_UCP, _VALID|_UCP, _UTF|_UCP]
const match_add  = UInt32[_VALID, _VALID, _VALID, 0, 0, 0, _VALID, _VALID, 0]

# Convert character set encoding types into indices
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

@noinline _check_compile(options) =
    (options & ~PCRE.COMPILE_MASK) == 0 ? UInt32(options) :
    throw(ArgumentError("invalid regex compile options: $options"))
@noinline _check_match(options) =
    (options & ~PCRE.MATCH_MASK) == 0 ? UInt32(options) :
    throw(ArgumentError("invalid regex match options: $options"))

function finalize! end

fin(exp) = (@static V6_COMPAT ? finalizer(exp, finalize!) : finalizer(finalize!, exp) ; exp)

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

# Match tables only need to be allocated for each size (8,16,32) and for each thread
mutable struct MatchTab
    match_data::NTuple{3, Ptr{Cvoid}}
    ovec::NTuple{3, Vector{Csize_t}}
    MatchTab() = new((C_NULL, C_NULL, C_NULL), (Csize_t[], Csize_t[], Csize_t[]))
end

Strs.is_empty(::Type{T}, mt::MatchTab) where {T<:CodeUnitTypes} =
    mt.match_data[codeunit_index(T)] == C_NULL

md_free(::Type{T}, md) where {T<:CodeUnitTypes} =
    md == C_NULL || PCRE.match_data_free(T, md)

get_match_data(::Type{T}, mt::MatchTab) where {T<:CodeUnitTypes} =
    mt.match_data[codeunit_index(T)]

function finalize!(mt::MatchTab)
    md_free(UInt8,  mt.match_data[1])
    md_free(UInt16, mt.match_data[2])
    md_free(UInt32, mt.match_data[3])
    mt.match_data = (C_NULL, C_NULL, C_NULL)
    mt.ovec = (Csize_t[], Csize_t[], Csize_t[])
end


mutable struct RegexStr
    pattern::String
    compile_options::UInt32
    match_options::UInt32
    table::NTuple{9, Ptr{Cvoid}}
    match::Vector{MatchTab}

    function RegexStr(pattern::AbstractString, compile_options::Integer, match_options::Integer)
        re = new(String(pattern), _check_compile(compile_options), _check_match(match_options),
                 (C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL),
                 [MatchTab() for i=1:Base.Threads.nthreads()])
        compile(cse(pattern), pattern, re)
        fin(re)
    end
end

const tabtype = (UInt8,UInt16,UInt32,UInt8,UInt16,UInt32,UInt8,UInt16,UInt8)

function finalize!(re::RegexStr)
    for (typ, r) in zip(tabtype, re.table)
        r == C_NULL || PCRE.code_free(typ, r)
    end
    re.table = (C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL)
    for m in re.match
        finalize!(m)
    end
end

_update_match(t, v, n) = n == 1 ? (v, t[2], t[3]) : n == 2 ? (t[1], v, t[3]) : (t[1], t[2], v)

_update_table(t, v, n) =
    (n == 1 ? v : t[1], n == 2 ? v : t[2], n == 3 ? v : t[3],
     n == 4 ? v : t[4], n == 5 ? v : t[5], n == 6 ? v : t[6],
     n == 7 ? v : t[7], n == 8 ? v : t[8], n == 9 ? v : t[9])

const RegexTypes = Union{Regex, RegexStr}

function _add_compile_options(flags)
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
    RegexStr(pattern, _add_compile_options(flags), DEFAULT_MATCH_OPTS)

Regex(pattern::MaybeSub{<:Str}, co, mo) = RegexStr(pattern, co, mo)
Regex(pattern::MaybeSub{<:Str}, flags::AbstractString) = RegexStr(pattern, flags)
Regex(pattern::MaybeSub{<:Str}) = RegexStr(pattern)

#=
# Yes, this is type piracy, but it is needed to make all string types work together easily
Regex(pattern::AbstractString, co::Integer, mo::Integer) = RegexStr(pattern, co, mo)
Regex(pattern::AbstractString, flags::AbstractString) = RegexStr(pattern, flags)
Regex(pattern::AbstractString) = RegexStr(pattern)
=#

function check_compile(::Type{C}, re::RegexStr) where {C<:CSE}
    try
        compile(C, re)
    catch ex
        (ex isa PCRE.PCRE2_Error && ex.errno in (134, 177)) || rethrow()
    end
end

function cmp_all(re::RegexStr)
    pat = re.pattern
    is_bmp(pat)   && check_compile(UCS2CSE,  re)
    is_latin(pat) && check_compile(LatinCSE, re)
    if is_unicode(pat)
        check_compile(RawUTF8CSE, re)
        check_compile(UTF32CSE, re)
        check_compile(UTF16CSE, re)
        check_compile(UTF8CSE,  re)
    end
    is_ascii(pat) && check_compile(ASCIICSE, re)
    re
end

export @R_str

macro R_str(pattern, flags...) cmp_all(RegexStr(pattern, flags...)) end

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

struct RegexStrMatch{T<:AbstractString}
    match::SubString{T}
    captures::Vector{Union{Nothing,SubString{T}}}
    offset::Int
    offsets::Vector{Int}
    regex::RegexStr
end

get_regex(::Type{C}, re::RegexStr) where {C<:CSE} = re.table[_match_type(C)]
get_regex(re::RegexStrMatch{T}) where {T<:AbstractString} = get_regex(cse(T), re.regex)
get_regex(re::RegexMatch) = re.regex.regex

function show(io::IO, m::RegexStrMatch{T}) where {T}
    print(io, "RegexStrMatch{$T}(")
    show(io, m.match)
    idx_to_capture_name = PCRE.capture_names(codeunit(T), get_regex(m))
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
    idx = PCRE.substring_number_from_name(codeunit(T), get_regex(m), name)
    idx <= 0 && error("no capture group named $name found in regex")
    m[idx]
end

function outtab(tab)
    for rt in tab
        print(rt == C_NULL ? "0 " : "0x" * Strs.outhex(reinterpret(UInt, rt)) * " ")
    end
end

getindex(m::RegexStrMatch, name::AbstractString) = m[Symbol(name)]

function compile(::Type{C}, pattern, regex::RegexStr) where {C<:Regex_CSEs}
    T = codeunit(C)
    nm = _match_type(C)
    # Todo!: This should be locked, so no race conditions with other threads
    if (re = regex.table[nm]) == C_NULL
        cvtcomp = regex.compile_options | comp_add[nm]
        pat = convert(Str{C,Nothing,Nothing,Nothing}, pattern)
        re = PCRE.compile(pat, cvtcomp)
        regex.table = _update_table(regex.table, re, nm)
        PCRE.jit_compile(T, re)
    end
    mt = regex.match[Threads.threadid()]
    if is_empty(T, mt)
        md = PCRE.match_data_create_from_pattern(T, re, C_NULL)
        ov = PCRE.get_ovec(T, md)
        index = codeunit_index(T)
        mt.match_data = _update_match(mt.match_data, md, index)
        mt.ovec       = _update_match(mt.ovec, ov, index)
    end
    regex
end

compile(::Type{C}, regex::RegexStr) where {C<:Regex_CSEs} = compile(C, regex.pattern, regex)

function compile(::Type{C}, regex::Regex) where {C<:Regex_CSEs}
    nm = _match_type(C)
    # Keep old match type in regex.extra (which doesn't seem to ever be used)
    oldind = reinterpret(UInt, regex.extra)%Int
    nm == oldind && return regex

    re = regex.regex
    regex.compile_options = cvtcomp = _clear_opts(regex.compile_options) | comp_add[nm]
    if oldind != 0
        S = tabtype[oldind]
        re == C_NULL || (PCRE.code_free(S, re); regex.regex = C_NULL)
    else
        S = Nothing
    end
    regex.regex = re = PCRE.compile(convert(Str{C,Nothing,Nothing,Nothing}, regex.pattern),
                                    cvtcomp)
    T = codeunit(C)
    PCRE.jit_compile(T, re)
    if S !== T
        S === Nothing || md_free(S, regex.match_data)
        regex.match_data = md = PCRE.match_data_create_from_pattern(T, re, C_NULL)
        regex.ovec = PCRE.get_ovec(T, md)
    end
    regex.extra = reinterpret(Ptr{Cvoid}, nm)
    regex
end

"""Get a thread-specific match context"""
function get_match_context(::Type{T}, tid=Threads.threadid()) where {T<:CodeUnitTypes}
    if (mc = match_context(T, tid)) == C_NULL
        mc = PCRE.match_context_create(T, C_NULL)
        js = PCRE.jit_stack_create(T, JIT_STACK_START_SIZE, JIT_STACK_MAX_SIZE, C_NULL)
        PCRE.jit_stack_assign(T, mc, C_NULL, js)
        # Todo!: This should be locked, so no race conditions with other threads
        MATCH_CONTEXT[tid] = _update_match(MATCH_CONTEXT[tid], mc, codeunit_index(T))
    end
    mc
end

function exec(C, re::RegexStr, subject, offset, options)
    #print("exec($re, \"$subject\", $offset, $options, match_data)")
    PCRE.@preserve subject begin
        pnt = pointer(subject)
        siz = ncodeunits(subject)
        T = eltype(pnt)
        #loc = bytoff(T, offset)
        0 <= offset <= siz || boundserr(subject, offset)
        tid = Threads.threadid()
        #print("\"$subject\", $offset, $(dump(re)) =>")
        rc = PCRE.match(T, re.table[_match_type(C)], pnt, siz, offset,
                        re.match_options | match_add[_match_type(C)] | _check_match(options),
                        get_match_data(T, re.match[tid]), get_match_context(T, tid))
        # rc == -1 means no match, -2 means partial match.
        #dump(re)
        rc < -2 && error("StrRegex.exec error: $(PCRE.err_message(rc))")
        rc >= 0
    end
end

function exec(C, re::Regex, subject, offset, options)
    PCRE.@preserve subject begin
        pnt = pointer(subject)
        siz = ncodeunits(subject)
        T = eltype(pnt)
        #loc = bytoff(T, offset)
        0 <= offset <= siz || boundserr(subject, offset)
        opts = re.match_options | match_add[_match_type(C)] | _check_match(options)
        rc = PCRE.match(T, re.regex, pnt, siz, offset, opts, re.match_data, get_match_context(T))
        # rc == -1 means no match, -2 means partial match.
        rc < -2 && error("StrRegex.exec error: $(PCRE.err_message(rc))")
        rc >= 0
    end
end

comp_exec(C, re, subject, offset, options) = exec(C, compile(C, re), subject, offset, options)

get_range(ov, str, i = 0) = Int(ov[2*i+1]+1) : prevind(str, Int(ov[2*i+2]+1))

get_ovec(::Type{<:Any}, re::Regex) = re.ovec
get_ovec(::Type{T}, re::RegexStr) where {T<:CodeUnitTypes} =
     re.match[Threads.threadid()].ovec[codeunit_index(T)]
get_ovec(::Type{C}, re::RegexStr) where {C<:CSE} = get_ovec(codeunit(C), re)


function _match(::Type{C}, re, str, idx, opts) where {C<:CSE}
    comp_exec(C, re, str, idx - 1, opts) || return nothing
    ov = get_ovec(C, re)
    n = div(length(ov),2) - 1
    rng = get_range(ov, str)
    mat = SubString(str, rng)
    cap = Union{Nothing,SubString{typeof(str)}}[ov[2*i+1] == PCRE.UNSET ? nothing :
                                                SubString(str, get_range(ov, str, i)) for i=1:n]
    RegexStrMatch(mat, cap, rng.start, Int[ ov[2*i+1]+1 for i=1:n ], re)
end

regex_type_error(T, S) =
    throw(ArgumentError("$T matching is not supported for $S; use UniStr(s) to convert"))

match(re::Regex, str::MaybeSub{<:Str}, idx::Integer, add_opts=0) =
    regex_type_error(Regex, typeof(str))

match(re::Regex, str::MaybeSub{<:Str{<:Regex_CSEs}}, idx::Integer, add_opts=0) =
    _match(basecse(str), re, str, Int(idx), UInt32(add_opts))

match(r::RegexStr, str::AbstractString, idx::Integer, add_opts=0) =
    regex_type_error(RegexStr, typeof(str))

match(re::RegexStr, str::MaybeSub{<:Str}, idx::Integer, add_opts=0) =
    _match(basecse(str), re, str, Int(idx), UInt32(add_opts))

match(re::RegexStr, str::MaybeSub{String}, idx::Integer, add_opts=0) =
    _match(RawUTF8CSE, re, str, Int(idx), UInt32(add_opts))


match(re::Regex, str::MaybeSub{<:Str})   = match(re, str, 1)
match(re::RegexStr, str::AbstractString) = match(re, str, 1)

@inline __find(::Type{C}, re, str, idx) where {C} =
    comp_exec(C, re, str, idx, 0) ? get_range(get_ovec(C, re), str) : _not_found

@inline _find(::Type{C}, re, str) where {C} = __find(C, re, str, 0)

@inline _find(::Type{C}, re, str, idx) where {C} =
    (idx-1 <= ncodeunits(str)
     ? __find(C, re, str, idx-1)
     : (@boundscheck boundserr(str, idx) ; return _not_found))

find(::Type{Fwd}, re::RegexTypes, str::AbstractString, idx::Integer) =
    regex_type_error(typeof(re), typeof(str))

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

function _write_capture(io, ::Type{C}, group::Integer, md) where {C<:CSE}
    T = codeunit(C)
    len = PCRE.sub_length_bynumber(T, md, group)
    buf, out = Strs._allocate(T, len)
    PCRE.sub_copy_bynumber(T, md, group, out, len+1)
    print(io, Str(C, buf))
end

function Base._replace(io, repl_s::SubstitutionString,
                       str::T, r, re::RegexStr) where {T<:AbstractString}
    SUB_CHAR = '\\'
    GROUP_CHAR = 'g'
    LBRACKET = '<'
    RBRACKET = '>'
    tid = Threads.threadid()
    C = cse(T)
    CU = codeunit(C)
    md = get_match_data(CU, re.match[tid])
    regex = get_regex(C, re)
    repl = repl_s.string
    pos = 1
    lst = lastindex(repl)
    # This needs to be careful with writes!
    while pos <= lst
        ch = repl[pos]
        if ch == SUB_CHAR
            nxt = nextind(repl, pos)
            nxt > lst && replace_err(repl)
            ch = repl[nxt]
            if ch == SUB_CHAR
                print(io, SUB_CHAR)
                pos = nextind(repl, nxt)
            elseif is_digit(ch)
                group = parse(Int, ch)
                pos = nextind(repl, nxt)
                while pos <= lst && (ch = repl[pos]; is_digit(ch))
                    group = 10 * group + parse(Int, ch)
                    pos = nextind(repl, pos)
                end
                _write_capture(io, C, group, md)
            elseif ch == GROUP_CHAR
                pos = nextind(repl, nxt)
                (pos > lst || repl[pos] != LBRACKET) && replace_err(repl)
                pos = nextind(repl, pos)
                pos > lst && replace_err(repl)
                groupstart = pos
                while repl[pos] != RBRACKET
                    pos = nextind(repl, pos)
                    pos > pos && replace_err(repl)
                end
                #  TODO: avoid this allocation
                groupname = SubString(repl, groupstart, prevind(repl, pos))
                if all(isdigit, groupname)
                    _write_capture(io, C, parse(Int, groupname), md)
                else
                    gn = convert(T, groupname)
                    group = PCRE.substring_number_from_name(CU, regex, gn)
                    group < 0 && replace_err("Group $groupname not found in regex $re")
                    _write_capture(io, C, group, md)
                end
                pos = nextind(repl, pos)
            else
                replace_err(repl)
            end
        else
            print(io, ch)
            pos = nextind(repl, pos)
        end
    end
end

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
    if is_empty(prev_match.match)
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

@static if V6_COMPAT
eachmatch(re::RegexStr, str::AbstractString, ov::Union{Bool,Nothing}=nothing; overlap = false) =
    RegexStrMatchIterator(re, str, ov === nothing ? overlap : ov)
else
eachmatch(re::RegexStr, str::AbstractString; overlap = false) =
    RegexStrMatchIterator(re, str, overlap)
end

using Strs: __split, __rsplit, __replace, splitarr, checkkeep
import Strs: split, rsplit, replace

const MS_Str    = MaybeSub{<:Str}
const MS_String = MaybeSub{String}

split(str::MS_Str, splitter::Regex;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) =
    __split(str, splitter, limit, checkkeep(keepempty, keep, :split), splitarr(str))
split(str::MS_Str, splitter::RegexStr;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) =
    __split(str, splitter, limit, checkkeep(keepempty, keep, :split), splitarr(str))
split(str::MS_String, splitter::RegexStr;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) =
    __split(str, splitter, limit, checkkeep(keepempty, keep, :split), splitarr(str))

rsplit(str::MS_Str, splitter::Regex;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) =
    __rsplit(str, splitter, limit, checkkeep(keepempty, keep, :rsplit), splitarr(str))
rsplit(str::MS_Str, splitter::RegexStr;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) =
    __rsplit(str, splitter, limit, checkkeep(keepempty, keep, :rsplit), splitarr(str))
rsplit(str::MS_String, splitter::RegexStr;
      limit::Integer=0, keepempty::Bool=true, keep::Union{Nothing,Bool}=nothing) =
    __rsplit(str, splitter, limit, checkkeep(keepempty, keep, :rsplit), splitarr(str))

replace(str::MS_Str, pat_repl::Pair{Regex}; count::Integer=typemax(Int)) =
    __replace(str, pat_repl; count=count)
replace(str::MS_Str, pat_repl::Pair{RegexStr}; count::Integer=typemax(Int)) =
    __replace(str, pat_repl; count=count)
replace(str::String, pat_repl::Pair{RegexStr}; count::Integer=typemax(Int)) =
    __replace(str, pat_repl; count=count)
replace(str::SubString{String}, pat_repl::Pair{RegexStr}; count::Integer=typemax(Int)) =
    __replace(str, pat_repl; count=count)
@static if V6_COMPAT
replace(str::MS_Str, pat::Regex, repl; count::Integer=typemax(Int)) =
    __replace(str, pat => repl; count=count)
replace(str::MS_Str, pat::RegexStr, repl; count::Integer=typemax(Int)) =
    __replace(str, pat => repl; count=count)
replace(str::MS_String, pat::RegexStr, repl; count::Integer=typemax(Int)) =
    __replace(str, pat => repl; count=count)
end


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
    comp_exec(UTF8CSE, r, UTF8Str(s), off, 0)
_occurs_in(r::RegexTypes, s::MaybeSub{<:Str{C}}, off::Integer) where {C<:Regex_CSEs} =
    comp_exec(C, r, s, off, 0)

occurs_in(needle::RegexStr, hay::AbstractString; off::Integer=0) = _occurs_in(needle, hay, off)
occurs_in(needle::Regex, hay::MaybeSub{<:Str}; off::Integer=0)   = _occurs_in(needle, hay, off)

Base.contains(hay::AbstractString, pat::RegexStr) = occurs_in(pat, hay)

@static if V6_COMPAT && !method_exists(contains, (AbstractString, Regex))
    Base.contains(hay::AbstractString, pat::Regex) = occurs_in(pat, hay)
end

end # module StrRegex
