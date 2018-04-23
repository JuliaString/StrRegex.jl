# This file includes code that was formerly a part of Julia.
# License is MIT: LICENSE.md

using Strs
using Strs: V6_COMPAT
import Strs: check_string, UTF_ERR_SHORT, UnicodeError, codepoint_adj, codepoint_rng

using StrRegex

@static V6_COMPAT ? (using Base.Test) : (using Test, Random, Unicode)

const IndexError = isdefined(Base, :StringIndexError) ? StringIndexError : UnicodeError

# Add definitions not present in v0.6.2 for GenericString
@static if V6_COMPAT
    Strs.ncodeunits(s::GenericString) = ncodeunits(s.string)
    Strs.codeunit(s::GenericString) = codeunit(s.string)
    Strs.codeunit(s::GenericString, i::Integer) = codeunit(s.string, i)
end
const CodeUnits = @static V6_COMPAT ? Strs.CodeUnits : Base.CodeUnits

# Should test GenericString also, once overthing else is working
const UnicodeStringTypes = (String, UTF8Str, )
    # (String, UTF16Str, UTF32Str, UniStr, UTF8Str)
const ASCIIStringTypes = (String, UTF8Str, ASCIIStr, LatinStr)
    #    (UnicodeStringTypes..., ASCIIStr, LatinStr, UCS2Str)

function test2(str, list)
    for (pat, res) in list
#        (r = fnd(First, pat, str)) == res ||
#            println("fnd(First, $(typeof(pat)):\"$pat\", $(typeof(str)):\"$str\") => $r != $res")
        @test fnd(First, pat, str) == res
    end
end

function test3(str, list)
    for (pat, beg, res) in list
#        (r = fnd(Fwd, pat, str, beg)) == res ||
#            println("fnd(Fwd, $(typeof(pat)):\"$pat\", $(typeof(str)):\"$str\", $beg) => $r != $res")
        @test fnd(Fwd, pat, str, beg) == res
    end
end

const fbbstr = "foo,bar,baz"
const astr = "Hello, world.\n"
const u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

@testset "ASCII Regex Tests" begin
    for T in ASCIIStringTypes
        fbb = T(fbbstr)
        str = T(astr)
        @testset "str: $T" begin
            # string forward search with a single-char regex
            let pats = (r"x", r"H", r"l", r"\n"),
                res  = (0:-1, 1:1, 3:3, 14:14)
                test2(str, zip(pats, res))
            end
            let pats = (r"H", r"l", r"l", r"l", r"\n"),
                pos  = (  2,    4,    5,   12,    14), # Was 15 for fndnext
                res  = (0:-1, 4:4,11:11, 0:-1, 14:14)
                test3(str, zip(pats, pos, res))
            end
            i = 1
            while i <= ncodeunits(str)
                @test fnd(Fwd, r"."s, str, i) == i:i
                # string forward search with a zero-char regex
                @test fnd(Fwd, r"", str, i) == i:i-1
                i = nextind(str, i)
            end
            let pats = (r"xx", r"fo", r"oo", r"o,", r",b", r"az"),
                res  = ( 0:-1,   1:2,   2:3,   3:4,   4:5, 10:11)
                test2(fbb, zip(pats, res))
            end
            let pats = (r"fo", r"oo", r"o,", r",b", r",b", r"az"),
                pos  = (    3,     4,     5,     6,    10,    11),
                res  = ( 0:-1,  0:-1,  0:-1,   8:9,  0:-1,  0:-1) # was 12 for fndnext
                test3(fbb, zip(pats, pos, res))
            end
        end
    end
end

@testset "Unicode Regex Tests" begin
    for T in UnicodeStringTypes
        @testset "str: $T" begin
            str = T(u8str)
            @testset "Regex" begin
                let pats = (r"z", r"∄", r"∀", r"∃", r"x", r"ε"),
                    res  = (0:-1, 0:-1, 1:1, 13:13,26:26,  5:5)
                    test2(str, zip(pats, res))
                end
                let pats = (r"∀", r"∃", r"x", r"x", r"ε", r"ε"),
                    pos  = (   4,   16,   27,   44,    7,   54), # was 56 for fndnext
                    res  = (0:-1, 0:-1,43:43, 0:-1,54:54,54:54)  # was 0:-1 for last
                    test3(str, zip(pats, pos, res))
                end
                @test fnd(First, r"∀", str)  == fnd(First, r"\u2200", str)
                @test fnd(Fwd, r"∀", str, 4) == fnd(Fwd, r"\u2200", str, 4)
                i = 1
                while i <= ncodeunits(str)
                    @test fnd(Fwd, r"."s, str, i) == i:i
                    # string forward search with a zero-char regex
                    @test fnd(Fwd, r"", str, i) == i:i-1
                    i = nextind(str, i)
                end
            end
        end
    end
end

const RegexStrings = (ASCIIStr, BinaryStr, Text1Str, LatinStr, Strs._LatinStr, UTF8Str)

@static if V6_COMPAT
    collect_eachmatch(re, str; overlap=false) =
        [m.match for m in collect(eachmatch(re, str, overlap))]
else
    collect_eachmatch(re, str; overlap=false) =
        [m.match for m in collect(eachmatch(re, str, overlap = overlap))]
end

@testset "UTF8Str Regex" begin
    # Proper unicode handling
    @test match(r"∀∀", UTF8Str("∀x∀∀∀")).match == "∀∀"
    @test collect_eachmatch(r".\s", UTF8Str("x \u2200 x \u2203 y")) == ["x ", "∀ ", "x ", "∃ "]
end

@testset "Regex" begin
    for T in RegexStrings
        @test collect_eachmatch(r"a?b?", T("asbd")) == ["a","","b","",""] ==
            collect_eachmatch(r"""a?b?""", T("asbd"))
        @test collect_eachmatch(r"a?b?", T("asbd"), overlap=true) == ["a","","b","",""]
        @test collect_eachmatch(r"\w+", T("hello"), overlap=true) ==
            ["hello","ello","llo","lo","o"]
        @test collect_eachmatch(r"(\w+)(\s*)", T("The dark side of the moon")) ==
            ["The ", "dark ", "side ", "of ", "the ", "moon"]
        @test collect_eachmatch(r"", T("")) == [""]
        @test collect_eachmatch(r"", T(""), overlap=true) == [""]
        @test collect_eachmatch(r"aa", T("aaaa")) == ["aa", "aa"]
        @test collect_eachmatch(r"aa", T("aaaa"), overlap=true) == ["aa", "aa", "aa"]
        @test collect_eachmatch(r"", T("aaa")) == ["", "", "", ""]
        @test collect_eachmatch(r"", T("aaa"), overlap=true) == ["", "", "", ""]
        @test collect_eachmatch(r"GCG", T("GCGCG")) == ["GCG"]
        @test collect_eachmatch(r"GCG", T("GCGCG"),overlap=true) == ["GCG","GCG"]

# Issue 8278
target = """71.163.72.113 - - [30/Jul/2014:16:40:55 -0700] "GET emptymind.org/thevacantwall/wp-content/uploads/2013/02/DSC_006421.jpg HTTP/1.1" 200 492513 "http://images.search.yahoo.com/images/view;_ylt=AwrB8py9gdlTGEwADcSjzbkF;_ylu=X3oDMTI2cGZrZTA5BHNlYwNmcC1leHAEc2xrA2V4cARvaWQDNTA3NTRiMzYzY2E5OTEwNjBiMjc2YWJhMjkxMTEzY2MEZ3BvcwM0BGl0A2Jpbmc-?back=http%3A%2F%2Fus.yhs4.search.yahoo.com%2Fyhs%2Fsearch%3Fei%3DUTF-8%26p%3Dapartheid%2Bwall%2Bin%2Bpalestine%26type%3Dgrvydef%26param1%3D1%26param2%3Dsid%253Db01676f9c26355f014f8a9db87545d61%2526b%253DChrome%2526ip%253D71.163.72.113%2526p%253Dgroovorio%2526x%253DAC811262A746D3CD%2526dt%253DS940%2526f%253D7%2526a%253Dgrv_tuto1_14_30%26hsimp%3Dyhs-fullyhosted_003%26hspart%3Dironsource&w=588&h=387&imgurl=occupiedpalestine.files.wordpress.com%2F2012%2F08%2F5-peeking-through-the-wall.jpg%3Fw%3D588%26h%3D387&rurl=http%3A%2F%2Fwww.stopdebezetting.com%2Fwereldpers%2Fcompare-the-berlin-wall-vs-israel-s-apartheid-wall-in-palestine.html&size=49.0KB&name=...+%3Cb%3EApartheid+wall+in+Palestine%3C%2Fb%3E...+%7C+Or+you+go+peeking+through+the+%3Cb%3Ewall%3C%2Fb%3E&p=apartheid+wall+in+palestine&oid=50754b363ca991060b276aba291113cc&fr2=&fr=&tt=...+%3Cb%3EApartheid+wall+in+Palestine%3C%2Fb%3E...+%7C+Or+you+go+peeking+through+the+%3Cb%3Ewall%3C%2Fb%3E&b=0&ni=21&no=4&ts=&tab=organic&sigr=13evdtqdq&sigb=19k7nsjvb&sigi=12o2la1db&sigt=12lia2m0j&sign=12lia2m0j&.crumb=.yUtKgFI6DE&hsimp=yhs-fullyhosted_003&hspart=ironsource" "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/36.0.1985.125 Safari/537.36"""

        pat = r"""([\d\.]+) ([\w.-]+) ([\w.-]+) (\[.+\]) "([^"\r\n]*|[^"\r\n\[]*\[.+\][^"]+|[^"\r\n]+.[^"]+)" (\d{3}) (\d+|-) ("(?:[^"]|\")+)"? ("(?:[^"]|\")+)"?"""

        match(pat, T(target))

        # Issue 9545 (32 bit)
        buf = PipeBuffer()
        show(buf, r"")
        @static if V6_COMPAT
            @test readstring(buf) == "r\"\""
        else
            @test read(buf, String) == "r\"\""
        end

        # see #10994, #11447: PCRE2 allows NUL chars in the pattern
        @test occurs_in(Regex(T("^a\0b\$")), T("a\0b"))

        # regex match / search string must be a String
        @test_throws ArgumentError match(r"test", GenericString("this is a test"))
        @test_throws ArgumentError fnd(First, r"test", GenericString("this is a test"))

        # Named subpatterns
        let m = match(r"(?<a>.)(.)(?<b>.)", T("xyz"))
            @test (m[:a], m[2], m["b"]) == ("x", "y", "z")
            typ = T === Strs._LatinStr ? ASCIIStr : T
            @test sprint(show, m) == "StrRegexMatch{$typ}(\"xyz\", a=\"x\", 2=\"y\", b=\"z\")"
        end
        # Backcapture reference in substitution string
        @static if V6_COMPAT
            @test replace(T("abcde"), r"(..)(?P<byname>d)", s"\g<byname>xy\\\1") == "adxy\\bce"
            @test_throws ErrorException replace("a", r"(?P<x>)", s"\g<y>")
        else
            @test replace(T("abcde"), r"(..)(?P<byname>d)" => s"\g<byname>xy\\\1") == "adxy\\bce"
            @test_throws ErrorException replace("a", r"(?P<x>)" => s"\g<y>")
        end
    end
end

@testset "Regex Utility functions" for ST in UnicodeStringTypes
    foobarbaz = ST("foo,bar,baz")
    foo = ST("foo")
    bar = ST("bar")
    baz = ST("baz")
    abc = ST("abc")
    abcd = ST("abcd")
    @testset "rsplit/split" begin
        @test split(foobarbaz, r",") == [foo,bar,baz]

        let str = ST("a.:.ba..:..cba.:.:.dcba.:.")
            @test split(str, r"\.(:\.)+") == ["a","ba.",".cba","dcba",""]
            @test split(str, r"\.(:\.)+"; keepempty=false) == ["a","ba.",".cba","dcba"]
            @test split(str, r"\.+:\.+") == ["a","ba","cba",":.dcba",""]
            @test split(str, r"\.+:\.+"; keepempty=false) == ["a","ba","cba",":.dcba"]
        end

        # zero-width splits

        @test split(ST(""), r"") == [""]
        @test split(abc,  r"") == ["a","b","c"]
        @test split(abcd, r"b?") == ["a","c","d"]
        @test split(abcd, r"b*") == ["a","c","d"]
        @test split(abcd, r"b+") == ["a","cd"]
        @test split(abcd, r"b?c?") == ["a","d"]
        @test split(abcd, r"[bc]?") == ["a","","d"]
        @test split(abcd, r"a*") == ["","b","c","d"]
        @test split(abcd, r"a+") == ["","bcd"]
        @test split(abcd, r"d*") == ["a","b","c",""]
        @test split(abcd, r"d+") == [abc,""]
        @test split(abcd, r"[ad]?") == ["","b","c",""]
    end

    @testset "replace" begin
        @test replace(abcd, r"b?" => "^") == "^a^c^d^"
        @test replace(abcd, r"b+" => "^") == "a^cd"
        @test replace(abcd, r"b?c?" => "^") == "^a^d^"
        @test replace(abcd, r"[bc]?" => "^") == "^a^^d^"

        @test replace("foobarfoo", r"(fo|ba)" => "xx") == "xxoxxrxxo"
        @test replace("foobarfoo", r"(foo|ba)" => bar) == "barbarrbar"

        @test replace(ST("äƀçđ"), r"ƀ?" => "π") == "πäπçπđπ"
        @test replace(ST("äƀçđ"), r"ƀ+" => "π") == "äπçđ"
        @test replace(ST("äƀçđ"), r"ƀ?ç?" => "π") == "πäπđπ"
        @test replace(ST("äƀçđ"), r"[ƀç]?" => "π") == "πäππđπ"

        @test replace(ST("foobarfoo"), r"(fo|ba)" => "ẍẍ") == "ẍẍoẍẍrẍẍo"

        @test replace(ST("ḟøøbarḟøø"), r"(ḟø|ba)" => "xx") == "xxøxxrxxø"
        @test replace(ST("ḟøøbarḟøø"), r"(ḟøø|ba)" => bar) == "barbarrbar"

        @test replace(ST("fooƀäṙfoo"), r"(fo|ƀä)" => "xx") == "xxoxxṙxxo"
        @test replace(ST("fooƀäṙfoo"), r"(foo|ƀä)" => "ƀäṙ") == "ƀäṙƀäṙṙƀäṙ"

        @test replace(ST("ḟøøƀäṙḟøø"), r"(ḟø|ƀä)" => "xx") == "xxøxxṙxxø"
        @test replace(ST("ḟøøƀäṙḟøø"), r"(ḟøø|ƀä)" => "ƀäṙ") == "ƀäṙƀäṙṙƀäṙ"

        # for Char pattern call Char replacement function
        @test replace(ST("a"), r"a" => typeof) == "SubString{$ST}"
    end
end
