"""
    AhoCorasickAutomaton{T}(keys::Vector{Pair{String, Ti}}; sort = false) where {T, Ti}
    AhoCorasickAutomaton{T}(keys::AbstractDict{String, Ti}; sort = false) where {T, Ti}
    AhoCorasickAutomaton{T}(keys::Vector{String}; sort = false) where T
    AhoCorasickAutomaton(keys::Vector{String}; sort = false) = AhoCorasickAutomaton{UInt32}(keys; sort = sort)
    AhoCorasickAutomaton(keys::Vector{Pair{String, Ti}}; sort = false) where Ti = AhoCorasickAutomaton{UInt32}(keys; sort = sort)

    T s.t. typemax(T) >= length(keys)

A 2-Array implementation of Aho–Corasick automaton(ACA)[^1], Most used as an engine to mine a long text string,
to get all occurences of substrings interested in the text. ACA is also adequate for unicode strings
as a `Set{String}` or `Dict{String, Unsigned}` (similar as Trie, but with a far more less space usage).

The ACA acts as a key data structrue in Aho–Corasick algorithm for multiple consecutive string
patterns finding[^2]. This particular implementation use no-decreasing `base`, i.e.,

    1. base[node] >= node
    2. children[node] > node

This choice make it suitable for large-size key set of not-very-long keys, with faster
insertion speed and moderate space usage.

# Notes
1. Maybe of slower speed than a oridinary tree-based ACA, specially for random generated keys.
2. When inserting duplicated keys, only the last one will make sense.
3. Inputing Sorted keys will be of (much) faster speed. Just turn the *sort* option on!

# Examples
```
julia> v = ["A", "B"]; obj1 = AhoCorasickAutomaton{UInt8}(v; sort = true) # constructor
type  AhoCorasickAutomaton{UInt8}
––––– –––––––––––––––––––––––––––
keys                            2
nodes                          67
nnz                             3
size                    623 bytes

julia> for key in v @assert key ∈ obj1 end

julia> @assert sort!(keys(obj1)) == v

julia> d = Dict("A" => 1, "B" => 2); obj2 = AhoCorasickAutomaton{UInt8}(d; sort = true); obj1 == obj2 # constructor
true

julia> obj3 = AhoCorasickAutomaton{UInt8}(collect(d); sort = true); obj2 == obj3 # constructor
true

julia> s = "AABDB"; matches = eachmatch(obj1, s) # eachmatch
4-element Array{ACMatch,1}:
 ACMatch(1, 1, 1)
 ACMatch(2, 2, 1)
 ACMatch(3, 3, 2)
 ACMatch(5, 5, 2)

julia> matches == eachmatch(obj1, codeunits(s)) # eachmatch
true

julia> map(x -> s[x], matches) # getindex
4-element Array{String,1}:
 "A"
 "A"
 "B"
 "B"

julia> ss = collect(codeunits(s)); foreach(x -> ss[x] = ss[x], matches); String(ss) == s # setindex!
true

julia> io = IOBuffer(); write(io, obj1); seek(io, 0); obj11 = read(io, AhoCorasickAutomaton); obj1 == obj11 # read & write
true
```
For more : `using Pkg; less(joinpath(Pkg.dir("AhoCorasickAutomatons"), "test", "runtests.jl"))`

# See also
*ACMatch*, *eachmatch*, *getindex*, *setindex!*,
*in*, *get*, *length*, *read*, *write*, *collect*, *keys*, *values*.

# References
[^1]: Jun‐Ichi Aoe, Katsushi Morimoto and Takashi Sato 'An Efficient Implementation of Trie Structures', September 1992.
[^2]:  Aho, Alfred V.; Corasick, Margaret J. (June 1975). "Efficient string matching: An aid to bibliographic search". Communications of the ACM. 18 (6): 333&ndash, 340. doi:10.1145/360825.360855. MR 0371172.

"""
struct AhoCorasickAutomaton{T <: Integer}
    base::Vector{T}
    from::Vector{T}
    ikey::Vector{T}
    deep::Vector{T}
    back::Vector{T}
    arcs::Vector{Vector{UInt8}}
end

function AhoCorasickAutomaton{T}() where T
    base = T[1]
    from = T[1]
    ikey = T[0]
    deep = T[0]
    back = T[0]
    arcs = [UInt8[]]
    return AhoCorasickAutomaton{T}(base, from, ikey, deep, back, arcs)
end

function AhoCorasickAutomaton{T}(keys::Vector{Pair{String, Ti}}; sort = false) where {T, Ti}
    nkeys = T(length(keys))
    obj = AhoCorasickAutomaton{T}()
    if !sort || issorted(keys)
        @showprogress 1 "Building ACA..." for i = T(1):nkeys
            addkey!(obj, codeunits(keys[i][1]), T(keys[i][2]))
        end
    else
        Base.sort!(keys)
        @showprogress 1 "Building ACA..." for i = T(1):nkeys
            addkey!(obj, codeunits(keys[i][1]), T(keys[i][2]))
        end
    end
    shrink!(obj)
    @inbounds fillback!(obj)
    validate(obj)
    resize!(obj.arcs, 0)
    obj
end

function AhoCorasickAutomaton{T}(keys::AbstractDict{String, Ti}; sort = false) where {T, Ti}
    nkeys = T(length(keys))
    if !sort || issorted(keys)
        obj = AhoCorasickAutomaton{T}()
        @showprogress 1 "Building ACA..." for (key, id) in keys
            addkey!(obj, codeunits(key), T(id))
        end
        return obj
    else
        return AhoCorasickAutomaton{T}(collect(keys); sort = sort)
    end
end

function AhoCorasickAutomaton{T}(keys::Vector{String}; sort = false) where T
    keys2 = Vector{Pair{String, T}}(undef, length(keys))
    for (i, key) in enumerate(keys) keys2[i] = key => T(i) end
    return AhoCorasickAutomaton{T}(keys2; sort = sort)
end

AhoCorasickAutomaton(keys::Vector{String}; sort = false) = AhoCorasickAutomaton{UInt32}(keys; sort = sort)
AhoCorasickAutomaton(keys::Vector{Pair{String, Ti}}; sort = false) where Ti = AhoCorasickAutomaton{UInt32}(keys; sort = sort)

function write(io::IO, obj::AhoCorasickAutomaton{T}) where T
    nbyte = 0
    if T isa Unsigned
        nbyte += write(io, +1)
    else
        nbyte += write(io, -1)
    end
    nbyte += write(io, sizeof(T))
    nbyte += write(io, length(obj.from))
    nbyte +=
    write(io, obj.base) +
    write(io, obj.from) +
    write(io, obj.ikey) +
    write(io, obj.deep) +
    write(io, obj.back)
    return nbyte;
end

function read(io::IO, ::Type{AhoCorasickAutomaton})
    signT = read(io, Int)
    byteT = read(io, Int)
    T = Int
    if signT > 0
        T = filter(x -> sizeof(x) == byteT, [UInt8, UInt16, UInt32, UInt64])[1]
    else
        T = filter(x -> sizeof(x) == byteT, [Int8, Int16, Int32, Int64])[1]
    end
    nnode = read(io, Int)
    base = reinterpret(T, read(io, nnode * sizeof(T)))
    from = reinterpret(T, read(io, nnode * sizeof(T)))
    ikey = reinterpret(T, read(io, nnode * sizeof(T)))
    deep = reinterpret(T, read(io, nnode * sizeof(T)))
    back = reinterpret(T, read(io, nnode * sizeof(T)))
    return AhoCorasickAutomaton{T}(base, from, ikey, deep, back, [UInt8[]])
end

function ==(x::AhoCorasickAutomaton, y::AhoCorasickAutomaton)
    return x.base == y.base && x.from == y.from && x.ikey == y.ikey && x.deep == y.deep && x.back == y.back
end

function in(key::AbstractString, obj::AhoCorasickAutomaton{T})::Bool where T
    return get(obj, key, T(0)) > 0
end

function in(key::DenseVector{UInt8}, obj::AhoCorasickAutomaton{T})::Bool where T
    return get(obj, key, T(0)) > 0
end

function get(obj::AhoCorasickAutomaton{T}, key::DenseVector{UInt8}, default::T)::T where T
    cur::T = 1
    n::T = length(obj.from)
    for c in key
        nxt = obj.base[cur] + c
        if (nxt <= n && obj.from[nxt] == cur)
            cur = nxt
        else
            return default
        end
    end
    return obj.ikey[cur]
end

function get(obj::AhoCorasickAutomaton{T}, key::AbstractString, default::T)::T where T
    return get(obj, codeunits(key), default)
end

function length(obj::AhoCorasickAutomaton{T}) where T
    return count(!iszero, obj.ikey)
end

function collect(obj::AhoCorasickAutomaton{T}) where T
    base = obj.base
    from = obj.from
    ikey = obj.ikey
    res = Pair{String, Int}[]
    for i = 1:length(ikey)
        if ikey[i] == 0 continue end
        codes = UInt8[]
        j = i
        while j > 1
            c = j - base[from[j]]
            push!(codes, c)
            j = from[j]
        end
        push!(res, String(reverse!(codes)) => ikey[i])
    end
    return res
end

function keys(obj::AhoCorasickAutomaton{T}) where T
    return map(first, collect(obj))
end

function values(obj::AhoCorasickAutomaton{T}) where T
    return filter(!iszero, obj.ikey)
end

function shrink!(obj::AhoCorasickAutomaton{T})::T where T
    actlen = findlast(!iszero, obj.from)
    if (actlen < length(obj.from))
        resize!(obj.base, actlen)
        resize!(obj.from, actlen)
        resize!(obj.ikey, actlen)
        resize!(obj.deep, actlen)
        resize!(obj.back, actlen)
        resize!(obj.arcs, actlen)
    end
    return actlen
end

function enlarge!(obj::AhoCorasickAutomaton{T}, newlen::T)::T where T
    base = obj.base; from = obj.from; deep = obj.deep; back = obj.back; ikey = obj.ikey; arcs = obj.arcs;
    oldlen::T = length(obj.base)
    newlen2 = oldlen
    while newlen2 < newlen newlen2 *= 2 end
    if (oldlen < newlen2)
        resize!(base, newlen2)
        resize!(from, newlen2)
        resize!(ikey, newlen2)
        resize!(deep, newlen2)
        resize!(back, newlen2)
        resize!(arcs, newlen2)
        # for i = oldlen + 1:newlen2 base[i] = i end
        base[oldlen + 1:newlen2] .= 0
        from[oldlen + 1:newlen2] .= 0
        ikey[oldlen + 1:newlen2] .= 0
        deep[oldlen + 1:newlen2] .= 0
        back[oldlen + 1:newlen2] .= 0
        for i in oldlen + 1:newlen2
            arcs[i] = UInt8[]
        end
    end
    return newlen2
end

function addkey!(obj::AhoCorasickAutomaton{T}, code::Base.CodeUnits{UInt8,String}, icode::T)::Nothing where T
    base = obj.base; from = obj.from; deep = obj.deep; back = obj.back; ikey = obj.ikey; arcs = obj.arcs;
    cur::T = 1
    nxt::T = 0
    for c in code
        nxt = base[cur] + c
        enlarge!(obj, nxt)
        if (from[nxt] == 0)
            from[nxt] = cur
            push!(arcs[cur], c)
            deep[nxt] = deep[cur] + 1
            cur = nxt
        elseif (from[nxt] == cur)
            cur = nxt
        else # from[nxt] != cur
            push!(arcs[cur], c)
            if length(arcs[cur]) <= length(arcs[from[nxt]]) || from[nxt] == from[cur]
                rebase!(obj, cur)
                nxt = base[cur] + c
            else
                rebase!(obj, from[nxt])
            end
            from[nxt] = cur
            deep[nxt] = deep[cur] + 1
            cur = nxt
        end
    end
    ikey[cur] = icode
    return nothing
end

function rebase!(obj::AhoCorasickAutomaton{T}, cur::T)::Nothing where T
    base = obj.base; from = obj.from; deep = obj.deep; back = obj.back; ikey = obj.ikey; arcs = obj.arcs;
    oldbase = base[cur]
    @assert length(arcs[cur]) > 0 string(cur)
    newbase = findbase(obj, cur)
    enlarge!(obj, newbase + maximum(arcs[cur]))
    for i = eachindex(arcs[cur])
        # arc = arcs[cur][i]
        newson = newbase + arcs[cur][i]
        from[newson] = cur
        oldson = oldbase + arcs[cur][i]
        if (from[oldson] != cur) continue end
        base[newson] = base[oldson]
        ikey[newson] = ikey[oldson]
        deep[newson] = deep[oldson]
        z = arcs[newson]; arcs[newson] = arcs[oldson]; arcs[oldson] = z;
        # grandsons
        for arc in arcs[newson]
            from[base[newson] + arc] = newson
        end
        # oldson
        base[oldson] = from[oldson] = ikey[oldson] = deep[oldson] = 0
    end
    base[cur] = newbase
    return nothing
end

function findbase(obj::AhoCorasickAutomaton{T}, cur::T)::T where T
    base = obj.base; from = obj.from; deep = obj.deep; back = obj.back; ikey = obj.ikey; arcs = obj.arcs;
    n::T = length(from)
    for b = max(cur + 1, base[cur]):n
        ok = true
        for arc in arcs[cur]
            @inbounds ok &= arc + b > n || from[arc + b] == 0
        end
        if (ok)
            return b
        end
    end
    return T(n + 1)
end

function fillback!(obj::AhoCorasickAutomaton{T})::Nothing where T
    base = obj.base; from = obj.from; deep = obj.deep; back = obj.back; ikey = obj.ikey; arcs = obj.arcs;
    #println(arcs)
    n::T = length(arcs)
    root::T = 1
    que = similar(base); head::T = 1; tail::T = 2;
    que[1] = root; back[root] = root;
    while head < tail
        cur = que[head]; head += 1;
        for arc in arcs[cur]
            chd = base[cur] + arc
            chdback = root
            if (cur != root)
                chdback = back[cur]
                while chdback != root && (base[chdback] + arc > n || from[base[chdback] + arc] != chdback)
                    chdback = back[chdback]
                end
                if base[chdback] + arc <= n && from[base[chdback] + arc] == chdback
                    chdback = base[chdback] + arc
                end
            end
            back[chd] = chdback
            que[tail] = chd; tail += 1;
        end
    end
    return nothing
end

function validate(obj::AhoCorasickAutomaton{T})::Nothing where T
    base = obj.base; from = obj.from; deep = obj.deep; back = obj.back; ikey = obj.ikey; arcs = obj.arcs;
    root = 1
    que = similar(base); head = 1; tail = 2;
    que[1] = root;
    while head < tail
        cur = que[head]; head += 1;
        for arc in arcs[cur]
            chd = base[cur] + arc
            # @assert from[chd] == cur && back[chd] != chd && back[chd] != 0 string(chd, " fa=", cur, " from=", from[chd], " back=", back[chd])
            @assert from[chd] == cur string("cur=", cur, " chd=", chd, " from=", from[chd])
            que[tail] = chd; tail += 1;
        end
    end
    return nothing
end

function display(obj::AhoCorasickAutomaton{T}) where T
    rows = Any[["type", typeof(obj)],
                ["keys", length(obj)],
                ["nodes", length(obj.from)],
                ["nnz", count(!iszero, obj.from)],
                ["size", format_bytes(summarysize(obj))]
    ];
    return display(Markdown.MD(Markdown.Table(rows, Symbol[:l, :r])))
end

function print(io::IO, obj::AhoCorasickAutomaton{T}) where T
    print(io, (nodes = length(obj.from), nnz = count(!iszero, obj.from), size = format_bytes(summarysize(obj))))
end

"""
ACMatch has 3 fields:

    1. s : start of match
    2. t : stop of match, using text[s, t] to get matched patterns
    3. i : index of the key in *obj*, which is the original insertion order of keys to *obj*

The field *i* may be use as index of external property arrays, i.e., the AhoCorasickAutomaton
can act as a `Map{String, Any}`.

# See also
*eachmatch*, *getindex*, *setindex!*
"""
struct ACMatch
    s::Int
    t::Int
    i::Int
end

length(pos::ACMatch) = pos.t - pos.s + 1

function isless(x::ACMatch, y::ACMatch)::Bool
    return x.s < y.s || x.s == y.s && x.t < y.t || x.s == y.s && x.t == y.t && x.i < y.i
end

"""
    eachmatch(obj::AhoCorasickAutomaton{T}, text::AbstractString)::Vector{ACMatch} where T
    eachmatch(obj::AhoCorasickAutomaton{T}, text::DenseVector{T2})::Vector{ACMatch} where T where T2

Search for all matches of a AhoCorasickAutomaton *obj* in *text* and return a
vector of the matches. Each match is represented as a `ACMatch` type.

# See also
*ACMatch*
"""
function eachmatch(obj::AhoCorasickAutomaton{T}, text::AbstractString)::Vector{ACMatch} where T
    return eachmatch(obj, codeunits(text))
end

function eachmatch(obj::AhoCorasickAutomaton{T}, codes::DenseVector{UInt8})::Vector{ACMatch} where T
    base = obj.base; from = obj.from; deep = obj.deep; back = obj.back; ikey = obj.ikey; arcs = obj.arcs;
    n = length(base)
    root = cur = T(1)
    res = Vector{ACMatch}(undef, 0)
    for i = 1:length(codes)
        c = codes[i]
        while cur != root && (base[cur] + c > n || from[base[cur] + c ] != cur)
            cur = back[cur]
        end
        if (base[cur] + c <= n && from[base[cur] + c] == cur)
            cur = base[cur] + c
        end
        # if (ikey[cur] > 0)
            node = cur
            while node != root
                if (ikey[node] > 0) push!(res, ACMatch(i - deep[node] + 1, i, ikey[node])) end
                node = back[node]
            end
        # end
    end
    res
end

"""
    getindex(xs::AbstractString, pos::ACMatch)

Retrieve a substring of *xs* specified by *pos*.
# See also
*ACMatch*
"""
function getindex(xs::AbstractString, pos::ACMatch)
    return xs[pos.s:pos.t]
end

"""
    getindex(xs::DenseVector{UInt8}, pos::ACMatch)

Retrieve a sub-codeunits of *xs* specified by *pos*.
# See also
*ACMatch*
"""
function getindex(xs::DenseVector{UInt8}, pos::ACMatch)
    return xs[pos.s:pos.t]
end

"""
    setindex!(xs::DenseVector{UInt8}, sub::DenseVector{UInt8}, pos::ACMatch)

Set values of region specified by *pos* of *xs* to *sub*.
# Note
 @assert length(sub) == length(pos)
# See also
*ACMatch*
"""
function setindex!(xs::DenseVector{UInt8}, sub::DenseVector{UInt8}, pos::ACMatch)
    return xs[pos.s:pos.t] = sub
end
