module AhoCorasickAutomatons

import Base: in, eachmatch, read, write, display,
    summarysize, format_bytes, resize!, isless, ==, print, get, length, collect, keys, values,
    getindex, setindex!
using Markdown, ProgressMeter

export AhoCorasickAutomaton, ACMatch

include("AhoCorasickAutomaton.jl")

end # module
