module AhoCorasickAutomatons

import Base: in, eachmatch, read, write, display,
    summarysize, format_bytes, resize!, isless, ==, print
using Markdown, ProgressMeter

export AhoCorasickAutomaton, ACPosition

include("AhoCorasickAutomaton.jl")

end # module
