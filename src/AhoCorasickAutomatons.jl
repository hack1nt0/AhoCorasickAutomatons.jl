module AhoCorasickAutomatons

import Base: in, eachmatch, read, write, display,
    summarysize, format_bytes, resize!, isless, ==, print, get
using Markdown, ProgressMeter

export AhoCorasickAutomaton, ACPosition, addkey!, validate

include("AhoCorasickAutomaton.jl")

end # module
