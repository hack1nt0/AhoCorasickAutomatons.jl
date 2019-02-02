using Test, AhoCorasickAutomatons
import Random

# @testset "Brute test." begin
#     keys = Vector{String}()
#     open("../data/dict2") do io
#         for w in eachline(io)
#             push!(keys, w)
#         end
#     end
#     Ti = UInt32
#     obj = AhoCorasickAutomaton{Ti}(keys; sort = true)
# end

@testset "Inserted in, not inserted not in." begin
    keys1 = Vector{String}()
    for i = 1:10
        key = Random.randstring(rand(0:10))
        push!(keys1, key)
    end
    keys2 = Vector{String}()
    for i = :10
        key = Random.randstring(rand(0:10))
        push!(keys2, key)
    end
    obj = AhoCorasickAutomaton(keys1)
    for key in keys1
        @test key in obj
    end
    for key in keys2
        @test key ∉ keys1 && key ∉ obj || key ∈ keys1 && key ∈ obj
    end
end

@testset "Result of eachmatch is correct." begin
    s = Random.randstring("AB", 10)
    keys = Vector{String}()
    for i = 1:10
        key = Random.randstring("AB", rand(1:10))
        push!(keys, key)
    end
    unique!(keys)
    obj = AhoCorasickAutomaton(keys)
    res1 = sort!(collect(eachmatch(obj, s)))
    res2 = Vector{ACPosition}()
    for i = 1:length(keys)
        key = keys[i]
        for j = 1:length(s) - length(key) + 1
            if s[j:j + length(key) - 1] == key
                push!(res2, ACPosition(j, j + length(key) - 1, i))
            end
        end
    end
    sort!(res2)
    @test begin
        res = res1 == res2
        if !res
            @show s keys
            flush(stdout)
        end
        res
    end
end

@testset "Read and write is correct." begin
    keys = Vector{String}()
    for i = 1:10
        key = Random.randstring("AB", rand(0:10))
        push!(keys, key)
    end
    obj = AhoCorasickAutomaton(keys)
    io = IOBuffer()
    nbytes = write(io, obj)
    @test nbytes == sizeof(io.data)
    seek(io, 0)
    obj2 = read(io, AhoCorasickAutomaton)
    @test obj == obj2
end
