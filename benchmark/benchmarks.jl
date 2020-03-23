export benchmark
using BenchmarkTools
using Catlab.LinearAlgebra.GraphicalLinearAlgebra
using Catlab


function deep_composition(f, g, n::Int64)
  if n == 1
    return f⋅g
  end
  next = deep_composition(f,g,n-1)
  return next⋅next
end

SUITE = BenchmarkGroup()
SUITE["gla"] = BenchmarkGroup()

gla = SUITE["gla"]

gla["composition"] = BenchmarkGroup()
gla["composition"]["func"] = BenchmarkGroup()
gla["composition"]["LinOps"] = BenchmarkGroup()
gla["compmap"] = BenchmarkGroup()
gla["compmap"]["func"] = BenchmarkGroup()
gla["compmap"]["LinMaps"] = BenchmarkGroup()
gla["compdepth"] = BenchmarkGroup()
gla["compdepth"]["func"] = BenchmarkGroup()
gla["compdepth"]["LinOps"] = BenchmarkGroup()

for m_exp in 1:10
 
  m_size = 2^m_exp
  A = Ob(FreeLinearFunctions, :A)
  f, g = Hom(:f, A, A), Hom(:g, A, A)
  fop = LinearOperator(rand(m_size, m_size))
  gop = LinearOperator(rand(m_size, m_size))

  d = Dict(f=>fop,
           g=>gop)

  F(ex) = functor((LinearOpDom, LinearOperator), ex, generators=d)

  gla["composition"]["func"][m_size] = @benchmarkable Matrix($(F)($g⋅$f))
  gla["composition"]["LinOps"][m_size] = @benchmarkable Matrix($fop*$gop)
end

for m_exp in 1:10
 
  m_size = 2^m_exp
  A = Ob(FreeLinearFunctions, :A)
  f, g = Hom(:f, A, A), Hom(:g, A, A)
  fop = LinearMap(rand(m_size, m_size))
  gop = LinearMap(rand(m_size, m_size))

  d = Dict(f=>fop,
           g=>gop)

  F(ex) = functor((LinearMapDom, LinearMap), ex, generators=d)

  gla["compmap"]["func"][m_size] = @benchmarkable Matrix($(F)($g⋅$f))
  gla["compmap"]["LinMaps"][m_size] = @benchmarkable Matrix($fop*$gop)
end


for n in 1:6
 
  A = Ob(FreeLinearFunctions, :A)
  f, g = Hom(:f, A, A), Hom(:g, A, A)
  fop = LinearOperator(rand(128, 128))
  gop = LinearOperator(rand(128, 128))

  d = Dict(f=>fop,
           g=>gop)

  F(ex) = functor((LinearOpDom, LinearOperator), ex, generators=d)

  gla["compdepth"]["func"][n] = @benchmarkable Matrix($(F)($(deep_composition(f,g,n))))
  gla["compdepth"]["LinOps"][n] = @benchmarkable Matrix($(deep_composition(fop,gop,n)))
end

