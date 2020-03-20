export benchmark
using BenchmarkTools
using Catlab.LinearAlgebra.GraphicalLinearAlgebra
using Catlab


function benchmark()
  suite = BenchmarkGroup()
  suite["gla"] = BenchmarkGroup()
  
  gla = suite["gla"]
  
  gla["composition"] = BenchmarkGroup()
  gla["composition"]["func"] = BenchmarkGroup()
  gla["composition"]["LinOps"] = BenchmarkGroup()
  gla["compmap"] = BenchmarkGroup()
  gla["compmap"]["func"] = BenchmarkGroup()
  gla["compmap"]["LinMaps"] = BenchmarkGroup()
  
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
    print("Got another benchmark set up")
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
    print("Got another benchmark set up")
  end
  
  tune!(suite)
  run(suite)
end
