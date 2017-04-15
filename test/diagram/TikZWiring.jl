module TestTikZWiring

using Base.Test

using CompCat.Doctrine
using CompCat.Diagram.TikZWiring
using CompCat.Diagram.TikZWiring.Defaults
import CompCat.Diagram: TikZ

# We can't test that these pictures look right, but we can at least test that
# they exist!
ispic(obj) = isa(obj, TikZ.Picture)

A, B = [ ob(FreeCategory, sym) for sym in [:A,:B] ]
f, g = hom(:f, A, B), hom(:g, B, A)
@test ispic(wiring_diagram(f))
@test ispic(wiring_diagram(compose(f,g)))
@test ispic(wiring_diagram(compose(id(A), f, id(B))))

A, B = [ ob(FreeDaggerCategory, sym) for sym in [:A,:B] ]
f, g = hom(:f, A, B), hom(:g, B, A)
@test ispic(wiring_diagram(dagger(f)))
@test ispic(wiring_diagram(dagger(compose(f,g))))

A, B = [ ob(FreeSymmetricMonoidalCategory, sym) for sym in [:A,:B] ]
f, g = hom(:f, A, B), hom(:g, B, A)
@test ispic(wiring_diagram(otimes(f,g)))
@test ispic(wiring_diagram(compose(otimes(f,g),braid(B,A))))

A, B = [ ob(FreeBiproductCategory, sym) for sym in [:A,:B] ]
f, g = hom(:f, A, B), hom(:g, B, A)
@test ispic(wiring_diagram(compose(mcopy(A), otimes(f,f), mmerge(B))))
@test ispic(wiring_diagram(compose(create(A), f, g, delete(A))))

A, B = [ ob(FreeCompactClosedCategory, sym) for sym in [:A,:B] ]
f, g = hom(:f, A, B), hom(:g, B, A)
h = hom(:h, otimes(A,B), otimes(A,B))
@test ispic(wiring_diagram(ev(A)))
@test ispic(wiring_diagram(coev(A)))
trace = compose(
  f,
  otimes(coev(A), id(B)),
  otimes(id(dual(A)), h),
  otimes(braid(dual(A),A), id(B)),
  otimes(ev(A), id(B)),
  g,
)
@test ispic(wiring_diagram(trace))

end