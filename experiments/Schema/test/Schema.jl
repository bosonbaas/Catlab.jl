using Schema
using Catlab, Catlab.Doctrines
using Test

records, vec, mat, model, prob = Ob(FreeMonoidalCategoryWithDiagonals, :records, :vec, :mat, :model, :prob)
extract, fit, evaluate = Hom(:extract, records, otimes(mat, vec)), Hom(:fit, otimes(mat, vec), otimes(vec, model)), Hom(:evaluate, otimes(vec, vec), prob)

rec_t = SchemaDom(["Record"])
vec_t = SchemaDom(["Vector"])
mat_t = SchemaDom(["Matrix"])
model_t = SchemaDom(["Model"])
prob_t = SchemaDom(["Probability"])

extr_t = FKTable(rec_t, otimes(mat_t, vec_t), "Extract")
fit_t = FKTable(otimes(mat_t, vec_t), otimes(vec_t, model_t), "Fit")
eval_t = FKTable(otimes(vec_t, vec_t), prob_t, "Evaluate")

d = Dict(records => rec_t,
         vec => vec_t,
         mat => mat_t,
         model => model_t,
         prob => prob_t,
         extract => extr_t,
         fit => fit_t,
         evaluate => eval_t)

F(ex) = functor((SchemaDom, FKTable), ex, generators=d)

M = extract⋅(id(mat)⊗Δ(vec))⋅(fit⊗id(vec))⋅(σ(vec,model)⊗id(vec))⋅(◊(model)⊗evaluate)

display_schema(F(M))
