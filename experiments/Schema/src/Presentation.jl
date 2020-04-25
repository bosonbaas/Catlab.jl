module Presentation
using Catlab
using Catlab.Doctrines
using Catlab.Present

TypeToSql = Dict(String => "text",
                 Int64 => "int",
                 Float64 => "float4")
             

struct Schema{T, U}
    types::Vector{T}
    relations::Vector{U}
end

sql(s::Schema) = begin
    primitives = map(s.types) do t
        @show t.args[1]
        # if our type is primitive, just use the SQL type
        if typeof(t.args[1]) <: DataType
          return "-- primitive type $(TypeToSql[t.args[1]])"
        end
        # construct a composite type
        f = (i,x) -> "$i $(TypeToSql[x])"
        @show components = t.args[1][2]
        fields = (f(k,components[k]) for k in keys(components)) |> x->join(x, ", ")
        "CREATE TYPE $(t.args[1][1]) as ($(fields));"
    end
    # for the relations in your presentation, you want to create tables with foreign keys
    tables = map(s.relations) do t
        fields = map(enumerate(t.args[2:end])) do (i, a)
            name = t.args[1].fields[i]
            @show a
            @show typeof(a.args[1])
            # for primitive types, we can just include them in the table directly
            if isa(a.args[1], DataType)
              return "$name $(TypeToSql[a.args[1]])"
            end
            @show col = a.args[1][1]
            # this creates the foreign key
            " $name $col"
        end |> xs-> join(xs, ",")
        "create table $(t.args[1].name) ($(fields));"
    end
    return primitives, tables
end

# here is an example
Name = Ob(FreeCartesianCategory, (:full_name, (first=String, last=String)))
Person = Ob(FreeCartesianCategory, (:person, (id=Int,)))
X = Ob(FreeCartesianCategory, Int)
F = Ob(FreeCartesianCategory, Float64)
ID = Ob(FreeCartesianCategory, (:ID, (id=Int,)))

name = Hom((name=:names, fields=(:person, :full_name)), Person, Name)
emply = Hom((name=:eployees, fields=(:person, :ID)), Person, ID)
manag = Hom((name=:manager, fields=(:person, :manager)), Person, Person)
salry = Hom((name=:salary, fields=(:person, :salary)), Person, F)

types = [Name, Person, X,F,ID]
rels = [name, emply, manag, salry]

prim, tab = sql(Schema(types, rels))
@show prim
@show tab

# get the salary of a person's manager
# query(manag⋅salry) == "select (manager.id, salary.salary) from manager join salary on manager.manager == salary.id"

end
