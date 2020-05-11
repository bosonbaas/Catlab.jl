module Presentation
export Schema, TypeToSql, sql
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

        # if our type is primitive, just use the SQL type
        if typeof(t.args[1]) <: DataType
          return "-- primitive type $(TypeToSql[t.args[1]]);"
        end
        # construct a composite type
        f = (i,x) -> "$i $(TypeToSql[x])"
         components = t.args[1][2]
        fields = (f(k,components[k]) for k in keys(components)) |> x->join(x, ", ")
        "CREATE TYPE $(t.args[1][1]) as ($(fields));"
    end
    # for the relations in your presentation, you want to create tables with foreign keys
    tables = map(s.relations) do t
        fields = map(enumerate(t.args[2:end])) do (i, a)
            name = t.args[1].fields[i]

            # for primitive types, we can just include them in the table directly
            if isa(a.args[1], DataType)
              return "$name $(TypeToSql[a.args[1]])"
            end
            col = a.args[1][1]
            # this creates the foreign key
            " $name $col"
        end |> xs-> join(xs, ",")
        "CREATE TABLE $(t.args[1].name) ($(fields));"
    end
    return primitives, tables
end
end
