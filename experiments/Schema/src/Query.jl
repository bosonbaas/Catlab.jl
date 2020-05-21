module QueryLib

export Ports, Query, make_query,
  Ob, Hom, dom, codom, compose, ⋅, ∘, id, otimes, ⊗, munit, braid, σ,
  dagger, dunit, dcounit, mcopy, Δ, delete, ◊, mmerge, ∇, create, □,
  meet, top, to_sql#, plus, zero, coplus, cozero,  join, bottom

using Catlab, Catlab.Doctrines, Catlab.Present, Catlab.WiringDiagrams
import Catlab.Doctrines:
  Ob, Hom, dom, codom, compose, ⋅, ∘, id, otimes, ⊗, munit, braid, σ,
  dagger, dunit, dcounit, mcopy, Δ, delete, ◊, mmerge, ∇, create, □,
  plus, zero, coplus, cozero, meet, top, join, bottom, distribute_dagger

using AutoHashEquals
using LightGraphs, MetaGraphs
import Schema.Presentation: Schema, TypeToSql

@auto_hash_equals struct Types
  types::Array{Symbol,1}
end

""" Query

This structure holds the relationship graph between fields in a query

# Fields
- `dom::Types`: the types of the columns in the domain
- `codom::Types`: the types of the columns in the codomain
- `dom_names::Array{Int,1}`: index of the domain fields
- `codom_names::Array{Int,1}`: index of the codomain fields
- `tables::Array{String,1}`: Names of the tables included in the relationship
                             graph
- `fields::Array{Tuple{Int,String},1}`: Connection between a table and its
                                        fields
- `edges::Array{Tuple{Int,Int},1}`: Equality relationship between fields of
                                    tables
"""
struct Query
  tables::Dict{Symbol, Tuple(Array{String,1}, Array{String,1})}
  wd::WiringDiagram
end

Query(wd::WiringDiagram)::Query = begin
  n_table = Dict{Symbol, Tuple(Array{String,1}, Array{String,1})}()
  Query(n_table, wd)
end

function to_sql(q::Query)::String

  ind_to_alias(i) = "t$i"
  wd = q.wd

  # Get the vertices as (name, is_dagger, is_junc)
  struct Vertex
    name::Symbol
    is_dagger::Bool
    is_junc::Bool
  end

  verts = fill(Vertex(:*,false,false),nv(wd.graph))
  verts[1] = Vertex(:input,false,false)
  verts[2] = Vertex(:output,false,false)

  for (k,v) in boxes(wd)
    is_junc   = false
    is_dagger = false
    name      = :*
    if typeof(v) <: Junction
      vert.is_junc = true
    elseif typeof(v) <: BoxOp{:dagger}
      vert.is_dagger = true
      vert.name = v.box.value
    else
      vert.name = v.value
    end
    verts[k] = Vertex(name, is_dagger, is_junc)
  end

  # Make the join statement
  alias_array = Array{String,1}
  for v in box_ids(wd)
    cur_b = vert[v]
    name = string(cur_b.name)
    if !cur_b.is_junc
      append!(alias_array, ["$name AS $(ind_to_alias(v))"])
  end

  # Make the relation statement
  relation_array = Array{String,1}
  dom_array   = fill("", length(wd.input_ports))
  codom_array = fill("", length(wd.output_ports))
  for e in wires(wd)
    if e.source.box == 1
      db = e.target.box
      dp = e.target.port
      df = tables[verts[db].name][dp]
      dom_array[e.source.port] = "$(ind_to_alias(db)).$df"
    end
    if e.target.box == 2
      sb = e.source.box
      sp = e.source.port
      sf = tables[verts[sb].name][sp]
      codom_array[e.target.port] = "$(ind_to_alias(sb)).$sf"
    end

    sb = e.source.box
    sp = e.source.port
    sf = tables[verts[sb].name][1][sp]
    if verts[sb].is_dagger
      sf = tables[verts[sb].name][2][sp]
    end
    src = "$(ind_to_alias(sb)).$sf"

    db = e.target.box
    dp = e.target.port
    df = tables[verts[db].name][2][dp]
    if verts[db].is_dagger
      df = tables[verts[db].name][1][dp]
    end
    dst = "$(ind_to_alias(db)).$df"

    if dst*"="*src in relation_array or src*"="*dst in relation_array
      continue
    append!(relation_array, [src*"="*dst])
  end

  select = "SELECT "*join(vcat(dom_array, codom_array), ", ")*"\n"
  from = "FROM "*join(alias_array, ", ")*"\n"
  condition = "WHERE "*join(relation_array, " AND ")*";"

  return select*from*condition
end

@instance BicategoryRelations(Ports{BicategoryRelations.Hom, Symbol}, Query) begin

  dom(f::Query)   = input_ports(Ports, f.wd)
  codom(f::Query) = output_ports(Ports, f.wd)
  munit(::Type{Ports}) = Ports{BicategoryRelations.Hom, Symbol}([])

  compose(f::Query, g::Query) = begin
    n_tables = merge(f.tables, g.tables)
    n_wd = compose(f.wd, g.wd)
    return Query(n_tables, n_wd)
  end

  otimes(A::Ports, B::Ports) = otimes(A,B)
  otimes(f::Query, g::Query) = begin
    n_tables = merge(f.tables, g.tables)
    n_wd = otimes(f.wd, g.wd)
    return Query(n_tables, n_wd)
  end

  meet(f::Query, g::Query) = begin
    n_tables = merge(f.tables, g.tables)
    n_wd = meet(f.wd, g.wd)
    return Query(n_tables, n_wd)
  end

  dagger(f::Query) = Query(f.tables, dagger(f.wd))

  dunit(A::Ports) = begin
    Query(dunit(A))
  end

  top(A::Ports, B::Ports) = begin
    Query(top(A,B))
  end

  dcounit(A::Ports) = begin
    Query(dcounit(A))
  end

  id(A::Ports) = begin
    Query(id(A))
  end

  braid(A::Ports, B::Ports) = begin
    Query(braid(A,B))
  end

  mcopy(A::Ports) = begin
    Query(implicit_mcopy(A,2))
  end

  mmerge(A::Ports) = begin
    Query(implicit_mmerge(A,2))
  end

  delete(A::Ports) = begin
    Query(delete(A))
  end

  create(A::Ports) = begin
    Query(create(A))
  end
end

make_query(s::Schema, q::GATExpr)::Query = begin

  # Keep the type names associated to consistent Types objects
  type_to_field = Dict{String,Types}()

  # Generate Types objects from object description
  q_types = map(s.types) do t

    # If our type is primitive, just use the SQL type
    if typeof(t.args[1]) <: DataType
      type_name    = TypeToSql[t.args[1]]
      fields  = [Array{String,1}()]
      type_to_field[type_name] = Types([type_name], fields)
      return type_to_field[type_name]
    end

    # Otherwise, we use the symbol name and keep track of fields
    components = t.args[1][2]
    type_name = string(t.args[1][1])
    fields = [TypeToSql[components[k]] for k in keys(components)]
    type_to_field[type_name] = Types([type_name], [fields])
    return type_to_field[type_name]
  end

  # Generate the Query objects from hom descriptions
  q_homs = map(s.relations) do t

    dom_type    = Array{Types, 1}()
    codom_type  = Array{Types, 1}()
    dom_name    = Array{String,1}()
    codom_name  = Array{String,1}()

    # First do domain, then codomain
    names = t.args[1].fields[1]
    types = t.args[2]

    # Check if domain is composition
    if typeof(types) <: Catlab.Doctrines.FreeBicategoryRelations.Ob{:otimes}
      dom_name = names
      dom_type = map(types.args) do cur_t
        if isa(cur_t.args[1], DataType)
          return type_to_field[TypeToSql[cur_t.args[1]]]
        else
          return type_to_field[string(cur_t.args[1][1])]
        end
      end
    else
      dom_name = [names]
      if typeof(types.args[1]) <: DataType
        dom_type = [type_to_field[TypeToSql[types.args[1]]]]
      else
        dom_type = [type_to_field[string(types.args[1][1])]]
      end
    end

    names = t.args[1].fields[2]
    types = t.args[3]
    # Check if codomain is composition
    if typeof(types) <: Catlab.Doctrines.FreeBicategoryRelations.Ob{:otimes}
      codom_name = names
      codom_type = map(types.args) do cur_t
        if isa(cur_t.args[1], DataType)
          return type_to_field[TypeToSql[cur_t.args[1]]]
        else
          return type_to_field[string(cur_t.args[1][1])]
        end
      end
    else
      codom_name = [names]
      if typeof(types.args[1]) <: DataType
        codom_type = [type_to_field[TypeToSql[types.args[1]]]]
      else
        codom_type = [type_to_field[string(types.args[1][1])]]
      end
    end

    tables = [String(t.args[1].name)]
    join_names = vcat(dom_name, codom_name)

    # Generate the fields array
    fields = Array{Tuple{Int,String},1}()
    for i in 1:length(join_names)
      append!(fields,[(1,join_names[i])])
    end

    dom_names   = collect(1:length(dom_name))
    codom_names = collect((1+length(dom_name)):length(join_names))

    Query(otimes(dom_type), otimes(codom_type),
          dom_names, codom_names, tables, fields,[])
  end

  d = Dict()
  for i in 1:length(q_types)
    d[s.types[i]] = q_types[i]
  end
  for i in 1:length(q_homs)
    d[s.relations[i]] = q_homs[i]
  end
  functor((Types, Query), q, generators=d)
end
end
