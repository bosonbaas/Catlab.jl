module QueryLib

export Types, Query, make_query,
  Ob, Hom, dom, codom, compose, ⋅, ∘, id, otimes, ⊗, munit, braid, σ,
  dagger, dunit, dcounit, mcopy, Δ, delete, ◊, mmerge, ∇, create, □,
  meet, top, to_sql#, plus, zero, coplus, cozero,  join, bottom

using Catlab, Catlab.Doctrines, Catlab.Present
import Catlab.Doctrines:
  Ob, Hom, dom, codom, compose, ⋅, ∘, id, otimes, ⊗, munit, braid, σ,
  dagger, dunit, dcounit, mcopy, Δ, delete, ◊, mmerge, ∇, create, □,
  plus, zero, coplus, cozero, meet, top, join, bottom, distribute_dagger

using AutoHashEquals
import Schema.Presentation: Schema, TypeToSql

@auto_hash_equals struct Types
  types::Array{String,1}
  sub_fields::Array{Array{String,1},1}
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
  dom::Types
  codom::Types
  dom_names::Array{Int,1}
  codom_names::Array{Int,1}
  tables::Array{String,1}
  fields::Array{Tuple{Int,String},1}
  edges::Array{Tuple{Int,Int},1}
end

function to_sql(q::Query)::String
  # Convert index to table alias
  ind_to_tab(i) = "t$i"

  # Index to table name + alias
  ind_to_alias(i) = "$(q.tables[i]) AS $(ind_to_tab(i))"

  # Get field name from field index
  field_to_s(i) = ind_to_tab(q.fields[i][1])*".$(q.fields[i][2])"

  # Select statement
  select = "SELECT "*join(field_to_s.(vcat(q.dom_names, q.codom_names)),", ")*"\n"

  # From statement
  from = "FROM "*join(ind_to_alias.(collect(1:length(q.tables))), ", ")*"\n"

  # Conditions
  s_edges = Array{String,1}()

  for e in q.edges
    # Don't include a value if it's just a variable (shows a variable loop)
    if q.fields[e[1]][2] == "*"
      continue
    end

    append!(s_edges,[field_to_s(e[1])*"="*field_to_s(e[2])])
  end
  condition = "WHERE "*join(s_edges, " AND ")*";"
  return select*from*condition
end


"""
    eval_variables!(q::Query)

Evaluate any "*" nodes in the relational graph.

This algorithm steps through every edge and acts on an edge if it is between a
"*" node and an actual node. This exits early if there are no more
modifications to make, but otherwise will execute no more times than there are
nodes in the network.
"""
function eval_variables!(q::Query)

  # There will be at least one update per iteration, so it won't go past the
  # number of fields
  for ind in 1:length(q.fields)

    # Keep track of the number of changes made (to see if we're done)
    changes = 0
    for e in q.edges
      src = e[1]
      dst = e[2]

      # Remove any "*" nodes using equality relationships they have to non-"*"
      # nodes
      if q.fields[src][2] == "*" && q.fields[dst][2] != "*"
        q.fields[src] = q.fields[dst]
        changes += 1
      elseif q.fields[src][2] != "*" && q.fields[dst][2] == "*"
        q.fields[dst] = q.fields[src]
        changes += 1
      end
    end

    # We're done if there are no more changes to make
    if changes == 0
      break
    end
  end

  # Remove self-connections
  is_self(x) = !(q.fields[x[1]] == q.fields[x[2]]) || q.fields[x[1]][2] == "*"
  filter!(is_self, q.edges)
  return q
end

@instance BicategoryRelations(Types, Query) begin

  dom(f::Query)   = f.dom
  codom(f::Query) = f.codom
  munit(::Type{Types}) = Types(Array{String, 1}(), Array{Array{String, 1},1}())

  compose(f::Query, g::Query) = begin
    # Set domain and codomain types and names
    n_dom = dom(f)
    n_cod = codom(g)
    n_dom_names = copy(f.dom_names)
    n_cod_names = copy(g.codom_names)
    n_tables = vcat(f.tables, g.tables)
    n_fields = vcat(f.fields, g.fields)
    n_edges = vcat(f.edges, g.edges)

    # Increment all table references in g's fields
    inc = length(f.tables)
    for i in (1+length(f.fields)):length(n_fields)
      n_fields[i] = (n_fields[i][1]+inc, n_fields[i][2])
    end

    # Increment all table references in g's fields
    inc = length(f.fields)
    for i in (1+length(f.edges)):length(n_edges)
      n_edges[i] = (n_edges[i][1]+inc, n_edges[i][2]+inc)
    end

    # Increment all table references in g's dom
    inc = length(f.fields)
    for i in 1:length(n_cod_names)
      n_cod_names[i] += inc
    end

    # Add connections between f.cdom and g.dom
    inc = length(f.fields)
    for i in 1:length(f.codom_names)
      src = f.codom_names[i]
      dst = g.dom_names[i] + inc
      append!(n_edges,[(src, dst)])
    end

    # Make new query
    n_query = Query(n_dom, n_cod, n_dom_names, n_cod_names, n_tables, n_fields, n_edges)
    eval_variables!(n_query)
    return n_query
  end

  otimes(A::Types, B::Types) = Types(vcat(A.types,B.types), vcat(A.sub_fields,B.sub_fields))
  otimes(f::Query, g::Query) = begin
    # Set domain and codomain types and names
    n_dom = otimes(dom(f), dom(g))
    n_cod = otimes(codom(f),codom(g))
    n_dom_names = vcat(f.dom_names,g.dom_names)
    n_cod_names = vcat(f.codom_names,g.codom_names)
    n_tables = vcat(f.tables, g.tables)
    n_fields = vcat(f.fields, g.fields)
    n_edges = vcat(f.edges, g.edges)

    # Increment g's table references in dom
    inc = length(f.fields)
    for i in (length(f.dom_names)+1):length(n_dom_names)
      n_dom_names[i] += inc
    end

    # Increment g's table references in codom
    inc = length(f.fields)
    for i in (length(f.codom_names)+1):length(n_cod_names)
      n_cod_names[i] += inc
    end

    # Increment all table references in g's fields
    inc = length(f.tables)
    for i in (1+length(f.fields)):length(n_fields)
      n_fields[i] = (n_fields[i][1]+inc, n_fields[i][2])
    end

    # Increment all table references in g's connections
    inc = length(f.fields)
    for i in (1+length(f.edges)):length(n_edges)
      n_edges[i] = (n_edges[i][1]+inc, n_edges[i][2]+inc)
    end

    # Make new query
    n_query = Query(n_dom, n_cod, n_dom_names, n_cod_names, n_tables, n_fields, n_edges)
    eval_variables!(n_query)
    return n_query

  end

  meet(f::Query, g::Query) = begin
    mcopy(dom(A))⋅(f⊗g)⋅mmerge(codom(f))
  end

  dagger(f::Query) = Query(f.codom, f.dom, f.codom_names, f.dom_names,
                           f.tables, f.fields, f.edges)

  dunit(A::Types) = begin
    create(A)⋅mcopy(A)
  end

  top(A::Types, B::Types) = begin
    delete(A)⋅create(B)
  end

  dcounit(A::Types) = begin
    dagger(dunit(A))
  end

  id(A::Types) = begin
    A_len = length(A.types)

    domain   = collect(1:A_len)
    codomain = collect((A_len+1):(2*A_len))

    fields = fill((0,"*"),2*A_len)

    edges = Array{Tuple{Int,Int},1}()
    for i in 1:A_len
      append!(edges, [(i,i+A_len)])
    end

    Query(A, A, domain, codomain, [],
          fields,
          edges)
  end

  braid(A::Types, B::Types) = begin
    A_len = length(A.types)
    B_len = length(B.types)
    tot_len = A_len + B_len

    domain   = collect(1:(tot_len))
    codomain = collect((tot_len+1):((tot_len)*2))

    fields = fill((0,"*"),(tot_len)*2)

    edges = Array{Tuple{Int,Int},1}()
    for i in 1:length(domain)
      if i > A_len
        append!(edges, [(i,i-A_len + tot_len)])
      else
        append!(edges, [(i,i+B_len + tot_len)])
      end
    end

    Query(otimes(A,B), otimes(B,A), domain, codomain, [],
          fields, edges)
  end

  mcopy(A::Types) = begin
    A_len = length(A.types)
    domain   = collect(1:A_len)
    codomain = collect((A_len+1):(A_len*3))

    edges = Array{Tuple{Int,Int},1}()
    for i in 1:length(domain)
      append!(edges, [(i,i+A_len),(i,i+A_len*2)])
    end

    fields = fill((0,"*"),A_len*3)

    Query(A, otimes(A,A), domain, codomain, [],
          fields, edges)
  end

  mmerge(A::Types) = begin
    dagger(mcopy(A))
  end

  delete(A::Types) = begin
    A_len = length(A.types)
    domain   = collect(1:A_len)
    codomain = Array{Int,1}()

    fields = fill((0,"*"),A_len)
    edges = Array{Tuple{Int,Int},1}()

    Query(A, munit(Types), domain, codomain, [],
          fields, edges)
  end

  create(A::Types) = begin
    dagger(delete(A))
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
