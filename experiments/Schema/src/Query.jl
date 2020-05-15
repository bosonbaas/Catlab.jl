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

  struct Query
    dom::Types
    codom::Types
    dom_names::Array{Int,1}
    codom_names::Array{Int,1}
    tables::Array{String,1}
    fields::Array{Tuple{Int,String},1}
    connections::Array{Tuple{Int,Int},1}
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
    s_conn = Array{String,1}()

    for conn in q.connections
      # Don't include a value if it's just a variable (shows a variable loop)
      if q.fields[conn[1]][2] == "*"
        continue
      end

      append!(s_conn,[field_to_s(conn[1])*"="*field_to_s(conn[2])])
    end
    condition = "WHERE "*join(s_conn, " AND ")*";"
    return select*from*condition
  end

  function eval_variables!(q::Query)
    changes = 1
    while changes > 0
      changes = 0
      for conn in q.connections
        src = conn[1]
        dst = conn[2]

        # Set variables to fields they are equal to
        if q.fields[src][2] == "*"
          if q.fields[dst][2] != "*"
            q.fields[src] = q.fields[dst]
            changes += 1
          end
        elseif q.fields[dst][2] == "*"
          if q.fields[src][2] != "*"
            q.fields[dst] = q.fields[src]
            changes += 1
          end
        end
      end
    end

    # Remove self-connections
    is_self(x) = !(q.fields[x[1]] == q.fields[x[2]]) || q.fields[x[1]][2] == "*"
    filter!(is_self, q.connections)
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
      n_conn = vcat(f.connections, g.connections)

      # Increment all table references in g's fields
      inc = length(f.tables)
      for i in (1+length(f.fields)):length(n_fields)
        n_fields[i] = (n_fields[i][1]+inc, n_fields[i][2])
      end

      # Increment all table references in g's fields
      inc = length(f.fields)
      for i in (1+length(f.connections)):length(n_conn)
        n_conn[i] = (n_conn[i][1]+inc, n_conn[i][2]+inc)
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
        append!(n_conn,[(src, dst)])
      end

      # Make new query
      n_query = Query(n_dom, n_cod, n_dom_names, n_cod_names, n_tables, n_fields, n_conn)
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
      n_conn = vcat(f.connections, g.connections)

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
      for i in (1+length(f.connections)):length(n_conn)
        n_conn[i] = (n_conn[i][1]+inc, n_conn[i][2]+inc)
      end

      # Make new query
      n_query = Query(n_dom, n_cod, n_dom_names, n_cod_names, n_tables, n_fields, n_conn)
      eval_variables!(n_query)
      return n_query

    end

    meet(f::Query, g::Query) = begin
      mcopy(dom(A))⋅(f⊗g)⋅mmerge(codom(f))
    end

    dagger(f::Query) = Query(f.codom, f.dom, f.codom_names, f.dom_names,
                             f.tables, f.fields, f.connections)

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
      Query(A, A, [1], [2], [],
            [(0,"*"),(0,"*")],
            [(1,2)])
    end

    braid(A::Types, B::Types) = begin
      A_len = length(A.types)
      B_len = length(B.types)
      tot_len = A_len + B_len

      domain   = collect(1:(tot_len))
      codomain = collect((tot_len+1):((tot_len)*2))

      fields = fill((0,"*"),(tot_len)*2)

      connections = Array{Tuple{Int,Int},1}()
      for i in 1:length(domain)
        if i > A_len
          append!(connections, [(i,i-A_len + tot_len)])
        else
          append!(connections, [(i,i+B_len + tot_len)])
        end
      end

      Query(otimes(A,B), otimes(B,A), domain, codomain, [],
            fields, connections)
    end

    mcopy(A::Types) = begin
      A_len = length(A.types)
      domain   = collect(1:A_len)
      codomain = collect((A_len+1):(A_len*3))

      connections = Array{Tuple{Int,Int},1}()
      for i in 1:length(domain)
        append!(connections, [(i,i+A_len),(i,i+A_len*2)])
      end

      fields = fill((0,"*"),A_len*3)

      Query(A, otimes(A,A), domain, codomain, [],
            fields, connections)
    end

    mmerge(A::Types) = begin
      dagger(mcopy(A))
    end

    delete(A::Types) = begin
      A_len = length(A.types)
      domain   = collect(1:A_len)
      codomain = Array{Int,1}()

      fields = fill((0,"*"),A_len)
      connections = Array{Tuple{Int,Int},1}()

      Query(A, munit(Types), domain, codomain, [],
            fields, connections)
    end

    create(A::Types) = begin
      dagger(delete(A))
    end
  end

  make_query(s::Schema, q::GATExpr)::Query = begin
    type_to_field = Dict{String,Types}()
    q_types = map(s.types) do t
      if typeof(t.args[1]) <: DataType
        type = TypeToSql[t.args[1]]
        type_to_field[type] = Types([type], [Array{String,1}()])
        return type_to_field[type]
      end
      components = t.args[1][2]
      type = string(t.args[1][1])
      fields = [TypeToSql[components[k]] for k in keys(components)]
      type_to_field[type] = Types([type], [fields])
      return type_to_field[type]
    end
    q_homs = map(s.relations) do t
      # Get the dom type and name
      dom_name    = [t.args[1].fields[1]]
      codom_name  = [t.args[1].fields[2]]
      dom_type    = t.args[2].args[1]
      codom_type  = t.args[3].args[1]
      if isa(dom_type,DataType)
        dom_type = TypeToSql[dom_type]
      else
        dom_type = String(dom_type[1])
      end
      if isa(codom_type,DataType)
        codom_type = TypeToSql[codom_type]
      else
        codom_type = String(codom_type[1])
      end
      tables = [String(t.args[1].name)]
      fields = [(1,dom_name[1]),(1,codom_name[1])]
      Query(type_to_field[dom_type], type_to_field[codom_type],
            [1], [2], tables, fields,[])
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
