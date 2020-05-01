module QueryLib

  export Types, Query, make_query,
    Ob, Hom, dom, codom, compose, ⋅, ∘, id, otimes, ⊗, munit, braid, σ,
    dagger, dunit, dcounit, mcopy, Δ, delete, ◊, mmerge, ∇, create, □,
    meet, top, encode, decode#, plus, zero, coplus, cozero,  join, bottom

  using Catlab, Catlab.Doctrines, Catlab.Present
  import Catlab.Doctrines:
    Ob, Hom, dom, codom, compose, ⋅, ∘, id, otimes, ⊗, munit, braid, σ,
    dagger, dunit, dcounit, mcopy, Δ, delete, ◊, mmerge, ∇, create, □,
    plus, zero, coplus, cozero, meet, top, join, bottom, distribute_dagger

  using AutoHashEquals
  import Schema.Presentation: Schema, TypeToSql

  struct Types
    type_query::String
    uid_names::Array{String,1}
    val_names::Array{String,1}
    types::Array{String,1}
  end
  Base.hash(a::Types, h::UInt) = hash(a.b, hash(a.a, hash(:Types, h)))
  Base.:(==)(a::Types, b::Types) = a.types == b.types

  struct Query
    dom::Types
    codom::Types
    dom_names::Array{String,1}
    codom_names::Array{String,1}
    query::String
  end

  # Auto-Generate type_query
  Types(types::Array{String,1}) = begin
    type_fields = join(map(enumerate(types)) do (i,a)
                         "$(a)_t AS $(Char(64 + i))"
                       end,',')
    type_cols = join(map(enumerate(types)) do (i,a)
                       field = Char(64 + i)
                       "$field.uid AS uid_$(a)_$i, $field.value as value_$(a)_$i"
                     end, ",")

    uid_names = map(enumerate(types)) do (i,a)
      field = Char(64 + i)
      "uid_$(a)_$i"
    end

    val_names = map(enumerate(types)) do (i,a)
      field = Char(64 + i)
      "value_$(a)_$i"
    end

    Types("SELECT $type_cols FROM $type_fields", uid_names, val_names, types)
  end

  # Generate from single type
  Types(type::String) = Types([type])

  uniquify(a::Array{String,1}) = begin
    a_n = Array{String,1}()
    for i in 1:length(a)
      cur_val = a[i]
      while cur_val in a_n
        if occursin(":", cur_val)
          cur_val = replace(cur_val, r":\d*$" => (c) -> ":"*string(parse(Int, c[2:end])+1))
        else
          cur_val = cur_val*":0"
        end
      end
      append!(a_n, [cur_val])
    end
    return a_n
  end

  @instance BicategoryRelations(Types, Query) begin

    dom(f::Query)   = f.dom
    codom(f::Query) = f.codom
    munit(::Type{Types}) = Types(Array{String, 1}())

    compose(f::Query, g::Query) = begin
      prepend(x) = (w) -> x*w

      unique_names = uniquify(vcat(f.dom_names, g.codom_names))
      dom_names = unique_names[1:length(f.dom_names)]
      codom_names = unique_names[(length(f.dom_names)+1):end]


      select_dom = prepend("A.").(f.dom_names.*" AS ".*dom_names)
      select_codom = prepend("B.").(g.codom_names.*" AS ".*codom_names)

      @show select_dom
      @show select_codom
      new_query = "SELECT $(join(vcat(select_dom, select_codom),",\n"))\n"*
                  "FROM ($(f.query)) AS A\n INNER JOIN ($(g.query)) AS B ON\n"

      conditions =  join(map(1:length(f.codom.types)) do i
                      "A.$(f.codom_names[i])=B.$(g.dom_names[i])"
                    end,
                    " AND\n")

      Query(f.dom, g.codom, dom_names, codom_names, new_query*conditions)
    end

    otimes(A::Types, B::Types) = Types(vcat(A.types,B.types))
    otimes(f::Query, g::Query) = begin
      prepend(x)  = (w) -> x*w
      append(x)   = (w) -> w*x
      alias(e)    = (w) -> w*" AS "*w*e

      select_dom    = prepend("A.").(alias("_top").(vcat(f.dom_names,f.codom_names)))
      select_codom  = prepend("B.").(alias("_bot").(vcat(g.dom_names,g.codom_names)))

      Query(otimes(f.dom, g.dom), otimes(f.codom,g.codom),
            vcat(append("_top").(f.dom_names), append("_bot").(g.dom_names)),
            vcat(append("_top").(f.codom_names), append("_bot").(g.codom_names)),
            "SELECT $(join(vcat(select_dom, select_codom),",\n"))\n"*
            "       FROM ($(f.query)) AS A,($(g.query)) AS B")
    end

    meet(f::Query, g::Query) = begin
      Query(f.dom, f.codom, f.dom_names, f.codom_names, f.query*"\nINTERSECT\n"*g.query)
    end

    dagger(f::Query) = Query(f.codom, f.dom, f.codom_names, f.dom_names, f.query)

    id(A::Types) = begin

      unique_names = uniquify(vcat(A.uid_names, A.uid_names))
      dom_names = unique_names[1:length(A.uid_names)]
      codom_names = unique_names[(length(A.uid_names)+1):end]

      new_query = "SELECT $(join(prepend("A.").(A.uid_names.*" AS ".*dom_names,","))),\n"*
                  "       $(join(prepend("A.").(A.uid_names.*" AS ".*codom_names,",")))\n"*
                  "FROM ($(A.type_query)) AS A"

      Query(A, A, dom_names, codom_names, new_query)
    end

    braid(A::Types, B::Types) = begin
      id_ab = id(otimes(A,B))
      Query(otimes(A,B),otimes(B,A),
            id_ab.dom_names,
            vcat(id_ab.codom_names[(length(A.types)+1):end],
                 id_ab.codom_names[1:length(A.types)]),
            id_ab.type_query)
    end

    mcopy(A::Types) = begin
      dom = A
      codom = otimes(A,A)

      table = Types(vcat(A.types, A.types, A.types))
      dom_names = table.uid_names[1:length(A.types)]
      codom_names = table.uid_names[(length(A.types)+1):(length(A.types)*3)]

      Query(dom, codom, dom_names, codom_names, table.type_query)
    end

    mmerge(A::Types) = begin
      dagger(mcopy(A))
    end

    create(A::Types) = begin
      Query(munit(Types), A, [], A.uid_names, A.type_query)
    end

    delete(A::Types) = begin
      dagger(create(A))
    end

    encode(A::Types) = begin
      Query(Types(fill("text",length(A.types))), A, A.val_names, A.uid_names, A.type_query)
    end

    decode(A::Types) = begin
      dagger(encode(A))
    end

    dunit(A::Types) = begin
      compose(create(A), mcopy(A))
    end

    dcounit(A::Types) = begin
      dagger(dunit(A))
    end

    top(A::Types, B::Types) = begin
      compose(delete(A),create(B))
    end
  end

  make_query(s::Schema, q) = begin
    q_types = map(s.types) do t
      if typeof(t.args[1]) <: DataType
        return Types([TypeToSql[t.args[1]]])
      end
      return Types([string(t.args[1][1])])
    end
    q_homs = map(s.relations) do t
      # Get the dom type and name
      dom_name    = [t.args[1].fields[1]]
      codom_name  = [t.args[1].fields[2]]
      dom_type    = [string(t.args[2].args[1])]
      codom_type  = [string(t.args[3].args[1])]
      if isa(dom_type[1],DataType)
        dom_type[1] = TypeToSql[dom_type[1]]
      end
      if isa(codom_type[1],DataType)
        codom_type[1] = TypeToSql[codom_type[1]]
      end
      Query(Types(dom_type), Types(codom_type),
            dom_name, codom_name,
            "Select * from $(t.args[1].name)")
    end
    d = Dict()
    for i in 1:length(q_types)
      d[s.types[i]] = q_types[i]
    end
    for i in 1:length(q_homs)
      d[s.relations[i]] = q_homs[i]
    end
    functor((Types, Query), q, generators=d).query
  end


end
