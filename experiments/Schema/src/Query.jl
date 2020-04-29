module QueryLib

  export Types, Query, make_query, FreeBicategoryRelationsMeet,
    Ob, Hom, dom, codom, compose, ⋅, ∘, id, otimes, ⊗, munit, braid, σ,
    dagger, dunit, dcounit, mcopy, Δ, delete, ◊, mmerge, ∇, create, □,
    meet, top#, plus, zero, coplus, cozero,  join, bottom

  using Catlab, Catlab.Doctrines, Catlab.Present
  import Catlab.Doctrines:
    Ob, Hom, dom, codom, compose, ⋅, ∘, id, otimes, ⊗, munit, braid, σ,
    dagger, dunit, dcounit, mcopy, Δ, delete, ◊, mmerge, ∇, create, □,
    plus, zero, coplus, cozero, meet, top, join, bottom, distribute_dagger

  using AutoHashEquals
  import Schema.Presentation: Schema, TypeToSql

  @auto_hash_equals struct Types
    types::Array{String,1}
  end

  struct Query
    dom::Types
    codom::Types
    dom_names::Array{String,1}
    codom_names::Array{String,1}
    query::String
    type::String
  end

  @syntax FreeBicategoryRelationsMeet(ObExpr,HomExpr) BicategoryRelations begin
    otimes(A::Ob, B::Ob) = associate_unit(new(A,B), munit)
    otimes(f::Hom, g::Hom) = associate(new(f,g))
    compose(f::Hom, g::Hom) = associate(new(f,g; strict=true))
    dagger(f::Hom) = distribute_unary(distribute_dagger(involute(new(f))),
                                      dagger, otimes)
  end

  @instance BicategoryRelations(Types, Query) begin

    dom(f::Query)   = f.dom
    codom(f::Query) = f.codom
    munit(::Type{Types}) = Types(Array{String, 1}())

    compose(f::Query, g::Query) = begin
      prepend(x) = (w) -> x*w
      new_query = "SELECT $(join(prepend("A.").(f.dom_names),",")),"*
                  "$(join(prepend("B.").(g.codom_names),","))\n"*
                  "FROM ($(f.query))\n AS A INNER JOIN ($(g.query))\n AS B ON\n"

      conditions =  join(map(1:length(f.codom.types)) do i
                      "A.$(f.codom_names[i])=B.$(g.dom_names[i])"
                    end,
                    " AND\n")

      Query(f.dom, g.codom, f.dom_names, g.codom_names, new_query*conditions, "DEFINED")
    end

    otimes(A::Types, B::Types) = Types(vcat(A.types,B.types))
    otimes(f::Query, g::Query) = begin
      prepend(x)  = (w) -> x*w
      append(x)   = (w) -> w*x
      alias(e)    = (w) -> w*" AS "*w*e
      Query(otimes(f.dom, g.dom), otimes(f.codom,g.codom),
            vcat(append("_top").(f.dom_names), append("_bot").(g.dom_names)),
            vcat(append("_top").(f.codom_names), append("_bot").(g.codom_names)),
            "SELECT $(join(prepend("A.").(alias("_top").(vcat(f.dom_names,f.codom_names))),",")),
                    $(join(prepend("B.").(alias("_bot").(vcat(g.dom_names,g.codom_names))),","))\n
                    FROM ($(f.query)) AS A,($(g.query)) AS B", "DEFINED")
    end

    meet(f::Query, g::Query) = begin
      Query(f.dom, f.codom, f.dom_names, f.codom_names, f.query*"\nINTERSECTION\n"*g.query, "DEFINED")
    end

    dagger(f::Query) = Query(f.codom, f.dom, f.codom_names, f.dom_names, f.query, f.type)

    dunit(A::Types) = begin
      throw(MethodError("dunit is not implemented"))
    end

    top(A::Types, B::Types) = begin
      throw(MethodError("top is not implemented"))
    end

    dcounit(A::Types) = begin
      throw(MethodError("dunit is not implemented"))
    end

    id(A::Types) = begin
      throw(MethodError("id is not implemented"))
    end

    braid(A::Types, B::Types) = begin
      throw(MethodError("braid is not implemented"))
    end

    mcopy(A::Types) = begin
      throw(MethodError("mcopy is not implemented"))
    end
    
    mmerge(A::Types) = begin
      throw(MethodError("mmerge is not implemented"))
    end

    delete(A::Types) = begin
      throw(MethodError("delete is not implemented"))
    end

    create(A::Types) = begin
      throw(MethodError("create is not implemented"))
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
            "Select * from $(t.args[1].name)", "DEFINED")
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
