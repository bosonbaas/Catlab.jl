module QueryLib

  export Types, Query, make_query, FreeAbBiRelMeetJoin,
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
    sub_fields::Array{Array{String,1},1}
  end

  struct Query
    dom::Types
    codom::Types
    dom_names::Array{String,1}
    codom_names::Array{String,1}
    query::String
    type::String
  end

  # Make every element of the array unique by appending ":X" with X as some digit
  # This is necessary to prevent namespace collisions between columns
  uniquify(a::Array{String,1}) = begin

    a_n = Array{String,1}()
    # Fill a_n with unique values
    for i in 1:length(a)
      cur_val = a[i]

      # Iterate the digit until the value is unique
      while cur_val in a_n
        if occursin("_", cur_val)
          cur_val = replace(cur_val, r"_\d*$" => (c) -> "_"*string(parse(Int, c[2:end])+1))
        else
          cur_val = cur_val*"_0"
        end
      end
      append!(a_n, [cur_val])
    end
    return a_n
  end

  @syntax FreeAbBiRelMeetJoin(ObExpr,HomExpr) AbelianBicategoryRelations begin
    otimes(A::Ob, B::Ob) = associate_unit(new(A,B), munit)
    otimes(f::Hom, g::Hom) = associate(new(f,g))
    compose(f::Hom, g::Hom) = associate(new(f,g; strict=true))
    dagger(f::Hom) = distribute_unary(distribute_dagger(involute(new(f))),
                                      dagger, otimes)
  end

  @instance AbelianBicategoryRelations(Types, Query) begin

    dom(f::Query)   = f.dom
    codom(f::Query) = f.codom
    munit(::Type{Types}) = Types(Array{String, 1}(), Array{Array{String, 1},1}())

    compose(f::Query, g::Query) = begin

			prepend(x) = (w) -> x*w # This is necessary for adding query aliasses to
                              # column names

      # Generate unique domain and codomain names
      unique_names = uniquify(vcat(f.dom_names, g.codom_names))
      dom_names = unique_names[1:length(f.dom_names)]
      codom_names = unique_names[(length(f.dom_names)+1):end]

      # Generate select statements, aliasing to the new unique names
      select_dom = prepend("A.").(f.dom_names.*" AS ".*dom_names)
      select_codom = prepend("B.").(g.codom_names.*" AS ".*codom_names)

      # Combine the query
      new_query = "SELECT DISTINCT $(join(vcat(select_dom, select_codom),",\n"))\n"*
                  "FROM ($(f.query)) AS A\n INNER JOIN ($(g.query)) AS B ON\n"

      # Define the joining conditions
      conditions =  join(map(1:length(f.codom.types)) do i
                      "A.$(f.codom_names[i])=B.$(g.dom_names[i])"
                    end,
                    " AND\n")

      Query(f.dom, g.codom, dom_names, codom_names, new_query*conditions, "DEFINED")
    end

    otimes(A::Types, B::Types) = Types(vcat(A.types,B.types), vcat(A.sub_fields,B.sub_fields))
    otimes(f::Query, g::Query) = begin
      prepend(x)  = (w) -> x*w
      append(x)   = (w) -> w*x
      alias(e)    = (w) -> w*" AS "*w*e
      Query(otimes(f.dom, g.dom), otimes(f.codom,g.codom),
            vcat(append("_top").(f.dom_names), append("_bot").(g.dom_names)),
            vcat(append("_top").(f.codom_names), append("_bot").(g.codom_names)),
            "SELECT DISTINCT $(join(prepend("A.").(alias("_top").(vcat(f.dom_names,f.codom_names))),",")),
                    $(join(prepend("B.").(alias("_bot").(vcat(g.dom_names,g.codom_names))),","))\n
                    FROM ($(f.query)) AS A,($(g.query)) AS B", "DEFINED")
    end

    meet(f::Query, g::Query) = begin
      Query(f.dom, f.codom, f.dom_names, f.codom_names, f.query*"\nINTERSECT\n"*g.query, "DEFINED")
    end

    join(f::Query, g::Query) = begin
      Query(f.dom, f.codom, f.dom_names, f.codom_names, f.query*"\nUNION\n"*g.query, "DEFINED")
    end

    dagger(f::Query) = Query(f.codom, f.dom, f.codom_names, f.dom_names, f.query, f.type)

    dunit(A::Types) = begin
      throw(MethodError(dunit, A))
    end

    top(A::Types, B::Types) = begin
      throw(MethodError(top, [A,B]))
    end

    dcounit(A::Types) = begin
      throw(MethodError(dcounit, A))
    end

    id(A::Types) = begin
      throw(MethodError(id, A))
    end

    braid(A::Types, B::Types) = begin
      throw(MethodError(braid, [A,B]))
    end

    mcopy(A::Types) = begin
      throw(MethodError(mcopy),A)
    end

    mmerge(A::Types) = begin
      throw(MethodError(mmerge,A))
    end

    delete(A::Types) = begin
      throw(MethodError(delete,A))
    end

    create(A::Types) = begin
      throw(MethodError(create,A))
    end

    plus(A::Types) = begin
      throw(MethodError(plus,A))
    end

    zero(A::Types) = begin
      throw(MethodError(zero,A))
    end

    coplus(A::Types) = begin
      throw(MethodError(coplus,A))
    end

    cozero(A::Types) = begin
      throw(MethodError(cozero,A))
    end

    bottom(A::Types,B::Types) = begin
      throw(MethodError(bottom,[A,B]))
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
        dom_type = string(dom_type[1])
      end
      if isa(codom_type,DataType)
        codom_type = TypeToSql[codom_type]
      else
        codom_type = string(codom_type[1])
      end
      Query(type_to_field[dom_type], type_to_field[codom_type],
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
    functor((Types, Query), q, generators=d)
  end


end
