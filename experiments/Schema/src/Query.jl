module Query

  export Types, Query, compose

  using Catlab, Catlab.Doctrines, Catlab.Present

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

  @instance BicategoryRelations(Types, Query) begin

    dom(f::Query)   = f.dom
    codom(f::Query) = f.codom

    compose(f::Query, g::Query) = begin
      new_query = "SELECT $(join(prepend("A.").(f.dom.names),",")),
                  $(join(prepend("B.").(g.codom.names),","))\n
                  FROM $(f.query) AS A INNER JOIN $(g.query) AS B ON\n"

      conditions =  join(map(1:length(f.codom)) do i
                      "WHERE A.$(f.codom_names[i])=B.$(f.codom_names[i])"
                    end
                  "\n")
    end

    Query(f.dom, g.codom, f.dom_names, g.codom_names, new_query*conditions, "DEFINED")
    end

    otimes(A::Types, B::Types) = vcat(A,B)
    otimes(f::Query, g::Query) = begin
      prepend(x) = (w) -> x*w
      alias(e) = (w) -> w*" AS "*w*e
      Query(otimes(f.dom, g.dom), otimes(f.codom,g.codom),
            vcat(alias("_top").(f.dom_names), alias("_bot").(g.dom_names)), 
            vcat(alias("_top").(f.codom_names), alias("_bot").(g.codom_names)),
            "SELECT $(join(alias("_top").(prepend("A.").(vcat(f.dom.names,f.codom.names))),",")),
                    $(join(alias("_bot").(prepend("B.").(vcat(g.dom.names,g.codom.names))),","))\n
                    FROM $(f.query) AS A,$(g.query) AS B;")
    end

    mcopy(A::Types) = begin
      Query(otimes(Types, Types), Types, undef, undef, "JOIN", "MCOPY")
    end
    
    mmerge(A::Types) = begin
      Query(otimes(Types, Types), Types, undef, undef, "INTERSECTION", "MCOPY")
    end

    delete(A::Types) = begin
      
    end

    create(A::Types) = begin

    end

  end

  compose_queries(f::Query, g::Query)::Query begin
    prepend(x) = (w) -> x*w
    alias(e) = (w) -> w*" AS "*w*e

    if( f.type == "DEFINED" || g.type == "DEFINED" )
      if( f.type == "MCOPY" )
        
      elseif( f.type == "MUNIT" )

      end
    else
      throw(DomainError(x, "two undefined tables cannot be directly operated on together"))
    end
    

    new_query = "SELECT $(join(prepend("A.").(f.dom.names),",")),
                  $(join(prepend("B.").(g.codom.names),","))\n
                  FROM $(f.query) AS A INNER JOIN $(g.query) AS B ON\n"

    conditions =  join(map(1:length(f.codom)) do i
                      "WHERE A.$(f.codom_names[i])=B.$(f.codom_names[i])"
                    end
                  "\n")

    Query(f.dom, g.codom, f.dom_names, g.codom_names, new_query*conditions, "DEFINED")
  end
end
