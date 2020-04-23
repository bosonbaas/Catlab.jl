module Schema

  using AutoHashEquals
  
  export SchemaDom, FKTable, copy,
         dom, codom, compose, id, otimes, munit,
         ⊗, braid, σ, mcopy, Δ, delete, ◊,
         FreeMonoidalCategoryWithDiagonals, display_schema
         # To implement
         # mcopy, delete, plus, zero,
         
  #using Catlab.Doctrines.AdditiveMonoidal
  using Catlab, Catlab.Doctrines, Catlab.Programs
  import Catlab.Doctrines:
    Ob, Hom, dom, codom, compose, ⋅, ∘, id, otimes, ⊗, munit, braid, σ,
    dagger, dunit, dcounit, mcopy, Δ, delete, ◊, mmerge, ∇, create, □,
    plus, zero, coplus, cozero, meet, top, join, bottom
  
  
  @auto_hash_equals struct SchemaDom
    types::Array{String,1}
  end
  
  struct FKTable
    dom::SchemaDom
    codom::SchemaDom
    types::Set{String}
    tables::Set{FKTable}
    name::String
  end
  
  FKTable(dom::Array{String,1},codom::Array{String,1}, name::String) = begin 
    FKTable(SchemaDom(dom), SchemaDom(codom), name)
  end

  FKTable(a::SchemaDom, b::SchemaDom, name::String) = begin
    FKTable(a, b, Set{String}(vcat(a.types, b.types)), Set{FKTable}(), name) 
  end
  
  @syntax FreeMonoidalCategoryWithDiagonals(ObExpr, HomExpr) MonoidalCategoryWithDiagonals begin
    otimes(A::Ob, B::Ob) = associate_unit(new(A,B), munit)
    otimes(f::Hom, g::Hom) = associate(new(f,g))
    compose(f::Hom, g::Hom) = associate(new(f,g; strict=true))
  end

  @instance MonoidalCategoryWithDiagonals(SchemaDom, FKTable) begin

    munit(::Type{SchemaDom}) = SchemaDom(Array{String,1}())
    dom(f::FKTable)    = f.dom
    codom(f::FKTable)  = f.codom

    id(A::SchemaDom)    = FKTable(A,A, "Identity")
    compose(f::FKTable,g::FKTable) = 
    FKTable(f.dom, g.codom, union(f.types, g.types), join_tables(f,g), "comp") 

    otimes(A::SchemaDom, B::SchemaDom) = SchemaDom(vcat(A.types, B.types))
    otimes(f::FKTable, g::FKTable)  = 
      FKTable(otimes(f.dom,g.dom), otimes(f.codom,g.codom), union(f.types, g.types), 
               join_tables(f,g), "otimes")

    braid(A::SchemaDom,B::SchemaDom) = FKTable(otimes(A,B), otimes(B,A), "Switch")
    mcopy(A::SchemaDom) = FKTable(A, otimes(A,A), "Copy")
    delete(A::SchemaDom) = FKTable(A, munit(SchemaDom), "Delete")
  end

  join_tables(f::FKTable, g::FKTable)::Set{FKTable} = begin
    new_table = union(f.tables, g.tables)

    if length(f.tables) == 0
      push!(new_table, f)
    end
    
    if length(g.tables) == 0
      push!(new_table, g)
    end

    return new_table  
  end

  display_schema(s::FKTable) = begin
    println("Type Tables")
    for type in s.types
      println(type)
    end

    println()
    println("Foreign Key Tables")
    for table in s.tables
      print(table.name*": ")
      for type in table.types
        print(type*", ")
      end
      println()
    end
  end
end
