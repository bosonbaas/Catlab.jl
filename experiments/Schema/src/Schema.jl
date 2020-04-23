# Schema @instance, for converting process diagrams -> formula -> database schema

module Schema

  using AutoHashEquals 
  using Catlab, Catlab.Doctrines, Catlab.Programs
  import Catlab.Doctrines:
    Ob, Hom, dom, codom, compose, ⋅, ∘, id, otimes, ⊗, munit, braid, σ,
    dagger, dunit, dcounit, mcopy, Δ, delete, ◊, mmerge, ∇, create, □,
    plus, zero, coplus, cozero, meet, top, join, bottom
  
  export SchemaDom, Table, copy,
         dom, codom, compose, id, otimes, munit,
         ⊗, braid, σ, mcopy, Δ, delete, ◊,
         FreeMonoidalCategoryWithDiagonals, display_schema
  
  
  @auto_hash_equals struct SchemaDom
    types::Array{String,1}
  end
  
  struct Table
    dom::SchemaDom
    codom::SchemaDom
    types::Set{String}
    tables::Set{Table}
    name::String
  end
  
  Table(dom::Array{String,1},codom::Array{String,1}, name::String) = begin 
    Table(SchemaDom(dom), SchemaDom(codom), name)
  end

  Table(a::SchemaDom, b::SchemaDom, name::String) = begin
    Table(a, b, Set{String}(vcat(a.types, b.types)), Set{Table}(), name) 
  end
  
  @syntax FreeMonoidalCategoryWithDiagonals(ObExpr, HomExpr) MonoidalCategoryWithDiagonals begin
    otimes(A::Ob, B::Ob) = associate_unit(new(A,B), munit)
    otimes(f::Hom, g::Hom) = associate(new(f,g))
    compose(f::Hom, g::Hom) = associate(new(f,g; strict=true))
  end

  @instance MonoidalCategoryWithDiagonals(SchemaDom, Table) begin

    munit(::Type{SchemaDom}) = SchemaDom(Array{String,1}())
    dom(f::Table)    = f.dom
    codom(f::Table)  = f.codom

    id(A::SchemaDom)    = Table(A,A, "Identity")
    compose(f::Table,g::Table) = 
    Table(f.dom, g.codom, union(f.types, g.types), join_tables(f,g), "comp") 

    otimes(A::SchemaDom, B::SchemaDom) = SchemaDom(vcat(A.types, B.types))
    otimes(f::Table, g::Table)  = 
      Table(otimes(f.dom,g.dom), otimes(f.codom,g.codom), union(f.types, g.types), 
               join_tables(f,g), "otimes")

    braid(A::SchemaDom,B::SchemaDom) = Table(otimes(A,B), otimes(B,A), "Switch")
    mcopy(A::SchemaDom) = Table(A, otimes(A,A), "Copy")
    delete(A::SchemaDom) = Table(A, munit(SchemaDom), "Delete")
  end

  join_tables(f::Table, g::Table)::Set{Table} = begin
    new_table = union(f.tables, g.tables)

    if length(f.tables) == 0
      push!(new_table, f)
    end
    
    if length(g.tables) == 0
      push!(new_table, g)
    end

    return new_table  
  end

  display_schema(s::Table) = begin
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
