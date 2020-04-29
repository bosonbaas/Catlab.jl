# here is an example
using Catlab, Catlab.Doctrines, Catlab.Present 
using Schema.QueryLib, Schema.Presentation
import Schema.Presentation: Schema, sql

# Define the Types
Name = Ob(FreeBicategoryRelationsMeet, (:full_name, (first=String, last=String)))
Person = Ob(FreeBicategoryRelationsMeet, (:person, (id=Int,)))
X = Ob(FreeBicategoryRelationsMeet, Int)
F = Ob(FreeBicategoryRelationsMeet, Float64)
ID = Ob(FreeBicategoryRelationsMeet, (:ID, (id=Int,)))

# Define the relationships
name = Hom((name=:names, fields=("person", "full_name")), Person, Name)
emply = Hom((name=:employees, fields=("person", "ID")), Person, ID)
manag = Hom((name=:manager, fields=("person", "manager")), Person, Person)
salry = Hom((name=:salary, fields=("person", "salary")), Person, F)

# Set up arrays of types and relationships for Schema
types = [Name, Person, X,F,ID]
rels = [name, emply, manag, salry]

# Generate the Schema
prim, tab = sql(Schema(types, rels))
@show prim
@show tab

# Generate and display a query to get (names, salaries)
println(make_query(Schema(types, rels), meet(dagger(emply)⋅name, dagger(emply)⋅dagger(manag)⋅name)))

# get the salary of a person's manager
# query(manag⋅salry) == "select (manager.id, salary.salary) from manager join salary on manager.manager == salary.id"
