module Interface
  export init_tables, prepare, execute, Connection  

  using Catlab
  using Schema.Presentation, Schema.QueryLib
  using LibPQ, Tables
  import LibPQ:
    Connection, Result, Statement
  import Schema.Presentation:
    Schema, sql


  function init_tables(conn::Connection, schema::Schema)
    prim, tab = sql(schema)
  end

  # prepare:
  # This function creates a prepared statement which can be executed later
  # with variable inserted into the query

  function prepare(conn::Connection, schema::Schema, expr::GATExpr)::Statement
    query = make_query(schema, expr)
    
    # Need to generate a wrapper call around this to insert parameters
    
    pre   = "SELECT * FROM\n("
    post  = ")\n AS A WHERE " * join(map(enumerate(query.dom_names)) do (i,a)
                                       "$(a)=\$$i"
                                     end, " AND ")
    LibPQ.prepare(conn, pre * query.query * post)
  end

  function execute(conn::Connection, schema::Schema, expr::GATExpr)::NamedTuple
    query = make_query(schema, expr)

    columntable(LibPQ.execute(conn, query.query))
  end

  function execute(st::Statement, input::AbstractArray)::NamedTuple
    columntable(LibPQ.execute(st, input))
  end

end
