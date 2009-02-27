%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%%
%% @copyright Bertram Harendt
%% @author Bertram Harendt <edbi@bertram-harendt.de>
%% @version {@version}
%% @end
%% =====================================================================


%% @doc EDBI - A database independent interface for Erlang
%%
%% This module provides the main user interface to EDBI.
%% @type pool() = atom(). The name of a connection pool. A connection pool is identified by this name and represents one or many connections.
%%       Any database operation that is done on that pool uses one (free) connection of the pool. See {@link connect/2} to see how to add 
%%       connections to an existing pool. The name must be unique for the whole application.
%% @type connection() = pid(). The process id of a connection in a pool. Any database operation can be done on that connection directly.
%% @type num_rows() = integer(). The number of affected rows from an INSERT or UPDATE statement. Is returned by the {@link execute/2}, 
%%       {@link execute/3} or {@link execute/4} function for sql or prepared statements that modify the data source.
%% @type col_names() = [col_name()]. List of the names of the selected columns in a result set. 
%% @type rows() = [row()]. A list of rows from the result set.
%% @type col_name() = string(). Name of a column in the result set.
%% @type row() = {value()}. One row of the result set.
%% @type query_result() = {updated, num_rows()} | {selected, col_names(), rows()} | {error, Reason}
%% @type tcp_port() = integer() | default. The port of the database to connect to or <code>default</code> to use the default port for connection.
%% @type connection_options() = [connection_option()]
%%  where connection_option() = {encoding, list()} | {reconnect, boolean()}. The optional options for a connection.
%%    <ul>
%%      <li><em>encoding</em>: The character encoding of the connection. Default is the default from the database.</li>
%%      <li><em>reconnect</em>: Enables or disables auto-reconnect for the connection. Default is <code>true</code>.<br/>
%%                          If <code>true</code>, the driver tries to reconnect automatically if connection was broken.</li>
%%    </ul>
%% @type connection_url() = string(). Url that is used to connect to a database. The format is:<br/><br/> 
%%       <code>"[Driver Name]://[Host]:[Port]/[Database]?[Option1]=[Value1]&amp;[Option1]=[Value1]"</code> or an odbc connection string.<br/>
%% <br/>
%% Example:
%% <pre>
%%  % connects to database on localhost using edbi mysql driver, port 3306 and database products, with options user and password
%%  "mysql://localhost:3306/products?user=user_name&amp;password=secret"
%%
%%  % connect to database on host 192.168.0.110 using edbi postgres driver, port 5432 and database users
%%  "postgres://192.168.0.110:5432/users?user=user_name&amp;password=secret"
%% </pre>



-module(edbi).

% connect to data source
-export([connect/1, connect/2, connect/4, connect/6, connect/7, connect/9, connect/10]).
-export([transaction/3]).
-export([prepare/3, prepare/5]).
% execute sql statement / prepared statements
-export([execute/2, execute/3, execute/4]).
% utilities
% -export([rows_to_records/2]).

-include("edbi.hrl").

-record(state, {
    pools = [],   %% [pool()]. The registered pools
    drivers = [] %% [driver()]. The registered drivers
  }).
  
-record(pool, {
    driver,       %% atom(). The name of the driver
    name,         %% atom(). The name of the pool
    connections = [], %% [connection()]. The connections of this pool.
    prepared_statements = [], %% [prepared_statement()]. The prepared statements 
    
    url,          %% string(). 
    host,         %% string(). The host name to connect to, if url is undefined
    port,         %% integer(). The port to connect to, if url is undefined
    user,         %% string(). The user name for the connection, if url is undefined
    password,     %% string(). The password for the connection, if url is undefined
    database,     %% string(). The database to connect to if url is undefined
    
    options = [], %% [option()]. The connection options
    encoding,     % deprecated, use options
    reconnect,    % deprecated, use options
    timeout       %% integer() | infinity. The connection timeout
  }).

-record(driver, {
    name,         %% atom(). The name of the driver
    module,       %% atom(). The name of the callback module with behaviour edbi_driver
    available_options = [] %% [option()]. The available options of this driver
  }).
  
-record(prepared_statement, {
    name,         %% atom(). The name of the prepared statement
    sql           %% string(). The sql statement to prepare
  }).
  
  
%%====================================================================
%% API
%%====================================================================


%% @spec connect(connection_url(), atom(), connection_options(), integer() | infinity)
%%   -> {ok, connection()} | {error, Reason} 
%% @doc Connects to a database <b>using a connection url</b> and register the connection <b>with the pool name</b>.
%% <ul>
%%  <li><em>ConnectionURL</em>: the connection url.</li>
%%  <li><em>PoolName</em>:  the name of the pool the new connection will belong to</li>
%%  <li><em>Options</em>:   the additional connection options.</li>
%%  <li><em>Timeout</em>:   the timeout until the connection should be established</li>
%% </ul>
%% Example:
%% <pre>
%%  % connect to mysql database on localhost, port 3306 and database products
%%  case edbi:connect("mysql://localhost:3306/products?user=user_name&amp;password=secret", products_pool, [{encoding, "utf8"}], infinity) of 
%%    {ok, ConnectionPid} -> ok; % connection to mysql database established and added to pool 'products_pool'
%%    {error, Reason} -> handle_connection_error(Reason)
%%  end,
%%
%%  % connect to postgres database on host 192.168.0.110, port 5432 and database users
%%  edbi:connect("postgres://192.168.0.110:5432/users?user=user_name&amp;password=secret", [{reconnect, false}], 5000). 
%% </pre>
connect(ConnectionURL, PoolName, Options, Timeout) ->
  ok.

%% @spec connect(atom(), list(), tcp_port(), list(), list(), list()) -> {ok, connection()} | {error, Reason} 
%% @doc Connects to a given data source <b>without connection pool support</b>.
%% like {@link connect/9} but connects with:
%% <ul>
%%  <li>default connection timeout = <code>infinity</code></li>
%%  <li>default {@link connection_options()}</li>
%%  <li>with a random pool id, so <b>with only one connection and no connection pool</b> support</li>
%% </ul>
%% @equiv connect/9
%% @see connect/9
connect(Driver, Host, Port, User, Password, Database) ->
  connect(Driver, _PoolName = undefined, Host, Port, User, Password, Database).

%% @spec connect(atom(), pool(), list(), tcp_port(), list(), list(), list()) -> {ok, connection()} | {error, Reason} 
%% @doc Connects to a given data source and creates a connection pool <b>with default connection options</b>.
%% like {@link connect/9} but connects with:
%% <ul>
%%  <li>default connection timeout = <code>infinity</code></li>
%%  <li>default {@link connection_options()}</li>
%% </ul>
%% @equiv connect/9
%% @see connect/9
connect(Driver, PoolName, Host, Port, User, Password, Database) ->
  connect(Driver, PoolName, Host, Port, User, Password, Database, _Options = [], _Timeout = infinity).

%% @spec connect(atom(), pool(), list(), tcp_port(), list(), list(), list(), connection_options(), integer() | infinity) 
%%    -> {ok, connection()} | {error, Reason} 
%% @doc Connects to a database using the given driver and register the connection <b>with the pool name</b>.
%% <ul>
%%  <li><em>Driver</em>:    the name of the edbi driver, e.g. <code>mysql</code> or <code>psql</code> 
%%                          or any name of a registered driver. See {@link registered_drivers/0}.</li>
%%  <li><em>PoolName</em>:  the name of the pool the new connection will belong to</li>
%%  <li><em>Host</em>:      the host name of the database to connect to</li>
%%  <li><em>Port</em>:      the port of the database to connect to, or <code>default</code></li>
%%  <li><em>User</em>:      the user name to use for connection</li>
%%  <li><em>Password</em>:  the password of the user.</li>
%%  <li><em>Database</em>:  the name of the database to use.</li>
%%  <li><em>Options</em>:   the additional connection options.</li>
%%  <li><em>Timeout</em>:   the timeout until the connection should be established</li>
%% </ul>
%% Example:
%% <pre>
%%  case edbi:connect(mysql, products_pool, "localhost", 3306, "user_a", "secret_pwd", "products", [{encoding, "utf8"}], infinity) of 
%%    {ok, ConnectionPid} -> % connection to mysql database established and added to pool 'products_pool'
%%      edbi:execute(ConnectionPid, "INSERT INTO products (id, name) VALUES (1, 'nice product')"), % execute statement on that connection
%%      edbi:connect(products_pool, 5), % add 5 more connection to pool
%%      edbi:execute(products_pool, "DELETE FROM products WHERE expired_at > NOW()"); % execute statement on any free connection from pool
%%    {error, Reason} -> handle_connection_error(Reason)
%%  end,
%%
%%  edbi:connect(psql, users, "127.0.0.1", 5432, "user_b", "other_pwd", "users_database", [{reconnect, false}], 5000). % connect to postgres database
%%
%% </pre>
%% See {@link connect/10} to see how to connect with initial pool size.<br/>
%% See {@link connect/2} to see how to add more connections to the created pool.
connect(Driver, PoolName, Host, Port, User, Password, Database, Options, Timeout) when ?IS_TIMEOUT(Timeout) ->
  gen_server:call(?MODULE, {connect, Driver, PoolName, Host, Port, User, Password, Database, Options, Timeout}, Timeout).
  

%% @spec connect(atom(), pool(), integer(), list(), tcp_port(), list(), list(), list(), connection_options(), integer() | infinity) 
%%    -> {ok, SuccessfulConnections} | {error, SuccessfulConnections, NumFailedConnections, ConnectionErrors} | {error, timeout}
%%      where SuccessfulConnections = [connection()]
%%            NumFailedConnections = integer()
%%            ConnectionErrors = [{error, Reason}]
%% @doc Connects to a given data source <b>with initial number of connections in the connection pool</b>.
%% like {@link connect/9} but connects with:
%% <ul>
%%  <li>Initial number of connections in the pool.</li>
%% </ul>
%% @equiv connect/9
%% @see connect/9
connect(Driver, PoolName, NumConnections, Host, Port, User, Password, Database, Options, Timeout) when ?IS_TIMEOUT(Timeout) ->
  gen_server:call(?MODULE, {connect, Driver, PoolName, NumConnections, Host, Port, User, Password, Database, Options, Timeout}, Timeout).


%% @spec connect(pool()) -> {ok, connection()} | {error, Reason}
%% @doc <b>Adds one more connection</b> to an existing pool. 
%% Like {@link connect/9} but adds only one connection to the existing pool.
%% @equiv connect/2
%% @see connect/2
connect(PoolName) when is_atom(PoolName) ->
  connect(PoolName, 1).

%% @spec connect(pool(), integer()) 
%%    -> {ok, SuccessfulConnections} | {error, SuccessfulConnections, NumFailedConnections, ConnectionErrors} | {error, timeout}
%%      where SuccessfulConnections = [connection()]
%%            NumFailedConnections = integer()
%%            ConnectionErrors = [{error, Reason}]
%% @doc <b>Adds more connections</b> to an existing pool. It uses the connection options from the first connection in the pool.
%% <ul>
%%  <li><em>PoolName</em>:  the name of the pool to add the connections to</li>
%%  <li><em>NumConnections</em>: The number of connections to add to the pool</li>
%% </ul>
connect(PoolName, NumConnections) when is_atom(PoolName), is_integer(NumConnections) ->
  gen_server:call(?MODULE, {connect, PoolName, NumConnections}).

% returns ok
disconnect(ConnectionPid) when is_pid(ConnectionPid) ->
  gen_server:call(?MODULE, {disconnect, connection, ConnectionPid}).

% returns {ok, NumDisconnectedConnections}
disconnect_pool(PoolName) when is_atom(PoolName) ->
  gen_server:call(?MODULE, {disconnect, pool, PoolName}).

% returns {ok, NumDisconnectedConnections}
disconnect_pools(Driver) when is_atom(Driver) ->
  gen_server:call(?MODULE, {disconnect, pools, Driver}).

%% @spec prepare(pool() | connection(), atom(), string()) -> ok
%% @doc Prepares a statement. The statement can then later be executed by calling {@link execute/3} or
%% {@link execute/4}. All "?" placeholders are then replaced by the parameters passed to the {@link execute/3}/
%% {@link execute/4} function.
%% Acts like {@link prepare/5} but with <em>Lazy</em> = <code>true</code> and <em>Timeout</em> = <code>infinity</code>.
%% <br/><br/>
%% <ul>
%%  <li><em>PoolName | ConnectionPid</em>: 
%%      <ul>
%%        <li><code>PoolName::atom()</code>: The name of a connection pool. The statement will be prepared on any connection of 
%%            the pool and will be prepared automatically for any new connection that is added to the pool 
%%            (e.g. with {@link connect/1} or {@link connect/2}).</li>
%%        <li><code>ConnectionPid::pid()</code>: The pid of a connection. The statement is then prepared on that connection 
%%            only <b>and on no other connection of the pool or new connections.</b> Use this only if you know what you are doing.</li>
%%      </ul>
%%  </li>
%%  <li><em>StatementName</em>: The unique name of the prepared statement. This is used to identify the statement when executing
%%        it with {@link execute/3} or {@link execute/4}. The name must be unique only for a pool (so it can be reused of other
%%        prepared statements on other pools).</li>
%%  <li><em>Statement</em>: The SQL statement to prepare. All "?" are replaced by the given values when executing the statement.<br/></li>
%%  <li><em>Returns</em>: always <code>ok</code></li>
%% </ul>
%% <br/>
%% Example:
%% <pre>
%%  % connect to mysql database and creatate to pool 'products_pool' 
%%  {ok, ConnectionPid} = edbi:connect(mysql, products_pool, "localhost", 3306, "user_a", "secret_pwd", "products", [], infinity),
%%  {ok, _NewConnections} = edbi:connect(products_pool, 5), % add 5 more connection to pool 'products_pool'
%%  
%%  % prepare a statement for the connection pool
%%  ok = edbi:prepare(products_pool, my_insert_statement, "INSERT INTO products (name, price) VALUES (?, ?)"),
%%
%%  % the prepared statement is prepared on any connection from the pool and can be executed on it
%%  {updated, _NumInsertedRows = 1} = edbi:execute(products_pool, my_insert_statement, ["Nice Product", 10.0]),
%%
%%  % add a new connection to the pool
%%  {ok, NewConnectionPid} = edbi:connect(products_pool),
%%
%%  % the prepared statement is also prepared for the new connection in the pool and can be executed on it
%%  {updated, _NumInsertedRows = 1} = edbi:execute(NewConnectionPid, my_insert_statement, ["Another Product", 22.0])
%% 
%% </pre><br/>
%% Tip: A statement needs to be prepared only once and than can be executed as often as needed. It is also recommended
%% to use prepared statements rather than unprepared statements like with {@link execute/2}, because they are faster
%% and safe agains sql injection.<br/><br/>
%% Tip: The statements are prepared lazy by default, that means they are prepared not before they are executed the 
%% first time by calling {@link execute/3} or {@link execute/4}. If you want to prepare them immediately, e.g. to
%% make sure that they will not fail, use {@link prepare/5} and set the <em>Lazy</em> parameter to <code>false</code>
%% @equiv prepare/5
%% @see prepare/5
%% @see unprepare/2
%% @see unprepare/1
prepare(PoolName, StatementName, Statement) when is_atom(PoolName), is_atom(StatementName), is_list(Statement)->
  prepare(PoolName, StatementName, Statement, _Lazy = true, _Timeout = infinity);
prepare(ConnectionPid, StatementName, Statement) when is_pid(ConnectionPid), is_atom(StatementName), is_list(Statement)->
  prepare(ConnectionPid, StatementName, Statement, _Lazy = true, _Timeout = infinity).

%% @spec prepare(pool() | connection(), atom(), string(), boolean(), integer() | infinity)  
%% -> ok | {error, Reason}
%% @doc Prepares a statement lazy or not lazy and with timeout.
%% like {@link prepare/3} but with the following options:
%% <ul>
%%  <li><em>Lazy</em>: If set to <code>true</code> (default), the statement is prepared when it is used the first time (the first time executed).
%%      Set this to <code>false</code> if you want to prepare the statement immediately to check whether it can be prepared or not. This is
%%      useful for debuging or if you generate the statements.</li>
%%  <li><em>Timeout</em>: The timeout until the opertation should be aborted. This has only affect when <em>Lazy</em> is set to 
%%      <code>true</code><br/></li>
%%  <li><em>Returns</em>: always <code>ok</code> when <em>Lazy</em> is set to <code>true</code> but can return <code>{error, Reason}</code>
%%      when <em>Lazy</em> is set to <code>false</code> and an the statement could not be prepared or the timeout exceeded.</li>
%% </ul>
%% Example:
%% <pre>
%%  prepare_generic_statement(ColumnName) when is_list(ColumnName) ->
%%    PreparedStatement = "SELECT " ++ ColumnName ++ " FROM products WHERE " ++ ColumnName ++ " == ?",
%%    % prepare the generated a statement immediately to make sure the generated statement is correct
%%    edbi:prepare(products_pool, list_to_atom("get_" ++ ColumnName), PreparedStatement, _Lazy = true, _Timeout = 5000).
%%
%%  init() ->
%%    ok = prepare_generic_statement("name"),
%%    
%%    % this will fail because "ORDER" is a reserved keyword and not a column name, so the statement cannot be prepared
%%    {error, _} = prepare_generic_statement("ORDER").
%% 
%% </pre>
prepare(PoolName, StatementName, Statement, Lazy, Timeout) when is_atom(PoolName), ?IS_TIMEOUT(Timeout) ->
  ok;
prepare(ConnectionPid, StatementName, Statement, Lazy, Timeout) when is_pid(ConnectionPid), ?IS_TIMEOUT(Timeout) ->
  ok.

% unprepares the statement for all pools on any driver
unprepare(StatementName) when is_atom(StatementName) ->
  ok.
unprepare(PoolName, StatementName) when is_atom(PoolName), is_atom(StatementName) ->
  ok;
unprepare(ConnectionPid, StatementName) when is_pid(ConnectionPid), is_atom(StatementName)->
  ok.

%% @spec execute(pool() | connection(), string())
%%  -> {updated, num_rows()} | {selected, col_names(), rows()} | {error, Reason}
%% @doc Executes an sql statement.
%% <ul>
%%  <li><em>PoolName | ConnectionPid</em>: 
%%      <ul>
%%        <li><code>PoolName::atom()</code>: The name of a connection pool. The statement is then executed on one connection from the pool</li>
%%        <li><code>ConnectionPid::pid()</code>: The pid of a connection. The statement is then executed on that connection</li>
%%      </ul>
%%  </li>
%%  <li><em>SQLstatement</em>: The SQL statement to execute<br/></li>
%%  <li><em>Returns</em>:  
%%      <ul>
%%        <li><code>{updated, num_rows()}</code>: for an INSERT,UPDATE or DELETE statement while <code>num_rows()</code> is the number of affected rows, or ...</li>
%%        <li><code>{selected, col_names(), rows()}</code>: for a SELECT statement while <code>col_names()</code> a list of the returned column names and
%%                                and <code>rows()</code> is a list of rows containing the columns as tuples(), or ...</li>
%%        <li><code>{error, Reason}</code>: if an error occurs</li>
%%      </ul>
%%  </li>
%% </ul>
%% <br/>
%% Examples:
%% <pre>
%%  % connect to mysql database and creatate to pool 'products_pool' 
%%  {ok, ConnectionPid} = edbi:connect(mysql, products_pool, "localhost", 3306, "user_a", "secret_pwd", "products", [], infinity),
%%  {ok, _NewConnections} = edbi:connect(products_pool, 5), % add 5 more connection to pool 'products_pool'
%%  
%%  % execute statement on any free connection from pool
%%  case edbi:execute(products_pool, "INSERT INTO products (id, name) VALUES (1, 'nice product')") of
%%    {updated, NumInsertedRows} -> io:format("well done!");
%%    {error, _Reason} -> io:format("shit. maybe product with id already exists and table has a unique key on it")
%%  end,
%% 
%%  % execute sql statement on a single connection
%%  {updated, NumDeletedRows} = edbi:execute(ConnectionPid, "DELETE FROM products WHERE expired_at > NOW()"); 
%%
%%  % execute a select statement
%%  {selected, _ColNames = ["id", "name"], _Rows = [FirstRow |_]} = edbi:execute(products_pool, "SELECT id, name FROM products LIMIT 100"),
%%  {FirstProductId, FirstProductName} = FirstRow,
%% </pre><br/>
%% Tip: To execute a SQL statement with a timeout, use the overloaded {@link execute/3} function:<br/>
%% <code>execute(PoolName::pool() | ConnectionPid::pid, SQLstatement::string(), Timeout::integer() | infinity) -></code> {@link query_result()}<br/><br/>
%% Tip: Better use prepared statements whenever possible. See {@link prepare/3}.
execute(PoolName, SQLstatement) when is_atom(PoolName), is_list(SQLstatement) ->
  execute(PoolName, SQLstatement, _Timeout = infinity);
execute(ConnectionPid, SQLstatement) when is_pid(ConnectionPid), is_list(SQLstatement) ->
  execute(ConnectionPid, SQLstatement, _Timeout = infinity).
  
%% @spec execute(pool() | connection(), atom(), list()) 
%%  -> {updated, num_rows()} | {selected, col_names(), rows()} | {error, Reason}
%% @doc Executes a prepared statement. The statment needs to be prepared by calling
%% the function {@link prepare/3} or {@link prepare/4} before. 
%% 
%% <ul>
%%  <li><em>PoolName | ConnectionPid</em>: 
%%      <ul>
%%        <li><code>PoolName::atom()</code>: The name of a connection pool. The prepared statement is then executed on one connection from the pool</li>
%%        <li><code>ConnectionPid::pid()</code>: The pid of a connection. The prepared statement is then executed on that connection</li>
%%      </ul>
%%  </li>
%%  <li><em>PreparedStatementName</em>: The name with that the statement was prepared before by calling {@link prepare/3} or {@link prepare/4}</li>
%%  <li><em>Params</em>: The parameters to pass to the prepared statement. Each parameter replaces a placeholder "?" from the prepared statement.<br/></li>
%%  <li><em>Returns</em>:  
%%      <ul>
%%        <li><code>{updated, num_rows()}</code>: if the prepared statement is an INSERT,UPDATE or DELETE statement while <code>num_rows()</code> is the number of affected rows, or ...</li>
%%        <li><code>{selected, col_names(), rows()}</code>: if the prepared statement is a SELECT statement while <code>col_names()</code> a list of the returned column names and
%%                                and <code>rows()</code> is a list of rows containing the columns as tuples(), or ...</li>
%%        <li><code>{error, Reason}</code>: if an error occurs</li>
%%      </ul>
%%  </li>
%% </ul>
%% <br/>
%% Examples:
%% <pre>
%%  % connect to mysql database and creatate to pool 'products_pool' 
%%  {ok, ConnectionPid} = edbi:connect(mysql, products_pool, "localhost", 3306, "user_a", "secret_pwd", "products", [], infinity),
%%  {ok, _NewConnections} = edbi:connect(products_pool, 5), % add 5 more connection to pool 'products_pool'
%%  
%%  % prepare a statement for the connection pool
%%  ok = edbi:prepare(products_pool, my_insert_statement, "INSERT INTO products (name, price) VALUES (?, ?)"),
%%
%%  % execute the prepared statement 'my_insert_statement' on any free connection from pool 'products_pool'
%%  ProductName = "Nice Product",
%%  ProductPrice = 10.5,
%%  case edbi:execute(products_pool, my_insert_statement, [ProductName, ProductPrice]) of
%%    {updated, _NumInsertedRows = 1} -> io:format("well done!");
%%    {error, _Reason} -> io:format("something did not work")
%%  end,
%% 
%%  % execute the prepared statement on a single connection
%%  {updated, InsertedRows = 1} = edbi:execute(ConnectionPid, my_insert_statement, ["Cheap Product", 0.10]); 
%%
%% </pre><br/>
%% Tip: This function is also overloaded to act like {@link execute/2} but with a timeout. <br/>
%% See also {@link execute/4} to see how to execute prepared statements with a timeout.
%% 
execute(PoolName, PreparedStatementName, Params) when is_atom(PoolName), is_atom(PreparedStatementName), is_list(Params) ->
  execute(PoolName, PreparedStatementName, Params, _Timeout = infinity);
execute(ConnectionPid, PreparedStatementName, Params) when is_pid(ConnectionPid), is_atom(PreparedStatementName), is_list(Params) ->
  execute(ConnectionPid, PreparedStatementName, Params, _Timeout = infinity);
execute(PoolName, SQLstatement, Timeout) when is_atom(PoolName), is_list(SQLstatement), ?IS_TIMEOUT(Timeout) ->
  ok;
execute(ConnectionPid, SQLstatement, Timeout) when is_pid(ConnectionPid), is_list(SQLstatement), ?IS_TIMEOUT(Timeout) ->
  ok.
  
%% @spec execute(pool() | connection(), atom(), list(), integer() | infinity) 
%%  -> {updated, num_rows()} | {selected, col_names(), rows()} | {error, Reason}
%% @doc Executes a prepared statement with a timeout.
%% Acts like {@link execute/3} but exits after a timeout.<br/>
%% <br/>
%% Example:
%% <pre>
%%  % prepare the statement
%%  ok = edbi:prepare(products_pool, my_prepared_statement, "SELECT * FROM products WHERE price &lt; ?"),
%%
%%  case edbi:execute(products, my_prepared_statement, [_Price = 0.50], _Timeout = 1000) of
%%    {selected, _ColNames, CheapProducts} -> send_cheap_products_offer(CheapProducts);
%%    {error, timeout} -> send("buy the expensive products - or be patient"); 
%%  end
%% </pre>
%% Tip: be careful when using a timeout with statements that modify the datasource, e.g. with INSERT, UPDATE or
%% DELETE statements, because a timeout does not mean, that the statement was not successful or will be successful later.
%% If you want to make sure, that the update operation is not performed when the timeout occurred, use a transaction and
%% roll it back then.
%%
%% @equiv execute/3
%% @see execute/3
execute(PoolName, PreparedStatementName, Params, Timeout) when is_atom(PoolName), is_atom(PreparedStatementName), is_list(Params), ?IS_TIMEOUT(Timeout) ->
  ok;
execute(ConnectionPid, PreparedStatementName, Params, Timeout) when is_pid(ConnectionPid), is_atom(PreparedStatementName), is_list(Params), ?IS_TIMEOUT(Timeout) ->
  ok.


  
transaction(ConnectionPid, start) when is_pid(ConnectionPid) ->
  transaction(ConnectionPid, start, _Timeout = infinity);  
transaction(ConnectionPid, commit) when is_pid(ConnectionPid) ->
  transaction(ConnectionPid, commit, _Timeout = infinity);
  
transaction(ConnectionPid, rollback) when is_pid(ConnectionPid) ->
  transaction(ConnectionPid, rollback, _Timeout = infinity);
  
transaction(ConnectionPid, Fun) when is_pid(ConnectionPid), is_function(Fun)->
  transaction(ConnectionPid, Fun, _Timeout = infinity);
% starts a transaction on any free connection in the pool
transaction(PoolName, Fun) when is_atom(PoolName), is_function(Fun)->
  transaction(PoolName, Fun, _Timeout = infinity).

%% @spec transaction(pool() | connection(), function(), integer()) 
%%  ->  ok | {error, Reason}
%% @doc Executes operations in a transaction or starts, commits or rolls back a transaction manually.
% starts a transaction on any free connection in the pool
transaction(PoolName, Fun, Timeout) when is_atom(PoolName), is_function(Fun), ?IS_TIMEOUT(Timeout) ->
  ok;
transaction(ConnectionPid, start, Timeout) when is_pid(ConnectionPid), ?IS_TIMEOUT(Timeout) ->
  ok;  
transaction(ConnectionPid, commit, Timeout) when is_pid(ConnectionPid), ?IS_TIMEOUT(Timeout) ->
  ok;
transaction(ConnectionPid, rollback, Timeout) when is_pid(ConnectionPid), ?IS_TIMEOUT(Timeout) ->
  ok;
transaction(ConnectionPid, Fun, Timeout) when is_pid(ConnectionPid), is_function(Fun), ?IS_TIMEOUT(Timeout) ->
  ok.


% to implement later
set_log_leve(LogLevel) -> ok.
set_log_fun(LogFun) -> ok.
commit(ConnectionPid) -> ok.
rollback(ConnectionPid) -> ok.
rollback(ConnectionPid, SavePointName) -> ok.
set_savepoint(ConnectionPid, SavePointName) -> ok.
release_savepoint(ConnectionPid, SavePointName) -> ok.
% sets auto commit to off and starts a transaction manually
start_transaction(ConnectionPid) -> ok.
start_transaction(ConnectionPid, SavePointName) -> ok.
%% @spec set_transaction_isolation(connection(), read_uncommitted | read_committed | repeatable_read | serializable | none)
%%  -> ok | {error, Reason}
set_transaction_isolation(ConnectionPid, IsolationLevel) -> ok.

%% gets a connection from a pool
get_connection(PoolName) ->
  ok.
  
%% USE ONLY in tranaction, so it gets the inserted id of the connection pid that is executing the transaction
get_inserted_id(TableName) ->
  ok.

describe_table(ConnectionPid, Table) ->
  ok.
  
register_driver(Driver) when is_atom(Driver) -> 
  ok.
  
registered_drivers() ->
  ok.
  
registered_pools() ->
  [{poolName, driverName, poolPids}].

%% @spec rows_to_records(atom(), query_result()) -> [record(RecordName)]  
rows_to_records(RecordName, QueryResult) ->
  ok.

rows_to_lists(QueryResult) ->
  ok.
  
%%====================================================================
%% Internal functions
%%====================================================================


handle_call({connect, PoolName, NumConnections}, _From, State) ->
  case get_pool(PoolName, State) of
    {value, Pool} ->
      {Reply, NewState} = start_connection(Pool, State),
      {reply, Reply, NewState};
    none -> {reply, {error, pool_not_registered}, State}      
  end;

handle_call({connect, Driver, _PoolName = undefined, Host, Port, User, Password, Database, Encoding, Reconnect, Timeout}, From, State) ->
  PoolName = create_default_pool_name(State),
  handle_call({connect, Driver, PoolName, Host, Port, User, Password, Database, Encoding, Reconnect, Timeout}, From, State);


handle_call({connect, Driver, PoolName, Host, Port, User, Password, Database, Encoding, Reconnect, Timeout}, _From, State) ->
  case is_registered(Driver, State) of
    true ->
      Pool = #pool {
        driver = Driver, 
        name = PoolName, 
        host = Host, 
        port = Port, 
        user = User, 
        password = Password, 
        database = Database, 
        encoding = Encoding, 
        reconnect = Reconnect, 
        timeout = Timeout
      },
      case get_pool(PoolName, State) of
        none -> % new pool 
          {Reply, NewState} = start_connection(Pool, State),
          {reply, Reply, NewState};
        {value, _Pool = #pool {driver = Driver, name = PoolName, host = Host, port = Port, user = User, password = Password, database = Database}} -> % pool with same connection properties exists
          {Reply, NewState} = start_connection(Pool, State),
          {reply, Reply, NewState};
        {value, DifferentPool} ->
          {reply, {error, {pool_already_registered, DifferentPool}}, State}
      end;
    false ->
      {reply, {error, driver_not_registered}, State}
  end.

get_pool(PoolName, State = #state {}) when is_atom(PoolName) ->
  none.
  
is_registered(Driver, State = #state {}) when is_atom(Driver) ->
  true.
  
create_default_pool_name(State = #state {}) ->
  default.

start_connection(Pool = #pool {}, State = #state {}) ->
  Driver = Pool#pool.driver,
  case Driver:connect(Pool#pool.name, Pool#pool.host, Pool#pool.port, Pool#pool.user, Pool#pool.password, Pool#pool.database, Pool#pool.encoding, Pool#pool.reconnect, Pool#pool.timeout) of
    {ok, ConnectionPid} -> 
      NewState = State,
      {{ok, ConnectionPid}, NewState};
    {error, Reason} -> 
      {{error, Reason}, State}
  end. 
