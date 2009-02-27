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
%% @author Bertram Harendt <info@bertram-harendt.de>
%% @version {@version}
%% @end
%% =====================================================================
%% ---------------------------------------------------
%% @doc EDBI driver behaviour - Behaviour for database drivers that can be used with edbi application.
%%
%% 
%% The idea behind this edbi_driver behaviour is that the user module that implements this behavior
%% provides functions to handle the available edbi database operations. 
%%
%% The user module should export:
%% <pre>
%%   connect(Driver, PoolName, Host, Port, User, Password, Database, Encoding, Reconnect, Timeout) 
%%     ==> {ok, ConnectionPid}
%%         {error, Reason}
%%
%%   get_default_port()
%%      ==> {ok, DefaultPort}
%%          undefined
%% </pre>
%% To be able to use your edbi driver implementation, you must register your module at the edbi
%% module by calling {@link edbi:register_driver/1}. 
%% If you driver comes as an <a href="http://erlang.org/doc/man/application.html">Erlang Application</a>,
%% make sure, that the edbi application is started before your driver application to be able to register
%% and set the <em>applications</em> configuration parameter in your .app file.
%% <pre>
%% {application, my_driver_app,
%% [{description, "driver to crazy database"},
%%  {vsn, "1"},
%%  {modules, [my_driver_sup, my_driver_app]},
%%  {registered, []},
%%  {applications, [edbi]},
%%  {mod, {my_driver_app,[]}},
%%  {env, []}
%% ]}.
%% </pre>
%% @end
%% ---------------------------------------------------
-module(edbi_driver).

%% API
-export([behaviour_info/1]).
-export([connect/9, get_default_port/0]).

%%====================================================================
%% API
%%====================================================================


%% @private
behaviour_info(callbacks) ->
  [{connect, 9}, {get_default_port, 0}];
behaviour_info(_Other) ->
  undefined.

%% @spec connect(atom(), list(), integer(), list(), list(), list(), list(), bool(), integer() | infinity) -> {ok, ConnectionPid::pid()} | {error, Reason::term()}
%% @doc Connects to the given data source
connect(PoolName, Host, Port, User, Password, Database, Encoding, Reconnect, Timeout) when ?IS_TIMEOUT(Timeout) -> {ok, self()}.

%% @spec get_default_port() -> port()
%%  where port() = integer() | undefined
%% @doc Gets the default port of the database.
%% Returns <code>undefined</code> if the database has no default port.
get_default_port() -> 80.


