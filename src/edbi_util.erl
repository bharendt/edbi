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


%% @doc Utilities for edbi driver.
%% This module provides utilities for edbi drivers.

-module(edbi_util).

-include("edbi.hrl").

-version(1.0).

-export([parse_url/1, get_option/2, get_option/3]).

-ifdef(TEST).
-include("test/edbi_util_test.erl").
-endif.

%%====================================================================
%% API functions
%%====================================================================


%% @spec parse_url(list()) -> {Driver, Host, Port, Database, UrlOptions} | {error, illegal_url}
%%  where Driver = atom(), Host = list(), Port = integer() | default, Database = list(), UrlOptions = [UrlOption],
%%        UrlOption = {Key::atom(), Value}, Value = bool() | integer() | float() | string()
%% @doc Parses a connection url.<br/>
%% It returns the details from the connection url as tuple, or <code>{error, illegal_url}</code> if the
%% connection url could not be parsed.<br/>
%% A valid connection url must have this format:<br/><br/> 
%%       <code>"[Driver Name]://[Host]:[Port]/[Database]?[Option1]=[Value1]&amp;[Option1]=[Value1]"</code><br/>
%% <br/>
%% <em>Port</em> and <em>Options</em> are optional.<br/>
%% The keys of the options are converted to atoms, the values to boolean, integer or float if possible. If a key
%% has no value, the boolean value <code>true</code> is used as value.<br/><br/>
%% Example:
%% <pre>
%%  case edbi_util:parse_url("mysql://localhost:3306/products?user=user_name&amp;password=secret") of
%%    {mysql, "localhost", 3306, "products", [{user, "user_name"}, {password, "secret"}] } -> ok;
%%    {error, illegal_url} -> shit
%%  end,
%%
%%  case edbi_util:parse_url("postgres://192.168.0.110/users?reconnect&amp;reconnect_timeout=1000") of
%%    {postgres, "localhost", default, "users", [{reconnect, true}, {reconnect_timeout, 1000}] } -> ok;
%%    {error, illegal_url} -> shit
%%  end,
%% </pre>
parse_url(Url) ->
  case string:tokens(Url, "://?") of
    [Driver, Host, Port, Database, Params] -> 
        return_parsed_url(Driver, Host, Port, Database, Params);
    [Driver, Host, Port, Database]  -> 
% | [Driver, Host, Database, Params]  -> 
        case string:to_integer(Port) of
          {Int, _} when is_integer(Int) ->
            return_parsed_url(Driver, Host, Port, Database, _Params = "");
          _ ->
            return_parsed_url(Driver, Host, _Port = "", _Database = Port, _Params = Database)
        end;            
    [Driver, Host, Database] -> 
        return_parsed_url(Driver, Host, _Port = "", Database, _Params = "");
    _ -> {error, illegal_url}
  end.
  
%% @spec get_option(atom(), [Option]) -> {value, Value::term()} | false
%%  where Option = {Key::atom(), Value::term()}
%% @doc Gets an option from an option lists. 
get_option(OptionName, Options) ->
  case lists:keysearch(OptionName, 1, Options) of
    {value, {OptionName, Value}} -> {value, Value};
    _ -> false
  end.

%% @spec get_option(atom(), [Option], term()) -> {value, Value::term()}
%%  where Option = {Key::atom(), Value::term()}
%% @doc Gets an option from an option lists or returns the default value, if option does not exist. 
get_option(OptionName, Options, DefaultValue) ->
  case get_option(OptionName, Options) of
    {value, Value} -> {value, Value};
    false -> {value, DefaultValue}
  end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @spec(string(), string(), string(), string(), string()) 
%%  -> {Driver, Host, Port, Database, UrlOptions} | {error, illegal_url}
%%  where Driver = atom(), Host = list(), Port = integer() | default, Database = list(), UrlOptions = [UrlOption],
%%        UrlOption = {Key::atom(), Value}, Value = bool() | integer() | float() | string()
%% @doc Returns the values from parsed url with correct types and parses the params.
return_parsed_url(Driver, Host, Port, Database, Params) -> 
  case parse_url_params(string:strip(Params)) of
    {error, illegal_url} -> {error, illegal_url};
    Options when is_list(Options) ->
      case string:strip(Port) of
        [] -> 
          {list_to_atom(Driver), string:strip(Host), _Port = default, string:strip(Database), Options};
        StrippedPort ->
          case string:to_integer(StrippedPort) of
            {error, _} -> {error, illegal_url};
            {Int, _} ->
              {list_to_atom(Driver), string:strip(Host), Int, string:strip(Database), Options}
          end
      end
  end.

%% @spec parse_url_params(UrlParams::string()) -> [UrlOption] | {error, illegal_url}
%%  where UrlOption = {Key::atom(), Value}, Value = bool() | integer() | float() | string()
%% @doc Parses the parameters from a connection url.
parse_url_params([]) ->
  [];
parse_url_params(UrlParams) ->
  ParseResult = lists:foldl(
    fun(_, {error, illegal_url}) -> {error, illegal_url};
       (KeyValuePair, Acc) -> 
      case string:tokens(KeyValuePair, "=") of
        [Key, Value] -> [{list_to_atom(string:strip(Key)), convert_url_param_value(string:strip(Value))} | Acc];
        [KeyValuePair] -> [{list_to_atom(string:strip(KeyValuePair)), true} | Acc];
        _ -> {error, illegal_url}
      end
  end, [], string:tokens(UrlParams, "&")),
  case ParseResult of
    {error, illegal_url} -> {error, illegal_url};
    Options -> lists:reverse(Options)
  end.
  
%% @spec convert_url_param_value(string()) -> bool() | integer() | float() | list()
%% @doc Converts the value from an url param to the closes possible type.
convert_url_param_value(Value) -> 
  case string:to_lower(Value) of
    "true" -> true;
    "false" -> false;
    _ ->
      case string:to_integer(Value) of
        {Int, []} -> Int;
        _ -> 
          case string:to_float(Value) of
            {Float, []} -> Float;
            _ -> Value
          end
      end
  end.