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

%% @doc EDBI Application
%%

-module(edbi_app).

-behaviour(application).

% exports
-export([start/0, stop/0]).
% callbacks
-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

%% @spec start() -> ok | {error, {already_started, edbi}} | {error, Reason}
%% @doc Starts the edbi application.
start() ->
  case application:load(edbi) of
    Loaded when Loaded == ok; {error,{already_loaded, mnesia}} == Loaded ->
      application:start(edbi);
    {error, Reason} -> 
      {error, Reason}
  end.
  
%% @spec stop() -> ok | {error, Reason}
%% @doc Stops the edbi application.
stop() ->
  application:stop(edbi).

%%====================================================================
%% Application callbacks
%%====================================================================

%% @hidden
start(_StartType, StartArgs) -> 
  case edbi_sup:start_link(StartArgs) of
    {ok, Pid} -> {ok, Pid};
    Error -> Error
  end.

%% @hidden  
start_phase(_Phase, _StartType, _PhaseArgs) -> 
  ok.

%% @hidden  
prep_stop(State) -> 
  State.

%% @hidden  
stop(State) ->
  ok.

%% @hidden  
config_change(_Changed, _New, _Removed) -> 
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
