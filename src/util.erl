%% =====================================================================
%% @copyright Bertram Harendt
%% @author Bertram Harendt <edbi@bertram-harendt.de>
%% @version {@version}
%% @end
%% =====================================================================


%% @doc Development Utilities.
%% Provides utilities to run eunit tests and recompile and reload the
%% code during development from erlang console.
-module(util).

-export([is_alive/1]).
-export([test/1, test_module/1]).
-export([reload_modules/0, refresh/0, refresh/1, refresh/3]).

-include("edbi.hrl").

%% @spec is_alive(pid() | atom()) -> true | false
%% @doc Tests whether a pid is alive or not.
%% Works also for process ids that runs on remote nodes. Instead
%% of pid you can also pass the name of a locally or globally
%% registered server.
is_alive(Pid) when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]);
is_alive(ResisteredName) when is_atom(ResisteredName) ->
  case whereis(ResisteredName) of
    undefined -> global:whereis_name(ResisteredName) /= undefined;
    _Pid -> true
  end;
is_alive(_) ->
    false.

%% @spec test(atom()) -> true | false
%% @doc Compiles a module and runs eunit tests for this module.
%% This can be used from the erlang console while developing.
test(ModuleName) when is_atom(ModuleName) ->
  case compile:file("src/" ++ atom_to_list(ModuleName) ++ ".erl", [report_errors,{i, "./include"}, {i, "./include/eunit"}, {outdir, "./ebin"}, {d,'TEST', true}]) of
   {ok, ModuleName} -> io:format("ok.~n"), true;
   Error -> io:format("~n  ****** FAILED ****** ~p~n",[Error]), false
  end.
  
%% @spec test_module(atom()) -> true | false
%% @doc Runs eunit test for a module (without compiling).
%% This is also used by Erlang textmate bundle.
test_module(Mod) ->
  eunit:test(Mod, [{verbose, true}]).
  

%% @spec refresh() -> ok
%% @doc Compiles all modules from the src directory and then performs hot code replace.
refresh() ->
  refresh(_ExcludedModules = [], []).

%% @spec refresh(atom()) -> ok
%% @doc Compiles a single module then performs hot code replace.
refresh(Module) when is_atom(Module) ->
  refresh(Module, get_processes(Module), []).

%% @hidden
refresh(ExcludedModules, AdditionalCompileOptions) ->
  refresh(ExcludedModules, AdditionalCompileOptions, [node()]).

%% @hidden
refresh(ExcludedModules, AdditionalCompileOptions, Nodes) when is_list(ExcludedModules), is_list(AdditionalCompileOptions), is_list(Nodes) ->
  io:format("~n"),
  AllModules = lists:map(
      fun(FileName) -> element(1,lists:splitwith(fun(C) -> C /= $. end, FileName)) 
  end, lists:filter(fun(FileName) -> lists:suffix(".erl", FileName) end, element(2,file:list_dir("src")))),

  Modules = lists:filter(fun(ModName) -> 
    not lists:member(list_to_atom(ModName), ExcludedModules)
  end, AllModules),

  Self = self(),
  
  NotCompiledOnNodes = lists:dropwhile(fun(Node) -> 
    io:format("Compiling modules on node ~p~n~n",[Node]),
    spawn(Node, fun() -> 
      CompiledModules = lists:takewhile(fun(Module) -> 
         io:format("\tCompiling Module ~s: ",[Module]),
         ModuleAsAtom = list_to_atom(Module),
         case compile:file("src/" ++ Module ++ ".erl", [report_errors,{i, "./include"}, {outdir, "./ebin"}] ++ AdditionalCompileOptions) of
           {ok, ModuleAsAtom} -> io:format("ok.~n"), true;
           _ -> io:format("~n\t  ****** FAILED ******~n"), false
         end
      end, Modules),
      if length(CompiledModules) == length(Modules) ->
           Self ! ok,
           io:format("done~n~n");
         true ->
           Self ! failed
      end
    end),
    receive 
      ok -> true;
      failed -> false
    end    
  end, Nodes),

   if length(NotCompiledOnNodes) == 0 ->
     io:format("~nCompiled modules.~n~n reloading modules ...~n~n"),
     reload_modules(Modules),
     io:format(" ok.~n");
   true -> error
   end,
  ok;
% refreshes a single module
refresh(Module, Processes, ExtraOptions) when is_atom(Module) ->
  io:format("~ncompiling module '~w':~n",[Module]),
  case compile:file("src/" ++ atom_to_list(Module) ++ ".erl", [verbose,report_errors,report_warnings, {i, "./include"}, {outdir, "./ebin"}] ++ ExtraOptions) of
    {ok, Module} ->
      [sys:suspend(Pid) || Pid <- Processes],
      code:purge(Module),
      case code:load_file(Module) of
        {module, Module} ->
          io:format("~nSUCCESSFULLY compiled and reloaded module '~w'",[Module]),
          if length(Processes) > 0 ->
            io:format("~nSending code change event to ~B processes~n",[length(Processes)]),
            [sys:change_code(Pid, Module, foo, foo) || Pid <- Processes];
          true -> ok
          end;
        ReloadError ->
          io:format("~nFAILED to reload module '~w': ~w",[Module, ReloadError])
      end,
      [sys:resume(Pid) || Pid <- Processes];
    CompileError ->
      io:format("~nFAILED to compile module '~w': ~w",[Module, CompileError])
  end.
  
%% @spec get_processes(atom() | list()) -> [pid()]
%% @doc Gets the processes for either one module or a list of modules.
%% To this processes the code change events will be sent.
get_processes(Modules) when is_list(Modules) ->
  lists:foldl(fun(Module, Pids) -> 
    Pids ++ get_processes(Module)
  end, [], Modules);
get_processes(edbi) ->
  get_pid_of_locally_registered_name(edbi);
get_processes(edbi_sup) ->
  get_pid_of_locally_registered_name(edbi_sup);
get_processes(_Module) ->
  [].

%%====================================================================
%% Internal functions.
%%====================================================================

get_pid_of_locally_registered_name(Name) ->
  case whereis(Name) of
    undefined -> [];
    Pid -> [Pid]
  end.


send_code_change_to_processes(ProcessId, Description) when is_pid(ProcessId), is_list(Description) ->
  send_code_change_to_processes([ProcessId], Description);

send_code_change_to_processes(ProcessIds, Description) when is_list(ProcessIds), is_list(Description) ->
  io:format("~n\tSending code_change to ~3.B ~s processes:~n", [length(ProcessIds), Description]),
  Processes = lists:filter(fun(Pid) -> 
   case catch sys:change_code(Pid, none, foo, foo) of
     ok -> true;
     Error -> io:format("Error: ~p~n",[Error]), false
   end
  end, ProcessIds),
  io:format("\tdone ----------------------------------------------~n"),
  Processes.

stop_processes(ProcessId, Description) when is_pid(ProcessId), is_list(Description) ->
  stop_processes([ProcessId], Description);

stop_processes(ProcessIds, Description) when is_list(ProcessIds), is_list(Description) ->
  io:format("\tStopping ~3.B ~s processes ...", [length(ProcessIds), Description]),
  StoppedProcesses = lists:filter(fun(Pid) -> 
   case catch sys:suspend(Pid) of
     ok -> true;
     Error -> io:format("Error: ~p~n",[Error]), false
   end
  end, ProcessIds),
  io:format(" done~n"),
  StoppedProcesses.

resume_processes(ProcessId, Description) when is_pid(ProcessId), is_list(Description) ->
  resume_processes([ProcessId], Description);

resume_processes(ProcessIds, Description) when is_list(ProcessIds), is_list(Description) ->
  io:format("\tResuming ~3.B ~s processes ...", [length(ProcessIds), Description]),
  lists:foreach(fun(Pid) -> 
     case catch sys:resume(Pid) of
       ok -> ok;
       Error -> io:format("Error: ~p~n",[Error]) 
     end
  end, ProcessIds),
  io:format(" done~n").

%% @hidden
reload_modules() ->
  AllModules = lists:map(
      fun(FileName) -> element(1,lists:splitwith(fun(C) -> C /= $. end, FileName)) 
  end, lists:filter(fun(FileName) -> lists:suffix(".erl", FileName) end, element(2,file:list_dir("src")))),
  ExcludedModules = [],
  Modules = lists:filter(fun(ModName) -> 
    not lists:member(list_to_atom(ModName), ExcludedModules)
  end, AllModules),
  reload_modules(Modules).

reload_modules(Modules) ->
  reload_modules(Modules, [node()]).  

%% @hidden
reload_modules(Modules, Nodes) when is_list(Modules), is_list(Nodes) ->

  Self = self(),
  
  Processes = lists:map(fun(Module) -> 
    {Module, get_processes(list_to_atom(Module))}
  end, Modules),
 
  lists:foreach(fun(Node) -> 
    io:format("Stopping processes on node ~p~n~n",[Node]),
    spawn(Node, fun() -> 
      lists:foreach(fun({ModuleName, Procs}) ->
        stop_processes(Procs, ModuleName)
      end, Processes),
      io:format("~n~n"),
      Self ! done
    end),
    receive done -> ok end    
  end, Nodes),


  lists:foreach(fun(Node) -> 
    io:format("Reloading modules on node ~p~n~n",[Node]),
    spawn(Node, fun() -> 
     ReloadedModules = lists:takewhile(fun(Module) -> 
       io:format("\tReloading Module ~s ...",[Module]),
       ModuleAsAtom = list_to_atom(Module),
       code:purge(ModuleAsAtom),
       case code:load_file(ModuleAsAtom) of
         {module, ModuleAsAtom} -> io:format(" ok.~n"), true;
         _ -> 
           io:format(" FAILED~n"), false end          
     end, Modules),
     if length(ReloadedModules) == length(Modules) -> 
        io:format("~n~B Modules reloaded.~n~nSending sys:change_code event to processes on node ~p ...~n~n",[length(ReloadedModules), Node]),
        lists:foreach(fun({ModuleName, Procs}) ->
          send_code_change_to_processes(Procs, ModuleName)
        end, Processes),
        io:format("~n~n");
     true ->
       io:format(" FAILED. Reloaded only ~B of ~B modules on node ~p: ~n~p~n",[length(ReloadedModules), length(Modules), Node, [Modules -- ReloadedModules]])
     end,
     io:format("~n~n"),
      Self ! done
    end),
    receive done -> ok end    
  end, Nodes),

  lists:foreach(fun(Node) -> 
    io:format("Resuming processes on node ~p~n~n",[Node]),
    spawn(Node, fun() -> 
      lists:foreach(fun({ModuleName, Procs}) ->
        resume_processes(Procs, ModuleName)
      end, Processes),
      io:format("~n~n"),
      Self ! done
    end),
    receive done -> ok end    
  end, Nodes),

 
  ok.
  


