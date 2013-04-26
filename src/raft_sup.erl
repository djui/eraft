%% @doc Top level supervisor.
-module(raft_sup).
-behaviour(supervisor).

%% API
-export([ start_link/0 ]).

%% Callbacks
-export([ init/1 ]).

%%% Macros =====================================================================
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%% API ========================================================================
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% Callbacks ==================================================================
init([]) ->
  {ok, {{one_for_one, 5, 10}, []}}.
