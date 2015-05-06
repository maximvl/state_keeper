-module(state_keeper_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = application:ensure_started(er),
    state_keeper_sup:start_link().

stop(_State) ->
    ok.
