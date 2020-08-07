-module(multiscreen_ws_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %server_session:start(),
    server_session_manager:start(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", rt_client_session, []},
            {"/websocket/config", config_client_session, []},
            {"/multiscreen/[...]", cowboy_static, {priv_dir, multiscreen_ws, "multiscreen"}}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    %%SSLDir = code:priv_dir(multiscreen_ws) ++ "/ssl/",
    %%io:format("~s ~n", [SSLDir]),
    %%{ok, _} = cowboy:start_https(https, 100, [{port, 8080}, {certfile, SSLDir ++ "cert.pem"}, {keyfile, SSLDir ++ "key.pem"}], [{env, [{dispatch, Dispatch}]}]),

    multiscreen_ws_sup:start_link().

stop(_State) ->
    server_session_manager:stop(first),
    ok.
