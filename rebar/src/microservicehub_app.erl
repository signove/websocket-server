-module(microservicehub_app).

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
            {"/microservicehub/[...]", cowboy_static, {priv_dir, microservicehub, "microservicehub"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    %%SSLDir = code:priv_dir(microservicehub) ++ "/ssl/",
    %%io:format("~s ~n", [SSLDir]),
    %%{ok, _} = cowboy:start_tls(https, [{port, 8080}, {certfile, SSLDir ++ "cert.pem"}, {keyfile, SSLDir ++ "key.pem"}], [{env, [{dispatch, Dispatch}]}]),

    microservicehub_sup:start_link().

stop(_State) ->
    server_session_manager:stop(first),
    ok.
