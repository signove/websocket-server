-module(server_session_manager).

-export([start/0, get_session_pid/1, delete_session_by_pid/1, stop/1]).

start() ->
    ets:new(session_list, [named_table, public]).

stop(first) ->
    stop(ets:first());
stop('$end_of_table') -> done;
stop({SessionKey, SessionServerPid}) ->
    io:format("Stopping Session ~s ~n", [SessionKey]),
    delete_session_by_pid(SessionServerPid),
    stop(ets:next()).

get_session_pid(SessionKey) ->
    SessionList = ets:lookup(session_list, SessionKey),
    case SessionList of
        [] ->
            {_Status, SessionServerPid} = gen_server:start(server_session, [], []),
            ets:insert(session_list, {SessionKey, SessionServerPid}),
            io:format("Creating new session ~s ~w ~n", [SessionKey, SessionServerPid]),
            SessionServerPid;
        [{SessionKey, SessionServerPid} ] ->
            io:format("Using existing session ~s ~w ~n", [SessionKey, SessionServerPid]),
            SessionServerPid
    end.

delete_session_by_pid(ServerSessionPid) ->
    case ets:match(session_list, {'$1', ServerSessionPid}) of
        SessionKey ->
            ets:delete(session_list, list_to_binary(SessionKey)),
            io:format("SessionKey removed ~s ~n", [SessionKey])
    end.
