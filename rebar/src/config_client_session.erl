-module(config_client_session).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, _) ->
	Opts = #{idle_timeout => infinity},
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
	ListQS = cowboy_req:parse_qs(Req2),
	{cowboy_websocket, Req2, ListQS, Opts}.


websocket_init(ListQS) ->
		case lists:keyfind(<<"session">>, 1, ListQS) of
        { _, undefined } ->
			{close};
        { _, SessionKey } ->
            SessionServerPid = server_session_manager:get_session_pid(SessionKey),
            case lists:keyfind(<<"client">>, 1, ListQS) of
				{ _, undefined } ->
					{close};
				{ _, ClientKey } ->
					ClientKeyHasSeparator = (string:chr(binary_to_list(ClientKey), $#) /= 0),
					case lists:keyfind(<<"secret_config_key">>, 1, ListQS) of
						{ _, undefined } ->
							{close};
						{ _, SecretKey } ->
							if
								ClientKeyHasSeparator ->
										io:format("ClientKey cannot have '#'", []),
										{close};
								true ->
									ClientPid = self(),
									Registered = (server_session:register(SessionServerPid, ClientKey, ClientPid, { config_session, SecretKey }) /= rt_session_unavailable),
									if
										Registered ->
										    io:format("Client has been registered: [SessionServerPid:~p] [ClientPid:~p] [ClientKey:~s] ~n", [SessionServerPid, ClientPid, ClientKey]),
											{[], { SessionServerPid , ClientKey}};
										true ->
											{close}
									end
							end
					end
			end	
    end.

websocket_handle({binary, Msg}, {SessionServerPid, ClientKey}) ->
	server_session:handle_config_message(SessionServerPid, ClientKey, Msg),
	{[], { SessionServerPid , ClientKey} }.

websocket_info({message, Msg}, {SessionServerPid, ClientKey}) ->
	{[{binary, Msg}],  { SessionServerPid , ClientKey} };
websocket_info({stop}, {SessionServerPid, ClientKey}) ->
	{stop, { SessionServerPid , ClientKey} }.

terminate(_Reason, _Req, {_SessionServerPid, ClientKey}) ->
	io:format("Config client ~s terminated ~n",[binary_to_list(ClientKey)]),
	ok.
