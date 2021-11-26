-module(rt_client_session).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, Req, Opts) ->
	{_, Req1} = cowboy_req:meta(websocket_version, Req, 13),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req1),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
	{upgrade, protocol, cowboy_websocket, Req3, Opts}.

websocket_init(_TransportName, Req, _Opts) ->
    case cowboy_req:qs_val(list_to_binary("session"), Req) of
        { undefined , _ } ->
            {shutdown, Req};
        {SessionKey, _ } ->
            SessionServerPid = server_session_manager:get_session_pid(SessionKey),
            case cowboy_req:qs_val(list_to_binary("client"), Req) of
								{ undefined, _ } ->
									 {shutdown, Req};
								{ClientKey, _ } ->
									  ClientKeyHasSeparator = (string:chr(binary_to_list(ClientKey), $#) /= 0),
										if
												ClientKeyHasSeparator ->
														io:format("ClientKey cannot have '#'", []),
														{shutdown, Req};
												true ->
													quickrand:seed(),
													RegisterReply = server_session:register(SessionServerPid, ClientKey, self(), rt_session),
													if
															(RegisterReply /= client_key_duplicated) ->
																  SecretConfigKey = element(2,RegisterReply),
																	server_session:set_secret_config_key(SessionServerPid, ClientKey, SecretConfigKey),
																  io:format("Secret key used by config session ~s ~n", [SecretConfigKey]),
																	self() ! {postinit, SecretConfigKey},
																	{ok, Req, { SessionServerPid , ClientKey} };
															true ->
							            				{shutdown, Req}
													end
										end
						end
    end.

websocket_handle({binary, Msg}, Req, {SessionServerPid, ClientKey}) ->
		JsonMsg = jsone:decode(Msg),
		MsgData = maps:get(<<"data">>, JsonMsg),
		MsgReceiver = maps:get(<<"receiver">>, JsonMsg),
		io:format("RealTime message ~s received from ~s to ~s ~n", [binary_to_list(MsgData), binary_to_list(ClientKey), binary_to_list(MsgReceiver)]),
		if
			MsgReceiver ==  <<"all">>->
				server_session:broadcast_rt_message(SessionServerPid, ClientKey, MsgData);
			true ->
				server_session:direct_rt_message(SessionServerPid, ClientKey, MsgReceiver, MsgData)
		end,
		{ok, Req,  { SessionServerPid , ClientKey} }.

websocket_info({postinit, SecretConfigKey}, Req, {SessionServerPid, ClientKey}) ->
		server_session:broadcast_config_message(SessionServerPid, binary_to_list(ClientKey) ++ " IN"),
		{reply, {binary, SecretConfigKey}, Req,  { SessionServerPid , ClientKey} };
websocket_info({message, Msg}, Req, {SessionServerPid, ClientKey}) ->
		{reply, {binary, Msg}, Req,  { SessionServerPid , ClientKey} };
websocket_info({stop}, Req, {SessionServerPid, ClientKey}) ->
		{shutdown, Req,  { SessionServerPid , ClientKey} }.

websocket_terminate(_Reason, _Req, {SessionServerPid, ClientKey}) ->
    server_session:unregister(SessionServerPid, ClientKey, self()),
		server_session:broadcast_config_message(SessionServerPid, binary_to_list(ClientKey) ++ " OUT").
