-module(rt_client_session).
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
    ListQS = cowboy_req:parse_qs(Req),
	{cowboy_websocket, Req2, ListQS, Opts}.

websocket_init(ListQS) ->
	case lists:keyfind(<<"session">>, 1, ListQS) of
        { _, undefined } ->
            {close};
        { _, SessionKey } ->
            SessionServerPid = server_session_manager:get_session_pid(SessionKey),
            case lists:keyfind(<<"client">>, 1, ListQS) of
				{_, undefined } ->
						{close};
				{_, ClientKey } ->						
					quickrand:seed(),
					RegisterReply = server_session:register(SessionServerPid, ClientKey, self(), rt_session),
					if
						(RegisterReply /= client_key_duplicated) ->
							SecretClientKey = element(2,RegisterReply),
							server_session:set_secret_config_key(SessionServerPid, ClientKey, SecretClientKey),
							io:format("Secret key used by RT session [SecretClientKey:~s] [ClientPid:~w] [ClientKey:~s] ~n", [SecretClientKey, self(), ClientKey]),
							self() ! {postinit, SecretClientKey},
							{[], { SessionServerPid , ClientKey }};
						true ->
							{close}
					end
			end
    end.

websocket_handle({binary, Msg}, {SessionServerPid, ClientKey}) ->
    server_session:handle_rt_message(SessionServerPid, ClientKey, Msg),
	{[], { SessionServerPid , ClientKey} }.

websocket_info({postinit, SecretClientKey}, {SessionServerPid, ClientKey}) ->
	Message = utils:generate_message(ClientKey, list_to_binary(SecretClientKey)),
	{[{binary, Message}], { SessionServerPid , ClientKey} };
websocket_info({message, Msg}, {SessionServerPid, ClientKey}) ->
    {[{binary, Msg}],  { SessionServerPid , ClientKey} };
websocket_info({stop}, {SessionServerPid, ClientKey}) ->
	{stop, { SessionServerPid , ClientKey} }.

terminate(Reason, _Req, {SessionServerPid, ClientKey}) ->
	io:format("Websocket closed, reason [~p] \n", [Reason]),
	server_session:unregister(SessionServerPid, ClientKey, self()),
	ok.