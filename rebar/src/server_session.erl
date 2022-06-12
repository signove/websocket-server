-module(server_session).
-behaviour(gen_server).

% gen server callback.
-export([init/1, handle_call/3, terminate/2]).

% API
-export([register/4, unregister/3]).
-export([set_secret_config_key/3]).
-export([handle_rt_message/3, handle_broadcast_rt_message/3]).

init(_Args) ->
    % Two tables, one for RT communication and other to Configuration
    ClientRTTable = ets:new(rt_clients, []),
    ClientSecretKeyTable = ets:new(client_secret_keys, []),
    {ok, { ClientRTTable, ClientSecretKeyTable } }.

register(ServerSessionPid, ClientKey, ClientPid, SessionType) ->
  gen_server:call(ServerSessionPid, { register, SessionType, ClientPid, ClientKey, ServerSessionPid }).

unregister(ServerSessionPid, ClientKey, ClientPid) ->
  gen_server:call(ServerSessionPid, { unregister, ClientPid, ClientKey, ServerSessionPid }).

set_secret_config_key(ServerSessionPid, ClientKey, SecretKey) ->
  gen_server:call(ServerSessionPid, { set_secret_config_key , ClientKey, SecretKey}).

handle_rt_message(ServerSessionPid, ClientKey, Message) ->
  <<Version:2/binary-unit:8, Receiver:20/binary-unit:8, Payload/binary>> = Message,
  gen_server:call(ServerSessionPid, { handle_rt_message, ServerSessionPid, ClientKey, Version, Receiver, Payload }).

handle_call({ register, rt_session, ClientRTPid, ClientKey, ServerSessionPid }, _From, { ClientRTTable, ClientSecretKeyTable }) ->
    HasClientKey = ets:member(ClientRTTable, ClientKey),
    if
      HasClientKey ->
          io:format("Client key already in use ~s ~n",[ClientKey]),
          {reply, client_key_duplicated, { ClientRTTable, ClientSecretKeyTable } };
      true ->
          io:format("RealTime session created to [ClientKey:~s] in session [ServerSessionPid:~w] ~n",[ClientKey, ServerSessionPid]),
          ets:insert(ClientRTTable, { ClientKey, ClientRTPid }),
          { Uuid, _ } = uuid:get_v1(uuid:new(ClientRTPid)),
          SecretKey = uuid:uuid_to_string(Uuid),
          {reply, {ok, SecretKey}, { ClientRTTable, ClientSecretKeyTable } }
    end;
handle_call({ unregister, ClientRTPid, ClientKey, ServerSessionPid }, _From, { ClientRTTable, ClientSecretKeyTable }) ->
    ets:delete(ClientRTTable, ClientKey),
    io:format("Client ~w unregistered, key ~s ~n",[ClientRTPid,  binary_to_list(ClientKey)]),
    case ets:info(ClientRTTable, size) of
        0 ->
            io:format("No more clients left ~n",[]),
            {stop, no_more_clients, ok, {ServerSessionPid} };
        _ ->
            {reply, ok, { ClientRTTable, ClientSecretKeyTable } }
    end;
handle_call({ set_secret_config_key, ClientKey, SecretKey }, _From, {ClientRTTable, ClientSecretKeyTable}) ->
    ets:insert(ClientSecretKeyTable, { ClientKey, SecretKey }),
    {reply, ok, { ClientRTTable, ClientSecretKeyTable } };
% Handle protocol version 1
handle_call({ handle_rt_message, _ServerSessionPid, ClientKey, <<0,1>>, ReceiverBin, Payload}, _From, {ClientRTTable, ClientSecretKeyTable}) ->
    protocol_v1:handle_message(Payload, ClientKey, ReceiverBin, ClientRTTable),
    {reply, ok, {ClientRTTable, ClientSecretKeyTable} };
% Handle protocol version 2
handle_call({ handle_rt_message, _ServerSessionPid, ClientKey, <<0,2>>, ReceiverBin, Payload}, _From, {ClientRTTable, ClientSecretKeyTable}) ->
    Receiver = utils:remove_right_zeros(ReceiverBin),
    io:format("Protocol version [2] - RealTime message [~p], received from ~p and sending to ~p ~n", [Payload, ClientKey, Receiver]),
    <<Command:1/binary-unit:8, Message/binary>> = Payload,
    protocol_v2:handle_message(Command, Message, ClientKey, Receiver, ClientRTTable),
    {reply, ok, {ClientRTTable, ClientSecretKeyTable} };
handle_call({ handle_rt_message, _ServerSessionPid, ClientKey, UnsupportedVersion, _ReceiverBin, Payload}, _From, {ClientRTTable, ClientSecretKeyTable}) ->
    io:format("Protocol version [~p] - RealTime message [~p], received from ~p NOT SUPPORTED ~n", [UnsupportedVersion, Payload, ClientKey]),
    {reply, ok, {ClientRTTable, ClientSecretKeyTable} }.

terminate( _ , {ServerSessionPid}) ->
    server_session_manager:delete_session_by_pid(ServerSessionPid).

handle_broadcast_rt_message(_, '$end_of_table', _) -> ok;
handle_broadcast_rt_message(ClientRTTable, ClientKey , Message) ->
    [{_, ClientPid }|_] = ets:lookup(ClientRTTable, ClientKey),
    ClientPid ! {message, Message},
    handle_broadcast_rt_message(ClientRTTable, ets:next(ClientRTTable, ClientKey), Message).
