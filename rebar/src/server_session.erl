-module(server_session).
-behaviour(gen_server).

% gen server callback.
-export([init/1, handle_call/3, terminate/2]).

% API
-export([register/4, unregister/3]).
-export([broadcast_config_message/3, set_secret_config_key/3, generate_message/2]).
-export([handle_config_message/3, handle_rt_message/3, handle_broadcast_rt_message/3, handle_broadcast_config_message/3]).

init(_Args) ->
    % Two tables, one for RT communication and other to Configuration
    ClientRTTable = ets:new(rt_clients, []),
    ClientConfigTable = ets:new(config_clients, []),
    ClientConfigKeyTable = ets:new(config_keys, []),
    {ok, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } }.

register(ServerSessionPid, ClientKey, ClientPid, SessionType) ->
  gen_server:call(ServerSessionPid, { register, SessionType, ClientPid, ClientKey, ServerSessionPid }).

unregister(ServerSessionPid, ClientKey, ClientPid) ->
  gen_server:call(ServerSessionPid, { unregister, ClientPid, ClientKey, ServerSessionPid }).

set_secret_config_key(ServerSessionPid, ClientKey, SecretKey) ->
  gen_server:call(ServerSessionPid, { set_secret_config_key , ClientKey, SecretKey}).

broadcast_config_message(ServerSessionPid, ClientKey, Message) ->
  gen_server:call(ServerSessionPid, { broadcast_config_message , ClientKey, Message }).

handle_config_message(ServerSessionPid, ClientKey, Message) ->
  <<Version:2/binary-unit:8, _Receiver:20/binary-unit:8, Payload/binary>> = Message,
  gen_server:call(ServerSessionPid, { handle_config_message , ServerSessionPid, ClientKey, Version, Payload }).

handle_rt_message(ServerSessionPid, ClientKey, Message) ->
  <<Version:2/binary-unit:8, Receiver:20/binary-unit:8, Payload/binary>> = Message,
  gen_server:call(ServerSessionPid, { handle_rt_message, ServerSessionPid, ClientKey, Version, Receiver, Payload }).

handle_call({ register, rt_session, ClientRTPid, ClientKey, ServerSessionPid }, _From, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable }) ->
    HasClientKey = ets:member(ClientRTTable, ClientKey),
    if
      HasClientKey ->
          io:format("Client key already in use ~s ~n",[ClientKey]),
          {reply, client_key_duplicated, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } };
      true ->
          io:format("RealTime session created to [ClientKey:~s] in session [ServerSessionPid:~w] ~n",[ClientKey, ServerSessionPid]),
          ets:insert(ClientRTTable, { ClientKey, ClientRTPid, [receiver_on, sender_on] }),
          { Uuid, _ } = uuid:get_v1(uuid:new(ClientRTPid)),
          SecretKey = uuid:uuid_to_string(Uuid),
          {reply, {ok, SecretKey}, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } }
    end;
handle_call({ register, { config_session, SentSecretKey} , ClientConfigPid, ClientKey, ServerSessionPid }, _From, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable }) ->
    HasClientKey = ets:member(ClientRTTable, ClientKey),
    ValidSecretKey = ets:lookup_element(ClientConfigKeyTable, ClientKey, 2),
    io:format("Secret keys [ValidSecretKey:~s] [SentSecretKey:~s] ~n", [ValidSecretKey, SentSecretKey]),
    if
      HasClientKey ->
          %{ _ , ClientRTPid } = ets:lookup_element(ClientRTTable, ClientKey, 1),
          io:format("Config session created [ClientKey:~s] [ServerSessionPid:~w] ~n",[ClientKey, ServerSessionPid]),
          ets:insert(ClientConfigTable, { ClientKey, ClientConfigPid }),
          {reply, ok, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } };
      true ->
          io:format("RT session unavailable to ~s ~n", [ClientKey]),
          {reply, rt_session_unavailable, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } }
    end;
handle_call({ unregister, ClientRTPid, ClientKey, ServerSessionPid }, _From, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable }) ->
    ets:delete(ClientRTTable, ClientKey),
    HasConfigClientKey = ets:member(ClientConfigTable, ClientKey),
    if
      HasConfigClientKey ->
        ClientConfigPid = ets:lookup_element(ClientConfigTable, ClientKey, 2),
        ClientConfigPid ! { stop },
        ets:delete(ClientConfigTable, ClientKey),
        io:format("Config client ~w unregistered, key ~s ~n",[ClientConfigPid,  binary_to_list(ClientKey)]);
      true ->
        io:format("There is no config client to be unregistered, key ~s ~n",[binary_to_list(ClientKey)])
    end,
    io:format("Client ~w unregistered, key ~s ~n",[ClientRTPid,  binary_to_list(ClientKey)]),
    case ets:info(ClientRTTable, size) of
        0 ->
            io:format("No more clients left ~n",[]),
            {stop, no_more_clients, ok, {ServerSessionPid} };
        _ ->
            {reply, ok, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } }
    end;
handle_call({ set_secret_config_key, ClientKey, SecretKey }, _From, {ClientRTTable, ClientConfigTable, ClientConfigKeyTable}) ->
    ets:insert(ClientConfigKeyTable, { ClientKey, SecretKey }),
    {reply, ok, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } };
handle_call({ broadcast_config_message , ClientKey, Payload }, _From, {ClientRTTable, ClientConfigTable, ClientConfigKeyTable}) ->
    Message = generate_message(ClientKey, Payload),
    handle_broadcast_config_message(ClientConfigTable, ets:first(ClientConfigTable), Message),
    {reply, ok, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } };
handle_call({ handle_rt_message, _ServerSessionPid, ClientKey, Version, ReceiverBin, Payload}, _From, {ClientRTTable, ClientConfigTable, ClientConfigKeyTable}) ->
    Receiver = remove_right_zeros(ReceiverBin),
    io:format("Protocol version [~p] - RealTime message [~p], received from ~p and sending to ~p ~n", [Version, Payload, ClientKey, Receiver]),
    if
      Receiver ==  <<"">>->
        ClientOptions = ets:lookup_element(ClientRTTable, ClientKey, 3),
        CanSend = lists:member(sender_on , ClientOptions),
        if
          CanSend ->
            Message = generate_message(ClientKey, Payload),
            handle_broadcast_rt_message(ClientRTTable, ets:first(ClientRTTable), Message);
          true ->
            io:format("send disabled for ~s ~n", [ClientKey])
        end;
      true ->
        Message = generate_message(ClientKey, Payload),
        IsValidReceiver = ets:member(ClientRTTable, Receiver),
        if
          IsValidReceiver ->
            [{_, ReceiverClientPid, _}|_] = ets:lookup(ClientRTTable, Receiver),
            ReceiverClientPid ! {message, Message};
          true ->
            io:format("invalid receiver ~s ~n", [Receiver])
        end
    end,
    {reply, ok, {ClientRTTable, ClientConfigTable, ClientConfigKeyTable} };
handle_call({ handle_config_message, _ServerSessionPid, ClientKey, Version, Payload }, _From, {ClientRTTable, ClientConfigTable, ClientConfigKeyTable}) ->
    io:format("Protocol version [~p] - Configuration message [~p], received from ~p ~n", [Version, Payload, ClientKey]),
    ClientOptions = ets:lookup_element(ClientRTTable, ClientKey, 3),
    case Payload of
      <<"SENDER_ON">> ->
        HasSenderOn = lists:member(sender_on, ClientOptions),
        if
          HasSenderOn == false ->
            NewList = [sender_on|lists:delete(sender_off, ClientOptions)],
            ets:update_element(ClientRTTable, ClientKey, { 3, NewList} );
          true ->
            ok
        end;
      <<"SENDER_OFF">> ->
        HasSenderOff = lists:member(sender_off, ClientOptions),
        if
          HasSenderOff == false ->
            NewList = [sender_off|lists:delete(sender_on, ClientOptions)],
            ets:update_element(ClientRTTable, ClientKey, { 3, NewList} );
          true ->
            ok
        end;
      <<"RECEIVER_ON">> ->
        HasReceiverOn = lists:member(receiver_on, ClientOptions),
        if
          HasReceiverOn == false ->
            NewList = [receiver_on|lists:delete(receiver_off, ClientOptions)],
            ets:update_element(ClientRTTable, ClientKey, { 3, NewList} );
          true ->
            ok
        end;
      <<"RECEIVER_OFF">> ->
        HasReceiverOff = lists:member(receiver_off, ClientOptions),
        if
          HasReceiverOff == false ->
            NewList = [receiver_off|lists:delete(receiver_on, ClientOptions)],
            ets:update_element(ClientRTTable, ClientKey, { 3, NewList} );
          true ->
            ok
        end;
      <<"GET_OPTIONS">> ->
        [{_, ClientPid}|_] = ets:lookup(ClientConfigTable, ClientKey),
        Msg = list_to_binary(lists:concat(items_join("|", ClientOptions))),
        Message = generate_message(ClientKey, Msg),
        ClientPid ! { message, Message };
      Msg ->
        Message = generate_message(ClientKey, Msg),
        handle_broadcast_config_message(ClientConfigTable, ets:first(ClientConfigTable), Message)
    end,
    {reply, ok, {ClientRTTable, ClientConfigTable, ClientConfigKeyTable} }.

items_join(Sep, List) ->
    [H|T] = List,
    items_join(Sep, T, [H, Sep]).

items_join(_, [H|[]], FinalList) ->
    lists:append(FinalList, [H]);
items_join(Sep, [H|T], FinalList) ->
    items_join(Sep, T, lists:append(FinalList,[H, Sep])).

remove_right_zeros(Bin) ->
  list_to_binary(lists:reverse(lists:dropwhile(fun(0) -> true; (_) -> false end, lists:reverse(binary_to_list(Bin))))).

pad_bin(Bin, FinalSize) ->
  if
    size(Bin) > FinalSize ->
      Bin;
    true ->
      <<Bin/binary, 0:((FinalSize - size(Bin))*8)>>
  end.

generate_message(Sender, Payload) ->
  Version = <<0,1>>,
  SenderPadded = pad_bin(Sender, 20),
  <<Version/binary, SenderPadded/binary, Payload/binary>>.

terminate( _ , {ServerSessionPid}) ->
    server_session_manager:delete_session_by_pid(ServerSessionPid).

handle_broadcast_config_message(_, '$end_of_table', _) -> ok;
handle_broadcast_config_message(ClientConfigTable, ClientKey , Message) ->
    [{_, ClientPid}|_] = ets:lookup(ClientConfigTable, ClientKey),
    io:format("handle_broadcast_config_message: [ClientPid:~p] [Message:~w] ~n", [ClientPid, Message]),
    ClientPid ! {message, Message},
    handle_broadcast_config_message(ClientConfigTable, ets:next(ClientConfigTable, ClientKey), Message).

handle_broadcast_rt_message(_, '$end_of_table', _) -> ok;
handle_broadcast_rt_message(ClientRTTable, ClientKey , Message) ->
    [{_, ClientPid, ClientOptions}|_] = ets:lookup(ClientRTTable, ClientKey),
    io:format("Options: ~w ClientPid: ~p ~n", [ClientOptions, ClientPid]),
    CanReceive = lists:member(receiver_on , ClientOptions),
    if
      CanReceive ->
        io:format("handle_broadcast_rt_message: [ClientPid:~p] [Message:~w] ~n", [ClientPid, Message]),
        ClientPid ! {message, Message};
      true ->
        io:format("receive disabled for ~s ~n", [ClientKey])
    end,
    handle_broadcast_rt_message(ClientRTTable, ets:next(ClientRTTable, ClientKey), Message).
