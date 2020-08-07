-module(server_session).
-behaviour(gen_server).

% gen server callback.
-export([init/1, handle_call/3, terminate/2]).

% API
-export([register/4, unregister/3]).
-export([broadcast_rt_message/3, direct_rt_message/4, broadcast_config_message/2, set_secret_config_key/3]).
-export([handle_config_message/3, handle_broadcast_rt_message/3, handle_broadcast_config_message/3]).

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

broadcast_rt_message(ServerSessionPid, ClientKey, Message) ->
    gen_server:call(ServerSessionPid, { broadcast_rt_message, ClientKey, Message }).

direct_rt_message(ServerSessionPid, ClientKey, ReceiverKey, Message) ->
    gen_server:call(ServerSessionPid, { direct_rt_message, ClientKey, ReceiverKey, Message }).

set_secret_config_key(ServerSessionPid, ClientKey, SecretKey) ->
    gen_server:call(ServerSessionPid, { set_secret_config_key , ClientKey, SecretKey}).

broadcast_config_message(ServerSessionPid, Message) ->
    gen_server:call(ServerSessionPid, { broadcast_config_message , Message }).

handle_config_message(ServerSessionPid, ClientKey, Message) ->
    gen_server:call(ServerSessionPid, { handle_config_message , ServerSessionPid, ClientKey, Message }).

handle_call({ register, rt_session, ClientRTPid, ClientKey, ServerSessionPid }, _From, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable }) ->
    HasClientKey = ets:member(ClientRTTable, ClientKey),
    if
      HasClientKey ->
          io:format("Client key already in use ~s ~n",[ClientKey]),
          {reply, client_key_duplicated, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } };
      true ->
          io:format("RealTime session created to ~s in session ~w ~n",[ClientKey, ServerSessionPid]),
          ets:insert(ClientRTTable, { ClientKey, ClientRTPid, [receiver_on, sender_on] }),
          { Uuid, _ } = uuid:get_v1(uuid:new(ClientRTPid)),
          SecretKey = uuid:uuid_to_string(Uuid),
          {reply, {ok, SecretKey}, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } }
    end;
handle_call({ register, { config_session, SentSecretKey} , ClientConfigPid, ClientKey, ServerSessionPid }, _From, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable }) ->
    HasClientKey = ets:member(ClientRTTable, ClientKey),
    ValidSecretKey = ets:lookup_element(ClientConfigKeyTable, ClientKey, 2),
    io:format("Valid secret key is ~s, used secret key is ~s ~n", [ValidSecretKey, SentSecretKey]),
    if
      HasClientKey ->
          %{ _ , ClientRTPid } = ets:lookup_element(ClientRTTable, ClientKey, 1),
          io:format("Config session created to ~s in session ~w ~n",[ClientKey, ServerSessionPid]),
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
handle_call({ broadcast_rt_message, ClientKey , Data }, _From, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable }) ->
    ClientOptions = ets:lookup_element(ClientRTTable, ClientKey, 3),
    CanSend = lists:member(sender_on , ClientOptions),
    if
      CanSend ->
        NewData = <<ClientKey/binary, <<"#">>/binary , Data/binary>>,
        handle_broadcast_rt_message(ClientRTTable, ets:first(ClientRTTable), NewData);
      true ->
        io:format("send disabled for ~s ~n", [ClientKey])
    end,
    {reply, ok, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } };
handle_call({ direct_rt_message, ClientKey, ReceiverKey, Data }, _From, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable }) ->
    NewData = <<ClientKey/binary, <<"#">>/binary , Data/binary>>,
    IsValidReceiver = ets:member(ClientRTTable, ReceiverKey),
    if
      IsValidReceiver ->
        [{_, ReceiverClientPid, _}|_] = ets:lookup(ClientRTTable, ReceiverKey),
        ReceiverClientPid ! {message, NewData};
      true ->
        io:format("invalid receiver ~s ~n", [ReceiverKey])
    end,
    {reply, ok, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } };
handle_call({ set_secret_config_key, ClientKey, SecretKey }, _From, {ClientRTTable, ClientConfigTable, ClientConfigKeyTable}) ->
    ets:insert(ClientConfigKeyTable, { ClientKey, SecretKey }),
    {reply, ok, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } };
handle_call({ broadcast_config_message , Message }, _From, {ClientRTTable, ClientConfigTable, ClientConfigKeyTable}) ->
    handle_broadcast_config_message(ClientConfigTable, ets:first(ClientConfigTable), Message),
    {reply, ok, { ClientRTTable, ClientConfigTable, ClientConfigKeyTable } };
handle_call({ handle_config_message, _ServerSessionPid, ClientKey, Message }, _From, {ClientRTTable, ClientConfigTable, ClientConfigKeyTable}) ->
    io:format("Configuration message ~s received from ~s ~n", [Message, binary_to_list(ClientKey)]),
    ClientOptions = ets:lookup_element(ClientRTTable, ClientKey, 3),
    case Message of
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
        ClientPid ! { message, lists:concat(items_join("|", ClientOptions)) };
      Msg ->
        handle_broadcast_config_message(ClientConfigTable, ets:first(ClientConfigTable), Msg)
    end,
    {reply, ok, {ClientRTTable, ClientConfigTable, ClientConfigKeyTable} }.

items_join(Sep, List) ->
    [H|T] = List,
    items_join(Sep, T, [H, Sep]).

items_join(_, [H|[]], FinalList) ->
    lists:append(FinalList, [H]);
items_join(Sep, [H|T], FinalList) ->
    items_join(Sep, T, lists:append(FinalList,[H, Sep])).



terminate( _ , {ServerSessionPid}) ->
    server_session_manager:delete_session_by_pid(ServerSessionPid).

handle_broadcast_config_message(_, '$end_of_table', _) -> ok;
handle_broadcast_config_message(ClientConfigTable, ClientKey , Message) ->
    [{_, ClientPid}|_] = ets:lookup(ClientConfigTable, ClientKey),
    ClientPid ! {message, Message},
    handle_broadcast_config_message(ClientConfigTable, ets:next(ClientConfigTable, ClientKey), Message).

handle_broadcast_rt_message(_, '$end_of_table', _) -> ok;
handle_broadcast_rt_message(ClientRTTable, ClientKey , Message) ->
    [{_, ClientPid, ClientOptions}|_] = ets:lookup(ClientRTTable, ClientKey),
    io:format("Options: ~w ~n", [ClientOptions]),
    CanReceive = lists:member(receiver_on , ClientOptions),
    if
      CanReceive ->
        ClientPid ! {message, Message};
      true ->
        io:format("receive disabled for ~s ~n", [ClientKey])
    end,
    handle_broadcast_rt_message(ClientRTTable, ets:next(ClientRTTable, ClientKey), Message).
