-module(protocol_v2).

% API
-export([start/0, handle_message/5]).

start() ->
  ets:new(protocol_v2_memory, [named_table, public]).

% Command 0 - RT default messages sent to another clients
handle_message(<<0>>, RawMessage, ClientKey, Receiver, ClientRTTable) ->
  if
    Receiver ==  <<"">>->
      Message = utils:generate_message(ClientKey, RawMessage),
      server_session:handle_broadcast_rt_message(ClientRTTable, ets:first(ClientRTTable), Message);
    true ->
      Message = utils:generate_message(ClientKey, RawMessage),
      IsValidReceiver = ets:member(ClientRTTable, Receiver),
      if
        IsValidReceiver ->
          [{_, ReceiverClientPid, _}|_] = ets:lookup(ClientRTTable, Receiver),
          ReceiverClientPid ! {message, Message};
        true ->
          io:format("invalid receiver ~s ~n", [Receiver])
      end
  end;
% Command 1 - Write data in internal memory for this client
handle_message(<<1>>, RawMessage, ClientKey, _Receiver, _ClientRTTable) ->
  ets:insert(protocol_v2_memory, {ClientKey, RawMessage}),
  io:format("Command 1");
% Command 2 - Read data from internal memory
handle_message(<<2>>, _RawMessage, _ClientKey, _Receiver, _ClientRTTable) ->
  Commands = get_command(ets:first(protocol_v2_memory)),
  io:format("Command 2 [~p]", [Commands]);
handle_message(UnknownCommand, RawMessage, ClientKey, _Receiver, _ClientRTTable) ->
  io:format("Unknown command [~p] in protocol version 2 RealTime message [~p], received from ~p NOT SUPPORTED ~n", [UnknownCommand, RawMessage, ClientKey]).

get_command(ClientKey) ->
  if
    ClientKey == '$end_of_table'->
      none;
    true ->
      Content = ets:lookup(protocol_v2_memory, ClientKey),
      {[Content|get_command(ets:next(protocol_v2_memory, ClientKey))]}
  end.