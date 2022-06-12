-module(protocol_v2).

% API
-export([handle_message/5]).

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
handle_message(UnknownCommand, RawMessage, ClientKey, _Receiver, _ClientRTTable) ->
  io:format("Unknown command [~p] in protocol version 2 RealTime message [~p], received from ~p NOT SUPPORTED ~n", [UnknownCommand, RawMessage, ClientKey]).
