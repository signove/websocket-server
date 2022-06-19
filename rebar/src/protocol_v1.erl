-module(protocol_v1).

% API
-export([handle_message/4]).

handle_message(Payload, ClientKey, ReceiverBin, ClientRTTable) ->
  MessageWithoutVersion = utils:generate_message(ClientKey, Payload),
  Message = << <<0,1>>/binary, MessageWithoutVersion/binary >>,
  Receiver = utils:remove_right_zeros(ReceiverBin),
  io:format("Protocol version [1] - RealTime message [~p], received from ~p and sending to ~p ~n", [Payload, ClientKey, Receiver]),
  if
    Receiver ==  <<"">>->
      server_session:handle_broadcast_message(ClientRTTable, ets:first(ClientRTTable), Message);
    true ->
      Message = utils:generate_message(ClientKey, Payload),
      IsValidReceiver = ets:member(ClientRTTable, Receiver),
      if
        IsValidReceiver ->
          [{_, ReceiverClientPid, _}|_] = ets:lookup(ClientRTTable, Receiver),
          ReceiverClientPid ! {message, Message};
        true ->
          io:format("invalid receiver ~s ~n", [Receiver])
      end
  end.