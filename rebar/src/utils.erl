-module(utils).

-export([remove_right_zeros/1, generate_message/2]).

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
  SenderPadded = pad_bin(Sender, 20),
  <<SenderPadded/binary, Payload/binary>>.
