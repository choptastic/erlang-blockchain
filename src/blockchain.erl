-module(blockchain).
-export([
	get_address/1,
	format_amount/1
]).

get_address(Address) ->
	URL = "https://blockchain.info/address/" ++ Address ++ "?format=json",
	{ok, _, _, Json} = ibrowse:send_req(URL, [], get),
	Struct = wf:json_decode(Json),
	Worth = proplists:get_value(<<"final_balance">>, Struct),
	Transactions = proplists:get_value(<<"txs">>, Struct),
	FinalTransactions = [format_transaction(iolist_to_binary(Address), T) || T <- Transactions],
	{Worth, FinalTransactions}.


format_transaction(WatchingAddress, {struct, T}) ->
	Time = proplists:get_value(<<"time">>, T),
	Hash = proplists:get_value(<<"hash">>, T),
	Inputs = proplists:get_value(<<"inputs">>, T),
	Outs = proplists:get_value(<<"out">>, T),
	InputAddresses = determine_input_addresses(Inputs),
	OutputAddresses = determine_output_addresses(Outs),
	Action = is_sent_or_received(WatchingAddress, InputAddresses),
	Value = determine_final_value(Action, WatchingAddress, Inputs, Outs),
	Addresses = case Action of
		sent -> OutputAddresses;
		received -> InputAddresses
	end,
	{Hash, Time, Value, Action, Addresses}.

is_sent_or_received(WatchingAddress, InputAddresses) ->
	case lists:member(WatchingAddress, InputAddresses) of
		true -> sent;
		false -> received
	end.

determine_input_addresses(Ins) ->
	lists:foldl(fun({struct, In}, Addresses) ->
		{struct, PrevOut} = proplists:get_value(<<"prev_out">>, In),
		add_tx_address(PrevOut, Addresses)
	end, [], Ins).

determine_output_addresses(Outs) ->
	lists:foldl(fun({struct, Out}, Addresses) ->
		add_tx_address(Out, Addresses)
	end, [], Outs).

add_tx_address(Tx, Addresses) ->
	Address = proplists:get_value(<<"addr">>, Tx),
	case lists:member(Address, Addresses) of
		true -> Addresses;
		false -> [Address | Addresses]
	end.

determine_final_value(received, WatchingAddress, _Ins, Outs) ->
	lists:foldl(fun({struct, Out}, Total) ->
		Total + add_value_for_out(Out, WatchingAddress)
	end, 0, Outs);
determine_final_value(sent, WatchingAddress, Ins, Outs) ->
	TempTotals = lists:foldl(fun({struct, In}, Total) ->
		{struct, PrevOut} = proplists:get_value(<<"prev_out">>, In),
		Total + add_value_for_out(PrevOut, WatchingAddress)
	end, 0, Ins),
	Refunded = determine_final_value(received, WatchingAddress, Ins, Outs),
	TempTotals - Refunded.

add_value_for_out(Out, WatchingAddress) ->
	ToAddress = proplists:get_value(<<"addr">>, Out),
	case ToAddress == WatchingAddress of
		true -> proplists:get_value(<<"value">>, Out);
		false -> 0
	end.

format_amount(Satoshis) when is_integer(Satoshis) ->
	Str = integer_to_list(Satoshis),
	format_amount(Str);
format_amount(Satoshis) when length(Satoshis) < 9 ->
	Padding = 9 - length(Satoshis),
	Padded = lists:duplicate(Padding, $0) ++ Satoshis,
	format_amount(Padded);
format_amount(Satoshis) ->
	SplitOn = length(Satoshis) - 8,
	{BTC, Frac} = lists:split(SplitOn, Satoshis),
	string:strip(string:strip(BTC ++ "." ++ Frac,right,$0),right, $.) ++ " BTC".
	
