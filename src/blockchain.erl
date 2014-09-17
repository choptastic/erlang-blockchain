-module(blockchain).
-export([
	get_address/1,
	format_amount/1
]).

get_address(Address) ->
	URL = "https://blockchain.info/address/" ++ Address ++ "?format=json&filter=2",
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
	Addresses = determine_input_addresses(Inputs),
	Value = determine_final_value(WatchingAddress, Outs),
	{Hash, Time, Value, Addresses}.
	
determine_input_addresses(Ins) ->
	lists:foldl(fun({struct, In}, Addresses) ->
		{struct, PrevOut} = proplists:get_value(<<"prev_out">>, In),
		Address = proplists:get_value(<<"addr">>, PrevOut),
		case lists:member(Address, Addresses) of
			true -> Addresses;
			false -> [Address | Addresses]
		end
	end, [], Ins).

determine_final_value(WatchingAddress, Outs) ->
	lists:foldl(fun({struct, Out}, Total) ->
		ToAddress = proplists:get_value(<<"addr">>, Out),
		case ToAddress == WatchingAddress of
			true -> Total + proplists:get_value(<<"value">>, Out);
			false -> Total
		end
	end, 0, Outs).

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
	string:strip(string:strip(BTC ++ "." ++ Frac,right,$0),right, $.).
	
