# Erlang Blockchain

A simple API to blockchain.info

Currently only exports two functions (more to come):

  + `get_address(Address)` -> `{CurrentValue, ListOfTransactions}`, where
    `ListOfTransactions` is a 4-tuple: `{Hash, Time, Value, ListOfAddresses}`.
    `Time` is an integer representing the unix timestamp of the transaction, Value
    is the integer value in Satoshis, and `ListOfAddresses` is a simple list of the
    addresses as binary strings.

  + `format_amount(Satoshis)` - Converts an integer of Satoshis into a
    formatted string of the format "2.165 BTC".

# License

Author: Jesse Gumm (@jessegumm)

MIT License
