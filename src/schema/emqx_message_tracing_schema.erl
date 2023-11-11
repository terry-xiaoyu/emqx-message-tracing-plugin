-module(emqx_message_tracing_schema).

-include("emqx_message_tracing.hrl").
-include_lib("hocon/include/hoconsc.hrl").

-export([
    roots/0,
    fields/1,
    namespace/0,
    desc/1
]).

namespace() -> ?CONFIG_ROOT.
roots() -> [?CONFIG_ROOT].

fields(?CONFIG_ROOT) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})},
        {payload_compression, ?HOCON(?R_REF(payload_compression))},
        {client_connect, ?HOCON(?R_REF(client_connect))},
        {client_connack, ?HOCON(?R_REF(client_connack))},
        {client_connected, ?HOCON(?R_REF(client_connected))},
        {client_disconnected, ?HOCON(?R_REF(client_disconnected))},
        {session_subscribed, ?HOCON(?R_REF(session_subscribed))},
        {session_unsubscribed, ?HOCON(?R_REF(session_unsubscribed))},
        {session_discarded, ?HOCON(?R_REF(session_discarded))},
        {session_takenover, ?HOCON(?R_REF(session_takenover))},
        {session_terminated, ?HOCON(?R_REF(session_terminated))},
        {message_received, ?HOCON(?R_REF(message_received))},
        {message_acked_by_emqx, ?HOCON(?R_REF(message_acked_by_emqx))},
        {message_sent, ?HOCON(?R_REF(message_sent))},
        {message_acked_by_client, ?HOCON(?R_REF(message_acked_by_client))},
        {message_dropped, ?HOCON(?R_REF(message_dropped))},
        {action_taken, ?HOCON(?R_REF(action_taken))},
        {action_complete, ?HOCON(?R_REF(action_complete))},
        {file_handler, ?HOCON(?R_REF(file_handler))}
    ];
fields(payload_compression) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(client_connect) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(client_connack) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(client_connected) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(client_disconnected) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(session_subscribed) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(session_unsubscribed) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(session_discarded) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(session_takenover) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(session_terminated) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(message_received) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(message_acked_by_emqx) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(message_sent) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(message_acked_by_client) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(message_dropped) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})},
        {notify_no_subscriber_drop, ?HOCON(boolean(), #{default => false})}
    ];
fields(action_taken) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(action_complete) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})}
    ];
fields(file_handler) ->
    [
        {enable, ?HOCON(boolean(), #{default => true})},
        {filename, ?HOCON(string(), #{default => "message_tracing.log"})},
        {max_no_bytes, ?HOCON(emqx_schema:bytesize(), #{default => <<"100MB">>})},
        {max_no_files, ?HOCON(integer(), #{default => 200})},
        {compress_on_rotate, ?HOCON(boolean(), #{default => true})}
    ].

desc(_) ->
    undefined.
