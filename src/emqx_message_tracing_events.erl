-module(emqx_message_tracing_events).

-include_lib("emqx/include/emqx.hrl").
-include("emqx_message_tracing.hrl").

-export([ ts_now/0
        ]).

-export([ notify_client_connect/2
        , notify_client_connack/3
        , notify_client_connected/2
        , notify_client_disconnected/3
        , notify_session_subscribed/3
        , notify_session_unsubscribed/3
        , notify_session_discarded/2
        , notify_session_takenover/2
        , notify_session_terminated/3
        , notify_message_received/1
        , notify_message_acked_by_emqx/2
        , notify_message_sent/2
        , notify_message_acked_by_client/2
        , notify_message_dropped/2
        , notify_action_taken/1
        , notify_action_complete/2
        ]).

-define(IF_ALLOW_TO_NOTIFY(EVENT, TRUE_EXPR),
    case emqx_message_tracing_plugin:is_enabled() andalso emqx_message_tracing_plugin:is_enabled(EVENT) of
        true -> TRUE_EXPR;
        false -> ok
    end).

-define(GET_CONNEINFO(KEY), maps:get(KEY, ConnInfo, null)).

%% =============================================================================
%% Event Structures
%% =============================================================================
notify_client_connect(
        ConnInfo = #{
            clientid := ClientId,
            username := Username,
            sockname := SockName,
            peername := PeerName
        }, ConnProps) ->
    ?IF_ALLOW_TO_NOTIFY(client_connect,
        emqx_message_tracing_proxy:notify(
            with_basic_fields(client_connect, #{
                clientid => ClientId,
                username => Username,
                peername => ntoa(PeerName),
                sockname => ntoa(SockName),
                proto_name => ?GET_CONNEINFO(proto_name),
                proto_ver => ?GET_CONNEINFO(proto_ver),
                keepalive => ?GET_CONNEINFO(keepalive),
                clean_start => ?GET_CONNEINFO(clean_start),
                receive_maximum => ?GET_CONNEINFO(receive_maximum),
                expiry_interval => ?GET_CONNEINFO(expiry_interval),
                conn_props => ConnProps
            }))).

notify_client_connack(#{clientid := ClientId}, ReasonCode, ConnAckProps) ->
    ?IF_ALLOW_TO_NOTIFY(client_connack,
        emqx_message_tracing_proxy:notify(
            with_basic_fields(client_connack, #{
                clientid => ClientId,
                reason_code => ReasonCode,
                connack_props => ConnAckProps
            }))).

notify_client_connected(#{clientid := ClientId}, ConnInfo) ->
    ?IF_ALLOW_TO_NOTIFY(client_connected,
        emqx_message_tracing_proxy:notify(
            with_basic_fields(client_connected, #{
                clientid => ClientId,
                is_superuser => ?GET_CONNEINFO(is_superuser),
                anonymous => ?GET_CONNEINFO(anonymous),
                cn => ?GET_CONNEINFO(cn)
            }))).

notify_client_disconnected(#{clientid := ClientId}, ConnInfo, Reason) ->
    ?IF_ALLOW_TO_NOTIFY(client_disconnected,
        emqx_message_tracing_proxy:notify(
          with_basic_fields(client_disconnected, #{
                clientid => ClientId,
                reason => disconnect_reason(Reason),
                disconn_props => maps:get(disconn_props, ConnInfo, #{})
            }))).

notify_session_subscribed(#{clientid := ClientId}, TopicFilter, SubProps) ->
    ?IF_ALLOW_TO_NOTIFY(session_subscribed,
        emqx_message_tracing_proxy:notify(with_basic_fields(session_subscribed,
            #{
                clientid => ClientId,
                topic_filter => TopicFilter,
                sub_props => SubProps
            }))).

notify_session_unsubscribed(#{clientid := ClientId}, TopicFilter, UnSubProps) ->
    ?IF_ALLOW_TO_NOTIFY(session_unsubscribed,
        emqx_message_tracing_proxy:notify(with_basic_fields(session_unsubscribed,
            #{
                clientid => ClientId,
                topic_filter => TopicFilter,
                unsub_props => UnSubProps
            }))).

notify_session_discarded(#{clientid := ClientId}, _SessInfo) ->
    ?IF_ALLOW_TO_NOTIFY(session_discarded,
        emqx_message_tracing_proxy:notify(with_basic_fields(session_discarded,
            #{
                clientid => ClientId
            }))).

notify_session_takenover(#{clientid := ClientId}, _SessInfo) ->
    ?IF_ALLOW_TO_NOTIFY(session_takenover,
        emqx_message_tracing_proxy:notify(with_basic_fields(session_takenover,
            #{
                clientid => ClientId
            }))).

notify_session_terminated(#{clientid := ClientId}, _SessInfo, Reason) ->
    ?IF_ALLOW_TO_NOTIFY(session_terminated,
        emqx_message_tracing_proxy:notify(with_basic_fields(session_terminated,
            #{
                clientid => ClientId,
                reason => bin(Reason)
            }))).

notify_message_received(#message{flags = #{sys := true}}) ->
    ok;
notify_message_received(Message = #message{id = Id, from = ClientId, qos = QoS,
        flags = Flags, topic = Topic, payload = Payload, timestamp = Timestamp}) ->
    ?IF_ALLOW_TO_NOTIFY(message_received,
        emqx_message_tracing_proxy:notify(with_basic_fields(message_received,
            add_payload_field(#{
                msg_id => hex_msg_id(Id),
                from_clientid => ClientId,
                from_username => emqx_message:get_header(username, Message, <<>>),
                topic => Topic,
                qos => QoS,
                flags => Flags,
                pub_props => emqx_message:get_header(properties, Message, #{}),
                message_received_at => Timestamp * 1000
            }, Payload)))).

notify_message_acked_by_emqx(#message{flags = #{sys := true}}, _) ->
    ok;
notify_message_acked_by_emqx(#message{id = Id}, ReasonCode) ->
    ?IF_ALLOW_TO_NOTIFY(message_acked_by_emqx,
        emqx_message_tracing_proxy:notify(
            with_basic_fields(message_acked_by_emqx, #{
                msg_id => hex_msg_id(Id),
                puback_reason_code => ReasonCode
            }))).

notify_message_sent(#message{flags = #{sys := true}}, _) ->
    ok;
notify_message_sent(Message = #message{id = Id, from = ClientId, topic = Topic, qos = QoS, flags = Flags},
        #{
            clientid := ReceiverCId,
            username := ReceiverUsername
        }) ->
    ?IF_ALLOW_TO_NOTIFY(message_sent,
        emqx_message_tracing_proxy:notify(with_basic_fields(message_sent,
            #{
                msg_id => hex_msg_id(Id),
                from_clientid => ClientId,
                from_username => emqx_message:get_header(username, Message, <<>>),
                to_clientid => ReceiverCId,
                to_username => ReceiverUsername,
                topic => Topic,
                qos => QoS,
                flags => Flags,
                pub_props => emqx_message:get_header(properties, Message, #{})
            }))).

notify_message_acked_by_client(#message{flags = #{sys := true}}, _) ->
    ok;
notify_message_acked_by_client(#message{id = Id} = Message, _ClientInfo) ->
    ?IF_ALLOW_TO_NOTIFY(message_acked_by_client,
        emqx_message_tracing_proxy:notify(with_basic_fields(message_acked_by_client,
            #{
                msg_id => hex_msg_id(Id),
                pub_props => emqx_message:get_header(puback_props, Message, #{})
            }))).

notify_message_dropped(#message{id = Id}, Reason) ->
    ?IF_ALLOW_TO_NOTIFY(message_dropped,
        case Reason =:= no_subscribers andalso emqx_message_tracing_plugin:dont_notify_no_subscriber_drop() of
            true -> ok;
            false ->
                emqx_message_tracing_proxy:notify(with_basic_fields(message_dropped,
                    #{
                        msg_id => hex_msg_id(Id),
                        reason_code => bin(Reason)
                    }))
        end).

notify_action_taken(#{event := 'message.publish', id := MsgId, topic := Topic,
      clientid := ClientId, username := Username, qos := QoS,
      metadata := #{action_name := ActionName, resource_id := ResourceId, rule_id := RuleId}}) ->
    ?IF_ALLOW_TO_NOTIFY(action_taken,
        emqx_message_tracing_proxy:notify(
            with_basic_fields(action_taken, #{
                msg_id => MsgId,
                from_clientid => ClientId,
                from_username => Username,
                topic => Topic,
                qos => QoS,
                action_name => bin(ActionName),
                resource_id => ResourceId,
                rule_id => RuleId
            })));
notify_action_taken(_) ->
    ok.

notify_action_complete(#{event := 'message.publish', id := MsgId, topic := Topic,
        clientid := ClientId, username := Username, qos := QoS,
        metadata := #{action_name := ActionName, resource_id := ResourceId, rule_id := RuleId}}, Reason) ->
    ?IF_ALLOW_TO_NOTIFY(action_complete,
        emqx_message_tracing_proxy:notify(
            with_basic_fields(action_complete, #{
                msg_id => MsgId,
                from_clientid => ClientId,
                from_username => Username,
                topic => Topic,
                qos => QoS,
                action_name => bin(ActionName),
                resource_id => ResourceId,
                rule_id => RuleId,
                reason => Reason
            })));
notify_action_complete(_, _) ->
    ok.

%% =============================================================================
with_basic_fields(EventName, EventMsg) when is_map(EventMsg) ->
    maps:merge(#{
        event => EventName,
        node => node(),
        cluster_name => ekka:cluster_name(),
        event_triggered_at => ts_now()
    }, EventMsg).

ts_now() ->
    erlang:system_time(microsecond).

ntoa(undefined) -> undefined;
ntoa({IpAddr, Port}) ->
    list_to_binary(inet:ntoa(IpAddr) ++ ":" ++ integer_to_list(Port)).

bin(A) when is_atom(A) -> atom_to_binary(A);
bin(B) when is_binary(B) -> B;
bin(L) when is_list(L) -> list_to_binary(L);
bin(T) ->
    iolist_to_binary(io_lib:format("~p", [T])).

disconnect_reason(Reason) when is_atom(Reason) -> Reason;
disconnect_reason({shutdown, Reason}) when is_atom(Reason) -> Reason;
disconnect_reason({Error, _}) when is_atom(Error) -> Error;
disconnect_reason(Reason) -> bin(Reason).

add_payload_field(EventMsg, Payload) ->
    case emqx_message_tracing_plugin:is_payload_compression_enabled() of
        true ->
            EventMsg#{payload => compress(Payload), compression => true};
        false ->
            EventMsg#{payload => Payload, compression => false}
    end.

compress(Data) ->
    zlib:zip(Data).

-dialyzer([{nowarn_function, [hex_msg_id/1]}]).
hex_msg_id(Bin) when is_binary(Bin) ->
    binary:encode_hex(Bin);
hex_msg_id(Id) when is_integer(Id) ->
    integer_to_binary(Id).

