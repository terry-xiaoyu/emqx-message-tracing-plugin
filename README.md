# emqx_message_tracing_plugin

Message tracing plugin for EMQX 5.3.x

## Install

1. Build

```shell
make rel

...
===> Release successfully assembled: _build/default/rel/emqx_message_tracing_plugin
===> [emqx_plugrel] creating /Users/liuxy/code/emqx-message-tracing-plugin/_build/default/emqx_plugrel/emqx_message_tracing_plugin-1.0.0.tar.gz
```

2. Install the `emqx_message_tracing_plugin-x.y.z.tar.gz` from the emqx dashboard

## Features

- Track the clients
  - connect/connack/disconnect
  - message sent/received by a client
  - [*] authentication/authorization for clients

- Track the messages

- [*] Messages sent to data integrations

3. Logs will be save to files:

```shell
tail -f /tmp/message_tracing.log
```

```json

{"reason":"remote","node":"emqx@127.0.0.1","event_triggered_at":1699614684390883,"event":"client_disconnected","disconn_props":{},"cluster_name":"emqxcl","clientid":"emqx_OTY5MT"}

{"reason":"normal","node":"emqx@127.0.0.1","event_triggered_at":1699614684394242,"event":"session_terminated","cluster_name":"emqxcl","clientid":"emqx_OTY5MT"}

{"username":"","sockname":"127.0.0.1:8083","receive_maximum":32,"proto_ver":5,"proto_name":"MQTT","peername":"127.0.0.1:60681","node":"emqx@127.0.0.1","keepalive":60,"expiry_interval":0,"event_triggered_at":1699614690979426,"event":"client_connect","conn_props":{"Session-Expiry-Interval":0},"cluster_name":"emqxcl","clientid":"emqx_OTY5MT","clean_start":true}

{"reason_code":"success","node":"emqx@127.0.0.1","event_triggered_at":1699614690990093,"event":"client_connack","connack_props":{"Wildcard-Subscription-Available":1,"Topic-Alias-Maximum":65535,"Subscription-Identifier-Available":1,"Shared-Subscription-Available":1,"Retain-Available":1,"Maximum-Packet-Size":1048576},"cluster_name":"emqxcl","clientid":"emqx_OTY5MT"}

{"topic_filter":"testtopic/#","sub_props":{"sub_props":{},"rh":0,"rap":0,"qos":0,"nl":0,"is_new":true},"node":"emqx@127.0.0.1","event_triggered_at":1699614698871022,"event":"session_subscribed","cluster_name":"emqxcl","clientid":"emqx_OTY5MT"}

{"topic":"testtopic/1","qos":0,"pub_props":{},"payload_encode_type":"base64","payload":"q1ZQyi1OV7JSUMpIzcnJV1KoBQA=","node":"emqx@127.0.0.1","msg_id":"000609CA6486B582F44500000F5C0002","message_received_at":1699614734857000,"from_username":"","from_clientid":"emqx_OTY5MT","flags":{"retain":false,"dup":false},"event_triggered_at":1699614734860852,"event":"message_received","compression":true,"cluster_name":"emqxcl"}

{"topic":"testtopic/1","to_username":"","to_clientid":"emqx_OTY5MT","qos":0,"pub_props":{},"node":"emqx@127.0.0.1","msg_id":"000609CA6486B582F44500000F5C0002","from_username":"","from_clientid":"emqx_OTY5MT","flags":{"retain":false,"dup":false},"event_triggered_at":1699614734861929,"event":"message_sent","cluster_name":"emqxcl"}

{"unsub_props":{"unsub_props":{},"sub_props":{},"rh":0,"rap":0,"qos":0,"nl":0},"topic_filter":"testtopic/#","node":"emqx@127.0.0.1","event_triggered_at":1699614838745056,"event":"session_unsubscribed","cluster_name":"emqxcl","clientid":"emqx_OTY5MT"}

{"topic_filter":"testtopic/#","sub_props":{"sub_props":{},"rh":0,"rap":0,"qos":2,"nl":0,"is_new":true},"node":"emqx@127.0.0.1","event_triggered_at":1699614842735036,"event":"session_subscribed","cluster_name":"emqxcl","clientid":"emqx_OTY5MT"}

{"topic":"testtopic/1","qos":2,"pub_props":{},"payload_encode_type":"base64","payload":"q1ZQyi1OV7JSUMpIzcnJV1KoBQA=","node":"emqx@127.0.0.1","msg_id":"000609CA6BB4789AF44500000F5C0003","message_received_at":1699614855297000,"from_username":"","from_clientid":"emqx_OTY5MT","flags":{"retain":false,"dup":false},"event_triggered_at":1699614855297553,"event":"message_received","compression":true,"cluster_name":"emqxcl"}

{"topic":"testtopic/1","to_username":"","to_clientid":"emqx_OTY5MT","qos":2,"pub_props":{},"node":"emqx@127.0.0.1","msg_id":"000609CA6BB4789AF44500000F5C0003","from_username":"","from_clientid":"emqx_OTY5MT","flags":{"retain":false,"dup":false},"event_triggered_at":1699614855297937,"event":"message_sent","cluster_name":"emqxcl"}

{"pub_props":{},"node":"emqx@127.0.0.1","msg_id":"000609CA6BB4789AF44500000F5C0003","event_triggered_at":1699614855300652,"event":"message_acked_by_client","cluster_name":"emqxcl"}
```

## Configuration

The config file is under the `plugins` directory of the installation directory of `emqx`:

```shell
ls plugins/emqx_message_tracing_plugin-1.0.0/emqx_message_tracing_plugin-0.1.0/priv

config.hocon
```
