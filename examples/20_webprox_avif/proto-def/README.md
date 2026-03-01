wireshark needs http2 to decode grpc
if data is compressed then brotli and snappy are also helpful

```
net-analyzer/wireshark plugins ssl maxminddb sdjournal  smi  sshdump wifi http2 brotli snappy  LUA_SINGLE_TARGET: lua5-4
```