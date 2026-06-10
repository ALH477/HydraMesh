# Legacy / Dead Tests

These test files reference APIs and functions that do not exist in the
current SDK (`dcf_config_load`, `dcf_config_get_host`, `streamdb_close`,
`dcf-group-peers`, `dcf-simulate-failure`, `dcf-visualize-topology`, etc.).

They are preserved here for reference but are **not compiled or run**.
Do not add them to the build without first implementing the missing APIs
against the current `dcf_types.h` interface.