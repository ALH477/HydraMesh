#include <dcf_sdk/dcf_plugin_manager.h>
#include <modbus/modbus.h>

typedef struct {
    modbus_t* ctx;
} ModbusTransport;

bool modbus_setup(void* self, const char* host, int port) {
    ModbusTransport* mt = (ModbusTransport*)self;
    mt->ctx = modbus_new_tcp(host, port);
    if (!mt->ctx) return false;
    return modbus_connect(mt->ctx) != -1;
}

bool modbus_send(void* self, const uint8_t* data, size_t size, const char* target) {
    ModbusTransport* mt = (ModbusTransport*)self;
    modbus_set_slave(mt->ctx, atoi(target));
    uint16_t regs[2] = { (data[0] << 8) | data[1], (data[2] << 8) | data[3] };
    return modbus_write_registers(mt->ctx, 0, 2, regs) != -1;
}

uint8_t* modbus_receive(void* self, size_t* size) {
    ModbusTransport* mt = (ModbusTransport*)self;
    uint16_t regs[2];
    if (modbus_read_registers(mt->ctx, 0, 2, regs) < 0) return NULL;
    uint8_t* buf = malloc(4);
    buf[0] = regs[0] >> 8; buf[1] = regs[0] & 0xFF;
    buf[2] = regs[1] >> 8; buf[3] = regs[1] & 0xFF;
    *size = 4;
    return buf;
}

void modbus_destroy(void* self) {
    ModbusTransport* mt = (ModbusTransport*)self;
    modbus_close(mt->ctx);
    modbus_free(mt->ctx);
    free(self);
}

ITransport iface = {modbus_setup, modbus_send, modbus_receive, modbus_destroy};

void* create_plugin() { return calloc(1, sizeof(ModbusTransport)); }

const char* get_plugin_version() { return "1.0"; }
