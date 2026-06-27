/* hydra_crc.c -- bitwise CRC-16/CCITT-FALSE. Small, table-free; the 17-byte
 * payload makes table acceleration pointless on the JH7110. */
#include "hydra_crc.h"

uint16_t hydra_crc16_ccitt(const uint8_t *data, size_t len)
{
    uint16_t crc = 0xFFFFu;
    size_t i;
    int b;
    for (i = 0; i < len; ++i) {
        crc ^= (uint16_t)data[i] << 8;
        for (b = 0; b < 8; ++b) {
            if (crc & 0x8000u) crc = (uint16_t)((crc << 1) ^ 0x1021u);
            else               crc = (uint16_t)(crc << 1);
        }
    }
    return crc;
}
