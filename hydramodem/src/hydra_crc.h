/* hydra_crc.h -- CRC-16/CCITT-FALSE (poly 0x1021, init 0xFFFF, no reflect). */
#ifndef HYDRA_CRC_H
#define HYDRA_CRC_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
uint16_t hydra_crc16_ccitt(const uint8_t *data, size_t len);
#ifdef __cplusplus
}
#endif
#endif
