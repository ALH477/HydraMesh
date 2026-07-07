// SPDX-License-Identifier: LGPL-3.0-only
/*
 * snake_l2.c — AF_PACKET socket helpers for the DCF-Snake raw-L2 Ethernet transport.
 * DeMoD LLC | LGPL-3.0.  Linux only (AF_PACKET); other platforms get stubs that fail
 * with ENOSYS.  The pure batch/unbatch codec is header-only in snake_l2.h.
 */
/* glibc feature macro: expose struct ifreq / IFNAMSIZ / sockaddr_ll under strict -std=c11
 * (real builds use -std=gnu11 via dcf-tools/build.sh; this makes the CI c11 compile pass too).
 * Must precede any libc header — snake_l2.h is the first include below. */
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif
#include "snake_l2.h"
#include <errno.h>

#if defined(__linux__)
#include <arpa/inet.h>
#include <linux/if_packet.h>
#include <net/ethernet.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <unistd.h>

int dcf_snake_l2_open(dcf_snake_l2_sock_t *s, const char *ifname, uint16_t ethertype) {
    memset(s, 0, sizeof *s);
    s->fd = -1;
    s->ethertype = ethertype;

    int fd = socket(AF_PACKET, SOCK_DGRAM, htons(ethertype));
    if (fd < 0) return -1;                     /* EPERM without CAP_NET_RAW */

    struct ifreq ifr;
    memset(&ifr, 0, sizeof ifr);
    strncpy(ifr.ifr_name, ifname, IFNAMSIZ - 1);
    if (ioctl(fd, SIOCGIFINDEX, &ifr) < 0) { int e = errno; close(fd); errno = e; return -1; }
    s->ifindex = ifr.ifr_ifindex;
    if (ioctl(fd, SIOCGIFHWADDR, &ifr) < 0) { int e = errno; close(fd); errno = e; return -1; }
    memcpy(s->mac, ifr.ifr_hwaddr.sa_data, 6);

    struct sockaddr_ll sll;
    memset(&sll, 0, sizeof sll);
    sll.sll_family   = AF_PACKET;
    sll.sll_protocol = htons(ethertype);
    sll.sll_ifindex  = s->ifindex;
    if (bind(fd, (struct sockaddr *)&sll, sizeof sll) < 0) {
        int e = errno; close(fd); errno = e; return -1;
    }
    s->fd = fd;
    return 0;
}

long dcf_snake_l2_send(const dcf_snake_l2_sock_t *s, const uint8_t dst_mac[6],
                       const uint8_t *payload, size_t len) {
    static const uint8_t bcast[6] = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF};
    struct sockaddr_ll sll;
    memset(&sll, 0, sizeof sll);
    sll.sll_family   = AF_PACKET;
    sll.sll_protocol = htons(s->ethertype);
    sll.sll_ifindex  = s->ifindex;
    sll.sll_halen    = 6;
    memcpy(sll.sll_addr, dst_mac ? dst_mac : bcast, 6);
    return sendto(s->fd, payload, len, 0, (struct sockaddr *)&sll, sizeof sll);
}

long dcf_snake_l2_recv(const dcf_snake_l2_sock_t *s, uint8_t *buf, size_t cap) {
    return recv(s->fd, buf, cap, 0);
}

void dcf_snake_l2_close(dcf_snake_l2_sock_t *s) {
    if (s->fd >= 0) close(s->fd);
    s->fd = -1;
}

#else  /* non-Linux: raw AF_PACKET is unavailable */

int dcf_snake_l2_open(dcf_snake_l2_sock_t *s, const char *ifname, uint16_t ethertype) {
    (void)ifname; (void)ethertype; memset(s, 0, sizeof *s); s->fd = -1; errno = ENOSYS; return -1;
}
long dcf_snake_l2_send(const dcf_snake_l2_sock_t *s, const uint8_t dst_mac[6],
                       const uint8_t *payload, size_t len) {
    (void)s; (void)dst_mac; (void)payload; (void)len; errno = ENOSYS; return -1;
}
long dcf_snake_l2_recv(const dcf_snake_l2_sock_t *s, uint8_t *buf, size_t cap) {
    (void)s; (void)buf; (void)cap; errno = ENOSYS; return -1;
}
void dcf_snake_l2_close(dcf_snake_l2_sock_t *s) { s->fd = -1; }

#endif
