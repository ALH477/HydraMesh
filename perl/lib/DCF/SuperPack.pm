# SPDX-License-Identifier: LGPL-3.0-only
package DCF::SuperPack;

# DCF SuperPack — a 32-byte container that losslessly carries TWO 17-byte
# DeModFrame quanta under a single joint CRC-16.
#
# Two raw frames cost 2*17 = 34 bytes. When frames are emitted in pairs the
# second header is largely recoverable from context (both inner sync bytes are
# 0xD3; each inner CRC is a pure function of its own 15 leading bytes). SuperPack
# drops those 6 redundant bytes and spends 4 back on one outer sync, a
# type/version tag, and ONE joint CRC over the whole container — net 34 -> 32
# bytes plus a strictly stronger integrity check.
#
# Why it is the lower-latency option: a SuperPack puts a frame pair on the wire
# as a SINGLE datagram instead of two — one packet, one IP/UDP header, one
# syscall — so paired traffic crosses the network with strictly lower per-pair
# overhead and latency than emitting the two frames separately.
#
# Unpack reconstructs each inner frame bit-exact, so the outputs are ordinary
# valid DeModFrames and the 246-vector wire certificate is untouched.

use strict;
use warnings;
use Exporter 'import';
use DCF::Frame qw(crc16 decode SYNC VERSION FRAME_SIZE CRC_COVER);

our $VERSION = '0.3.0';
our @EXPORT_OK = qw(pack_super unpack_super is_superpack SUPER_TYPE SUPER_LEN SUPER_CORE_LEN);

use constant {
    SUPER_TYPE     => 0x05,
    SUPER_LEN      => 32,
    SUPER_CORE_LEN => 14,
};
use constant SUPER_SFLAGS => (VERSION << 4) | SUPER_TYPE;

# The 14 reconstructable bytes of a 17-byte frame, validating sync, version, and
# the inner CRC so a corrupt frame is never packed. Dies on failure.
sub _frame_core {
    my ($frame) = @_;
    die "need a 17-byte frame\n" unless length($frame) == FRAME_SIZE;
    my @b = unpack('C17', $frame);
    die "bad sync byte\n"      unless $b[0] == SYNC;
    die "bad version nibble\n" unless ($b[1] >> 4) == VERSION;
    my $stored = unpack('n', substr($frame, 15, 2));
    die "inner frame CRC mismatch\n" unless crc16($frame, CRC_COVER) == $stored;
    return substr($frame, 1, SUPER_CORE_LEN);
}

# Rebuild a full 17-byte frame from its 14-byte core (sync + recomputed CRC).
sub _rebuild_frame {
    my ($core) = @_;
    my $body = chr(SYNC) . $core;
    return $body . pack('n', crc16($body, CRC_COVER));
}

# Combine two valid 17-byte frames into one 32-byte SuperPack string.
sub pack_super {
    my ($a, $b) = @_;
    my $head = chr(SYNC) . chr(SUPER_SFLAGS) . _frame_core($a) . _frame_core($b);
    return $head . pack('n', crc16($head, 30));
}

# True iff buf looks like a SuperPack (length + sync + version/type tag).
sub is_superpack {
    my ($buf) = @_;
    return 0 unless length($buf) == SUPER_LEN;
    my @b = unpack('C2', $buf);
    return ($b[0] == SYNC && $b[1] == SUPER_SFLAGS) ? 1 : 0;
}

# Split a 32-byte SuperPack into (frame_a, frame_b), each a bit-exact 17-byte
# frame. Dies on any integrity failure.
sub unpack_super {
    my ($buf) = @_;
    die "length != 32\n" unless length($buf) == SUPER_LEN;
    my @b = unpack('C2', $buf);
    die "bad sync byte\n"      unless $b[0] == SYNC;
    die "bad version nibble\n" unless ($b[1] >> 4) == VERSION;
    die "not a SuperPack type\n" unless ($b[1] & 0x0F) == SUPER_TYPE;
    my $stored = unpack('n', substr($buf, 30, 2));
    die "SuperPack CRC mismatch\n" unless crc16($buf, 30) == $stored;
    my $frame_a = _rebuild_frame(substr($buf, 2, SUPER_CORE_LEN));
    my $frame_b = _rebuild_frame(substr($buf, 2 + SUPER_CORE_LEN, SUPER_CORE_LEN));
    decode($frame_a);  # belt and braces: must decode cleanly
    decode($frame_b);
    return ($frame_a, $frame_b);
}

1;
