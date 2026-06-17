# SPDX-License-Identifier: LGPL-3.0-only
package DCF::Frame;

# The DeMoD 17-byte DeModFrame wire quantum (version 1), byte-identical to the
# reference codec in python/MCP/wirelab_core.py and certified against
# Documentation/golden_vectors.json (the cross-language contract).
#
# Wire layout (big-endian):
#   [0]     sync = 0xD3
#   [1]     flags: version[7:4]=1 | frame_type[3:0]
#   [2:4]   seq      u16
#   [4:6]   src      u16
#   [6:8]   dst      u16
#   [8:12]  payload  4 bytes
#   [12:15] ts_us    u24
#   [15:17] CRC-16/CCITT-FALSE over bytes [0..14]

use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.3.0';
our @EXPORT_OK = qw(crc16 encode decode syndrome SYNC VERSION FRAME_SIZE CRC_COVER BROADCAST);

use constant {
    SYNC       => 0xD3,
    VERSION    => 1,
    FRAME_SIZE => 17,
    CRC_COVER  => 15,
    BROADCAST  => 0xFFFF,
};

# CRC-16/CCITT-FALSE (poly 0x1021, init 0xFFFF, no reflection, no xorout).
sub crc16 {
    my ($data, $len) = @_;
    $len = length($data) unless defined $len;
    my $crc = 0xFFFF;
    for my $k (0 .. $len - 1) {
        $crc ^= (ord(substr($data, $k, 1)) << 8);
        $crc &= 0xFFFF;
        for (1 .. 8) {
            if ($crc & 0x8000) {
                $crc = (($crc << 1) ^ 0x1021) & 0xFFFF;
            } else {
                $crc = ($crc << 1) & 0xFFFF;
            }
        }
    }
    return $crc & 0xFFFF;
}

# Serialise a frame (named args: version/type/seq/src/dst/payload/ts_us) into a
# 17-byte string, computing and appending the CRC. payload is a 4-byte string.
sub encode {
    my (%f) = @_;
    my $version = defined $f{version} ? $f{version} : 1;
    my $type    = defined $f{type}    ? $f{type}    : 0;
    my $seq     = defined $f{seq}     ? $f{seq}     : 0;
    my $src     = defined $f{src}     ? $f{src}     : 0;
    my $dst     = defined $f{dst}     ? $f{dst}     : 0;
    my $ts      = defined $f{ts_us}   ? $f{ts_us}   : 0;
    my $payload = defined $f{payload} ? $f{payload} : "\0\0\0\0";
    die "payload must be exactly 4 bytes\n" unless length($payload) == 4;

    my $body = pack('C C n n n a4 C C C',
        SYNC,
        (($version & 0x0F) << 4) | ($type & 0x0F),
        $seq & 0xFFFF, $src & 0xFFFF, $dst & 0xFFFF,
        $payload,
        ($ts >> 16) & 0xFF, ($ts >> 8) & 0xFF, $ts & 0xFF,
    );
    return $body . pack('n', crc16($body, CRC_COVER));
}

# Affine validity syndrome of a 17-byte word: CRC-valid iff this returns 0.
sub syndrome {
    my ($word) = @_;
    die "need 17 bytes\n" unless length($word) == FRAME_SIZE;
    my $stored = unpack('n', substr($word, 15, 2));
    return (crc16($word, CRC_COVER) ^ $stored) & 0xFFFF;
}

# Parse a 17-byte buffer, validating sync, version nibble, and CRC. Returns a
# hashref (suitable to feed straight back into encode), or dies with a reason.
sub decode {
    my ($word) = @_;
    die "length != 17\n" unless length($word) == FRAME_SIZE;
    my @b = unpack('C17', $word);
    die "bad sync byte\n"      unless $b[0] == SYNC;
    die "bad version nibble\n" unless ($b[1] >> 4) == VERSION;
    die "CRC mismatch\n"       unless syndrome($word) == 0;
    return {
        version => $b[1] >> 4,
        type    => $b[1] & 0x0F,
        seq     => ($b[2] << 8) | $b[3],
        src     => ($b[4] << 8) | $b[5],
        dst     => ($b[6] << 8) | $b[7],
        payload => substr($word, 8, 4),
        ts_us   => ($b[12] << 16) | ($b[13] << 8) | $b[14],
    };
}

# Self-certify on load: die (refuse to load) if the codec has diverged from the
# reference (CRC + example-frame anchors).
{
    my $c1 = crc16("123456789");
    die sprintf("DCF::Frame: CRC anchor diverged: CRC(\"123456789\")=0x%04X\n", $c1)
        unless $c1 == 0x29B1;
    my $c0 = crc16("\0" x 15);
    die sprintf("DCF::Frame: CRC anchor diverged: CRC(0^15)=0x%04X\n", $c0)
        unless $c0 == 0x4EC3;
    my $got = unpack('H*', encode(
        version => 1, type => 3, seq => 0x1234, src => 1, dst => BROADCAST,
        payload => "\xDE\xAD\xBE\xEF", ts_us => 0xAB12CD,
    ));
    die "DCF::Frame: exampleFrame anchor diverged: $got\n"
        unless $got eq 'd31312340001ffffdeadbeefab12cd24c0';
}

1;
