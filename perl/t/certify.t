# SPDX-License-Identifier: LGPL-3.0-only
#
# Certifies DCF::Frame byte-for-byte against the cross-language golden vectors in
# Documentation/golden_vectors.json. Core-module only (JSON::PP + Test::More).
#   prove -l t/certify.t        (from the perl/ directory)
#   perl Makefile.PL && make test

use strict;
use warnings;
use Test::More;
use JSON::PP qw(decode_json);
use FindBin;
use lib "$FindBin::Bin/../lib";
use DCF::Frame qw(crc16 encode decode syndrome FRAME_SIZE SYNC CRC_COVER BROADCAST);

my @candidates = (
    "$FindBin::Bin/../../Documentation/golden_vectors.json",
    "$FindBin::Bin/../../python/MCP/golden_vectors.json",
);
my ($path) = grep { -e $_ } @candidates;
BAIL_OUT("golden_vectors.json not found") unless $path;

open my $fh, '<:raw', $path or BAIL_OUT("open $path: $!");
local $/;
my $gv = decode_json(<$fh>);
close $fh;

# 1. CRC anchors
is(crc16("123456789"), 0x29B1, 'CRC("123456789") = 0x29B1');
is(crc16("\0" x 15),   0x4EC3, 'CRC(0^15) = 0x4EC3');

# 2. example frame anchor
my $ex = unpack('H*', encode(
    version => 1, type => 3, seq => 0x1234, src => 1, dst => BROADCAST,
    payload => "\xDE\xAD\xBE\xEF", ts_us => 0xAB12CD,
));
is($ex, lc($gv->{anchors}{exampleFrame_full}), 'exampleFrame_full matches');

# 3. encode_basis: raw-CRC-valid + (known types) decode/roundtrip
sub raw_valid {
    my ($b) = @_;
    return 0 unless length($b) == FRAME_SIZE;
    my @x = unpack('C*', $b);
    return 0 unless $x[0] == SYNC;
    my $stored = unpack('n', substr($b, 15, 2));
    return crc16($b, CRC_COVER) == $stored ? 1 : 0;
}

my $enc = $gv->{encode_basis};
my $enc_fail = 0;
for my $i (0 .. $#$enc) {
    my $rawf = pack('H*', $enc->[$i]{frame});
    unless (raw_valid($rawf)) {
        $enc_fail++;
        diag("encode_basis[$i]: raw CRC invalid");
        next;
    }
    my @b = unpack('C17', $rawf);
    if (($b[1] & 0x0F) <= 3) {
        my $f = eval { decode($rawf) };
        if (!defined $f) {
            $enc_fail++;
            diag("encode_basis[$i]: decode died: $@");
        } elsif (encode(%$f) ne $rawf) {
            $enc_fail++;
            diag("encode_basis[$i]: roundtrip mismatch");
        }
    }
}
is($enc_fail, 0, scalar(@$enc) . ' encode_basis vectors (decode + roundtrip)');

# 4. syndrome_basis: reproduce each basis word and check its syndrome
my $syn = $gv->{syndrome_basis};
my $syn_fail = 0;
for my $i (0 .. $#$syn) {
    my $v = $syn->[$i];
    my @w = (0) x FRAME_SIZE;
    if (defined $v->{bit}) {
        my $bit = $v->{bit};
        $w[int($bit / 8)] = 1 << (7 - ($bit % 8));
    } elsif (defined $v->{word}) {
        @w = unpack('C17', pack('H*', $v->{word}));
    }
    my $got = syndrome(pack('C17', @w));
    if ($got != $v->{syndrome}) {
        $syn_fail++;
        diag(sprintf("syndrome_basis[%d]: got 0x%04X want 0x%04X", $i, $got, $v->{syndrome}));
    }
}
is($syn_fail, 0, scalar(@$syn) . ' syndrome_basis vectors');

done_testing();
