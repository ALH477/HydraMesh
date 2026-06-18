# SPDX-License-Identifier: LGPL-3.0-only
#
# Certifies DCF::SuperPack byte-for-byte against the cross-language golden vectors
# in Documentation/superpack_vectors.json. Core-module only (JSON::PP + Test::More).
#   prove -l t/superpack.t        (from the perl/ directory)

use strict;
use warnings;
use Test::More;
use JSON::PP qw(decode_json);
use FindBin;
use lib "$FindBin::Bin/../lib";
use DCF::Frame qw(encode);
use DCF::SuperPack qw(pack_super unpack_super is_superpack SUPER_LEN);

my @candidates = (
    "$FindBin::Bin/../../Documentation/superpack_vectors.json",
    "$FindBin::Bin/../../python/MCP/superpack_vectors.json",
);
my ($path) = grep { -e $_ } @candidates;
BAIL_OUT("superpack_vectors.json not found (run gen_superpack_vectors.py)") unless $path;

open my $fh, '<:raw', $path or BAIL_OUT("open $path: $!");
local $/;
my $sv = decode_json(<$fh>);
close $fh;

my $cases = $sv->{cases};

# 1. pack(A, B) matches the golden 32-byte container
for my $i (0 .. $#$cases) {
    my $c = $cases->[$i];
    my $a  = pack('H*', $c->{a});
    my $b  = pack('H*', $c->{b});
    my $sp = pack_super($a, $b);
    is(length($sp), SUPER_LEN, "case $i length 32");
    ok(is_superpack($sp), "case $i recognised as SuperPack");
    is(unpack('H*', $sp), lc($c->{super}), "case $i pack bytes match");
}

# 2. unpack(S) reconstructs both frames bit-exact
for my $i (0 .. $#$cases) {
    my $c = $cases->[$i];
    my ($a, $b) = unpack_super(pack('H*', $c->{super}));
    is(unpack('H*', $a), lc($c->{a}), "case $i unpack frame A");
    is(unpack('H*', $b), lc($c->{b}), "case $i unpack frame B");
}

# 3. joint CRC is tamper-evident on case 0
{
    my $sp = pack('H*', $cases->[0]{super});
    my $rejected = 0;
    for my $i (0 .. SUPER_LEN - 1) {
        my $bad = $sp;
        substr($bad, $i, 1) = chr(ord(substr($bad, $i, 1)) ^ 0x01);
        eval { unpack_super($bad); 1 } or $rejected++;
    }
    is($rejected, SUPER_LEN, 'every single-bit flip rejected');
}

# 4. zero-core anchor: SuperPack of two all-zero-core frames has joint CRC 0x5B75
{
    my $zero = encode(version => 1, type => 0);
    my $spz  = pack_super($zero, $zero);
    my $joint = unpack('n', substr($spz, 30, 2));
    is($joint, 0x5B75, 'zero-core joint CRC anchor = 0x5B75');
    is($joint, $sv->{anchors}{zero_core_joint_crc}, 'anchor matches vectors');
}

done_testing();
