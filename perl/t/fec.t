# SPDX-License-Identifier: LGPL-3.0-only
#
# Certifies DCF::FEC byte-for-byte against the cross-language golden vectors in
# Documentation/fec_vectors.json. Core-module only (JSON::PP + Test::More).
#   prove -l t/fec.t        (from the perl/ directory)

use strict;
use warnings;
use Test::More;
use JSON::PP qw(decode_json);
use FindBin;
use lib "$FindBin::Bin/../lib";
use DCF::FEC qw(rs_encode rs_decode encode_message decode_message);

my @candidates = (
    "$FindBin::Bin/../../Documentation/fec_vectors.json",
    "$FindBin::Bin/../../python/MCP/fec_vectors.json",
);
my ($path) = grep { -e $_ } @candidates;
BAIL_OUT("fec_vectors.json not found (run gen_fec_vectors.py)") unless $path;

open my $fh, '<:raw', $path or BAIL_OUT("open $path: $!");
local $/;
my $v = decode_json(<$fh>);
close $fh;

# 1. systematic encode byte-identical
for my $i (0 .. $#{ $v->{cases} }) {
    my $c = $v->{cases}[$i];
    is(unpack('H*', rs_encode(pack('H*', $c->{msg}), $v->{nparity})), lc($c->{code}), "encode case $i");
}

# 2. decode corrects the pinned corrupted codewords
for my $i (0 .. $#{ $v->{correct} }) {
    my $c = $v->{correct}[$i];
    my ($msg, $n) = rs_decode(pack('H*', $c->{corrupt}), $v->{nparity}, 17);
    is(unpack('H*', $msg), lc($c->{msg}), "decode case $i recovers");
    is($n, $c->{nerr}, "decode case $i count");
}

# 3. multi-codeword messages: golden blob byte-identical + round-trip
for my $i (0 .. $#{ $v->{messages} }) {
    my $m = $v->{messages}[$i];
    my $blob = encode_message(pack('H*', $m->{msg}), $v->{nparity});
    is(unpack('H*', $blob), lc($m->{blob}), "message $i blob (len $m->{len})");
    my ($out) = decode_message($blob);
    is(unpack('H*', $out), lc($m->{msg}), "message $i round-trip");
}

# 4. interleaved burst across codewords is corrected
{
    my ($out) = decode_message(pack('H*', $v->{message_burst}{corrupt}));
    is(unpack('H*', $out), lc($v->{message_burst}{msg}), "interleaved burst corrected");
}

done_testing();
