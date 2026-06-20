# SPDX-License-Identifier: LGPL-3.0-only
package DCF::FEC;

# DCF forward-error-correction adapter — systematic Reed-Solomon over GF(2^8),
# byte-identical to python/MCP/feclab_core.py, codec/demod_fec.h, codec/src/fec.rs,
# go/dcf/fec.go, JS/nodejs/src/fec.js, and cpp/include/dcf/fec.hpp (pinned by
# Documentation/fec_vectors.json). FEC wraps a 17-byte DeModFrame so a lossy medium
# (RF/SDR, acoustic) can CORRECT it, not just detect damage.
#
# Field GF(2^8), prim 0x11D, generator a=2, fcr=0. Systematic: codeword =
# message ++ parity. Default 2t=16 parity -> corrects 8 byte-errors. The
# multi-codeword message layer chunks + interleaves any-length payloads.

use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.3.0';
our @EXPORT_OK = qw(rs_encode rs_decode interleave deinterleave encode_message
                    decode_message RS_DEFAULT_NPARITY HDR_PARITY HDR_LEN);

use constant {
    GF_PRIM            => 0x11D,
    GF_GEN             => 2,
    FCR                => 0,
    RS_DEFAULT_NPARITY => 16,
    HDR_PARITY         => 16,
    HDR_LEN            => 21,
};

my (@EXP, @LOG);
{
    my $x = 1;
    for my $i (0 .. 254) { $EXP[$i] = $x; $LOG[$x] = $i; $x <<= 1; $x ^= GF_PRIM if $x & 0x100; }
    for my $i (255 .. 511) { $EXP[$i] = $EXP[$i - 255]; }
}

sub gmul { my ($a, $b) = @_; return 0 if $a == 0 || $b == 0; return $EXP[$LOG[$a] + $LOG[$b]]; }
sub gdiv { my ($a, $b) = @_; return 0 if $a == 0; return $EXP[($LOG[$a] + 255 - $LOG[$b]) % 255]; }
sub gpow { my ($a, $p) = @_; return $EXP[(($LOG[$a] * $p) % 255 + 255) % 255]; }
sub ginv { my ($a) = @_; return $EXP[255 - $LOG[$a]]; }

# GF polynomials as arrayrefs of ints, high-order coefficient first.
sub _pmul {
    my ($p, $q) = @_;
    my @r = (0) x (@$p + @$q - 1);
    for my $j (0 .. $#$q) { for my $i (0 .. $#$p) { $r[$i + $j] ^= gmul($p->[$i], $q->[$j]); } }
    return \@r;
}
sub _padd {
    my ($p, $q) = @_;
    my $n = @$p > @$q ? @$p : @$q;
    my @r = (0) x $n;
    for my $i (0 .. $#$p) { $r[$i + $n - @$p] = $p->[$i]; }
    for my $i (0 .. $#$q) { $r[$i + $n - @$q] ^= $q->[$i]; }
    return \@r;
}
sub _pscale { my ($p, $s) = @_; return [ map { gmul($_, $s) } @$p ]; }
sub _peval { my ($p, $x) = @_; my $y = $p->[0]; for my $i (1 .. $#$p) { $y = gmul($y, $x) ^ $p->[$i]; } return $y; }

sub _genpoly { my ($np) = @_; my $g = [1]; for my $i (0 .. $np - 1) { $g = _pmul($g, [1, gpow(GF_GEN, FCR + $i)]); } return $g; }

sub rs_encode {
    my ($msg, $np) = @_;
    my @m = unpack('C*', $msg);
    my $gen = _genpoly($np);
    my @out = (@m, (0) x $np);
    for my $i (0 .. $#m) {
        my $coef = $out[$i];
        if ($coef) { for my $j (1 .. $#$gen) { $out[$i + $j] ^= gmul($gen->[$j], $coef); } }
    }
    return pack('C*', @m, @out[scalar(@m) .. $#out]);
}

sub _syndromes {
    my ($cw, $np) = @_;
    my @s = (0);
    for my $i (0 .. $np - 1) { push @s, _peval($cw, gpow(GF_GEN, FCR + $i)); }
    return \@s;
}
sub _errloc {
    my ($synd, $np) = @_;
    my $el = [1];
    my $ol = [1];
    for my $i (0 .. $np - 1) {
        my $delta = $synd->[$i + 1];
        for my $j (1 .. $#$el) { $delta ^= gmul($el->[$#$el - $j], $synd->[$i + 1 - $j]); }
        push @$ol, 0;
        if ($delta != 0) {
            if (@$ol > @$el) {
                my $nl = _pscale($ol, $delta);
                $ol = _pscale($el, ginv($delta));
                $el = $nl;
            }
            $el = _padd($el, _pscale($ol, $delta));
        }
    }
    shift @$el while @$el && $el->[0] == 0;
    return $el;
}
sub _correct {
    my ($cw, $synd, $pos) = @_;
    my $n = @$cw;
    my @coefpos = map { $n - 1 - $_ } @$pos;
    my $eloc = [1];
    my @X;
    for my $p (@coefpos) { $eloc = _pmul($eloc, [gpow(GF_GEN, $p), 1]); push @X, gpow(GF_GEN, $p); }
    my $syndrev = [reverse @$synd];
    my $prod = _pmul($syndrev, $eloc);
    my $remlen = @$eloc;
    my $rem = [@{$prod}[(@$prod - $remlen) .. $#$prod]];
    for my $i (0 .. $#X) {
        my $Xi = $X[$i];
        my $XiInv = ginv($Xi);
        my $denom = 1;
        for my $j (0 .. $#X) { if ($j != $i) { $denom = gmul($denom, 1 ^ gmul($XiInv, $X[$j])); } }
        my $numer = _peval($rem, $XiInv);
        $numer = gmul($numer, gpow($Xi, 1 - FCR));
        return 0 if $denom == 0;
        $cw->[$pos->[$i]] ^= gdiv($numer, $denom);
    }
    return 1;
}

# Returns (message bytes, corrected count). Dies if uncorrectable.
sub rs_decode {
    my ($codeword, $np, $msglen) = @_;
    my @cw = unpack('C*', $codeword);
    $msglen = @cw - $np unless defined $msglen;
    my $synd = _syndromes(\@cw, $np);
    my $allzero = 1;
    for (@$synd) { if ($_ != 0) { $allzero = 0; last; } }
    return (pack('C*', @cw[0 .. $msglen - 1]), 0) if $allzero;
    my $el = _errloc($synd, $np);
    die "fec: too many errors" if @$el - 1 > int($np / 2);
    my @rev = reverse @$el;
    my $errs = @rev - 1;
    my @pos;
    for my $i (0 .. $#cw) { push @pos, $#cw - $i if _peval(\@rev, gpow(GF_GEN, $i)) == 0; }
    die "fec: error location failed" if @pos != $errs;
    _correct(\@cw, $synd, \@pos) or die "fec: forney failed";
    for (@{ _syndromes(\@cw, $np) }) { die "fec: residual syndrome" if $_ != 0; }
    return (pack('C*', @cw[0 .. $msglen - 1]), scalar(@pos));
}

sub interleave {
    my (@cws) = @_;
    return '' unless @cws;
    my $d = @cws;
    my $n = length($cws[0]);
    my @b = map { [unpack('C*', $_)] } @cws;
    my @out = (0) x ($d * $n);
    for my $r (0 .. $d - 1) { for my $c (0 .. $n - 1) { $out[$c * $d + $r] = $b[$r][$c]; } }
    return pack('C*', @out);
}
sub deinterleave {
    my ($stream, $depth, $n) = @_;
    my @s = unpack('C*', $stream);
    my @cws = map { [] } (1 .. $depth);
    for my $c (0 .. $n - 1) { for my $r (0 .. $depth - 1) { $cws[$r][$c] = $s[$c * $depth + $r]; } }
    return map { pack('C*', @$_) } @cws;
}

sub _chunking {
    my ($l, $np) = @_;
    my $maxk = 255 - $np;
    my $nchunks = $l == 0 ? 1 : int(($l + $maxk - 1) / $maxk);
    my $k = $l == 0 ? 1 : int(($l + $nchunks - 1) / $nchunks);
    return ($nchunks, $k);
}
sub encode_message {
    my ($msg, $np) = @_;
    my $l = length($msg);
    my ($nchunks, $k) = _chunking($l, $np);
    my $hdr = pack('C*', ($l >> 24) & 255, ($l >> 16) & 255, ($l >> 8) & 255, $l & 255, $np);
    my $out = rs_encode($hdr, HDR_PARITY);
    my @cws;
    for my $c (0 .. $nchunks - 1) {
        my $block = substr($msg, $c * $k, $k);
        $block .= "\0" x ($k - length($block)) if length($block) < $k;
        push @cws, rs_encode($block, $np);
    }
    return $out . interleave(@cws);
}
sub decode_message {
    my ($blob) = @_;
    die "fec: short blob" if length($blob) < HDR_LEN;
    my ($hdr) = rs_decode(substr($blob, 0, HDR_LEN), HDR_PARITY, 5);
    my @h = unpack('C*', $hdr);
    my $l = ($h[0] << 24) | ($h[1] << 16) | ($h[2] << 8) | $h[3];
    my $np = $h[4];
    my ($nchunks, $k) = _chunking($l, $np);
    my $cwlen = $k + $np;
    my $body = substr($blob, HDR_LEN);
    die "fec: blob length mismatch" if length($body) != $nchunks * $cwlen;
    my @cws = deinterleave($body, $nchunks, $cwlen);
    my $out = '';
    my $total = 0;
    for my $cw (@cws) { my ($block, $nc) = rs_decode($cw, $np, $k); $total += $nc; $out .= $block; }
    $out = substr($out, 0, $l) if $l < length($out);
    return ($out, $total);
}

1;
