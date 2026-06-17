# SPDX-License-Identifier: LGPL-3.0-only
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME             => 'DCF::Frame',
    VERSION_FROM     => 'lib/DCF/Frame.pm',
    ABSTRACT         => 'DeMoD DCF 17-byte DeModFrame wire codec (certified Perl binding)',
    AUTHOR           => 'DeMoD LLC',
    LICENSE          => 'lgpl_3_0',
    MIN_PERL_VERSION => '5.010',
    PREREQ_PM        => {
        'JSON::PP'   => 0,
        'Test::More' => 0,
        'Exporter'   => 0,
    },
    test             => { TESTS => 't/*.t' },
);
