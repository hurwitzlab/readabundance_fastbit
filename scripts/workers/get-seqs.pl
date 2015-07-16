#!/usr/bin/env perl

use common::sense;
use autodie;
use Data::Dump 'dump';
use Getopt::Long;
use Pod::Usage;
use Readonly;

# --------------------------------------------------
package Fasta;

sub new {
    my $class = shift;
    my $file  = shift;
    open my $fh, '<', $file;
    my $obj  = { file => $file, fh => $fh };
    my $self = bless $obj, $class;
    $self->next(); # first "record" is empty
    return $self;
}

# --------------------------------------------------
sub next {
    my $self = shift;
    my $fh   = $self->{'fh'};

    local $/ = '>';

    chomp(my $rec = <$fh>);

    return $rec;
}

# --------------------------------------------------
package IdFile;

sub new {
    my $class = shift;
    my $file  = shift;
    open my $fh, '<', $file;
    my $obj  = { file => $file, fh => $fh };
    return bless $obj, $class;
}

# --------------------------------------------------
sub next {
    my $self = shift;
    my $fh   = $self->{'fh'};

    chomp(my $rec = <$fh>);

    return $rec;
}

# --------------------------------------------------
package main;

main();

# --------------------------------------------------
sub main {
    my $read_file = '';
    my $id_file   = '';
    my ($help, $man_page);
    GetOptions(
        'read=s' => \$read_file,
        'id=s'   => \$id_file,
        'help'   => \$help,
        'man'    => \$man_page,
    ) or pod2usage(2);

    if ($help || $man_page) {
        pod2usage({
            -exitval => 0,
            -verbose => $man_page ? 2 : 1
        });
    }; 

    unless ($read_file && -e $read_file) {
        pod2usage('Missing or bad read file');
    }

    unless ($id_file && -e $id_file) {
        pod2usage('No id file');
    }

    my $fasta   = Fasta->new($read_file);
    my $id_file = IdFile->new($id_file);
    my $id      = $id_file->next;
    my $cur_id  = 0;

    while (my $seq = $fasta->next) {
        $cur_id++;
        if ($cur_id == $id) {
            print ">$seq";
            $id = $id_file->next;
        }
    }
}

__END__

# --------------------------------------------------

=pod

=head1 NAME

get-seqs.pl - get the reads using a read id file

=head1 SYNOPSIS

  get-seqs.pl -r reads_file -i read_id_file

Options:

  --help   Show brief help and exit
  --man    Show full documentation

=head1 DESCRIPTION

The "reads" file should be a FASTA file.  The "read id" file should 
have the ordered read ids (position in the FASTA file).

=head1 SEE ALSO

perl.

=head1 AUTHOR

Ken Youens-Clark E<lt>kyclark@email.arizona.eduE<gt>.

=head1 COPYRIGHT

Copyright (c) 2015 Hurwitz Lab

This module is free software; you can redistribute it and/or
modify it under the terms of the GPL (either version 1, or at
your option, any later version) or the Artistic License 2.0.
Refer to LICENSE for the full license text and to DISCLAIMER for
additional warranty disclaimers.

=cut
