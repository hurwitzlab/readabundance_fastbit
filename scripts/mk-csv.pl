#!/usr/bin/env perl

use common::sense;
use autodie;
use Bit::Vector;
use Data::Dump 'dump';
use Hurwitz::Utils qw'commify timer_calc';
use File::Basename qw'basename dirname';
use File::Find::Rule;
use File::Spec::Functions qw'catfile';
use File::Path 'make_path';
use List::Util 'max';
use List::MoreUtils qw'all uniq';
use Math::Round 'round';
use Getopt::Long 'GetOptions';
use Pod::Usage 'pod2usage';
use Readonly;

# --------------------------------------------------
package ModeFile;

use Moose;
use Moose::Util::TypeConstraints;

has filename     => (is => 'rw', isa => 'Str');
has fnum         => (is => 'rw', isa => 'Int'); 
has fh           => (is => 'rw', isa => 'FileHandle', lazy_build => 1);
has last_read_id => (is => 'rw', isa => 'Int', default => 0);
has last_mode    => (is => 'rw', isa => 'Int', default => 0); 
has is_exhausted => (is => 'rw', isa => 'Int', default => 0); 

# --------------------------------------------------
sub _build_fh {
    my $self = shift;
    open my $fh, '<', $self->filename;
    $self->fh($fh);
}

# --------------------------------------------------
sub mode {
    my $self = shift;
    my $mode = shift // 0;

    return ($self->fnum, $mode);
}

# --------------------------------------------------
sub find {
    my ($self, $read_id) = @_;

    if ($read_id == $self->last_read_id) {
        return $self->mode($self->last_mode);
    }
    elsif ($read_id < $self->last_read_id) {
        return $self->mode(0);
    }
    elsif ($read_id > $self->last_read_id) {
        if ($self->is_exhausted) {
            return $self->mode(0);
        }

        my $fh   = $self->fh;
        my $line = <$fh>;

        if (!defined $line) {
            $self->is_exhausted(1);
            return ($self->fnum, 0);
        }

        if ($line =~ /(\d+)\t(\d+)\n/) {
            my ($file_read_id, $mode) = ($1, $2);

            $self->last_read_id($file_read_id);
            $self->last_mode($mode);

            if ($read_id == $file_read_id) {
                return $self->mode($mode);
            }
            elsif ($file_read_id > $read_id) {
                return $self->mode(0);
            }
        }
        else {
            return $self->find($read_id);
        }
    }
}

__PACKAGE__->meta->make_immutable;

# --------------------------------------------------
package main;

main();

# --------------------------------------------------
sub main {
    my $in_dir   = '';
    my $in_file  = '';
    my $out_dir  = '';
    my $min_mode = 1;
    my ($help, $man_page);
    GetOptions(
        'file=s'  => \$in_file,
        'dir=s'   => \$in_dir,
        'out=s'   => \$out_dir,
        'm|min:i' => \$min_mode,
        'help'    => \$help,
        'man'     => \$man_page,
    ) or pod2usage(2);

    if ($help || $man_page) {
        pod2usage({
            -exitval => 0,
            -verbose => $man_page ? 2 : 1
        });
    }; 

    unless (-d $out_dir) {
        make_path($out_dir);
    }

    unless ($in_dir) {
        pod2usage("Missing input directory");
    }

    unless (-d $in_dir) {
        pod2usage("Bad input directory ($in_dir)");
    }

    say "Looking in '$in_dir'";
    my @files = File::Find::Rule->file()->in($in_dir);
    printf "Found %s files.\n", commify(scalar @files);

    unless (@files) {
        pod2usage("No input files found.");
    }

    if ($in_file) {
        unless (-e $in_file) {
            pod2usage("Bad input file ($in_file)");
        }

        say "Using input file '$in_file'";
    }

    my $timer = timer_calc();

    process(
        out_dir  => $out_dir,
        min_mode => $min_mode,
        files    => \@files,
    );

    printf "Finished in %s\n", $timer->();
}

# --------------------------------------------------
sub process {
    my %args     = @_;
    my $min_mode = $args{'min_mode'};
    my $out_dir  = $args{'out_dir'};
    my $files    = $args{'files'};
    my $file_ct  = 0;
    my %file_num = map {$_, ++$file_ct} sort(uniq(map {basename($_)} @$files));

    my $i;
    for my $fname (sort keys %file_num) {
        printf "%5d: %s", ++$i, $fname;
        my $timer    = timer_calc();
        my $cur_fnum = $file_num{ $fname };

        my @fhs;
        for my $path (grep { basename($_) eq $fname } @$files) {
            push @fhs, ModeFile->new(
                filename => $path, 
                fnum     => $file_num{ basename(dirname($path)) }
            );
        }

        open my $out_fh, '>', catfile($out_dir, $fname);

        my $next_read_id = 0;
        while (1) {
            $next_read_id++;
            my %vals = map { $_->find($next_read_id) } @fhs;

            last if all { $_->is_exhausted } @fhs;

            my @bin;
            for my $fnum (1 .. $file_ct) {
                my $val = 0;
                if ($fnum == $cur_fnum) {
                    $val = 1;
                }
                else {
                    $val = $vals{ $fnum } >= $min_mode ? 1 : 0 
                }

                push @bin, $val;
            }

            my $num_pos = grep { $_ > 0 } @bin;
            push @bin, round(($num_pos / scalar @bin) * 100);
            say $out_fh join(',', $next_read_id, @bin);
        }

        close $out_fh;
        say ' (', $timer->(), ')';
    }
}

__END__

# --------------------------------------------------

=pod

=head1 NAME

mk-csv.pl - create the CSV files for Fastbit

=head1 SYNOPSIS

  mk-csv.pl -i input_dir -o output_dir

Options:

  -i|--in   Input directory
  -o|--out  Ouput directory
  --help    Show brief help and exit
  --man     Show full documentation

=head1 DESCRIPTION

Expects "in" directory to contain directories that each contain 
tab-delimited files with read id/mode values, e.g.:

  in/
    A/
      B
      C
    B/
      A
      C
    C/
      A
      B

  cat A/B
  1   4
  2   0
  3   1

The "out" directory will have CSV files in directories with the same 
names as the files in the "in" directory that contain a complete 
matrix of every read's mode to every other file.  There will also be a 
"meta" file showing the file numbers (1, 2, ...) to their file names
that will correspond to the "f1," "f2," etc., column names for Fastbit.

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
