#!/usr/bin/env perl

use common::sense;
use autodie;
use Data::Dump 'dump';
use File::Basename qw(basename dirname);
use File::Find::Rule;
use File::Spec::Functions;
use File::Path 'make_path';
use List::Util 'max';
use List::MoreUtils 'uniq';
use Getopt::Long;
use Pod::Usage;
use Readonly;

main();

# --------------------------------------------------
sub main {
    my $in_dir  = '';
    my $out_dir = '';
    my ($help, $man_page);
    GetOptions(
        'in=s'  => \$in_dir,
        'out=s' => \$out_dir,
        'help'  => \$help,
        'man'   => \$man_page,
    ) or pod2usage(2);

    if ($help || $man_page) {
        pod2usage({
            -exitval => 0,
            -verbose => $man_page ? 2 : 1
        });
    }; 

    unless ($in_dir && -d $in_dir) {
        pod2usage('Bad or missing input directory');
    }

    unless (-d $out_dir) {
        make_path($out_dir);
    }

    say "Search for files in directory '$in_dir'";
    my @files = File::Find::Rule->file()->in($in_dir);

    unless (@files) {
        die "No files found\n";
    }

    my $nfile = 1;
    my %uniq  = map { $_, $nfile++ } uniq(map { basename($_) } @files);
    my %copy  = %uniq;
    my @ordered_fnums = sort { $a <=> $b } values %uniq;

    printf "Found %s files, %s unique.\n", scalar @files, scalar keys %uniq;

    open my $meta_fh, '>', catfile($out_dir, 'meta');
    my $i;
    while (my ($file, $file_num) = each %uniq) {
        my %read_matrix;

        printf "%5d: %s\n", ++$i, $file;

        say $meta_fh join("\t", $file, $file_num);

        while (my ($other_file, $other_file_num) = each %copy) {
            next if $other_file eq $file;
            my $path = catdir($in_dir, $other_file, $file);
            next unless -e $path;

            open my $fh, '<', $path;

            while (my $line = <$fh>) {
                chomp($line);
                my ($read_id, $mode) = split("\t", $line);
                $read_matrix{ $read_id }{ $other_file_num } = $mode;
            }

            close $fh;
        }

        my $out_file = catfile($out_dir, $file . '.csv');
        open my $out_fh, '>', $out_file;

        my $max_read_id = max(keys %read_matrix);
        for my $read_id (1 .. $max_read_id) {
            my @vals = map { $read_matrix{ $read_id }{ $_ } ? 1 : 0 } 
                       @ordered_fnums;
            say $out_fh join(',', $read_id, @vals); 
        }

        close $out_fh;
    }
    close $meta_fh;

    say "Done.";
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
      B.txt
      C.txt
    B/
      A.txt
      C.txt
    C/
      A.txt
      B.txt

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
