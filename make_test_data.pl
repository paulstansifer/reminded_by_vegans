#!/usr/bin/perl
use DBI;
# connect
my $dbh = DBI->connect("DBI:Pg:db=rbv", "vegans", "local_login_password", 
     { RaiseError => 1, AutoCommit => 1 });

$dbh->do("insert into usr (email) values (?)", undef, 'paul@saffron');


system "./generate_examples.sh";
