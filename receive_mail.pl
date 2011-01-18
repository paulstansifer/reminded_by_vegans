#!/usr/bin/perl
use DBI;
use Email::Address;
use strict; 

# connect
my $dbh = DBI->connect("DBI:Pg:db=rbv", "vegans", "local_login_password", 
     { RaiseError => 1, AutoCommit => 1 });




my $headers = "";
my $body;
my $from_address;
my $original_to;
my $nominal_dt;
my $auto_submitted = "false";
my $subject;

#TODO: what about MIME messages?

`echo "--==**==--" >> /home/paul/src/reminded_by_vegans/foobar`;
while(<>) {
    
    `echo "$_" >> /home/paul/src/reminded_by_vegans/foobar`; #TEMPORARY I HOPE HOPE HOPE

    last if($_ eq "\n");

    if(/^From: (.*)/) {
        my @addresses = Email::Address->parse($1);
        $from_address = $addresses[0]->address;
    } elsif(/^X-Original-To: (.*)/) {  #this is probably tied to Postfix
        $original_to = $1;
    } elsif(/^Date: \D*(\d?\d)\s+(\S\S\S)\s+(\d\d\d\d)\s+(\d\d:\d\d:\d\d)\s+([+-]\d\d\d\d)/) { 
        $nominal_dt = "$2 $1 $4 $3 $5";
        `echo "got date: $nominal_dt" >> /home/paul/src/reminded_by_vegans/foobar`;
    } elsif(/^Auto-Submitted:/) { #we may only want to reject auto-replied mail
        # http://www.iana.org/assignments/auto-submitted-keywords/auto-submitted-keywords.xhtml
        $auto_submitted = "TRUE";
    } elsif(/^Subject: (.*)/) {
        $subject = $1;
    }
    $headers .= $_;
}

$body = join("", <>);


my $sth = $dbh->do("insert into inbox (subject, from_address, original_to, nominal_dt, auto_submitted, headers, body) values (?, ?, ?, ?, ?, ?, ?)", undef, $subject, $from_address, $original_to, $nominal_dt, $auto_submitted, $headers, $body);









# date-time       =       [ day-of-week "," ] date FWS time [CFWS]

# day-of-week     =       ([FWS] day-name) / obs-day-of-week

# day-name        =       "Mon" / "Tue" / "Wed" / "Thu" /
#                         "Fri" / "Sat" / "Sun"

# date            =       day month year

# year            =       4*DIGIT / obs-year

# month           =       (FWS month-name FWS) / obs-month

# month-name      =       "Jan" / "Feb" / "Mar" / "Apr" /
#                         "May" / "Jun" / "Jul" / "Aug" /
#                         "Sep" / "Oct" / "Nov" / "Dec"

# day             =       ([FWS] 1*2DIGIT) / obs-day

# time            =       time-of-day FWS zone

# time-of-day     =       hour ":" minute [ ":" second ]

# hour            =       2DIGIT / obs-hour

# minute          =       2DIGIT / obs-minute

# second          =       2DIGIT / obs-second

# zone            =       (( "+" / "-" ) 4DIGIT) / obs-zone
