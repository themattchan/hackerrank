#! /usr/bin/perl

$w8='(25[0-5]|2[0-4][\d]|[01]?[\d][\d]?)';
$ipv4="^($w8\.)\{3\}$w8\$";

$hex='[0-9a-fA-F]';
$ipv6="^($hex\{1,4\}:)\{7\}$hex\{1,4\}\$";

$n=<STDIN>;
while ($n--) {
	$input=<STDIN>;
	if ($input =~ /$ipv4/){
		print "IPv4\n";
	} elsif ($input =~ /$ipv6/) {
		print "IPv6\n";
	} else {
		print "Neither\n"
	}
}
