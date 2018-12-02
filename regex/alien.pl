#! /usr/bin/perl

$n=<STDIN>;
while ($n--) {
	if(<STDIN> =~ /^[\_\.]\d+[a-zA-Z]*\_?$/){
		print "VALID\n";
	} else {
		print "INVALID\n";
	}
}
