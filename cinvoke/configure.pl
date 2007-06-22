#!/usr/bin/perl
use IO::File;
use strict;

my $PREFIX = '/usr/local';
my @TARGETS = qw(Makefile lib/Makefile);

parse_args();

my $LINUX = 0;
my $SOLARIS = 0;

my $platform = determine_platform();
print "Using platform '$platform'\n";

my $BUILDARCH = '-DARCH_GCC_' . $platform;
my $CFLAGS = "-g -Wall -DCINVOKE_BUILD $BUILDARCH";
my $ARCH_HEADER = 'gcc_' . lc($platform) . '.h';
my $DYNEXT;
my $JNIDYNEXT;
my $JNIINCLUDE;
my $BUILDSHARED;
my $CXXBUILDSHARED;
my $BUILDSTATIC;
my $RANLIB;
my $DYNCFLAGS;
my $LIBDL;

# Solaris has an issue whereby all object files,
# even those compiled in static libraries, must
# have -fpic set if they will eventually be linked
# by shared libraries
if ($SOLARIS) {
	$CFLAGS .= " -fpic";
}

# Linux and OSX have libdl, *BSD doesn't
if ($LINUX || $platform =~ m/_OSX$/) {
	$LIBDL = '-ldl';
} else {
	$LIBDL = '';
}
# OSX build environment is all wonky
if ($platform =~ m/_OSX$/) {
	$DYNEXT = 'dylib';
	$JNIDYNEXT = 'jnilib';
	$JNIINCLUDE = '-I/System/Library/Frameworks/JavaVM.framework/Headers';
	$BUILDSHARED = 'libtool -dynamic -lc -o';
	$CXXBUILDSHARED = 'g++ -dynamiclib -o';
	$DYNCFLAGS = '-dynamic';
	$BUILDSTATIC = 'libtool -static -o';
	$RANLIB = 'echo';
} else {
	$DYNEXT = 'so';
	$JNIDYNEXT = 'so';
	my $jni_include_dir = find_jni();
	if ($jni_include_dir) {
		$JNIINCLUDE = "-I$jni_include_dir";
		if ($SOLARIS) { $JNIINCLUDE .= " -I$jni_include_dir/solaris"; }
		elsif ($LINUX) { $JNIINCLUDE .= " -I$jni_include_dir/linux"; }
	} else {
		$JNIINCLUDE = "";
	}
	$BUILDSHARED = 'gcc -shared -o';
	$CXXBUILDSHARED = 'g++ -shared -o';
	if ($SOLARIS) { $DYNCFLAGS = ''; }
	else { $DYNCFLAGS = '-fpic'; }
	$BUILDSTATIC = 'rm -f $(TARGET) && ar rs';
	$RANLIB = 'ranlib';
}

my $ARCH_SOURCE = $ARCH_HEADER;
my $ARCH_OBJ = $ARCH_HEADER;
$ARCH_SOURCE =~ s/\.h$/.c/;
$ARCH_OBJ =~ s/\.h$/.o/;

foreach my $target (@TARGETS) {
	process($target);
}

print "Complete.\n";
exit;

sub find_jni {
	my $ret = '/usr/jdk/default/include';
	if (!(-d $ret)) {
		$ret = '/usr/java/default/include';
		if (!(-d $ret)) {
			$ret = '';
		}
	}
	return $ret;
}

sub determine_platform {
	my $processor;
	
	my $gccout = `gcc -dumpmachine`;
	if ($? != 0) { die "error executing gcc -dumpmachine: $!"; }
	if ($gccout =~ m/i[3456]86/) {
		$processor = 'X86';
	} elsif ($gccout =~ m/x86_64/) {
		$processor = 'X64';
	} elsif ($gccout =~ m/ppc/) {
		$processor = 'PPC';
	} elsif ($gccout =~ m/sparc/) {
		$processor = 'SPARC';
	} else {
		die "unrecognized architecture $gccout";
	}

	my $os;

	my $uname = `uname`;
	if ($? != 0) { die "error executing uname: $!"; }
	if ($uname =~ m/Darwin/) {
		$os = "OSX";
	} else {
		$os = "UNIX"; # hey why not

		# specific unices
		if ($uname =~ m/Linux/) {
			$LINUX = 1;
		} elsif ($uname =~ m/SunOS/) {
			$SOLARIS = 1;
		}
	}

	return $processor . '_' . $os;
}

sub process {
	my ($ofn) = @_;

	print "Writing $ofn...\n";

	my $fn = $ofn . ".templ";
	my $input = new IO::File("<$fn") or die "cannot open $fn: $!";
	my $output = new IO::File(">$ofn") or die "cannot open $ofn: $!";

	while (<$input>) {
		$_ =~ s/\\/\\\\/g;
		$_ =~ s/"/\\"/g;
		$_ =~ s/\$/\\\$/g;
		$_ =~ s/\@/\\\@/g;
		$_ =~ s/\[([A-Za-z_][A-Za-z_0-9]*)\]/\$$1/g;
		eval("print \$output \"$_\"");
	}
	$input->close();
	$output->close();
}

sub parse_args {
	foreach my $arg (@ARGV) {
		if ($arg eq "--distclean") {
			my $targets = join(" ", @TARGETS);
			system("rm -f $targets");
			exit;
		} elsif ($arg =~ m/^--prefix=/) {
			$PREFIX = substr($arg, 9);
		} else {
			print <<EOF;
usage: configure.pl [--prefix=<path>]
       configure.pl --distclean
EOF
			exit;
		}
	}
}
