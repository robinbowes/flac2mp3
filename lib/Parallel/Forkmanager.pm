=head1 NAME

Parallel::ForkManager - A simple parallel processing fork manager

=head1 SYNOPSIS

  use Parallel::ForkManager;

  $pm = Parallel::ForkManager->new($MAX_PROCESSES);

  foreach $data (@all_data) {
    # Forks and returns the pid for the child:
    my $pid = $pm->start and next;

    ... do some work with $data in the child process ...

    $pm->finish; # Terminates the child process
  }

=head1 DESCRIPTION

This module is intended for use in operations that can be done in parallel
where the number of processes to be forked off should be limited. Typical
use is a downloader which will be retrieving hundreds/thousands of files.

The code for a downloader would look something like this:

  use LWP::Simple;
  use Parallel::ForkManager;

  ...

  @links=(
    ["http://www.foo.bar/rulez.data","rulez_data.txt"],
    ["http://new.host/more_data.doc","more_data.doc"],
    ...
  );

  ...

  # Max 30 processes for parallel download
  my $pm = Parallel::ForkManager->new(30);

  foreach my $linkarray (@links) {
    $pm->start and next; # do the fork

    my ($link,$fn) = @$linkarray;
    warn "Cannot get $fn from $link"
      if getstore($link,$fn) != RC_OK;

    $pm->finish; # do the exit in the child process
  }
  $pm->wait_all_children;

First you need to instantiate the ForkManager with the "new" constructor.
You must specify the maximum number of processes to be created. If you
specify 0, then NO fork will be done; this is good for debugging purposes.

Next, use $pm->start to do the fork. $pm returns 0 for the child process,
and child pid for the parent process (see also L<perlfunc(1p)/fork()>).
The "and next" skips the internal loop in the parent process. NOTE:
$pm->start dies if the fork fails.

$pm->finish terminates the child process (assuming a fork was done in the
"start").

NOTE: You cannot use $pm->start if you are already in the child process.
If you want to manage another set of subprocesses in the child process,
you must instantiate another Parallel::ForkManager object!

=head1 METHODS

The comment letter indicates where the method should be run. P for parent,
C for child.

=over 5

=item new $processes [, $tempdir]  # P

Instantiate a new Parallel::ForkManager object. You must specify the maximum
number of children to fork off. If you specify 0 (zero), then no children
will be forked. This is intended for debugging purposes.

The optional second parameter, $tempdir, is only used if you want the
children to send back a reference to some data (see RETRIEVING DATASTRUCTURES
below). If not provided, it is set to $L<File::Temp::tempdir().

The new method will die if the temporary directory does not exist or it is not
a directory, whether you provided this parameter or the
$L<File::Temp::tempdir() is used.

=item start [ $process_identifier ]  # P

This method does the fork. It returns the pid of the child process for
the parent, and 0 for the child process. If the $processes parameter
for the constructor is 0 then, assuming you're in the child process,
$pm->start simply returns 0.

An optional $process_identifier can be provided to this method... It is used by
the "run_on_finish" callback (see CALLBACKS) for identifying the finished
process.

=item finish [ $exit_code [, $data_structure_reference] ]  # C

Closes the child process by exiting and accepts an optional exit code
(default exit code is 0) which can be retrieved in the parent via callback.
If the second optional parameter is provided, the child attempts to send
it's contents back to the parent. If you use the program in debug mode
($processes == 0), this method just calls the callback.

If the $data_structure_reference is provided, then it is serialized and
passed to the parent process. See RETRIEVING DATASTRUCTURES for more info.

=item set_max_procs $processes  # P

Allows you to set a new maximum number of children to maintain.

=item wait_all_children  # P

You can call this method to wait for all the processes which have been
forked. This is a blocking wait.

=back

=head1 CALLBACKS

You can define callbacks in the code, which are called on events like starting
a process or upon finish. Declare these before the first call to start().

The callbacks can be defined with the following methods:

=over 4

=item run_on_finish $code [, $pid ]  # P

You can define a subroutine which is called when a child is terminated. It is
called in the parent process.

The paremeters of the $code are the following:

  - pid of the process, which is terminated
  - exit code of the program
  - identification of the process (if provided in the "start" method)
  - exit signal (0-127: signal name)
  - core dump (1 if there was core dump at exit)
  - datastructure reference or undef (see RETRIEVING DATASTRUCTURES)

=item run_on_start $code  # P

You can define a subroutine which is called when a child is started. It called
after the successful startup of a child in the parent process.

The parameters of the $code are the following:

  - pid of the process which has been started
  - identification of the process (if provided in the "start" method)

=item run_on_wait $code, [$period]  # P

You can define a subroutine which is called when the child process needs to wait
for the startup. If $period is not defined, then one call is done per
child. If $period is defined, then $code is called periodically and the
module waits for $period seconds betwen the two calls. Note, $period can be
fractional number also. The exact "$period seconds" is not guarranteed,
signals can shorten and the process scheduler can make it longer (on busy
systems).

The $code called in the "start" and the "wait_all_children" method also.

No parameters are passed to the $code on the call.

=back

=head1 RETRIEVING DATASTRUCTURES from child processes

The ability for the parent to retrieve data structures is new as of version
0.7.6.

Each child process may optionally send 1 data structure back to the parent.
By data structure, we mean a reference to a string, hash or array. The
contents of the data structure are written out to temporary files on disc
using the L<Storable> modules' store() method. The reference is then
retrieved from within the code you send to the run_on_finish callback.

The data structure can be any scalar perl data structure which makes sense:
string, numeric value or a reference to an array, hash or object.

There are 2 steps involved in retrieving data structures:

1) A reference to the data structure the child wishes to send back to the
parent is provided as the second argument to the finish() call. It is up
to the child to decide whether or not to send anything back to the parent.

2) The data structure reference is retrieved using the callback provided in
the run_on_finish() method.

Keep in mind that data structure retrieval is not the same as returning a
data structure from a method call. That is not what actually occurs. The
data structure referenced in a given child process is serialized and
written out to a file by L<Storable>. The file is subsequently read back
into memory and a new data structure belonging to the parent process is
created. Please consider the performance penality it can imply, so try to
keep the returned structure small.

=head1 EXAMPLES

=head2 Parallel get

This small example can be used to get URLs in parallel.

  use Parallel::ForkManager;
  use LWP::Simple;
  my $pm= Parallel::ForkManager->new(10);
  for my $link (@ARGV) {
    $pm->start and next;
    my ($fn)= $link =~ /^.*\/(.*?)$/;
    if (!$fn) {
      warn "Cannot determine filename from $fn\n";
    } else {
      $0.=" ".$fn;
      print "Getting $fn from $link\n";
      my $rc=getstore($link,$fn);
      print "$link downloaded. response code: $rc\n";
    };
    $pm->finish;
  };

=head2 Callbacks

Example of a program using callbacks to get child exit codes:

  use strict;
  use Parallel::ForkManager;

  my $max_procs = 5;
  my @names = qw( Fred Jim Lily Steve Jessica Bob Dave Christine Rico Sara );
  # hash to resolve PID's back to child specific information

  my $pm = Parallel::ForkManager->new($max_procs);

  # Setup a callback for when a child finishes up so we can
  # get it's exit code
  $pm->run_on_finish( sub {
      my ($pid, $exit_code, $ident) = @_;
      print "** $ident just got out of the pool ".
        "with PID $pid and exit code: $exit_code\n";
  });

  $pm->run_on_start( sub {
      my ($pid,$ident)=@_;
      print "** $ident started, pid: $pid\n";
  });

  $pm->run_on_wait( sub {
      print "** Have to wait for one children ...\n"
    },
    0.5
  );

  foreach my $child ( 0 .. $#names ) {
    my $pid = $pm->start($names[$child]) and next;

    # This code is the child process
    print "This is $names[$child], Child number $child\n";
    sleep ( 2 * $child );
    print "$names[$child], Child $child is about to get out...\n";
    sleep 1;
    $pm->finish($child); # pass an exit code to finish
  }

  print "Waiting for Children...\n";
  $pm->wait_all_children;
  print "Everybody is out of the pool!\n";

=head2 Data structure retrieval

In this simple example, each child sends back a string reference.

  use Parallel::ForkManager 0.7.6;
  use strict;
  
  my $pm = Parallel::ForkManager->new(2, '/server/path/to/temp/dir/');
  
  # data structure retrieval and handling
  $pm -> run_on_finish ( # called BEFORE the first call to start()
    sub {
      my ($pid, $exit_code, $ident, $exit_signal, $core_dump, $data_structure_reference) = @_;
  
      # retrieve data structure from child
      if (defined($data_structure_reference)) {  # children are not forced to send anything
        my $string = ${$data_structure_reference};  # child passed a string reference
        print "$string\n";
      } else {  # problems occuring during storage or retrieval will throw a warning
        print qq|No message received from child process $pid!\n|;
      }
    }
  );
  
  # prep random statement components
  my @foods = ('chocolate', 'ice cream', 'peanut butter', 'pickles', 'pizza', 'bacon', 'pancakes', 'spaghetti', 'cookies');
  my @preferences = ('loves', q|can't stand|, 'always wants more', 'will walk 100 miles for', 'only eats', 'would starve rather than eat');
  
  # run the parallel processes
  my $person = '';
  foreach $person (qw(Fred Wilma Ernie Bert Lucy Ethel Curly Moe Larry)) {
    $pm->start() and next;
  
    # generate a random statement about food preferences
    my $statement = $person . ' ' . $preferences[int(rand @preferences)] . ' ' . $foods[int(rand @foods)];
  
    # send it back to the parent process
    $pm->finish(0, \$statement);  # note that it's a scalar REFERENCE, not the scalar itself
  }
  $pm->wait_all_children;

A second datastructure retrieval example demonstrates how children decide
whether or not to send anything back, what to send and how the parent should
process whatever is retrieved.

=for example begin

  use Parallel::ForkManager 0.7.6;
  use Data::Dumper;  # to display the data structures retrieved.
  use strict;
  
  my $pm = Parallel::ForkManager->new(20);  # using the system temp dir $L<File::Temp::tempdir()
  
  # data structure retrieval and handling
  my %retrieved_responses = ();  # for collecting responses
  $pm -> run_on_finish (
    sub {
      my ($pid, $exit_code, $ident, $exit_signal, $core_dump, $data_structure_reference) = @_;
  
      # see what the child sent us, if anything
      if (defined($data_structure_reference)) {  # test rather than assume child sent anything
        my $reftype = ref($data_structure_reference);
        print qq|ident "$ident" returned a "$reftype" reference.\n\n|;
        if (1) {  # simple on/off switch to display the contents
          print &Dumper($data_structure_reference) . qq|end of "$ident" sent structure\n\n|;
        }
        
        # we can also collect retrieved data structures for processing after all children have exited
        $retrieved_responses{$ident} = $data_structure_reference;
      } else {
        print qq|ident "$ident" did not send anything.\n\n|;  
      }
    }
  );
  
  # generate a list of instructions
  my @instructions = (  # a unique identifier and what the child process should send
    {'name' => '%ENV keys as a string', 'send' => 'keys'},
    {'name' => 'Send Nothing'},  # not instructing the child to send anything back to the parent
    {'name' => 'Childs %ENV', 'send' => 'all'},
    {'name' => 'Child chooses randomly', 'send' => 'random'},
    {'name' => 'Invalid send instructions', 'send' => 'Na Na Nana Na'},
    {'name' => 'ENV values in an array', 'send' => 'values'},
  );
  
  my $instruction = '';
  foreach $instruction (@instructions) {
    $pm->start($instruction->{'name'}) and next;  # this time we are using an explicit, unique child process identifier
  
    # last step in child processing
    $pm->finish(0) unless $instruction->{'send'};  # no data structure is sent unless this child is told what to send.
    
    if ($instruction->{'send'} eq 'keys') {
      $pm->finish(0, \join(', ', keys %ENV));
      
    } elsif ($instruction->{'send'} eq 'values') {
      $pm->finish(0, [values %ENV]);  # kinda useless without knowing which keys they belong to...
      
    } elsif ($instruction->{'send'} eq 'all') {
      $pm->finish(0, \%ENV);  # remember, we are not "returning" anything, just copying the hash to disc
    
    # demonstrate clearly that the child determines what type of reference to send
    } elsif ($instruction->{'send'} eq 'random') {
      my $string = q|I'm just a string.|;
      my @array = qw(I am an array);
      my %hash = (type => 'associative array', synonym => 'hash', cool => 'very :)');
      my $return_choice = ('string', 'array', 'hash')[int(rand 3)];  # randomly choose return data type
      $pm->finish(0, \$string) if ($return_choice eq 'string');
      $pm->finish(0, \@array) if ($return_choice eq 'array');
      $pm->finish(0, \%hash) if ($return_choice eq 'hash');
      
    # as a responsible child, inform parent that their instruction was invalid
    } else {  
      $pm->finish(0, \qq|Invalid instructions: "$instruction->{'send'}".|);  # ordinarily I wouldn't include invalid input in a response...
    }
  }
  $pm->wait_all_children;  # blocks until all forked processes have exited
  
  # post fork processing of returned data structures
  for (sort keys %retrieved_responses) {
    print qq|Post processing "$_"...\n|;
  }

=for example end

=head1 BUGS AND LIMITATIONS

Do not use Parallel::ForkManager in an environment, where other child
processes can affect the run of the main program, so using this module
is not recommended in an environment where fork() / wait() is already used.

If you want to use more than one copies of the Parallel::ForkManager, then
you have to make sure that all children processes are terminated, before you
use the second object in the main program.

You are free to use a new copy of Parallel::ForkManager in the child
processes, although I don't think it makes sense.

=head1 COPYRIGHT

Copyright (c) 2000-2010 Szabó, Balázs (dLux)

All right reserved. This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=head1 AUTHOR

  dLux (Szabó, Balázs) <dlux@dlux.hu>

=head1 CREDITS

  Noah Robin <sitz@onastick.net> (documentation tweaks)
  Chuck Hirstius <chirstius@megapathdsl.net> (callback exit status, example)
  Grant Hopwood <hopwoodg@valero.com> (win32 port)
  Mark Southern <mark_southern@merck.com> (bugfix)
  Ken Clarke <www.perlprogrammer.net>  (datastructure retrieval)

=cut

package Parallel::ForkManager;
use POSIX ":sys_wait_h";
use Storable qw(store retrieve);
use File::Spec;
use File::Temp ();
use File::Path ();
use strict;
use vars qw($VERSION);
$VERSION="1.02";
$VERSION = eval $VERSION;

sub new {
  my ($c,$processes, $tempdir)=@_;

  my $h={
    max_proc   => $processes,
    processes  => {},
    in_child   => 0,
    parent_pid => $$,
  };
  
  # determine temporary directory for storing data structures
  # add it to Parallel::ForkManager object so children can use it
  # We don't let it clean up so it won't do it in the child process
  # but we have our own DESTROY to do that.
  $h->{tempdir} = File::Temp::tempdir(CLEANUP => 0);
  
  return bless($h,ref($c)||$c);
};

sub start {
  my ($s,$identification)=@_;

  die "Cannot start another process while you are in the child process"
    if $s->{in_child};
  while ($s->{max_proc} && ( keys %{ $s->{processes} } ) >= $s->{max_proc}) {
    $s->on_wait;
    $s->wait_one_child(defined $s->{on_wait_period} ? &WNOHANG : undef);
  };
  $s->wait_children;
  if ($s->{max_proc}) {
    my $pid=fork();
    die "Cannot fork: $!" if !defined $pid;
    if ($pid) {
      $s->{processes}->{$pid}=$identification;
      $s->on_start($pid,$identification);
    } else {
      $s->{in_child}=1 if !$pid;
    }
    return $pid;
  } else {
    $s->{processes}->{$$}=$identification;
    $s->on_start($$,$identification);
    return 0; # Simulating the child which returns 0
  }
}

sub finish {
  my ($s, $x, $r)=@_;

  if ( $s->{in_child} ) {
    if (defined($r)) {  # store the child's data structure
      my $storable_tempfile = File::Spec->catfile($s->{tempdir}, 'Parallel-ForkManager-' . $s->{parent_pid} . '-' . $$ . '.txt');
      my $stored = eval { return &store($r, $storable_tempfile); };

      # handle Storables errors, IE logcarp or carp returning undef, or die (via logcroak or croak)
      if (not $stored or $@) {
        warn(qq|The storable module was unable to store the child's data structure to the temp file "$storable_tempfile":  | . join(', ', $@));
      }
    }
    CORE::exit($x || 0);
  }
  if ($s->{max_proc} == 0) { # max_proc == 0
    $s->on_finish($$, $x ,$s->{processes}->{$$}, 0, 0, $r);
    delete $s->{processes}->{$$};
  }
  return 0;
}

sub wait_children {
  my ($s)=@_;

  return if !keys %{$s->{processes}};
  my $kid;
  do {
    $kid = $s->wait_one_child(&WNOHANG);
  } while $kid > 0 || $kid < -1; # AS 5.6/Win32 returns negative PIDs
};

*wait_childs=*wait_children; # compatibility

sub wait_one_child {
  my ($s,$par)=@_;

  my $kid;
  while (1) {
    $kid = $s->_waitpid(-1,$par||=0);
    last if $kid == 0 || $kid == -1; # AS 5.6/Win32 returns negative PIDs
    redo if !exists $s->{processes}->{$kid};
    my $id = delete $s->{processes}->{$kid};

    # retrieve child data structure, if any
    my $retrieved = undef;
    my $storable_tempfile = File::Spec->catfile($s->{tempdir}, 'Parallel-ForkManager-' . $$ . '-' . $kid . '.txt');
    if (-e $storable_tempfile) {  # child has option of not storing anything, so we need to see if it did or not
      $retrieved = eval { return &retrieve($storable_tempfile); };

      # handle Storables errors
      if (not $retrieved or $@) {
        warn(qq|The storable module was unable to retrieve the child's data structure from the temporary file "$storable_tempfile":  | . join(', ', $@));
      }
      
      # clean up after ourselves
      unlink $storable_tempfile;
    }

    $s->on_finish( $kid, $? >> 8 , $id, $? & 0x7f, $? & 0x80 ? 1 : 0, $retrieved);
    last;
  }
  $kid;
};

sub wait_all_children {
  my ($s)=@_;

  while (keys %{ $s->{processes} }) {
    $s->on_wait;
    $s->wait_one_child(defined $s->{on_wait_period} ? &WNOHANG : undef);
  };
}

*wait_all_childs=*wait_all_children; # compatibility;

sub run_on_finish {
  my ($s,$code,$pid)=@_;

  $s->{on_finish}->{$pid || 0}=$code;
}

sub on_finish {
  my ($s,$pid,@par)=@_;

  my $code=$s->{on_finish}->{$pid} || $s->{on_finish}->{0} or return 0;
  $code->($pid,@par);
};

sub run_on_wait {
  my ($s,$code, $period)=@_;

  $s->{on_wait}=$code;
  $s->{on_wait_period} = $period;
}

sub on_wait {
  my ($s)=@_;

  if(ref($s->{on_wait}) eq 'CODE') {
    $s->{on_wait}->();
    if (defined $s->{on_wait_period}) {
        local $SIG{CHLD} = sub { } if ! defined $SIG{CHLD};
        select undef, undef, undef, $s->{on_wait_period}
    };
  };
};

sub run_on_start {
  my ($s,$code)=@_;

  $s->{on_start}=$code;
}

sub on_start {
  my ($s,@par)=@_;

  $s->{on_start}->(@par) if ref($s->{on_start}) eq 'CODE';
};

sub set_max_procs {
  my ($s, $mp)=@_;

  $s->{max_proc} = $mp;
}

# OS dependant code follows...

sub _waitpid { # Call waitpid() in the standard Unix fashion.
  return waitpid($_[1],$_[2]);
}

# On ActiveState Perl 5.6/Win32 build 625, waitpid(-1, &WNOHANG) always
# blocks unless an actual PID other than -1 is given.
sub _NT_waitpid {
  my ($s, $pid, $par) = @_;

  if ($par == &WNOHANG) { # Need to nonblock on each of our PIDs in the pool.
    my @pids = keys %{ $s->{processes} };
    # Simulate -1 (no processes awaiting cleanup.)
    return -1 unless scalar(@pids);
    # Check each PID in the pool.
    my $kid;
    foreach $pid (@pids) {
      $kid = waitpid($pid, $par);
      return $kid if $kid != 0; # AS 5.6/Win32 returns negative PIDs.
    }
    return $kid;
  } else { # Normal waitpid() call.
    return waitpid($pid, $par);
  }
}

{
  local $^W = 0;
  if ($^O eq 'NT' or $^O eq 'MSWin32') {
    *_waitpid = \&_NT_waitpid;
  }
}

sub DESTROY {
  my ($self) = @_;

  if ($self->{parent_pid} == $$ && -d $self->{tempdir}) {
    File::Path::remove_tree($self->{tempdir});
  }
}

1;
