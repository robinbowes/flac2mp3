# ParallelLoop.pm
#

# This module provides a way to easily write for loops and foreach loops
# that run with a controlled degree of parallelism.  One very nice feature
# is that bufferring is used when necessary such that the output from STDERR
# and STDOUT looks exactly as if it was produced by running your subroutine
# on each parameter in plain old sequential fashion.
#
# The degree of parallelism defaults to 5.  No more than that many
# subprocesses will be allowed to run at any time.  The default can be
# overridden by setting {"Max_Workers"=>n} after a loop body.

# There are two interfaces to this package: pardo and pareach.  The first
# approximates the semantics of a typical for loop.  pareach is more like
# a typical foreach loop in Perl.  (Actually, for and foreach are synonyms
# in Perl, so I emphasize "typical" because they're usually used as if they
# have different semantics.)

# You pass pardo() three args: a loop test, an update function, and
# a loop body.  It behaves mostly like a for loop but be careful that your
# loop test and update functions don't assume sequential execution.
#
# For example:
#
#    for (my $i=0; $i<100; $i++) {
#       ...
#    }
#
# Can be parallelized as:
#
#    { my $i=0; pardo sub{ $i<100 }, sub{ $i++ }, sub{
#       ...
#    };}

# You pass pareach() two args: A subroutine reference and an array of
# parameters.  The subroutine will be called once for each item in the
# array, with the item passed as the arg.
#
# For example:
#
#    foreach my $i ( @stuff ) {
#       ...
#    }
#
# Can be parallelized as:
#
#    pareach [ @stuff ], sub{
#       my $i=shift;
#       ...
#    };
#

# Copyright (c) 2002 Byron C. Darrah.  All rights reserved. This program
# is free software; you can redistribute it and/or modify it under the
# same terms as Perl itself. 

# By Byron Darrah  10/06/2000
#                  11/17/2000 -- Added serialization of IO
#                  07/16/2001 -- Added exporting of symbols

package Proc::ParallelLoop;
use strict;
use POSIX ":sys_wait_h";
use POSIX;
use Fcntl;

# Change this if you really want to alter the default behavior.
$Proc::ParallelLoop::DEFAULT_MAX_WORKERS = 5;

BEGIN {
	use Exporter ();
	use vars qw ($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
	$VERSION     = 0.5;
	@ISA         = qw (Exporter);
	#Give a hoot don't pollute, do not export more than needed by default
	@EXPORT      = qw (&pardo &pareach);
	@EXPORT_OK   = qw ();
	%EXPORT_TAGS = ();
}

###########################################################################
# Global Variables

use vars qw($Queue $Count $Handle_Count %Proc_Order %Dead_Procs
            %Proc_Out %Proc_Err %Proc_Handles %Proc_Handle_Num
            @Exit_Status @Reusable_Handles $Readset $Max_Workers);

###########################################################################
# Subroutine prototypes

sub pardo(&&&;$);
sub pareach($&;$);
sub init_state();
sub wait_for_available_queue($);
sub make_names();
sub dispatch($$);
sub cleanup_worker($);
sub reclaim_worker_io($);
sub check_for_death();
sub handle_event();
sub gather_all_output();
sub gather_proc_output($);
sub wait_for_all_jobs_to_finish();

########################################### main pod documentation begin ##
# Below is the documentation for ParallelLoop

=head1 NAME

Proc::ParallelLoop - Parallel looping constructs for Perl programs

=head1 SYNOPSIS

  use Proc::ParallelLoop

  pardo sub{loop_test}, sub{loop_update}, sub{
     loop_body
  };

  pareach array_ref, sub{
     loop_body
  };


=head1 DESCRIPTION

This module provides a way to easily write for loops and foreach loops
that run with a controlled degree of parallelism.  One very nice feature
is that bufferring is used when necessary such that the output from STDERR
and STDOUT looks exactly as if it was produced by running your subroutine
on each parameter in plain old sequential fashion.  Return status from
each loop iteration is also preserved.


=head1 USAGE

The degree of parallelism defaults to 5.  No more than that many
subprocesses will be allowed to run at any time.  The default can be
overridden by setting {"Max_Workers"=>n} after a loop body.

There are two interfaces to this package: pardo and pareach.  The first
approximates the semantics of a typical for loop.  pareach is more like
a typical foreach loop in Perl.  (Actually, for and foreach are synonyms
in Perl, so I emphasize "typical" because they're usually used as if they
have different semantics.)

=head1 LOOP CONTROL

The Perl keywords "next" and "last" do not work inside a pardo loop.
However, can simulate a "next" statement by using a "return"
or "exit n" statement instead.   "exit n" will end the current loop
iteration, and the value of integer n will be preserved for possible
use outside of the pardo loop.  "return" has the same effect as
"exit 0".

There is no approximation for "last", since it would not really make
sense in the context of parallel loop iterations.


=head1 BUGS

Signal handlers in Perl are documented to be unreliable.
Proc::ParallelLoop avoids relying on signals by making the assumption
that a child process closing its output descriptors means the child is
finished, and that an IO event will be observable via select when this
happens.  It remains to be seen whether this will turn out to be a
more reliable approach, though it seems to be holding up so far.

=head1 AUTHOR

	Byron C. Darrah
	bdarrah@pacbell.net

=head1 COPYRIGHT

Copyright (c) 2002 Byron C. Darrah. All rights reserved.
This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.

=head1 SEE ALSO

perl(1).

=head1 PUBLIC METHODS

You pass pardo() three args: a loop test, an update function, and
a loop body.  It behaves mostly like a for loop but be careful that your
loop test and update functions don't assume sequential execution.

For example:

   for (my $i=0; $i<100; $i++) {
      ...
   }

can be parallelized as:

   { my $i=0; pardo sub{ $i<100 }, sub{ $i++ }, sub{
      ...
   };}

You pass pareach() two args: A subroutine reference and an array of
parameters.  The subroutine will be called once for each item in the
array, with the item passed as the arg.

For example:

   foreach my $i ( @stuff ) {
      ...
   }

can be parallelized as:

   pareach [ @stuff ], sub{
      my $i=shift;
      ...
   };

Both pardo and pareach return an array containing the return statuses of each
iteration of the loop body, in order as if the loop had been executed
sequentially.

=cut

############################################# main pod documentation end ##
# Public methods follow.

###########################################################################
# pardo -- Like a for loop, but with parallelization.

sub pardo(&&&;$) {
   my ($test, $update, $routine, $options) = (shift, shift, shift, shift);
   my $result;
   init_state();
   if (defined $options->{"Max_Workers"}) {
      $Max_Workers=$options->{"Max_Workers"};
   }

   # Turn off buffering;
   my $oldfh = select(STDERR); $| = 1;
   select($oldfh);             $| = 1;

   for ( ; $result=&$test; &$update ) { dispatch($routine, $result); }
   wait_for_all_jobs_to_finish();

   return(@Exit_Status);
}

###########################################################################
# pareach -- like a foreach loop, but with parallelization.

sub pareach($&;$) {
   my ($list, $routine, $options) = (shift, shift, shift);
   my $i;
   init_state();
   if (defined $options->{"Max_Workers"}) {
      $Max_Workers=$options->{"Max_Workers"};
   }

   # Turn off buffering;
   my $oldfh = select(STDERR); $| = 1;
   select($oldfh);             $| = 1;

   foreach $i (@$list) { dispatch($routine, $i); }
   wait_for_all_jobs_to_finish();

   return(@Exit_Status);
}

########################################### main pod documentation begin ##

=head1 PRIVATE METHODS

And of course, here are all the methods you should never call.

=cut

############################################# main pod documentation end ##

# Private methods and functions follow.

###########################################################################
################################################ subroutine header begin ##

=head2 wait_for_all_jobs_to_finish

 Usage     : wait_for_all_jobs_to_finish()
 Purpose   : Wait for pending jobs to finish.
 Returns   : N/A.
 Argument  : None.
 Throws    : No exceptions.
 Comments  : Call this just before returning from a pardo-like loop.

=cut

################################################## subroutine header end ##

sub wait_for_all_jobs_to_finish() {
   while ($Queue > 0) { wait_for_available_queue($Max_Workers); }
}

###########################################################################
################################################ subroutine header begin ##

=head2 init_state

 Usage     : init_state()
 Purpose   : Initialize global loop state.
 Returns   : N/A.
 Argument  : None.
 Throws    : No exceptions.
 Comments  : Note that even though pardo loops may nest, or be used by
           : modules that know nothing of each other, it is safe to
           : use global variables to store the loop state, because:
           :    1.  pardo is a synchronous function which does not
           :        return until it no longer needs the state
           :        information.
           :    2.  Child processes do not depend on the state
           :        variables.
           :    3.  ParallelLoop is not recursive and even if the outer
           :        program calling it is, each pardo task executes in an
           :        isolated subprocess.
           : Of course, pardo is not re-entrant or thread-safe, but if
           : you are doing anything in Perl that could try to invoke
           : pardo from a signal handler or a (non-process) thread,
           : you probably need to see the BOFH about increasing your
           : disk quota.

=cut

################################################## subroutine header end ##

sub init_state() {
   $Queue            = 0;
   $Count            = 0;
   $Handle_Count     = 0;
   @Reusable_Handles = ();  # Generated symbols that can be reused.
   %Proc_Order       = ();  # Active processes and the order they were started.
   %Dead_Procs       = ();  # Dead processes with possible output pending.
   %Proc_Out         = ();  # Standard output from active processes.
   %Proc_Err         = ();  # Standard error from active processes.
   %Proc_Handles     = ();  # IO Handles for active processes.
   %Proc_Handle_Num  = ();  # IO Handle counts for active processes.
   @Exit_Status      = ();  # Exit status for finished processes.
   $Readset          = '';  # Set of file handles we like to use with select(2).
   $Max_Workers      = $Proc::ParallelLoop::DEFAULT_MAX_WORKERS;
}

###########################################################################
################################################ subroutine header begin ##

=head2 dispatch

 Usage     : dispatch($subroutine, $parm)
 Purpose   : Assign a worker process to execute a loop body.
 Returns   : N/A.
 Argument  : A subroutine representing a loop body, and a parameter to
           : be passed to the loop body as $_[0].
 Throws    : No exceptions.
 Comments  : If a loop body throws an exception, it will go uncaught.

=cut

################################################## subroutine header end ##

# Run a routine in parallel (in a child process without waiting).
sub dispatch($$) {
   my ($routine, $parm) = (shift, shift);

   # Wait for the work queue to have an available slot.
   wait_for_available_queue(1);

   # Dork some file handles.
   my ($new_err_read, $new_err_write, $new_std_read, $hnum) = make_names();
   no strict qw(refs);
   pipe $new_err_read, $new_err_write;

   open(OLD_ERR, ">&STDERR");             # Save original STDERR.
   print OLD_ERR "";                      # Avoid perl -w warning.
   open(STDERR, ">&" . $new_err_write);   # Dup $new_err_write to STDERR
   my $oldfh = select(STDERR); $| = 1;
   select($oldfh);

   # Fork a process to run the job!
   my $kid = open($new_std_read, "-|");
   if ( !defined $kid or $kid < 0 ) {
      die "ERROR: ParallelLoop::dispatch(): unable to fork.\n";
   }
   if ( $kid == 0 ) {
      eval { &$routine($parm) };
      warn $@ if $@;
      exit;
   }
   open(STDERR, ">&OLD_ERR");             # Return STDERR to original state.
   close $new_err_write;

   $Proc_Out{"$kid"}                     = "";
   $Proc_Err{"$kid"}                     = "";
   $Proc_Handles{"$kid"}                 = [ $new_std_read, $new_err_read ];
   $Proc_Handle_Num{"$kid"}              = $hnum;
   vec($Readset,fileno($new_err_read),1) = 1;
   vec($Readset,fileno($new_std_read),1) = 1;
   
   # Set read handles to non-blocking.
   fcntl($new_err_read, F_SETFL, &POSIX::O_NONBLOCK);
   fcntl($new_std_read, F_SETFL, &POSIX::O_NONBLOCK);
   use strict qw(refs);

   $Proc_Order{"$kid"}                   = $Count;
   $Count++;
   $Queue++;
}

###########################################################################
################################################ subroutine header begin ##

=head2 wait_for_available_queue

 Usage     : wait_for_available_queue($slots)
 Purpose   : Sleep until we are allowed to start a new subprocess.
 Returns   : N/A.
 Argument  : Number of queue slots that must be available before returning.
 Throws    : No exceptions.

=cut

################################################## subroutine header end ##

sub wait_for_available_queue($) {
   my $slots = shift;
   my $kid;

   check_for_death();
   gather_all_output();

   # Sleep until the requested number of slots are available.
   while ($Queue + $slots > $Max_Workers) {
      # Wait for something to happen (child dies or output is available).
      handle_event();
   }

}

###########################################################################
################################################ subroutine header begin ##

=head2 check_for_death()

 Usage     : check_for_death()
 Purpose   : Nonblocking check and handling for death of any worker
           : process.
 Returns   : N/A.
 Argument  : Nothing.
 Throws    : No exceptions.

See Also   : waitpid

=cut

################################################## subroutine header end ##

sub check_for_death() {
   my $kid;
   do {
      $kid = waitpid(-1,&POSIX::WNOHANG);
      if ($kid > 0) { cleanup_worker($kid) }
   } until $kid < 1;
}

###########################################################################
################################################ subroutine header begin ##

=head2 handle_event()

 Usage     : handle_event()
 Purpose   : Wait for a child to die or for output to be available.
 Returns   : N/A.
 Argument  : None.
 Throws    : No exceptions.
 Comments  : Makes the assumption that child process death will cause
           : an IO event on that process's output descriptors.

=cut

################################################## subroutine header end ##

sub handle_event() {
   my $rin = $Readset;
   my $ein = $Readset;
   my $nfound;
   $nfound = select($rin, undef, $ein, undef);
   check_for_death();
   if ($rin) { gather_all_output() }
}

###########################################################################
################################################ subroutine header begin ##

=head2 cleanup_worker

 Usage     : cleanup_worker($worker_index)
 Purpose   : Clean up after a worker has been reaped.
 Returns   : N/A.
 Argument  : The index of the Proc_Order and other hashes.
 Throws    : No exceptions.

=cut

################################################## subroutine header end ##

sub cleanup_worker($) {
   my $kid = shift;

   if (defined $Proc_Order{"$kid"}) {
      $Queue--;
      $Exit_Status[$Proc_Order{"$kid"}] = $?/256;

      # Gather any last words from the dead worker.
      gather_proc_output($kid);

      # We don't need to watch the worker's output any more.
      no strict qw(refs);
      my ($std_read, $err_read) = @{$Proc_Handles{"$kid"}};
      vec($Readset,fileno($std_read),1) = 0;
      vec($Readset,fileno($err_read),1) = 0;
      use strict qw(refs);

      # Add the process to the dead list for output collection.
      $Dead_Procs{"$kid"} = 1;
   }
}

###########################################################################
################################################ subroutine header begin ##

=head2 reclaim_worker_io

 Usage     : reclaim_worker_io($worker_index)
 Purpose   : Reclaim resources no longer needed for a long-dead worker process.
 Returns   : N/A.
 Argument  : Index of the dead worker in Proc_Order and other hashes.
 Throws    : No exceptions.

=cut

################################################## subroutine header end ##

sub reclaim_worker_io($) {
   my $kid = shift;

   my ($std_read, $err_read) = @{$Proc_Handles{"$kid"}};
   no strict qw(refs);
   close $std_read;
   close $err_read;
   use strict qw(refs);

   delete $Proc_Order{"$kid"};
   delete $Dead_Procs{"$kid"};

   # Clean up IO on dead processes.
   push @Reusable_Handles, ($Proc_Handle_Num{"$kid"});

   # Nice but not strictly needed.
   delete $Proc_Handles{"$kid"};
   delete $Proc_Handle_Num{"$kid"};
   delete $Proc_Err{"$kid"};
   delete $Proc_Out{"$kid"};
}

###########################################################################
################################################ subroutine header begin ##

=head2 gather_all_output

 Usage     : gather_all_output()
 Purpose   : Gather any output that may have been produced by child
           : processes and flush the output buffers of the current
           : process.
 Returns   : N/A.
 Argument  : None.
 Throws    : No exceptions.

=cut

################################################## subroutine header end ##

sub gather_all_output() {

   my ($kid, $deadkid);
   my $current_proc = -1;
   my %death_flush  = ();

   # Gather output from active processes.
   foreach $kid ( keys %Proc_Order ) {
      if (defined $Dead_Procs{"$kid"}) { next }  # Skip dead processes
      gather_proc_output($kid);
      if ($current_proc < 0 or
                     $Proc_Order{"$kid"} < $Proc_Order{"$current_proc"}) {
         $current_proc = $kid;
      }
   }

   # Now gather output from dead processes.
   foreach $kid ( keys %Dead_Procs ) {
      if ($current_proc < 0 or
             $Proc_Order{"$kid"} < $Proc_Order{"$current_proc"}) {
         $death_flush{$Proc_Order{"$kid"}} = "$kid";
      }
   }

   # Flush any dead processes that are in line, and reclaim their resources.
   foreach $deadkid (sort {$a <=> $b} keys %death_flush) {
      print        $Proc_Out{$death_flush{"$deadkid"}};
      print STDERR $Proc_Err{$death_flush{"$deadkid"}};
      reclaim_worker_io($death_flush{"$deadkid"});
   }

   # Flush the output buffer of the current active process.
   if ($current_proc < 0) { return }
   print        $Proc_Out{"$current_proc"};  $Proc_Out{"$current_proc"} = '';
   print STDERR $Proc_Err{"$current_proc"};  $Proc_Err{"$current_proc"} = '';

}

###########################################################################
################################################ subroutine header begin ##

=head2 gather_proc_output

 Usage     : gather_proc_output($worker_index)
 Purpose   : Collect error and standard output from a worker process.
 Returns   : N/A.
 Argument  : Index of a worker process in Proc_Order and other hashes.
 Throws    : No exceptions.

=cut

################################################## subroutine header end ##

sub gather_proc_output($) {
   my ($kid) = shift;
   my ($std_read, $err_read) = @{$Proc_Handles{"$kid"}};
   my ($b, $result);

   no strict qw(refs);
   $result = sysread($err_read, $b, 1);
   while( defined $result and $result > 0 ) {
      $Proc_Err{"$kid"} .= $b;
      $result = sysread($err_read, $b, 1);
   }

   $result = sysread($std_read, $b, 1);
   while( defined $result and $result > 0 ) {
      $Proc_Out{"$kid"} .= $b;
      $result = sysread($std_read, $b, 1);
   }
   use strict qw(refs);
}

###########################################################################
################################################ subroutine header begin ##

=head2 make_names

 Usage     : make_names()
 Purpose   : Make up some names for use as file handles.
 Returns   : A list of three names.
 Argument  : N/A.
 Throws    : No exceptions.
 Comments  : Reuse reclaimed names when possible, so we don't bloat
           : the symbol table needlessly.

=cut

################################################## subroutine header end ##

sub make_names() {
   my $num;

   if (@Reusable_Handles > 0) { $num = shift @Reusable_Handles }
   else                       { $Handle_Count++ ; $num = $Handle_Count }
   return ("ER" . $num, "EW" . $num, "SR" . $num, $num);
}

###############################################################################
1; #this line is important and will help the module return a true value
__END__
# EOF: ParallelLoop.pm
