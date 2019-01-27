#!/usr/bin/perl

# Copyright (c) 2016-2018, Iurii Gribov
#
# Use of this source code is governed by MIT license that can be
# found in the LICENSE.txt file.

# A sloppy script which looks for violations
# of gating requirements (all registers wider than 4 bits
# must be clock-gated).

use strict;
use warnings;

use File::Find;
use bigint;

use Getopt::Long qw(GetOptions);
Getopt::Long::Configure qw(gnu_getopt);

use Data::Dumper;

{
  package Lexer;

  my $type_re = '(?:integer|logic|wire|reg|(real)?time|signed|real)';
  my $inout_re = '(?:input|output|inout)';
  my $param_re = '(?:parameter|localparam)';

  my %block_hash = (
    always => 1,
    assign => 1,
    initial => 1,
    generate => 1,
    parameter => 1,
    localparam => 1,
    function => 1,
    task => 1,
    module => 1
  );

  my @lines;
  my @peek_buf;
  my $cur_file;
  my $cur_line;
  my $cur_nest;
  my $cur_scope;
  my $verbose;

  sub init($$) {
    ($cur_file, $verbose) = @_;
    open FILE, $cur_file or die "unable to open $cur_file";
    @lines = <FILE>;
    close FILE;
    $cur_line = 1;
    $cur_nest = 0;
    $cur_scope = undef;
    @peek_buf = ();
  }

  sub done() {
    return $#lines < 0;
  }

  sub warn($$) {
    # TODO: avoid dups?
    my ($loc, $msg) = @_;
    print STDERR "warning: $loc->{file}:$loc->{line}: $msg\n";
  }

  sub is_type_tok($) {
    return $_[0]->{type} eq 'keyword' && $_[0]->{info} =~ /^(?:$type_re|$inout_re)$/;
  }

  sub is_param_tok($) {
    return $_[0]->{type} eq 'keyword' && $_[0]->{info} =~ /^$param_re$/;
  }

  sub is_start_of_block($) {
    return $_[0]->{type} eq 'keyword' && exists $block_hash{$_[0]->{info}};
  }

  sub nest() {
    return $cur_nest;
  }

  sub _tok();  # To make compiler happy...
  sub _tok() {
    # This code is intentionally simple.

    my $line;

    while(!$line) {
      return undef if(Lexer::done());

      $line = shift @lines;

      # Skip whites
      while(1) {
        next if($line =~ s/^[\s\r\n]+//m);
        next if($line =~ s/^\/\/.*//);
        next if($line =~ s/^\\$//);
        if($line =~ /^\/\*/) {
          while($line !~ s/^(.*?)\*\/// && !done()) {
            $line = shift @lines;
            ++$cur_line;
          }
          next;
        }
        last;
      }

      if($line) {
        last;
      } else {
        ++$cur_line;
      }
    }

    my $type;
    my $info = undef;
    my $nline = $cur_line;

    # Useful ref: http://www.verilog.com/VerilogBNF.html
    if($line =~ s/^([a-zA-Z_\$.][a-zA-Z_\$.\d]*)//) {
      $info = $1;
      # TODO: replace 'keyword' with name of keyword?
      $type = $info =~ /^(begin|end|case|endcase|if|else|$type_re|$inout_re|$param_re|genvar|(?:end)?module|(?:end)?generate|always|assign|defparam|initial|function|task|(pos|neg)edge)$/ ? 'keyword'
              : $info =~ /^\./ ? 'bind'
              : 'id';
      $info =~ s/^\.//;
    } elsif($line =~ s/^\\(\S+)\s//) {  # Escaped id
      $info = $1;
      $type = 'id';
    } elsif($line =~ s/^([0-9'][0-9'a-fohxzs_ .]*)//i) {  # TODO: make this less permissive?
      $info = $1;
      $info =~ s/ *$//;
      $type = 'number';
    } elsif($line =~ s/^#([0-9][0-9.]*)//) {
      $info = $1;
      $type = 'delay';
    } elsif($line =~ s/^(\*\*|<<<|>>>|<<|>>|<=|>=|==|!=|&&|\|\||[*;,#(){}=?:!<>~|&^+\-*%@\/\[\]])//) {
      $type = $1;
    } elsif($line =~ s/"((?:\\"|[^"])*)"//) {
      $info = $1;
      $info =~ s/\\"/"/g;
      $type = 'string';
    } elsif($line =~ s/`([a-zA-Z_0-9]+)//) {
      $info = $1;
      $type = 'macro';
    } else {
      Lexer::warn({ line => $cur_line, file => $cur_file }, "failed to recognize token '$line'");
      $line = '';
    }

    if($line) {
      unshift @lines, $line;
    } else {
      ++$cur_line;
    }


    my $info_ = defined $info ? ", $info" : '';

    return {
      type => $type,
      info => $info,
      file => $cur_file,
      line => $nline
    };
  }

  sub tok();  # To make compiler happy...
  sub tok() {
    my $l = @peek_buf ? shift @peek_buf : Lexer::_tok();

    if(defined $l) {
      my $info = defined $l->{info} ? " ($l->{info})" : "";
      print STDERR "$cur_file:$cur_line: token '$l->{type}'$info\n" if($verbose);

      if($l->{type} =~ /^[({\[]$/) {
        $cur_nest++;
        print STDERR "$cur_file:$cur_line: nesting is $cur_nest\n" if($verbose);
      } elsif($l->{type} =~ /^[)}\]]$/) {
        $cur_nest--;
        print STDERR "$cur_file:$cur_line: nesting is $cur_nest\n" if($verbose);
        if($cur_nest < 0) {
          Lexer::warn(Lexer::loc(), "negative nesting level");
        }
      }
    }

    return $l;
  }

  sub loc() {
    return @peek_buf ? $peek_buf[0] : { file => $cur_file, line => $cur_line };
  }

  sub peek() {
    return $peek_buf[0] if(@peek_buf);

    my $l = Lexer::_tok();
    unshift @peek_buf, $l;
    return $l;
  }

  sub expect {
    my $l = Lexer::peek();
    if(defined $l) {
      for(@_) {
        if($l->{type} eq $_) {
          Lexer::tok();
          return $l;
        }
      }
    }
    my $type = defined $l ? "$l->{type}" : "undef";
    print STDERR "$cur_file:$cur_line: expected token of type '@_', got '$type'\n";
    return undef;
  }

  sub maybe_skip($) {
    my $l = Lexer::peek();
    if($l->{type} eq $_[0]) {
      Lexer::tok();
    }
  }

  sub skip_until($$) {
    my ($type, $info) = @_;
    while(1) {
      my $l = Lexer::tok();
      return if(!defined $l);
      return if($l->{type} eq $type && (!defined $info || $l->{info} eq $info));
    }
  }
}

sub worn($;$) {
  my $loc;
  if(@_ > 1) {
    $loc = shift @_;
  }
  my $msg = shift @_;
  if(defined $loc) {
    if(ref $loc) {
      $loc = "$loc->{file}:$loc->{line}";
    }
    $loc = "$loc: ";
  } else {
    $loc = '';
  }
  print STDERR "warning: $loc$msg\n";
}

sub note($$) {
  my ($loc, $msg) = @_;
  if(ref $loc) {
    $loc = "$loc->{file}:$loc->{line}";
  }
  $loc = "$loc: ";
  print STDERR "$loc$msg\n";
}

sub is_verilog_file($) {
  $_[0] =~ /\.(v|sv|vh|svh|ver)$/i;
}

my $verbose = 0;
my $use_defaults = 0;

# file => module name => { tok, regs, pars, edges }
my %mods;

my $mod;
my $mod_info;
my @edges;
my @gclks;
my $in_generate;

# Artificial node for global parameters (from param files)
my $global_mod = { type => 'id', info => '<global>', file => '<global>', line => 1 };
$mods{'<builtin>'} = {};
my $global_mod_info = $mods{'<global>'}->{$global_mod->{info}} = {
  regs  => {},
  pars  => {},
  gclks => {},
  loc   => $global_mod
};

sub print_toks($) {
  my @ls = map { $_->{defined $_->{info} ? 'info' : 'type'} } @{$_[0]};
  return @ls ? join(' ', @ls) : '';
}

# TODO: this is superficial...
sub eval_number($) {
  my $val = $_[0]->{data};
  return undef if($val =~ /[xXzZ?]/);
  $val =~ s/[_ ]*//g;
  $val =~ s/^[0-9]*'//;
  if($val =~ /^h *([0-9a-fA-F]+)$/) {
    return oct("0x$1");
  } elsif($val =~ /^(?:d *)?([0-9]+)$/) {
    return $1 + 0;
  } elsif($val =~ /^o *([0-7]+)$/) {
    return oct("0$1");
  } elsif($val =~ /^b *([01]+)$/) {
    return oct("0b$1");
  } elsif($val =~ /^[+-]?[0-9]+\.[0-9]+$/) {
    return $val + 0;
  } else {
    worn($_[0]->{loc}, "sorry, can't evaluate this number: $val");
    return undef;
  }
}

# Make compiler happy
sub eval_expr($$);
sub print_expr($);
sub parse_unary_expr();
sub parse_binary_expr($);
sub parse_expr();

sub eval_expr($$) {
  my ($e, $pars) = @_;
  return undef if(!defined $e);

  # TODO: string literals (amber/hw/vlog/amber23/a23_decompile.v)
  if($e->{type} eq 'number') {
    return eval_number($e);
  }

  if($e->{type} eq 'id') {
    my $name = $e->{data};
    return exists $pars->{$name} && ($use_defaults || !$pars->{$name}->{default})
      ? $pars->{$name}->{value}
      : undef;
  }

  if($e->{type} eq 'error') {
    return undef;
  }

  if(!defined $e->{args}) {
    worn($e->{loc}, "sorry, can't yet evaluate literal of type '$e->{type}'");
    return undef;
  }

  my @args;
  for(@{$e->{args}}) {
    my $arg = eval_expr($_, $pars);
    return undef if(!defined $arg);
    push @args, $arg;
  }

  my $arity = @{$e->{args}};

  if($arity == 2) {
    my ($l, $r) = @args;
    if($e->{type} eq '**') {
      return $l ** $r;
    } elsif($e->{type} eq '+') {
      return $l + $r;
    } elsif($e->{type} eq '-') {
      return $l - $r;
    } elsif($e->{type} eq '*') {
      return $l * $r;
    } elsif($e->{type} eq '/') {
      return $l / $r;
    } elsif($e->{type} eq '%') {
      return $l % $r;
    } elsif($e->{type} eq '==') {
      return $l == $r;
    } elsif($e->{type} eq '!=') {
      return $l != $r;
    } elsif($e->{type} eq '<') {
      return $l < $r;
    } elsif($e->{type} eq '>') {
      return $l > $r;
    } elsif($e->{type} eq '<=') {
      return $l <= $r;
    } elsif($e->{type} eq '>=') {
      return $l >= $r;
    } elsif($e->{type} eq '>>') {
      return $l >> $r;
    } elsif($e->{type} eq '<<') {
      return $l << $r;
    } elsif($e->{type} eq '|') {
      return $l | $r;
    } elsif($e->{type} eq '&') {
      return $l & $r;
    } elsif($e->{type} eq '||') {
      return $l || $r;
    } elsif($e->{type} eq '&&') {
      return $l && $r;
    }
  } elsif($arity == 1) {
    my $val = $args[0];
    if($e->{type} eq '!') {
      return !$val;
    } elsif($e->{type} eq '~') {
      return ~$val;
    } elsif($e->{type} eq '+') {
      return $val;
    } elsif($e->{type} eq '-') {
      return -$val;
    }
  } elsif($e->{type} eq '?:') {
    return $args[0] ? $args[1] : $args[2];
  }

  worn($e->{loc}, "sorry, can't yet evaluate $arity-ary expression of type '$e->{type}'");
  return undef;
}

sub print_expr($) {
  my $e = $_[0];

  if($e->{type} eq 'id' || $e->{type} eq 'number') {
    return $e->{data};
  }

  if($e->{type} eq 'macro') {
    return "`$e->{data}";
  }

  if($e->{type} eq 'error') {
    return '?';
  }

  my @args;
  for(@{$e->{args}}) {
    my $arg = print_expr($_);
    return undef if(!defined $arg);
    push @args, $arg;
  }

  my $arity = @{$e->{args}};

  if($arity == 2) {
    my $l = $args[0];
    my $r = $args[1];
    return "($l) $e->{type} ($r)";
  } elsif($arity == 1) {  # Unop
    my $l = $args[0];
    return "$e->{type} ($l)";
  } elsif($e->{type} eq '?:') {
    return "($args[0]) ? ($args[1]) : ($args[2])";
  } else {
    worn($e->{loc}, "don't know how to print $arity-ary expression of type '$e->{type}'");
    return '?';
  }
}

# This isn't exhaustive but enough for our purposes
# and easily extendable. Based on
# http://www.asic-world.com/systemverilog/operators4.html
my %binop_prec = (
  '**'  => 2,
  '*'   => 3,
  '/'   => 3,
  '%'   => 3,
  '+'   => 4,
  '-'   => 4,
  '<<'  => 5,
  '>>'  => 5,
  '<<<' => 5,
  '>>>' => 5,
  '>'   => 6,
  '>='  => 6,
  '<'   => 6,
  '<='  => 6,
  '=='  => 7,
  '!='  => 7,
  '&'   => 8,
  '^'   => 9,
  '|'   => 9,
  '&&'  => 10,
  '||'  => 11,
);

my %unop = (
  '+' => 1,
  '-' => 1,
  '!' => 1,
  '~' => 1,
  '|' => 1,
  '&' => 1,
  '^' => 1,
);

sub make_error_expr($) {
  return {
    type => 'error',
    loc  => $_[0],
    data => undef,
    args => undef
  };
}

sub parse_unary_expr() {
  my $tok = Lexer::peek();

  note($tok, "parse_unary_expr") if($verbose);

  if(!defined $tok) {
    worn($tok, "failed to parse unary expression: EOF");
    return make_error_expr(Lexer::loc());
  }

  # Integer literal
  if($tok->{type} eq 'number') {
    Lexer::tok();
    note($tok, "parse_unary_expr: read number '$tok->{info}'") if($verbose);
    return { type => $tok->{type}, data => $tok->{info}, loc => $tok, args => undef };
  }

  # String literal
  if($tok->{type} eq 'string') {
    Lexer::tok();
    note($tok, "parse_unary_expr: read string '$tok->{info}'") if($verbose);
    return { type => $tok->{type}, data => $tok->{info}, loc => $tok, args => undef };
  }

  # Id
  if($tok->{type} eq 'id' || $tok->{type} eq 'macro') {
    Lexer::tok();
    note($tok, "parse_unary_expr: read $tok->{type}-atom '$tok->{info}'") if($verbose);
    my $id_expr = { type => $tok->{type}, data => $tok->{info}, loc => $tok, args => undef };
    $tok = Lexer::peek();
    if($tok->{type} eq '(' || $tok->{type} eq '[') {
      my $start_nest = Lexer::nest();
      Lexer::tok();
      worn($tok, "sorry, function calls and indexing not yet supported");
      # Skip till closing bracket
      while(1) {
        my $l = Lexer::tok();
        last if(!defined $l || $start_nest == Lexer::nest());
      }
      return make_error_expr($tok);
    }
    return $id_expr; 
  }

  # Nested expression
  if($tok->{type} eq '(') {
    Lexer::tok();
    note($tok, "parse_unary_expr: start nested expression") if($verbose);
    my $e = parse_expr();
    note($tok, "parse_unary_expr: finish nested expression") if($verbose);
    if(!defined Lexer::expect(')')) {
      return make_error_expr(Lexer::loc());
    }
    note($tok, "parse_unary_expr: read $tok->{type}-atom '$tok->{info}'") if($verbose);
    return $e;
  }

  # Concatenations
  if($tok->{type} eq '{') {
    # Skip till closing bracket
    worn($tok, "sorry, concatenations not yet supported");
    my $start_nest = Lexer::nest();
    while(1) {
      my $l = Lexer::tok();
      last if(!defined $l || $start_nest == Lexer::nest());
    }
    return make_error_expr($tok);
  }

  if(exists $unop{$tok->{type}}) {
    Lexer::tok();
    my $e = parse_unary_expr();
    return defined $e ? { type => $tok->{type}, data => undef, loc => $tok, args => [$e] } : undef;
  }

  worn($tok, "failed to parse unary expression: unexpected token '$tok->{type}'");
  return undef;
}

# Precedence climbing (see https://en.wikipedia.org/wiki/Operator-precedence_parser)
sub parse_binary_expr($) {
  my $min_prec = $_[0];

  my $l = parse_unary_expr();

  while(1) {
    my $tok = Lexer::peek();
    last if(!defined $tok);

    note($tok, "parse_binary_expr($min_prec)") if($verbose);
    if(!exists $binop_prec{$tok->{type}}) {
      note($tok, "parse_binary_expr($min_prec): done (token '$tok->{type}' does not belong to expression)") if($verbose);
      last;
    }

    my $new_prec = $binop_prec{$tok->{type}};
    note($tok, "parse_binary_expr($min_prec): read operator '$tok->{type}' of precedence $new_prec") if($verbose);

    last if($new_prec > $min_prec);
    Lexer::tok();

    my $r = parse_binary_expr($new_prec);
    $l = { type => $tok->{type}, data => undef, loc => $tok, args => [$l, $r] };
  }

  note($l->{loc}, "parse_binary_expr($min_prec): return " . print_expr($l)) if($verbose);
  return $l;
}

sub parse_expr() {
  my $start_nest = Lexer::nest();

  my $e = parse_binary_expr(100);

  # Handle ?:

  my $tok = Lexer::peek();
  return $e if($tok->{type} ne '?');

  Lexer::tok();

  my $a = parse_expr();

  if(!defined Lexer::expect(':')) {
    while(1) {
      my $l = Lexer::peek();
      return make_error_expr($l) if(
        !defined $l
        || $l->{type} eq ';'
        || ($l->{type} eq ',' || $l->{type} eq ')' || $l->{type} eq ':') && Lexer::nest() == $start_nest
        || $l->{type} eq 'keyword');
      Lexer::tok();
    }
  }

  my $b = parse_expr();

  return { type => '?:', data => undef, loc => $e->{loc}, args => [$e, $a, $b] };
}

sub maybe_parse_range($) {
  my $make_default = $_[0];

  my $top_nest = Lexer::nest();

  my $l = Lexer::peek();
  if($l->{type} ne '[') {
    my $zero = { type => 'number', data => 0, args => undef };
    return $make_default
      ? ($zero, $zero)
      : (undef, undef);
  }

  Lexer::tok();

  my $msb = parse_expr();
  return undef, undef if(!defined Lexer::expect(':'));
  my $lsb = parse_expr();
  return undef, undef if(!defined Lexer::expect(']'));

  return $msb, $lsb;
}

sub maybe_parse_type() {
  my @types;
  while(1) {
    my $l = Lexer::peek();
    last if(!defined $l || !Lexer::is_type_tok($l));
    push @types, Lexer::tok();
  }
  return \@types;
}

sub maybe_parse_rhs() {
  my @rhs;
  my $l = Lexer::peek();
  if($l->{type} eq '=') {
    Lexer::tok();

    my $top_nest = Lexer::nest();
    while(!Lexer::done()) {
      my $l = Lexer::peek();
      last if(!defined $l);
      last if($l->{type} eq ';'
              || $l->{type} eq ')' && Lexer::nest() == $top_nest
              || $l->{type} eq ',' && Lexer::nest() == $top_nest);
      Lexer::tok();
      push @rhs, $l;
    }
  }
  return \@rhs;
}

sub are_regs_equal($$) {
  my ($a, $b) = @_;

  if(defined $a->{lsb_value}
     && defined $b->{lsb_value}
     && $a->{lsb_value} == $b->{lsb_value}) {
    return 1;
  }

  if(defined $a->{msb_value}
     && defined $b->{msb_value}
     && $a->{msb_value} == $b->{msb_value}) {
    return 1;
  }

  return 0;
}

# Read comma-separated list of declarations
sub parse_decl() {
  my $top_nest = Lexer::nest();

  my $main_type;

  while(1) {
    my @decls;
    my @cur_decl;

    my $type = maybe_parse_type();
    if(!defined $main_type) {
      $main_type = $type;
    } elsif(!@$type) {
      $type = $main_type;
    }

    my $range_loc = Lexer::loc();
    my ($msb, $lsb) = maybe_parse_range(1);
    last if(!defined $msb || !defined $lsb);

    my $id = Lexer::expect('id');
    last if(!defined $id);
#print "id = ". Dumper($id) . "\n";

    my $is_ram = 0;
    while(1) {
      my $tok = Lexer::peek();
      last if(!defined $tok);
      last if ($tok->{type} ne '[');
      maybe_parse_range(0);
      $is_ram = 1;
    }

    my $l = Lexer::peek();
    if(defined $l && $l->{type} eq '=') {
      Lexer::tok();

      my $start_nest = Lexer::nest();
      while(1) {
        $l = Lexer::peek();
        last if(!defined $l);
        last if(($l->{type} eq ',' || $l->{type} eq ')') && Lexer::nest() == $start_nest);
        last if($l->{type} eq ';');
        Lexer::tok();
      }
    }

    my $name = $id->{info};

    my $is_reg = grep {$_->{type} eq 'keyword' && $_->{info} =~ /^reg$/} @$type;
    if(!$is_ram && $is_reg) {
      my $lsb_value = eval_expr($lsb, $mod_info->{pars});  # Try to evaluate here
      my $msb_value = eval_expr($msb, $mod_info->{pars});  # Try to evaluate here
      my $reg = {
          id => $id,
          lsb => $lsb,
          lsb_value => $lsb_value,
          msb => $msb,
          msb_value => $msb_value,
          edges => []
      };

      if(exists $mod_info->{regs}->{$name}) {
        my $prev = $mod_info->{regs}->{$name};

        # Only warn if declarations are incompatible
        if(!are_regs_equal($prev, $reg)) {
          worn($id, "duplicate declaration of register '$name' (previous declaration at $prev->{file}:$prev->{line}), re-definition will be ignored");
        }
      } else {
        $mod_info->{regs}->{$name} = $reg;
      }
    } elsif($verbose) {
      print STDERR "$id->{file}:$id->{line}: ignoring array declaration of $name\n" if($is_ram);
      print STDERR "$id->{file}:$id->{line}: ignoring non-register declaration of $name\n" if(!$is_reg);
    }

    $l = Lexer::expect(',', ';', ')');
    last if(!defined $l || $l->{type} eq ';' || $l->{type} eq ')');
    next if($l->{type} eq ',');
  }
}

sub maybe_parse_param_type() {
  my @types;
  my $local = 0;
  while(1) {
    my $l = Lexer::peek();
    last if(!defined $l || !Lexer::is_type_tok($l) && !Lexer::is_param_tok($l));
    push @types, Lexer::tok();
    $local = 1 if($l->{type} eq 'keyword' && $l->{info} eq 'localparam');
  }
  return \@types, $local;
}

# Read comma-separated list of param declarations
sub parse_param_decl() {
  my $main_type;
  my $main_local;

  my $cur_nest = Lexer::nest();
  if($cur_nest != 0 && $cur_nest != 1) {
    worn(Lexer::loc(), "parameter declaration has unexpected nesting level");
  }

  while(1) {
    my @decls;
    my @cur_decl;

    my ($type, $local) = maybe_parse_param_type();
    if(!defined $main_type) {
      $main_type = $type;
      $main_local = $local;
    } elsif(!@$type) {
      $type = $main_type;
    }

    maybe_parse_range(0);
    my $id = Lexer::expect('id');
    last if(!defined $id);

    my $rhs;
    my $l = Lexer::peek();
    if(defined $l && $l->{type} eq '=') {
      Lexer::tok();
      $rhs = parse_expr();
    }

    my $name = $id->{info};
    if(defined $mod_info->{pars}->{$name}) {
      my $prev = $mod_info->{pars}->{$name}->{id};
      worn($id, "duplicate declaration of parameter '$name' (previous declaration at $prev->{file}:$prev->{line}), re-definition will be ignored");
    } else {
      my $rhs_value = defined $rhs ? eval_expr($rhs, $mod_info->{pars}) : undef;  # Try to evaluate here
      $mod_info->{pars}->{$id->{info}} = {
        id      => $id,
        local   => $main_local,
        top     => ($mod eq $global_mod),
        rhs     => $rhs,
        value   => $rhs_value,
        default => $cur_nest != 0
      };
    }

    $l = Lexer::expect(',', ';', ')');
    last if(!defined $l || $l->{type} eq ';' || $l->{type} eq ')');
    next if($l->{type} eq ',');
  }
}

sub maybe_parse_nba() {
  my $id = Lexer::expect('id');

  my $l = Lexer::peek();
  if($l->{type} eq '[') {
    my $top_nest = Lexer::nest();
    Lexer::tok();
    while(!Lexer::done() && Lexer::nest() != $top_nest) {
      Lexer::tok();
    }
  }

  $l = Lexer::tok();
  return if($l->{type} ne '<=');

  if(!@edges) {
    worn($id, "NBA not in edge-triggered always block");
  } else {
    # Codebase-specific code.
    #
    # Some parts of codebase allow non-clock-gated registers
    # to improve timing.
    # Such code is always inside a generate block so for now
    # we chose to brutally ignore all NBAs in generates.
    if($in_generate) {
      my $l = $edges[0];
      note($l, "ignored NBA in generate block") if($verbose);
      return;
    }

    my $mod_edges = $mod_info->{regs}->{$id->{info}}->{edges};

    # Skip if we already saw NBA for this reg in current always block
    return if(@$mod_edges && $mod_edges->[-1]->[0] eq $edges[0]);

    push @$mod_edges, [@edges];
  }
}

sub analyze_file($$) {
  my ($file, $v) = @_;
  print STDERR "Analyzing file $file...\n";

  $mods{$file} = {};
  Lexer::init($file, $v > 0);

  $mod = $global_mod;
  $mod_info = $global_mod_info;
  @edges = @gclks = ();
  $in_generate = 0;

  while(!Lexer::done()) {
    my $l = Lexer::peek();
    last if(!defined $l);

    if(Lexer::is_start_of_block($l)) {
      @edges = ();
      note($l, "edges cleared") if($verbose);
    }

    if(Lexer::is_type_tok($l)) {
      parse_decl();
    } elsif($l->{type} eq 'id' && defined $mod_info && defined $mod_info->{regs}->{$l->{info}} && Lexer::nest() == 0) {
      maybe_parse_nba();
    } elsif(Lexer::is_param_tok($l)) {
      parse_param_decl();
    } elsif($l->{type} eq 'keyword') {
      if($l->{info} eq 'posedge' || $l->{info} eq 'negedge') {
        Lexer::tok();
        Lexer::maybe_skip('(');
        my $id = Lexer::tok();
        if($id->{type} ne 'id') {
          worn($id, "unexpected token of type '$id->{type}' after pos/negedge");
        } else {
          push @edges, $id;
        }
      } elsif($l->{info} eq 'function') { # Ignore internal function definitions
        Lexer::skip_until('keyword', 'endfunction');
      } elsif($l->{info} eq 'defparam') {
        worn($l, "defparams are not supported");
        Lexer::tok();
      } elsif($l->{info} eq 'module') {
        Lexer::tok();
        $mod = Lexer::tok();
        my $name = $mod->{info};
        my $prev = $mods{$file}->{$name};
        if(defined $prev) {
          worn($mod, "duplicate declaration of module $name (previous declaration at $prev->{loc}->{file}:$prev->{loc}->{line}), re-definition will be ignored");
          Lexer::skip_until('keyword', 'endmodule');
        }
        $mod_info = $mods{$file}->{$name} = {
          regs  => {},
          pars  => {},
          gclks => {},
          loc   => $mod
        };
      } elsif($l->{info} eq 'generate') {
        Lexer::tok();
        if($in_generate) {
          worn($l, "nested generate block");
        } else {
          $in_generate = 1;
          note($l, "entered generate block") if($verbose);
        }
      } elsif($l->{info} eq 'endgenerate') {
        Lexer::tok();
        if(!$in_generate) {
          worn($l, "unexpected 'endgenerate'");
        } else {
          $in_generate = 0;
          note($l, "exited generate block") if($verbose);
        }
      } elsif($l->{info} eq 'endmodule') {
        Lexer::tok();
        $mod = $global_mod;
        $mod_info = $global_mod_info;
        @edges = ();
        if($in_generate) {
          worn($l, "endmodule inside generate block");
          $in_generate = 0;
        }
      } else {
        Lexer::tok();
      }
    } elsif($l->{type} eq 'bind') {
      # Codebase-specifics code: we rely on name of clock gater's pin
      Lexer::tok();
      if($l->{info} eq 'gclk') {
        next if(!defined Lexer::expect('('));
        my $clk = Lexer::expect('id');
        next if(!defined $clk);
        $mod_info->{gclks}->{$clk->{info}} = $clk;
        note($clk, "found gated clock '$clk->{info}'") if($verbose);
      }
    } else {
      Lexer::tok();
    }
  }
}

my $help = 0;
my $info = 0;

GetOptions(
  'help|h'       => \$help,
  'info'         => \$info,
  'verbose|v+'   => \$verbose,
  'use-defaults' => \$use_defaults,
);

if($help) {
  print <<EOF;
Usage: gatecheck.pl [OPT]... ROOT...

Gatecheck is a sloppy script which looks for violations
of gating requirements (all registers wider than 4 bits
must be clock-gated).

Each ROOT is either a folder with Verilog files, file
with names of Verilog files or a single Verilog file to
analyze.

OPT can be one of
  --help, -h           Print this help and exit.
  --verbose, -v        Show diagnostics.
  --info               Print summary info about analyzed code.
  --use-defaults       Use default values of module parameters
                       when available (off by default).
EOF
  exit(0);
}

if($#ARGV < 0) {
  print STDERR "No root folders present at command line\n";
  exit(1);
}

my @verilog_files;

sub find_verilog_files() {
  if(is_verilog_file($File::Find::name)) {
    push @verilog_files, $File::Find::name;
  }
}

foreach my $root (@ARGV) {
  if(-d $root) {
    find({ wanted => \&find_verilog_files, no_chdir => 1}, $root);
  } elsif(is_verilog_file($root)) {
    push @verilog_files, $root;
  } else {
    open ROOT, $root or die "unable to open file $root";
    while(<ROOT>) {
      chop;
      if(!-f $_) {
        worn("file '$root' not found");
      } elsif(!is_verilog_file($_)) {
        worn("file '$root' does not look like a Verilog file");
      } else {
        push @verilog_files, $_;
      }
    }
    close ROOT;
  }
}

for(@verilog_files) {
  analyze_file($_, $verbose);
}

# Globally link parameters
#
# This is extremely hacky as we do not trace how parameters
# are passed to module instantiations but just do braindead
# name matching (with couple of chiklufs). But it works for
# our needs.

my %global_pars;
for(sort keys %mods) {
  my $file = $_;
  for(sort keys %{$mods{$file}}) {
    my $mod_name = $_;
    my $mod = $mods{$file}->{$_};

    for(sort keys %{$mod->{pars}}) {
      my $p = $mod->{pars}->{$_};
      my $id = $p->{id};

      # Ignore defaults in global analysis
      next if $p->{default};

      # Ignore locals
      next if $p->{local};

      if(!exists $global_pars{$_}) {
        $global_pars{$_} = $p;
        next;
      }

      my $prev = $global_pars{$_};

      # Codebase-specific: prefer top-level definitions
      # (supposedly from parameter files)
      if($prev->{top} != $p->{top}) {
        $global_pars{$_} = $p if($p->{top});
        next;
      }

      # Only warn if values are different (or unknown)
      next if(defined $p->{value} && defined $prev->{value} && $p->{value} == $prev->{value});

      worn($p->{id}, "duplicate assignment of parameter '$_' (previously defined at $prev->{id}->{file}:$prev->{id}->{line}), re-definition will be ignored");
    }
  }
}

# Recursively evaluate global parameters
my $changed = 1;
my $guard = 0;
while($changed) {
  $changed = 0;
  if(++$guard > 100) {
    worn("too many iterations of global parameter evaluation");
    last;
  }
  for(sort keys %mods) {
    my $file = $_;
    for(sort keys %{$mods{$file}}) {
      my $mod_name = $_;
      my $mod = $mods{$file}->{$_};
      for(sort keys %{$mod->{pars}}) {
        my $p = $mod->{pars}->{$_};
        next if(defined $p->{value});
        my $new_value = eval_expr($p->{rhs}, \%global_pars);
        if(defined $new_value) {
          $changed = 1;
          $p->{value} = $new_value;
        }
      }
    }
  }
}

# Evaluate register sizes using global params
for(sort keys %mods) {
  my $file = $_;
  for(sort keys %{$mods{$file}}) {
    my $mod_name = $_;
    my $mod = $mods{$file}->{$_};

    for(sort keys %{$mod->{regs}}) {
      my $r = $mod->{regs}->{$_};
      if(!defined $r->{lsb_value}) {
        $r->{lsb_value} = eval_expr($r->{lsb}, \%global_pars);
      }
      if(!defined $r->{msb_value}) {
        $r->{msb_value} = eval_expr($r->{msb}, \%global_pars);
      }
    }
  }
}

if($info) {
  for(sort keys %mods) {
    my $file = $_;
    for(sort keys %{$mods{$file}}) {
      my $mod_name = $_;
      my $mod = $mods{$file}->{$_};

      for(sort keys %{$mod->{regs}}) {
        my $r = $mod->{regs}->{$_};

        next if(!@{$r->{edges}});
        my $edges;
        for(@{$r->{edges}}) {
          $edges .= ', ' if($edges);
          $edges .= join(' ', map { $_->{info} } @$_);
          $edges .= " (at $_->[0]->{file}:$_->[0]->{line})";
        }

        my $id = $r->{id};
        my $lsb = print_expr($r->{lsb});
        my $lsb_value = defined $r->{lsb_value} ? " ($r->{lsb_value})" : '';
        my $msb = print_expr($r->{msb});
        my $msb_value = defined $r->{msb_value} ? " ($r->{msb_value})" : '';

        print <<EOF;
Register '$_' defined in module '$mod_name' at $id->{file}:$id->{line}: lsb = '$lsb'$lsb_value, msb = '$msb'$msb_value, edges = $edges
EOF
      }

      for(sort keys %{$mod->{pars}}) {
        my $p = $mod->{pars}->{$_};
        my $id = $p->{id};
        my $rhs = defined $p->{rhs} ? print_expr($p->{rhs}) : '?';
        my $value = defined $p->{value} ? " ($p->{value})" : '';
        my $default = $p->{default} ? " (default)" : '';
        print <<EOF;
Parameter '$_' defined in module '$mod_name' at $id->{file}:$id->{line}: rhs = '$rhs'$value$default
EOF
      }
    }
  }

  for(sort keys %global_pars) {
    my $p = $global_pars{$_};
    my $rhs = defined $p->{rhs} ? print_expr($p->{rhs}) : '?';
    my $value = defined $p->{value} ? " ($p->{value})" : '';
    print <<EOF;
Global parameter '$_' at $p->{id}->{file}:$p->{id}->{line}: rhs='$rhs'$value
EOF
  }
}

my ($nregs, $nerrs, $nfails) = (0, 0, 0);

for(sort keys %mods) {
  my $file = $_;
  for(sort keys %{$mods{$file}}) {
    my $mod_name = $_;
    my $mod = $mods{$file}->{$_};
    my $gclks = $mod->{gclks};

    for(sort keys %{$mod->{regs}}) {
      my $r = $mod->{regs}->{$_};
      ++$nregs;

      my $id = $r->{id};
      my $lsb = print_expr($r->{lsb});
      my $lsb_value = defined $r->{lsb_value} ? " ($r->{lsb_value})" : '';
      my $msb = print_expr($r->{msb});
      my $msb_value = defined $r->{msb_value} ? " ($r->{msb_value})" : '';

      next if(!@{$r->{edges}});

      if(!defined $r->{lsb_value} || !defined $r->{msb_value}) {
        ++$nfails;
        worn($id, "failed to calculate width for register '$id->{info}'");
        next;
      }

      my $width = $r->{msb_value} - $r->{lsb_value} + 1;
      worn($id, "width of register '$id->{info}' is <= 0 ($width)") if($width <= 0);
      next if($width < 4);

      for my $edges (@{$r->{edges}}) {
        my $is_gated = 0;
        for my $clk (@$edges) {
          if($clk->{info} =~ /_wr$/ || exists $mod->{gclks}->{$clk->{info}}) {
            $is_gated = 1;
            last;
          }
        }
        next if($is_gated);
        print <<EOF;
$edges->[0]->{file}:$edges->[0]->{line}: register '$id->{info}' (defined in module '$mod_name' at $id->{file}:$id->{line}) is $width bits wide but not clock-gated
EOF
        ++$nerrs;
      }
    }
  }
}

print "Analyzed $nregs, found $nerrs violations, ignored $nfails registers.\n";
