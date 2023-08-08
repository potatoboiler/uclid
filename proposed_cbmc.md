
# Proposed integration of software verification tools as an oracle for verifying procedures

NB: this document was written using the old SV-COMP syntax. The syntax appears to have changed, so would be good to check this (https://sv-comp.sosy-lab.org/)

## Verifying procedures where the body is provided as C code

### The easy case:
- The procedure is `[noinline]`. This means that we can use a software verification tool for C (e.g., CBMC) to check that the pre condition guarantees the post condition holds (i.e., to do the procedural verification)
- Then we use UCLID to verify the rest of the model, using the standard `[noinline]` semantics (i.e., at every call to the procedure, we havoc the modifies set and assume the post condition)
- User provides the body of the procedure `procedure-name` in a C file which is named `procedure-name.c`. 

#### Process:
- we construct a C file which does the following:
```
void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: {reach_error();abort();} } }
int main(int)
{
__VERIFIER_assume(precondition)
[body of function]
__VERIFIER_assert(postcondition)
}
```

Can we translate our precondition and postcondition accurately? How do we map the variables in the pre and post condition to the ones in the C file? Assume they are the same? 
- We call CBMC on these C files. If CBMC reports no errors, we know that the 
 post conditions are valid. If it reports errors, we report these back to the user (translate them back into UCLID?)
- We then use UCLID to verify the rest of the file as standard (we have noinlined the procedures so there are no issues with interactions between the rest of the module and the C procedures) 


#### side note

Currently if you ask UCLID to do BMC on a module that calls a no-inline procedure, it won't automatically run verify on the no-inline procedures, so it never checks any of the procedure code. Is this expected/ok?


### The hard case:
- The procedure is `[inline]`. We need to figure out how the C code interacts with the UCLID module that surrounds it. This needs the SMTO algorithm?


## Syntax

option 1:

~~~
 procedure [noinline,svcomp-verifier] add(a : integer, b : integer) returns (c : integer)
    requires (a >= 0 && a < 10);
    requires (b >= 0 && b < 10);
    ensures c == a + b;
    ensures (c >= 0 && c < 18);
  {
    // no body given because body is external?
  }
~~~
option 2
~~~
 procedure [noinline,svcomp-verifier] add(a : integer, b : integer) returns (c : integer)
    requires (a >= 0 && a < 10);
    requires (b >= 0 && b < 10);
    ensures c == a + b;
    ensures (c >= 0 && c < 18);
  {
    // or body is given as C code within the UCLID model
    $
     C code goes here in between "$"" signs (or use lingua franca syntax)
    $
  }
~~~
option 3
~~~
 procedure [noinline,svcomp-verifier] add(a : integer, b : integer) returns (c : integer)
    requires (a >= 0 && a < 10);
    requires (b >= 0 && b < 10);
    ensures c == a + b;
    ensures (c >= 0 && c < 18);
  {
    // or body is given as a UCLID model and we translate it to C?? 
  }
~~~

## What do we need in UCLID?
- a file-based interface (instead of interactive process. CBMC/sv comp tools may  not have an interactive mode)
- translation of UCLID to C (at least for the pre/post conditions, unless we trust the user to do that)
- some way of parsing back the response

