# Proposed integration of CBMC as an oracle for verifying procedures

## Verifying procedures where the body is provided as C code

### The easy case:
- The procedure is `[noinline]`. This means that we can use CBMC to check that the pre condition guarantees the post condition holds (i.e., to do the procedural verification)
- Then we use UCLID to verify the rest of the model, using the standard `[noinline]` semantics (i.e., at every call to the procedure, we havoc the modifies set and assume the post condition)
- User provides the body of the procedure `procedure-name` in a C file which is named `procedure-name.c`. 

#### Process:
- we construct a C file which does the following:
```
int main(int)
{
__CPROVER_assume(precondition)
[body of function]
__CPROVER_assert(postcondition)
}
```

Can we translate our precondition and postcondition accurately? How do we map the variables in the pre and post condition to the ones in the C file? Assume they are the same? 
- We call CBMC on these C files. If CBMC reports no errors, we know that the 
 post conditions are valid. If it reports errors, we report these back to the user (translate them back into UCLID?)
- We then use UCLID to verify the rest of the file as standard (we have noinlined the procedures so there are no issues with interactions between the rest of the module and the C procedures) 

### The hard case:
- The procedure is `[inline]`. We need to figure out how the C code interacts with the UCLID module that surrounds it. This needs the SMTO algorithm


## Syntax
~~~
 procedure [noinline,cbmc] add(a : integer, b : integer) returns (c : integer)
    requires (a >= 0 && a < 10);
    requires (b >= 0 && b < 10);
    ensures c == a + b;
    ensures (c >= 0 && c < 18);
  {
    // no body given because body is external?
    // or body is given as C code within the UCLID model

    $
     C code goes here in between "$"" signs
    $
  }
~~~





