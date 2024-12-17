# Test Sequence Generation in CN (VERSE tech note)
Mike Dodds, 2024-12-13

## Installation Instructions

Download the beta tool from Leo’s fork of Cerberus 
[here](https://github.com/echoumcp1/cerberus/tree/testgennest). Build and
install as normal for CN. Run the `cn seq_test` command as follows: 

```sh
$  cn seq_test --output-dir <directory> <target_code>.c 
```

This will run the tool and build the test file in
`./<directory>/<target_code>_test.c`. The tool will exit silently with any error
messages suppressed. Currently, the way to determine whether the test passed or
failed is to look at the test file. Any errors will be appended to the sequence
of function calls. 

In order to re-run the test file, which will display error messages, run the
following script: 

```sh 
$  ./<directory>/run_test.sh 
```

The queue code which I use as an example below is hosted
[here](https://github.com/rems-project/cerberus/blob/master/tests/cn-test-gen/src/tutorial_queue.pass.c). 

## Context: Testing CN Specifications 

Suppose we have a library consisting of several functions with CN annotations: 

```
// file: tests/cn-test-gen/src/tutorial_queue.pass.c

struct queue* empty_queue ()
/*@ ensures take ret = Queue_At(return);
            ret == Nil{};
@*/

void push_queue (int x, struct queue *q)
/*@ requires take Q = Queue_At(q);
    ensures take Q_post = Queue_At(q);
            Q_post == Append (Q, x);
@*/ 

int pop_queue (struct queue *q) 
/*@ requires take Q = Queue_At(q);
             Q != Nil{};
    ensures take Q_post = Queue_At(q);
            Q_post == Tl(Q);
            return == Hd(Q);
@*/
```

We want to test that under every reasonable input, the functions obey the
specifications we’ve written in CN. There are two problems: 

1. Run-time checking: does a particular program state satisfy the specification?
   (typically the post-condition)   
2. **Input generation**: can we construct a program state that satisfies a
   specification? (typically the pre-condition) 

In the VERSE project, we are developing solutions to both of these problems,
which we combine in a single run-time testing tool, `cn test`. 

Our existing `cn test` tool solves the input generation problem (problem 2\) by
directly synthesizing a memory state. This reuses techniques from
*property-based testing*. This features works as follows: 

* _Input_: a property written in CN’s specification language   
* _Output_: a C memory state that satisfies the CN specification

This tool works for many cases, but it can be difficult to construct complex
structures like queues directly. Today we’ll discuss an alternative approach,
which operates more like fuzzing. 

## Test Sequence Generation

Instead of generating a memory state directly, we might just generate the state
by executing a sequence of calls. For example, if we want to test popping from a
queue of size 3, we could run the following program: 

```C
int main()
{
  struct queue* x0 = empty_queue();
  push_queue(27565, x0);
  push_queue(-10754, x0);
  push_queue(1837, x0);
  signed int x5 = pop_queue(x0);
  // ... 
} 
```

The problem with doing this automatically is that we want to call these
functions in a valid sequence that exercises the specifications that we have
written. This is what our new tool, `cn seq_test` does. 

The tool works as follows: 

1. The tool compiles all of the functions with run-time checking of the CN
   pre-conditions and post-conditions.   
2. The tool stores a context consisting of all the currently initialized
   variables and their types. At the start of execution, this is empty.   
3. At each step, the tool randomly picks a function with arguments that can be
   called using the currently available types. The function is added to the end
   of the sequence of calls, with the arguments instantiated appropriately.   
4. The program is executed, with three possible results:   
   1. If the execution succeeds, we add the variable to the context, and
      generate the next instruction.   
   2. If the execution fails with a post-condition failure, this is a true bug
      in the code. This is a state that should not be reachable according to the
      CN specification.   
   3. If execution fails with a pre-condition failure, we drop this function and
      pick a different one. This means that the function can’t be called in the
      current program state.   
5. Execution continues until either there are no functions that can be added to
   the sequence, or the tool reaches a particular number of function calls. 

We can run the tool on the test file as follows: 

```sh
$  cn seq_test --output-dir=queue_test_pass tutorial_queue.pass.c
```

## Example 

Suppose we have the library above. The constructed test program starts empty: 

```c
int main()
{
  // Variable context: [] 
} 
```

The only function that can be called is `empty_queue()`, which doesn’t take any
arguments. We construct the following program, which runs successfully and
passes the pre/post-condition checks. 

```c
int main() 
{
  // Variable context: {}  
  struct queue* x0 = empty_queue();
  // Variable context: { struct queue* x0 }  
} 
```

We now have three choices of which function to call: `empty_queue()`,
`push_queue(x0)`, and `pop_queue(x0)`. Suppose we pick `pop_queue`: 

```sh
int main() 
{
  // Variable context: {}  
  struct queue* x0 = empty_queue();
  // Variable context: { struct queue* x0 }  
  signed int x1 = pop_queue(x0); 
} 
```

In fact, this call will fail, but this is not a bug! If we inspect the
specification for `pop_queue`, we can see that it can only be legitimately
called when the queue is non-empty: 

```
int pop_queue (struct queue *q) 
/*@ requires take Q = Queue_At(q);
             Q != Nil{};
...
```

Fortunately `cn seq_test` detects this by checking that the pre-condition is
satisfiable. Because it is not, it rejects the choice of `pop_queue` and picks a
different function, `push_queue`: 

```c
int main() 
{
  // Variable context: {}  
  struct queue* x0 = empty_queue();
  // Variable context: { struct queue* x0 }  
  push_queue(27565, x0);
  // Variable context: { struct queue* x0 } 
} 
```

We allow `cn seq` to instantiate base types randomly, as in the integer argument
`27565` to `push_queue`. 

We continue until we have a long sequence of calls to the library: 

```c
int main() 
{
  // Variable context: {}  
  struct queue* x0 = empty_queue();
  // Variable context: { struct queue* x0 }  
  push_queue(27565, x0);
  // Variable context: { struct queue* x0 } 
  push_queue(16111, x0);
  signed int x2 =pop_queue(x0);
  push_queue(17125, x0);
  signed int x4 =pop_queue(x0);
  push_queue(x4, x0);
  push_queue(-9684, x0);
  push_queue(24355, x0);
  struct queue* x11 =empty_queue();
  signed int x12 =pop_queue(x0);
  push_queue(x12, x0);
  ... 
```

## Detecting a Bug

Suppose we introduce a synthetic bug to the queue implementation. This might
either crash, or generate a counterexample to a post-condition. 

```c
int pop_queue (struct queue *q)
{
  struct queue_cell* h = q->front;
  if (h == q->back) {
    int x = h->first;
    cn_free_sized(h, sizeof(struct queue_cell));
    q->front = 0;
    q->back = 0;
    return x;
  } else {
    // int x = h->first; // <-- correct behavior 
    int x = 0;           // <-- synthetic bug 
    q->front = h->next;
    cn_free_sized(h, sizeof(struct queue_cell));
    return x;
  }
}
```

We can run the test on this file as follows: 

```sh
cn seq_test --output-dir=queue_test_fail tutorial_queue.pass.c
```

Within a few calls, `cn seq_test` reliably finds that the post-condition of
`pop_queue` is violated, and records this fact in the constructed program: 

```c
int main(int argc, char* argv[])
{
  initialise_ownership_ghost_state();
  initialise_ghost_stack_depth();
  struct queue* x0 =empty_queue();
  push_queue(-26983, x0);
  push_queue(-6758, x0);
  push_queue(3779, x0);
  /* post-condition violation detected on this call: */
  signed int x4 =pop_queue(x0);

  return 0;
}
```

If we run the program, we can pinpoint the violation of the invariant. In fact,
this isn’t a crash, but rather a problem where the function doesn’t return the
head of the queue—in other words, a *functional correctness bug*. 

These kinds of bugs are nearly impossible for fuzzers to detect, because
typically the only signal fuzzers have that something has gone wrong is that the
program crashes.  They can't determine if a returned value is the "right" value.

```
function pop_queue, file ./tutorial_queue.pass-exec.c, line 285
original source location: 
            return == Hd(Q);
            ^~~~~~~~~~~~~~~~ tutorial_queue.pass.c:171:13-29
CN assertion failed.
```

## Significance, and Next Steps

This functionality for CN demonstrates the power of our run-time checking
engine, which can check complex functional properties with very little overhead.
In fact, this example was not written to be tested by `cn seq_test`, but was
just a part of the CN tutorial, picked off the shelf. 

We see significant value in this type of checking as a kind of
specification-driven integration testing. The tradeoff with property-based
testing is as follows: 

* Property-based testing of individual functions can be more comprehensive, but
  generation is guided by the structure of the invariant. This means that for
  some cases, it may be difficult to exercise all control-flow paths, unless we
  encode this structure into the specifications.   
* Sequence testing only requires that specifications fully characterize when it
  is safe to call a function. This means we can construct complex states without
  having to characterize them in specifications. 

These are complementary capabilities, both of which we expect to be useful as we
build more complex specifications and proofs. 

Sequence testing also complements PBT by generating a second form of evidence
that is familiar to developers (and auditors) – running the whole program and
making sure it all works together, in addition to running it in isolation, as
with PBT.