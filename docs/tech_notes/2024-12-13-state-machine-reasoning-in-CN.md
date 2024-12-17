# State Machine Reasoning in CN (VERSE tech note)
Mike Dodds, 2024-12-13

## Context: Reasoning in CN 

Reasoning in CN is entirely encoded into `requires` / `ensures` contracts over
memory states. For example, we can verify a simple addition function as follows: 

```c
void add_1(int *i)
/*@ requires take I_pre = Owned(i);
             I_pre == 0i32;
    ensures  take I_post = Owned(i);
             I_post == 1i32;
@*/ 
{ 
  (*i)++; 
} 
```

The `requires`  / `ensures` clause here is entirely about memory and variable
states. This means that if we want to reason about some additional construct,
such as a state machine, we have to encode it into an object in memory. 


## Encoding a Simple State Machine

For example, suppose we have a cryptographic API which has three operations:
`init`, `encrypt`, and `send`. The state machine for these operations looks like
this: 

```
            Init        Init         
              ┌─┐ ┌──────────────┐   
              │ │ │              │   
┌─────┐ Init ┌┴─▼─▼┐ Encrypt  ┌──┴──┐
│  0  │─────►│  1  ├─────────►│  2  │
└──▲──┘      └─────┘          └──┬──┘
   │                             │   
   └─────────────────────────────┘   
               Send                  
```

This state machine requires that the three operations happen in sequence, but we
can always transition back to the initial state, `0`. 

We can encode this into CN using a struct containing a field storing the state
machine state. We call this a *ghost object* because it doesn’t change the
behavior of the ‘real’ code. It’s just a convenience for encoding verification
properties. For our encryption example, we define the ghost object as follows: 

```c
struct machine_state {
  int stateM;
};
```

We define three CN-level functions which encode the meaning of each state of the
state machine: 

```c
/*@ 
function (boolean) state_0 (struct machine_state s) {
  s.mState == 0i32
}  

function (boolean) state_1 (struct machine_state s) {
  s.mState == 1i32
}  

function (boolean) state_2 (struct machine_state s) {
  s.mState == 2i32
} 
@*/
```

We also define a helper function, `state_any`, which represents that the current
state can be any of the valid values: 

```c
/*@ 
function (boolean) state_any (struct machine_state s) {
  state_0(s) || state_1(s) || state_2(s)
} 
@*/ 
```


## Instrumenting the Library Operations 

In order to enforce the state machine, we add the required state predicates to
the function specifications. For example, for the `init` function, we require
that the function is always called from `state_0`, and the operation may result
in a state that can be treated as either `state_0` or `state_1`. 

```c
void init(struct encrypt_state *e, struct machine_state *s)
/*@ requires take S_in = Owned(s); 
             state_any(S_in); 
    ensures  take S_out = Owned(s); 
             state_1(S_out); 
@*/
{
  // ... intialize the encryption state 
  s->mState = 1; // Ghost operation: state machine update 
}
```

It’s worth noting that this is an encoding, which is why the transition function
is duplicated in the ensures clause and in the code. In the future, we expect to
move everything to the spec and remove the duplication.

The other operations can be modified in the same way: 

```c
void encrypt(struct encrypt_state *e, struct machine_state *s, int message, int length)
/*@ requires take S_in = Owned(s); 
             state_1(S_in); 
    ensures  take S_out = Owned(s); 
             state_2(S_out); 
@*/
{
  // ... encrypt the message  
  s->mState = 2; // Ghost operation: state machine update 
}

void send(struct encrypt_state *e, struct machine_state *s, int channelID)
/*@ requires take S_in = Owned(s); 
             state_2(S_in); 
    ensures  take S_out = Owned(s); 
             state_0(S_out); 
@*/
{
  // ... send the message down the channel 
  s->mState = 0; // Ghost operation: state machine update 
}
```


## Reasoning About a Client

The purpose of this kind of state machine reasoning is that it restricts in what
sequence clients of the functions can call the functions. For example, the
following client code respects the state machine sequencing, and can be
verified: 

```c
void client_good_1(struct encrypt_state *e, struct machine_state *s)
/*@ requires take S_in = Owned(s); 
             state_any(S_in); 
    ensures  take S_out = Owned(s); 
@*/
{
  init(e,s); 
  init(e,s); 
  encrypt(e,s,0,0); 
  init(e,s); 
  encrypt(e,s,0,0); 
  send(e,s,0); 
}
```

However, the following client skips the encryption step, and verification in CN
will fail: 

```c
void client_bad_1(struct encrypt_state *e, struct machine_state *s)
/*@ requires take S_in = Owned(s); 
             state_any(S_in); 
    ensures  take S_out = Owned(s); 
@*/
{
  init(e,s); 
  send(e,s,0); 
}
```


## Using Test Sequencing to Generate Clients

(See [here](TODO) for a longer writeup of the test sequencing tool)

We can use CN’s test sequencing tool to generate clients. We can both generate
clients that conform to the state machine, and clients that do not (for the
purposes of testing). Suppose we want to do the former. We run the tool as
follows: 

```sh
$  cn seq_test --output-dir seq_test api_state_machine.c
```

The tool then generates a client which matches the API protocol: 

```c
int main
{
  struct machine_state* x0 =new_machine_state();
  init(x0);
  encrypt(x0, 7023, -30103);
  send(x0, 3231);
  init(x0);
  encrypt(x0, 19656, 6222);
  send(x0, -29312);
  init(x0);
  init(x0);
  encrypt(x0, 30959, 22186);
  init(x0);
  struct machine_state* x12 =new_machine_state();
  encrypt(x0, 1823, -12751);
  init(x0);
  encrypt(x0, 27184, 2885);
  init(x12);
  encrypt(x12, -3792, -23055);
  init(x0);
  struct machine_state* x19 =new_machine_state();
  init(x0);
  ... 
} 
```
