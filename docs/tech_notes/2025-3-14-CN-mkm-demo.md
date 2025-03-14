# MKM Demo -- plan 
2025-03-14

## 1 - MKM Purpose 

> This server processes key requests and distributes keys to other components.
> Any component can connect to the MKM, request a key, and attest to the code
> that it's running; the MKM will then send the key if allowed by the MKM's
> built-in policy. 

(from [here](https://github.com/GaloisInc/VERSE-OpenSUT/tree/main/components/mission_key_management#mission-key-management-server))


## 2 - MKM High Level Protocol

> The protocol that components use to communicate with the MKM works as follows:
> 
> - The client connects to the MKM over TCP.  
> - The client component sends a key ID (1 byte), indicating which key it is
>   requesting.  
> - The MKM sends a random nonce (16 bytes).  
> - The client obtains an attestation matching the challenge (by communicating
>   with its trusted boot daemon) and sends the attestation (64 bytes).  
> - If the attestation is valid and MKM policy allows the component to receive the
>   requested key, the MKM sends the key (32 bytes).
> 
> If an error occurs, such as an invalid attestation or a policy violation, the
> MKM simply closes the connection without sending the key.

(from [here](https://github.com/GaloisInc/VERSE-OpenSUT/tree/main/components/mission_key_management#protocol))


## 3 - MKM State Machine 

We can realize the protocol as a simple state machine: 

```
   ┌────────────────┐          > The client component sends a key ID (1 byte), 
┌─►│ CS_RECV_KEY_ID ├──────┐   > indicating which key it is requesting.
│  └┬─────┬─────────┘      │
└───┘     │                │
   ┌──────▼────────────┐   │   > The MKM sends a random nonce (16 bytes).  
┌─►│ CS_SEND_CHALLENGE ├──►│
│  └┬─────┬────────────┘   │
└───┘     │                │
   ┌──────▼───────────┐    │   > The client obtains an attestation matching the 
┌─►│ CS_RECV_RESPONSE ├───►│   > challenge (by communicating with its trusted boot
│  └┬─────┬───────────┘    │   > daemon) and sends the attestation (64 bytes).
└───┘     │                │
   ┌──────▼──────┐         │   > If the attestation is valid and MKM policy allows 
┌─►│ CS_SEND_KEY ├────────►│   > the component to receive the requested key, the MKM 
│  └┬─────┬──────┘         │   > sends the key (32 bytes).
└───┘     │                │
   ┌──────▼──┐             │
┌─►│ CS_DONE │◄────────────┘   > If an error occurs, such as an invalid attestation 
│  └┬────────┘                 > or a policy violation, the MKM simply closes the 
└───┘                          > connection without sending the key.
```

In a real engineering environment, we would expect a requirements engineer to 
perform this translation. 


## 4 - MKM Protocol Encoded as a CN Predicate 

We can quite easily express this property as a CN predicate. If we wrote the
state machine in a formal language, rather than a diagram, we could even extract
it directly to this representation. 

```C
function (boolean) ValidTransition (u32 state1, u32 state2) {
    // The MKM module may take no transition
       ( state1 == state2 ) 
    // The client component sends a key ID 
    || ( (state1 == (u32) CS_RECV_KEY_ID)    && (state2 == (u32) CS_SEND_CHALLENGE) )
    // The MKM sends a random nonce 
    || ( (state1 == (u32) CS_SEND_CHALLENGE) && (state2 == (u32) CS_RECV_RESPONSE)  )
    // The client sends the attestation 
    || ( (state1 == (u32) CS_RECV_RESPONSE)  && (state2 == (u32) CS_SEND_KEY)       )
    // The MKM sends the key  OR  the MKM closes the connection 
    || ( ValidState(state1)                  && (state2 == (u32) CS_DONE)           )
}
``` 


## 5 - CN Predicate as part of a function contract

The main event handler for MKM is the `client_event()` function. It is a
requirement of the MKM module that this function to respects the structure of the
state machine. 

We can give the `client_event` function the following contract in CN: 

```C
enum client_event_result client_event(struct client* c, uint32_t events) 
/*$
requires 
    take Client_in = ClientObject(c); 
ensures 
    take Client_out = ClientObject(c); 
    ValidTransition(Client_in.state, Client_out.state); 
$*/
``` 

Here `ClientObject()` is an *object predicate* defining the internal structure
of the CN object. The object itself is defined in `client.h` as follows (with
some minor simplifications)

```C
struct client {
    int fd;
    // Buffers for async read/write operations.
    uint8_t challenge[NONCE_SIZE];
    uint8_t response[MEASURE_SIZE + HMAC_SIZE];
    const uint8_t* key;
    uint8_t key_id[KEY_ID_SIZE];
    // Read/write position within the current buffer.  Which buffer this refers
    // to depends on the current state.  For the chosen buffer, `buf[i]` is
    // initialized only within the range `0 <= i < pos`.
    uint8_t pos;
    enum client_state state;
};
```

The corresponding `ClientObject()` predicate is defined like this: 

```C
predicate (struct client) ClientObject (pointer p)
{
    // Read/write access to the object itself 
    take C = RW<struct client>(p); 
    // The allocation record -- allow us to dispose the object 
    take Log = ClientAlloc(p); 
    // Ensure that the state machine is in a valid state 
    assert ( ValidState(C.state) ) ; 
    // The current key 
    take K = KeyPred(C.key);
    // The 
    return C; 
}
``` 


## 6 - Testing the MKM State Machine 

Once we have set up the predicates appropriately, we can test them: 

```sh 
cn test client.c 
``` 

Actually it's quite a bit more complicated than just this, for two reasons: 

1. We need to make sure we're including all the necessary dependencies. 
2. Testing works by injection into the code, which means handling some quite
   subtle interactions with the C preprocessor.

Instead, we can just call: 

```sh 
make cn_test
``` 

This calls 1000 randomized tests on each function in the file. 

Likewise, if we want to prove the code correct, intuitively we just call the
following: 

```sh
cn verify client.c
```

But for the same reasons as above, actually, we have a Makefile that does this
for us: 

```sh 
make cn_proof
``` 


## 7 - Introducing a Synthetic Bug 

We can show that verification is enforcing correct behavior by introducing a synthetic
bug into the state machine. 

```C
/*$
function (boolean) ValidTransition (u32 state1, u32 state2) {
       ( state1 == state2 ) 
    || ( (state1 == (u32) CS_RECV_KEY_ID)    && (state2 == (u32) CS_SEND_CHALLENGE) )
    || ( (state1 == (u32) CS_SEND_CHALLENGE) && (state2 == (u32) CS_RECV_RESPONSE)  )
    || ( (state1 == (u32) CS_RECV_RESPONSE)  && (state2 == (u32) CS_SEND_KEY)       )
    || ( ValidState(state1)                  && (state2 == (u32) CS_SEND_KEY)       )
    //                                                           ~~~~~~~~~~~
    //                                        Should be CS_DONE ------^
}
$*/
```

We can also show that randomized testing will detect this same bug. This is
very nice - it means that abstract properties of the state machine are enforced
in the code by run-time tests. 

We can likewise introduce a bug into the CN code and show that we can detect it: 

```C 
        case CS_RECV_KEY_ID: { // NOTE additional block needed for declaration 
            memcpy(c->challenge, "random challenge", NONCE_SIZE);
            client_change_state(c, CS_RECV_RESPONSE);
            //                     ~~~~~~~~~~~~~~~~
            // Should be CS_SEND_CHALLENGE --^
            break; } 
``` 

## 8 - Walking Through `client_event()`

Some properties to note in `client_event()`: 
* Changing the state actually relies on a sub-function, `client_change_state()`.
* It calls several sub-functions which may modify the client object, and the
  reasoning has to account for such modifications. For example, `client_read()`. 


## 9 - An Example Sub-function - `client_read()`

The purpose of `client_read()` is to read from a file descriptor into one of 
the buffers that MKM is managing. This has the following signature and contract:

```C 
enum client_event_result client_read(struct client* c) 
/*$ 
requires 
    take Client_in = ClientObject(c); 
    let pos = (u64) Client_in.pos; 
ensures 
    take Client_out = ClientObject(c); 
    Client_out.fd == Client_in.fd; 
    Client_out.challenge == Client_in.challenge; 
    ptr_eq(Client_out.key, Client_in.key);
    Client_out.state == Client_in.state; 
$*/
```