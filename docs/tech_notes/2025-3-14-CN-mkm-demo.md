# MKM Demo
2025-03-14

- [MKM Demo](#mkm-demo)
  - [1 - MKM Purpose](#1---mkm-purpose)
  - [2 - MKM High Level Protocol](#2---mkm-high-level-protocol)
  - [3 - MKM Requirements In Detail](#3---mkm-requirements-in-detail)
  - [4 - MKM State Machine](#4---mkm-state-machine)
  - [5 - MKM Protocol Encoded as a CN Predicate](#5---mkm-protocol-encoded-as-a-cn-predicate)
  - [6 - CN Predicate as part of a function contract](#6---cn-predicate-as-part-of-a-function-contract)
  - [7 - Testing the MKM State Machine](#7---testing-the-mkm-state-machine)
  - [8 - Introducing a Synthetic Bug](#8---introducing-a-synthetic-bug)
  - [9 - Walking Through `client_event()`](#9---walking-through-client_event)
  - [10 - An Example Sub-function - `client_read()`](#10---an-example-sub-function---client_read)



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

We also have other requirements for the MKM component, including the requirement
that the component is *memory-safe / free of undefined behavior*. 



## 3 - MKM Requirements In Detail 

Below are high-level requirements for the MKM:

* **TA2-REQ-66: Close connection on error**
  * If an error occurs at any time during the key exchange protocol, such as an
    invalid attestation or a policy violation, the MKM shall close the
    connection without sending the key.
* **TA2-REQ-67: No headers or delimiters for messages**
  * All MKM messages shall have a fixed size and occur in a fixed order, and the
    protocol shall not use any headers or delimiters for messages.
* **TA2-REQ-68: TCP connection**
  * The client shall connect to the MKM over TCP via a socket.
* **TA2-REQ-69: Wait for key ID**
  * While the MKM is ready to receive connections, a client component shall send
    a key ID (1 byte), indicating which key it is requesting.
* **TA2-REQ-70: Send challenge**
  * When a key ID is received from a client, the MKM shall send a random nonce
    (16 bytes) in return.
* **TA2-REQ-71: Valid key ID**
  * The MKM shall process only a valid key ID.
* **TA2-REQ-72: Calculate attestation**
  * Once the client receives an attestation challenge (nonce) from the MKM, the
    client shall compute the response by communicating with its trusted boot
    daemon and send the response back to the MKM.
* **TA2-REQ-73: Challenge response format**
  * The challenge response shall be computed by concatenating the current
    measured value (matching the expected hash of the binary) with the received
    nonce, and then computing HMAC of the concatenated value using a secret key.
    The resulting response is 64 bytes long.
* **TA2-REQ-74: Secure boot secret key**
  * The secret key may be identical across different components, so as to
    simplify the key management. This key is known at build time to the MKM.
  * **Rationale:** In real world, secure boot would store unique and shared keys
    in a Hardware Root of Trust (HROT) and the decision whether to use unique or
    shared keys would be based on the actual threat model. In either way, the
    MKM must know the key to validate the attestation response.
* **TA2-REQ-75: Receive response**
  * Once the MKM receives the attestation response, it shall check its validity.
    A valid attestation is calculated as described in TA2-REQ-73.
* **TA2-REQ-76: Send key**
  * If the received response is valid, the MKM shall send back to the client the
    associated mission key and terminate the connection.
* **TA2-REQ-77: Key format**
  * The mission key is 32-bytes long symmetric AES key.

(from [here](https://github.com/GaloisInc/VERSE-OpenSUT/blob/prelease-v1.0/components/mission_key_management/README.md#requirements))



## 4 - MKM State Machine 

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



## 5 - MKM Protocol Encoded as a CN Predicate 

We can quite easily express this property as a CN predicate (from `client.h`, line 153):

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

If we wrote the state machine in a formal language, rather than a diagram, we
could even extract it directly to this representation. 



## 6 - CN Predicate as part of a function contract

The main event handler for MKM is the `client_event()` function. It is a
requirement of the MKM module that this function respects the structure of the
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



## 7 - Testing the MKM State Machine 

We have all the ingredients we need to test the function contract: 

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

Once we have set up the predicates appropriately, we can test them: 

```sh 
cn test client.c  # not quite ... 
``` 

Actually it's quite a bit more complicated than just this, for two reasons: 

1. We need to make sure we're including all the necessary dependencies. 
2. Testing works by injection into the code, which means handling some quite
   subtle interactions with the C preprocessor.

Instead, we have a Makefile that sets everything up correctly, so can call: 

```sh 
make cn_test
``` 

This calls 1000 randomized tests on each function in the file. This involves
synthesizing valid instances of the `struct client` type in memory, and checking
the code steps to a valid state machine state, as well as preserving memory
safety / ownership. 

Testing has several advantages over verification: 

- We don't need to write loop invariants and internal annotations 
- We always get concrete counter-examples if we fail 
- For our intended clients, sometimes their certification requirements involve
  testing not verification 

If we do want to prove the code correct, intuitively we just call the following: 

```sh
cn verify client.c  # not quite ... 
```

But for the same reasons as above, actually we have a Makefile that does this
for us: 

```sh 
make cn_proof
``` 


## 8 - Introducing a Synthetic Bug 

We can introduce a bug into the CN code and show that we can detect it: 

(`client.c` line 411)
```C 
        case CS_RECV_KEY_ID: { // NOTE additional block needed for declaration 
            memcpy(c->challenge, "random challenge", NONCE_SIZE);
            client_change_state(c, CS_RECV_RESPONSE);
            //                     ~~~~~~~~~~~~~~~~
            // Should be CS_SEND_CHALLENGE --^
            break; } 
``` 

We can also show that randomized testing will detect this same bug. This is very
nice - it means that abstract properties of the state machine are enforced in
the code by run-time tests. 

We can also show that verification is enforcing correct behavior by introducing
a synthetic bug into the state machine. 

(`client.h` line 153)
```C
/*$
function (boolean) ValidTransition (u32 state1, u32 state2) {
       ( state1 == state2 ) 
    || ( (state1 == (u32) CS_RECV_KEY_ID)    && (state2 == (u32) CS_RECV_RESPONSE) )
    //                                                           ~~~~~~~~~~~~~~~~
    //                             Should be CS_SEND_CHALLENGE ------^
    || ( (state1 == (u32) CS_SEND_CHALLENGE) && (state2 == (u32) CS_RECV_RESPONSE)  )
    || ( (state1 == (u32) CS_RECV_RESPONSE)  && (state2 == (u32) CS_SEND_KEY)       )
    || ( ValidState(state1)                  && (state2 == (u32) CS_DONE)           )
}
$*/
```


## 9 - Walking Through `client_event()`

Some properties to note in `client_event()`: 
* Changing the state actually relies on a sub-function, `client_change_state()`.
* It calls several sub-functions which may modify the client object, and the
  reasoning has to account for such modifications. For example, `client_read()`. 
* The state machine code is quite simple, and all of the read/write logic
  actually occurs in `client_write()` and `client_read()`.



## 10 - An Example Sub-function - `client_read()`

The purpose of `client_read()` is to read from a file descriptor into one of the
buffers that MKM is managing. This has the following signature and contract:

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

The logic in this function is complex: 

- Get a pointer to the buffer that should be accessed - `client_read_buffer()`
- Get the size of the buffer - `client_buffer_size()`
- Use both values to access the buffer 
