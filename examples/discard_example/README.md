# Discard Example

This example demonstrates the use of `Discard` to throw away a `Confirm_Promise`.
This is useful in cases where a Service User sends a request that requires a
confirmation, but the Service User later decides that it no longer needs the
confirmation.
Instead of needing to keep the `Confirm_Promise` around (to avoid leaking
resources), the Service User can explicitly discard the promise to ensure that
the resources held will eventually be freed up automatically when the
confirmation is sent to the SAP.

```mermaid
sequenceDiagram
    participant User@{ "type": "control" }
    participant SAP as SAP
    participant Provider@{ "type": "control" }

    User->>+SAP: Send_Request
    SAP-->>-User: promise
    Provider->>+SAP: Get_Next_Request
    SAP-->>-Provider: request
    User->>SAP: Discard(promise)
    Provider->>Provider: Process Request
    Provider->>SAP: Send_Confirm
    note left of SAP: All resources used by the transaction <br/> are released at this point
```

## Building

Building the program requires Alire:
```sh
alr build
```

## Running

```sh
alr run
```

>[!NOTE]
> You will need to force the program to exit with Ctrl+C since
> `Service_Provider_Task` does not exit (due to the requirements of the Jorvik
> tasking profile which prohibits tasks from terminating/returning).

## Proving

To formally verify the program with GNATprove:

```
alr exec -- gnatprove -P discard_example.gpr --level=1 -j0
```