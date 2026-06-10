--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System;

private with LibSAP.Singleton_Transaction_Queues;

--  This package defines a Service Access Point (SAP) for transferring
--  indication and response primitives between between a Service Provider and
--  Service User.
--
--  Transactions are initiated by a Service Provider by sending an indication
--  primitive. The Service User can then retrieve the indication, then send
--  a response primitive back to the Service Provider if one is required by
--  the original indication.
--
--  The primitives are transferred synchronously, so the SAP can be safely
--  accessed by multiple tasks concurrently.

generic
   type Indication_Kind_Type is (<>);
   --  Discrete type (e.g. enumeration) to distinguish between different kinds
   --  of indications.
   --
   --  This is typically an enumeration of all the different kinds of
   --  indications that can be sent through the SAP, but any discrete type will
   --  also work.

   type Indication_Type is limited private;
   --  The data type for indication primitives.
   --
   --  This type must be a definite, unconstrained type with default
   --  initialization.

   type Response_Type is limited private;
   --  The data type for response primitives.
   --
   --  This type must be a definite, unconstrained type with default
   --  initialization.

   Queue_Capacity : Positive;
   --  Configures the maximum number of concurrent transactions

   with
     function Indication_Kind
       (Indication : Indication_Type) return Indication_Kind_Type;
   --  Returns the Kind of an indication.
   --
   --  This is used to distinguish between different kinds of indications.
   --  For example, to distinguish between a CONNECT.ind and DATA.ind, which
   --  may have different requirements around cleanup.

   with
     function Requires_Response (Indication : Indication_Type) return Boolean;
   --  Returns true if the Indication requires a corresponding response
   --  primitive.

   with
     function Indication_Requires_Cleanup
       (Indication : Indication_Type) return Boolean;
   --  Returns True if the Indication object requires explicit cleanup before
   --  it can be released at the end of the transaction.
   --
   --  This is intended for cases when the Indication contains a non-null
   --  pointer field, in which case the pointer needs to be either freed or
   --  moved before the transaction is completed.
   --
   --  If the Indication object never has ownership semantics, then this
   --  function always returns False.
   --
   --  This must always return False for a default-initialized Indication_Type.

   with
     function Response_Requires_Cleanup
       (Response : Response_Type) return Boolean;
   --  Returns True if the Response object requires explicit cleanup before
   --  it can be released at the end of the transaction.
   --
   --  This is intended for cases when the Response contains a non-null pointer
   --  field, in which case the pointer needs to be either freed or moved
   --  before the transaction is completed.
   --
   --  If the Response object never has ownership semantics, then this function
   --  always returns False.
   --
   --  This must always return False for a default-initialized Response_Type.

   with
     function Might_Require_Cleanup
       (Kind : Indication_Kind_Type) return Boolean;
   --  Returns True if an Indication OR Response primitive of this Kind might
   --  require cleanup before they are freed at the end of a transaction.
   --
   --  This is intended for cases when certain kinds of indications (and/or
   --  related response primitives) contain parameters that have ownership
   --  semantics in SPARK (e.g. pointers).
   --
   --  This must return True if any indication primitive of type Kind, or
   --  related response primitives, contain one or more parameters that have
   --  ownership semantics. Otherwise, it should return False.

   with
     function Valid_Indication (Indication : Indication_Type) return Boolean;
   --  Returns True if the Indication object is valid
   --
   --  This can be used to validate the contents of an indication to ensure
   --  that only well-formed indications are sent via the SAP.

   with
     function Valid_Response
       (Indication : Indication_Type; Response : Response_Type) return Boolean;
   --  Returns True if the Response object is valid for the given Indication
   --
   --  This can be used to validate the contents of a response primitive to
   --  ensure that only valid responses are sent for an indication.

   with procedure Notify_Indication_Pending is null;
   --  An optional procedure that is called when a new indication is queued.
   --
   --  The purpose of this procedure is to allow the Service User task
   --  to be notified when a new indication is pending, e.g. unblocking the
   --  task that is waiting on a protected entry.

   with procedure Notify_Response_Pending (TID : Positive) is null;
   --  An optional procedure that is called when a new response primitive is
   --  pending.
   --
   --  The purpose of this procedure is to allow the Service User task
   --  to be notified (by e.g. waking it up) when a new response is pending.
   --
   --  @param TID The ID of the transaction whose response has been posted.

   Priority : System.Priority;
   --  The ceiling priority of the transaction queue.

package LibSAP.Synchronous_User_Service_Access_Point with
    Elaborate_Body,
    Abstract_State => (Transaction_Queue with Synchronous)
is

   type Transaction_ID is new Positive range 1 .. Queue_Capacity;

   function Always_True
     (Indication : Indication_Type with Unreferenced) return Boolean
   is (True);

   function Always_True
     (Indication : Indication_Type with Unreferenced;
      Response   : Response_Type with Unreferenced) return Boolean
   is (True);

   ------------------------
   -- Indication Handles --
   ------------------------

   --  An indication handle holds a reference to an indication object.
   --
   --  It is used by the Service Provider to build a indication and send it to
   --  the Service User.

   type Indication_Handle is limited private
   with Default_Initial_Condition => Is_Null (Indication_Handle);

   function Is_Null (Handle : Indication_Handle) return Boolean
   with Inline, Global => null;

   function Empty_Handle return Indication_Handle
   with Inline, Global => null, Post => Is_Null (Empty_Handle'Result);

   function Get_TID (Handle : Indication_Handle) return Transaction_ID
   with Inline, Global => null, Pre => not Is_Null (Handle);

   function Indication_Reference
     (Handle : Indication_Handle)
      return not null access constant Indication_Type
   with Global => null, Pre => not Is_Null (Handle);

   function Requires_Response (Handle : Indication_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Requires_Response'Result
       = Requires_Response (Indication_Reference (Handle).all);

   function Indication_Written (Handle : Indication_Handle) return Boolean
   with Global => null, Pre => not Is_Null (Handle);

   function Indication_Kind
     (Handle : Indication_Handle) return Indication_Kind_Type
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Indication_Kind'Result
       = Indication_Kind (Indication_Reference (Handle).all);

   procedure Move
     (Target : in out Indication_Handle; Source : in out Indication_Handle)
   with
     Global => null,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       not Is_Null (Target)
       and Is_Null (Source)
       and (Is_Null (Target) = Is_Null (Source)'Old)
       and (Requires_Response (Target) = Requires_Response (Source)'Old)
       and (Indication_Written (Target) = Indication_Written (Source)'Old)
       and (Indication_Kind (Target) = Indication_Kind (Source)'Old);

   generic
      with procedure Initialize (Indication : out Indication_Type);
      with function Precondition return Boolean is Always_True;
      with
        function Postcondition (Indication : Indication_Type) return Boolean
        is Always_True;
   procedure Initialize_Indication (Handle : in out Indication_Handle)
   with
     Inline,
     Pre  =>
       not Is_Null (Handle)
       and then Precondition
       and then not Indication_Written (Handle),
     Post =>
       not Is_Null (Handle)
       and Indication_Written (Handle)
       and Postcondition (Indication_Reference (Handle).all)
       and (Get_TID (Handle) = Get_TID (Handle)'Old);
   --  Builds the indication payload for an active, unwritten transaction
   --  handle.
   --
   --  This procedure utilizes a generic callback (`Initialize`) to directly
   --  populate the indication object in place, supporting zero-copy message
   --  passing.
   --
   --  The Precondition and Postcondition generic formal functions are optional
   --  proof bridge functions. They are used to pass verification context from
   --  the caller to the call to the `Initialize` procedure.

   generic
      with procedure Update (Indication : in out Indication_Type);

      with function Precondition (Indication : Indication_Type) return Boolean;

      with
        function Postcondition (Indication : Indication_Type) return Boolean;
   procedure Update_Indication (Handle : in out Indication_Handle)
   with
     Inline,
     Pre  =>
       not Is_Null (Handle)
       and then Precondition (Indication_Reference (Handle).all)
       and then Indication_Written (Handle),
     Post =>
       not Is_Null (Handle)
       and Indication_Written (Handle)
       and Postcondition (Indication_Reference (Handle).all)
       and (Get_TID (Handle) = Get_TID (Handle)'Old);
   --  Modifies a previously written indication payload.
   --
   --  This procedure utilizes a generic callback (`Update`) to directly
   --  update the indication object in place, supporting zero-copy message
   --  passing.
   --
   --  The Precondition and Postcondition generic formal functions are optional
   --  proof bridge functions. They are used to pass verification context from
   --  the caller to the call to the `Update` procedure.

   ----------------------
   -- Response Promise --
   ----------------------

   --  Response_Promise represents the eventual completion of an indication
   --  that requires a response from the Service User.
   --
   --  It is used by the Service Provider to get the corresponding response
   --  primitive for a previous indication, after the Service User has
   --  processed the indication and sent the response.

   type Response_Promise is limited private
   with Default_Initial_Condition => Is_Null (Response_Promise);

   function Is_Null (Promise : Response_Promise) return Boolean;

   function Empty_Promise return Response_Promise
   with Inline, Global => null, Post => Is_Null (Empty_Promise'Result);

   function Get_TID (Promise : Response_Promise) return Transaction_ID
   with Inline, Global => null, Pre => not Is_Null (Promise);

   function Indication_Kind
     (Promise : Response_Promise) return Indication_Kind_Type
   with Global => null;

   procedure Move
     (Target : in out Response_Promise; Source : in out Response_Promise)
   with
     Inline,
     Global => null,
     Pre    => Is_Null (Target),
     Post   =>
       (Is_Null (Target) = Is_Null (Source)'Old)
       and Is_Null (Source)
       and (Indication_Kind (Target) = Indication_Kind (Source)'Old);

   ---------------------
   -- Response Handles --
   ---------------------

   --  A response handle holds a reference to a response object.
   --
   --  It is used by the Service Provider to read the response primitive that
   --  was sent back by the Service User. The original indication primitive can
   --  also be read via the handle.

   type Response_Handle is limited private
   with Default_Initial_Condition => Is_Null (Response_Handle);

   function Is_Null (Handle : Response_Handle) return Boolean
   with Inline, Global => null;

   function Empty_Handle return Response_Handle
   with Inline, Global => null, Post => Is_Null (Empty_Handle'Result);

   function Get_TID (Handle : Response_Handle) return Transaction_ID
   with Inline, Global => null, Pre => not Is_Null (Handle);

   function Indication_Reference
     (Handle : Response_Handle) return not null access constant Indication_Type
   with Inline, Global => null, Pre => not Is_Null (Handle);

   function Response_Reference
     (Handle : Response_Handle) return not null access constant Response_Type
   with Inline, Global => null, Pre => not Is_Null (Handle);

   function Indication_Kind
     (Handle : Response_Handle) return Indication_Kind_Type
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Indication_Kind'Result
       = Indication_Kind (Indication_Reference (Handle).all);

   function Requires_Cleanup (Handle : Response_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Requires_Cleanup'Result
       = (Indication_Requires_Cleanup (Indication_Reference (Handle).all)
          or else Response_Requires_Cleanup (Response_Reference (Handle).all));

   procedure Move
     (Target : in out Response_Handle; Source : in out Response_Handle)
   with
     Inline,
     Global => null,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       (Is_Null (Target) = Is_Null (Source)'Old)
       and Is_Null (Source)
       and (Indication_Kind (Target) = Indication_Kind (Source)'Old);

   generic
      with
        procedure Clean
          (Indication : in out Indication_Type;
           Response   : in out Response_Type);

      with
        function Precondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;

      with
        function Postcondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;
   procedure Cleanup (Handle : in out Response_Handle)
   with
     Inline,
     Pre  =>
       not Is_Null (Handle)
       and then
         Precondition
           (Indication_Reference (Handle).all,
            Response_Reference (Handle).all),
     Post =>
       not Is_Null (Handle)
       and then
         Postcondition
           (Indication_Reference (Handle).all, Response_Reference (Handle).all)
       and then
         not Indication_Requires_Cleanup (Indication_Reference (Handle).all);
   --  Performs custom resource cleanup on an active transaction's primitives.
   --
   --  This procedure utilizes a generic callback (`Clean`) to safely extract,
   --  deallocate, or move parameters with SPARK ownership semantics (e.g.,
   --  pointers) out of the indication and response primitives. This must occur
   --  before the handle's memory can be safely recycled or freed back to the
   --  SAP.
   --
   --  The Precondition and Postcondition generic formal functions are optional
   --  proof bridge functions. They are used to pass verification context from
   --  the caller to the call to the Build procedure.

   ---------------------
   -- Service Handles --
   ---------------------

   type Service_Handle is limited private
   with Default_Initial_Condition => Is_Null (Service_Handle);

   function Is_Null (Handle : Service_Handle) return Boolean
   with Global => null;

   function Empty_Handle return Service_Handle
   with Inline, Global => null, Post => Is_Null (Empty_Handle'Result);

   function Get_TID (Handle : Service_Handle) return Transaction_ID
   with Inline, Global => null, Pre => not Is_Null (Handle);

   function Indication_Reference
     (Handle : Service_Handle) return not null access constant Indication_Type
   with Global => null, Pre => not Is_Null (Handle);

   function Indication_Kind
     (Handle : Service_Handle) return Indication_Kind_Type
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Indication_Kind'Result
       = Indication_Kind (Indication_Reference (Handle).all);

   function Requires_Response (Handle : Service_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Requires_Response'Result
       = Requires_Response (Indication_Reference (Handle).all);

   function Response_Written (Handle : Service_Handle) return Boolean
   with Global => null, Pre => not Is_Null (Handle);

   function Response_Reference
     (Handle : Service_Handle) return not null access constant Response_Type
   with
     Global => null,
     Pre    =>
       not Is_Null (Handle)
       and then Response_Written (Handle)
       and then Requires_Response (Handle);

   procedure Move
     (Target : in out Service_Handle; Source : in out Service_Handle)
   with
     Global => null,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       not Is_Null (Target)
       and Is_Null (Source)
       and (Is_Null (Target) = Is_Null (Source)'Old)
       and (Requires_Response (Target) = Requires_Response (Source)'Old)
       and (Response_Written (Target) = Response_Written (Source)'Old)
       and (Indication_Kind (Target) = Indication_Kind (Source)'Old);

   ---------------------------------
   -- Service Provider Operations --
   ---------------------------------

   procedure Try_Allocate_Indication (Handle : in out Indication_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   =>
       (if not Is_Null (Handle)
        then
          not Indication_Written (Handle)
          and then
            not Indication_Requires_Cleanup
                  (Indication_Reference (Handle).all));
   --  Attempts to allocate memory to initiate a new service transaction.
   --
   --  If allocation is successful, the provided `Indication_Handle` is
   --  populated, granting exclusive write access to the transaction's
   --  indication primitive.
   --
   --  This is a non-blocking operation.

   procedure Send_Indication
     (Handle : in out Indication_Handle; Promise : in out Response_Promise)
   with
     Inline,
     Pre            =>
       not Is_Null (Handle)
       and then Is_Null (Promise)
       and then Indication_Written (Handle)
       and then Valid_Indication (Indication_Reference (Handle).all),
     Post           => Is_Null (Handle),
     Contract_Cases =>
       (Requires_Response (Handle) =>
          not Is_Null (Promise)
          and (Get_TID (Promise) = Get_TID (Handle)'Old)
          and (Indication_Kind (Promise) = Indication_Kind (Handle)'Old),
        others                     => Is_Null (Promise));
   --  Dispatches a prepared indication primitive from the Service Provider to
   --  the Service User via the global transaction queue.
   --
   --  If the prepared indication expects a response primitive, then a
   --  `Response_Promise` is given which grants the caller access to retrieve
   --  the response primitive in the future, once it has been sent by the
   --  Service User.
   --
   --  This is a non-blocking operation.

   procedure Abort_Indication (Handle : in out Indication_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    =>
       not Is_Null (Handle)
       and then
         not Indication_Requires_Cleanup (Indication_Reference (Handle).all),
     Post   => Is_Null (Handle);
   --  Abandons an allocated indication transaction, reclaiming its resources
   --  and releasing its memory back to the SAP without dispatching it.
   --
   --  This procedure acts as a cancellation mechanism in cases where the
   --  caller allocates an indication handle, but subsequently determines it
   --  should not or cannot be sent.
   --
   --  This is a non-blocking operation.

   procedure Discard (Promise : in out Response_Promise)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => not Might_Require_Cleanup (Indication_Kind (Promise)),
     Post   => Is_Null (Promise);
   --  Releases resources associated with a promise that is no longer needed.
   --
   --  This is intended for cases when the caller has sent an indication that
   --  expects a response primitive to be sent back, but the caller does not
   --  care about reading the response primitive.
   --
   --  This procedure immediately invalidates the promise handle, preventing
   --  the caller from waiting on or retrieving the future value. It must only
   --  be called on promises that do not require complex cleanup or
   --  finalization. If the transaction might require cleanup, then the caller
   --  cannot discard the `Response_Promise` and must read the response and
   --  perform any required cleanup before releasing the transaction.
   --
   --  Discarding a `Response_Promise` does not prevent the original indication
   --  from being processed by the Service User; it only causes the
   --  transaction's resources to be automatically released when the Service
   --  User sends the response primitive.

   procedure Try_Get_Response
     (Handle : in out Response_Handle; Promise : in out Response_Promise)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle) and then not Is_Null (Promise),
     Post   =>
       (Is_Null (Handle) = not Is_Null (Promise))
       and
         (Get_TID (Promise)'Old
          = (if not Is_Null (Handle)
             then Get_TID (Handle)
             else Get_TID (Promise)))
       and
         (if not Is_Null (Handle)
          then Indication_Kind (Handle)
          else Indication_Kind (Promise))
         = Indication_Kind (Promise)'Old
       and
         (if not Is_Null (Handle)
          then
            Valid_Response
              (Indication_Reference (Handle).all,
               Response_Reference (Handle).all));
   --  Attempt to get the pending response primitive associated with a
   --  `Response_Promise`.
   --
   --  If the response primitive has been sent by the Service User, then
   --  `Handle` is set to hold a reference to the primitive and `Promise` is
   --  set to null. Otherwise, if the Service User has not yet sent the
   --  response primitive, then both `Handle` and `Promise` are unchanged.
   --
   --  This is a non-blocking operation.

   procedure Release (Handle : in out Response_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => not Is_Null (Handle) and then not Requires_Cleanup (Handle),
     Post   => Is_Null (Handle);
   --  Finalizes a transaction and releases all resources held by it.
   --
   --  This is called by the Service Provider at the end of the transaction
   --  lifecycle; when the caller has finished processing a response primitive.
   --
   --  Any cleanup required by the transaction must be done before calling
   --  this procedure. See the generic procedure `Cleanup`.
   --
   --  This is a non-blocking operation.

   procedure New_Indication
     (Res_Handle : in out Response_Handle;
      Ind_Handle : in out Indication_Handle)
   with
     Global => null,
     Pre    =>
       not Is_Null (Res_Handle)
       and then Is_Null (Ind_Handle)
       and then not Requires_Cleanup (Res_Handle),
     Post   =>
       not Is_Null (Ind_Handle)
       and Is_Null (Res_Handle)
       and (Get_TID (Ind_Handle) = Get_TID (Res_Handle)'Old);
   --  Recycles an existing, completed transaction to begin a new transaction.
   --
   --  This is equivalent to calling `Release` then `Try_Allocate_Indication`,
   --  but eliminates the possibility of allocation failure if another task
   --  jumps in and steals the transaction resources between the two calls.
   --
   --  This is a non-blocking operation.

   -----------------------------
   -- Service User Operations --
   -----------------------------

   function Has_Pending_Indication return Boolean
   with Global => (Input => Transaction_Queue), Volatile_Function;
   --  Returns True if there is at least one pending indication, or False
   --  otherwise.

   procedure Get_Next_Indication (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   =>
       not Is_Null (Handle)
       and then Valid_Indication (Indication_Reference (Handle).all)
       and then not Response_Written (Handle);
   --  Attempts to retrieve the next available service indication from the
   --  global transaction queue.
   --
   --  This is a potentially blocking operation.
   --
   --  If no indication is pending, then this procedure blocks until a
   --  indication is sent via `Send_Indication`.

   procedure Try_Get_Next_Indication (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   =>
       (if not Is_Null (Handle)
        then
          Valid_Indication (Indication_Reference (Handle).all)
          and then not Response_Written (Handle));
   --  Attempts to retrieve the next available indication primitive from the
   --  global transaction queue.
   --
   --  This is a non-blocking operation.

   procedure Release (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    =>
       not Is_Null (Handle)
       and then not Requires_Response (Handle)
       and then
         not Indication_Requires_Cleanup (Indication_Reference (Handle).all),
     Post   => Is_Null (Handle);
   --  Finalizes a transaction and releases all resources held by it.
   --
   --  This is called by the Service User when it has finished processing
   --  an indication that does not require a response primitive.
   --
   --  Any cleanup required by the transaction must be done before calling
   --  this procedure. This can be done by using `Consume_Indication`.
   --
   --  This is a non-blocking operation.

   procedure Send_Response (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    =>
       not Is_Null (Handle)
       and then Requires_Response (Handle)
       and then Response_Written (Handle)
       and then
         Valid_Response
           (Indication_Reference (Handle).all,
            Response_Reference (Handle).all),
     Post   => Is_Null (Handle);
   --  Send a response primitive to a Service Provider.
   --
   --  This must be called when the Service User has finished processing an
   --  indication and has prepared a response primitive.
   --
   --  This is a non-blocking operation.

   generic
      with
        procedure Initialize
          (Indication : Indication_Type; Response : out Response_Type);

      with
        function Precondition (Indication : Indication_Type) return Boolean
        is Always_True;

      with
        function Postcondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;
   procedure Initialize_Response (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then not Response_Written (Handle)
       and then Precondition (Indication_Reference (Handle).all)
       and then Requires_Response (Handle),
     Post =>
       not Is_Null (Handle)
       and (Requires_Response (Handle) = Requires_Response (Handle)'Old)
       and (Indication_Kind (Handle) = Indication_Kind (Handle)'Old)
       and Response_Written (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
       and
         Postcondition
           (Indication_Reference (Handle).all,
            Response_Reference (Handle).all);
   --  Initializes a response primitive.
   --
   --  The response primitive is passed to the `Initialize` procedure, which
   --  writes to it.

   generic
      with
        procedure Update
          (Indication : Indication_Type; Response : in out Response_Type);

      with
        function Precondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;

      with
        function Postcondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;
   procedure Update_Response (Handle : in out Service_Handle)
   with
     Inline,
     Pre  =>
       not Is_Null (Handle)
       and then Response_Written (Handle)
       and then Requires_Response (Handle)
       and then
         Precondition
           (Indication_Reference (Handle).all,
            Response_Reference (Handle).all),
     Post =>
       not Is_Null (Handle)
       and (Indication_Kind (Handle) = Indication_Kind (Handle)'Old)
       and (Requires_Response (Handle) = Requires_Response (Handle)'Old)
       and Response_Written (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
       and
         Postcondition
           (Indication_Reference (Handle).all,
            Response_Reference (Handle).all);
   --  Modifies a previously written response payload.
   --
   --  This procedure utilizes a generic callback (`Update`) to directly
   --  update the response object in place, supporting zero-copy message
   --  passing.
   --
   --  The Precondition and Postcondition generic formal functions are optional
   --  proof bridge functions. They are used to pass verification context from
   --  the caller to the call to the `Update` procedure.

   generic
      with procedure Consume (Indication : in out Indication_Type);

      with
        function Precondition (Indication : Indication_Type) return Boolean
        is Always_True;

      with
        function Postcondition (Indication : Indication_Type) return Boolean
        is Always_True;
   procedure Consume_Indication (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then Precondition (Indication_Reference (Handle).all),
     Post =>
       not Is_Null (Handle)
       and (Indication_Kind (Handle) = Indication_Kind (Handle)'Old)
       and (Requires_Response (Handle) = Requires_Response (Handle)'Old)
       and (Response_Written (Handle) = Response_Written (Handle)'Old)
       and Postcondition (Indication_Reference (Handle).all);
   --  Modify a indication object.
   --
   --  The purpose of this procedure is to provide a way to "consume" data from
   --  the indication object by modifying some fields of the indication.
   --  For example, to take ownership over a pointer field in the indication,
   --  which requires setting it to null in the indication.
   --
   --  The Consume general formal procedure must not modify the Indication_Kind
   --  or Requires_Response properties on the indication, and this must be
   --  specified in the postcondition for Consume.

   generic
      with
        procedure Initialize
          (Indication : in out Indication_Type; Response : out Response_Type);

      with
        function Precondition (Indication : Indication_Type) return Boolean
        is Always_True;

      with
        function Postcondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;
   procedure Consume_Indication_And_Initialize_Response
     (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then not Response_Written (Handle)
       and then Precondition (Indication_Reference (Handle).all)
       and then Requires_Response (Handle),
     Post =>
       not Is_Null (Handle)
       and (Indication_Kind (Handle) = Indication_Kind (Handle)'Old)
       and (Requires_Response (Handle) = Requires_Response (Handle)'Old)
       and Response_Written (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
       and
         Postcondition
           (Indication_Reference (Handle).all,
            Response_Reference (Handle).all);
   --  Initialize a Response primitive with the ability to consume data from
   --  the Indication primitive.
   --
   --  This is the same as Initialize_Response, except with the ability to
   --  modify the indication object.
   --
   --  This is intended for use with primitives that have ownership semantics.
   --  It allows pointer values in the Indication primitive to be moved
   --  elsewhere, which requires the ability to write to the indication to set
   --  the pointer to null.

   generic
      with
        procedure Update
          (Indication : in out Indication_Type;
           Response   : in out Response_Type);

      with
        function Precondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;

      with
        function Postcondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;
   procedure Consume_Indication_And_Update_Response
     (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then Response_Written (Handle)
       and then Requires_Response (Handle)
       and then
         Precondition
           (Indication_Reference (Handle).all,
            Response_Reference (Handle).all),
     Post =>
       not Is_Null (Handle)
       and (Indication_Kind (Handle) = Indication_Kind (Handle)'Old)
       and (Requires_Response (Handle) = Requires_Response (Handle)'Old)
       and Response_Written (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
       and
         Postcondition
           (Indication_Reference (Handle).all,
            Response_Reference (Handle).all);
   --  Modifies a previously written response payload with the ability to
   --  consume data from the Indication primitive.
   --
   --  This procedure utilizes a generic callback (`Update`) to directly
   --  update the response object in place, supporting zero-copy message
   --  passing.
   --
   --  This is the same as the `Update_Response` procedure, except with the
   --  ability to modify the request object.
   --
   --  The Precondition and Postcondition generic formal functions are optional
   --  proof bridge functions. They are used to pass verification context from
   --  the caller to the call to the `Update` procedure.

private

   package STQ is new
     LibSAP.Singleton_Transaction_Queues
       (Request_Kind_Type        => Indication_Kind_Type,
        Request_Type             => Indication_Type,
        Confirm_Type             => Response_Type,
        Queue_Capacity           => Queue_Capacity,
        Request_Kind             => Indication_Kind,
        Requires_Confirm         => Requires_Response,
        Request_Requires_Cleanup => Indication_Requires_Cleanup,
        Confirm_Requires_Cleanup => Response_Requires_Cleanup,
        Might_Require_Cleanup    => Might_Require_Cleanup,
        Valid_Request            => Valid_Indication,
        Valid_Confirm            => Valid_Response);
   pragma Part_Of (Transaction_Queue);

   type Indication_Handle is limited record
      Handle : STQ.Request_Handle;
   end record;

   type Response_Handle is limited record
      Handle : STQ.Confirm_Handle;
   end record;

   type Service_Handle is limited record
      Handle : STQ.Service_Handle;
   end record;

   type Response_Promise is limited record
      Handle : STQ.Confirm_Promise;
   end record;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Handle : Indication_Handle) return Boolean
   is (STQ.Is_Null (Handle.Handle));

   function Is_Null (Handle : Response_Handle) return Boolean
   is (STQ.Is_Null (Handle.Handle));

   function Is_Null (Handle : Service_Handle) return Boolean
   is (STQ.Is_Null (Handle.Handle));

   function Is_Null (Promise : Response_Promise) return Boolean
   is (STQ.Is_Null (Promise.Handle));

   -------------
   -- Get_TID --
   -------------

   function Get_TID (Handle : Indication_Handle) return Transaction_ID
   is (Transaction_ID (STQ.Get_TID (Handle.Handle)));

   function Get_TID (Promise : Response_Promise) return Transaction_ID
   is (Transaction_ID (STQ.Get_TID (Promise.Handle)));

   function Get_TID (Handle : Response_Handle) return Transaction_ID
   is (Transaction_ID (STQ.Get_TID (Handle.Handle)));

   function Get_TID (Handle : Service_Handle) return Transaction_ID
   is (Transaction_ID (STQ.Get_TID (Handle.Handle)));

   ---------------------
   -- Indication_Kind --
   ---------------------

   function Indication_Kind
     (Handle : Indication_Handle) return Indication_Kind_Type
   is (STQ.Request_Kind (Handle.Handle));

   function Indication_Kind
     (Promise : Response_Promise) return Indication_Kind_Type
   is (STQ.Request_Kind (Promise.Handle));

   function Indication_Kind
     (Handle : Response_Handle) return Indication_Kind_Type
   is (STQ.Request_Kind (Handle.Handle));

   function Indication_Kind
     (Handle : Service_Handle) return Indication_Kind_Type
   is (STQ.Request_Kind (Handle.Handle));

   ----------------------
   -- Requires_Cleanup --
   ----------------------

   function Requires_Cleanup (Handle : Response_Handle) return Boolean
   is (STQ.Requires_Cleanup (Handle.Handle));

   ------------------------
   -- Indication_Written --
   ------------------------

   function Indication_Written (Handle : Indication_Handle) return Boolean
   is (STQ.Request_Written (Handle.Handle));

   ----------------------
   -- Response_Written --
   ----------------------

   function Response_Written (Handle : Service_Handle) return Boolean
   is (STQ.Confirm_Written (Handle.Handle));

   ------------------
   -- Empty_Handle --
   ------------------

   function Empty_Handle return Indication_Handle is
   (Indication_Handle'(Handle => STQ.Empty_Handle));

   function Empty_Promise return Response_Promise is
   (Response_Promise'(Handle => STQ.Empty_Promise));

   function Empty_Handle return Response_Handle is
   (Response_Handle'(Handle => STQ.Empty_Handle));

   function Empty_Handle return Service_Handle is
   (Service_Handle'(Handle => STQ.Empty_Handle));

end LibSAP.Synchronous_User_Service_Access_Point;
