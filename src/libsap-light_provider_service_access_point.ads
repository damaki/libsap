--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

private with LibSAP.Singleton_Transaction_Queues;

--  This package defines a Service Access Point (SAP) for transferring request
--  and confirm primitives between between a Service User and Service Provider.
--
--  This package does not use any of Ada's tasking mechanisms and is compatible
--  with the "light" GNAT runtime profile. It is therefore not safe to access
--  the SAP from multiple tasks without additional protection.
--
--  Transactions are initiated by a Service User by sending an request
--  primitive. The Service Provider can then retrieve the request, then send
--  a confirm primitive back to the Service User if one is required by the
--  original request.

generic
   type Request_Kind_Type is (<>);
   --  Discrete type (e.g. enumeration) to distinguish between different kinds
   --  of requests.
   --
   --  This is typically an enumeration of all the different kinds of requests
   --  that can be sent through the SAP, but any discrete type will also work.

   type Request_Type is limited private;
   --  The data type for request primitives.
   --
   --  This type must be a definite, unconstrained type with default
   --  initialization.

   type Confirm_Type is limited private;
   --  The data type for confirm primitives.
   --
   --  This type must be a definite, unconstrained type with default
   --  initialization.

   Queue_Capacity : Positive;
   --  Configures the maximum number of concurrent transactions

   with
     function Request_Kind (Request : Request_Type) return Request_Kind_Type;
   --  Returns the Kind of a request.
   --
   --  This is used to distinguish between different kinds of requests.
   --  For example, to distinguish between a CONNECT.req and DATA.req, which
   --  may have different requirements around cleanup.

   with function Requires_Confirm (Request : Request_Type) return Boolean;
   --  Returns True if the Request requires a confirm primitive to be sent in
   --  response, or False otherwise.

   with
     function Request_Requires_Cleanup (Request : Request_Type) return Boolean;
   --  Returns True if the Request object requires explicit cleanup before
   --  it can be released at the end of the transaction.
   --
   --  This is intended for cases when the Request contains a non-null pointer
   --  field, in which case the pointer needs to be either freed or moved
   --  before the transaction is completed.
   --
   --  If the Request object never has ownership semantics, then this function
   --  always returns False.
   --
   --  This must always return False for a default-initialized Request_Type.

   with
     function Confirm_Requires_Cleanup (Confirm : Confirm_Type) return Boolean;
   --  Returns True if the Confirm object requires explicit cleanup before
   --  it can be released at the end of the transaction.
   --
   --  This is intended for cases when the Confirm contains a non-null pointer
   --  field, in which case the pointer needs to be either freed or moved
   --  before the transaction is completed.
   --
   --  If the Confirm object never has ownership semantics, then this function
   --  always returns False.
   --
   --  This must always return False for a default-initialized Confirm_Type.

   with
     function Might_Require_Cleanup (Kind : Request_Kind_Type) return Boolean;
   --  Returns True if a Request OR Confirm primitive of this Kind might
   --  require cleanup before they are freed at the end of a transaction.
   --
   --  This is intended for cases when certain kinds of requests (and/or
   --  related confirm primitives) contain parameters that have ownership
   --  semantics in SPARK (e.g. pointers).
   --
   --  This must return True if any request primitive of type Kind, or related
   --  confirm primitives, contain one or more parameters that have ownership
   --  semantics. Otherwise, it should return False.

   with function Valid_Request (Request : Request_Type) return Boolean;
   --  Returns True if the Request object is valid
   --
   --  This can be used to validate the contents of a request to ensure that
   --  only well-formed requests are sent via the SAP.

   with
     function Valid_Confirm
       (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   --  Returns True if the Confirm object is valid for the given Request
   --
   --  This can be used to validate the contents of a confirm primitive to
   --  ensure that only valid confirmations are sent in response to a request.

package LibSAP.Light_Provider_Service_Access_Point with
    Elaborate_Body,
    Abstract_State => (Transaction_Queue, (Transaction_Pool with Synchronous))
is

   type Transaction_ID is new Positive range 1 .. Queue_Capacity;

   function Always_True
     (Request : Request_Type with Unreferenced) return Boolean
   is (True);

   function Always_True
     (Request : Request_Type with Unreferenced;
      Confirm : Confirm_Type with Unreferenced) return Boolean
   is (True);

   ---------------------
   -- Request Handles --
   ---------------------

   --  A request handle holds a reference to a request object.
   --
   --  It is used by the Service User to build a request and send it to the
   --  Service Provider.

   type Request_Handle is limited private
   with Default_Initial_Condition => Is_Null (Request_Handle);

   function Is_Null (Handle : Request_Handle) return Boolean
   with Inline, Global => null;

   function Get_TID (Handle : Request_Handle) return Transaction_ID
   with Inline, Global => null, Pre => not Is_Null (Handle);

   function Request_Reference
     (Handle : Request_Handle) return not null access constant Request_Type
   with Global => null, Pre => not Is_Null (Handle);

   function Request_Kind (Handle : Request_Handle) return Request_Kind_Type
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Request_Kind'Result = Request_Kind (Request_Reference (Handle).all);

   function Requires_Confirm (Handle : Request_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Requires_Confirm'Result
       = Requires_Confirm (Request_Reference (Handle).all);

   function Request_Written (Handle : Request_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle);

   procedure Move
     (Target : in out Request_Handle; Source : in out Request_Handle)
   with
     Global => null,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       not Is_Null (Target)
       and Is_Null (Source)
       and (Is_Null (Target) = Is_Null (Source)'Old)
       and (Requires_Confirm (Target) = Requires_Confirm (Source)'Old)
       and (Request_Written (Target) = Request_Written (Source)'Old)
       and (Request_Kind (Target) = Request_Kind (Source)'Old);

   generic
      with procedure Initialize (Request : out Request_Type);
      with function Precondition return Boolean is Always_True;
      with
        function Postcondition (Request : Request_Type) return Boolean
        is Always_True;
   procedure Initialize_Request (Handle : in out Request_Handle)
   with
     Inline,
     Pre  =>
       not Is_Null (Handle)
       and then Precondition
       and then not Request_Written (Handle),
     Post =>
       not Is_Null (Handle)
       and Request_Written (Handle)
       and Postcondition (Request_Reference (Handle).all)
       and (Get_TID (Handle) = Get_TID (Handle)'Old);
   --  Builds the request payload for an active, unwritten transaction handle.
   --
   --  This procedure utilizes a generic callback (`Initialize`) to directly
   --  populate the request object in place, supporting zero-copy message
   --  passing.
   --
   --  The Precondition and Postcondition generic formal functions are optional
   --  proof bridge functions. They are used to pass verification context from
   --  the caller to the call to the `Initialize` procedure.

   generic
      with procedure Update (Request : in out Request_Type);
      with function Precondition (Request : Request_Type) return Boolean;
      with function Postcondition (Request : Request_Type) return Boolean;
   procedure Update_Request (Handle : in out Request_Handle)
   with
     Inline,
     Pre  =>
       not Is_Null (Handle)
       and then Precondition (Request_Reference (Handle).all)
       and then Request_Written (Handle),
     Post =>
       not Is_Null (Handle)
       and Request_Written (Handle)
       and Postcondition (Request_Reference (Handle).all)
       and (Get_TID (Handle) = Get_TID (Handle)'Old);
   --  Modifies a previously written request payload.
   --
   --  This procedure utilizes a generic callback (`Initialize`) to directly
   --  update the request object in place, supporting zero-copy message
   --  passing.
   --
   --  The Precondition and Postcondition generic formal functions are optional
   --  proof bridge functions. They are used to pass verification context from
   --  the caller to the call to the `Update` procedure.

   ---------------------
   -- Confirm Promise --
   ---------------------

   --  Confirm_Promise represents the eventual completion of a request that
   --  results in a confirm primitive from the Service Provider.
   --
   --  It is used by the Service User to get the corresponding confirm
   --  primitive for a previous request, after the Service Provider has
   --  processed the request and sent the confirm primitive.

   type Confirm_Promise is limited private
   with Default_Initial_Condition => Is_Null (Confirm_Promise);

   function Is_Null (Promise : Confirm_Promise) return Boolean;

   function Get_TID (Promise : Confirm_Promise) return Transaction_ID
   with Inline, Global => null, Pre => not Is_Null (Promise);

   function Request_Kind (Handle : Confirm_Promise) return Request_Kind_Type
   with Global => null;

   procedure Move
     (Target : in out Confirm_Promise; Source : in out Confirm_Promise)
   with
     Inline,
     Global => null,
     Pre    => Is_Null (Target),
     Post   =>
       (Is_Null (Target) = Is_Null (Source)'Old)
       and Is_Null (Source)
       and (Request_Kind (Target) = Request_Kind (Source)'Old);

   ---------------------
   -- Confirm Handles --
   ---------------------

   --  A confirm handle holds a reference to a confirm object.
   --
   --  It is used by the Service User to read the confirm primitive that was
   --  sent back by the Service Provider. The original request primitive can
   --  also be read via the handle.

   type Confirm_Handle is limited private
   with Default_Initial_Condition => Is_Null (Confirm_Handle);

   function Is_Null (Handle : Confirm_Handle) return Boolean
   with Inline, Global => null;

   function Get_TID (Handle : Confirm_Handle) return Transaction_ID
   with Inline, Global => null, Pre => not Is_Null (Handle);

   function Request_Reference
     (Handle : Confirm_Handle) return not null access constant Request_Type
   with Inline, Global => null, Pre => not Is_Null (Handle);

   function Confirm_Reference
     (Handle : Confirm_Handle) return not null access constant Confirm_Type
   with Inline, Global => null, Pre => not Is_Null (Handle);

   function Request_Kind (Handle : Confirm_Handle) return Request_Kind_Type
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Request_Kind'Result = Request_Kind (Request_Reference (Handle).all);

   function Requires_Cleanup (Handle : Confirm_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Requires_Cleanup'Result
       = (Request_Requires_Cleanup (Request_Reference (Handle).all)
          or else Confirm_Requires_Cleanup (Confirm_Reference (Handle).all));

   procedure Move
     (Target : in out Confirm_Handle; Source : in out Confirm_Handle)
   with
     Inline,
     Global => null,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       (Is_Null (Target) = Is_Null (Source)'Old)
       and Is_Null (Source)
       and (Request_Kind (Target) = Request_Kind (Source)'Old);

   generic
      with
        procedure Clean
          (Request : in out Request_Type; Confirm : in out Confirm_Type);

      with
        function Precondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean
        is Always_True;

      with
        function Postcondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean
        is Always_True;
   procedure Cleanup (Handle : in out Confirm_Handle)
   with
     Inline,
     Pre  =>
       not Is_Null (Handle)
       and then
         Precondition
           (Request_Reference (Handle).all, Confirm_Reference (Handle).all),
     Post =>
       not Is_Null (Handle)
       and then
         Postcondition
           (Request_Reference (Handle).all, Confirm_Reference (Handle).all)
       and then not Request_Requires_Cleanup (Request_Reference (Handle).all);
   --  Performs custom resource cleanup on an active transaction's primitives.
   --
   --  This procedure utilizes a generic callback (`Clean`) to safely extract,
   --  deallocate, or move parameters with SPARK ownership semantics (e.g.,
   --  pointers) out of the request and confirm primitives. This must occur
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

   function Get_TID (Handle : Service_Handle) return Transaction_ID
   with Inline, Global => null, Pre => not Is_Null (Handle);

   function Request_Reference
     (Handle : Service_Handle) return not null access constant Request_Type
   with Global => null, Pre => not Is_Null (Handle);

   function Requires_Confirm (Handle : Service_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Requires_Confirm'Result
       = Requires_Confirm (Request_Reference (Handle).all);

   function Request_Kind (Handle : Service_Handle) return Request_Kind_Type
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Request_Kind'Result = Request_Kind (Request_Reference (Handle).all);

   function Request_Consumed (Handle : Service_Handle) return Boolean
   with Global => null, Pre => not Is_Null (Handle);

   function Confirm_Written (Handle : Service_Handle) return Boolean
   with Global => null, Pre => not Is_Null (Handle);

   function Confirm_Reference
     (Handle : Service_Handle) return not null access constant Confirm_Type
   with
     Global => null,
     Pre    =>
       not Is_Null (Handle)
       and then Confirm_Written (Handle)
       and then Requires_Confirm (Handle);

   procedure Move
     (Target : in out Service_Handle; Source : in out Service_Handle)
   with
     Global => null,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       not Is_Null (Target)
       and Is_Null (Source)
       and (Is_Null (Target) = Is_Null (Source)'Old)
       and (Requires_Confirm (Target) = Requires_Confirm (Source)'Old)
       and (Confirm_Written (Target) = Confirm_Written (Source)'Old)
       and (Request_Consumed (Target) = Request_Consumed (Source)'Old)
       and (Request_Kind (Target) = Request_Kind (Source)'Old);

   -----------------------------
   -- Service User Operations --
   -----------------------------

   procedure Try_Allocate_Request (Handle : in out Request_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Pool),
     Pre    => Is_Null (Handle),
     Post   =>
       (if not Is_Null (Handle)
        then
          not Request_Written (Handle)
          and then
            not Request_Requires_Cleanup (Request_Reference (Handle).all));
   --  Attempts to allocate memory to initiate a new service transaction.
   --
   --  If allocation is successful, the provided `Request_Handle` is populated,
   --  granting exclusive write access to the transaction's request primitive.

   procedure Send_Request
     (Handle : in out Request_Handle; Promise : in out Confirm_Promise)
   with
     Inline,
     Global         => (In_Out => Transaction_Queue),
     Pre            =>
       not Is_Null (Handle)
       and then Is_Null (Promise)
       and then Request_Written (Handle)
       and then Valid_Request (Request_Reference (Handle).all),
     Post           => Is_Null (Handle),
     Contract_Cases =>
       (Requires_Confirm (Handle) =>
          not Is_Null (Promise)
          and (Get_TID (Promise) = Get_TID (Handle)'Old)
          and (Request_Kind (Promise) = Request_Kind (Handle)'Old),
        others                    => Is_Null (Promise));
   --  Dispatches a prepared request primitive from the Service User to the
   --  Service Provider via the global transaction queue.
   --
   --  If the prepared request expects a confirm primitive to be sent in
   --  response, then a `Confirm_Promise` is given which grants the caller
   --  access to retrieve the confirm primitive in the future, once it has been
   --  sent by the Service Provider.

   procedure Abort_Request (Handle : in out Request_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Pool),
     Pre    =>
       not Is_Null (Handle)
       and then not Request_Requires_Cleanup (Request_Reference (Handle).all),
     Post   => Is_Null (Handle);
   --  Abandons an allocated request transaction, reclaiming its resources
   --  and releasing its memory back to the SAP without dispatching it.
   --
   --  This procedure acts as a cancellation mechanism in cases where the
   --  caller allocates an request handle, but subsequently determines it
   --  should not or cannot be sent.

   procedure Discard (Promise : in out Confirm_Promise)
   with
     Inline,
     Global => (In_Out => Transaction_Pool),
     Pre    => not Might_Require_Cleanup (Request_Kind (Promise)),
     Post   => Is_Null (Promise);
   --  Releases resources associated with a promise that is no longer needed.
   --
   --  This is intended for cases when the caller has sent a request that
   --  expects a confirm primitive to be sent back in response, but the caller
   --  does not care about reading the confirm primitive.
   --
   --  This procedure immediately invalidates the promise handle, preventing
   --  the caller from waiting on or retrieving the future value. It must only
   --  be called on promises that do not require complex cleanup or
   --  finalization. If the transaction might require cleanup, then the caller
   --  cannot discard the `Confirm_Promise` and must read the confirm and
   --  perform any required cleanup before releasing the transaction.
   --
   --  Discarding a `Confirm_Promise` does not prevent the original request
   --  from being processed by the Service Provider; it only causes the
   --  transaction's resources to be automatically released when the Service
   --  Provider sends the confirm primitive.

   procedure Try_Get_Confirm
     (Handle : in out Confirm_Handle; Promise : in out Confirm_Promise)
   with
     Inline,
     Global => (In_Out => Transaction_Pool),
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
          then Request_Kind (Handle)
          else Request_Kind (Promise))
         = Request_Kind (Promise)'Old
       and
         (if not Is_Null (Handle)
          then
            Valid_Confirm
              (Request_Reference (Handle).all,
               Confirm_Reference (Handle).all));
   --  Attempt to get the pending confirm primitive associated with a
   --  `Confirm_Promise`.
   --
   --  If the confirm primitive has been sent by the Service Provider, then
   --  `Handle` is set to hold a reference to the primitive and `Promise` is
   --  set to null. Otherwise, if the Service Provider has not yet sent the
   --  confirm primitive, then both `Handle` and `Promise` are unchanged.

   procedure Release (Handle : in out Confirm_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Pool),
     Pre    => not Is_Null (Handle) and then not Requires_Cleanup (Handle),
     Post   => Is_Null (Handle);
   --  Finalizes a transaction and releases all resources held by it.
   --
   --  This is called by the Service User at the end of the transaction
   --  lifecycle; when the caller has finished processing a confirm primitive.
   --
   --  Any cleanup required by the transaction must be done before calling
   --  this procedure. See the generic procedure `Cleanup`.

   procedure New_Request
     (Cfm_Handle : in out Confirm_Handle; Req_Handle : in out Request_Handle)
   with
     Global => null,
     Pre    =>
       not Is_Null (Cfm_Handle)
       and then Is_Null (Req_Handle)
       and then not Requires_Cleanup (Cfm_Handle),
     Post   =>
       not Is_Null (Req_Handle)
       and Is_Null (Cfm_Handle)
       and (Get_TID (Req_Handle) = Get_TID (Cfm_Handle)'Old);
   --  Recycles an existing, completed transaction to begin a new transaction.
   --
   --  This is equivalent to calling `Release` then `Try_Allocate_Request`, but
   --  eliminates the possibility of allocation failure if another task jumps
   --  in and steals the transaction resources between the two calls.

   ---------------------------------
   -- Service Provider Operations --
   ---------------------------------

   function Has_Pending_Request return Boolean
   with Global => (Input => Transaction_Queue);
   --  Returns True if there is at least one pending request, or False
   --  otherwise.

   procedure Try_Get_Next_Request (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   =>
       (if not Is_Null (Handle)
        then
          Valid_Request (Request_Reference (Handle).all)
          and then not Confirm_Written (Handle)
          and then not Request_Consumed (Handle));
   --  Attempts to retrieve the next available request primitive from the
   --  global transaction queue.

   procedure Release (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    =>
       not Is_Null (Handle)
       and then not Requires_Confirm (Handle)
       and then not Request_Requires_Cleanup (Request_Reference (Handle).all),
     Post   => Is_Null (Handle);
   --  Finalizes a transaction and releases all resources held by it.
   --
   --  This is called by the Service Provider when it has finished processing
   --  a request that does not require a confirm primitive to be sent in
   --  response.
   --
   --  Any cleanup required by the transaction must be done before calling
   --  this procedure. This can be done by using `Consume_Request`.

   procedure Send_Confirm (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    =>
       not Is_Null (Handle)
       and then Confirm_Written (Handle)
       and then Requires_Confirm (Handle),
     Post   => Is_Null (Handle);
   --  Send a confirm primitive to a Service User.
   --
   --  This must be called when the Service Provider has finished processing a
   --  request and has prepared a confirm primitive in response.

   generic
      with procedure Process_Request_No_Confirm (Request : Request_Type);

      with
        procedure Process_Request_With_Confirm
          (Request : Request_Type; Confirm : out Confirm_Type);

      with function Precondition return Boolean is Always_True;

      with
        function Postcondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean
        is Always_True;
   procedure Process_Request (Handle : in out Service_Handle)
   with
     Pre  => not Is_Null (Handle) and then Precondition,
     Post =>
       not Is_Null (Handle)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and (Request_Kind (Handle) = Request_Kind (Handle)'Old)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
       and Confirm_Written (Handle)
       and
         (if Requires_Confirm (Handle)'Old
          then
            Confirm_Written (Handle)
            and then
              Postcondition
                (Request_Reference (Handle).all,
                 Confirm_Reference (Handle).all));
   --  Process a request, and generate a confirm if one is required.
   --
   --  This procedure passes the request to either Process_Request_No_Confirm
   --  or Process_Request_With_Confirm, depending on whether a confirmation
   --  is required.

   generic
      with
        procedure Build (Request : Request_Type; Confirm : out Confirm_Type);

      with function Precondition return Boolean is Always_True;

      with
        function Postcondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean
        is Always_True;
   procedure Build_Confirm (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then Precondition
       and then Requires_Confirm (Handle),
     Post =>
       not Is_Null (Handle)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and (Request_Kind (Handle) = Request_Kind (Handle)'Old)
       and Confirm_Written (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
       and
         Postcondition
           (Request_Reference (Handle).all, Confirm_Reference (Handle).all);
   --  Builds a confirm primitive.
   --
   --  The confirm primitive is passed to the Build procedure, which writes
   --  to it.

   generic
      with procedure Consume (Request : in out Request_Type);
      with function Precondition return Boolean;
      with function Postcondition (Request : Request_Type) return Boolean;
   procedure Consume_Request (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then not Request_Consumed (Handle)
       and then Precondition,
     Post =>
       not Is_Null (Handle)
       and (Request_Kind (Handle) = Request_Kind (Handle)'Old)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and Request_Consumed (Handle)
       and Postcondition (Request_Reference (Handle).all);
   --  Modify a request object.
   --
   --  The purpose of this procedure is to provide a way to "consume" data from
   --  the request object by modifying some fields of the request. For example,
   --  to take ownership over a pointer field in the request, which requires
   --  setting it to null in the request.
   --
   --  The Consume general formal procedure must not modify the Request_Kind
   --  or Requires_Confirm properties on the request, and this must be
   --  specified in the postcondition for Consume.

   generic
      with
        procedure Build
          (Request : in out Request_Type; Confirm : out Confirm_Type);

      with
        function Precondition (Request : Request_Type) return Boolean
        is Always_True;

      with
        function Postcondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean
        is Always_True;
   procedure Consume_Request_And_Build_Confirm (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then not Request_Consumed (Handle)
       and then not Confirm_Written (Handle)
       and then Precondition (Request_Reference (Handle).all)
       and then Requires_Confirm (Handle),
     Post =>
       not Is_Null (Handle)
       and (Request_Kind (Handle) = Request_Kind (Handle)'Old)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and Confirm_Written (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
       and
         Postcondition
           (Request_Reference (Handle).all, Confirm_Reference (Handle).all);
   --  Build a Confirm primitive with the ability to consume data from the
   --  Request primitive.
   --
   --  This is intended for use with primitives that have ownership semantics.
   --  It allows pointer values in the Request primitive to be moved elsewhere,
   --  which requires the ability to write to the request to set the pointer
   --  to null.

private

   package STQ is new
     LibSAP.Singleton_Transaction_Queues
       (Request_Kind_Type        => Request_Kind_Type,
        Request_Type             => Request_Type,
        Confirm_Type             => Confirm_Type,
        Queue_Capacity           => Queue_Capacity,
        Request_Kind             => Request_Kind,
        Requires_Confirm         => Requires_Confirm,
        Request_Requires_Cleanup => Request_Requires_Cleanup,
        Confirm_Requires_Cleanup => Confirm_Requires_Cleanup,
        Might_Require_Cleanup    => Might_Require_Cleanup,
        Valid_Request            => Valid_Request,
        Valid_Confirm            => Valid_Confirm);
   pragma Part_Of (Transaction_Pool);

   type Request_Handle is limited record
      Handle : STQ.Request_Handle;
   end record;

   type Confirm_Handle is limited record
      Handle : STQ.Confirm_Handle;
   end record;

   type Service_Handle is limited record
      Handle : STQ.Service_Handle;
   end record;

   type Confirm_Promise is limited record
      Handle : STQ.Confirm_Promise;
   end record;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Handle : Request_Handle) return Boolean
   is (STQ.Is_Null (Handle.Handle));

   function Is_Null (Handle : Confirm_Handle) return Boolean
   is (STQ.Is_Null (Handle.Handle));

   function Is_Null (Handle : Service_Handle) return Boolean
   is (STQ.Is_Null (Handle.Handle));

   function Is_Null (Promise : Confirm_Promise) return Boolean
   is (STQ.Is_Null (Promise.Handle));

   -------------
   -- Get_TID --
   -------------

   function Get_TID (Handle : Request_Handle) return Transaction_ID
   is (Transaction_ID (STQ.Get_TID (Handle.Handle)));

   function Get_TID (Promise : Confirm_Promise) return Transaction_ID
   is (Transaction_ID (STQ.Get_TID (Promise.Handle)));

   function Get_TID (Handle : Confirm_Handle) return Transaction_ID
   is (Transaction_ID (STQ.Get_TID (Handle.Handle)));

   function Get_TID (Handle : Service_Handle) return Transaction_ID
   is (Transaction_ID (STQ.Get_TID (Handle.Handle)));

   ------------------
   -- Request_Kind --
   ------------------

   function Request_Kind (Handle : Request_Handle) return Request_Kind_Type
   is (STQ.Request_Kind (Handle.Handle));

   function Request_Kind (Handle : Confirm_Promise) return Request_Kind_Type
   is (STQ.Request_Kind (Handle.Handle));

   function Request_Kind (Handle : Confirm_Handle) return Request_Kind_Type
   is (STQ.Request_Kind (Handle.Handle));

   function Request_Kind (Handle : Service_Handle) return Request_Kind_Type
   is (STQ.Request_Kind (Handle.Handle));

   ----------------------
   -- Requires_Cleanup --
   ----------------------

   function Requires_Cleanup (Handle : Confirm_Handle) return Boolean
   is (STQ.Requires_Cleanup (Handle.Handle));

   ---------------------
   -- Request_Written --
   ---------------------

   function Request_Written (Handle : Request_Handle) return Boolean
   is (STQ.Request_Written (Handle.Handle));

   ----------------------
   -- Request_Consumed --
   ----------------------

   function Request_Consumed (Handle : Service_Handle) return Boolean
   is (STQ.Request_Consumed (Handle.Handle));

end LibSAP.Light_Provider_Service_Access_Point;
