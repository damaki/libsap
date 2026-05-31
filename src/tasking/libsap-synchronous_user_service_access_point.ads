--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System;

private with LibSAP.Singleton_Transaction_Queues;

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
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       (if Indication_Written'Result
        then Valid_Indication (Indication_Reference (Handle).all));

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
      with procedure Build (Indication : out Indication_Type);
      with function Precondition return Boolean is Always_True;
      with
        function Postcondition (Indication : Indication_Type) return Boolean
        is Always_True;
   procedure Build_Indication (Handle : in out Indication_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then Precondition
       and then not Indication_Written (Handle),
     Post =>
       not Is_Null (Handle)
       and Indication_Written (Handle)
       and Postcondition (Indication_Reference (Handle).all)
       and (Get_TID (Handle) = Get_TID (Handle)'Old);
   --  Write an indication primitive.
   --
   --  The indication object is passed to the Build generic formal procedure,
   --  which does the actual write.
   --
   --  The Precondition can be used to express the information needed to prove
   --  the precondition of Build.
   --
   --  Postcondition can be used to express information from Build's
   --  postcondition that is needed after calling this function.

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

   ---------------------
   -- Service Handles --
   ---------------------

   type Service_Handle is limited private
   with Default_Initial_Condition => Is_Null (Service_Handle);

   function Is_Null (Handle : Service_Handle) return Boolean
   with Global => null;

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

   function Indication_Consumed (Handle : Service_Handle) return Boolean
   with Global => null, Pre => not Is_Null (Handle);

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
       and (Indication_Consumed (Target) = Indication_Consumed (Source)'Old)
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
   --  Try to allocate a new indication object.
   --
   --  If there is enough free resources for a new transaction, then one is
   --  allocated and stored in the Handle. If there are no free resources, then
   --  Handle is set to null.
   --
   --  This is a non-blocking operation.

   procedure Send_Indication
     (Handle : in out Indication_Handle; Promise : in out Response_Promise)
   with
     Inline,
     Global         => (In_Out => Transaction_Queue),
     Pre            =>
       not Is_Null (Handle)
       and then Is_Null (Promise)
       and then Indication_Written (Handle),
     Post           => Is_Null (Handle),
     Contract_Cases =>
       (Requires_Response (Handle) =>
          not Is_Null (Promise)
          and (Get_TID (Promise) = Get_TID (Handle)'Old)
          and (Indication_Kind (Promise) = Indication_Kind (Handle)'Old),
        others                     => Is_Null (Promise));
   --  Send a prepared indication to the Service User.
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
   --  Abort a indication.
   --
   --  This frees up the resources held by Handle without sending the
   --  indication to the Service User.
   --
   --  This is a non-blocking operation.

   procedure Discard (Promise : in out Response_Promise)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => not Might_Require_Cleanup (Indication_Kind (Promise)),
     Post   => Is_Null (Promise);
   --  Discard a response promise.
   --
   --  This should be used if the Service Provider decides that they no longer
   --  need the response to an indication.
   --
   --  Note that this does not prevent the Service User from seeing and
   --  processing the indication, but rather ensures that any resources used
   --  for the transaction are released when the Service User sends the
   --  response.

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
   --  Try to get the pending response primitive from a Promise.
   --
   --  If the pending response primitive has been sent by the Service User,
   --  then Handle is set to hold a reference to the primitive and Promise is
   --  set to null. Otherwise, if the Service User has not yet sent the
   --  response primitive, then both Handle and Promise are unchanged.
   --
   --  This is a non-blocking operation.

   procedure Release (Handle : in out Response_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => not Is_Null (Handle) and then not Requires_Cleanup (Handle),
     Post   => Is_Null (Handle);
   --  Release a response handle.
   --
   --  This must be called when the Service Provider has finished reading the
   --  response primitive to reliquish the resources held by the handle.
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
   --  Finish the transaction held by Res_Handle and begin a new transaction
   --  in Ind_Handle.
   --
   --  This is useful to begin a new indication without needing to reallocate a
   --  new handle.
   --
   --  This is a non-blocking operation.

   -----------------------------
   -- Service User Operations --
   -----------------------------

   function Has_Pending_Indication return Boolean
   with Global => (Input => Transaction_Queue), Volatile_Function;

   procedure Get_Next_Indication (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   =>
       not Is_Null (Handle)
       and then Valid_Indication (Indication_Reference (Handle).all)
       and then not Response_Written (Handle)
       and then not Indication_Consumed (Handle);
   --  Wait for an indication from a Service Provider.
   --
   --  This is a potentially blocking operation.

   procedure Try_Get_Next_Indication (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   =>
       (if not Is_Null (Handle)
        then
          Valid_Indication (Indication_Reference (Handle).all)
          and then not Response_Written (Handle)
          and then not Indication_Consumed (Handle));
   --  Get the next indication from a Service Provider, if one is currently
   --  pending.
   --
   --  This is a non-blocking operation.

   procedure Release (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => not Is_Null (Handle) and then not Requires_Response (Handle),
     Post   => Is_Null (Handle);
   --  Release a service handle.
   --
   --  This must be called when the Service Provider has finished processing an
   --  indication that does not require a response primitive. This releases any
   --  resources held by the handle.
   --
   --  This is a non-blocking operation.

   procedure Send_Response (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    =>
       not Is_Null (Handle)
       and then Response_Written (Handle)
       and then Requires_Response (Handle),
     Post   => Is_Null (Handle);
   --  Send a response primitive to a Service Provider.
   --
   --  This must be called when the Service User has finished processing an
   --  indication and has prepared a response primitive.
   --
   --  This is a non-blocking operation.

   generic
      with
        procedure Process_Indication_No_Response
          (Indication : Indication_Type);

      with
        procedure Process_Indication_With_Response
          (Indication : Indication_Type; Response : out Response_Type);

      with function Precondition return Boolean is Always_True;

      with
        function Postcondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;
   procedure Process_Indication (Handle : in out Service_Handle)
   with
     Pre  => not Is_Null (Handle) and then Precondition,
     Post =>
       not Is_Null (Handle)
       and (Requires_Response (Handle) = Requires_Response (Handle)'Old)
       and (Indication_Kind (Handle) = Indication_Kind (Handle)'Old)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
       and
         (if Requires_Response (Handle)'Old
          then
            Response_Written (Handle)
            and then
              Postcondition
                (Indication_Reference (Handle).all,
                 Response_Reference (Handle).all));
   --  Process an indication, and generate a response if one is required.
   --
   --  This procedure passes the indication to either
   --  Process_Indication_No_Response or Process_Indication_With_Response,
   --  depending on whether a response is required.

   generic
      with
        procedure Build
          (Indication : Indication_Type; Response : out Response_Type);

      with function Precondition return Boolean is Always_True;

      with
        function Postcondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;
   procedure Build_Response (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then Precondition
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
   --  Builds a response primitive.
   --
   --  The response primitive is passed to the Build procedure, which writes
   --  to it.

   generic
      with procedure Consume (Indication : in out Indication_Type);
      with function Precondition return Boolean;
      with
        function Postcondition (Indication : Indication_Type) return Boolean;
   procedure Consume_Indication (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then not Indication_Consumed (Handle)
       and then Precondition,
     Post =>
       not Is_Null (Handle)
       and (Indication_Kind (Handle) = Indication_Kind (Handle)'Old)
       and (Requires_Response (Handle) = Requires_Response (Handle)'Old)
       and Indication_Consumed (Handle)
       and Postcondition (Indication_Reference (Handle).all);
   --  Modify an indication object.
   --
   --  The purpose of this procedure is to provide a way to "consume" data from
   --  the indication object by modifying some fields of the indication.
   --  For example, to take ownership over a pointer field in the indication,
   --  which requires setting it to null in the process.
   --
   --  The Consume general formal procedure must not modify the Indication_Kind
   --  or Requires_Response properties on the indication, and this must be
   --  specified in the postcondition for Consume.

   generic
      with
        procedure Build
          (Indication : in out Indication_Type; Response : out Response_Type);

      with
        function Precondition (Indication : Indication_Type) return Boolean
        is Always_True;

      with
        function Postcondition
          (Indication : Indication_Type; Response : Response_Type)
           return Boolean is Always_True;
   procedure Consume_Indication_And_Build_Response
     (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then not Indication_Consumed (Handle)
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
   --  Build a Response primitive with the ability to consume data from the
   --  Indication primitive.
   --
   --  This is intended for use with primitives that have ownership semantics.
   --  It allows pointer values in the Indication primitive to be moved
   --  elsewhere, which requires the ability to write to the request to set the
   --  pointer to null.

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

   -------------------------
   -- Indication_Consumed --
   -------------------------

   function Indication_Consumed (Handle : Service_Handle) return Boolean
   is (STQ.Request_Consumed (Handle.Handle));

end LibSAP.Synchronous_User_Service_Access_Point;
