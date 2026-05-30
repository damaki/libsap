--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System;

private with LibSAP.Singleton_Transaction_Queues;

generic
   type Request_Kind_Type is (<>);

   type Request_Type is limited private;
   type Confirm_Type is limited private;

   Queue_Capacity : Positive;
   --  Configures the maximum number of concurrent transactions

   with
     function Request_Kind (Request : Request_Type) return Request_Kind_Type;

   with function Requires_Confirm (Request : Request_Type) return Boolean;
   --  Returns true if the Request requires a confirm primitive to be sent in
   --  response.

   with
     function Request_Requires_Cleanup (Request : Request_Type) return Boolean;

   with
     function Confirm_Requires_Cleanup (Confirm : Confirm_Type) return Boolean;

   with
     function Might_Require_Cleanup (Kind : Request_Kind_Type) return Boolean;
   --  Returns True if a Request OR Confirm primitive of this kind might
   --  require cleanup before they are freed at the end of a transaction.

   with function Valid_Request (Request : Request_Type) return Boolean;
   --  Returns True if the Request object is valid

   with
     function Valid_Confirm
       (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   --  Returns True if the Confirm object is valid for the given Request

   Priority : System.Priority;

package LibSAP.Synchronous_Provider_Service_Access_Point with
    Elaborate_Body,
    Abstract_State => (Transaction_Queue with Synchronous)
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
     Pre    => not Is_Null (Handle),
     Post   =>
       (if Request_Written'Result
        then Valid_Request (Request_Reference (Handle).all));

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
      with procedure Build (Request : out Request_Type);
      with function Precondition return Boolean is Always_True;
      with
        function Postcondition (Request : Request_Type) return Boolean
        is Always_True;
   procedure Build_Request (Handle : in out Request_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then Precondition
       and then not Request_Written (Handle),
     Post =>
       not Is_Null (Handle)
       and Request_Written (Handle)
       and Postcondition (Request_Reference (Handle).all)
       and (Get_TID (Handle) = Get_TID (Handle)'Old);
   --  Write a request primitive.
   --
   --  The request object is passed to the Build generic formal procedure,
   --  which does the actual write.
   --
   --  The Precondition can be used to express the information needed to prove
   --  the precondition of Build.
   --
   --  Postcondition can be used to express information from Build's
   --  postcondition that is needed after calling this function.

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

      with function Precondition return Boolean is Always_True;

      with
        function Postcondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean
        is Always_True;
   procedure Cleanup (Handle : in out Confirm_Handle)
   with
     Inline,
     Global => null,
     Pre    => Precondition,
     Post   =>
       Postcondition
         (Request_Reference (Handle).all, Confirm_Reference (Handle).all)
       and then not Request_Requires_Cleanup (Request_Reference (Handle).all);

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
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       (if not Requires_Confirm (Handle) then not Confirm_Written'Result);

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
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   =>
       (if not Is_Null (Handle)
        then
          not Request_Written (Handle)
          and then
            not Request_Requires_Cleanup (Request_Reference (Handle).all));
   --  Try to allocate a new request object.
   --
   --  If there is enough free resources for a new transaction, then one is
   --  allocated and stored in the Handle. If there are no free resources, then
   --  Handle is set to null.
   --
   --  This is a non-blocking operation.

   procedure Send_Request
     (Handle : in out Request_Handle; Promise : in out Confirm_Promise)
   with
     Inline,
     Global         => (In_Out => Transaction_Queue),
     Pre            =>
       not Is_Null (Handle)
       and then Is_Null (Promise)
       and then Request_Written (Handle),
     Post           => Is_Null (Handle),
     Contract_Cases =>
       (Requires_Confirm (Handle) =>
          not Is_Null (Promise)
          and (Get_TID (Promise) = Get_TID (Handle)'Old)
          and (Request_Kind (Promise) = Request_Kind (Handle)'Old),
        others                    => Is_Null (Promise));
   --  Send a prepared request to the Service Provider.
   --
   --  This is a non-blocking operation.

   procedure Abort_Request (Handle : in out Request_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => not Is_Null (Handle),
     Post   => Is_Null (Handle);
   --  Abort a request.
   --
   --  This frees up the resources held by Handle without sending the request
   --  to the Service Provider.
   --
   --  This is a non-blocking operation.

   procedure Discard (Promise : in out Confirm_Promise)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => not Might_Require_Cleanup (Request_Kind (Promise)),
     Post   => Is_Null (Promise);
   --  Discard a confirm promise.
   --
   --  This should be used if the Service User decides that they no longer
   --  need the confirmation to a request.
   --
   --  Note that this does not prevent the Service Provider from seeing and
   --  processing the request, but rather ensures that any resources used for
   --  the transaction are released when the Service Provider sends the
   --  confirmation.

   procedure Try_Get_Confirm
     (Handle : in out Confirm_Handle; Promise : in out Confirm_Promise)
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
          then Request_Kind (Handle)
          else Request_Kind (Promise))
         = Request_Kind (Promise)'Old
       and
         (if not Is_Null (Handle)
          then
            Valid_Confirm
              (Request_Reference (Handle).all,
               Confirm_Reference (Handle).all));
   --  Try to get the pending confirm primitive from a Promise.
   --
   --  If the pending confirm primitive has been sent by the Service Provider,
   --  then Handle is set to hold a reference to the primitive and Promise is
   --  set to null. Otherwise, if the Service has not yet sent the confirm
   --  primitive, then both Handle and Promise are unchanged.
   --
   --  This is a non-blocking operation.

   procedure Release (Handle : in out Confirm_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => not Is_Null (Handle),
     Post   => Is_Null (Handle);
   --  Release a confirm handle.
   --
   --  This must be called when the Service User has finished reading the
   --  confirm primitive to reliquish the resources held by the handle.
   --
   --  This is a non-blocking operation.

   procedure New_Request
     (Cfm_Handle : in out Confirm_Handle; Req_Handle : in out Request_Handle)
   with
     Global => null,
     Pre    => not Is_Null (Cfm_Handle) and then Is_Null (Req_Handle),
     Post   =>
       not Is_Null (Req_Handle)
       and Is_Null (Cfm_Handle)
       and (Get_TID (Req_Handle) = Get_TID (Cfm_Handle)'Old);
   --  Finish the transaction held by Cfm_Handle and begin a new transaction
   --  in Req_Handle.
   --
   --  This is useful to begin a new request without needing to reallocate a
   --  new handle.
   --
   --  This is a non-blocking operation.

   ---------------------------------
   -- Service Provider Operations --
   ---------------------------------

   function Has_Pending_Request return Boolean
   with Global => (Input => Transaction_Queue), Volatile_Function;

   procedure Get_Next_Request (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   =>
       not Is_Null (Handle)
       and then Valid_Request (Request_Reference (Handle).all)
       and then not Confirm_Written (Handle)
       and then not Request_Consumed (Handle);
   --  Wait for a request from a Service User.
   --
   --  This is a potentially blocking operation.

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
   --  Get the next request from a Service User, if one is currently pending.
   --
   --  This is a non-blocking operation.

   procedure Release (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => not Is_Null (Handle) and then not Requires_Confirm (Handle),
     Post   => Is_Null (Handle);
   --  Release a service handle.
   --
   --  This must be called when the Service Provider has finished processing a
   --  request that does not require a confirm primitive in response. This
   --  releases any resources held by the handle.
   --
   --  This is a non-blocking operation.

   procedure Send_Confirm (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    =>
       not Is_Null (Handle)
       and then Confirm_Written (Handle)
       and then Requires_Confirm (Handle),
     Post   => Is_Null (Handle);
   --  Send a confirm primitive to a Service User.
   --
   --  This must be called when the Service Provider has finished processing a
   --  request and has prepared a confirm primitive in response.
   --
   --  This is a non-blocking operation.

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
     Pre  =>
       not Is_Null (Handle)
       and then Precondition
       and then not Confirm_Written (Handle),
     Post =>
       not Is_Null (Handle)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and (Request_Kind (Handle) = Request_Kind (Handle)'Old)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
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
   pragma Part_Of (Transaction_Queue);

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
   -- Confirm_Written --
   ---------------------

   function Confirm_Written (Handle : Service_Handle) return Boolean
   is (STQ.Confirm_Written (Handle.Handle));

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

end LibSAP.Synchronous_Provider_Service_Access_Point;
