--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

private with LibSAP.Singleton_Transaction_Queues;

generic
   type Request_Type is limited private;
   type Confirm_Type is limited private;

   Queue_Capacity : Positive;
   --  Configures the maximum number of concurrent transactions

   with function Requires_Confirm (Request : Request_Type) return Boolean;
   --  Returns true if the Request requires a confirm primitive to be sent in
   --  response.

   with
     function Valid_Confirm
       (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   --  Returns True if the Confirm object is valid for the given Request

package LibSAP.Light_Provider_Service_Access_Point with
    Elaborate_Body,
    Abstract_State => (Transaction_Queue, Queue_Memory)
is

   type Transaction_ID is new Positive range 1 .. Queue_Capacity;

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

   function Requires_Confirm (Handle : Request_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Requires_Confirm'Result
       = Requires_Confirm (Request_Reference (Handle).all);

   function Request_Ready (Handle : Request_Handle) return Boolean
   with Global => null, Pre => not Is_Null (Handle);

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
       and (Request_Ready (Target) = Request_Ready (Source)'Old);

   generic
      with procedure Build (Request : out Request_Type);
   procedure Build_Request (Handle : in out Request_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post => not Is_Null (Handle) and then Request_Ready (Handle);
   --  Write a request.
   --
   --  The request object is passed to the Build generic formal procedure,
   --  which does the actual write.

   generic
      with procedure Build (Request : out Request_Type);
   procedure Build_Request_No_Confirm (Handle : in out Request_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post =>
       not Is_Null (Handle)
       and then Request_Ready (Handle)
       and then not Requires_Confirm (Handle);
   --  Write a request that does not require a confirm.
   --
   --  The request object is passed to the Build generic formal procedure,
   --  which does the actual write.
   --
   --  The postcondition of Build must contain: not Requires_Confirm (Request)

   generic
      with procedure Build (Request : out Request_Type);
   procedure Build_Request_With_Confirm (Handle : in out Request_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post =>
       not Is_Null (Handle)
       and then Request_Ready (Handle)
       and then Requires_Confirm (Handle);
   --  Write a request that requires a confirm.
   --
   --  The request object is passed to the Build generic formal procedure,
   --  which does the actual write.
   --
   --  The postcondition of Build must contain: Requires_Confirm (Request)

   generic
      with procedure Build (Request : out Request_Type);
      with function Precondition return Boolean;
      with function Postcondition return Boolean;
   procedure Build_Contextual_Request (Handle : in out Request_Handle)
   with
     Pre  => not Is_Null (Handle) and then Precondition,
     Post =>
       not Is_Null (Handle)
       and then Request_Ready (Handle)
       and then Postcondition;
   --  Same as Build_Request, but provides additional proof context to be
   --  passed to and from the call to Build via a precondition and
   --  postcondition.

   generic
      with procedure Build (Request : out Request_Type);
      with function Precondition return Boolean;
      with function Postcondition return Boolean;
   procedure Build_Contextual_Request_No_Confirm
     (Handle : in out Request_Handle)
   with
     Pre  => not Is_Null (Handle) and then Precondition,
     Post =>
       not Is_Null (Handle)
       and then Request_Ready (Handle)
       and then not Requires_Confirm (Handle)
       and then Postcondition;
   --  Same as Build_Request_No_Confirm, but provides additional proof context
   --  to be passed to and from the call to Build via a precondition and
   --  postcondition.

   generic
      with procedure Build (Request : out Request_Type);
      with function Precondition return Boolean;
      with function Postcondition return Boolean;
   procedure Build_Contextual_Request_With_Confirm
     (Handle : in out Request_Handle)
   with
     Pre  => not Is_Null (Handle) and then Precondition,
     Post =>
       not Is_Null (Handle)
       and then Request_Ready (Handle)
       and then Requires_Confirm (Handle)
       and then Postcondition;
   --  Same as Build_Request_With_Confirm, but provides additional proof
   --  context to be passed to and from the call to Build via a precondition
   --  and postcondition.

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

   procedure Move
     (Target : in out Confirm_Promise; Source : in out Confirm_Promise)
   with
     Inline,
     Global => null,
     Pre    => Is_Null (Target),
     Post   => (Is_Null (Target) = Is_Null (Source)'Old) and Is_Null (Source);

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
   with
     Inline,
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Valid_Confirm
         (Request_Reference (Handle).all, Confirm_Reference'Result.all);

   procedure Move
     (Target : in out Confirm_Handle; Source : in out Confirm_Handle)
   with
     Inline,
     Global => null,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   => (Is_Null (Target) = Is_Null (Source)'Old) and Is_Null (Source);

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

   function Has_Valid_Confirm (Handle : Service_Handle) return Boolean
   with Global => null, Pre => not Is_Null (Handle);

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
       and (Has_Valid_Confirm (Target) = Has_Valid_Confirm (Source)'Old);

   -----------------------------
   -- Service User Operations --
   -----------------------------

   procedure Try_Allocate_Request (Handle : in out Request_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   => (if not Is_Null (Handle) then not Request_Ready (Handle));
   --  Try to allocate a new request object.
   --
   --  If there is enough free resources for a new transaction, then one is
   --  allocated and stored in the Handle. If there are no free resources, then
   --  Handle is set to null.

   procedure Send_Request
     (Handle : in out Request_Handle; Promise : in out Confirm_Promise)
   with
     Inline,
     Global         => (In_Out => Transaction_Queue),
     Pre            =>
       not Is_Null (Handle)
       and then Is_Null (Promise)
       and then Request_Ready (Handle),
     Post           => Is_Null (Handle),
     Contract_Cases =>
       (Requires_Confirm (Handle) => not Is_Null (Promise),
        others                    => Is_Null (Promise));
   --  Send a prepared request to the Service Provider.

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

   generic
      with procedure Build (Request : out Request_Type);
   procedure Try_Build_And_Send_Request
     (Promise : in out Confirm_Promise; Was_Sent : out Boolean)
   with
     Pre  => Is_Null (Promise),
     Post => (if not Was_Sent then Is_Null (Promise));
   --  Tries to allocate a request, then builds the request and sends it to the
   --  Service Provider.
   --
   --  If the allocation step fails, then Was_Sent is set to False and no
   --  request is sent. Otherwise, Was_Sent is set to True and the request is
   --  sent.
   --
   --  If the request expects to receive a confirm primitive in response, then
   --  Promise is non-null and is used by the caller to get the confirm in the
   --  future when it is ready. Otherwise, if no confirm primitive is required,
   --  then Promise is set to null.

   procedure Try_Get_Confirm
     (Handle : in out Confirm_Handle; Promise : in out Confirm_Promise)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle) and then not Is_Null (Promise),
     Post   => Is_Null (Handle) = not Is_Null (Promise);
   --  Try to get the pending confirm primitive from a Promise.
   --
   --  If the pending confirm primitive has been sent by the Service Provider,
   --  then Handle is set to hold a reference to the primitive and Promise is
   --  set to null. Otherwise, if the Service has not yet sent the confirm
   --  primitive, then both Handle and Promise are unchanged.

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

   procedure New_Request
     (Cfm_Handle : in out Confirm_Handle; Req_Handle : in out Request_Handle)
   with
     Global => null,
     Pre    => not Is_Null (Cfm_Handle) and then Is_Null (Req_Handle),
     Post   => not Is_Null (Req_Handle) and then Is_Null (Cfm_Handle);
   --  Finish the transaction held by Cfm_Handle and begin a new transaction
   --  in Req_Handle.
   --
   --  This is useful to begin a new request without needing to reallocate a
   --  new handle.

   ---------------------------------
   -- Service Provider Operations --
   ---------------------------------

   procedure Try_Get_Next_Request (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle);
   --  Try to get the next pending request

   procedure Request_Completed (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => not Is_Null (Handle) and then not Requires_Confirm (Handle),
     Post   => Is_Null (Handle);
   --  Release a service handle.
   --
   --  This must be called when the Service Provider has finished processing a
   --  request that does not require a confirm primitive in response. This
   --  releases any resources held by the handle.

   procedure Send_Confirm (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    =>
       not Is_Null (Handle)
       and then Requires_Confirm (Handle)
       and then Has_Valid_Confirm (Handle),
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
   procedure Process_Request (Handle : in out Service_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post =>
       not Is_Null (Handle)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and (if Requires_Confirm (Handle) then Has_Valid_Confirm (Handle));
   --  Process a request, and generate a confirm if one is required.
   --
   --  This procedure passes the request to either Process_Request_No_Confirm
   --  or Process_Request_With_Confirm, depending on whether a confirmation
   --  is required.

   generic
      with
        procedure Build (Request : Request_Type; Confirm : out Confirm_Type);
   procedure Build_Confirm (Handle : in out Service_Handle)
   with
     Pre  => not Is_Null (Handle) and then Requires_Confirm (Handle),
     Post =>
       not Is_Null (Handle)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and Has_Valid_Confirm (Handle);
   --  Builds a confirm primitive.
   --
   --  The confirm primitive is passed to the Build procedure, which writes
   --  to it.

private

   package STQ is new
     LibSAP.Singleton_Transaction_Queues
       (Request_Type     => Request_Type,
        Confirm_Type     => Confirm_Type,
        Queue_Capacity   => Queue_Capacity,
        Requires_Confirm => Requires_Confirm,
        Valid_Confirm    => Valid_Confirm);
   pragma Part_Of (Queue_Memory);

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

   -------------------
   -- Request_Ready --
   -------------------

   function Request_Ready (Handle : Request_Handle) return Boolean
   is (STQ.Request_Ready (Handle.Handle));

end LibSAP.Light_Provider_Service_Access_Point;
