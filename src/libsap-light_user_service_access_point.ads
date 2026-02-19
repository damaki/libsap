--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

private with LibSAP.Singleton_Transaction_Queues;

generic
   type Indication_Type is limited private;
   type Response_Type is limited private;

   Queue_Capacity : Positive;
   --  Configures the maximum number of concurrent transactions

   with
     function Requires_Response (Indication : Indication_Type) return Boolean;
   --  Returns true if the Indication requires a corresponding response
   --  primitive.

   with
     function Valid_Response
       (Indication : Indication_Type; Response : Response_Type) return Boolean;
   --  Returns True if the Response object is valid for the given Indication

package LibSAP.Light_User_Service_Access_Point with
    Elaborate_Body,
    Abstract_State => (Transaction_Queue, (Transaction_Pool with Synchronous))
is

   type Transaction_ID is new Positive range 1 .. Queue_Capacity;

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

   function Indication_Ready (Handle : Indication_Handle) return Boolean
   with Global => null, Pre => not Is_Null (Handle);

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
       and (Indication_Ready (Target) = Indication_Ready (Source)'Old);

   generic
      with procedure Build (Indication : out Indication_Type);
   procedure Build_Indication (Handle : in out Indication_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post => not Is_Null (Handle) and then Indication_Ready (Handle);
   --  Write an indication.
   --
   --  The indication object is passed to the Build generic formal procedure,
   --  which does the actual write.
   --
   --  This is a non-blocking operation if and only if Build is non-blocking.

   generic
      with procedure Build (Indication : out Indication_Type);
   procedure Build_Indication_No_Response (Handle : in out Indication_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post =>
       not Is_Null (Handle)
       and then Indication_Ready (Handle)
       and then not Requires_Response (Handle);
   --  Write an indication that does not require a confirm.
   --
   --  The indication object is passed to the Build generic formal procedure,
   --  which does the actual write.
   --
   --  The postcondition of Build must contain: not Requires_Confirm (Request)
   --
   --  This is a non-blocking operation if and only if Build is non-blocking.

   generic
      with procedure Build (Indication : out Indication_Type);
   procedure Build_Indication_With_Response (Handle : in out Indication_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post =>
       not Is_Null (Handle)
       and then Indication_Ready (Handle)
       and then Requires_Response (Handle);
   --  Write an indication that requires a confirm.
   --
   --  The indication object is passed to the Build generic formal procedure,
   --  which does the actual write.
   --
   --  The postcondition of Build must contain: Requires_Confirm (Request)
   --
   --  This is a non-blocking operation if and only if Build is non-blocking.

   generic
      with procedure Build (Indication : out Indication_Type);
      with function Precondition return Boolean;
      with function Postcondition return Boolean;
   procedure Build_Contextual_Indication (Handle : in out Indication_Handle)
   with
     Pre  => not Is_Null (Handle) and then Precondition,
     Post =>
       not Is_Null (Handle)
       and then Indication_Ready (Handle)
       and then Postcondition;
   --  Same as Build_Indication, but provides additional proof context to be
   --  passed to and from the call to Build via a precondition and
   --  postcondition.

   generic
      with procedure Build (Indication : out Indication_Type);
      with function Precondition return Boolean;
      with function Postcondition return Boolean;
   procedure Build_Contextual_Indication_No_Response
     (Handle : in out Indication_Handle)
   with
     Pre  => not Is_Null (Handle) and then Precondition,
     Post =>
       not Is_Null (Handle)
       and then Indication_Ready (Handle)
       and then not Requires_Response (Handle)
       and then Postcondition;
   --  Same as Build_Indication_No_Confirm, but provides additional proof
   --  context to be passed to and from the call to Build via a precondition
   --  and postcondition.

   generic
      with procedure Build (Indication : out Indication_Type);
      with function Precondition return Boolean;
      with function Postcondition return Boolean;
   procedure Build_Contextual_Indication_With_Response
     (Handle : in out Indication_Handle)
   with
     Pre  => not Is_Null (Handle) and then Precondition,
     Post =>
       not Is_Null (Handle)
       and then Indication_Ready (Handle)
       and then Requires_Response (Handle)
       and then Postcondition;
   --  Same as Build_Indication_With_Confirm, but provides additional proof
   --  context to be passed to and from the call to Build via a precondition
   --  and postcondition.

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

   procedure Move
     (Target : in out Response_Promise; Source : in out Response_Promise)
   with
     Inline,
     Global => null,
     Pre    => Is_Null (Target),
     Post   => (Is_Null (Target) = Is_Null (Source)'Old) and Is_Null (Source);

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
   with
     Inline,
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Valid_Response
         (Indication_Reference (Handle).all, Response_Reference'Result.all);

   procedure Move
     (Target : in out Response_Handle; Source : in out Response_Handle)
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

   function Indication_Reference
     (Handle : Service_Handle) return not null access constant Indication_Type
   with Global => null, Pre => not Is_Null (Handle);

   function Requires_Response (Handle : Service_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Requires_Response'Result
       = Requires_Response (Indication_Reference (Handle).all);

   function Has_Valid_Response (Handle : Service_Handle) return Boolean
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
       and (Requires_Response (Target) = Requires_Response (Source)'Old)
       and (Has_Valid_Response (Target) = Has_Valid_Response (Source)'Old);

   ---------------------------------
   -- Service Provider Operations --
   ---------------------------------

   procedure Try_Allocate_Indication (Handle : in out Indication_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Pool),
     Pre    => Is_Null (Handle),
     Post   => (if not Is_Null (Handle) then not Indication_Ready (Handle));
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
       and then Indication_Ready (Handle),
     Post           => Is_Null (Handle),
     Contract_Cases =>
       (Requires_Response (Handle) => not Is_Null (Promise),
        others                     => Is_Null (Promise));
   --  Send a prepared indication to the Service User.
   --
   --  This is a non-blocking operation.

   procedure Abort_Indication (Handle : in out Indication_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Pool),
     Pre    => not Is_Null (Handle),
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
     Global => (In_Out => Transaction_Pool),
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
     Global => (In_Out => Transaction_Pool),
     Pre    => Is_Null (Handle) and then not Is_Null (Promise),
     Post   => Is_Null (Handle) = not Is_Null (Promise);
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
     Global => (In_Out => Transaction_Pool),
     Pre    => not Is_Null (Handle),
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
     Pre    => not Is_Null (Res_Handle) and then Is_Null (Ind_Handle),
     Post   => not Is_Null (Ind_Handle) and then Is_Null (Res_Handle);
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

   procedure Try_Get_Next_Indication (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle);
   --  Try to get the next pending request

   procedure Indication_Completed (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Pool),
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
     Global => (In_Out => Transaction_Pool),
     Pre    =>
       not Is_Null (Handle)
       and then Requires_Response (Handle)
       and then Has_Valid_Response (Handle),
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
   procedure Process_Indication (Handle : in out Service_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post =>
       not Is_Null (Handle)
       and (Requires_Response (Handle) = Requires_Response (Handle)'Old)
       and (if Requires_Response (Handle) then Has_Valid_Response (Handle));
   --  Process an indication, and generate a response if one is required.
   --
   --  This procedure passes the indication to either
   --  Process_Indication_No_Response or Process_Indication_With_Response,
   --  depending on whether a response is required.

   generic
      with
        procedure Build
          (Indication : Indication_Type; Response : out Response_Type);
   procedure Build_Response (Handle : in out Service_Handle)
   with
     Pre  => not Is_Null (Handle) and then Requires_Response (Handle),
     Post =>
       not Is_Null (Handle)
       and (Requires_Response (Handle) = Requires_Response (Handle)'Old)
       and Has_Valid_Response (Handle);
   --  Builds a response primitive.
   --
   --  The response primitive is passed to the Build procedure, which writes
   --  to it.

private

   package STQ is new
     LibSAP.Singleton_Transaction_Queues
       (Request_Type     => Indication_Type,
        Confirm_Type     => Response_Type,
        Queue_Capacity   => Queue_Capacity,
        Requires_Confirm => Requires_Response,
        Valid_Confirm    => Valid_Response);
   pragma Part_Of (Transaction_Pool);

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

   -------------------
   -- Indication_Ready --
   -------------------

   function Indication_Ready (Handle : Indication_Handle) return Boolean
   is (STQ.Request_Ready (Handle.Handle));

end LibSAP.Light_User_Service_Access_Point;
