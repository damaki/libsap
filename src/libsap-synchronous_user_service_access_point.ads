--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System;

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

   Priority : System.Priority;

package LibSAP.Synchronous_User_Service_Access_Point with
    Elaborate_Body,
    Abstract_State => ((Transaction_Queue with Synchronous), Queue_Memory)
is

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
   --  Write to the indication object held by Handle.
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

   generic
      with procedure Build (Indication : out Indication_Type);
   procedure Build_Indication_With_Response (Handle : in out Indication_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post =>
       not Is_Null (Handle)
       and then Indication_Ready (Handle)
       and then Requires_Response (Handle);

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

   procedure Allocate_Indication (Handle : in out Indication_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   => not Is_Null (Handle) and then not Indication_Ready (Handle);
   --  Allocate a new indication object.
   --
   --  This starts a new transaction.
   --
   --  This is a potentially blocking operation.

   procedure Try_Allocate_Indication (Handle : in out Indication_Handle)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
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
     Global => (In_Out => Transaction_Queue),
     Pre    => not Is_Null (Handle),
     Post   => Is_Null (Handle);
   --  Abort a indication.
   --
   --  This frees up the resources held by Handle without sending the
   --  indication to the Service User.
   --
   --  This is a non-blocking operation.

   generic
      with procedure Build (Indication : out Indication_Type);
   procedure Build_And_Send_Indication (Promise : in out Response_Promise)
   with Pre => Is_Null (Promise);
   --  Allocates and builds an indication object, then sends it to the
   --  Service User.
   --
   --  This procedure is used to send indications that may or may not require
   --  a response.
   --
   --  If the indication expects to receive a response primitive in response,
   --  then Promise is non-null and is used by the caller to get the response
   --  in the future when it is ready. Otherwise, if the indication does not
   --  expect a response primitive, then Promise is set to null.
   --
   --  This is a potentially blocking operation.

   generic
      with procedure Build (Indication : out Indication_Type);
   procedure Build_And_Send_Indication_No_Response;
   --  Allocates and builds an indication object, then sends it to the
   --  Service User.
   --
   --  This procedure is used to send indications that never require a
   --  response.
   --
   --  The postcondition of the Build generic formal procedure must contain:
   --  not Requires_Response (Indication)
   --
   --  This is a potentially blocking operation.

   generic
      with procedure Build (Indication : out Indication_Type);
   procedure Build_And_Send_Indication_With_Response
     (Promise : in out Response_Promise)
   with Pre => Is_Null (Promise), Post => not Is_Null (Promise);
   --  Allocates and builds an indication, then sends it to the Service User.
   --
   --  This procedure is used to send indications that always require a
   --  response.
   --
   --  The postcondition of the Build generic formal procedure must contain:
   --  Requires_Response (Indication)
   --
   --  This is a potentially blocking operation.

   generic
      with procedure Build (Indication : out Indication_Type);
   procedure Try_Build_And_Send_Indication
     (Promise : in out Response_Promise; Was_Sent : out Boolean)
   with
     Pre  => Is_Null (Promise),
     Post => (if not Was_Sent then Is_Null (Promise));
   --  Tries to allocate an indication, then builds the indication and sends it
   --  to the Service User.
   --
   --  If the allocation step fails, then Was_Sent is set to False and no
   --  indication is sent. Otherwise, Was_Sent is set to True and the
   --  indication is sent.
   --
   --  If the indication expects to receive a response, then Promise is
   --  non-null and is used by the caller to get the response in the future
   --  when it is ready. Otherwise, if no response primitive is required, then
   --  Promise is set to null.
   --
   --  This is a potentially blocking operation.

   procedure Try_Get_Response
     (Handle : in out Response_Handle; Promise : in out Response_Promise)
   with
     Inline,
     Global => (In_Out => Transaction_Queue),
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
     Global => (In_Out => Transaction_Queue),
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

   procedure Get_Next_Indication (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Queue),
     Pre    => Is_Null (Handle),
     Post   => not Is_Null (Handle);
   --  Wait for an indication from a Service Provider.
   --
   --  This is a potentially blocking operation.

   procedure Indication_Completed (Handle : in out Service_Handle)
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
   pragma Part_Of (Queue_Memory);

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

   -------------------
   -- Indication_Ready --
   -------------------

   function Indication_Ready (Handle : Indication_Handle) return Boolean
   is (STQ.Request_Ready (Handle.Handle));

end LibSAP.Synchronous_User_Service_Access_Point;
