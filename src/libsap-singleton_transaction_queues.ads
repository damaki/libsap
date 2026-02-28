--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This is an internal package that implements the core message-passing
--  mechanism between Service Providers and Service Users.
--
--  Instantiations of this package contain a pool of one or more transaction
--  objects. The pointer to each transaction is owned by one entity at any
--  point in time (enforced by SPARK's ownership rules): either the
--  Service Provider, Service User, or by the transaction queue. When the
--  pointer is held outside the transaction queue it is held in a handle, which
--  is a limited type so cannot be copied.
--
--  SPARK's ownership rules ensure that the pointer is not leaked at the end of
--  its scope; the Service Provider and Service User must call the appropriate
--  procedure to relinquish the handle back to the transaction queue.

private with LibSAP.Pointer_Queues;

private generic
   type Request_Type is limited private;
   type Confirm_Type is limited private;

   Queue_Capacity : Positive;

   with function Requires_Confirm (Request : Request_Type) return Boolean;
   --  Returns true if the Request requires a confirm primitive to be sent in
   --  response.

   with
     function Valid_Confirm
       (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   --  Returns True if the Confirm object is valid for the given Request
package LibSAP.Singleton_Transaction_Queues with
    Elaborate_Body,
    Abstract_State => (Transaction_Pool with Synchronous)
is

   subtype Transaction_ID is Positive range 1 .. Queue_Capacity;

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
   with Global => null, Pre => not Is_Null (Handle);

   function Request_Reference
     (Handle : Request_Handle) return not null access constant Request_Type
   with Inline, Global => null, Pre => not Is_Null (Handle);

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
     Inline,
     Global => null,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       not Is_Null (Target)
       and Is_Null (Source)
       and (Get_TID (Target) = Get_TID (Source)'Old)
       and (Requires_Confirm (Target) = Requires_Confirm (Source)'Old)
       and (Request_Ready (Target) = Request_Ready (Source)'Old);

   generic
      with procedure Build (Request : out Request_Type);
   procedure Build_Request (Handle : in out Request_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post =>
       not Is_Null (Handle)
       and Request_Ready (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old);

   generic
      with procedure Build (Request : out Request_Type);
   procedure Build_Request_No_Confirm (Handle : in out Request_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post =>
       not Is_Null (Handle)
       and Request_Ready (Handle)
       and not Requires_Confirm (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old);

   generic
      with procedure Build (Request : out Request_Type);
   procedure Build_Request_With_Confirm (Handle : in out Request_Handle)
   with
     Pre  => not Is_Null (Handle),
     Post =>
       not Is_Null (Handle)
       and Request_Ready (Handle)
       and Requires_Confirm (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old);

   generic
      with procedure Build (Request : out Request_Type);
      with function Precondition return Boolean;
      with function Postcondition return Boolean;
   procedure Build_Contextual_Request (Handle : in out Request_Handle)
   with
     Pre  => not Is_Null (Handle) and then Precondition,
     Post =>
       not Is_Null (Handle)
       and Request_Ready (Handle)
       and Postcondition
       and (Get_TID (Handle) = Get_TID (Handle)'Old);

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
       and Request_Ready (Handle)
       and not Requires_Confirm (Handle)
       and Postcondition
       and (Get_TID (Handle) = Get_TID (Handle)'Old);

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
       and Request_Ready (Handle)
       and Requires_Confirm (Handle)
       and Postcondition
       and (Get_TID (Handle) = Get_TID (Handle)'Old);

   ---------------------
   -- Confirm Promise --
   ---------------------

   --  Confirm_Promise represents the eventual completion of a request that
   --  results in a confirm primitive from the Service Provider.
   --
   --  It is used by the Service User to get the corresponding confirm
   --  primitive for a previous request, after the Service Provider has
   --  processed the request and sent the confirm primitive.

   type Confirm_Promise is limited private;

   function Is_Null (Promise : Confirm_Promise) return Boolean;
   --  A confirm handle provides a way for the Service User to hold a reference
   --  to the Confirm object that was sent by the Service Provider.

   function Get_TID (Handle : Confirm_Promise) return Transaction_ID
   with Global => null, Pre => not Is_Null (Handle);

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
   with Global => null, Pre => not Is_Null (Handle);

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

   --  A service handle holds a reference to a request and confirm pair.
   --
   --  It is used by the Service Provider to read the request that was sent by
   --  the Service User, and optionally write the confirm primitive if the
   --  request requires one.

   type Service_Handle is limited private
   with Default_Initial_Condition => Is_Null (Service_Handle);

   function Is_Null (Handle : Service_Handle) return Boolean
   with Global => null;

   function Get_TID (Handle : Service_Handle) return Transaction_ID
   with Global => null, Pre => not Is_Null (Handle);

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
     Inline,
     Global => null,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       not Is_Null (Target)
       and Is_Null (Source)
       and (Is_Null (Target) = Is_Null (Source)'Old)
       and (Get_TID (Target) = Get_TID (Source)'Old)
       and (Requires_Confirm (Target) = Requires_Confirm (Source)'Old)
       and (Has_Valid_Confirm (Target) = Has_Valid_Confirm (Source)'Old);

   generic
      with
        procedure Build (Request : Request_Type; Confirm : out Confirm_Type);
   procedure Build_Confirm (Handle : in out Service_Handle)
   with
     Pre  => not Is_Null (Handle) and then Requires_Confirm (Handle),
     Post =>
       not Is_Null (Handle)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and Has_Valid_Confirm (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old);

   ----------------------------
   -- Transaction_Queue_Type --
   ----------------------------

   type Transaction_Queue_Type is limited private
   with
     Default_Initial_Condition =>
       not Has_Pending_Request (Transaction_Queue_Type);

   -----------------------------
   -- Service User Operations --
   -----------------------------

   function Has_Pending_Request (Queue : Transaction_Queue_Type) return Boolean
   with Global => null;
   --  Returns True if there is at least one pending request for the
   --  Service Provider.

   procedure Try_Allocate_Request (Handle : in out Request_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    => Is_Null (Handle),
     Post   => (if not Is_Null (Handle) then not Request_Ready (Handle));

   procedure Abort_Request (Handle : in out Request_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    => not Is_Null (Handle),
     Post   => Is_Null (Handle);

   procedure Send_Request
     (Queue   : in out Transaction_Queue_Type;
      Handle  : in out Request_Handle;
      Promise : in out Confirm_Promise)
   with
     Global         => null,
     Pre            =>
       not Is_Null (Handle)
       and then Is_Null (Promise)
       and then Request_Ready (Handle),
     Post           => Is_Null (Handle) and Has_Pending_Request (Queue),
     Contract_Cases =>
       (Requires_Confirm (Handle) =>
          not Is_Null (Promise) and (Get_TID (Promise) = Get_TID (Handle)'Old),
        others                    => Is_Null (Promise));

   procedure Discard (Promise : in out Confirm_Promise)
   with Global => (In_Out => Transaction_Pool), Post => Is_Null (Promise);

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
             else Get_TID (Promise)));

   procedure Release (Handle : in out Confirm_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    => not Is_Null (Handle),
     Post   => Is_Null (Handle);

   procedure New_Request
     (Cfm_Handle : in out Confirm_Handle; Req_Handle : in out Request_Handle)
   with
     Global => null,
     Pre    => not Is_Null (Cfm_Handle) and then Is_Null (Req_Handle),
     Post   =>
       not Is_Null (Req_Handle)
       and Is_Null (Cfm_Handle)
       and (Get_TID (Req_Handle) = Get_TID (Cfm_Handle)'Old);

   ------------------------
   -- Service Operations --
   ------------------------

   procedure Try_Get_Next_Request
     (Queue : in out Transaction_Queue_Type; Handle : in out Service_Handle)
   with
     Global         => null,
     Pre            => Is_Null (Handle),
     Contract_Cases =>
       (not Has_Pending_Request (Queue) => Is_Null (Handle),
        others                          => not Is_Null (Handle));

   procedure Send_Confirm (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    =>
       not Is_Null (Handle)
       and then Requires_Confirm (Handle)
       and then Has_Valid_Confirm (Handle),
     Post   => Is_Null (Handle);

   procedure Request_Completed (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    => not Is_Null (Handle) and then not Requires_Confirm (Handle),
     Post   => Is_Null (Handle);

private

   --  These types are incomplete in the spec to prevent entities outside this
   --  package from being able to allocate or deallocate objects. Only the
   --  package body can allocate them.

   type Transaction_Data;
   type Transaction_Data_Access is access all Transaction_Data;

   type Confirm_Promise_Token;
   type Confirm_Promise_Token_Access is access all Confirm_Promise_Token;

   function Pending_Request_Predicate
     (TD : not null Transaction_Data_Access) return Boolean
   with Ghost;

   subtype Pending_Request_Transaction_Data_Access is Transaction_Data_Access
   with
     Ghost_Predicate =>
       (if Pending_Request_Transaction_Data_Access /= null
        then
          Pending_Request_Predicate (Pending_Request_Transaction_Data_Access));

   package Transaction_Data_Access_Queues is new
     LibSAP.Pointer_Queues
       (Transaction_Data,
        Pending_Request_Transaction_Data_Access,
        Queue_Capacity);
   package TDAQ renames Transaction_Data_Access_Queues;

   type Transaction_Queue_Type is limited record
      Pending_Queue : TDAQ.Queue_Type;
      --  Holds the indices of slots that contain pending requests, in FIFO
      --  order.
   end record
   with Ghost_Predicate => TDAQ.Is_Valid (Pending_Queue);

   ------------------------
   -- Transaction_Handle --
   ------------------------

   type Transaction_Handle is limited record
      TD : Transaction_Data_Access := null;
   end record;

   --------------------
   -- Request_Handle --
   --------------------

   function Request_Handle_Predicate
     (TD : not null Transaction_Data_Access) return Boolean
   with Ghost;

   type Request_Handle is limited new Transaction_Handle
   with Ghost_Predicate => (if TD /= null then Request_Handle_Predicate (TD));

   function Is_Null (Handle : Request_Handle) return Boolean
   is (Handle.TD = null);

   --------------------
   -- Service_Handle --
   --------------------

   function Service_Handle_Predicate
     (TD : not null Transaction_Data_Access) return Boolean
   with Ghost;

   type Service_Handle is limited new Transaction_Handle
   with Ghost_Predicate => (if TD /= null then Service_Handle_Predicate (TD));

   function Is_Null (Handle : Service_Handle) return Boolean
   is (Handle.TD = null);

   --------------------
   -- Confirm_Handle --
   --------------------

   function Confirm_Handle_Predicate
     (TD : not null Transaction_Data_Access) return Boolean
   with Ghost;

   type Confirm_Handle is limited new Transaction_Handle
   with Ghost_Predicate => (if TD /= null then Confirm_Handle_Predicate (TD));

   function Is_Null (Handle : Confirm_Handle) return Boolean
   is (Handle.TD = null);

   ---------------------
   -- Confirm_Promise --
   ---------------------

   type Confirm_Promise is limited record
      Token : Confirm_Promise_Token_Access := null;
   end record;

   function Is_Null (Promise : Confirm_Promise) return Boolean
   is (Promise.Token = null);

end LibSAP.Singleton_Transaction_Queues;
