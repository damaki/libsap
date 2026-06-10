--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

private with LibSAP.Pointer_Queues;

--  @private
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

private generic
   type Request_Kind_Type is (<>);

   type Request_Type is limited private;
   type Confirm_Type is limited private;

   Queue_Capacity : Positive;

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
   --  Returns True if the Request contains a valid request.

   with
     function Valid_Confirm
       (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   --  Returns True if the Confirm object is valid for the given Request
package LibSAP.Singleton_Transaction_Queues with
    Elaborate_Body,
    Abstract_State => (Transaction_Pool with Synchronous)
is

   subtype Transaction_ID is Positive range 1 .. Queue_Capacity;

   package Parameter_Checks
     with Ghost
   is

      --  Check the relationship between Request_Requires_Cleanup and
      --  Might_Require_Cleanup for Request primitives.
      --
      --  If a request kind never requires cleanup, then
      --  Request_Requires_Cleanup must always return false for all requests of
      --  that kind.
      --
      --  If a request object requires cleanup, then Might_Require_Cleanup
      --  must return True for that kind of request.

      procedure Request_Cleanup_Implied (Request : Request_Type)
      with
        Post =>
          (if Request_Requires_Cleanup (Request)
           then Might_Require_Cleanup (Request_Kind (Request)));

      procedure Request_No_Cleanup_Implied (Request : Request_Type)
      with
        Post =>
          (if not Might_Require_Cleanup (Request_Kind (Request))
           then not Request_Requires_Cleanup (Request));

      --  Check the relationship between Confirm_Requires_Cleanup and
      --  Might_Require_Cleanup for Confirm primitives.
      --
      --  If a request kind never requires cleanup, then
      --  Confirm_Requires_Cleanup must always return false for all
      --  confirmations of that kind.
      --
      --  If a confirm object requires cleanup, then Might_Require_Cleanup
      --  must return True for that kind.

      procedure Confirm_Cleanup_Implied
        (Request : Request_Type; Confirm : Confirm_Type)
      with
        Pre  => Valid_Confirm (Request, Confirm),
        Post =>
          (if Confirm_Requires_Cleanup (Confirm)
           then Might_Require_Cleanup (Request_Kind (Request)));

      procedure Confirm_No_Cleanup_Implied
        (Request : Request_Type; Confirm : Confirm_Type)
      with
        Pre  => Valid_Confirm (Request, Confirm),
        Post =>
          (if not Might_Require_Cleanup (Request_Kind (Request))
           then not Confirm_Requires_Cleanup (Confirm));

   end Parameter_Checks;

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

   function Empty_Handle return Request_Handle
   with Inline, Global => null, Post => Is_Null (Empty_Handle'Result);

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

   function Request_Written (Handle : Request_Handle) return Boolean
   with Global => null, Pre => not Is_Null (Handle);

   function Request_Kind (Handle : Request_Handle) return Request_Kind_Type
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Request_Kind'Result = Request_Kind (Request_Reference (Handle).all);

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
       and (Request_Written (Target) = Request_Written (Source)'Old)
       and (Request_Kind (Target) = Request_Kind (Source)'Old)
       and
         (Valid_Request (Request_Reference (Target).all)
          = Valid_Request (Request_Reference (Source).all)'Old);

   generic
      with function Property (Request : Request_Type) return Boolean;
   procedure Move_Request_Handle_With_Property
     (Target : in out Request_Handle; Source : in out Request_Handle)
   with
     Inline,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       not Is_Null (Target)
       and Is_Null (Source)
       and (Get_TID (Target) = Get_TID (Source)'Old)
       and (Request_Written (Target) = Request_Written (Source)'Old)
       and
         (Property (Request_Reference (Target).all)
          = Property (Request_Reference (Source).all)'Old);

   generic
      with procedure Initialize (Request : out Request_Type);
      with function Precondition return Boolean;
      with function Postcondition (Request : Request_Type) return Boolean;
   procedure Initialize_Request (Handle : in out Request_Handle)
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

   generic
      with procedure Update (Request : in out Request_Type);
      with function Precondition (Request : Request_Type) return Boolean;
      with function Postcondition (Request : Request_Type) return Boolean;
   procedure Update_Request (Handle : in out Request_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then Precondition (Request_Reference (Handle).all)
       and then Request_Written (Handle),
     Post =>
       not Is_Null (Handle)
       and Request_Written (Handle)
       and Postcondition (Request_Reference (Handle).all)
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

   function Empty_Promise return Confirm_Promise
   with Inline, Global => null, Post => Is_Null (Empty_Promise'Result);

   function Get_TID (Handle : Confirm_Promise) return Transaction_ID
   with Global => null, Pre => not Is_Null (Handle);

   function Request_Kind (Promise : Confirm_Promise) return Request_Kind_Type
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

   function Empty_Handle return Confirm_Handle
   with Inline, Global => null, Post => Is_Null (Empty_Handle'Result);

   function Get_TID (Handle : Confirm_Handle) return Transaction_ID
   with Global => null, Pre => not Is_Null (Handle);

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
       and (Request_Kind (Target) = Request_Kind (Source)'Old)
       and (Requires_Cleanup (Target) = Requires_Cleanup (Source)'Old)
       and
         (Valid_Request (Request_Reference (Target).all)
          = Valid_Request (Request_Reference (Source).all)'Old)
       and
         (Valid_Confirm
            (Request_Reference (Target).all, Confirm_Reference (Target).all)
          = Valid_Confirm
              (Request_Reference (Source).all,
               Confirm_Reference (Source).all)'Old);

   generic
      with function Request_Property (Request : Request_Type) return Boolean;
      with function Confirm_Property (Confirm : Confirm_Type) return Boolean;

      with
        function Pair_Property
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   procedure Move_Confirm_Handle_With_Property
     (Target : in out Confirm_Handle; Source : in out Confirm_Handle)
   with
     Inline,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       not Is_Null (Target)
       and Is_Null (Source)
       and (Get_TID (Target) = Get_TID (Source)'Old)
       and
         (Request_Property (Request_Reference (Target).all)
          = Request_Property (Request_Reference (Source).all))
       and
         (Confirm_Property (Confirm_Reference (Target).all)
          = Confirm_Property (Confirm_Reference (Source).all))
       and
         (Pair_Property
            (Request_Reference (Target).all, Confirm_Reference (Target).all)
          = Pair_Property
              (Request_Reference (Source).all,
               Confirm_Reference (Source).all)'Old);

   generic
      with
        procedure Clean
          (Request : in out Request_Type; Confirm : in out Confirm_Type);

      with
        function Precondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean;

      with
        function Postcondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
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

   function Empty_Handle return Service_Handle
   with Inline, Global => null, Post => Is_Null (Empty_Handle'Result);

   function Get_TID (Handle : Service_Handle) return Transaction_ID
   with Global => null, Pre => not Is_Null (Handle);

   function Request_Reference
     (Handle : Service_Handle) return not null access constant Request_Type
   with Global => null, Pre => not Is_Null (Handle);

   function Request_Kind (Handle : Service_Handle) return Request_Kind_Type
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Request_Kind'Result = Request_Kind (Request_Reference (Handle).all);

   function Requires_Confirm (Handle : Service_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Requires_Confirm'Result
       = Requires_Confirm (Request_Reference (Handle).all);

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

   function Requires_Cleanup (Handle : Service_Handle) return Boolean
   with
     Global => null,
     Pre    => not Is_Null (Handle),
     Post   =>
       Requires_Cleanup'Result
       = (Request_Requires_Cleanup (Request_Reference (Handle).all)
          or else
            (Confirm_Written (Handle)
             and then
               Confirm_Requires_Cleanup (Confirm_Reference (Handle).all)));

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
       and (Confirm_Written (Target) = Confirm_Written (Source)'Old)
       and (Request_Kind (Target) = Request_Kind (Source)'Old)
       and (Requires_Cleanup (Target) = Requires_Cleanup (Source)'Old)
       and
         (Valid_Request (Request_Reference (Target).all)
          = Valid_Request (Request_Reference (Source).all)'Old);

   generic
      with function Request_Property (Request : Request_Type) return Boolean;
      with function Confirm_Property (Confirm : Confirm_Type) return Boolean;
      with
        function Pair_Property
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   procedure Move_Service_Handle_With_Property
     (Target : in out Service_Handle; Source : in out Service_Handle)
   with
     Inline,
     Pre    => Is_Null (Target) and not Is_Null (Source),
     Post   =>
       not Is_Null (Target)
       and Is_Null (Source)
       and (Get_TID (Target) = Get_TID (Source)'Old)
       and (Confirm_Written (Target) = Confirm_Written (Source)'Old)
       and
         (Request_Property (Request_Reference (Target).all)
          = Request_Property (Request_Reference (Source).all)'Old)
       and
         (if Confirm_Written (Source)'Old
          then
            (Confirm_Property (Confirm_Reference (Target).all)
             = Confirm_Property (Confirm_Reference (Source).all)'Old)
            and
              (Pair_Property
                 (Request_Reference (Target).all,
                  Confirm_Reference (Target).all)
               = Pair_Property
                   (Request_Reference (Source).all,
                    Confirm_Reference (Source).all)'Old));

   generic
      with
        procedure Initialize
          (Request : Request_Type; Confirm : out Confirm_Type);

      with function Precondition (Request : Request_Type) return Boolean;

      with
        function Postcondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   procedure Initialize_Confirm (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
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

   generic
      with
        procedure Update
          (Request : Request_Type; Confirm : in out Confirm_Type);

      with
        function Precondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean;

      with
        function Postcondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   procedure Update_Confirm (Handle : in out Service_Handle)
   with
     Inline,
     Pre  =>
       not Is_Null (Handle)
       and then Confirm_Written (Handle)
       and then Requires_Confirm (Handle)
       and then
         Precondition
           (Request_Reference (Handle).all, Confirm_Reference (Handle).all),
     Post =>
       not Is_Null (Handle)
       and (Request_Kind (Handle) = Request_Kind (Handle)'Old)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and Confirm_Written (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
       and
         Postcondition
           (Request_Reference (Handle).all, Confirm_Reference (Handle).all);

   generic
      with procedure Consume (Request : in out Request_Type);
      with function Precondition (Request : Request_Type) return Boolean;
      with function Postcondition (Request : Request_Type) return Boolean;
   procedure Consume_Request (Handle : in out Service_Handle)
   with
     Inline,
     Pre  =>
       not Is_Null (Handle)
       and then Precondition (Request_Reference (Handle).all),
     Post =>
       not Is_Null (Handle)
       and (Request_Kind (Handle) = Request_Kind (Handle)'Old)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and (Confirm_Written (Handle) = Confirm_Written (Handle)'Old)
       and Postcondition (Request_Reference (Handle).all);

   generic
      with
        procedure Initialize
          (Request : in out Request_Type; Confirm : out Confirm_Type);

      with function Precondition (Request : Request_Type) return Boolean;

      with
        function Postcondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   procedure Consume_Request_And_Initialize_Confirm
     (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
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

   generic
      with
        procedure Update
          (Request : in out Request_Type; Confirm : in out Confirm_Type);

      with
        function Precondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean;

      with
        function Postcondition
          (Request : Request_Type; Confirm : Confirm_Type) return Boolean;
   procedure Consume_Request_And_Update_Confirm
     (Handle : in out Service_Handle)
   with
     Pre  =>
       not Is_Null (Handle)
       and then Confirm_Written (Handle)
       and then Requires_Confirm (Handle)
       and then
         Precondition
           (Request_Reference (Handle).all, Confirm_Reference (Handle).all),
     Post =>
       not Is_Null (Handle)
       and (Request_Kind (Handle) = Request_Kind (Handle)'Old)
       and (Requires_Confirm (Handle) = Requires_Confirm (Handle)'Old)
       and Confirm_Written (Handle)
       and (Get_TID (Handle) = Get_TID (Handle)'Old)
       and
         Postcondition
           (Request_Reference (Handle).all, Confirm_Reference (Handle).all);

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
     Post   =>
       (if not Is_Null (Handle)
        then
          not Request_Written (Handle)
          and then
            not Request_Requires_Cleanup (Request_Reference (Handle).all));

   procedure Abort_Request (Handle : in out Request_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    =>
       not Is_Null (Handle)
       and then not Request_Requires_Cleanup (Request_Reference (Handle).all),
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
       and then Request_Written (Handle)
       and then Valid_Request (Request_Reference (Handle).all),
     Post           => Is_Null (Handle) and Has_Pending_Request (Queue),
     Contract_Cases =>
       (Requires_Confirm (Handle) =>
          not Is_Null (Promise)
          and (Get_TID (Promise) = Get_TID (Handle)'Old)
          and (Request_Kind (Promise) = Request_Kind (Handle)'Old),
        others                    => Is_Null (Promise));

   procedure Discard (Promise : in out Confirm_Promise)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    => not Might_Require_Cleanup (Request_Kind (Promise)),
     Post   => Is_Null (Promise);

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

   procedure Release (Handle : in out Confirm_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    => not Is_Null (Handle) and then not Requires_Cleanup (Handle),
     Post   => Is_Null (Handle);

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
        others                          =>
          not Is_Null (Handle)
          and then Valid_Request (Request_Reference (Handle).all)
          and then not Confirm_Written (Handle));

   procedure Send_Confirm (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    =>
       not Is_Null (Handle)
       and then Requires_Confirm (Handle)
       and then Confirm_Written (Handle)
       and then
         Valid_Confirm
           (Request_Reference (Handle).all, Confirm_Reference (Handle).all),
     Post   => Is_Null (Handle);

   procedure Release (Handle : in out Service_Handle)
   with
     Global => (In_Out => Transaction_Pool),
     Pre    =>
       not Is_Null (Handle)
       and then not Requires_Confirm (Handle)
       and then not Request_Requires_Cleanup (Request_Reference (Handle).all),
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
     (TD                 : not null Transaction_Data_Access;
      Fixed_Request_Kind : Request_Kind_Type) return Boolean
   with Ghost;

   type Service_Handle is limited record
      TD                 : Transaction_Data_Access := null;
      Fixed_Request_Kind : Request_Kind_Type := Request_Kind_Type'First;
   end record
   with
     Ghost_Predicate =>
       (if TD /= null then Service_Handle_Predicate (TD, Fixed_Request_Kind));

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

      Request_Kind : Request_Kind_Type := Request_Kind_Type'First;
      --  Holds a copy of the Request_Kind at the point when the request
      --  was sent to the Service Provider. This helps the Service User to
      --  prove that the Request_Kind is preserved when they get the confirm
      --  primitve from the Service Provider.
      --
      --  This field must not be modified while Token /= null.
   end record;

   function Is_Null (Promise : Confirm_Promise) return Boolean
   is (Promise.Token = null);

   ------------------
   -- Empty_Handle --
   ------------------

   function Empty_Handle return Request_Handle
   is (Request_Handle'(others => <>));

   function Empty_Promise return Confirm_Promise
   is (Confirm_Promise'(others => <>));

   function Empty_Handle return Confirm_Handle
   is (Confirm_Handle'(others => <>));

   function Empty_Handle return Service_Handle
   is (Service_Handle'(others => <>));

end LibSAP.Singleton_Transaction_Queues;
