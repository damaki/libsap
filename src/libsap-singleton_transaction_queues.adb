--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body LibSAP.Singleton_Transaction_Queues
  with Refined_State => (Single_Instance => Memory_Holder)
is

   Memory_Holder : Valid_Queue_Type;

   type Confirm_Promise_Token (TID : Transaction_ID) is null record;

   type Slot_States is
     (Free,
      Request_Allocated,
      Request_Written,
      Request_Pending,
      Request_Read,
      Confirm_Written,
      Confirm_Pending,
      Confirm_Read);

   type Transaction_Data (TID : Transaction_ID) is record
      Request      : aliased Request_Type;
      Confirm      : aliased Confirm_Type;
      State        : Slot_States;
      Next_Pending : Transaction_ID;
      Cfm_Token    : Confirm_Promise_Token_Access;
   end record
   with Predicate => (if Cfm_Token /= null then Cfm_Token.all.TID = TID);

   procedure Allocate_Memory (Queue : in out Queue_Type)
   with
     Global => null,
     Pre    =>
       Is_Valid (Queue)
       and then (for all TID in Transaction_ID => Queue.Slots (TID) = null),
     Post   => Is_Valid (Queue);

   procedure Move (Target : in out Queue_Type; Source : in out Queue_Type)
   with
     Global => null,
     Pre    =>
       Is_Valid (Source) and then Is_Valid (Target) and then Is_Empty (Target),
     Post   => Is_Valid (Target) and then Is_Valid (Source);

   procedure Move (Target, Source : in out Transaction_Data_Access)
   with
     Inline,
     Global         => null,
     Pre            => Target = null and then Source /= null,
     Post           =>
       Target.all.TID = Source.all.TID'Old
       and Target.all.State = Source.all.State'Old
       and
         (Requires_Confirm (Target.all.Request)
          = Requires_Confirm (Source.all.Request)'Old)
       and
         (Valid_Confirm (Target.all.Request, Target.all.Confirm)
          = Valid_Confirm (Source.all.Request, Source.all.Confirm)'Old)
       and Source = null
       and Target /= null,
     Contract_Cases =>
       (Source.all.Cfm_Token = null  => Target.all.Cfm_Token = null,
        Source.all.Cfm_Token /= null => Target.all.Cfm_Token /= null);

   procedure Move (Target, Source : in out Confirm_Promise_Token_Access)
   with
     Inline,
     Global => null,
     Pre    => Target = null and then Source /= null,
     Post   =>
       Target /= null
       and Source = null
       and Target.all.TID = Source.all.TID'Old;

   procedure Check_Slot_Is_Empty
     (Queue : Valid_Queue_Type; Handle : Transaction_Handle)
   with
     Global => null,
     Pre    => Handle.TD /= null,
     Post   => Queue.Slots (Handle.TD.all.TID) = null;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Queue : Queue_Type) return Boolean
   is (
       --  Transaction_Queue elements can only be placed in slots that match
       --  their transaction ID (TID).

       (for all I in Queue.Slots'Range =>
          (if Queue.Slots (I) /= null then Queue.Slots (I).all.TID = I))

       --  Transaction slots are never in certain states while they are being
       --  held in Queue. In those states, the slot is held by a handle
       --  instead.

       and then
         (for all S of Queue.Slots =>
            (if S /= null
             then
               S.all.State
               not in Request_Allocated | Request_Read | Confirm_Read))

       --  Transactions in the Pending_Request state have a null Cfm_Token
       --  (the Cfm_Token was moved to a Confirm_Promise) if and only if the
       --  associated request requires a confirm primitive in response to the
       --  request.

       and then
         (for all S of Queue.Slots =>
            (if S /= null
             then
               (if S.all.State = Request_Pending
                then
                  (S.all.Cfm_Token = null) = Requires_Confirm (S.all.Request)
                elsif S.all.State = Confirm_Pending
                then S.all.Cfm_Token = null
                elsif S.all.State = Free
                then S.all.Cfm_Token /= null)))

       --  If Has_Free_Slot is true, then there is at least one slot that is
       --  in the Free state.

       and then
         (if Queue.Has_Free_Slot
          then
            (for some S of Queue.Slots =>
               S /= null and then S.all.State = Free))

       --  If a slot is in the Request_Pending state, then Next_Pending
       --  references another slot that is also in the Request_Pending state.

       and then
         (for all S of Queue.Slots =>
            (if S /= null and then S.all.State = Request_Pending
             then
               Queue.Slots (S.all.Next_Pending) /= null
               and then
                 Queue.Slots (S.all.Next_Pending).all.State = Request_Pending))

       --  Has_Pending_Request is true if and only if there is at least one
       --  slot in the Request_Pending state.

       and then
         (Queue.Has_Pending_Request
          = (for some S of Queue.Slots =>
               S /= null and then S.all.State = Request_Pending))

       --  If Has_Pending_Request is true, then the slot referenced by
       --  First_Pending and Last_Pending are in the Request_Pending state.

       and then
         (if Queue.Has_Pending_Request
          then
            Queue.Slots (Queue.First_Pending) /= null
            and then Queue.Slots (Queue.Last_Pending) /= null
            and then
              Queue.Slots (Queue.First_Pending).all.State = Request_Pending
            and then
              Queue.Slots (Queue.Last_Pending).all.State = Request_Pending)

       --  If a slot in the Request_Pending state is referenced by another
       --  pending request, then it is not the first pending request.

       and then
         (for all I in Transaction_ID =>
            (if Queue.Slots (I) /= null
               and then Queue.Slots (I).all.State = Request_Pending
               and then
                 (for some J in Transaction_ID =>
                    J /= I
                    and then Queue.Slots (J) /= null
                    and then Queue.Slots (J).all.State = Request_Pending
                    and then Queue.Slots (J).all.Next_Pending = I)
             then I /= Queue.First_Pending))

       --  If a slot in the Request_Pending state references itself, then it is
       --  the last slot.

       and then
         (for all I in Transaction_ID =>
            (if Queue.Slots (I) /= null
               and then Queue.Slots (I).all.State = Request_Pending
               and then Queue.Slots (I).all.Next_Pending = I
             then I = Queue.Last_Pending))

       --  The last slot in the Request_Pending state references itself

       and then
         (if Queue.Has_Pending_Request
          then
            Queue.Slots (Queue.Last_Pending).Next_Pending = Queue.Last_Pending)

       --  No other slot in the Request_Pending state references the first
       --  pending request, except for itself.

       and then
         (for all I in Transaction_ID =>
            (if Queue.Slots (I) /= null
               and then Queue.Slots (I).all.State = Request_Pending
               and then Queue.Slots (I).all.Next_Pending = Queue.First_Pending
             then I = Queue.First_Pending and then I = Queue.Last_Pending))

       --  A slot in the Request_Pending state is referenced by no more than
       --  one other request.

       and then
         (for all I in Transaction_ID =>
            (for all J in Transaction_ID =>
               (if Queue.Slots (I) /= null
                  and then Queue.Slots (J) /= null
                  and then Queue.Slots (I).all.State = Request_Pending
                  and then Queue.Slots (J).all.State = Request_Pending
                  and then Queue.Slots (I).all.Next_Pending /= I
                  and then Queue.Slots (J).all.Next_Pending /= J
                  and then
                    Queue.Slots (I).all.Next_Pending
                    = Queue.Slots (J).all.Next_Pending
                then I = J)))

       --  A slot in the Confirm_Pending state has a valid confirm object

       and then
         (for all S of Queue.Slots =>
            (if S /= null and then S.all.State = Confirm_Pending
             then Valid_Confirm (S.all.Request, S.all.Confirm))));

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Queue : Queue_Type) return Boolean
   is (for all S of Queue.Slots => S = null);

   ------------------------------
   -- Request_Handle_Predicate --
   ------------------------------

   function Request_Handle_Predicate
     (TD : not null Transaction_Data_Access) return Boolean
   is (TD.all.State in Request_Allocated | Request_Written
       and then TD.all.Cfm_Token /= null);

   ------------------------------
   -- Service_Handle_Predicate --
   ------------------------------

   function Service_Handle_Predicate
     (TD : not null Transaction_Data_Access) return Boolean
   is (TD.all.State in Request_Read | Confirm_Written

       and then
         (if not Requires_Confirm (TD.all.Request)
          then TD.all.State /= Confirm_Written)

       and then
         (if TD.all.State = Confirm_Written
          then Valid_Confirm (TD.all.Request, TD.all.Confirm))

       and then (TD.all.Cfm_Token = null) = Requires_Confirm (TD.all.Request));

   ------------------------------
   -- Confirm_Handle_Predicate --
   ------------------------------

   function Confirm_Handle_Predicate
     (TD : not null Transaction_Data_Access) return Boolean
   is (TD.all.State = Confirm_Read
       and then TD.all.Cfm_Token /= null
       and then Valid_Confirm (TD.all.Request, TD.all.Confirm));

   -------------
   -- Get_TID --
   -------------

   function Get_TID (Handle : Request_Handle) return Transaction_ID
   is (Handle.TD.all.TID);

   function Get_TID (Handle : Confirm_Handle) return Transaction_ID
   is (Handle.TD.all.TID);

   function Get_TID (Handle : Confirm_Promise) return Transaction_ID
   is (Handle.Token.all.TID);

   function Get_TID (Handle : Service_Handle) return Transaction_ID
   is (Handle.TD.all.TID);

   -----------------------
   -- Request_Reference --
   -----------------------

   function Request_Reference
     (Handle : Request_Handle) return not null access constant Request_Type
   is (Handle.TD.all.Request'Access);

   function Request_Reference
     (Handle : Confirm_Handle) return not null access constant Request_Type
   is (Handle.TD.all.Request'Access);

   function Request_Reference
     (Handle : Service_Handle) return not null access constant Request_Type
   is (Handle.TD.all.Request'Access);

   -----------------------
   -- Confirm_Reference --
   -----------------------

   function Confirm_Reference
     (Handle : Confirm_Handle) return not null access constant Confirm_Type
   is (Handle.TD.all.Confirm'Access);

   ----------------------
   -- Requires_Confirm --
   ----------------------

   function Requires_Confirm (Handle : Request_Handle) return Boolean
   is (Requires_Confirm (Handle.TD.all.Request));

   function Requires_Confirm (Handle : Service_Handle) return Boolean
   is (Requires_Confirm (Handle.TD.all.Request));

   -------------------
   -- Request_Ready --
   -------------------

   function Request_Ready (Handle : Request_Handle) return Boolean
   is (Handle.TD.all.State = Request_Written);

   -----------------------
   -- Has_Valid_Confirm --
   -----------------------

   function Has_Valid_Confirm (Handle : Service_Handle) return Boolean
   is (Handle.TD.all.State = Confirm_Written);

   -------------------
   -- Build_Request --
   -------------------

   procedure Build_Request (Handle : in out Request_Handle) is
   begin
      Handle.TD.all.State := Request_Written;
      Build (Handle.TD.all.Request);
   end Build_Request;

   ------------------------------
   -- Build_Request_No_Confirm --
   ------------------------------

   procedure Build_Request_No_Confirm (Handle : in out Request_Handle) is
   begin
      Handle.TD.all.State := Request_Written;
      Build (Handle.TD.all.Request);

      pragma Assert (not Requires_Confirm (Handle.TD.all.Request));

   end Build_Request_No_Confirm;

   --------------------------------
   -- Build_Request_With_Confirm --
   --------------------------------

   procedure Build_Request_With_Confirm (Handle : in out Request_Handle) is
   begin
      Handle.TD.all.State := Request_Written;
      Build (Handle.TD.all.Request);

      pragma Assert (Requires_Confirm (Handle.TD.all.Request));

   end Build_Request_With_Confirm;

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : in out Request_Handle; Source : in out Request_Handle) is
   begin
      Target.TD := Source.TD;
      Source.TD := null;
   end Move;

   procedure Move
     (Target : in out Confirm_Promise; Source : in out Confirm_Promise) is
   begin
      Target.Token := Source.Token;
      Source.Token := null;
   end Move;

   procedure Move
     (Target : in out Confirm_Handle; Source : in out Confirm_Handle) is
   begin
      Target.TD := Source.TD;
      Source.TD := null;
   end Move;

   procedure Move
     (Target : in out Service_Handle; Source : in out Service_Handle) is
   begin
      Target.TD := Source.TD;
      Source.TD := null;
   end Move;

   procedure Move (Target, Source : in out Transaction_Data_Access) is
   begin
      Target := Source;
      Source := null;
   end Move;

   procedure Move (Target, Source : in out Confirm_Promise_Token_Access) is
   begin
      Target := Source;
      Source := null;
   end Move;

   procedure Move (Target : in out Queue_Type; Source : in out Queue_Type) is
   begin
      Target.Has_Free_Slot := False;
      Target.Has_Pending_Request := False;
      Target.First_Pending := Source.First_Pending;
      Target.Last_Pending := Source.Last_Pending;

      for I in Transaction_ID loop
         pragma Loop_Invariant (Is_Valid (Target));

         --  Slots that are not visited yet are null in Target

         pragma
           Loop_Invariant
             (for all J in Transaction_ID =>
                (if J >= I then Target.Slots (I) = null));

         --  Has_Free_Slot reflects the slots visited so far

         pragma
           Loop_Invariant
             (Target.Has_Free_Slot
              = (for some J in Transaction_ID =>
                   J < I
                   and then Target.Slots (J) /= null
                   and then Target.Slots (J).all.State = Free));

         --  Has_Pending_Request reflects the slots visited so far

         pragma
           Loop_Invariant
             (Target.Has_Pending_Request
              = (for some J in Transaction_ID =>
                   J < I
                   and then Target.Slots (J) /= null
                   and then Target.Slots (J).all.State = Request_Pending));

         --  All slots visited so far in Source are now null

         pragma
           Loop_Invariant
             (for all J in Transaction_ID =>
                (if J < I then Source.Slots (J) = null));

         if Source.Slots (I) /= null then
            Move (Target => Target.Slots (I), Source => Source.Slots (I));

            if Target.Slots (I) /= null then
               if Target.Slots (I).all.State = Free then
                  Target.Has_Free_Slot := True;
               elsif Target.Slots (I).all.State = Request_Pending then
                  Target.Has_Pending_Request := True;
               end if;
            end if;
         end if;
      end loop;

      Source.Has_Pending_Request := False;
      Source.Has_Free_Slot := False;
   end Move;

   -------------------------
   -- Check_Slot_Is_Empty --
   -------------------------

   procedure Check_Slot_Is_Empty
     (Queue : Valid_Queue_Type; Handle : Transaction_Handle) is
   begin
      --  Rationale for pragma Assume:
      --
      --  If Handle contains a non-null pointer, then it is guaranteed that
      --  the corresponding slot in Queue is null.
      --
      --  This is guaranteed because:
      --   1. there is always exactly one Transaction_Data instance for each
      --      transaction ID (TID), so two Transaction_Data objects cannot
      --      share the same TID.
      --   2. each slot in Queue can only hold a pointer to a Transaction_Data
      --      object that has the TID matching the array index, so slots can
      --      only ever hold a pointer to one Transaction_Data instance.
      --   3. SPARK's ownership rules guarantee that only one mutable pointer
      --      can point to (i.e. "own") an object at a time.
      --
      --  Therefore, if a Transaction_Data object is owned by a Handle, then
      --  it cannot also be owned by the Queue slot for that TID (doing so
      --  would violate SPARK's ownership rules), and so it must be null.
      --
      --  Further rationale:
      --
      --  (1) is guaranteed because the allocation of Transaction_Data objects
      --  is completely controlled by this package body, and this package body
      --  only ever allocates Transaction_Data objects (one object per TID)
      --  *once* during elaboration via the call to Allocate_Memory. Pointers
      --  to these allocated objects are initially held in Memory_Holder, and
      --  users of this package may *move* the allocated objects to their
      --  own Queue_Type instances (by calling Claim_Single_Instance), but they
      --  cannot allocate more Transaction_Data objects.
      --
      --  Users cannot allocate more Transaction_Data objects because:
      --   a. The Transaction_Data type is incomplete in the package spec,
      --      so no other unit can allocate or deallocate objects of that type
      --      (only this package body has full visibility of Transaction_Data).
      --   b. The only subprogram in this package that allocates
      --      Transaction_Data objects is Allocate_Memory, and it is private
      --      to this package body and so is not visible to other units.
      --
      --  (2) is guaranteed by the Is_Valid predicate for Queue_Type.
      --
      --  (3) is verified by GNATprove.
      --
      --  Note that we also assume that other units outside this package do not
      --  intentionally violate the SPARK's rules and checks, which could
      --  violate this assumption. To detect such cases we do a defensive check
      --  at run-time.

      pragma Assert (Handle.TD /= null);
      pragma Assert (Is_Valid (Queue));

      pragma Assume (Queue.Slots (Handle.TD.all.TID) = null);

      --  Defensive check for the above assumption

      if Queue.Slots (Handle.TD.all.TID) /= null then
         raise Program_Error;
      end if;
   end Check_Slot_Is_Empty;

   -------------------
   -- Build_Confirm --
   -------------------

   procedure Build_Confirm (Handle : in out Service_Handle) is
   begin
      Build (Handle.TD.all.Request, Handle.TD.all.Confirm);

      pragma
        Assert (Valid_Confirm (Handle.TD.all.Request, Handle.TD.all.Confirm));

      Handle.TD.all.State := Confirm_Written;
   end Build_Confirm;

   --------------------------
   -- Try_Allocate_Request --
   --------------------------

   procedure Try_Allocate_Request
     (Queue : in out Queue_Type; Handle : in out Request_Handle)
   is
      Free_TID : Transaction_ID
      with Relaxed_Initialization;
   begin
      if Queue.Has_Free_Slot then

         --  Find a free slot

         for I in Queue.Slots'Range loop
            pragma
              Loop_Invariant
                (for all J in Queue.Slots'Range =>
                   (if J < I
                    then
                      Queue.Slots (J) = null
                      or else Queue.Slots (J).all.State /= Free));

            if Queue.Slots (I) /= null
              and then Queue.Slots (I).all.State = Free
            then
               Free_TID := I;
               exit;
            end if;
         end loop;

         --  Update Has_Free_Slot

         Queue.Has_Free_Slot :=
           (for some I in Queue.Slots'Range =>
              (I /= Free_TID
               and then Queue.Slots (I) /= null
               and then Queue.Slots (I).all.State = Free));

         --  Move the transition data to the handle

         Queue.Slots (Free_TID).all.State := Request_Allocated;

         Move (Target => Handle.TD, Source => Queue.Slots (Free_TID));
      end if;
   end Try_Allocate_Request;

   -------------------
   -- Abort_Request --
   -------------------

   procedure Abort_Request
     (Queue : in out Queue_Type; Handle : in out Request_Handle)
   is
      TID : constant Transaction_ID := Handle.TD.all.TID;
   begin
      Check_Slot_Is_Empty (Queue, Transaction_Handle (Handle));

      Queue.Slots (TID) := Handle.TD;
      Handle.TD := null;

      Queue.Slots (TID).State := Free;
      Queue.Has_Free_Slot := True;
   end Abort_Request;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Queue   : in out Queue_Type;
      Handle  : in out Request_Handle;
      Promise : in out Confirm_Promise)
   is
      TID  : constant Transaction_ID := Handle.TD.all.TID;
      Temp : Transaction_Data_Access;

   begin
      Check_Slot_Is_Empty (Queue, Transaction_Handle (Handle));

      pragma Assert (Handle.TD.all.State = Request_Written);

      Temp := Handle.TD;
      Handle.TD := null;

      Temp.all.State := Request_Pending;
      Temp.all.Next_Pending := TID;

      if Requires_Confirm (Temp.all.Request) then
         Promise.Token := Temp.all.Cfm_Token;
         Temp.all.Cfm_Token := null;
      end if;

      pragma Assert (Temp.all.TID = TID);

      Queue.Slots (TID) := Temp;

      --  Insert into the pending requests queue

      if Queue.Has_Pending_Request then
         Queue.Slots (Queue.Last_Pending).all.Next_Pending := TID;
         Queue.Last_Pending := TID;

      else
         Queue.First_Pending := TID;
         Queue.Last_Pending := TID;
         Queue.Has_Pending_Request := True;
      end if;
   end Send_Request;

   --------------------------
   -- Try_Get_Next_Request --
   --------------------------

   procedure Try_Get_Next_Request
     (Queue : in out Queue_Type; Handle : in out Service_Handle)
   is
      TID : Transaction_ID;
   begin
      if Queue.Has_Pending_Request then
         if Queue.Last_Pending = Queue.First_Pending then

            Queue.Slots (Queue.First_Pending).all.State := Request_Read;
            Queue.Has_Pending_Request := False;

            Move
              (Target => Handle.TD,
               Source => Queue.Slots (Queue.First_Pending));
         else
            TID := Queue.First_Pending;
            Queue.Slots (TID).all.State := Request_Read;
            Queue.First_Pending := Queue.Slots (TID).all.Next_Pending;

            Move (Target => Handle.TD, Source => Queue.Slots (TID));
         end if;
      end if;
   end Try_Get_Next_Request;

   ------------------
   -- Send_Confirm --
   ------------------

   procedure Send_Confirm
     (Queue : in out Queue_Type; Handle : in out Service_Handle)
   is
      TID : constant Transaction_ID := Handle.TD.all.TID;
   begin
      Check_Slot_Is_Empty (Queue, Transaction_Handle (Handle));

      Queue.Slots (TID) := Handle.TD;
      Handle.TD := null;

      Queue.Slots (TID).all.State := Confirm_Pending;
   end Send_Confirm;

   -----------------------
   -- Request_Completed --
   -----------------------

   procedure Request_Completed
     (Queue : in out Queue_Type; Handle : in out Service_Handle)
   is
      TID : constant Transaction_ID := Handle.TD.all.TID;
   begin
      Check_Slot_Is_Empty (Queue, Transaction_Handle (Handle));

      Queue.Slots (TID) := Handle.TD;
      Handle.TD := null;

      Queue.Slots (TID).all.State := Free;
      Queue.Has_Free_Slot := True;
   end Request_Completed;

   -------------------------
   -- Has_Pending_Confirm --
   -------------------------

   function Has_Pending_Confirm
     (Queue : Queue_Type; Promise : Confirm_Promise) return Boolean
   is (Queue.Slots (Promise.Token.all.TID) /= null
       and then
         Queue.Slots (Promise.Token.all.TID).all.State = Confirm_Pending);

   ---------------------
   -- Try_Get_Confirm --
   ---------------------

   procedure Try_Get_Confirm
     (Queue   : in out Queue_Type;
      Handle  : in out Confirm_Handle;
      Promise : in out Confirm_Promise)
   is
      TID : constant Transaction_ID := Promise.Token.all.TID;
   begin
      if Has_Pending_Confirm (Queue, Promise) then
         Queue.Slots (TID).all.State := Confirm_Read;

         Move
           (Target => Queue.Slots (TID).all.Cfm_Token,
            Source => Promise.Token);

         Move (Target => Handle.TD, Source => Queue.Slots (TID));
      end if;
   end Try_Get_Confirm;

   -------------
   -- Release --
   -------------

   procedure Release
     (Queue : in out Queue_Type; Handle : in out Confirm_Handle)
   is
      TID : constant Transaction_ID := Handle.TD.all.TID;
   begin
      Check_Slot_Is_Empty (Queue, Transaction_Handle (Handle));

      Queue.Slots (TID) := Handle.TD;
      Handle.TD := null;

      Queue.Slots (TID).all.State := Free;
      Queue.Has_Free_Slot := True;
   end Release;

   -----------------
   -- New_Request --
   -----------------

   procedure New_Request
     (Cfm_Handle : in out Confirm_Handle; Req_Handle : in out Request_Handle)
   is
      Temp : constant Transaction_Data_Access := Cfm_Handle.TD;
   begin
      Cfm_Handle.TD := null;
      Temp.all.State := Request_Allocated;
      Req_Handle.TD := Temp;
   end New_Request;

   ---------------------
   -- Allocate_Memory --
   ---------------------

   procedure Allocate_Memory (Queue : in out Queue_Type) is
   begin
      Queue.Has_Free_Slot := True;
      Queue.Has_Pending_Request := False;

      for I in Transaction_ID loop
         pragma Loop_Invariant (Queue.Has_Free_Slot = True);
         pragma Loop_Invariant (Queue.Has_Pending_Request = False);

         pragma
           Loop_Invariant
             (for all J in Transaction_ID =>
                (if J < I
                 then
                   Queue.Slots (J) /= null
                   and then Queue.Slots (J).all.State = Free
                 else Queue.Slots (J) = null));

         declare
            Token : constant Confirm_Promise_Token_Access :=
              new Confirm_Promise_Token'(TID => I);
         begin
            Queue.Slots (I) :=
              new Transaction_Data'
                (TID          => I,
                 Request      => <>,
                 Confirm      => <>,
                 State        => Free,
                 Next_Pending => Transaction_ID'First,
                 Cfm_Token    => Token);
         end;
      end loop;
   end Allocate_Memory;

   ---------------------------
   -- Claim_Single_Instance --
   ---------------------------

   procedure Claim_Single_Instance (Queue : in out Queue_Type) is
   begin
      Move (Target => Queue, Source => Memory_Holder);
   end Claim_Single_Instance;

begin
   Allocate_Memory (Memory_Holder);
end LibSAP.Singleton_Transaction_Queues;
