--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body LibSAP.Singleton_Transaction_Queues
  with Refined_State => (Single_Instance => Memory_Holder)
is

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
      Request   : aliased Request_Type;
      Confirm   : aliased Confirm_Type;
      State     : Slot_States;
      Cfm_Token : Confirm_Promise_Token_Access;
   end record
   with Predicate => (if Cfm_Token /= null then Cfm_Token.all.TID = TID);

   type Memory_Holder_Type is record
      Slots : Transaction_Data_Access_Array;
   end record
   with
     Predicate =>
       (for all I in Transaction_ID =>
          (if Slots (I) /= null
           then
             Slots (I).all.TID = I
             and then Slots (I).all.State = Free
             and then Slots (I).all.Cfm_Token /= null));

   Memory_Holder : Memory_Holder_Type := (Slots => [others => null]);

   procedure Allocate_Memory
   with
     Global => (In_Out => Memory_Holder),
     Pre    => (for all S of Memory_Holder.Slots => S = null),
     Post   => (for all S of Memory_Holder.Slots => S /= null);

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

   ------------------------
   -- Property Functions --
   ------------------------

   subtype Valid_Queue_Slot_State is Slot_States
   with
     Static_Predicate =>
       Valid_Queue_Slot_State in Free | Request_Pending | Confirm_Pending;
   --  Set of states that a transaction can be in while it is held in the
   --  transaction queue. The transaction can only be in other states while
   --  it is held by a handle.

   function P_Slot_Index_Matches_TID (Queue : Queue_Type) return Boolean
   is (for all I in Queue.Slots'Range =>
         (if Queue.Slots (I) /= null then Queue.Slots (I).all.TID = I))
   with Ghost;
   --  True when the TID of all transactions matches the index of the slot
   --  they're stored in.

   function P_Cfm_Token_Present (Queue : Queue_Type) return Boolean
   is (for all S of Queue.Slots =>
         (if S /= null
          then
            (if S.all.State = Request_Pending
             then (S.all.Cfm_Token = null) = Requires_Confirm (S.all.Request)
             elsif S.all.State = Confirm_Pending
             then S.all.Cfm_Token = null
             elsif S.all.State = Free
             then S.all.Cfm_Token /= null)))
   with Ghost;
   --  Cfm_Token is not held by a transaction only when:
   --    * the transaction is in the Cfm_Pending state; or
   --    * the transaction is in the Request_Pending state and the request
   --      requires a confirmation.

   function P_Has_Free_Slot_Valid (Queue : Queue_Type) return Boolean
   is (if Queue.Has_Free_Slot
       then
         (for some S of Queue.Slots => S /= null and then S.all.State = Free))
   with Ghost;
   --  If Has_Free_Slot is true, then there is at least one slot that is in the
   --  Free state.

   function P_Has_Pending_Request_Valid (Queue : Queue_Type) return Boolean
   is (Queue.Has_Pending_Request
       = (for some S of Queue.Slots =>
            S /= null and then S.all.State = Request_Pending)

       and then
         Queue.Has_Pending_Request
         = not LibSAP.Unique_Integer_Queues.Is_Empty (Queue.Pending_Queue))
   with Ghost;

   function P_Confirm_Pending_Valid (Queue : Queue_Type) return Boolean
   is (for all S of Queue.Slots =>
         (if S /= null and then S.all.State = Confirm_Pending
          then
            Requires_Confirm (S.all.Request)
            and then Valid_Confirm (S.all.Request, S.all.Confirm)))
   with Ghost;
   --  Slots in the Confirm_Pending state have a valid confirm object

   function P_Pending_Requests_In_Queue (Queue : Queue_Type) return Boolean
   is (for all I in Transaction_ID =>
         LibSAP.Unique_Integer_Queues.Contains (Queue.Pending_Queue, I)
         = (Queue.Slots (I) /= null
            and then Queue.Slots (I).all.State = Request_Pending))
   with Ghost;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Queue : Queue_Type) return Boolean
   is ((for all S of Queue.Slots =>
          (if S /= null then S.all.State in Valid_Queue_Slot_State))

       and then P_Slot_Index_Matches_TID (Queue)
       and then P_Cfm_Token_Present (Queue)
       and then P_Has_Free_Slot_Valid (Queue)
       and then P_Has_Pending_Request_Valid (Queue)
       and then P_Confirm_Pending_Valid (Queue)
       and then P_Pending_Requests_In_Queue (Queue));

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
         (if TD.all.State = Confirm_Written
          then
            Requires_Confirm (TD.all.Request)
            and then Valid_Confirm (TD.all.Request, TD.all.Confirm))

       and then (TD.all.Cfm_Token = null) = Requires_Confirm (TD.all.Request));

   ------------------------------
   -- Confirm_Handle_Predicate --
   ------------------------------

   function Confirm_Handle_Predicate
     (TD : not null Transaction_Data_Access) return Boolean
   is (TD.all.State = Confirm_Read
       and then TD.all.Cfm_Token /= null
       and then Requires_Confirm (TD.all.Request)
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

   ------------------------------
   -- Build_Contextual_Request --
   ------------------------------

   procedure Build_Contextual_Request (Handle : in out Request_Handle) is
   begin
      Handle.TD.all.State := Request_Written;

      pragma Assert (Precondition);

      Build (Handle.TD.all.Request);

      pragma Assert (Postcondition);
   end Build_Contextual_Request;

   -----------------------------------------
   -- Build_Contextual_Request_No_Confirm --
   -----------------------------------------

   procedure Build_Contextual_Request_No_Confirm
     (Handle : in out Request_Handle) is
   begin
      Handle.TD.all.State := Request_Written;

      pragma Assert (Precondition);

      Build (Handle.TD.all.Request);

      pragma Assert (Postcondition);
      pragma Assert (not Requires_Confirm (Handle.TD.all.Request));

   end Build_Contextual_Request_No_Confirm;

   -------------------------------------------
   -- Build_Contextual_Request_With_Confirm --
   -------------------------------------------

   procedure Build_Contextual_Request_With_Confirm
     (Handle : in out Request_Handle) is
   begin
      Handle.TD.all.State := Request_Written;

      pragma Assert (Precondition);

      Build (Handle.TD.all.Request);

      pragma Assert (Postcondition);
      pragma Assert (Requires_Confirm (Handle.TD.all.Request));

   end Build_Contextual_Request_With_Confirm;

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

      if Requires_Confirm (Temp.all.Request) then
         Promise.Token := Temp.all.Cfm_Token;
         Temp.all.Cfm_Token := null;
      end if;

      pragma Assert (Temp.all.TID = TID);

      Queue.Slots (TID) := Temp;

      Queue.Has_Pending_Request := True;
      LibSAP.Unique_Integer_Queues.Append (Queue.Pending_Queue, TID);
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
         LibSAP.Unique_Integer_Queues.Pop_Front (Queue.Pending_Queue, TID);

         Queue.Has_Pending_Request :=
           not LibSAP.Unique_Integer_Queues.Is_Empty (Queue.Pending_Queue);

         Queue.Slots (TID).all.State := Request_Read;
         Move (Target => Handle.TD, Source => Queue.Slots (TID));
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

   procedure Allocate_Memory is
   begin
      for I in Transaction_ID loop
         pragma
           Loop_Invariant
             (for all J in Transaction_ID =>
                (if J < I
                 then
                   Memory_Holder.Slots (J) /= null
                   and then Memory_Holder.Slots (J).all.State = Free
                 else Memory_Holder.Slots (J) = null));

         declare
            Token : constant Confirm_Promise_Token_Access :=
              new Confirm_Promise_Token'(TID => I);
         begin
            Memory_Holder.Slots (I) :=
              new Transaction_Data'
                (TID       => I,
                 Request   => <>,
                 Confirm   => <>,
                 State     => Free,
                 Cfm_Token => Token);
         end;
      end loop;
   end Allocate_Memory;

   ---------------------------
   -- Claim_Single_Instance --
   ---------------------------

   procedure Claim_Single_Instance (Queue : in out Queue_Type) is
   begin
      Queue.Has_Free_Slot := False;
      Queue.Has_Pending_Request := False;

      for I in Transaction_ID loop
         pragma Loop_Invariant (Is_Valid (Queue));
         pragma Loop_Invariant (not Queue.Has_Pending_Request);

         pragma
           Loop_Invariant
             (for all S of Queue.Slots =>
                (if S /= null then S.all.State = Free));

         pragma
           Loop_Invariant
             (for all J in Transaction_ID =>
                (if J < I
                 then Memory_Holder.Slots (J) = null
                 else Queue.Slots (J) = null));

         if Memory_Holder.Slots (I) /= null then
            Queue.Has_Free_Slot := True;
            Move
              (Target => Queue.Slots (I), Source => Memory_Holder.Slots (I));
         end if;
      end loop;
   end Claim_Single_Instance;

begin
   Allocate_Memory;
end LibSAP.Singleton_Transaction_Queues;
