--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with LibSAP.Pointer_Holders;

package body LibSAP.Singleton_Transaction_Queues
  with
    Refined_State =>
      (Transaction_Pool =>
         (Free_Pool.Pointer_Pool, Pending_Confirms_Pool.Pointer_Pool))
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

   type Allocatable_Transaction_Data_Access is access Transaction_Data;

   ---------------
   -- Free Pool --
   ---------------

   subtype Free_Transaction_Data_Access is Transaction_Data_Access
   with
     Predicate =>
       (if Free_Transaction_Data_Access /= null
        then
          Free_Transaction_Data_Access.all.State = Free
          and then Free_Transaction_Data_Access.all.Cfm_Token /= null);

   package Free_Pool is new
     LibSAP.Pointer_Holders
       (Element_ID     => Transaction_ID,
        Element_Type   => Transaction_Data,
        Element_Access => Free_Transaction_Data_Access);

   ---------------------------
   -- Pending Confirms Pool --
   ---------------------------

   subtype Confirm_Pending_Transaction_Data_Access is Transaction_Data_Access
   with
     Predicate =>
       (if Confirm_Pending_Transaction_Data_Access /= null
        then
          Confirm_Pending_Transaction_Data_Access.all.State = Confirm_Pending
          and then Confirm_Pending_Transaction_Data_Access.all.Cfm_Token = null
          and then
            Requires_Confirm
              (Confirm_Pending_Transaction_Data_Access.all.Request)
          and then
            Valid_Confirm
              (Confirm_Pending_Transaction_Data_Access.all.Request,
               Confirm_Pending_Transaction_Data_Access.all.Confirm));

   package Pending_Confirms_Pool is new
     LibSAP.Pointer_Holders
       (Element_ID     => Transaction_ID,
        Element_Type   => Transaction_Data,
        Element_Access => Confirm_Pending_Transaction_Data_Access);

   procedure Fill_Free_Pool
   with Global => (In_Out => Free_Pool.Pointer_Pool);
   --  Allocates Transaction_Data objects (one per TID) and stores them
   --  in the Free_Pool.
   --
   --  This must be called once only, during elaboration.

   procedure Store_In_Free_Pool (Ptr : in out Free_Transaction_Data_Access)
   with
     Global => (In_Out => Free_Pool.Pointer_Pool),
     Pre    => Ptr /= null,
     Post   => Ptr = null;

   procedure Store_In_Pending_Confirms_Pool
     (Ptr : in out Confirm_Pending_Transaction_Data_Access)
   with
     Global => (In_Out => Pending_Confirms_Pool.Pointer_Pool),
     Pre    => Ptr /= null,
     Post   => Ptr = null;

   -------------------------------
   -- Pending_Request_Predicate --
   -------------------------------

   function Pending_Request_Predicate
     (TD : not null Transaction_Data_Access) return Boolean
   is (TD.all.State = Request_Pending
       and then (TD.all.Cfm_Token = null) = Requires_Confirm (TD.all.Request));

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

   -------------------------
   -- Has_Pending_Request --
   -------------------------

   function Has_Pending_Request (Queue : Transaction_Queue_Type) return Boolean
   is (Transaction_Data_Access_Queues.Length (Queue.Pending_Queue) > 0);

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

   procedure Try_Allocate_Request (Handle : in out Request_Handle) is
   begin
      for I in Transaction_ID loop
         declare
            Free_Ptr : Free_Transaction_Data_Access;
            Temp     : Transaction_Data_Access;
         begin
            Free_Pool.Retrieve (I, Free_Ptr);

            if Free_Ptr /= null then
               Temp := Transaction_Data_Access (Free_Ptr);
               Temp.all.State := Request_Allocated;
               Handle.TD := Temp;
               exit;
            end if;
         end;
      end loop;
   end Try_Allocate_Request;

   -------------------
   -- Abort_Request --
   -------------------

   procedure Abort_Request (Handle : in out Request_Handle) is
      Temp     : Transaction_Data_Access;
      Free_Ptr : Free_Transaction_Data_Access;
   begin
      Temp := Handle.TD;
      Handle.TD := null;

      Temp.all.State := Free;
      Free_Ptr := Free_Transaction_Data_Access (Temp);

      Store_In_Free_Pool (Free_Ptr);

      pragma Unreferenced (Free_Ptr);
   end Abort_Request;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Queue   : in out Transaction_Queue_Type;
      Handle  : in out Request_Handle;
      Promise : in out Confirm_Promise)
   is
      package TDAQ renames Transaction_Data_Access_Queues;

      Temp : Transaction_Data_Access;

   begin
      Temp := Handle.TD;
      Handle.TD := null;

      Temp.all.State := Request_Pending;

      if Requires_Confirm (Temp.all.Request) then
         Promise.Token := Temp.all.Cfm_Token;
         Temp.all.Cfm_Token := null;
      end if;

      --  Rationale for pragma Assume:
      --
      --  If we have a pointer to a Transaction_Data (currently held in Temp at
      --  this point), then the Pending_Queue cannot be full. This is because
      --  there are only ever Queue_Capacity Transaction_Data objects in
      --  existence at a time (they are allocated one time, during the
      --  elaboration of this package), and SPARK's ownership rules ensure that
      --  a pointer to each object can exist in one place at a time. So if we
      --  have a valid pointer in Temp, then there must be at least one free
      --  space in Pending_Queue.

      pragma Assume (TDAQ.Length (Queue.Pending_Queue) < Queue_Capacity);

      --  Defensive check for the above assumption.

      if TDAQ.Length (Queue.Pending_Queue) = Queue_Capacity then
         raise Program_Error;
      end if;

      TDAQ.Append (Queue.Pending_Queue, Temp);
   end Send_Request;

   --------------------------
   -- Try_Get_Next_Request --
   --------------------------

   procedure Try_Get_Next_Request
     (Queue : in out Transaction_Queue_Type; Handle : in out Service_Handle)
   is
      package TDAQ renames Transaction_Data_Access_Queues;

      Pending_Ptr : Pending_Request_Transaction_Data_Access;
      Temp        : Transaction_Data_Access;

   begin
      if TDAQ.Length (Queue.Pending_Queue) > 0 then
         Transaction_Data_Access_Queues.Pop_Front
           (Queue.Pending_Queue, Pending_Ptr);

         Temp := Transaction_Data_Access (Pending_Ptr);

         Temp.all.State := Request_Read;
         Handle.TD := Temp;
      end if;
   end Try_Get_Next_Request;

   ------------------
   -- Send_Confirm --
   ------------------

   procedure Send_Confirm (Handle : in out Service_Handle) is
      Temp        : Transaction_Data_Access;
      Pending_Ptr : Confirm_Pending_Transaction_Data_Access;
   begin
      Temp := Handle.TD;
      Handle.TD := null;

      Temp.all.State := Confirm_Pending;
      Pending_Ptr := Confirm_Pending_Transaction_Data_Access (Temp);

      Store_In_Pending_Confirms_Pool (Pending_Ptr);

      pragma Unreferenced (Pending_Ptr);
   end Send_Confirm;

   -----------------------
   -- Request_Completed --
   -----------------------

   procedure Request_Completed (Handle : in out Service_Handle) is
      Temp     : Transaction_Data_Access;
      Free_Ptr : Free_Transaction_Data_Access;
   begin
      Temp := Handle.TD;
      Handle.TD := null;

      Temp.all.State := Free;
      Free_Ptr := Free_Transaction_Data_Access (Temp);

      Store_In_Free_Pool (Free_Ptr);

      pragma Unreferenced (Free_Ptr);
   end Request_Completed;

   ---------------------
   -- Try_Get_Confirm --
   ---------------------

   procedure Try_Get_Confirm
     (Handle : in out Confirm_Handle; Promise : in out Confirm_Promise)
   is
      TID : constant Transaction_ID := Promise.Token.all.TID;

      Pending_Ptr : Confirm_Pending_Transaction_Data_Access;
      Temp        : Transaction_Data_Access;
   begin
      Pending_Confirms_Pool.Retrieve (TID, Pending_Ptr);

      if Pending_Ptr /= null then
         Temp := Transaction_Data_Access (Pending_Ptr);

         Temp.all.Cfm_Token := Promise.Token;
         Promise.Token := null;

         Temp.all.State := Confirm_Read;
         Handle.TD := Temp;
      end if;
   end Try_Get_Confirm;

   -------------
   -- Release --
   -------------

   procedure Release (Handle : in out Confirm_Handle) is
      Temp     : Transaction_Data_Access;
      Free_Ptr : Free_Transaction_Data_Access;
   begin
      Temp := Handle.TD;
      Handle.TD := null;

      Temp.all.State := Free;
      Free_Ptr := Free_Transaction_Data_Access (Temp);

      Store_In_Free_Pool (Free_Ptr);

      pragma Unreferenced (Free_Ptr);
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

   ------------------------
   -- Store_In_Free_Pool --
   ------------------------

   procedure Store_In_Free_Pool (Ptr : in out Free_Transaction_Data_Access) is
   begin
      pragma Assert (Ptr /= null);

      Free_Pool.Exchange (Ptr);

      --  Rationale for pragma Assume:
      --
      --  There is only ever one instance of each Transaction_Data object
      --  for each TID, and SPARK's ownership rules guarantee that a pointer to
      --  each object can only ever be owned by one place at a time, either:
      --   * in the free pool;
      --   * in the confirm pending pool;
      --   * in a pending request queue; or
      --   * in a handle.
      --
      --  Therefore, if we had a pointer to a Transaction_Data object with a
      --  specific TID on entry to this procedure, then the corresponding
      --  slot for that TID in the free pool must have been null.
      --
      --  Note that the uniqueness of Transaction_Data objects (one per TID)
      --  is guaranteed by:
      --   * Transaction_Data objects cannot be allocated or created outside of
      --     this package body since the Transaction_Data type in the spec is
      --     incomplete, so the only place objects can be created is in this
      --     package body.
      --   * Transaction_Data objects are allocated once only in this package
      --     body (during elaboration) and it allocates exactly one object per
      --     TID.

      pragma Assume (Ptr = null);

      --  Defensive check for the above assumption

      if Ptr /= null then
         raise Program_Error;
      end if;
   end Store_In_Free_Pool;

   ------------------------------------
   -- Store_In_Pending_Confirms_Pool --
   ------------------------------------

   procedure Store_In_Pending_Confirms_Pool
     (Ptr : in out Confirm_Pending_Transaction_Data_Access) is
   begin
      pragma Assert (Ptr /= null);

      Pending_Confirms_Pool.Exchange (Ptr);

      --  Rationale for pragma Assume is the same as the rationale
      --  described in Store_In_Free_Pool.

      pragma Assume (Ptr = null);

      --  Defensive check for the above assumption

      if Ptr /= null then
         raise Program_Error;
      end if;
   end Store_In_Pending_Confirms_Pool;

   --------------------
   -- Fill_Free_Pool --
   --------------------

   procedure Fill_Free_Pool is
   begin
      for I in Transaction_ID loop
         declare
            Token : constant Confirm_Promise_Token_Access :=
              new Confirm_Promise_Token'(TID => I);

            Obj : constant Allocatable_Transaction_Data_Access :=
              new Transaction_Data'
                (TID       => I,
                 Request   => <>,
                 Confirm   => <>,
                 State     => Free,
                 Cfm_Token => Token);

            Ptr : Free_Transaction_Data_Access :=
              Free_Transaction_Data_Access (Obj);
         begin
            Free_Pool.Exchange (Ptr);

            --  Rationale for pragma Assume:
            --
            --  The Free pool is initially all null, and it is not possible for
            --  anything else to store a pointer in Free_Pool before this
            --  package is elaborated, so Ptr must be null here.

            pragma
              Assume
                (Ptr = null,
                 "Slots in Free_Pool are initially null after elaboration");

            --  Defensive check for the above assumption

            if Ptr /= null then
               raise Program_Error;
            end if;
         end;
      end loop;
   end Fill_Free_Pool;

begin
   Fill_Free_Pool;
end LibSAP.Singleton_Transaction_Queues;
