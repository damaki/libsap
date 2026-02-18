--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Concurrency in SPARK is limited to the Ravenscar or Jorvik profiles, and
--  requires a sequential partition elaboration policy.
--
--  See the SPARK User's Guide (26.1) section 5.10

pragma Profile (Jorvik);
pragma Partition_Elaboration_Policy (Sequential);

package body LibSAP.Synchronous_User_Service_Access_Point
  with
    Refined_State =>
      (Transaction_Queue => Protected_Queue,
       Queue_Memory      => STQ.Single_Instance)
is

   type Holder_Data is limited record
      Queue                  : STQ.Valid_Queue_Type;
      Has_Free_Slot          : Boolean := False;
      Has_Pending_Indication : Boolean := False;
   end record;

   function Is_Valid (HD : Holder_Data) return Boolean
   is ((if HD.Has_Free_Slot then STQ.Can_Allocate_Request (HD.Queue))
       and then
         (if HD.Has_Pending_Indication
          then STQ.Has_Pending_Request (HD.Queue)));

   subtype Valid_Holder_Data is Holder_Data
   with Predicate => Is_Valid (Valid_Holder_Data);

   protected Protected_Queue
     with Priority => Priority
   is

      procedure Claim_Memory;

      entry Allocate_Indication (Handle : in out STQ.Request_Handle)
      with
        Pre  => STQ.Is_Null (Handle),
        Post => not STQ.Is_Null (Handle) and not STQ.Request_Ready (Handle);

      procedure Abort_Indication (Handle : in out STQ.Request_Handle)
      with Pre => not STQ.Is_Null (Handle), Post => STQ.Is_Null (Handle);

      procedure Try_Allocate_Indication (Handle : in out STQ.Request_Handle)
      with
        Pre  => STQ.Is_Null (Handle),
        Post =>
          (if not STQ.Is_Null (Handle) then not STQ.Request_Ready (Handle));

      procedure Send_Indication
        (Handle  : in out STQ.Request_Handle;
         Promise : in out STQ.Confirm_Promise)
      with
        Pre            =>
          not STQ.Is_Null (Handle)
          and then STQ.Is_Null (Promise)
          and then STQ.Request_Ready (Handle),
        Post           => STQ.Is_Null (Handle),
        Contract_Cases =>
          (STQ.Requires_Confirm (Handle) => not STQ.Is_Null (Promise),
           others                        => STQ.Is_Null (Promise));

      entry Get_Next_Indication (Handle : in out STQ.Service_Handle)
      with Pre => STQ.Is_Null (Handle), Post => not STQ.Is_Null (Handle);

      procedure Indication_Completed (Handle : in out STQ.Service_Handle)
      with
        Pre  =>
          not STQ.Is_Null (Handle) and then not STQ.Requires_Confirm (Handle),
        Post => STQ.Is_Null (Handle);

      procedure Send_Response (Handle : in out STQ.Service_Handle)
      with
        Pre  =>
          not STQ.Is_Null (Handle)
          and then STQ.Requires_Confirm (Handle)
          and then STQ.Has_Valid_Confirm (Handle),
        Post => STQ.Is_Null (Handle);

      procedure Try_Get_Response
        (Handle  : in out STQ.Confirm_Handle;
         Promise : in out STQ.Confirm_Promise)
      with
        Pre  => STQ.Is_Null (Handle) and then not STQ.Is_Null (Promise),
        Post => STQ.Is_Null (Handle) = not STQ.Is_Null (Promise);

      procedure Release (Handle : in out STQ.Confirm_Handle)
      with Pre => not STQ.Is_Null (Handle), Post => STQ.Is_Null (Handle);

   private

      Data : Valid_Holder_Data := (others => <>);

   end Protected_Queue;

   ---------------------
   -- Protected_Queue --
   ---------------------

   protected body Protected_Queue is

      ------------------
      -- Claim_Memory --
      ------------------

      procedure Claim_Memory is
      begin
         if STQ.Is_Empty (Data.Queue) then
            STQ.Claim_Single_Instance (Data.Queue);

            Data.Has_Free_Slot := STQ.Can_Allocate_Request (Data.Queue);
            Data.Has_Pending_Indication :=
              STQ.Has_Pending_Request (Data.Queue);
         end if;
      end Claim_Memory;

      -------------------------
      -- Allocate_Indication --
      -------------------------

      entry Allocate_Indication (Handle : in out STQ.Request_Handle)
        when Data.Has_Free_Slot
      is

         --  Operate on type Holder_Data instead of Valid_Holder_Data to
         --  allow the type predicate to be (temporarily) violated.

         procedure Wrapper (HD : in out Holder_Data)
         with
           Inline,
           Pre  =>
             Is_Valid (HD)
             and then STQ.Is_Null (Handle)
             and then HD.Has_Free_Slot,
           Post =>
             Is_Valid (HD)
             and then not STQ.Is_Null (Handle)
             and then not STQ.Request_Ready (Handle);

         procedure Wrapper (HD : in out Holder_Data) is
         begin
            STQ.Try_Allocate_Request (HD.Queue, Handle);

            HD.Has_Free_Slot := STQ.Can_Allocate_Request (HD.Queue);
         end Wrapper;

      begin
         Wrapper (Data);
      end Allocate_Indication;

      ----------------------
      -- Abort_Indication --
      ----------------------

      procedure Abort_Indication (Handle : in out STQ.Request_Handle) is
      begin
         STQ.Abort_Request (Data.Queue, Handle);

         Data.Has_Free_Slot := True;
      end Abort_Indication;

      -----------------------------
      -- Try_Allocate_Indication --
      -----------------------------

      procedure Try_Allocate_Indication (Handle : in out STQ.Request_Handle) is

         --  Operate on type Holder_Data instead of Valid_Holder_Data to
         --  allow the type predicate to be (temporarily) violated.

         procedure Wrapper (HD : in out Holder_Data)
         with
           Inline,
           Pre  => Is_Valid (HD) and then STQ.Is_Null (Handle),
           Post =>
             Is_Valid (HD)
             and then
               (if not STQ.Is_Null (Handle)
                then not STQ.Request_Ready (Handle));

         procedure Wrapper (HD : in out Holder_Data) is
         begin
            STQ.Try_Allocate_Request (HD.Queue, Handle);

            HD.Has_Free_Slot := STQ.Can_Allocate_Request (HD.Queue);
         end Wrapper;

      begin
         Wrapper (Data);
      end Try_Allocate_Indication;

      ---------------------
      -- Send_Indication --
      ---------------------

      procedure Send_Indication
        (Handle  : in out STQ.Request_Handle;
         Promise : in out STQ.Confirm_Promise) is
      begin
         STQ.Send_Request (Data.Queue, Handle, Promise);

         Data.Has_Pending_Indication := True;
      end Send_Indication;

      -------------------------
      -- Get_Next_Indication --
      -------------------------

      entry Get_Next_Indication (Handle : in out STQ.Service_Handle)
        when Data.Has_Pending_Indication
      is

         --  Operate on type Holder_Data instead of Valid_Holder_Data to
         --  allow the type predicate to be (temporarily) violated.

         procedure Wrapper (HD : in out Holder_Data)
         with
           Inline,
           Pre  =>
             Is_Valid (HD)
             and then STQ.Is_Null (Handle)
             and then HD.Has_Pending_Indication,
           Post => Is_Valid (HD) and then not STQ.Is_Null (Handle);

         procedure Wrapper (HD : in out Holder_Data) is
         begin
            STQ.Try_Get_Next_Request (HD.Queue, Handle);

            HD.Has_Pending_Indication := STQ.Has_Pending_Request (HD.Queue);
         end Wrapper;

      begin
         Wrapper (Data);
      end Get_Next_Indication;

      --------------------------
      -- Indication_Completed --
      --------------------------

      procedure Indication_Completed (Handle : in out STQ.Service_Handle) is
      begin
         STQ.Request_Completed (Data.Queue, Handle);

         Data.Has_Free_Slot := True;
      end Indication_Completed;

      -------------------
      -- Send_Response --
      -------------------

      procedure Send_Response (Handle : in out STQ.Service_Handle) is
      begin
         STQ.Send_Confirm (Data.Queue, Handle);
      end Send_Response;

      ----------------------
      -- Try_Get_Response --
      ----------------------

      procedure Try_Get_Response
        (Handle  : in out STQ.Confirm_Handle;
         Promise : in out STQ.Confirm_Promise) is
      begin
         STQ.Try_Get_Confirm (Data.Queue, Handle, Promise);
      end Try_Get_Response;

      -------------
      -- Release --
      -------------

      procedure Release (Handle : in out STQ.Confirm_Handle) is
      begin
         STQ.Release (Data.Queue, Handle);

         Data.Has_Free_Slot := True;
      end Release;

   end Protected_Queue;

   --=========================--
   -- Public Operation Bodies --
   --=========================--

   ------------------------
   -- Response_Reference --
   ------------------------

   function Response_Reference
     (Handle : Response_Handle) return not null access constant Response_Type
   is (STQ.Confirm_Reference (Handle.Handle));

   --------------------------
   -- Indication_Reference --
   --------------------------

   function Indication_Reference
     (Handle : Indication_Handle)
      return not null access constant Indication_Type
   is (STQ.Request_Reference (Handle.Handle));

   function Indication_Reference
     (Handle : Service_Handle) return not null access constant Indication_Type
   is (STQ.Request_Reference (Handle.Handle));

   function Indication_Reference
     (Handle : Response_Handle) return not null access constant Indication_Type
   is (STQ.Request_Reference (Handle.Handle));

   -----------------------
   -- Requires_Response --
   -----------------------

   function Requires_Response (Handle : Indication_Handle) return Boolean
   is (STQ.Requires_Confirm (Handle.Handle));

   function Requires_Response (Handle : Service_Handle) return Boolean
   is (STQ.Requires_Confirm (Handle.Handle));

   ------------------------
   -- Has_Valid_Response --
   ------------------------

   function Has_Valid_Response (Handle : Service_Handle) return Boolean
   is (STQ.Has_Valid_Confirm (Handle.Handle));

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : in out Indication_Handle; Source : in out Indication_Handle) is
   begin
      STQ.Move (Target => Target.Handle, Source => Source.Handle);
   end Move;

   procedure Move
     (Target : in out Response_Promise; Source : in out Response_Promise) is
   begin
      STQ.Move (Target => Target.Handle, Source => Source.Handle);
   end Move;

   procedure Move
     (Target : in out Response_Handle; Source : in out Response_Handle) is
   begin
      STQ.Move (Target => Target.Handle, Source => Source.Handle);
   end Move;

   procedure Move
     (Target : in out Service_Handle; Source : in out Service_Handle) is
   begin
      STQ.Move (Target => Target.Handle, Source => Source.Handle);
   end Move;

   -------------------------
   -- Allocate_Indication --
   -------------------------

   procedure Allocate_Indication (Handle : in out Indication_Handle) is
   begin
      Protected_Queue.Allocate_Indication (Handle.Handle);
   end Allocate_Indication;

   ----------------------
   -- Abort_Indication --
   ----------------------

   procedure Abort_Indication (Handle : in out Indication_Handle) is
   begin
      Protected_Queue.Abort_Indication (Handle.Handle);
   end Abort_Indication;

   -----------------------------
   -- Try_Allocate_Indication --
   -----------------------------

   procedure Try_Allocate_Indication (Handle : in out Indication_Handle) is
   begin
      Protected_Queue.Try_Allocate_Indication (Handle.Handle);
   end Try_Allocate_Indication;

   ----------------------
   -- Build_Indication --
   ----------------------

   procedure Build_Indication (Handle : in out Indication_Handle) is
      procedure Build_Wrapper is new STQ.Build_Request (Build);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Indication;

   ----------------------------------
   -- Build_Indication_No_Response --
   ----------------------------------

   procedure Build_Indication_No_Response (Handle : in out Indication_Handle)
   is
      procedure Build_Wrapper is new STQ.Build_Request_No_Confirm (Build);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Indication_No_Response;

   ------------------------------------
   -- Build_Indication_With_Response --
   ------------------------------------

   procedure Build_Indication_With_Response (Handle : in out Indication_Handle)
   is
      procedure Build_Wrapper is new STQ.Build_Request_With_Confirm (Build);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Indication_With_Response;

   ---------------------------------
   -- Build_Contextual_Indication --
   ---------------------------------

   procedure Build_Contextual_Indication (Handle : in out Indication_Handle) is
      procedure Build_Wrapper is new
        STQ.Build_Contextual_Request
          (Build         => Build,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Contextual_Indication;

   ---------------------------------------------
   -- Build_Contextual_Indication_No_Response --
   ---------------------------------------------

   procedure Build_Contextual_Indication_No_Response
     (Handle : in out Indication_Handle)
   is
      procedure Build_Wrapper is new
        STQ.Build_Contextual_Request_No_Confirm
          (Build         => Build,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Contextual_Indication_No_Response;

   -----------------------------------------------
   -- Build_Contextual_Indication_With_Response --
   -----------------------------------------------

   procedure Build_Contextual_Indication_With_Response
     (Handle : in out Indication_Handle)
   is
      procedure Build_Wrapper is new
        STQ.Build_Contextual_Request_With_Confirm
          (Build         => Build,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Contextual_Indication_With_Response;

   ---------------------
   -- Send_Indication --
   ---------------------

   procedure Send_Indication
     (Handle : in out Indication_Handle; Promise : in out Response_Promise) is
   begin
      Protected_Queue.Send_Indication (Handle.Handle, Promise.Handle);
   end Send_Indication;

   -------------------------------
   -- Build_And_Send_Indication --
   -------------------------------

   procedure Build_And_Send_Indication (Promise : in out Response_Promise) is

      procedure Build_Indication_Wrapper is new Build_Indication (Build);

      Handle : Indication_Handle;

   begin
      Allocate_Indication (Handle);
      Build_Indication_Wrapper (Handle);
      Send_Indication (Handle, Promise);

      pragma Unreferenced (Handle);
   end Build_And_Send_Indication;

   -------------------------------------------
   -- Build_And_Send_Indication_No_Response --
   -------------------------------------------

   procedure Build_And_Send_Indication_No_Response is

      procedure Build_Indication_Wrapper is new
        Build_Indication_No_Response (Build);

      Handle  : Indication_Handle;
      Promise : Response_Promise;
   begin
      Allocate_Indication (Handle);
      Build_Indication_Wrapper (Handle);
      Send_Indication (Handle, Promise);

      pragma Unreferenced (Handle);
      pragma Unreferenced (Promise);
   end Build_And_Send_Indication_No_Response;

   ---------------------------------------------
   -- Build_And_Send_Indication_With_Response --
   ---------------------------------------------

   procedure Build_And_Send_Indication_With_Response
     (Promise : in out Response_Promise)
   is

      procedure Build_Indication_Wrapper is new
        Build_Indication_With_Response (Build);

      Handle : Indication_Handle;

   begin
      Allocate_Indication (Handle);
      Build_Indication_Wrapper (Handle);
      Send_Indication (Handle, Promise);

      pragma Unreferenced (Handle);
   end Build_And_Send_Indication_With_Response;

   -----------------------------------
   -- Try_Build_And_Send_Indication --
   -----------------------------------

   procedure Try_Build_And_Send_Indication
     (Promise : in out Response_Promise; Was_Sent : out Boolean)
   is

      procedure Build_Indication_Wrapper is new Build_Indication (Build);

      Handle : Indication_Handle;

   begin
      Try_Allocate_Indication (Handle);

      if Is_Null (Handle) then
         Was_Sent := False;
      else
         Was_Sent := True;

         Build_Indication_Wrapper (Handle);
         Send_Indication (Handle, Promise);

         pragma Unreferenced (Handle);
      end if;
   end Try_Build_And_Send_Indication;

   -------------------------
   -- Get_Next_Indication --
   -------------------------

   procedure Get_Next_Indication (Handle : in out Service_Handle) is
   begin
      Protected_Queue.Get_Next_Indication (Handle.Handle);
   end Get_Next_Indication;

   --------------------------
   -- Indication_Completed --
   --------------------------

   procedure Indication_Completed (Handle : in out Service_Handle) is
   begin
      Protected_Queue.Indication_Completed (Handle.Handle);
   end Indication_Completed;

   -------------------
   -- Send_Response --
   -------------------

   procedure Send_Response (Handle : in out Service_Handle) is
   begin
      Protected_Queue.Send_Response (Handle.Handle);
   end Send_Response;

   ------------------------
   -- Process_Indication --
   ------------------------

   procedure Process_Indication (Handle : in out Service_Handle) is

      procedure Process_Indication_With_Response_Wrapper is new
        STQ.Build_Confirm (Process_Indication_With_Response);

      Needs_Confirm : Boolean;

   begin
      declare
         Indication : constant not null access constant Indication_Type :=
           STQ.Request_Reference (Handle.Handle);
      begin
         Needs_Confirm := Requires_Response (Indication.all);

         if not Needs_Confirm then
            Process_Indication_No_Response (Indication.all);
         end if;
      end;

      if Needs_Confirm then
         Process_Indication_With_Response_Wrapper (Handle.Handle);
      end if;
   end Process_Indication;

   --------------------
   -- Build_Response --
   --------------------

   procedure Build_Response (Handle : in out Service_Handle) is
      procedure Build_Wrapper is new STQ.Build_Confirm (Build);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Response;

   -------------
   -- Release --
   -------------

   procedure Release (Handle : in out Response_Handle) is
   begin
      Protected_Queue.Release (Handle.Handle);
   end Release;

   --------------------
   -- New_Indication --
   --------------------

   procedure New_Indication
     (Res_Handle : in out Response_Handle;
      Ind_Handle : in out Indication_Handle) is
   begin
      STQ.New_Request (Res_Handle.Handle, Ind_Handle.Handle);
   end New_Indication;

   ----------------------
   -- Try_Get_Response --
   ----------------------

   procedure Try_Get_Response
     (Handle : in out Response_Handle; Promise : in out Response_Promise) is
   begin
      Protected_Queue.Try_Get_Response (Handle.Handle, Promise.Handle);
   end Try_Get_Response;

begin
   Protected_Queue.Claim_Memory;
end LibSAP.Synchronous_User_Service_Access_Point;
