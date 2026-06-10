--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Concurrency in SPARK is limited to the Ravenscar or Jorvik profiles, and
--  requires a sequential partition elaboration policy.
--
--  See the SPARK User's Guide (26.1) section 5.10

package body LibSAP.Synchronous_User_Service_Access_Point
  with
    Refined_State =>
      (Transaction_Queue => (Protected_Queue, STQ.Transaction_Pool))
is

   type Holder_Data is limited record
      Queue                  : STQ.Transaction_Queue_Type;
      Has_Pending_Indication : Boolean := False;
   end record;

   function Is_Valid (HD : Holder_Data) return Boolean
   is (if HD.Has_Pending_Indication then STQ.Has_Pending_Request (HD.Queue));

   subtype Valid_Holder_Data is Holder_Data
   with Predicate => Is_Valid (Valid_Holder_Data);

   protected Protected_Queue
     with Priority => Priority
   is

      procedure Send_Indication
        (Handle  : in out STQ.Request_Handle;
         Promise : in out STQ.Confirm_Promise)
      with
        Pre            =>
          not STQ.Is_Null (Handle)
          and then STQ.Is_Null (Promise)
          and then STQ.Request_Written (Handle)
          and then Valid_Indication (STQ.Request_Reference (Handle).all),
        Post           => STQ.Is_Null (Handle),
        Contract_Cases =>
          (STQ.Requires_Confirm (Handle) =>
             not STQ.Is_Null (Promise)
             and (STQ.Get_TID (Promise) = STQ.Get_TID (Handle)'Old)
             and (STQ.Request_Kind (Promise) = STQ.Request_Kind (Handle)'Old),
           others                        => STQ.Is_Null (Promise));

      function Has_Pending_Indication return Boolean;

      entry Get_Next_Indication (Handle : in out STQ.Service_Handle)
      with
        Pre  => STQ.Is_Null (Handle),
        Post =>
          not STQ.Is_Null (Handle)
          and then Valid_Indication (STQ.Request_Reference (Handle).all)
          and then not STQ.Confirm_Written (Handle);

      procedure Try_Get_Next_Indication (Handle : in out STQ.Service_Handle)
      with
        Pre  => STQ.Is_Null (Handle),
        Post =>
          (if not STQ.Is_Null (Handle)
           then
             Valid_Indication (STQ.Request_Reference (Handle).all)
             and then not STQ.Confirm_Written (Handle));

   private

      Data : Valid_Holder_Data := (others => <>);

   end Protected_Queue;

   ---------------------
   -- Protected_Queue --
   ---------------------

   protected body Protected_Queue is

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

      ----------------------------
      -- Has_Pending_Indication --
      ----------------------------

      function Has_Pending_Indication return Boolean
      is (STQ.Has_Pending_Request (Data.Queue));

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
           Post =>
             Is_Valid (HD)
             and then Valid_Indication (STQ.Request_Reference (Handle).all)
             and then not STQ.Is_Null (Handle)
             and then not STQ.Confirm_Written (Handle);

         procedure Wrapper (HD : in out Holder_Data) is
         begin
            STQ.Try_Get_Next_Request (HD.Queue, Handle);

            HD.Has_Pending_Indication := STQ.Has_Pending_Request (HD.Queue);
         end Wrapper;

      begin
         Wrapper (Data);
      end Get_Next_Indication;

      -----------------------------
      -- Try_Get_Next_Indication --
      -----------------------------

      procedure Try_Get_Next_Indication (Handle : in out STQ.Service_Handle) is

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
                then
                  Valid_Indication (STQ.Request_Reference (Handle).all)
                  and then not STQ.Confirm_Written (Handle));

         procedure Wrapper (HD : in out Holder_Data) is
         begin
            STQ.Try_Get_Next_Request (HD.Queue, Handle);

            HD.Has_Pending_Indication := STQ.Has_Pending_Request (HD.Queue);
         end Wrapper;

      begin
         Wrapper (Data);
      end Try_Get_Next_Indication;

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
   -- Response_Reference --
   ------------------------

   function Response_Reference
     (Handle : Service_Handle) return not null access constant Response_Type
   is (STQ.Confirm_Reference (Handle.Handle));

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

   ------------------------------------------
   -- Move_Indication_Handle_With_Property --
   ------------------------------------------

   procedure Move_Indication_Handle_With_Property
     (Target : in out Indication_Handle; Source : in out Indication_Handle)
   is
      procedure Move_Wrapper is new
        STQ.Move_Request_Handle_With_Property (Property => Property);
   begin
      Move_Wrapper (Target => Target.Handle, Source => Source.Handle);
   end Move_Indication_Handle_With_Property;

   ----------------------------------------
   -- Move_Response_Handle_With_Property --
   ----------------------------------------

   procedure Move_Response_Handle_With_Property
     (Target : in out Response_Handle; Source : in out Response_Handle)
   is
      procedure Move_Wrapper is new
        STQ.Move_Confirm_Handle_With_Property
          (Request_Property => Indication_Property,
           Confirm_Property => Response_Property,
           Pair_Property    => Pair_Property);
   begin
      Move_Wrapper (Target => Target.Handle, Source => Source.Handle);
   end Move_Response_Handle_With_Property;

   ---------------------------------------
   -- Move_Service_Handle_With_Property --
   ---------------------------------------

   procedure Move_Service_Handle_With_Property
     (Target : in out Service_Handle; Source : in out Service_Handle)
   is
      procedure Move_Wrapper is new
        STQ.Move_Service_Handle_With_Property
          (Request_Property => Indication_Property,
           Confirm_Property => Response_Property,
           Pair_Property    => Pair_Property);
   begin
      Move_Wrapper (Target => Target.Handle, Source => Source.Handle);
   end Move_Service_Handle_With_Property;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Handle : in out Response_Handle) is
      procedure Cleanup_Wrapper is new
        STQ.Cleanup
          (Clean         => Clean,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Cleanup_Wrapper (Handle.Handle);
   end Cleanup;

   ----------------------
   -- Abort_Indication --
   ----------------------

   procedure Abort_Indication (Handle : in out Indication_Handle) is
   begin
      STQ.Abort_Request (Handle.Handle);
   end Abort_Indication;

   -------------
   -- Discard --
   -------------

   procedure Discard (Promise : in out Response_Promise) is
   begin
      STQ.Discard (Promise.Handle);
   end Discard;

   -----------------------------
   -- Try_Allocate_Indication --
   -----------------------------

   procedure Try_Allocate_Indication (Handle : in out Indication_Handle) is
   begin
      STQ.Try_Allocate_Request (Handle.Handle);
   end Try_Allocate_Indication;

   ---------------------------
   -- Initialize_Indication --
   ---------------------------

   procedure Initialize_Indication (Handle : in out Indication_Handle) is
      procedure Initialize_Wrapper is new
        STQ.Initialize_Request
          (Initialize    => Initialize,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Initialize_Wrapper (Handle.Handle);
   end Initialize_Indication;

   -----------------------
   -- Update_Indication --
   -----------------------

   procedure Update_Indication (Handle : in out Indication_Handle) is
      procedure Update_Wrapper is new
        STQ.Update_Request
          (Update        => Update,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Update_Wrapper (Handle.Handle);
   end Update_Indication;

   ---------------------
   -- Send_Indication --
   ---------------------

   procedure Send_Indication
     (Handle : in out Indication_Handle; Promise : in out Response_Promise) is
   begin
      Protected_Queue.Send_Indication (Handle.Handle, Promise.Handle);
      Notify_Indication_Pending;
   end Send_Indication;

   ----------------------------
   -- Has_Pending_Indication --
   ----------------------------

   function Has_Pending_Indication return Boolean
   is (Protected_Queue.Has_Pending_Indication);

   -------------------------
   -- Get_Next_Indication --
   -------------------------

   procedure Get_Next_Indication (Handle : in out Service_Handle) is
   begin
      Protected_Queue.Get_Next_Indication (Handle.Handle);
   end Get_Next_Indication;

   -----------------------------
   -- Try_Get_Next_Indication --
   -----------------------------

   procedure Try_Get_Next_Indication (Handle : in out Service_Handle) is
   begin
      Protected_Queue.Try_Get_Next_Indication (Handle.Handle);
   end Try_Get_Next_Indication;

   -------------
   -- Release --
   -------------

   procedure Release (Handle : in out Service_Handle) is
   begin
      STQ.Release (Handle.Handle);
   end Release;

   -------------------
   -- Send_Response --
   -------------------

   procedure Send_Response (Handle : in out Service_Handle) is
      TID : constant Transaction_ID := Get_TID (Handle);
   begin
      STQ.Send_Confirm (Handle.Handle);
      Notify_Response_Pending (Positive (TID));
   end Send_Response;

   -------------------------
   -- Initialize_Response --
   -------------------------

   procedure Initialize_Response (Handle : in out Service_Handle) is
      procedure Initialize_Wrapper is new
        STQ.Initialize_Confirm
          (Initialize    => Initialize,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Initialize_Wrapper (Handle.Handle);
   end Initialize_Response;

   ---------------------
   -- Update_Response --
   ---------------------

   procedure Update_Response (Handle : in out Service_Handle) is
      procedure Update_Wrapper is new
        STQ.Update_Confirm
          (Update        => Update,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Update_Wrapper (Handle.Handle);
   end Update_Response;

   ------------------------
   -- Consume_Indication --
   ------------------------

   procedure Consume_Indication (Handle : in out Service_Handle) is
      procedure Consume_Wrapper is new
        STQ.Consume_Request
          (Consume       => Consume,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Consume_Wrapper (Handle.Handle);
   end Consume_Indication;

   ------------------------------------------------
   -- Consume_Indication_And_Initialize_Response --
   ------------------------------------------------

   procedure Consume_Indication_And_Initialize_Response
     (Handle : in out Service_Handle)
   is
      procedure Initialize_Wrapper is new
        STQ.Consume_Request_And_Initialize_Confirm
          (Initialize    => Initialize,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Initialize_Wrapper (Handle.Handle);
   end Consume_Indication_And_Initialize_Response;

   --------------------------------------------
   -- Consume_Indication_And_Update_Response --
   --------------------------------------------

   procedure Consume_Indication_And_Update_Response
     (Handle : in out Service_Handle)
   is
      procedure Update_Wrapper is new
        STQ.Consume_Request_And_Update_Confirm
          (Update        => Update,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Update_Wrapper (Handle.Handle);
   end Consume_Indication_And_Update_Response;

   -------------
   -- Release --
   -------------

   procedure Release (Handle : in out Response_Handle) is
   begin
      STQ.Release (Handle.Handle);
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
      STQ.Try_Get_Confirm (Handle.Handle, Promise.Handle);
   end Try_Get_Response;

end LibSAP.Synchronous_User_Service_Access_Point;
