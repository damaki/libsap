--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Concurrency in SPARK is limited to the Ravenscar or Jorvik profiles, and
--  requires a sequential partition elaboration policy.
--
--  See the SPARK User's Guide (26.1) section 5.10

package body LibSAP.Synchronous_Provider_Service_Access_Point
  with
    Refined_State =>
      (Transaction_Queue => (Protected_Queue, STQ.Transaction_Pool))
is

   type Holder_Data is limited record
      Queue               : STQ.Transaction_Queue_Type;
      Has_Pending_Request : Boolean := False;
   end record;

   function Is_Valid (HD : Holder_Data) return Boolean
   is (if HD.Has_Pending_Request then STQ.Has_Pending_Request (HD.Queue));

   subtype Valid_Holder_Data is Holder_Data
   with Predicate => Is_Valid (Valid_Holder_Data);

   protected Protected_Queue
     with Priority => Priority
   is

      procedure Send_Request
        (Handle  : in out STQ.Request_Handle;
         Promise : in out STQ.Confirm_Promise)
      with
        Pre            =>
          not STQ.Is_Null (Handle)
          and then STQ.Is_Null (Promise)
          and then STQ.Request_Written (Handle),
        Post           => STQ.Is_Null (Handle),
        Contract_Cases =>
          (STQ.Requires_Confirm (Handle) =>
             not STQ.Is_Null (Promise)
             and STQ.Get_TID (Promise) = STQ.Get_TID (Handle)'Old
             and STQ.Request_Kind (Promise) = STQ.Request_Kind (Handle)'Old,
           others                        => STQ.Is_Null (Promise));

      function Has_Pending_Request return Boolean;

      entry Get_Next_Request (Handle : in out STQ.Service_Handle)
      with
        Pre  => STQ.Is_Null (Handle),
        Post =>
          not STQ.Is_Null (Handle)
          and then Valid_Request (STQ.Request_Reference (Handle).all)
          and then not STQ.Confirm_Written (Handle)
          and then not STQ.Request_Consumed (Handle);

      procedure Try_Get_Next_Request (Handle : in out STQ.Service_Handle)
      with
        Pre  => STQ.Is_Null (Handle),
        Post =>
          (if not STQ.Is_Null (Handle)
           then
             Valid_Request (STQ.Request_Reference (Handle).all)
             and then not STQ.Confirm_Written (Handle)
             and then not STQ.Request_Consumed (Handle));

   private

      Data : Valid_Holder_Data := (others => <>);

   end Protected_Queue;

   ---------------------
   -- Protected_Queue --
   ---------------------

   protected body Protected_Queue is

      ------------------
      -- Send_Request --
      ------------------

      procedure Send_Request
        (Handle  : in out STQ.Request_Handle;
         Promise : in out STQ.Confirm_Promise) is
      begin
         STQ.Send_Request (Data.Queue, Handle, Promise);

         Data.Has_Pending_Request := True;
      end Send_Request;

      -------------------------
      -- Has_Pending_Request --
      -------------------------

      function Has_Pending_Request return Boolean
      is (STQ.Has_Pending_Request (Data.Queue));

      ----------------------
      -- Get_Next_Request --
      ----------------------

      entry Get_Next_Request (Handle : in out STQ.Service_Handle)
        when Data.Has_Pending_Request
      is

         --  Operate on type Holder_Data instead of Valid_Holder_Data to
         --  allow the type predicate to be (temporarily) violated.

         procedure Wrapper (HD : in out Holder_Data)
         with
           Inline,
           Pre  =>
             Is_Valid (HD)
             and then STQ.Is_Null (Handle)
             and then HD.Has_Pending_Request,
           Post =>
             Is_Valid (HD)
             and then Valid_Request (STQ.Request_Reference (Handle).all)
             and then not STQ.Is_Null (Handle)
             and then not STQ.Confirm_Written (Handle)
             and then not STQ.Request_Consumed (Handle);

         procedure Wrapper (HD : in out Holder_Data) is
         begin
            STQ.Try_Get_Next_Request (HD.Queue, Handle);

            HD.Has_Pending_Request := STQ.Has_Pending_Request (HD.Queue);
         end Wrapper;

      begin
         Wrapper (Data);
      end Get_Next_Request;

      --------------------------
      -- Try_Get_Next_Request --
      --------------------------

      procedure Try_Get_Next_Request (Handle : in out STQ.Service_Handle) is

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
                  Valid_Request (STQ.Request_Reference (Handle).all)
                  and then not STQ.Confirm_Written (Handle)
                  and then not STQ.Request_Consumed (Handle));

         procedure Wrapper (HD : in out Holder_Data) is
         begin
            STQ.Try_Get_Next_Request (HD.Queue, Handle);

            HD.Has_Pending_Request := STQ.Has_Pending_Request (HD.Queue);
         end Wrapper;

      begin
         Wrapper (Data);
      end Try_Get_Next_Request;

   end Protected_Queue;

   --=========================--
   -- Public Operation Bodies --
   --=========================--

   -----------------------
   -- Confirm_Reference --
   -----------------------

   function Confirm_Reference
     (Handle : Confirm_Handle) return not null access constant Confirm_Type
   is (STQ.Confirm_Reference (Handle.Handle));

   -----------------------
   -- Request_Reference --
   -----------------------

   function Request_Reference
     (Handle : Request_Handle) return not null access constant Request_Type
   is (STQ.Request_Reference (Handle.Handle));

   function Request_Reference
     (Handle : Service_Handle) return not null access constant Request_Type
   is (STQ.Request_Reference (Handle.Handle));

   function Request_Reference
     (Handle : Confirm_Handle) return not null access constant Request_Type
   is (STQ.Request_Reference (Handle.Handle));

   ----------------------
   -- Requires_Confirm --
   ----------------------

   function Requires_Confirm (Handle : Request_Handle) return Boolean
   is (STQ.Requires_Confirm (Handle.Handle));

   function Requires_Confirm (Handle : Service_Handle) return Boolean
   is (STQ.Requires_Confirm (Handle.Handle));

   -----------------------
   -- Confirm_Reference --
   -----------------------

   function Confirm_Reference
     (Handle : Service_Handle) return not null access constant Confirm_Type
   is (STQ.Confirm_Reference (Handle.Handle));

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : in out Request_Handle; Source : in out Request_Handle) is
   begin
      STQ.Move (Target => Target.Handle, Source => Source.Handle);
   end Move;

   procedure Move
     (Target : in out Confirm_Promise; Source : in out Confirm_Promise) is
   begin
      STQ.Move (Target => Target.Handle, Source => Source.Handle);
   end Move;

   procedure Move
     (Target : in out Confirm_Handle; Source : in out Confirm_Handle) is
   begin
      STQ.Move (Target => Target.Handle, Source => Source.Handle);
   end Move;

   procedure Move
     (Target : in out Service_Handle; Source : in out Service_Handle) is
   begin
      STQ.Move (Target => Target.Handle, Source => Source.Handle);
   end Move;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Handle : in out Confirm_Handle) is
      procedure Cleanup_Wrapper is new
        STQ.Cleanup
          (Clean         => Clean,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Cleanup_Wrapper (Handle.Handle);
   end Cleanup;

   -------------------
   -- Abort_Request --
   -------------------

   procedure Abort_Request (Handle : in out Request_Handle) is
   begin
      STQ.Abort_Request (Handle.Handle);
   end Abort_Request;

   -------------
   -- Discard --
   -------------

   procedure Discard (Promise : in out Confirm_Promise) is
   begin
      STQ.Discard (Promise.Handle);
   end Discard;

   --------------------------
   -- Try_Allocate_Request --
   --------------------------

   procedure Try_Allocate_Request (Handle : in out Request_Handle) is
   begin
      STQ.Try_Allocate_Request (Handle.Handle);
   end Try_Allocate_Request;

   -------------------
   -- Build_Request --
   -------------------

   procedure Build_Request (Handle : in out Request_Handle) is
      procedure Build_Wrapper is new
        STQ.Build_Request
          (Build         => Build,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Request;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Handle : in out Request_Handle; Promise : in out Confirm_Promise) is
   begin
      Protected_Queue.Send_Request (Handle.Handle, Promise.Handle);
      Notify_Request_Pending;
   end Send_Request;

   -------------------------
   -- Has_Pending_Request --
   -------------------------

   function Has_Pending_Request return Boolean
   is (Protected_Queue.Has_Pending_Request);

   ----------------------
   -- Get_Next_Request --
   ----------------------

   procedure Get_Next_Request (Handle : in out Service_Handle) is
   begin
      Protected_Queue.Get_Next_Request (Handle.Handle);
   end Get_Next_Request;

   --------------------------
   -- Try_Get_Next_Request --
   --------------------------

   procedure Try_Get_Next_Request (Handle : in out Service_Handle) is
   begin
      Protected_Queue.Try_Get_Next_Request (Handle.Handle);
   end Try_Get_Next_Request;

   -------------
   -- Release --
   -------------

   procedure Release (Handle : in out Service_Handle) is
   begin
      STQ.Release (Handle.Handle);
   end Release;

   ------------------
   -- Send_Confirm --
   ------------------

   procedure Send_Confirm (Handle : in out Service_Handle) is
      TID : constant Transaction_ID := Get_TID (Handle);
   begin
      STQ.Send_Confirm (Handle.Handle);
      Notify_Confirm_Pending (Positive (TID));
   end Send_Confirm;

   ---------------------
   -- Process_Request --
   ---------------------

   procedure Process_Request (Handle : in out Service_Handle) is

      procedure Process_Request_With_Confirm_Wrapper is new
        STQ.Build_Confirm
          (Build         => Process_Request_With_Confirm,
           Precondition  => Precondition,
           Postcondition => Postcondition);

      Needs_Confirm : Boolean;

   begin
      declare
         Request : constant not null access constant Request_Type :=
           STQ.Request_Reference (Handle.Handle);
      begin
         Needs_Confirm := Requires_Confirm (Request.all);

         if not Needs_Confirm then
            Process_Request_No_Confirm (Request.all);
         end if;
      end;

      if Needs_Confirm then
         Process_Request_With_Confirm_Wrapper (Handle.Handle);
      end if;
   end Process_Request;

   -------------------
   -- Build_Confirm --
   -------------------

   procedure Build_Confirm (Handle : in out Service_Handle) is
      procedure Build_Wrapper is new
        STQ.Build_Confirm
          (Build         => Build,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Confirm;

   ---------------------
   -- Consume_Request --
   ---------------------

   procedure Consume_Request (Handle : in out Service_Handle) is
      procedure Consume_Wrapper is new
        STQ.Consume_Request
          (Consume       => Consume,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Consume_Wrapper (Handle.Handle);
   end Consume_Request;

   ---------------------------------------
   -- Consume_Request_And_Build_Confirm --
   ---------------------------------------

   procedure Consume_Request_And_Build_Confirm (Handle : in out Service_Handle)
   is
      procedure Build_Wrapper is new
        STQ.Consume_Request_And_Build_Confirm
          (Build         => Build,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Build_Wrapper (Handle.Handle);
   end Consume_Request_And_Build_Confirm;

   -------------
   -- Release --
   -------------

   procedure Release (Handle : in out Confirm_Handle) is
   begin
      STQ.Release (Handle.Handle);
   end Release;

   -----------------
   -- New_Request --
   -----------------

   procedure New_Request
     (Cfm_Handle : in out Confirm_Handle; Req_Handle : in out Request_Handle)
   is
   begin
      STQ.New_Request (Cfm_Handle.Handle, Req_Handle.Handle);
   end New_Request;

   ---------------------
   -- Try_Get_Confirm --
   ---------------------

   procedure Try_Get_Confirm
     (Handle : in out Confirm_Handle; Promise : in out Confirm_Promise) is
   begin
      STQ.Try_Get_Confirm (Handle.Handle, Promise.Handle);
   end Try_Get_Confirm;

end LibSAP.Synchronous_Provider_Service_Access_Point;
