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
          and then STQ.Request_Ready (Handle),
        Post           => STQ.Is_Null (Handle),
        Contract_Cases =>
          (STQ.Requires_Confirm (Handle) => not STQ.Is_Null (Promise),
           others                        => STQ.Is_Null (Promise));

      function Has_Pending_Request return Boolean;

      entry Get_Next_Request (Handle : in out STQ.Service_Handle)
      with Pre => STQ.Is_Null (Handle), Post => not STQ.Is_Null (Handle);

      procedure Try_Get_Next_Request (Handle : in out STQ.Service_Handle)
      with Pre => STQ.Is_Null (Handle);

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
           Post => Is_Valid (HD) and then not STQ.Is_Null (Handle);

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
           Post => Is_Valid (HD);

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
   -- Has_Valid_Confirm --
   -----------------------

   function Has_Valid_Confirm (Handle : Service_Handle) return Boolean
   is (STQ.Has_Valid_Confirm (Handle.Handle));

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
      procedure Build_Wrapper is new STQ.Build_Request (Build);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Request;

   ------------------------------
   -- Build_Request_No_Confirm --
   ------------------------------

   procedure Build_Request_No_Confirm (Handle : in out Request_Handle) is
      procedure Build_Wrapper is new STQ.Build_Request_No_Confirm (Build);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Request_No_Confirm;

   --------------------------------
   -- Build_Request_With_Confirm --
   --------------------------------

   procedure Build_Request_With_Confirm (Handle : in out Request_Handle) is
      procedure Build_Wrapper is new STQ.Build_Request_With_Confirm (Build);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Request_With_Confirm;

   ------------------------------
   -- Build_Contextual_Request --
   ------------------------------

   procedure Build_Contextual_Request (Handle : in out Request_Handle) is
      procedure Build_Wrapper is new
        STQ.Build_Contextual_Request
          (Build         => Build,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Contextual_Request;

   -----------------------------------------
   -- Build_Contextual_Request_No_Confirm --
   -----------------------------------------

   procedure Build_Contextual_Request_No_Confirm
     (Handle : in out Request_Handle)
   is
      procedure Build_Wrapper is new
        STQ.Build_Contextual_Request_No_Confirm
          (Build         => Build,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Contextual_Request_No_Confirm;

   -------------------------------------------
   -- Build_Contextual_Request_With_Confirm --
   -------------------------------------------

   procedure Build_Contextual_Request_With_Confirm
     (Handle : in out Request_Handle)
   is
      procedure Build_Wrapper is new
        STQ.Build_Contextual_Request_With_Confirm
          (Build         => Build,
           Precondition  => Precondition,
           Postcondition => Postcondition);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Contextual_Request_With_Confirm;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Handle : in out Request_Handle; Promise : in out Confirm_Promise) is
   begin
      Protected_Queue.Send_Request (Handle.Handle, Promise.Handle);
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

   -----------------------
   -- Request_Completed --
   -----------------------

   procedure Request_Completed (Handle : in out Service_Handle) is
   begin
      STQ.Request_Completed (Handle.Handle);
   end Request_Completed;

   ------------------
   -- Send_Confirm --
   ------------------

   procedure Send_Confirm (Handle : in out Service_Handle) is
   begin
      STQ.Send_Confirm (Handle.Handle);
   end Send_Confirm;

   ---------------------
   -- Process_Request --
   ---------------------

   procedure Process_Request (Handle : in out Service_Handle) is

      procedure Process_Request_With_Confirm_Wrapper is new
        STQ.Build_Confirm (Process_Request_With_Confirm);

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
      procedure Build_Wrapper is new STQ.Build_Confirm (Build);
   begin
      Build_Wrapper (Handle.Handle);
   end Build_Confirm;

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
