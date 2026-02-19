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

package body LibSAP.Light_Provider_Service_Access_Point
  with
    Refined_State =>
      (Transaction_Queue => Queue, Transaction_Pool => STQ.Transaction_Pool)
is

   Queue : STQ.Transaction_Queue_Type;

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
      STQ.Send_Request (Queue, Handle.Handle, Promise.Handle);
   end Send_Request;

   --------------------------
   -- Try_Get_Next_Request --
   --------------------------

   procedure Try_Get_Next_Request (Handle : in out Service_Handle) is
   begin
      STQ.Try_Get_Next_Request (Queue, Handle.Handle);
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

end LibSAP.Light_Provider_Service_Access_Point;
