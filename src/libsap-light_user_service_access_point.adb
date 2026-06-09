--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Concurrency in SPARK is limited to the Ravenscar or Jorvik profiles, and
--  requires a sequential partition elaboration policy.
--
--  See the SPARK User's Guide (26.1) section 5.10

package body LibSAP.Light_User_Service_Access_Point
  with
    Refined_State =>
      (Transaction_Queue => Queue, Transaction_Pool => STQ.Transaction_Pool)
is

   Queue : STQ.Transaction_Queue_Type;

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

   ----------------------
   -- Response_Written --
   ----------------------

   function Response_Written (Handle : Service_Handle) return Boolean
   is (STQ.Confirm_Written (Handle.Handle));

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
      STQ.Send_Request (Queue, Handle.Handle, Promise.Handle);
   end Send_Indication;

   ----------------------------
   -- Has_Pending_Indication --
   ----------------------------

   function Has_Pending_Indication return Boolean
   is (STQ.Has_Pending_Request (Queue));

   -----------------------------
   -- Try_Get_Next_Indication --
   -----------------------------

   procedure Try_Get_Next_Indication (Handle : in out Service_Handle) is
   begin
      STQ.Try_Get_Next_Request (Queue, Handle.Handle);
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
   begin
      STQ.Send_Confirm (Handle.Handle);
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

end LibSAP.Light_User_Service_Access_Point;
