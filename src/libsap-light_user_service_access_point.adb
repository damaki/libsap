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

package body LibSAP.Light_User_Service_Access_Point
  with
    Refined_State =>
      (Transaction_Queue => Queue, Queue_Memory => STQ.Single_Instance)
is

   Queue : STQ.Valid_Queue_Type;

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

   ----------------------
   -- Abort_Indication --
   ----------------------

   procedure Abort_Indication (Handle : in out Indication_Handle) is
   begin
      STQ.Abort_Request (Queue, Handle.Handle);
   end Abort_Indication;

   -----------------------------
   -- Try_Allocate_Indication --
   -----------------------------

   procedure Try_Allocate_Indication (Handle : in out Indication_Handle) is
   begin
      STQ.Try_Allocate_Request (Queue, Handle.Handle);
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
      STQ.Send_Request (Queue, Handle.Handle, Promise.Handle);
   end Send_Indication;

   -----------------------------
   -- Try_Get_Next_Indication --
   -----------------------------

   procedure Try_Get_Next_Indication (Handle : in out Service_Handle) is
   begin
      STQ.Try_Get_Next_Request (Queue, Handle.Handle);
   end Try_Get_Next_Indication;

   --------------------------
   -- Indication_Completed --
   --------------------------

   procedure Indication_Completed (Handle : in out Service_Handle) is
   begin
      STQ.Request_Completed (Queue, Handle.Handle);
   end Indication_Completed;

   -------------------
   -- Send_Response --
   -------------------

   procedure Send_Response (Handle : in out Service_Handle) is
   begin
      STQ.Send_Confirm (Queue, Handle.Handle);
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
      STQ.Release (Queue, Handle.Handle);
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
      STQ.Try_Get_Confirm (Queue, Handle.Handle, Promise.Handle);
   end Try_Get_Response;

begin
   STQ.Claim_Single_Instance (Queue);
end LibSAP.Light_User_Service_Access_Point;
