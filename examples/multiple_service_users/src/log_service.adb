--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Log_Service
  with SPARK_Mode
is

   -----------------
   -- Log_Message --
   -----------------

   procedure Log_Message (Message : String) is

      function Always_True return Boolean
      is (True);

      function Message_Length_In_Range return Boolean
      is (Message'Length <= Message_Length'Last)
      with Global => (Input => Message);

      procedure Write_Message (Request : out LOG_Req_Type)
      with
        Global => (Input => Message),
        Pre    =>
          not Request'Constrained
          and then Message'Length <= Message_Length'Last;

      -------------------
      -- Write_Message --
      -------------------

      procedure Write_Message (Request : out LOG_Req_Type) is
      begin
         Request := (Length => Message'Length, Message => Message);
      end Write_Message;

      -------------------
      -- Build_Request --
      -------------------

      procedure Build_Request is new
        SAP.Build_Contextual_Request_No_Confirm
          (Build         => Write_Message,
           Precondition  => Message_Length_In_Range,
           Postcondition => Always_True);

      Handle : SAP.Request_Handle;
      Cfm_Promise : SAP.Confirm_Promise;

   begin
      loop
         pragma Loop_Invariant (SAP.Is_Null (Handle));

         SAP.Try_Allocate_Request (Handle);
         exit when not SAP.Is_Null (Handle);
      end loop;

      Build_Request (Handle);
      SAP.Send_Request (Handle, Cfm_Promise);

      pragma Unreferenced (Handle);
      pragma Unreferenced (Cfm_Promise);
   end Log_Message;

   --------------
   -- Log_Task --
   --------------

   task body Log_Task is
      Handle : SAP.Service_Handle;
   begin
      loop
         pragma Loop_Invariant (SAP.Is_Null (Handle));

         SAP.Get_Next_Request (Handle);
         Ada.Text_IO.Put_Line (SAP.Request_Reference (Handle).all.Message);
         SAP.Request_Completed (Handle);
      end loop;
   end Log_Task;

end Log_Service;
