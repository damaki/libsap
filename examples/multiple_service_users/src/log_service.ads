--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Profile (Jorvik);
pragma Partition_Elaboration_Policy (Sequential);

with Ada.Text_IO;
with System;
with LibSAP.Synchronous_Provider_Service_Access_Point;

--  This is a simple service that handles printing messages to Ada.Text_IO.
--
--  This protects Ada.Text_IO from data races when multiple tasks want to
--  log messages concurrently.

package Log_Service
  with SPARK_Mode
is

   --  This service only provides one request (LOG.req), so there's no need to
   --  use a discriminated record.

   subtype Message_Length is Natural range 0 .. 128;

   type LOG_Req_Type (Length : Message_Length := 0) is record
      Message : String (1 .. Length) := [others => ' '];
   end record;

   --  No requests require a confirmation

   type Confirm_Type is record
      Dummy : Boolean := False; --  Needed to ensure default initialization
   end record;

   function Requires_Confirm
     (Request : LOG_Req_Type with Unreferenced) return Boolean
   is (False);

   function Valid_Confirm
     (Request : LOG_Req_Type with Unreferenced;
      Confirm : Confirm_Type with Unreferenced) return Boolean
   is (False);

   package SAP is new
     LibSAP.Synchronous_Provider_Service_Access_Point
       (Request_Type     => LOG_Req_Type,
        Confirm_Type     => Confirm_Type,
        Requires_Confirm => Requires_Confirm,
        Valid_Confirm    => Valid_Confirm,
        Priority         => System.Priority'Last,
        Queue_Capacity   => 20);

   procedure Log_Message (Message : String)
   with
     Global => (In_Out => SAP.Transaction_Queue),
     Pre    => Message'Length <= Message_Length'Last;

   task Log_Task
     with
       Global => (In_Out => (SAP.Transaction_Queue, Ada.Text_IO.File_System));

end Log_Service;
