--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;

with Service_Provider;

pragma Unreferenced (Service_Provider);

procedure Main
with
  SPARK_Mode,
  Global =>
    (In_Out =>
       (Ada.Text_IO.File_System, Service_Provider.SAP.Transaction_Queue))
is
   use all type Service_Provider.Request_Kind;

   Cfm_Promise : Service_Provider.SAP.Confirm_Promise;
begin

   ----------------------
   -- Sending ECHO.req --
   ----------------------

   Ada.Text_IO.Put_Line ("[Service User] Sending ECHO.req");

   --  Build and send ECHO.req to the Service Provider.
   --
   --  LibSAP does not expose the request object to be written to directly,
   --  but instead calls Build_ECHO_Request and passes the request object as
   --  an "out" parameter so we can write to it.
   --
   --  By using a nested procedure, Build_ECHO_Request can access other
   --  entities outside its scope, such as the constant Value_To_Echo in this
   --  example.

   declare
      Value_To_Echo : constant Natural := 123;

      procedure Build_ECHO_Request
        (Request : out Service_Provider.Request_Type)
      with
        Global => null,
        Pre    => not Request'Constrained,
        Post   => Service_Provider.Requires_Confirm (Request)
      is
      begin
         Request :=
           (Kind => ECHO_Req, ECHO_Req => (Value_To_Echo => Value_To_Echo));
      end Build_ECHO_Request;

      procedure Build_Request is new
        Service_Provider.SAP.Build_Request_With_Confirm (Build_ECHO_Request);

      Req_Handle : Service_Provider.SAP.Request_Handle;

   begin
      Service_Provider.SAP.Try_Allocate_Request (Req_Handle);

      if Service_Provider.SAP.Is_Null (Req_Handle) then
         Ada.Text_IO.Put_Line ("[Service User] Allocation failed");
         return;
      end if;

      Build_Request (Req_Handle);
      Service_Provider.SAP.Send_Request (Req_Handle, Cfm_Promise);

      pragma Unreferenced (Req_Handle);
   end;

   ---------------------------------
   -- Discarding the Confirmation --
   ---------------------------------

   --  At this point we have a Cfm_Promise that we could use to eventually
   --  get the confirmation to our request in the future.
   --
   --  However, if we decide that we no longer need the confirmation,
   --  then we can explicitly discard the promise to avoid needing to keep it
   --  around.
   --
   --  Note that this does NOT stop the Service Provider from receiving
   --  the request and sending a confirmation (and it might have already done
   --  so before Discard is even called).

   Service_Provider.SAP.Discard (Cfm_Promise);

   pragma Unreferenced (Cfm_Promise);

   Ada.Text_IO.Put_Line ("[Service User] Discarded confirm promise");

end Main;
