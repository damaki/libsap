--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Service_Users is

   task body Service_User is
      use all type Service_Provider.Request_Kind;

      Req_Handle  : Service_Provider.SAP.Request_Handle;
      Cfm_Promise : Service_Provider.SAP.Confirm_Promise;
      Cfm_Handle  : Service_Provider.SAP.Confirm_Handle;
   begin

      ----------------------
      -- Sending ECHO.req --
      ----------------------

      Log_Service.Log_Message
        ("[Service User" & SUID'Image & "] Sending ECHO.req");

      --  Build and send ECHO.req to the Service Provider.
      --
      --  LibSAP does not expose the request object to be written to directly,
      --  but instead calls Build_ECHO_Request and passes the request object as
      --  an "out" parameter so we can write to it.
      --
      --  By using a nested procedure, Build_ECHO_Request can access other
      --  entities outside its scope, such as the constant Value_To_Echo in
      --  this example.

      declare
         Value_To_Echo : constant Natural := Natural (SUID);

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
           Service_Provider.SAP.Build_Request_With_Confirm
             (Build_ECHO_Request);

      begin
         Service_Provider.SAP.Allocate_Request (Req_Handle);

         Build_Request (Req_Handle);

         Service_Provider.Confirm_Barriers (SUID).Add_To_Filter
           (Service_Provider.SAP.Get_TID (Req_Handle));

         Service_Provider.SAP.Send_Request (Req_Handle, Cfm_Promise);
      end;

      pragma Unreferenced (Req_Handle);

      ----------------------------------------
      --  Wait for a reply from the Service --
      ----------------------------------------

      Service_Provider.Confirm_Barriers (SUID).Wait_For_Any_Confirm;

      loop
         pragma Loop_Invariant (Service_Provider.SAP.Is_Null (Cfm_Handle));

         pragma
           Loop_Invariant (not Service_Provider.SAP.Is_Null (Cfm_Promise));

         Service_Provider.SAP.Try_Get_Confirm (Cfm_Handle, Cfm_Promise);

         exit when not Service_Provider.SAP.Is_Null (Cfm_Handle);
      end loop;

      Service_Provider.Confirm_Barriers (SUID).Remove_From_Filter
        (Service_Provider.SAP.Get_TID (Cfm_Handle));

      --------------------------
      -- Process the ECHO.cfm --
      --------------------------

      --  LibSAP ensures that the Service sends the correct kind of confirm
      --  based on the request (in this case, that ECHO.cfm is sent in response
      --  to an ECHO.req), and this is proved in SPARK.
      --
      --  LibSAP also ensures that the request in the Confirm_Handle is the
      --  same as the original request, but this is not currently provable in
      --  SPARK so we use an assumption.

      pragma
        Assume
          (Service_Provider.SAP.Request_Reference (Cfm_Handle).all.Kind
           = ECHO_Req);

      declare
         Confirm :
           constant not null access constant Service_Provider.Confirm_Type :=
             Service_Provider.SAP.Confirm_Reference (Cfm_Handle);
      begin
         Log_Service.Log_Message
           ("[Service User"
            & SUID'Image
            & "] Got ECHO.cfm with Value ="
            & Confirm.all.ECHO_Cfm.Value'Image);
      end;

      --  The transaction is now complete. Release the handle to relinquish
      --  its resources.

      Service_Provider.SAP.Release (Cfm_Handle);

      pragma Unreferenced (Cfm_Handle);
   end Service_User;

end Service_Users;
