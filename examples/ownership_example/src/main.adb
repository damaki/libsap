--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Real_Time;
with Ada.Text_IO;

with Service_Provider;

pragma Unreferenced (Service_Provider);

--  This file implements the Service User logic.
--
--  It polls the Service Provider until it receives an indication (.ind)
--  primitive, in which case it prints a message and then sends a response
--  (.res) primitive if required.
--
--  When a DATA.ind primitive is received, it performs a move of the pointer
--  parameter from the DATA.ind primitive to the DATA.res primitive.

procedure Main
with
  SPARK_Mode,
  Global =>
    (Input  => Ada.Real_Time.Clock_Time,
     In_Out =>
       (Ada.Text_IO.File_System, Service_Provider.SAP.Transaction_Queue))
is
   use all type Service_Provider.Indication_Kind;
   use all type Service_Provider.Response_Kind;

   package SP_SAP renames Service_Provider.SAP;

   procedure Send_DATA_Res (Handle : in out SP_SAP.Service_Handle)
   with
     Pre  =>
       not SP_SAP.Is_Null (Handle)
       and then SP_SAP.Indication_Kind (Handle) = DATA_Ind
       and then not SP_SAP.Indication_Consumed (Handle)
       and then not SP_SAP.Response_Written (Handle)
       and then
         Service_Provider.Valid_Indication
           (SP_SAP.Indication_Reference (Handle).all),
     Post => SP_SAP.Is_Null (Handle);

   -------------------
   -- Send_DATA_Res --
   -------------------

   procedure Send_DATA_Res (Handle : in out SP_SAP.Service_Handle) is

      function Precondition
        (Indication : Service_Provider.Indication_Type) return Boolean
      is (Indication.Kind = DATA_Ind
          and then Service_Provider.Valid_Indication (Indication));

      procedure Build_DATA_Res
        (Indication : in out Service_Provider.Indication_Type;
         Response   : out Service_Provider.Response_Type)
      with
        Global => null,
        Pre    => Precondition (Indication),
        Post   => Service_Provider.Valid_Response (Indication, Response);

      --------------------
      -- Build_DATA_Res --
      --------------------

      procedure Build_DATA_Res
        (Indication : in out Service_Provider.Indication_Type;
         Response   : out Service_Provider.Response_Type) is
      begin
         --  In this example, we just move the pointer from the Indication
         --  into the Response.

         Response :=
           (Kind => DATA_Res, DATA_Res => (Data => Indication.DATA_Ind.Data));
         Indication.DATA_Ind.Data := null;
      end Build_DATA_Res;

      procedure Build_Response is new
        SP_SAP.Consume_Indication_And_Build_Response
          (Build         => Build_DATA_Res,
           Precondition  => Precondition,
           Postcondition => Service_Provider.Valid_Response);

   begin
      Build_Response (Handle);
      SP_SAP.Send_Response (Handle);
   end Send_DATA_Res;

   Handle   : SP_SAP.Service_Handle;
   Ind_Kind : Service_Provider.Indication_Kind;

begin

   loop
      pragma Loop_Invariant (SP_SAP.Is_Null (Handle));

      --  Keep polling until we get a request.

      loop
         pragma Loop_Invariant (SP_SAP.Is_Null (Handle));
         SP_SAP.Try_Get_Next_Indication (Handle);
         exit when not SP_SAP.Is_Null (Handle);
         delay 0.1;
      end loop;

      --  Process the request and send a response if necessary.

      Ind_Kind := SP_SAP.Indication_Reference (Handle).all.Kind;

      case Ind_Kind is
         when START_Ind =>
            Ada.Text_IO.Put_Line ("[Service User] Received START.ind");
            SP_SAP.Release (Handle);

         when DATA_Ind  =>
            Ada.Text_IO.Put_Line ("[Service User] Received DATA.ind");
            Ada.Text_IO.Put_Line ("[Service User] Sending DATA.res");
            Send_DATA_Res (Handle);
      end case;

   end loop;

end Main;
