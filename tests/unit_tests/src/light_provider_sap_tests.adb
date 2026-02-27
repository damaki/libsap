--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with AUnit.Assertions;  use AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites; use AUnit.Test_Suites;

with LibSAP.Light_Provider_Service_Access_Point;

package body Light_Provider_SAP_Tests is

   package Test_Caller is new AUnit.Test_Caller (Test);

   ------------------
   -- Request_Type --
   ------------------

   type Request_Kind is (REQ_1, REQ_2);

   type REQ_1_Type is record
      Value : Integer := 0;
   end record;

   type REQ_2_Type is record
      Flag : Boolean := False;
   end record;

   type Request_Type (Kind : Request_Kind := REQ_1) is record
      case Kind is
         when REQ_1 =>
            REQ_1 : REQ_1_Type;

         when REQ_2 =>
            REQ_2 : REQ_2_Type;
      end case;
   end record;

   ------------------
   -- Confirm_Type --
   ------------------

   type Confirm_Kind is (CFM_1);

   type CFM_1_Type is record
      Value : Integer := 0;
   end record;

   type Confirm_Type (Kind : Confirm_Kind := CFM_1) is record
      case Kind is
         when CFM_1 =>
            CFM_1 : CFM_1_Type;
      end case;
   end record;

   function Requires_Confirm (Request : Request_Type) return Boolean
   is (Request.Kind = REQ_1);

   function Valid_Confirm
     (Request : Request_Type; Confirm : Confirm_Type with Unreferenced)
      return Boolean
   is (Request.Kind = REQ_1);

   ---------------------------------
   -- Test_One_Normal_Transaction --
   ---------------------------------

   --  This test goes through the steps to complete one normal transaction

   package Test_One_Normal_Transaction_SAP is new
     LibSAP.Light_Provider_Service_Access_Point
       (Request_Type     => Request_Type,
        Confirm_Type     => Confirm_Type,
        Queue_Capacity   => 3,
        Requires_Confirm => Requires_Confirm,
        Valid_Confirm    => Valid_Confirm);

   procedure Test_One_Normal_Transaction (T : in out Test) is
      package SAP renames Test_One_Normal_Transaction_SAP;

      Req_Handle  : SAP.Request_Handle;
      Cfm_Promise : SAP.Confirm_Promise;
      Cfm_Handle  : SAP.Confirm_Handle;
      S_Handle    : SAP.Service_Handle;

   begin
      SAP.Try_Allocate_Request (Req_Handle);
      Assert (not SAP.Is_Null (Req_Handle), "request allocation failed");

      declare
         procedure Build (Request : out Request_Type) is
         begin
            Request := (Kind => REQ_1, REQ_1 => (Value => 123));
         end Build;

         procedure Build_Request is new SAP.Build_Request (Build);
      begin
         Build_Request (Req_Handle);
      end;

      SAP.Send_Request (Req_Handle, Cfm_Promise);
      Assert (not SAP.Is_Null (Cfm_Promise), "did not get confirm promise");

      SAP.Try_Get_Next_Request (S_Handle);
      Assert (not SAP.Is_Null (S_Handle), "failed to get next request");

      declare
         procedure Build (Request : Request_Type; Confirm : out Confirm_Type)
         is
         begin
            Assert
              (Request.Kind = REQ_1,
               "got wrong request kind: " & Request.Kind'Image);
            Assert
              (Request.REQ_1.Value = 123,
               "got wrong request value: " & Request.REQ_1.Value'Image);
            Confirm := (Kind => CFM_1, CFM_1 => (Value => 321));
         end Build;

         procedure Build_Confirm is new SAP.Build_Confirm (Build);
      begin
         Build_Confirm (S_Handle);
      end;

      SAP.Try_Get_Confirm (Cfm_Handle, Cfm_Promise);
      Assert (SAP.Is_Null (Cfm_Handle), "got confirm prematurely");

      SAP.Send_Confirm (S_Handle);

      SAP.Try_Get_Confirm (Cfm_Handle, Cfm_Promise);
      Assert (not SAP.Is_Null (Cfm_Handle), "failed to get confirm");

      declare
         Cfm_Ref : constant not null access constant Confirm_Type :=
           SAP.Confirm_Reference (Cfm_Handle);
      begin
         Assert (Cfm_Ref.all.CFM_1.Value = 321, "got wrong confirm value");
      end;

      SAP.Release (Cfm_Handle);
   end Test_One_Normal_Transaction;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Test_Caller.Create
           ("[Light_Provider_Service_Access_Point] "
            & "Test_One_Normal_Transaction",
            Test_One_Normal_Transaction'Access));
      return Ret;
   end Suite;

end Light_Provider_SAP_Tests;
