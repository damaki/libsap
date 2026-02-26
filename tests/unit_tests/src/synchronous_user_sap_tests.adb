--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with System;

with AUnit.Assertions;  use AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites; use AUnit.Test_Suites;

with LibSAP.Synchronous_User_Service_Access_Point;

package body Synchronous_User_SAP_Tests is

   package Test_Caller is new AUnit.Test_Caller (Test);

   ---------------------
   -- Indication_Type --
   ---------------------

   type Indication_Kind is (IND_1, IND_2);

   type IND_1_Type is record
      Value : Integer := 0;
   end record;

   type IND_2_Type is record
      Flag : Boolean := False;
   end record;

   type Indication_Type (Kind : Indication_Kind := IND_1) is record
      case Kind is
         when IND_1 =>
            IND_1 : IND_1_Type;

         when IND_2 =>
            IND_2 : IND_2_Type;
      end case;
   end record;

   ------------------
   -- Response_Type --
   ------------------

   type Response_Kind is (CFM_1);

   type CFM_1_Type is record
      Value : Integer := 0;
   end record;

   type Response_Type (Kind : Response_Kind := CFM_1) is record
      case Kind is
         when CFM_1 =>
            CFM_1 : CFM_1_Type;
      end case;
   end record;

   function Requires_Response (Indication : Indication_Type) return Boolean
   is (Indication.Kind = IND_1);

   function Valid_Response
     (Indication : Indication_Type; Response : Response_Type with Unreferenced)
      return Boolean
   is (Indication.Kind = IND_1);

   ---------------------------------
   -- Test_One_Normal_Transaction --
   ---------------------------------

   --  This test goes through the steps to complete one normal transaction

   package Test_One_Normal_Transaction_SAP is new
     LibSAP.Synchronous_User_Service_Access_Point
       (Indication_Type   => Indication_Type,
        Response_Type     => Response_Type,
        Queue_Capacity    => 3,
        Priority          => System.Priority'Last,
        Requires_Response => Requires_Response,
        Valid_Response    => Valid_Response);

   procedure Test_One_Normal_Transaction (T : in out Test) is
      package SAP renames Test_One_Normal_Transaction_SAP;

      Ind_Handle  : SAP.Indication_Handle;
      Res_Promise : SAP.Response_Promise;
      Res_Handle  : SAP.Response_Handle;
      S_Handle    : SAP.Service_Handle;

   begin
      SAP.Try_Allocate_Indication (Ind_Handle);
      Assert (not SAP.Is_Null (Ind_Handle), "indication allocation failed");

      declare
         procedure Build (Indication : out Indication_Type) is
         begin
            Indication := (Kind => IND_1, IND_1 => (Value => 123));
         end Build;

         procedure Build_Indication is new SAP.Build_Indication (Build);
      begin
         Build_Indication (Ind_Handle);
      end;

      SAP.Send_Indication (Ind_Handle, Res_Promise);
      Assert (not SAP.Is_Null (Res_Promise), "did not get response promise");

      SAP.Try_Get_Next_Indication (S_Handle);
      Assert (not SAP.Is_Null (S_Handle), "failed to get next indication");

      declare
         procedure Build
           (Indication : Indication_Type; Response : out Response_Type) is
         begin
            Assert
              (Indication.Kind = IND_1,
               "got wrong indication kind: " & Indication.Kind'Image);
            Assert
              (Indication.IND_1.Value = 123,
               "got wrong indication value: " & Indication.IND_1.Value'Image);
            Response := (Kind => CFM_1, CFM_1 => (Value => 321));
         end Build;

         procedure Build_Response is new SAP.Build_Response (Build);
      begin
         Build_Response (S_Handle);
      end;

      SAP.Try_Get_Response (Res_Handle, Res_Promise);
      Assert (SAP.Is_Null (Res_Handle), "got response prematurely");

      SAP.Send_Response (S_Handle);

      SAP.Try_Get_Response (Res_Handle, Res_Promise);
      Assert (not SAP.Is_Null (Res_Handle), "failed to get response");

      declare
         Res_Ref : constant not null access constant Response_Type :=
           SAP.Response_Reference (Res_Handle);
      begin
         Assert (Res_Ref.all.CFM_1.Value = 321, "got wrong response value");
      end;

      SAP.Release (Res_Handle);
   end Test_One_Normal_Transaction;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Test_Caller.Create
           ("[Synchronous_User_Service_Access_Point] "
            & "Test_One_Normal_Transaction",
            Test_One_Normal_Transaction'Access));
      return Ret;
   end Suite;

end Synchronous_User_SAP_Tests;
