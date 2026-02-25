--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with AUnit.Assertions;  use AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites; use AUnit.Test_Suites;

with LibSAP.Pointer_Holders;

package body LibSAP.Pointer_Holder_Tests is

   package Test_Caller is new AUnit.Test_Caller (Test);

   type Element_ID is range 1 .. 5;
   type Element_Type (ID : Element_ID := 1) is null record;
   type Element_Access is access all Element_Type;

   Elements : array (Element_ID) of aliased Element_Type :=
     [for I in Element_ID => Element_Type'(ID => I)];

   ------------------------
   -- Test_Initial_State --
   ------------------------

   procedure Test_Initial_State (T : in out Test) is
      pragma Unreferenced (T);

      package Instance is new
        LibSAP.Pointer_Holders (Element_ID, Element_Type, Element_Access);

      Is_Null : Boolean;
      Ptr     : Element_Access;

   begin
      for I in Element_ID loop
         Instance.Check_Is_Null (I, Is_Null);
         Assert (Is_Null, "Check_Is_Null is False for element" & I'Image);
      end loop;

      for I in Element_ID loop
         Instance.Retrieve (I, Ptr);
         Assert (Ptr = null, "Retrieve /= null for element" & I'Image);
      end loop;
   end Test_Initial_State;

   -------------------
   -- Test_Exchange --
   -------------------

   procedure Test_Exchange (T : in out Test) is
      pragma Unreferenced (T);

      package Instance is new
        LibSAP.Pointer_Holders (Element_ID, Element_Type, Element_Access);

      Ptr_1   : Element_Access := Elements (1)'Access;
      Is_Null : Boolean;

   begin
      Instance.Exchange (Ptr_1);
      Assert (Ptr_1 = null, "Ptr_1 is not null after Exchange");

      --  Check_Is_Null should output False for element 1

      Instance.Check_Is_Null (Elements (1).ID, Is_Null);
      Assert
        (not Is_Null,
         "Check_Is_Null is True for element" & Elements (1).ID'Image);

      --  Check_Is_Null should output True for all other elements

      for I in 2 .. Element_ID'Last loop
         Instance.Check_Is_Null (I, Is_Null);
         Assert (Is_Null, "Check_Is_Null is False for element" & I'Image);
      end loop;
   end Test_Exchange;

   -------------------
   -- Test_Retrieve --
   -------------------

   procedure Test_Retrieve (T : in out Test) is
      pragma Unreferenced (T);

      package Instance is new
        LibSAP.Pointer_Holders (Element_ID, Element_Type, Element_Access);

      Ptr_1   : Element_Access := Elements (1)'Access;
      Ptr_2   : Element_Access := null;
      Is_Null : Boolean;

   begin
      Instance.Exchange (Ptr_1);
      Assert (Ptr_1 = null, "Ptr_1 is not null after first Exchange");

      Instance.Retrieve (1, Ptr_2);
      Assert (Ptr_2 = Elements (1)'Access, "Got wrong pointer after Retrieve");

      Instance.Check_Is_Null (1, Is_Null);
      Assert (Is_Null, "Check_Is_Null is not True after Retrieve");

      Ptr_1 := Elements (1)'Access;
      Instance.Exchange (Ptr_1);
      Assert (Ptr_1 = null, "Ptr_1 is not null after second Exchange");
   end Test_Retrieve;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Test_Caller.Create
           ("[Pointer_Holders] Test_Initial_State",
            Test_Initial_State'Access));
      Ret.Add_Test
        (Test_Caller.Create
           ("[Pointer_Holders] Test_Exchange", Test_Exchange'Access));
      Ret.Add_Test
        (Test_Caller.Create
           ("[Pointer_Holders] Test_Retrieve", Test_Retrieve'Access));
      return Ret;
   end Suite;

end LibSAP.Pointer_Holder_Tests;
