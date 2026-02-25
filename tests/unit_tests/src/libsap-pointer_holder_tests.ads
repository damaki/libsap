--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

package LibSAP.Pointer_Holder_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Initial_State (T : in out Test);
   procedure Test_Exchange (T : in out Test);
   procedure Test_Retrieve (T : in out Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end LibSAP.Pointer_Holder_Tests;