--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

package Synchronous_Provider_SAP_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_One_Normal_Transaction (T : in out Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Synchronous_Provider_SAP_Tests;