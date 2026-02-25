--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

package LibSAP.Pointer_Queues_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  LibSAP.Pointer_Queues is in SPARK, so we only test things that are not
   --  covered by proof.

   procedure Test_One (T : in out Test);

   procedure Test_FIFO_Two (T : in out Test);
   procedure Test_FIFO_Full (T : in out Test);
   procedure Test_Wraparound (T : in out Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end LibSAP.Pointer_Queues_Tests;