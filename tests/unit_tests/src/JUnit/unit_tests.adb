--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with AUnit.Reporter.JUnit;
with AUnit.Run;

with Test_Suites;

procedure Unit_Tests is
   procedure Runner is new
     AUnit.Run.Test_Runner (Test_Suites.Suite);

   Reporter : AUnit.Reporter.JUnit.JUnit_Reporter;
begin
   Runner (Reporter);
end Unit_Tests;
