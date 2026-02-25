--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with AUnit.Reporter.Text;
with AUnit.Run;

with LibSAP.Pointer_Holder_Tests;

procedure Unit_Tests is
   procedure Runner is new
     AUnit.Run.Test_Runner (LibSAP.Pointer_Holder_Tests.Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Unit_Tests;
