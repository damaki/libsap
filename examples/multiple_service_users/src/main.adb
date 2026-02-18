--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Service_Users;
with Ada.Real_Time;

pragma Unreferenced (Service_Users);

procedure Main with SPARK_Mode is
begin

   --  Nothing to do here. Everything is driven by Service_Users tasks

   loop
      delay until Ada.Real_Time.Time_Last;
   end loop;
end Main;
