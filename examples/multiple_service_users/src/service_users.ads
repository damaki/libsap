--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Real_Time;

with Service_Provider;
with Log_Service;

package Service_Users
  with SPARK_Mode
is

   task type Service_User
     (SUID : Service_Provider.Service_User_ID)
     with
       Global =>
         (Input  => Ada.Real_Time.Clock_Time,
          In_Out =>
            (Service_Provider.SAP.Transaction_Queue,
             Service_Provider.Confirm_Barriers,
             Log_Service.SAP.Transaction_Queue));

   Service_User_1 : Service_User (SUID => 1);
   Service_User_2 : Service_User (SUID => 2);
   Service_User_3 : Service_User (SUID => 3);

end Service_Users;
