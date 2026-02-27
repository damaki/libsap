
with LibSAP.Pointer_Holder_Tests;
with LibSAP.Pointer_Queues_Tests;
with Synchronous_Provider_SAP_Tests;
with Synchronous_User_SAP_Tests;
with Light_Provider_SAP_Tests;
with Light_User_SAP_Tests;

package body Test_Suites is

   function Suite return Access_Test_Suite
   is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (LibSAP.Pointer_Holder_Tests.Suite);
      Ret.Add_Test (LibSAP.Pointer_Queues_Tests.Suite);
      Ret.Add_Test (Synchronous_Provider_SAP_Tests.Suite);
      Ret.Add_Test (Synchronous_User_SAP_Tests.Suite);
      Ret.Add_Test (Light_Provider_SAP_Tests.Suite);
      Ret.Add_Test (Light_User_SAP_Tests.Suite);
      return Ret;
   end Suite;

end Test_Suites;