--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Profile (Jorvik);
pragma Partition_Elaboration_Policy (Sequential);

with System;
with LibSAP.Synchronous_Provider_Service_Access_Point;

package Service_Provider
  with SPARK_Mode
is

   --  This example implements the following request primitives:
   --  ECHO.req - request to echo back an integer value in an ECHO.cfm
   --  INCREMENT.req - request to increment an integer in an INCREMENT.cfm
   --  INCREMENT-SET.req - request to change the amount to increment by

   type Request_Kind is (ECHO_Req, INCREMENT_Req, INCREMENT_SET_Req);

   type ECHO_Req_Type is record
      Value_To_Echo : Natural := 0;
   end record;

   type INCREMENT_Req_Type is record
      Value_To_Increment : Natural := 0;
   end record;

   type INCREMENT_SET_Req_Type is record
      New_Amount : Natural := 0;
   end record;

   type Request_Type (Kind : Request_Kind := Request_Kind'First) is record
      case Kind is
         when ECHO_Req =>
            ECHO_Req : ECHO_Req_Type := (others => <>);

         when INCREMENT_Req =>
            INCREMENT_Req : INCREMENT_Req_Type := (others => <>);

         when INCREMENT_SET_Req =>
            INCREMENT_SET_Req : INCREMENT_SET_Req_Type := (others => <>);
      end case;
   end record;

   --  Only ECHO.req and INCREMENT.req have corresponding confirm primitives

   type Confirm_Kind is (ECHO_Cfm, INCREMENT_Cfm);

   type ECHO_Cfm_Type is record
      Value : Natural := 0;
   end record;

   type INCREMENT_Cfm_Type is record
      Value    : Natural := 0;
      Overflow : Boolean := False;
   end record;

   type Confirm_Type (Kind : Confirm_Kind := Confirm_Kind'First) is record
      case Kind is
         when ECHO_Cfm =>
            ECHO_Cfm : ECHO_Cfm_Type := (others => <>);

         when INCREMENT_Cfm =>
            INCREMENT_Cfm : INCREMENT_Cfm_Type := (others => <>);
      end case;
   end record;

   --  Requires_Confirm is used to determine whether a request primitive
   --  requires the Service to send a corresponding confirm primitive.
   --
   --  In this example, only ECHO.req and INCREMENT.req require a matching
   --  confirm primitive.

   function Requires_Confirm (Request : Request_Type) return Boolean
   is (Request.Kind in ECHO_Req | INCREMENT_Req);

   --  Valid_Confirm is used to check whether a confirm primitive is a correct
   --  response to a request primitive. In this example it is sufficient to
   --  simply check that the the message kind is correct, e.g. that a ECHO.cfm
   --  is sent in response to an ECHO.req.

   function Valid_Confirm
     (Request : Request_Type; Confirm : Confirm_Type) return Boolean
   is (case Request.Kind is
         when ECHO_Req          => Confirm.Kind = ECHO_Cfm,
         when INCREMENT_Req     => Confirm.Kind = INCREMENT_Cfm,
         when INCREMENT_SET_Req => False);

   --  Now we can instantiate the Service Access Point (SAP) for the
   --  Service Provider.
   --
   --  Note that Priority is the ceiling priority for the SAP. Only tasks
   --  whose priority is less than or equal to Priority may make calls to the
   --  SAP.
   --
   --  Queue_Capacity is the maximum number of concurrent transactions that
   --  can be in progress at any point in time. This example only performs one
   --  transaction at a time, so we set Queue_Capacity to 1.

   package SAP is new
     LibSAP.Synchronous_Provider_Service_Access_Point
       (Request_Type     => Request_Type,
        Confirm_Type     => Confirm_Type,
        Requires_Confirm => Requires_Confirm,
        Valid_Confirm    => Valid_Confirm,
        Priority         => System.Priority'Last,
        Queue_Capacity   => 1);

   --  The SAP does not provide a mechanism to block (wait) for a confirm
   --  primitive to be posted for a specific transaction. Since this example
   --  has a single Service User (the Main procedure), we use a simple
   --  protected object to allow it to block until it is notified by the
   --  Service Provider.

   protected Confirm_Barrier is
      entry Wait_For_Confirm;
      procedure Notify_Confirm_Pending;
   private
      Confirm_Pending : Boolean := False;
   end Confirm_Barrier;

   task Service_Provider_Task
     with
       Global =>
         (In_Out => (SAP.Transaction_Queue, Confirm_Barrier));

end Service_Provider;
