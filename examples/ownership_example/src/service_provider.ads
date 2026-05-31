--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Profile (Jorvik);
pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Interfaces;
with System;
with LibSAP.Synchronous_User_Service_Access_Point;

--  This file defines the Service Access Point (SAP) to the Service Provider.
--
--  In this example, we define two indication (.ind) primitives:
--    * START.ind is sent to notify that the service has started
--    * DATA.ind is sent to notify that some data has been received, and
--      contains a pointer to the data buffer.
--
--  The Service User is able to send the following response primitives:
--    * DATA.res is sent in response to a DATA.ind. It returns the data pointer
--      back to the Service Provider.

package Service_Provider
  with SPARK_Mode
is

   type Data_Buffer_Type is
     array (Positive range 1 .. 16) of Interfaces.Unsigned_8;

   type Data_Pointer is access Data_Buffer_Type;

   --  This example implements the following indication primitives:
   --  START.ind - notifies that the Service Provider has started up.
   --  DATA.ind - notifies the Service User that some data has been received.

   type Indication_Kind is (START_Ind, DATA_Ind);

   type START_Ind_Type is null record;

   type DATA_Ind_Type is record
      Data : Data_Pointer := null;
   end record;

   type Indication_Type (Kind : Indication_Kind := Indication_Kind'First) is
   record
      case Kind is
         when START_Ind =>
            null;

         when DATA_Ind =>
            DATA_Ind : DATA_Ind_Type := (others => <>);
      end case;
   end record;

   --  Only ECHO.req and INCREMENT.req has a corresponding response primitives

   type Response_Kind is (DATA_Res);

   type DATA_Res_Type is record
      Data : Data_Pointer := null;
   end record;

   type Response_Type (Kind : Response_Kind := Response_Kind'First) is record
      case Kind is
         when DATA_Res =>
            DATA_Res : DATA_Res_Type := (others => <>);
      end case;
   end record;

   function Get_Indication_Kind
     (Indication : Indication_Type) return Indication_Kind
   is (Indication.Kind);

   --  Requires_Response is used to determine whether an indication primitive
   --  requires the Service to send a corresponding response primitive.
   --
   --  In this example, only DATA.ind requires a matching response primitive.

   function Requires_Response (Indication : Indication_Type) return Boolean
   is (Indication.Kind = DATA_Ind);

   --  The following properties determine when a primitive requires explicit
   --  clean up to release any held resources before the transaction is
   --  completed.
   --
   --  This example passes pointers in DATA.ind and DATA.res primitives, which
   --  are required to be null initially.

   function Indication_Requires_Cleanup
     (Indication : Indication_Type) return Boolean
   is (case Indication.Kind is
         when START_Ind => False,
         when DATA_Ind  => Indication.DATA_Ind.Data /= null);

   function Response_Requires_Cleanup (Response : Response_Type) return Boolean
   is (Response.DATA_Res.Data /= null);

   function Might_Require_Cleanup (Kind : Indication_Kind) return Boolean
   is (Kind = DATA_Ind);

   --  Valid_Indication is used to prove that an indication contains valid data
   --
   --  In this case, we require that DATA.ind contains a non-null pointer to
   --  the data buffer.

   function Valid_Indication (Indication : Indication_Type) return Boolean
   is (case Indication.Kind is
         when START_Ind => True,
         when DATA_Ind  => Indication.DATA_Ind.Data /= null);

   --  Valid_Response is used to check whether a response primitive is a
   --  correct response to an indication primitive. In this example we want to
   --  prove that the pointer has been moved from the Indication to the
   --  Response.

   function Valid_Response
     (Indication : Indication_Type; Response : Response_Type) return Boolean
   is (case Indication.Kind is
         when START_Ind => False,

         --  In this example, we require the Data pointer to moved to the
         --  Response.
         when DATA_Ind  =>
           Indication.DATA_Ind.Data = null
           and then Response.DATA_Res.Data /= null);

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
     LibSAP.Synchronous_User_Service_Access_Point
       (Indication_Kind_Type        => Indication_Kind,
        Indication_Type             => Indication_Type,
        Response_Type               => Response_Type,
        Indication_Kind             => Get_Indication_Kind,
        Requires_Response           => Requires_Response,
        Indication_Requires_Cleanup => Indication_Requires_Cleanup,
        Response_Requires_Cleanup   => Response_Requires_Cleanup,
        Might_Require_Cleanup       => Might_Require_Cleanup,
        Valid_Indication            => Valid_Indication,
        Valid_Response              => Valid_Response,
        Priority                    => System.Priority'Last,
        Queue_Capacity              => 2);

   task Service_Provider_Task
     with
       Global =>
         (Input => Ada.Real_Time.Clock_Time, In_Out => SAP.Transaction_Queue);

end Service_Provider;
