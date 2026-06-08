--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This file contains the logic for the Service Provider.
--
--  In this example, the Service Provider performs the following sequence:
--   1. Send a START.ind
--   2. Send a DATA.ind (containing a pointer to a data buffer)
--   3. Wait for a DATA.res to be sent back, and extract the returned pointer
--   4. Delay for 1 second
--   5. Repeat this process from step 2.

package body Service_Provider
  with SPARK_Mode
is

   procedure Send_START_Ind
   with Global => (In_Out => SAP.Transaction_Queue);
   --  Builds and sends a START.ind primitive

   procedure Send_DATA_Ind
     (Data : in out Data_Pointer; Res_Promise : in out SAP.Response_Promise)
   with
     Global => (In_Out => SAP.Transaction_Queue),
     Pre    => Data /= null and then SAP.Is_Null (Res_Promise),
     Post   =>
       (if not SAP.Is_Null (Res_Promise)
        then
          Data = null
          and then not SAP.Is_Null (Res_Promise)
          and then SAP.Indication_Kind (Res_Promise) = DATA_Ind
        else Data /= null);
   --  Builds and sends a DATA.ind primitive
   --
   --  The Data pointer is moved into the DATA.ind primitive and set to null.

   procedure Process_DATA_Res
     (Handle : in out SAP.Response_Handle; Data : in out Data_Pointer)
   with
     Global => (In_Out => SAP.Transaction_Queue),
     Pre    =>
       Data = null
       and then not SAP.Is_Null (Handle)
       and then SAP.Indication_Kind (Handle) = DATA_Ind
       and then
         Valid_Response
           (SAP.Indication_Reference (Handle).all,
            SAP.Response_Reference (Handle).all),
     Post   => SAP.Is_Null (Handle) and then Data /= null;
   --  Process a DATA.res primitive that was received from the Service User.
   --
   --  This procedure extracts the Data pointer from the DATA.res primitive
   --  and moves it to the Data output parameter.

   --------------------
   -- Send_START_Ind --
   --------------------

   procedure Send_START_Ind is
      Handle      : SAP.Indication_Handle;
      Res_Promise : SAP.Response_Promise;
   begin
      SAP.Try_Allocate_Indication (Handle);

      --  Don't care in this example if allocation fails

      if SAP.Is_Null (Handle) then
         return;
      end if;

      --  Build and send the START.Ind

      declare
         function Postcondition (Indication : Indication_Type) return Boolean
         is (Indication.Kind = START_Ind);

         procedure Build_START_Ind (Indication : out Indication_Type)
         with
           Pre  => not Indication'Constrained,
           Post => Postcondition (Indication);

         ---------------------
         -- Build_START_Ind --
         ---------------------

         procedure Build_START_Ind (Indication : out Indication_Type) is
         begin
            Indication := (Kind => START_Ind);
         end Build_START_Ind;

         procedure Initialize_Indication is new
           SAP.Initialize_Indication
             (Initialize    => Build_START_Ind,
              Postcondition => Postcondition);

      begin
         Initialize_Indication (Handle);
         SAP.Send_Indication (Handle, Res_Promise);
      end;

      pragma Unreferenced (Handle);
      pragma Unreferenced (Res_Promise);

   end Send_START_Ind;

   -------------------
   -- Send_DATA_Ind --
   -------------------

   procedure Send_DATA_Ind
     (Data : in out Data_Pointer; Res_Promise : in out SAP.Response_Promise)
   is
      Handle : SAP.Indication_Handle;
   begin
      SAP.Try_Allocate_Indication (Handle);

      --  Don't care in this example if allocation fails

      if SAP.Is_Null (Handle) then
         return;
      end if;

      --  Build and send the DATA.Ind

      declare
         function Precondition return Boolean
         is (Data /= null);

         function Postcondition (Indication : Indication_Type) return Boolean
         is (Indication.Kind = DATA_Ind
             and then Valid_Indication (Indication)
             and then Data = null);

         procedure Build_DATA_Ind (Indication : out Indication_Type)
         with
           Global => (In_Out => Data),
           Pre    => not Indication'Constrained and then Precondition,
           Post   => Postcondition (Indication);

         --------------------
         -- Build_DATA_Ind --
         --------------------

         procedure Build_DATA_Ind (Indication : out Indication_Type) is
         begin
            Indication := (Kind => DATA_Ind, DATA_Ind => (Data => Data));
            Data := null;
         end Build_DATA_Ind;

         procedure Initialize_Indication is new
           SAP.Initialize_Indication
             (Initialize    => Build_DATA_Ind,
              Precondition  => Precondition,
              Postcondition => Postcondition);

      begin
         Initialize_Indication (Handle);
         SAP.Send_Indication (Handle, Res_Promise);
      end;

      pragma Unreferenced (Handle);
   end Send_DATA_Ind;

   ----------------------
   -- Process_DATA_Res --
   ----------------------

   procedure Process_DATA_Res
     (Handle : in out SAP.Response_Handle; Data : in out Data_Pointer)
   is

      function Precondition
        (Indication : Indication_Type; Response : Response_Type) return Boolean
      is (Valid_Response (Indication, Response) and then Data = null);

      function Postcondition
        (Indication : Indication_Type; Response : Response_Type) return Boolean
      is (not Indication_Requires_Cleanup (Indication)
          and then not Response_Requires_Cleanup (Response)
          and then Data /= null);

      procedure Extract_Pointer
        (Indication : in out Indication_Type; Response : in out Response_Type)
      with
        Global => (In_Out => Data),
        Pre    => Precondition (Indication, Response),
        Post   => Postcondition (Indication, Response);

      ---------------------
      -- Extract_Pointer --
      ---------------------

      procedure Extract_Pointer
        (Indication : in out Indication_Type with Unreferenced;
         Response   : in out Response_Type)
      is
         pragma Unmodified (Indication);
      begin
         Data := Response.DATA_Res.Data;
         Response.DATA_Res.Data := null;
      end Extract_Pointer;

      procedure Cleanup is new
        SAP.Cleanup
          (Clean         => Extract_Pointer,
           Precondition  => Precondition,
           Postcondition => Postcondition);

   begin
      pragma Assert (Data = null);

      Cleanup (Handle);
      SAP.Release (Handle);
   end Process_DATA_Res;

   ---------------------------
   -- Service_Provider_Task --
   ---------------------------

   task body Service_Provider_Task is
      Data        : Data_Pointer := new Data_Buffer_Type'(others => 0);
      Res_Promise : SAP.Response_Promise;
      Res_Handle  : SAP.Response_Handle;
   begin
      Send_START_Ind;

      loop
         pragma Loop_Invariant (SAP.Is_Null (Res_Promise));
         pragma Loop_Invariant (SAP.Is_Null (Res_Handle));
         pragma Loop_Invariant (Data /= null);

         Send_DATA_Ind (Data, Res_Promise);

         if not SAP.Is_Null (Res_Promise) then

            --  Poll until the response is received

            loop
               pragma Loop_Invariant (not SAP.Is_Null (Res_Promise));
               pragma Loop_Invariant (SAP.Is_Null (Res_Handle));

               SAP.Try_Get_Response (Res_Handle, Res_Promise);
               exit when not SAP.Is_Null (Res_Handle);
               delay 0.1;
            end loop;

            Process_DATA_Res (Res_Handle, Data);
         end if;

         --  Short delay before sending the next DATA.ind

         delay 1.0;
      end loop;
   end Service_Provider_Task;

end Service_Provider;
