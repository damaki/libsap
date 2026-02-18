--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Service_Provider
  with SPARK_Mode
is

   function Add_Would_Overflow (A, B : Natural) return Boolean
   is (A > Natural'Last - B);
   --  Returns True if A + B would overflow

   --------------------------
   -- Confirm_Barrier_Type --
   --------------------------

   protected body Confirm_Barrier_Type is

      -------------------
      -- Add_To_Filter --
      -------------------

      procedure Add_To_Filter (TID : SAP.Transaction_ID) is
      begin
         Filter (TID) := True;
      end Add_To_Filter;

      ------------------------
      -- Remove_From_Filter --
      ------------------------

      procedure Remove_From_Filter (TID : SAP.Transaction_ID) is
      begin
         Filter (TID) := False;
         Pending (TID) := False;

         Has_Pending := (for some P of Pending => P);
      end Remove_From_Filter;

      --------------------------
      -- Wait_For_Any_Confirm --
      --------------------------

      entry Wait_For_Any_Confirm when Has_Pending is
      begin
         for I in Filter'Range loop
            if Pending (I) then
               Filter (I) := False;
            end if;
         end loop;

         Pending := [others => False];
         Has_Pending := False;
      end Wait_For_Any_Confirm;

      ----------------------------
      -- Notify_Confirm_Pending --
      ----------------------------

      procedure Notify_Confirm_Pending (TID : SAP.Transaction_ID) is
      begin
         if Filter (TID) then
            Pending (TID) := True;
            Has_Pending := True;
         end if;
      end Notify_Confirm_Pending;

   end Confirm_Barrier_Type;

   ---------------------------
   -- Service_Provider_Task --
   ---------------------------

   task body Service_Provider_Task is
      Increment_Amount : Natural := 1;

      procedure Process_Request_No_Confirm (Request : Request_Type)
      with
        Global =>
          (In_Out => Log_Service.SAP.Transaction_Queue,
           Output => Increment_Amount),
        Pre    => not Requires_Confirm (Request);

      procedure Process_Request_With_Confirm
        (Request : Request_Type; Confirm : out Confirm_Type)
      with
        Global =>
          (Input  => Increment_Amount,
           In_Out => Log_Service.SAP.Transaction_Queue),
        Pre    => Requires_Confirm (Request) and then not Confirm'Constrained,
        Post   => Valid_Confirm (Request, Confirm);

      --------------------------------
      -- Process_Request_No_Confirm --
      --------------------------------

      procedure Process_Request_No_Confirm (Request : Request_Type) is
      begin
         case Request.Kind is
            when INCREMENT_SET_Req =>
               Log_Service.Log_Message
                 ("[Service Provider] Got INCREMENT-SET.req (New_Amount ="
                  & Request.INCREMENT_SET_Req.New_Amount'Image
                  & ")");

               Increment_Amount := Request.INCREMENT_SET_Req.New_Amount;

            when others            =>
               --  Unreachable since this procedure is only called on requests
               --  that do not require a confirm primitive.

               pragma Assert (False);
         end case;
      end Process_Request_No_Confirm;

      ----------------------------------
      -- Process_Request_With_Confirm --
      ----------------------------------

      procedure Process_Request_With_Confirm
        (Request : Request_Type; Confirm : out Confirm_Type) is
      begin
         case Request.Kind is
            when ECHO_Req      =>
               Log_Service.Log_Message
                 ("[Service Provider] Got ECHO.req (Value_To_Echo ="
                  & Request.ECHO_Req.Value_To_Echo'Image
                  & ")");
               Log_Service.Log_Message ("[Service Provider] Sending ECHO.cfm");

               Confirm :=
                 (Kind     => ECHO_Cfm,
                  ECHO_Cfm => (Value => Request.ECHO_Req.Value_To_Echo));

            when INCREMENT_Req =>
               Log_Service.Log_Message
                 ("[Service Provider] Got INCREMENT.req (Value_To_Increment ="
                  & Request.INCREMENT_Req.Value_To_Increment'Image
                  & ")");
               Log_Service.Log_Message
                 ("[Service Provider] Sending INCREMENT.cfm");

               if Add_Would_Overflow
                    (Request.INCREMENT_Req.Value_To_Increment,
                     Increment_Amount)
               then
                  Confirm :=
                    (Kind          => INCREMENT_Cfm,
                     INCREMENT_Cfm =>
                       (Value => Natural'Last, Overflow => True));
               else
                  Confirm :=
                    (Kind          => INCREMENT_Cfm,
                     INCREMENT_Cfm =>
                       (Value    =>
                          Request.INCREMENT_Req.Value_To_Increment
                          + Increment_Amount,
                        Overflow => True));
               end if;

            when others        =>
               --  Unreachable since this procedure is only called on requests
               --  that require a confirm primitive.

               pragma Assert (False);
         end case;
      end Process_Request_With_Confirm;

      procedure Process_Request is new
        SAP.Process_Request
          (Process_Request_No_Confirm   => Process_Request_No_Confirm,
           Process_Request_With_Confirm => Process_Request_With_Confirm);

      Handle : SAP.Service_Handle;
      TID    : SAP.Transaction_ID;

   begin
      loop
         pragma Loop_Invariant (SAP.Is_Null (Handle));

         --  Wait for a request from the Service User

         SAP.Get_Next_Request (Handle);

         Process_Request (Handle);

         --  Send the confirmation (if one is required) and notify the
         --  Service User task that it is pending by setting the
         --  Confirm_Pending suspension object to True.

         if SAP.Requires_Confirm (Handle) then
            TID := SAP.Get_TID (Handle);

            SAP.Send_Confirm (Handle);

            for CB of Confirm_Barriers loop
               CB.Notify_Confirm_Pending (TID);
            end loop;
         else
            SAP.Request_Completed (Handle);
         end if;

      end loop;
   end Service_Provider_Task;

end Service_Provider;
