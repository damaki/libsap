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

   ---------------------
   -- Confirm_Barrier --
   ---------------------

   protected body Confirm_Barrier is

      ----------------------
      -- Wait_For_Confirm --
      ----------------------

      entry Wait_For_Confirm when Confirm_Pending is
      begin
         Confirm_Pending := False;
      end Wait_For_Confirm;

      ----------------------------
      -- Notify_Confirm_Pending --
      ----------------------------

      procedure Notify_Confirm_Pending is
      begin
         Confirm_Pending := True;
      end Notify_Confirm_Pending;

   end Confirm_Barrier;

   ---------------------------
   -- Service_Provider_Task --
   ---------------------------

   task body Service_Provider_Task is
      Increment_Amount : Natural := 1;

      procedure Process_Request_No_Confirm (Request : Request_Type)
      with
        Global => (Output => Increment_Amount),
        Pre    => not Requires_Confirm (Request);

      procedure Process_Request_With_Confirm
        (Request : Request_Type; Confirm : out Confirm_Type)
      with
        Global => (Input => Increment_Amount),
        Pre    => Requires_Confirm (Request) and then not Confirm'Constrained,
        Post   => Valid_Confirm (Request, Confirm);

      --------------------------------
      -- Process_Request_No_Confirm --
      --------------------------------

      procedure Process_Request_No_Confirm (Request : Request_Type) is
      begin
         case Request.Kind is
            when INCREMENT_SET_Req =>
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
               Confirm :=
                 (Kind     => ECHO_Cfm,
                  ECHO_Cfm => (Value => Request.ECHO_Req.Value_To_Echo));

            when INCREMENT_Req =>
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

   begin
      loop
         pragma Loop_Invariant (SAP.Is_Null (Handle));

         --  Wait for a request from the Service User

         SAP.Get_Next_Request (Handle);

         Process_Request (Handle);

         --  Send the confirmation (if one is required) and notify the
         --  Service User task that it is pending.

         if SAP.Requires_Confirm (Handle) then
            SAP.Send_Confirm (Handle);

            Confirm_Barrier.Notify_Confirm_Pending;

         else
            SAP.Request_Completed (Handle);
         end if;

      end loop;
   end Service_Provider_Task;

end Service_Provider;
