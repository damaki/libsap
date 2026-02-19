--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This is a helper package that implements an extremely simple FIFO queue
--  that can only contain unique integers (similar to an ordered set).
--
--  While other container libraries like SPARKlib could be used (and would
--  be more flexible than this implementation), this package is intended to
--  be fast and have an extremely small memory footprint, and is only needed
--  to manage the pending request queue in Singleton_Transaction_Queues.

package body LibSAP.Pointer_Queues
  with SPARK_Mode
is

   procedure Move (Target : out Element_Access; Source : in out Element_Access)
   with
     Inline,
     Global         => null,
     Post           => Source = null,
     Contract_Cases =>
       (Source = null => Target = null, Source /= null => Target /= null);

   ----------
   -- Move --
   ----------

   procedure Move (Target : out Element_Access; Source : in out Element_Access)
   is
   begin
      Target := Source;
      Source := null;
   end Move;

   ---------------
   -- Pop_Front --
   ---------------

   procedure Pop_Front
     (Queue : in out Queue_Type; Pointer : out Element_Access)
   is
      Increment_Amount : constant Physical_Index_Type'Base :=
        (if Physical_Index_Type'Last = Physical_Index_Type'First
         then 0
         else 1);
      --  This is a workaround for a compiler warning when Queue_Capacity = 1
      --  where the compiler thinks that First := First + 1 would overflow,
      --  even though the 'if' statement below guards against that case.

   begin
      Move (Target => Pointer, Source => Queue.Items (Queue.First));

      Queue.Length := Queue.Length - 1;

      if Queue.First = Physical_Index_Type'Last then
         Queue.First := 1;
      else
         Queue.First := Queue.First + Increment_Amount;
      end if;
   end Pop_Front;

   ------------
   -- Append --
   ------------

   procedure Append
     (Queue : in out Queue_Type; Pointer : in out Element_Access) is
   begin
      Queue.Length := Queue.Length + 1;

      Move
        (Target => Queue.Items (Physical_Index (Queue, Queue.Length)),
         Source => Pointer);
   end Append;

end LibSAP.Pointer_Queues;
