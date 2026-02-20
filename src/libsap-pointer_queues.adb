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

   procedure Lemma_Physical_Positions_Incremented
     (First_Old, First : Physical_Index_Type)
   with
     Ghost,
     Pre  => First = Increment_Wrapping (First_Old),
     Post =>
       (for all I in Logical_Index_Type =>
          Physical_Index (First, I)
          = Increment_Wrapping (Physical_Index (First_Old, I)));
   --  Lemma to show how physical indices are incremented when First is
   --  incremented.

   procedure Lemma_Is_Null_At_Shifted
     (Items     : Element_Access_Array;
      First_Old : Physical_Index_Type;
      First     : Physical_Index_Type)
   with
     Ghost,
     Pre  => First = Increment_Wrapping (First_Old),
     Post =>
       (for all I in Logical_Index_Type =>
          Is_Null_At (Items, First_Old, Increment_Wrapping (I))
          = Is_Null_At (Items, First, I));
   --  Lemma to show how Is_Null_At changes when First is incremented

   procedure Move (Target : out Element_Access; Source : in out Element_Access)
   with
     Inline,
     Global         => null,
     Post           => Source = null,
     Contract_Cases =>
       (Source = null => Target = null, Source /= null => Target /= null);

   ------------
   -- Append --
   ------------

   procedure Append
     (Queue : in out Queue_Type; Pointer : in out Element_Access)
   is
      Last : Physical_Index_Type;

   begin
      Queue.Length := Queue.Length + 1;
      Last := Physical_Index (Queue.First, Queue.Length);
      Move (Target => Queue.Items (Last), Source => Pointer);

      pragma Assert (Queue.Items (Last) /= null);

      pragma
        Assert
          (for all I in Positive range Logical_Index_Type'Range =>
             (I in 1 .. Queue.Length)
             = (Queue.Items (Physical_Index (Queue.First, I)) /= null));
   end Append;

   ------------------------------
   -- Lemma_Is_Null_At_Shifted --
   ------------------------------

   procedure Lemma_Is_Null_At_Shifted
     (Items     : Element_Access_Array;
      First_Old : Physical_Index_Type;
      First     : Physical_Index_Type)
   is
      pragma Unreferenced (Items); --  only referenced in postcondition
   begin
      Lemma_Physical_Positions_Incremented (First_Old, First);
   end Lemma_Is_Null_At_Shifted;

   ------------------------------------------
   -- Lemma_Physical_Positions_Incremented --
   ------------------------------------------

   procedure Lemma_Physical_Positions_Incremented
     (First_Old, First : Physical_Index_Type) is
   begin
      null;
   end Lemma_Physical_Positions_Incremented;

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
      First_Old : constant Physical_Index_Type := Queue.First
      with Ghost;

   begin
      Move (Target => Pointer, Source => Queue.Items (Queue.First));

      --  Intermediate assertions needed to help prove Is_Valid (Queue) in
      --  the postcondition.

      pragma Assert (Is_Null_At (Queue.Items, First_Old, 1));

      --  Only items in range 2 .. Queue.Length are now non-null

      pragma
        Assert
          (for all I in Positive range Logical_Index_Type'Range =>
             (I in 2 .. Queue.Length)
             = not Is_Null_At (Queue.Items, First_Old, I));

      Queue.Length := Queue.Length - 1;
      Queue.First := Increment_Wrapping (Queue.First);

      --  Now that the logical -> physical mapping has shifted, prove
      --  that the range of non-null items has shifted from
      --  2 .. Queue.Length'Old to 1 .. Queue.Length

      Lemma_Is_Null_At_Shifted (Queue.Items, First_Old, Queue.First);

      pragma
        Assert
          (for all I in Positive range Logical_Index_Type'Range =>
             (I in 1 .. Queue.Length)
             = not Is_Null_At (Queue.Items, Queue.First, I));

   end Pop_Front;

end LibSAP.Pointer_Queues;
