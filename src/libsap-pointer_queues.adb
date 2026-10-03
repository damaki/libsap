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

   type Boolean_Array is array (Physical_Index_Type) of Boolean with Ghost;
   --  Model type to record which pointers in the queue memory are non-null

   function Model (Items : Element_Access_Array) return Boolean_Array
   is ([for I in Physical_Index_Type => Items (I) /= null])
   with
     Ghost,
     Post =>
       (for all I in Physical_Index_Type =>
          Model'Result (I) = (Items (I) /= null));
   --  Get a Boolean array which records the positions in Items that are
   --  non-null.

   procedure Lemma_Next_Free_Slot_Is_Null
     (Items  : Element_Access_Array;
      First  : Physical_Index_Type;
      Length : Count_Type)
   with
     Ghost,
     Pre  =>
       Length < Queue_Capacity
       and then
         (for all I in Logical_Index_Type =>
            (I <= Length) = not Is_Null_At (Items, First, I)),
     Post => Is_Null_At (Items, First, Length + 1);
   --  Given an element access array where only elements at logical positions
   --  1 .. Length are non-null, prove that the next element (Length + 1)
   --  is null.

   procedure Lemma_Model_Validity (Queue : Queue_Type)
   with
     Ghost,
     Pre  => Is_Valid (Queue),
     Post =>
       (for all I in Logical_Index_Type =>
          (I <= Queue.Length)
          = Model (Queue.Items) (Physical_Index (Queue.First, I)));
   --  Given a valid queue, prove that only the Model elements in the range
   --  1 .. Queue.Length are True.

   procedure Lemma_Model_Appended
     (A, B   : Boolean_Array;
      First  : Physical_Index_Type;
      Next   : Physical_Index_Type;
      Length : Count_Type)
   with
     Ghost,
     Pre  =>
       Length < Count_Type'Last
       and then Next = Physical_Index (First, Length + 1)

       --  Only model elements in the range 1 .. Length are True
       and then
         (for all I in Logical_Index_Type =>
            (I <= Length) = A (Physical_Index (First, I)))

       --  A and B are equal, except for the element at position Next
       and then
         (for all I in Physical_Index_Type =>
            (if I /= Next then A (I) = B (I)))

       --  The element at Next is set in B, but not A
       and then not A (Next)
       and then B (Next),
     Post =>
       (for all I in Logical_Index_Type =>
          (I <= Length + 1) = B (Physical_Index (First, I)));
   --  Lemma to prove the range of model elements that are True when an element
   --  is appended to the queue.

   procedure Lemma_Logical_Mapping_Shifted (First : Physical_Index_Type)
   with
     Ghost,
     Post =>
       (for all I in Logical_Index_Type =>
          (if I > 1
           then
             Physical_Index (First, I)
             = Physical_Index
                 (Increment_Wrapping (First), I - Increment_Amount)));
   --  Prove the mapping from logical indices to physical indices is shifted by
   --  one when the first index is incremented.

   procedure Lemma_Model_Front_Popped
     (A, B : Boolean_Array; First : Physical_Index_Type; Length : Count_Type)
   with
     Ghost,
     Pre  =>
       Length > 0
       and then
         (for all I in Logical_Index_Type =>
            (I <= Length) = A (Physical_Index (First, I)))
       and then
         (for all I in Physical_Index_Type =>
            (if I /= First then A (I) = B (I)))
       and then A (First)
       and then not B (First),
     Post =>
       (for all I in Logical_Index_Type =>
          (I <= Length - 1)
          = B (Physical_Index (Increment_Wrapping (First), I)));
   --  Lemma to prove the range of model elements that are True when an element
   --  is popped from the front of the queue.

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
      Model_Old : constant Boolean_Array := Model (Queue.Items)
      with Ghost;

      Last : Physical_Index_Type;

   begin
      Lemma_Model_Validity (Queue);
      Lemma_Next_Free_Slot_Is_Null (Queue.Items, Queue.First, Queue.Length);

      --  Move the pointer into the next free slot in the queue memory

      Queue.Length := Queue.Length + 1;
      Last := Physical_Index (Queue.First, Queue.Length);
      Move (Target => Queue.Items (Last), Source => Pointer);

      --  Help prove the postcondition

      Lemma_Model_Appended
        (A      => Model_Old,
         B      => Model (Queue.Items),
         First  => Queue.First,
         Next   => Last,
         Length => Queue.Length - 1);

      pragma
        Assert
          (for all I in Logical_Index_Type =>
             (I <= Queue.Length)
             = not Is_Null_At (Queue.Items, Queue.First, I));
   end Append;

   --------------------------
   -- Lemma_Model_Appended --
   --------------------------

   procedure Lemma_Model_Appended
     (A, B   : Boolean_Array;
      First  : Physical_Index_Type;
      Next   : Physical_Index_Type;
      Length : Count_Type) is
   begin
      null;
   end Lemma_Model_Appended;

   -----------------------------------
   -- Lemma_Logical_Mapping_Shifted --
   -----------------------------------

   procedure Lemma_Logical_Mapping_Shifted (First : Physical_Index_Type) is
   begin
      null;
   end Lemma_Logical_Mapping_Shifted;

   ------------------------------
   -- Lemma_Model_Front_Popped --
   ------------------------------

   procedure Lemma_Model_Front_Popped
     (A, B : Boolean_Array; First : Physical_Index_Type; Length : Count_Type)
   is
      pragma Unreferenced (A, B, Length);
   begin
      Lemma_Logical_Mapping_Shifted (First);
   end Lemma_Model_Front_Popped;

   --------------------------
   -- Lemma_Model_Validity --
   --------------------------

   procedure Lemma_Model_Validity (Queue : Queue_Type) is
   begin
      pragma
        Assert
          (for all I in Logical_Index_Type =>
             Model (Queue.Items) (Physical_Index (Queue.First, I))
             = not Is_Null_At (Queue.Items, Queue.First, I));
   end Lemma_Model_Validity;

   ----------------------------------
   -- Lemma_Next_Free_Slot_Is_Null --
   ----------------------------------

   procedure Lemma_Next_Free_Slot_Is_Null
     (Items  : Element_Access_Array;
      First  : Physical_Index_Type;
      Length : Count_Type) is
   begin
      null;
   end Lemma_Next_Free_Slot_Is_Null;

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
      Model_Old : constant Boolean_Array := Model (Queue.Items)
      with Ghost;

   begin
      Lemma_Model_Validity (Queue);

      pragma Assert (Queue.First = Physical_Index (Queue.First, 1));
      pragma Assert (not Is_Null_At (Queue.Items, Queue.First, 1));

      Move (Target => Pointer, Source => Queue.Items (Queue.First));

      Lemma_Model_Front_Popped
        (A      => Model_Old,
         B      => Model (Queue.Items),
         First  => Queue.First,
         Length => Queue.Length);

      Queue.Length := Queue.Length - 1;
      Queue.First := Increment_Wrapping (Queue.First);

      --  Help prove the postcondition

      pragma
        Assert
          (for all I in Positive range Logical_Index_Type'Range =>
             (I in 1 .. Queue.Length)
             = not Is_Null_At (Queue.Items, Queue.First, I));

   end Pop_Front;

end LibSAP.Pointer_Queues;
