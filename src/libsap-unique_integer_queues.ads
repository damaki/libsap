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

private package LibSAP.Unique_Integer_Queues
  with Pure, SPARK_Mode, Always_Terminates
is

   subtype Element_Type is Positive;

   type Queue_Type (Capacity : Positive) is private;

   function Is_Valid (Queue : Queue_Type) return Boolean
   with Ghost;

   function Contains (Queue : Queue_Type; Item : Element_Type) return Boolean
   with
     Ghost,
     Global => null,
     Post   => (if Item > Queue.Capacity then not Contains'Result);

   function Is_Empty (Queue : Queue_Type) return Boolean
   with
     Post =>
       Is_Empty'Result
       = (for all E in Element_Type range 1 .. Queue.Capacity =>
            not Contains (Queue, E));

   procedure Pop_Front (Queue : in out Queue_Type; Item : out Element_Type)
   with
     Global => null,
     Pre    => Is_Valid (Queue) and then not Is_Empty (Queue),
     Post   =>
       Is_Valid (Queue)
       and then Contains (Queue'Old, Item)
       and then not Contains (Queue, Item)
       and then Item in 1 .. Queue.Capacity
       and then
         (for all I in 1 .. Queue.Capacity =>
            (Contains (Queue, I)
             = (I /= Item and then Contains (Queue'Old, I))));

   procedure Append (Queue : in out Queue_Type; Item : Element_Type)
   with
     Global => null,
     Pre    =>
       Is_Valid (Queue)
       and then Item in 1 .. Queue.Capacity
       and then not Contains (Queue, Item),
     Post   =>
       Is_Valid (Queue)
       and then Contains (Queue, Item)
       and then
         (for all I in 1 .. Queue.Capacity =>
            (Contains (Queue, I) = Contains (Queue'Old, I) or else I = Item));

private

   subtype Logical_Index_Type is Positive;
   subtype Physical_Index_Type is Positive;
   subtype Count_Type is Natural;

   type Element_Array is array (Physical_Index_Type range <>) of Element_Type;

   type Queue_Type (Capacity : Positive) is record
      Items  : Element_Array (1 .. Capacity) := [others => Element_Type'First];
      First  : Physical_Index_Type := Physical_Index_Type'First;
      Length : Count_Type := 0;
   end record
   with
     Predicate =>
       First <= Capacity
       and then Length <= Capacity
       and then (for all E of Items => E in 1 .. Capacity);

   ------------------------
   -- Property Functions --
   ------------------------

   function Physical_Index
     (Queue : Queue_Type; I : Logical_Index_Type) return Physical_Index_Type
   is (if I <= (Queue.Capacity - Queue.First) + 1
       then Queue.First + (I - 1)
       else
         Logical_Index_Type'First + ((I - 2) - (Queue.Capacity - Queue.First)))
   with Pre => I <= Queue.Capacity;
   --  Maps a logical index (in the range 1 .. Queue.Capacity) to its physical
   --  index in the Queue.Items array.

   function Length (Queue : Queue_Type) return Count_Type
   is (Queue.Length)
   with Ghost;

   function Element_At
     (Queue : Queue_Type; I : Logical_Index_Type) return Element_Type
   is (Queue.Items (Physical_Index (Queue, I)))
   with Ghost, Pre => I <= Length (Queue);

   function No_Duplicates (Queue : Queue_Type) return Boolean
   is (for all I in 1 .. Length (Queue) =>
         (for all J in 1 .. Length (Queue) =>
            (if Element_At (Queue, I) = Element_At (Queue, J) then I = J)))
   with Ghost;

   -------------
   -- Helpers --
   -------------

   type Boolean_Array is array (Element_Type range <>) of Boolean;

   function Count_True
     (BA : Boolean_Array; Item : Element_Type) return Count_Type
   is ((if BA (Item) then 1 else 0)
       + (if Item = Element_Type'First then 0 else Count_True (BA, Item - 1)))
   with
     Ghost,
     Pre                =>
       BA'First = 1 and then BA'Last >= BA'First and then Item in BA'Range,
     Post               => Count_True'Result in 0 .. Item,
     Subprogram_Variant => (Decreases => Item);
   --  Count the number of elements that are True in range BA (1 .. Item)

   function Count_True (BA : Boolean_Array) return Count_Type
   is (Count_True (BA, BA'Last))
   with
     Ghost,
     Pre  => BA'First = 1 and then BA'Last >= BA'First,
     Post => Count_True'Result in 0 .. BA'Length;
   --  Count the number of element that are True in BA

   function Set_True
     (BA : Boolean_Array; I : Element_Type) return Boolean_Array
   with
     Ghost,
     Pre  =>
       BA'First = 1
       and then BA'Last >= BA'First
       and then I in BA'Range
       and then not BA (I),
     Post =>
       Set_True'Result'First = BA'First
       and then Set_True'Result'Last = BA'Last
       and then
         (for all J in BA'Range =>
            (if J /= I then Set_True'Result (J) = BA (J)))
       and then Set_True'Result (I)
       and then Count_True (Set_True'Result) = Count_True (BA) + 1;
   --  Return a copy of BA that has BA (I) set to True.
   --
   --  This is useful to preserve information about Count_True.

   ------------
   -- Lemmas --
   ------------

   procedure Lemma_Not_Full (Queue : Queue_Type; Item : Element_Type)
   with
     Ghost,
     Pre  =>
       No_Duplicates (Queue)
       and then Item in 1 .. Queue.Capacity
       and then not Contains (Queue, Item),
     Post => Length (Queue) < Queue.Capacity;

   procedure Lemma_Full_Queue_Contains_All (Queue : Queue_Type)
   with
     Ghost,
     Pre  => No_Duplicates (Queue) and then Length (Queue) = Queue.Capacity,
     Post => (for all E in 1 .. Queue.Capacity => Contains (Queue, E));

   procedure Lemma_All_True (BA : Boolean_Array)
   with
     Ghost,
     Pre  =>
       BA'First = 1
       and then BA'Last >= BA'First
       and then Count_True (BA) = BA'Length,
     Post => (for all B of BA => B);

   procedure Lemma_Count_True_All_False (BA : Boolean_Array)
   with
     Ghost,
     Pre  =>
       BA'First = 1
       and then BA'Last >= BA'First
       and then (for all B of BA => not B),
     Post => Count_True (BA) = 0;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Queue : Queue_Type) return Boolean
   is (Queue.Length = 0);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Queue : Queue_Type) return Boolean
   is (No_Duplicates (Queue));

   --------------
   -- Contains --
   --------------

   function Contains (Queue : Queue_Type; Item : Element_Type) return Boolean
   is (for some I in 1 .. Length (Queue) => Element_At (Queue, I) = Item);

end LibSAP.Unique_Integer_Queues;
