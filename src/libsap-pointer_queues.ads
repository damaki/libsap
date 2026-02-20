--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This is a helper package that implements an extremely simple FIFO queue
--  that holds pointers.

private generic
   type Element_Type;
   type Element_Access is access all Element_Type;
   Queue_Capacity : Positive;
package LibSAP.Pointer_Queues with Pure, SPARK_Mode, Always_Terminates is

   type Queue_Type is private;

   subtype Count_Type is Natural range 0 .. Queue_Capacity;

   function Is_Valid (Queue : Queue_Type) return Boolean
   with Ghost, Global => null;

   subtype Valid_Queue_Type is Queue_Type
   with Ghost_Predicate => Is_Valid (Valid_Queue_Type);

   function Length (Queue : Queue_Type) return Count_Type
   with Global => null;

   procedure Pop_Front
     (Queue : in out Queue_Type; Pointer : out Element_Access)
   with
     Global => null,
     Pre    => Is_Valid (Queue) and then Length (Queue) > 0,
     Post   =>
       Is_Valid (Queue)
       and Length (Queue) = Length (Queue)'Old - 1
       and Pointer /= null;

   procedure Append
     (Queue : in out Queue_Type; Pointer : in out Element_Access)
   with
     Global => null,
     Pre    =>
       Is_Valid (Queue)
       and then Length (Queue) < Queue_Capacity
       and then Pointer /= null,
     Post   =>
       Is_Valid (Queue)
       and Length (Queue) = Length (Queue)'Old + 1
       and Pointer = null;

private

   --  The queue is implemented as a ring buffer.
   --
   --  It is sometimes convenient to be able to index elements relative to the
   --  first one. For example, refer to elements in the range 1 .. Length.
   --  However, the first element in the ring buffer is not always at index
   --  1 in the array. To manage this we use two indexing schemes:
   --
   --  The logical index is the index relative to the first element, so
   --  1 refers to the first element.
   --
   --  The physical index is the index of the element in the ring buffer.

   subtype Index_Type is Positive range 1 .. Queue_Capacity;
   subtype Logical_Index_Type is Index_Type;
   subtype Physical_Index_Type is Index_Type;

   type Element_Access_Array is array (Physical_Index_Type) of Element_Access;

   type Queue_Type is record
      Items  : Element_Access_Array := [others => null];
      First  : Physical_Index_Type := Physical_Index_Type'First;
      Length : Count_Type := 0;
   end record;

   function Physical_Index
     (First : Physical_Index_Type; I : Logical_Index_Type)
      return Physical_Index_Type'Base
   is (if I <= (Queue_Capacity - First) + 1
       then First + (I - 1)
       else 1 + ((I - 2) - (Queue_Capacity - First)));
   --  Maps a logical index to its physical index in the Queue.Items array

   function Increment_Wrapping (I : Index_Type) return Index_Type
   is (if I = Index_Type'Last
       then 1
       else

         --  Add 1 or 0 to work around a compiler warning when
         --  Queue_Capacity = 1 where the compiler thinks that I + 1 would
         --  overflow, even though the above 'if' condition guards against that
         --  case.
         --
         --  This should fold down to a simple constant at compile time
         --  so should be equivalent to I + 1.

          (I + (if Index_Type'First /= Index_Type'Last then 1 else 0)));

   function Is_Null_At
     (Items : Element_Access_Array;
      First : Physical_Index_Type;
      I     : Logical_Index_Type) return Boolean
   is (Items (Physical_Index (First, I)) = null)
   with Ghost;

   ------------
   -- Length --
   ------------

   function Length (Queue : Queue_Type) return Count_Type
   is (Queue.Length);

   --------------
   -- Is_Valid --
   --------------

   --  A valid queue has non-null pointers in the logical range 1 .. Length
   --  and null pointers at all other positions.

   function Is_Valid (Queue : Queue_Type) return Boolean
   is (for all I in Logical_Index_Type =>
         (I <= Queue.Length) = not Is_Null_At (Queue.Items, Queue.First, I));

end LibSAP.Pointer_Queues;
