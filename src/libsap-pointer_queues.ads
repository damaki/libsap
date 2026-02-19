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

   subtype Logical_Index_Type is Positive range 1 .. Queue_Capacity;
   subtype Physical_Index_Type is Positive range 1 .. Queue_Capacity;

   type Element_Access_Array is array (Physical_Index_Type) of Element_Access;

   type Queue_Type is record
      Items  : Element_Access_Array := [others => null];
      First  : Physical_Index_Type := Physical_Index_Type'First;
      Length : Count_Type := 0;
   end record;

   function Physical_Index
     (Queue : Queue_Type; I : Logical_Index_Type)
      return Physical_Index_Type'Base
   is (if I <= (Queue_Capacity - Queue.First) + 1
       then Queue.First + (I - 1)
       else 1 + ((I - 2) - Integer (Queue_Capacity - Queue.First)))
   with Post => Physical_Index'Result in Physical_Index_Type;
   --  Maps a logical index to its physical index in the Queue.Items array

   function Is_Null_At
     (Queue : Queue_Type; I : Logical_Index_Type) return Boolean
   is (Queue.Items (Physical_Index (Queue, I)) = null)
   with Ghost;

   ------------
   -- Length --
   ------------

   function Length (Queue : Queue_Type) return Count_Type
   is (Queue.Length);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Queue : Queue_Type) return Boolean
   is (for all I in Logical_Index_Type =>
         (I <= Queue.Length) = not Is_Null_At (Queue, I));

end LibSAP.Pointer_Queues;
