--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body LibSAP.Unique_Integer_Queues
  with SPARK_Mode
is

   ------------
   -- Append --
   ------------

   procedure Append (Queue : in out Queue_Type; Item : Element_Type) is
      Q_Old : constant Queue_Type := Queue
      with Ghost;

   begin
      Lemma_Not_Full (Queue, Item);

      if Queue.Length = 0 then
         Queue.Length := 1;
         Queue.First := Logical_Index_Type'First;
         Queue.Items (Queue.First) := Item;

         pragma Assert (Length (Queue) = 1);
         pragma Assert (Element_At (Queue, 1) = Item);
         pragma Assert (No_Duplicates (Queue));

      else
         Queue.Length := Queue.Length + 1;
         Queue.Items (Physical_Index (Queue, Queue.Length)) := Item;

         pragma
           Assert
             (for all I in 1 .. Queue.Capacity =>
                Physical_Index (Queue, I) = Physical_Index (Q_Old, I));

         pragma
           Assert
             (for all I in 1 .. Queue.Capacity =>
                (for all J in 1 .. Queue.Capacity =>
                   (if Physical_Index (Queue, I) = Physical_Index (Queue, J)
                    then I = J)));

         pragma
           Assert
             (for all I in 1 .. Length (Q_Old) =>
                Element_At (Queue, I) = Element_At (Q_Old, I));

         pragma Assert (Element_At (Queue, Length (Queue)) = Item);
         pragma Assert (No_Duplicates (Queue));
      end if;
   end Append;

   --------------------------------
   -- Lemma_Count_True_All_False --
   --------------------------------

   procedure Lemma_Count_True_All_False (BA : Boolean_Array) is
   begin
      for I in BA'Range loop
         pragma Loop_Invariant (Count_True (BA, I) = 0);
      end loop;
   end Lemma_Count_True_All_False;

   --------------------
   -- Lemma_All_True --
   --------------------

   procedure Lemma_All_True (BA : Boolean_Array) is
   begin
      for I in BA'Range loop
         pragma
           Loop_Invariant
             (if (for some J in 1 .. I => not BA (J))
              then Count_True (BA, I) < I);
      end loop;
   end Lemma_All_True;

   -----------------------------------
   -- Lemma_Full_Queue_Contains_All --
   -----------------------------------

   procedure Lemma_Full_Queue_Contains_All (Queue : Queue_Type) is
      E_Contains : Boolean_Array (1 .. Queue.Capacity) := [others => False];
   begin
      Lemma_Count_True_All_False (E_Contains);

      for I in 1 .. Queue.Capacity loop
         pragma Loop_Invariant (Count_True (E_Contains) = I - 1);

         pragma
           Loop_Invariant
             (for all E in 1 .. Queue.Capacity =>
                E_Contains (E)
                = (for some J in Logical_Index_Type'First .. I - 1 =>
                     Element_At (Queue, J) = E));

         E_Contains := Set_True (E_Contains, Element_At (Queue, I));
      end loop;

      pragma Assert (Count_True (E_Contains) = Queue.Capacity);

      pragma
        Assert
          (for all E in 1 .. Queue.Capacity =>
             E_Contains (E) = Contains (Queue, E));

      Lemma_All_True (E_Contains);

   end Lemma_Full_Queue_Contains_All;

   --------------------
   -- Lemma_Not_Full --
   --------------------

   procedure Lemma_Not_Full (Queue : Queue_Type; Item : Element_Type) is
      Found : Boolean_Array (1 .. Queue.Capacity) := [others => False];
   begin
      if Length (Queue) = Queue.Capacity then
         Lemma_Full_Queue_Contains_All (Queue);
         pragma Assert (False);
      end if;
   end Lemma_Not_Full;

   ---------------
   -- Pop_Front --
   ---------------

   procedure Pop_Front (Queue : in out Queue_Type; Item : out Element_Type) is
      Q_Old : constant Queue_Type := Queue
      with Ghost;

   begin
      Item := Queue.Items (Queue.First);
      Queue.Length := Queue.Length - 1;

      if Queue.First = Queue.Capacity then
         Queue.First := Logical_Index_Type'First;
      else
         Queue.First := Queue.First + 1;
      end if;

      pragma
        Assert
          (for all I in 1 .. Length (Queue) =>
             Physical_Index (Queue, I) = Physical_Index (Q_Old, I + 1));

      pragma
        Assert
          (for all I in 1 .. Length (Queue) =>
             Element_At (Queue, I) = Element_At (Q_Old, I + 1));

      pragma
        Assert
          (for all I in 2 .. Length (Q_Old) =>
             Element_At (Q_Old, I) = Element_At (Queue, I - 1));

      pragma
        Assert
          (for all I in 2 .. Length (Q_Old) =>
             Element_At (Q_Old, I) /= Element_At (Q_Old, 1));

      pragma Assert (No_Duplicates (Q_Old));
      pragma Assert (No_Duplicates (Queue));
   end Pop_Front;

   --------------
   -- Set_True --
   --------------

   function Set_True
     (BA : Boolean_Array; I : Element_Type) return Boolean_Array
   is
      Result : constant Boolean_Array (BA'Range) := [BA with delta I => True];
   begin
      for J in BA'Range loop
         pragma
           Loop_Invariant
             (if J < I
              then Count_True (Result, J) = Count_True (BA, J)
              else Count_True (Result, J) = Count_True (BA, J) + 1);
      end loop;

      return Result;
   end Set_True;

end LibSAP.Unique_Integer_Queues;
