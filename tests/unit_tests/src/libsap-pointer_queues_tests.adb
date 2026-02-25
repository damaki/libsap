--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with AUnit.Assertions;  use AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites; use AUnit.Test_Suites;

with LibSAP.Pointer_Queues;

package body LibSAP.Pointer_Queues_Tests is

   package Test_Caller is new AUnit.Test_Caller (Test);

   type Integer_Access is access all Integer;

   Element_1 : aliased Integer := 1;
   Element_2 : aliased Integer := 2;
   Element_3 : aliased Integer := 3;

   --------------
   -- Test_One --
   --------------

   --  This test checks that given a queue with capacity 1, appending an item
   --  then popping the queue retrieves the original item.

   procedure Test_One (T : in out Test) is
      package Instance is new
        LibSAP.Pointer_Queues
          (Element_Type   => Integer,
           Element_Access => Integer_Access,
           Queue_Capacity => 1);

      Queue : Instance.Queue_Type;
      Ptr   : Integer_Access;

   begin
      Ptr := Element_1'Access;
      Instance.Append (Queue, Ptr);
      Assert (Ptr = null, "Ptr not null after Append");

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_1'Access, "Wrong value popped from queue");
   end Test_One;

   -------------------
   -- Test_FIFO_Two --
   -------------------

   --  This test checks that given a queue with capacity 3 and two items
   --  appended to the queue, they are popped off the queue in FIFO order.

   procedure Test_FIFO_Two (T : in out Test) is
      package Instance is new
        LibSAP.Pointer_Queues
          (Element_Type   => Integer,
           Element_Access => Integer_Access,
           Queue_Capacity => 3);

      Queue : Instance.Queue_Type;
      Ptr   : Integer_Access;

   begin
      Ptr := Element_1'Access;
      Instance.Append (Queue, Ptr);
      Assert (Ptr = null, "Ptr not null after first Append");

      Ptr := Element_2'Access;
      Instance.Append (Queue, Ptr);
      Assert (Ptr = null, "Ptr not null after second Append");

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_1'Access, "Wrong first value popped from queue");

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_2'Access, "Wrong second value popped from queue");
   end Test_FIFO_Two;

   --------------------
   -- Test_FIFO_Full --
   --------------------

   --  This test checks that given a queue with capacity 3 and a full queue,
   --  items are popped off in FIFO order.

   procedure Test_FIFO_Full (T : in out Test) is
      package Instance is new
        LibSAP.Pointer_Queues
          (Element_Type   => Integer,
           Element_Access => Integer_Access,
           Queue_Capacity => 3);

      Queue : Instance.Queue_Type;
      Ptr   : Integer_Access;

   begin
      Ptr := Element_1'Access;
      Instance.Append (Queue, Ptr);
      Assert (Ptr = null, "Ptr not null after first Append");

      Ptr := Element_2'Access;
      Instance.Append (Queue, Ptr);
      Assert (Ptr = null, "Ptr not null after second Append");

      Ptr := Element_3'Access;
      Instance.Append (Queue, Ptr);
      Assert (Ptr = null, "Ptr not null after third Append");

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_1'Access, "Wrong first value popped from queue");

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_2'Access, "Wrong second value popped from queue");

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_3'Access, "Wrong third value popped from queue");
   end Test_FIFO_Full;

   ---------------------
   -- Test_Wraparound --
   ---------------------

   --  This test keeps popping and appending one item from a full queue
   --  several times to check that the FIFO behaviour is correct when the
   --  ring buffer wraps around.

   procedure Test_Wraparound (T : in out Test) is
      package Instance is new
        LibSAP.Pointer_Queues
          (Element_Type   => Integer,
           Element_Access => Integer_Access,
           Queue_Capacity => 3);

      Queue : Instance.Queue_Type;
      Ptr   : Integer_Access;

   begin
      --  Fill the queue

      Ptr := Element_1'Access;
      Instance.Append (Queue, Ptr);
      Ptr := Element_2'Access;
      Instance.Append (Queue, Ptr);
      Ptr := Element_3'Access;
      Instance.Append (Queue, Ptr);

      --  Pop and re-append several times

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_1'Access, "Wrong item after pop 1");
      Instance.Append (Queue, Ptr);

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_2'Access, "Wrong item after pop 2");
      Instance.Append (Queue, Ptr);

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_3'Access, "Wrong item after pop 3");
      Instance.Append (Queue, Ptr);

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_1'Access, "Wrong item after pop 4");
      Instance.Append (Queue, Ptr);

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_2'Access, "Wrong item after pop 5");
      Instance.Append (Queue, Ptr);

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_3'Access, "Wrong item after pop 6");
      Instance.Append (Queue, Ptr);

      --  Empty the queue

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_1'Access, "Wrong item after pop 7");

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_2'Access, "Wrong item after pop 8");

      Instance.Pop_Front (Queue, Ptr);
      Assert (Ptr = Element_3'Access, "Wrong item after pop 9");
   end Test_Wraparound;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Test_Caller.Create ("[Pointer_Queues] Test_One", Test_One'Access));
      Ret.Add_Test
        (Test_Caller.Create
           ("[Pointer_Queues] Test_FIFO_Two", Test_FIFO_Two'Access));
      Ret.Add_Test
        (Test_Caller.Create
           ("[Pointer_Queues] Test_FIFO_Full", Test_FIFO_Full'Access));
      Ret.Add_Test
        (Test_Caller.Create
           ("[Pointer_Queues] Test_Wraparound", Test_Wraparound'Access));
      return Ret;
   end Suite;

end LibSAP.Pointer_Queues_Tests;
