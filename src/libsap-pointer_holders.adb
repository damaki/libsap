--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  SPARK mode is off in this body since atomic pointers are not compatible
--  with SPARK. Specifically, moves are not allowed for objects that are
--  effectively volatile (atomic types in this case. See SPARK RM 3.10 (16)).
--
--  This package therefore wraps such operations while still preserving
--  SPARK ownership semantics in the package spec. It uses the "atomic" crate
--  for the atomic operations.

with Atomic;
with Atomic.Basic_Operations;

package body LibSAP.Pointer_Holders
  with SPARK_Mode => Off
is

   package Atomic_Pointers is new Atomic.Basic_Operations (Element_Access);

   type Element_Access_Array is
     array (Element_ID) of aliased Atomic_Pointers.Instance;

   Pool : Element_Access_Array := [others => Atomic_Pointers.Init (null)];

   -------------------
   -- Check_Is_Null --
   -------------------

   procedure Check_Is_Null (ID : Element_ID; Is_Null : out Boolean) is
      Element : constant Element_Access :=
        Atomic_Pointers.Load (Pool (ID), Atomic.Relaxed);
   begin
      Is_Null := Element = null;
   end Check_Is_Null;

   --------------
   -- Exchange --
   --------------

   procedure Exchange (Element : in out Element_Access) is
   begin
      Element :=
        Atomic_Pointers.Exchange
          (Pool (Element.all.ID), Element, Atomic.Acq_Rel);
   end Exchange;

   --------------
   -- Retrieve --
   --------------

   procedure Retrieve (ID : Element_ID; Element : out Element_Access) is
   begin
      Element := Atomic_Pointers.Exchange (Pool (ID), null, Atomic.Acq_Rel);
   end Retrieve;

end LibSAP.Pointer_Holders;
