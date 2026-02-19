--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System.Atomic_Operations.Exchange;

--  SPARK mode is off in this body since System.Atomic_Operations.Exchange
--  with pointers is not compatible with SPARK. Specifically, moves are not
--  allowed for objects that are effectively volatile (atomic types in this
--  case. See SPARK RM 3.10 (16).
--
--  This package therefore wraps such operations while still preserving
--  SPARK ownership semantics in the package spec.

package body LibSAP.Pointer_Holders
  with SPARK_Mode => Off
is

   type Atomic_Element_Access is access all Element_Type with Atomic;

   type Atomic_Element_Access_Array is
     array (Element_ID) of aliased Atomic_Element_Access
   with Atomic_Components;

   package Atomic_Exchange_Operations is new
     System.Atomic_Operations.Exchange (Atomic_Element_Access);

   Pool : Atomic_Element_Access_Array := [others => null];

   --------------
   -- Exchange --
   --------------

   procedure Exchange (Element : in out Element_Access) is
      Temp : Atomic_Element_Access := Atomic_Element_Access (Element);
   begin
      Temp :=
        Atomic_Exchange_Operations.Atomic_Exchange (Pool (Temp.all.ID), Temp);

      Element := Element_Access (Temp);
   end Exchange;

   --------------
   -- Retrieve --
   --------------

   procedure Retrieve (ID : Element_ID; Element : out Element_Access) is
      Temp : Atomic_Element_Access;
   begin
      Temp := Atomic_Exchange_Operations.Atomic_Exchange (Pool (ID), null);
      Element := Element_Access (Temp);
   end Retrieve;

end LibSAP.Pointer_Holders;
