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
--  SPARK ownership semantics in the package spec.

with System;

package body LibSAP.Pointer_Holders
  with SPARK_Mode => Off
is

   --  Note that System.Atomic_Operations is not available on most embedded
   --  targets, so we instead choose to bind GCC's __atomic_exchange_N
   --  intrinsic directly.
   --
   --  This might still be unavailable on some targets (e.g. Armv6-M), but
   --  it's better than nothing until the atomic crate has atomic pointer
   --  support.

   type Mem_Order is (Relaxed, Consume, Acquire, Release, Acq_Rel, Seq_Cst);

   for Mem_Order use
     (Relaxed => 0,
      Consume => 1,
      Acquire => 2,
      Release => 3,
      Acq_Rel => 4,
      Seq_Cst => 5);

   function Atomic_Exchange
     (Ptr : System.Address; Val : Element_Access; Order : Mem_Order := Seq_Cst)
      return Element_Access
   is

      --  Handle different pointer sizes

      --  Note that since we have different bindings to support any possible
      --  pointer size, GNAT will warn about the binding type mismatch on
      --  parameter 'Val' and the result. This is to be expected on some of
      --  the bindings, since the pointer size will not match all bindings.
      --
      --  However, only the correct binding is actually called.

      pragma Warnings (Off);

      function Intrinsic8
        (Ptr : System.Address; Val : Element_Access; Model : Integer)
         return Element_Access;
      pragma Import (Intrinsic, Intrinsic8, "__atomic_exchange_1");

      function Intrinsic16
        (Ptr : System.Address; Val : Element_Access; Model : Integer)
         return Element_Access;
      pragma Import (Intrinsic, Intrinsic16, "__atomic_exchange_2");

      function Intrinsic32
        (Ptr : System.Address; Val : Element_Access; Model : Integer)
         return Element_Access;
      pragma Import (Intrinsic, Intrinsic32, "__atomic_exchange_4");

      function Intrinsic64
        (Ptr : System.Address; Val : Element_Access; Model : Integer)
         return Element_Access;
      pragma Import (Intrinsic, Intrinsic64, "__atomic_exchange_8");

      pragma Warnings (On);

   begin
      case Element_Access'Object_Size is
         when 8      =>
            return Intrinsic8 (Ptr, Val, Order'Enum_Rep);

         when 16     =>
            return Intrinsic16 (Ptr, Val, Order'Enum_Rep);

         when 32     =>
            return Intrinsic32 (Ptr, Val, Order'Enum_Rep);

         when 64     =>
            return Intrinsic64 (Ptr, Val, Order'Enum_Rep);

         when others =>
            raise Program_Error;
      end case;
   end Atomic_Exchange;

   type Element_Access_Array is array (Element_ID) of aliased Element_Access
   with Atomic_Components;

   Pool : Element_Access_Array := [others => null];

   -------------------
   -- Check_Is_Null --
   -------------------

   procedure Check_Is_Null (ID : Element_ID; Is_Null : out Boolean) is
   begin
      Is_Null := Pool (ID) = null;
   end Check_Is_Null;

   --------------
   -- Exchange --
   --------------

   procedure Exchange (Element : in out Element_Access) is
   begin
      Element :=
        Atomic_Exchange (Pool (Element.all.ID)'Address, Element, Seq_Cst);
   end Exchange;

   --------------
   -- Retrieve --
   --------------

   procedure Retrieve (ID : Element_ID; Element : out Element_Access) is
   begin
      Element := Atomic_Exchange (Pool (ID)'Address, null, Seq_Cst);
   end Retrieve;

end LibSAP.Pointer_Holders;
