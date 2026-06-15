--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  @private
--
--  This package encapsulates an array of pointers, where each position in the
--  array only holds pointers whose ID matches the index of the array.
--
--  Initially, each pointer in the array is null. Pointers can be moved into
--  the array via Exchange, and moved out of the array via Retrieve.
--
--  The package uses atomics internally, so calls to Exchange and Retrieve are
--  safe against data races.

private generic
   type Element_ID is range <>;
   type Element_Type (ID : Element_ID) is limited private;
   type Element_Access is access Element_Type;
package LibSAP.Pointer_Holders with
    SPARK_Mode,
    Abstract_State => (Pointer_Pool with Synchronous),
    Initializes    => Pointer_Pool
is

   procedure Check_Is_Null (ID : Element_ID; Is_Null : out Boolean)
   with Inline, Always_Terminates, Global => (Input => Pointer_Pool);
   --  Query if a pointer in a slot is currently null.
   --
   --  Note that even if this procedure reports Is_Null is True or False,
   --  it is possible for another task to jump in and Exchange or Retrieve
   --  the pointer between the calls to Check_Is_Null and Exchange/Retrieve.
   --
   --  This uses the relaxed memory order.

   procedure Store (Element : in out Element_Access)
   with
     Inline,
     Always_Terminates,
     Global => (In_Out => Pointer_Pool),
     Pre    => Element /= null,
     Post   => Element = null;
   --  Store a pointer in the slot determined by Element.all.ID.
   --
   --  Warning: Storing a pointer in a slot that already contains a non-null
   --  pointer may cause a memory leak since the existing slot pointer will be
   --  overwritten. This procedure should only be used when it can be
   --  guaranteed that the slot is null, such as after this package is
   --  elaborated.
   --
   --  This uses release memory order.

   procedure Exchange (Element : in out Element_Access)
   with
     Inline,
     Always_Terminates => False,
     Global            => (In_Out => Pointer_Pool),
     Pre               => Element /= null;
   --  Exchange a pointer in the slot determined by Element.all.ID.
   --
   --  This exchanges Element with the pool already stored at that position.
   --  If the pool did not contain an element at that position, then Element
   --  is set to null.
   --
   --  Note that this procedure is not guaranteed to terminate due to the use
   --  of atomic exchange intrinsics internally, which may use retry loops on
   --  some architecture (e.g. Armv7-M which uses a LDREX/STREX loop to do
   --  the exchange, which may experience a livelock or infinite retry loops).
   --
   --  This uses acquire-release memory order.

   procedure Retrieve (ID : Element_ID; Element : out Element_Access)
   with
     Inline,
     Always_Terminates => False,
     Global            => (In_Out => Pointer_Pool),
     Post              => (if Element /= null then Element.all.ID = ID);
   --  Retieve the pointer from the pool stored in the slot at ID.
   --
   --  Element is set to the pointer that was stored in the pool in the slot
   --  at ID (possibly null).
   --
   --  The slot in the pool is set to null.
   --
   --  Note that this procedure is not guaranteed to terminate due to the use
   --  of atomic exchange intrinsics internally, which may use retry loops on
   --  some architecture (e.g. Armv7-M which uses a LDREX/STREX loop to do
   --  the exchange, which may experience a livelock or infinite retry loops).
   --
   --  This uses acquire-release memory order.

end LibSAP.Pointer_Holders;
