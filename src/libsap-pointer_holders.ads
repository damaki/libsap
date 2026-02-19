--
--  Copyright 2026 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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
   type Element_Access is access all Element_Type;
package LibSAP.Pointer_Holders with
    Preelaborate,
    SPARK_Mode,
    Abstract_State => (Pointer_Pool with Synchronous),
    Always_Terminates
is

   procedure Check_Is_Null (ID : Element_ID; Is_Null : out Boolean)
   with Inline, Global => (Input => Pointer_Pool);
   --  Query if a pointer in a slot is currently null.
   --
   --  Note that even if this procedure reports Is_Null is True or False,
   --  it is possible for another task to jump in and Exchange or Retrieve
   --  the pointer.

   procedure Exchange (Element : in out Element_Access)
   with Inline, Global => (In_Out => Pointer_Pool), Pre => Element /= null;
   --  Exchange a pointer in the slot determined by Element.all.ID.
   --
   --  This exchanges Element with the pool already stored at that position.
   --  If the pool did not contain an element at that position, then Element
   --  is set to null.

   procedure Retrieve (ID : Element_ID; Element : out Element_Access)
   with
     Inline,
     Global => (In_Out => Pointer_Pool),
     Post   => (if Element /= null then Element.all.ID = ID);
   --  Retieve the pointer from the pool stored in the slot at ID.
   --
   --  Element is set to the pointer that was stored in the pool in the slot
   --  at ID (possibly null).
   --
   --  The slot in the pool is set to null.

end LibSAP.Pointer_Holders;
