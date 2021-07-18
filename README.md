# CRDTs in Elm

Elm implementations of some of the most common CRDT data structures.

## Introduction
CRDTs are data structures that supports concurrent collaboration without
conflicts. Concurrent updates to a CRDT are guaranteed to always converge
to a stable value, which makes it possible to automatically merge concurrent
updates without any manual conflict resolution. CRDTs are a great option
when you need to do synchronization or P2P editing.

There are two ways to interact with a CRDT. Either by merging the complete
state, or by applying atomic operations that updates the CRDT incrementally.
Either way, the updates are guaranteed to always be commutative, associative
and (typically) idempotent.

This library currently implements two basic abstract data types: counters and sets.
The data types come in several different flavours depending on the specific
use case needed (and space/time complexity of the underlying data struecture).

## Counters
- **GCounter**: A counter that can only increment.
- **PNCounter**: A counter that can both increment and decrement.

## Sets
- **GSet**: A set that can be only added to.
- **TwoPSet**: A set that supports both insert and remove (with concurrent removals taking precendence over inserts).
- **ORSet**: A set that supports both insert and remove (with concurrent inserts taking precendence over removal).
- **LWWElementSet**: A set that supports both insert and remove (with the latest concurrent operation taking precendence).
