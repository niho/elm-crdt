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

This library currently implements three basic abstract data types: counters,
sets and sequences. The data types come in several different flavours depending
on the specific use case needed (and space/time complexity of the underlying
data structure).

## Counters
- **GCounter**: A counter that can only increment.
- **PNCounter**: A counter that can be both incremented and decremented.

## Sets
- **GSet**: A set that can be only added to.
- **TwoPSet**: A set that supports both insert and remove (with concurrent removals taking precedence over inserts).
- **ORSet**: A set that supports both insert and remove (with concurrent inserts taking precedence over removal).
- **LWWElementSet**: A set that supports both insert and remove (with the latest concurrent operation taking precedence).

## Sequences
- **RGA**: Replicated Growable Array, a good general purpose sequence type.

## License

This library is licensed under the **GNU General Public Licence v3 (GPLv3)**.

Copyright (C) 2021 Niklas Holmgren Holding AB.
