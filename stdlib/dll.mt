package std::dll

import std::iterator::{Iterable, Iterator}
import std::option::{Option, Some, None}

type DLLNode<a> = DLLNode {
    item: a,
    mut next: Option<DLLNode<a>>,
    mut prev: Option<DLLNode<a>>,
}

type DLL<a> = DLL {
    mut start: Option<DLLNode<a>>,
    mut end: Option<DLLNode<a>>,
    mut size: int,
}

type DLLIterator<a> = DLLIterator { mut curr: Option<DLLNode<a>> }

implement DLL<a> {
    static def new(): DLL<a> = DLL { start: None, end: None, size: 0 }

    def pushFront(x: a) {
        match this.start
        // If list is nonempty add new start node
        | Some(start) -> {
            let node = DLLNode { item: x, next: this.start, prev: None }
            start.prev = Some(node)
            this.start = Some(node)
        }
        // If list is empty, insert node as start and end
        | None -> {
            let node = DLLNode { item: x, next: None, prev: None }
            this.start = Some(node)
            this.end = Some(node)
        }

        this.size = this.size + 1
    }

    def pushBack(x: a) {
        match this.end
        // If list is nonempty add new end node
        | Some(end) -> {
            let node = DLLNode { item: x, next: None, prev: this.end }
            end.next = Some(node)
            this.end = Some(node)
        }
        // If list is empty, insert node as start and end
        | None -> {
            let node = DLLNode { item: x, next: None, prev: None }
            this.start = Some(node)
            this.end = Some(node)
        }

        this.size = this.size + 1
    }

    def popFront() {
        return match this.start
        // If removing the last element of the list, set start and end to None
        | Some(DLLNode { item: item, next: None }) -> {
            this.start = None
            this.end = None
            this.size = this.size - 1

            Some(item)
        }
        // If removing elemnt from nonempty list, pop off the front
        | Some(DLLNode { item: item, next: Some(next) }) -> {
            next.prev = None
            this.start = Some(next)
            this.size = this.size - 1

            Some(item)
        }
        | None -> None
    }

    def popBack() {
        return match this.end
        // If removing the last element of the list, set start and end to None
        | Some(DLLNode { item: item, prev: None }) -> {
            this.start = None
            this.end = None
            this.size = this.size - 1

            Some(item)
        }
        | Some(DLLNode { item: item, prev: Some(prev) }) -> {
            prev.next = None
            this.end = Some(prev)
            this.size = this.size - 1

            Some(item)
        }
        | None -> None
    }

    def front() = match this
        | DLL { start: None } -> None
        | DLL { start: Some(DLLNode { item: item })} -> Some(item)

    def back() = match this
        | DLL { end: None } -> None
        | DLL { end: Some(DLLNode { item: item })} -> Some(item)

    def get(i) {
        if (i < 0 || i >= this.size) {
            return None
        }

        let iter: Iterator<a> = this.iterator()

        for (let curr = 0, curr < i, curr = curr + 1) {
            iter.next()
        }

        return iter.next()
    }

    def insert(i: int, x: a) {
        if (i < 0 || i > this.size) {
            return ()
        }

        // If inserting at beginning or end, delegate to pushFront or pushBack
        if (i == 0) {
            this.pushFront(x)
            return ()
        } else if (i == this.size) {
            this.pushBack(x)
            return ()
        }

        // Find the DLLNode right after where this item should be inserted
        let curr = this.start
        for (let idx = 0, idx < i, idx = idx + 1) {
            match curr
            | Some(DLLNode { next: next }) -> curr = next
            | None -> {}
        }

        // Create node and set pointers of previous and next node to point to new node
        match curr
        | Some(next) -> {
            let node = DLLNode { item: x, next: curr, prev: next.prev }
            next.prev = Some(node)

            match node.prev
            | Some(prev) -> prev.next = Some(node)
            // This should never happen, since pushFront is special cased
            | None -> {}
        }
        // This should never happen, since pushBack is special cased
        | None -> {}

        this.size = this.size + 1
    }

    def removeAt(i: int) {
        if (i < 0 || i >= this.size) {
            return ()
        }

        // If removing from beginning or end, delegate to popFront or popBack
        if (i == 0) {
            this.popFront()
            return ()
        } else if (i == this.size - 1) {
            this.popBack()
            return ()
        }

        // Find the DLLNode to be deleted
        let curr = this.start
        for (let idx = 0, idx < i, idx = idx + 1) {
            match curr
            | Some(DLLNode { next: next }) -> curr = next
            | None -> {}
        }

        // Set pointers of previous and next node to point to each other, removing node
        match curr
        | Some(DLLNode { next: next, prev: prev }) -> (match (next, prev)
            | (Some(nextNode), Some(prevNode)) -> {
                nextNode.prev = prev
                prevNode.next = next
            }
            // This should never happen, as deleting from edges of list is already handled
            | _ -> {})
        // This should never happen, since node must be within bounds of list
        | None -> {}

        this.size = this.size -1 1
    }

    def remove(x: a) {
        let iter: Iterator<a> = this.iterator()

        // Search for the first occurrence of the element in the list by iterating through it
        let idx = None
        for (let i = 0, i < this.size, i = i + 1) {
            match iter.next()
            // If the element was found, delegate to removeAt at the element's index
            | Some(curr) when curr == x -> {
                this.removeAt(i)
                return ()
            }
            | _ -> {}
        }
    }

    def printNode(node: DLLNode<a>) {
        print("Node with item: ")
        print(node.item.toString())

        print(", and prev: ")
        match node.prev
        | Some(prev) -> print(prev.item.toString())
        | None -> print("None")

        print(", and next: ")
        match node.next
        | Some(next) -> print(next.item.toString())
        | None -> print("None")

        println("")
    }

    def printList() {
        // Print the size of the list
        print("List of size: ")
        print(this.size.toString())

        // Print the start node
        print(", with start: ")
        match this.start
        | Some(DLLNode{ item: item }) -> print(item.toString())
        | None -> print("None")

        // Print the end node
        print(", and end: ")
        match this.end
        | Some(DLLNode{ item: item }) -> print(item.toString())
        | None -> print("None")
        println("")

        // Print a string representation of every node in the DLL
        let currNode = this.start
        while (true) {
            match currNode
            | Some(node) -> {
                this.printNode(node)
                currNode = node.next
            }
            | None -> break
        }

        println("End of list")
    }
}

implement DLL<a> extends Iterable<a> {
    def iterator() = DLLIterator { curr: this.start }
}

implement DLLIterator<a> extends Iterator<a> {
    def next() = match this.curr
        | None -> None
        | Some(DLLNode { item: item, next: next }) -> {
            this.curr = next
            Some(item)
        }
}
