package myte.eval

/**
 * Find the next free index for the current index. If it exists, the next index is
 * removed from freeInds and the current index is added.
 * 
 * @param i the current index to find the next index for
 * @param freeInds the set of all free indices to choose from
 * @return the smallest index in freeInds greater than i, or null if none exists
 */
private fun nextInd(i: Int, freeInds: MutableSet<Int>): Int? {
    // Find the smallest index in freeInds larger than i
    var nextLargest: Int? = null
    for (idx in freeInds) {
        if (idx > i && (nextLargest == null || idx < nextLargest)) {
            nextLargest = idx
        }
    }

    // If found, remove the next index is no longer free and the current index becomes free
    if (nextLargest != null) {
        // Only add the free index if it not the sentinel value
        if (i != -1) {
            freeInds += i
        }

        freeInds -= nextLargest
    }

    return nextLargest
}

/**
 * Find all matches between two lists, given a particular matching function. All possible
 * permutations will be searched, and a mapping from the first to the second list will be returned.
 *
 * @param l1 the first list of items, of any particular type. Must have same size as l2
 * @param l2 the second list of items, of any particular type. Must have same size as l1
 * @param isMatch a function that takes in an item from l1 and l2, and returns whether they match
 * @return a mapping from l1 to l2, or null if no mapping exists. This mapping will be a list of
 *         ints of the same size as l1 and l2, where entry map[i] = j means l1[i] maps to l2[j].
 */
fun <A, B> findMatches(l1: List<A>, l2: List<B>, isMatch: (A, B) -> Boolean): List<Int>? {
    // Error if lists have different sizes
    if (l1.size != l2.size) {
        throw Exception("findMatches expects two lists of the same size, received " +
                "${l1.size} and ${l2.size}")
    }

    // If both lists are empty, successfully return empty mapping
    if (l1.size == 0) {
        return listOf()
    }

    val lastInd = l1.size - 1

    // A list mapping indices from l1 to l2. An entry indsMap[i] = j means l1[i] maps to l2[j]
    val indsMap: MutableList<Int> = mutableListOf()
    for (i in 0..lastInd) {
        indsMap += -1
    }

    // The set of all indicies in l2 that have not yet been mapped up to the current index
    val freeInds = (0..lastInd).toMutableSet()

    // i is the current index. As an invariant, all indices before i have been successfully matched.
    var i = 0

    while (true) {
        // Find the next mapping for the current index
        val next = nextInd(indsMap[i], freeInds)
        if (next == null) {
            // If we are at the beginning and are at the largest mapping, we have exhausted all
            // possibilities and the search should terminate.
            if (i == 0) {
                return null
            }

            // Otherwise, free the current mapping and set it to the sentinel value, and go back
            // to the previous index.
            freeInds += indsMap[i]
            indsMap[i] = -1
            i--
        } else {
            // Tru the next mapping for the current index
            indsMap[i] = next
            if (isMatch(l1[i], l2[indsMap[i]])) {
                // If successful and we are at the last index, (and we know all previous indicies
                // have been successfully matched), then we have found a mapping.
                if (i == lastInd) {
                    return indsMap
                }

                // If not at last index, increment current index since it was successfully matched
                i++
            }
        }
    }    
}
