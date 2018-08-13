package myte.ir

import myte.shared.*

/**
 * A collection of all outstanding constraints.
 */
class ConstraintGraph {
    // A map from constaints to the set of type variables that must be resolved before the
    // constraint can be resolved.
    val constraintDepends: MutableMap<Constraint, MutableSet<OpenTypeVariable>> =
            mutableMapOf()

    // A map from unresolved type variables to the constraints that are waiting for the type
    // variable's resolution.
    private val typeVarConstraints: MutableMap<OpenTypeVariable, MutableSet<Constraint>> =
            mutableMapOf()

    /**
     * Add a constraint to the set of all constraints.
     *
     * @param constraint the new constraint
     * @param dependencies a set of all the type variables that must be resolved before the
     *        constraint can be resolved
     */
    fun addConstraint(constraint: Constraint, dependencies: Set<OpenTypeVariable>) {
        constraintDepends[constraint] = dependencies.toMutableSet()

        // For each dependency, add the constraint to the set of constraints that depend on
        // the given type variable.
        dependencies.forEach { dependency ->
            val existingConstraints = typeVarConstraints[dependency]
            if (existingConstraints == null) {
                typeVarConstraints[dependency] = mutableSetOf(constraint)
            } else {
                existingConstraints.add(constraint)
            }
        }
    }

    /**
     * Resolve an unresolved type variable. All constraints for which this type variable was the
     * last dependency will be resolved.
     *
     * @param typeVar the unresolved type variable that will be resolved
     */
    fun resolveVariable(typeVar: OpenTypeVariable) {
        val constraints = typeVarConstraints[typeVar] ?: mutableSetOf()

        for (constraint in constraints) {
            val dependencies = constraintDepends[constraint]!!
            dependencies.remove(typeVar)

            // If the type variable was the last dependency for a constraint, resolve it
            if (dependencies.isEmpty()) {
                constraintDepends.remove(constraint)
                constraint.resolve()
            }
        }

        typeVarConstraints.remove(typeVar)
    }

    /**
     * Replace all occurences of one type variable with another. This will transfer constraints
     * from the old type variable to the new type variable.
     *
     * @param oldVar the old unresolved type variable
     * @param newVar the new unresolved type variable
     */
    fun moveConstraints(oldVar: OpenTypeVariable, newVar: OpenTypeVariable) {
        val constraints = typeVarConstraints[oldVar] ?: mutableSetOf()

        // Switch all occurrences of oldVar with newVar in the constraint dependencies map
        for (constraint in constraints) {
            val dependencies = constraintDepends[constraint]!!
            dependencies.remove(oldVar)
            dependencies.add(newVar)
        }

        typeVarConstraints.remove(oldVar)

        // Add the constraints on oldVar to the set of constraints that depend on newVar
        val existingNewTypeVarConstraints = typeVarConstraints[newVar]
        if (existingNewTypeVarConstraints == null) {
            typeVarConstraints[newVar] = constraints
        } else {
            existingNewTypeVarConstraints.addAll(constraints)
        }
    }
}
