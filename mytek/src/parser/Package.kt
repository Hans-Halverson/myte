package myte.parser

import myte.parser.ast.*
import myte.shared.*

class PackageTreeNode(
    val name: String,
    val pack: Package?,
    val children: MutableMap<String, PackageTreeNode> = mutableMapOf()
) {
    /**
     * Get the subpackage with the given name if one exists, or return null if one doesn't exist.
     */
    fun getSubPackage(packageParts: List<String>): Package? {
        if (packageParts.isEmpty()) {
            return null
        } else if (packageParts.size == 1) {
            return children[packageParts[0]]?.pack
        } else {
            return children[packageParts[0]]?.getSubPackage(packageParts.drop(1))
        }
    }

    /**
     * Create the subpackage with the given name if one exists, creating empty nodes on the way
     * down to the package node.
     */
    fun createSubPackage(packageParts: List<String>): Package {
        // If are at the last part of the package name, create new package and package node
        if (packageParts.size == 1) {
            val subPackage = Package(packageParts[0])
            children[subPackage.name] = subPackage.packageTreeNode
            return subPackage
        // Otherwise descend one package down through the package tree
        } else {
            val packagePart = packageParts[0]
            val subPackageNode = children[packagePart]

            // If the subpackage with the current name exists, delegate creation to it
            if (subPackageNode != null) {
                return subPackageNode.createSubPackage(packageParts.drop(1))
            // Otherwise create an empty package node and delegate creation to it
            } else {
                val newSubPackageNode = PackageTreeNode(packagePart, null)
                children[packagePart] = newSubPackageNode
                return newSubPackageNode.createSubPackage(packageParts.drop(1))
            }
        }
    }
}

class Package(
    val name: String,
    val scope: Scope = Scope(),
    val typeDefs: MutableList<TypeDefinitionStatement> = mutableListOf(),
    val typeImpls: MutableList<TypeImplementationStatement> = mutableListOf(),
    val traitDefs: MutableList<TraitDefinitionStatement> = mutableListOf(),
    val statements: MutableList<Statement> = mutableListOf()
) {
    val packageTreeNode: PackageTreeNode = PackageTreeNode(name, this)
}

fun formatPackageName(parts: List<String>): String = parts.joinToString("::")

class ImportContext(
    val rootPackageNode: PackageTreeNode,
    val imports: MutableList<Triple<String, List<String>, Location>> = mutableListOf()
) {
    fun findImportForAlias(aliasToFind: String): List<String>? {
        for ((alias, import, _) in imports) {
            if (alias == aliasToFind) {
                return import
            }
        }

        return null
    }

    fun copyForRepl(): ImportContext {
        return ImportContext(rootPackageNode, imports.toMutableList())
    }
}
