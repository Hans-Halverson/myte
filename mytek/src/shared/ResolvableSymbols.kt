package myte.shared

import myte.ir.IRConversionException
import myte.parser.formatPackageName
import myte.parser.ImportContext
import myte.parser.Package

/**
 * A symbol that can be resolved into an identifer during conversion.
 */
abstract class ResolvableSymbol() {
    /**
     * Resolve this symbol into an identifier, throwing an IRConversionException if it cannot be
     * resolved.
     */
    abstract fun resolve(): Identifier

    /**
     * The full name of this symbol (with scope prefixed)
     */
    abstract fun symbol(): String
}

/**
 * An identifier that has already been resolved.
 *
 * @property ident the identifier the symbol has been resolved to
 */
class ResolvedIdentifier(val ident: Identifier) : ResolvableSymbol() {
    override fun resolve(): Identifier = ident
    override fun symbol(): String = ident.name
}

/**
 * A variable typed symbol that needs to be resolved. This can be resolved to a function, variable,
 * or type constructor.
 *
 * @property name the name of the variable
 * @property scopePrefixes a list of scope names that prefix this variable's name
 * @property location the location of the beginning of the scope prefixed name
 * @property scope the scope that the variable needs to be resolved in
 * @property symbolTable the symbol table for this variable
 * @property importContext the import context that the variable needs to be resolved in
 */
class VariableSymbolPendingResolution(
    val name: String,
    val scopePrefixes: List<String>,
    val location: Location,
    val scope: Scope,
    val symbolTable: SymbolTable,
    val importContext: ImportContext
) : ResolvableSymbol() {
    override fun resolve(): Identifier {
        // If there are no scope prefixes, either variable is in this package's scope or it is an
        // imported function or variable from another scope.
        if (scopePrefixes.isEmpty()) {
            // Lookup variable in current lexical scope
            val ident = scope.lookupVariable(name)
            if (ident != null) {
                return ident
            }

            // Otherwise try to find import with the same alias
            val packageAndVariable = findPackageForAlias(name, importContext)
            if (packageAndVariable == null) {
                throw IRConversionException("Identifier ${symbol()} could not be resolved",
                        location)
            }
            
            val (importedPackage, importedVariable) = packageAndVariable
            val variableIdent = importedPackage.scope.lookupVariable(importedVariable)
            if (variableIdent == null) {
                throw IRConversionException("Identifier ${symbol()} could not be resolved",
                        location)
            }

            return variableIdent
        // If there are scope prefixes, find correct package and lookup in it
        } else {
            // If there is one scope prefix, this could be a static method in the current scope,
            // or a static method
            if (scopePrefixes.size == 1) {
                // First check whether this is a static method where the type is in the current
                // lexical scope.
                var typeIdent = scope.lookupType(scopePrefixes[0])
                if (typeIdent != null) {
                    val identInfo = symbolTable.getInfo(typeIdent)!!

                    // If a trait, return concrete static method with the same name if one exists
                    if (identInfo.idClass == IdentifierClass.TRAIT) {
                        val staticMethod = identInfo.traitSig.staticMethods[name]
                        if (staticMethod != null) {
                            return staticMethod
                        }
                    // If an ADT, return static method with the same name if one exists
                    } else if (identInfo.idClass == IdentifierClass.ALGEBRAIC_DATA_TYPE) {
                        val staticMethod = identInfo.adtSig.staticMethods[name]
                        if (staticMethod != null) {
                            return staticMethod
                        }
                    }
                }

                // Then check whether this is a static method where the type is an imported alias
                val importParts = importContext.findImportForAlias(scopePrefixes[0])
                if (importParts != null) {
                    val packageParts = importParts.dropLast(1)
                    val typeName = importParts[importParts.size - 1]

                    // First find package containing trait or type
                    val subPackage = importContext.rootPackageNode.getSubPackage(packageParts)
                    if (subPackage != null) {
                        // Then find trait or type within package
                        typeIdent = subPackage.scope.lookupType(typeName)
                        if (typeIdent != null) {
                            val identInfo = symbolTable.getInfo(typeIdent)!!

                            // If a trait, return concrete static method with the same name if found
                            if (identInfo.idClass == IdentifierClass.TRAIT) {
                                val staticMethod = identInfo.traitSig.staticMethods[name]
                                if (staticMethod != null) {
                                    return staticMethod
                                }
                            // If an ADT, return static method with the same name if one exists
                            } else if (identInfo.idClass == IdentifierClass.ALGEBRAIC_DATA_TYPE) {
                                val staticMethod = identInfo.adtSig.staticMethods[name]
                                if (staticMethod != null) {
                                    return staticMethod
                                }
                            }
                        }
                    }
                }
            }

            // Next, try to find imported trait or type with the same name, and find static method..
            // First resolve type alias from first scope prefix.
            val importParts = importContext.findImportForAlias(scopePrefixes[0])
            if (importParts != null) {
                // All scope parts but last are subpackages, last scope part must be the type
                val restOfImportParts = scopePrefixes.drop(1).dropLast(1)
                val typeName = scopePrefixes[scopePrefixes.size - 1]

                // Find package containing type, by looking up package from alias and rest of scope
                val subPackage = importContext.rootPackageNode.getSubPackage(importParts +
                        restOfImportParts)

                if (subPackage != null) {
                    // Then look up trait or type with that name in the package
                    val typeIdent = subPackage.scope.lookupType(typeName)
                    if (typeIdent != null) {
                        val identInfo = symbolTable.getInfo(typeIdent)!!
                        // If a trait, return concrete static method with the same name if found
                        if (identInfo.idClass == IdentifierClass.TRAIT) {
                            val staticMethod = identInfo.traitSig.staticMethods[name]
                            if (staticMethod != null) {
                                return staticMethod
                            }
                        // If an ADT, return static method with the same name if one exists
                        } else if (identInfo.idClass == IdentifierClass.ALGEBRAIC_DATA_TYPE) {
                            val staticMethod = identInfo.adtSig.staticMethods[name]
                            if (staticMethod != null) {
                                return staticMethod
                            }
                        }
                    }
                }
            }

            // Otherwise find the package and lookup in it
            val importedPackage = findPackage(scopePrefixes, importContext)
            if (importedPackage == null) {
                val fullReference = (scopePrefixes + name).joinToString("::")
                throw IRConversionException("Unresolved reference $fullReference", location)
            }

            val variableIdent = importedPackage.scope.lookupVariable(name)
            if (variableIdent == null) {
                val fullReference = (scopePrefixes + name).joinToString("::")
                throw IRConversionException("Unresolved reference: $fullReference", location)
            }

            return variableIdent
        }
    }

    override fun symbol(): String {
        if (scopePrefixes.isEmpty()) {
            return name
        } else {
            return "${scopePrefixes.joinToString("::")}::${name}"
        }
    }
}

/**
 * A symbol in a pattern that needs to be resolved. This can be resolved to either a type
 * type constructor or a new variable.
 *
 * @property name the name of the variable
 * @property scopePrefixes a list of scope names that prefix this variable's name
 * @property location the location of the beginning of the scope prefixed name
 * @property scope the scope that the variable needs to be resolved in
 * @property symbolTable the symbol table for this variable
 * @property importContext the import context that the variable needs to be resolved in
 */
class PatternSymbolPendingResolution(
    val name: String,
    val scopePrefixes: List<String>,
    val location: Location,
    val canCreate: Boolean,
    val scope: Scope,
    val symbolTable: SymbolTable,
    val importContext: ImportContext
) : ResolvableSymbol() {
    override fun resolve(): Identifier {
        // If there are no scope prefixes, this is either a type constructor 1. Defined in this
        // package, 2. Directly imported, or 3. A new variable if the former are false.
        if (scopePrefixes.isEmpty()) {
            // First check whether this is a type constructor defined in this package
            val ident = scope.lookupVariable(name)
            if (ident != null) {
                if (symbolTable.getInfo(ident)?.idClass ==
                        IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
                    return ident
                }
            }

            // Next try to find an imported type constructor with this alias
            val packageAndVariable = findPackageForAlias(name, importContext)
            if (packageAndVariable != null) {
                val (importedPackage, importedVariable) = packageAndVariable
                val variableIdent = importedPackage.scope.lookupVariable(importedVariable)
                if (variableIdent != null) {
                    return variableIdent
                }
            }

            // Otherwise create a new variable and identifier if the canCreate flag is set
            if (canCreate) {
                val patternIdent = symbolTable.addSymbolInScope(scope, name,
                        IdentifierClass.VARIABLE, location, hashSetOf(), true)

                // Annotate this new identifier with a new type variable
                symbolTable.getInfo(patternIdent)?.type = OpenTypeVariable()

                return patternIdent
            } else {
                throw IRConversionException("No type constructor with name ${name} found", location)
            }
        // If there are scope prefixes, find correct package and lookup in it
        } else {
            val importedPackage = findPackage(scopePrefixes, importContext)
            if (importedPackage == null) {
                val fullReference = (scopePrefixes + name).joinToString("::")
                throw IRConversionException("Unresolved reference: $fullReference", location)
            }

            val variableIdent = importedPackage.scope.lookupVariable(name)
            if (variableIdent == null) {
                val fullReference = (scopePrefixes + name).joinToString("::")
                throw IRConversionException("Unresolved reference: $fullReference", location)
            }

            return variableIdent
        }
    }

    override fun symbol(): String = name
}

/**
 * A type symbol that needs to be resolved. This can be resolved to either an existing ADT or
 * to a new type parameter variable if the corresponding flag is set.
 *
 * @property name the name of the type
 * @property scopePrefixes a list of scope names that prefix this types's name
 * @property location the location of the beginning of the scope prefixed name
 * @property inFunctionDef whether in a function definition or not
 * @property scope the scope that the variable needs to be resolved in
 * @property symbolTable the symbol table for this variable
 * @property importContext the import context that the variable needs to be resolved in
 */
class TypeSymbolPendingResolution(
    val name: String,
    val scopePrefixes: List<String>,
    val location: Location,
    val inFunctionDef: Boolean,
    val scope: Scope,
    val symbolTable: SymbolTable,
    val importContext: ImportContext
) : ResolvableSymbol() {
    override fun resolve(): Identifier {
        // If there are no scope prefixes, this is either a type defined 1. In this package,
        // 2. Directly imported, or 3. A new type variable to create if inFunctionDef flag is set.
        if (scopePrefixes.isEmpty()) {
            // Look up type in current lexical scope
            val ident = scope.lookupType(name)
            if (ident != null) {
                return ident
            }

            // Next try to find an imported type with this alias
            val packageAndType = findPackageForAlias(name, importContext)
            if (packageAndType != null) {
                val (importedPackage, importedType) = packageAndType
                val typeIdent = importedPackage.scope.lookupType(importedType)
                if (typeIdent != null) {
                    return typeIdent
                }
            }

            // If no type was found yet, then create a type parameter if inFunctionDef flag is set
            if (inFunctionDef) {
                val typeParamIdent = symbolTable.addSymbolInScope(scope, name,
                        IdentifierClass.TYPE_PARAMETER, location, hashSetOf(), false)

                // Annotate type parameter ident with new type variable
                symbolTable.getInfo(typeParamIdent)?.type = TypeParameter()

                return typeParamIdent
            } else {
                throw IRConversionException("No type with name ${name} found", location)
            }
        // If there are scope prefixes, find correct package and lookup in it
        } else {
            val importedPackage = findPackage(scopePrefixes, importContext)
            if (importedPackage == null) {
                val fullReference = (scopePrefixes + name).joinToString("::")
                throw IRConversionException("Unresolved reference: $fullReference", location)
            }

            val typeIdent = importedPackage.scope.lookupType(name)
            if (typeIdent == null) {
                val fullReference = (scopePrefixes + name).joinToString("::")
                throw IRConversionException("Unresolved reference: $fullReference", location)
            }

            return typeIdent
        }
    }

    override fun symbol(): String = name
}

/**
 * Find the package with the given name in the given import context, throwing an error if the
 * package cannot be found.
 */
private fun findPackage(
    scopePrefixes: List<String>,
    importContext: ImportContext
): Package? {
    // The first scope prefix is a scope alias, try to resolve it
    val importAlias = scopePrefixes[0]
    val importParts = importContext.findImportForAlias(importAlias)
    if (importParts == null) {
        return null
    }

    // The full import statement is the unaliased import prepended to the rest of the scope
    val restOfScopePrefixes = scopePrefixes.drop(1)
    val fullImport = importParts + restOfScopePrefixes

    return importContext.rootPackageNode.getSubPackage(fullImport)
}

/**
 * Find the package and type name for the given alias, or null if the alias could not be resolved.
 */
private fun findPackageForAlias(
    alias: String,
    importContext: ImportContext
): Pair<Package, String>? {
    val importParts = importContext.findImportForAlias(alias)
    if (importParts != null) {
        // Find the package from the first portion of the imported statement
        val importedPackageParts = importParts.take(importParts.size - 1)
        val importedName = importParts[importParts.size - 1]

        // Return the package and imported name if the package is found
        val pack = importContext.rootPackageNode.getSubPackage(importedPackageParts)
        if (pack != null) {
            return Pair(pack, importedName)
        }
    }

    return null
}
