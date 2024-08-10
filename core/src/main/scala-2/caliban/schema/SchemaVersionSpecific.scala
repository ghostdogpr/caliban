package caliban.schema

trait GenericSchema[R] extends SchemaInstances with SchemaDerivation[R]

private[caliban] trait SchemaInstancesVersionSpecific
