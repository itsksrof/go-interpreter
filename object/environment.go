package object

type Environment struct {
	store map[string]Object
}

// NewEnvironment initializes an empty map[string]Object, and returns an
// *Environment struct with it's store field set to the empty map.
func NewEnvironment() *Environment {
	s := make(map[string]Object)
	return &Environment{store: s}
}

// Get uses the given name to access a map entry.
func (e *Environment) Get(name string) (Object, bool) {
	obj, ok := e.store[name]
	return obj, ok
}

// Set uses the given name and the given val to add a new map entry.
func (e *Environment) Set(name string, val Object) Object {
	e.store[name] = val
	return val
}
