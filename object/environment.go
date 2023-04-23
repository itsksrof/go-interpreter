package object

type Environment struct {
	store	map[string]Object
	outer	*Environment
}

// NewEnvironment initializes an empty map[string]Object, and returns an
// *Environment struct with it's store field set to the empty map.
func NewEnvironment() *Environment {
	s := make(map[string]Object)
	return &Environment{store: s}
}

// NewEnclosedEnvironment initializes an empty map[string]Object, sets
// the env.outer field to the given outer env and returns the *Environment.
func NewEnclosedEnvironment(outer *Environment) *Environment {
	env := NewEnvironment()
	env.outer = outer
	return env
}

// Get uses the given name to access a map entry. If the value
// cannot be found and the outer *Environment is not nil, try's to
// access a map entry in the outer *Environment.
func (e *Environment) Get(name string) (Object, bool) {
	obj, ok := e.store[name]
	if !ok && e.outer != nil {
		obj, ok = e.outer.Get(name)
	}

	return obj, ok
}

// Set uses the given name and the given val to add a new map entry.
func (e *Environment) Set(name string, val Object) Object {
	e.store[name] = val
	return val
}
