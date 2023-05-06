package object

import "testing"

func TestStringHashKey(t *testing.T) {
	hello1 := &String{Value: "Hello World"}
	hello2 := &String{Value: "Hello World"}
	diff1 := &String{Value: "ThePrimeagen"}
	diff2 := &String{Value: "ThePrimeagen"}

	if hello1.HashKey() != hello2.HashKey() {
		t.Errorf("strings with same content have different hash keys")
	}

	if diff1.HashKey() != diff2.HashKey() {
		t.Errorf("strings with same content have different hash keys")
	}

	if hello1.HashKey() == diff1.HashKey() {
		t.Errorf("strings with different content have same hash keys")
	}
}

func TestIntegerHashKey(t *testing.T) {
	num1 := &Integer{Value: 314}
	num2 := &Integer{Value: 314}
	diff1 := &Integer{Value: 413}
	diff2 := &Integer{Value: 413}

	if num1.HashKey() != num2.HashKey() {
		t.Errorf("integers with same content have different hash keys")
	}

	if diff1.HashKey() != diff2.HashKey() {
		t.Errorf("integers with same content have different hash keys")
	}

	if num1.HashKey() == diff1.HashKey() {
		t.Errorf("integers with different content have same hash keys")
	}
}

func TestBooleanHashKey(t *testing.T) {
	bool1 := &Boolean{Value: true}
	bool2 := &Boolean{Value: true}
	diff1 := &Boolean{Value: false}
	diff2 := &Boolean{Value: false}

	if bool1.HashKey() != bool2.HashKey() {
		t.Errorf("booleans with same content have different hash keys")
	}

	if diff1.HashKey() != diff2.HashKey() {
		t.Errorf("booleans with same content have different hash keys")
	}

	if bool1.HashKey() == diff1.HashKey() {
		t.Errorf("booleans with different content have same hash keys")
	}
}